#!/usr/bin/env bash
# Repository build, test, and theorem-axiom checks.
#
#   check.sh [--quiet] AREA...      AREA in: sim bundle site tex lean det
#   check.sh --changed              pick areas from `git status` (what the Stop hook does)
#   check.sh --all                  builds, full corpus/certificates, simulator, bundle, site and paper
#
# Exit 0 = all selected checks passed, 1 = at least one failed. A human-readable
# summary goes to stdout; the last ~40 lines of any failing tool go there too.
set -uo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=tools/dev-shell.sh
source "$ROOT/tools/dev-shell.sh"
cd "$ROOT"

areas=()
quiet=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --quiet) quiet=1 ;;
    --all) areas+=(lean det sim bundle site tex) ;;
    --changed)
      changed="$(git status --porcelain --untracked-files=all | cut -c4-)"
      grep -qE '^(check\.sh|tools/dev-shell\.sh)$' <<<"$changed" && areas+=(lean det sim bundle site tex)
      # The simulator's toolchain: the flake, which pins Node.js, and the sim shell.
      toolchain='^flake\.(nix|lock)$|^flake-modules/(systems|devshells/sim)\.nix$'
      grep -qE "^sim/(src|test)/|^sim/build\.|^sim/(package(-lock)?|biome|tsconfig(\.node)?)\.json|^(tests|examples)/|$toolchain" <<<"$changed" && areas+=(sim)
      grep -qE "^sim/(src|test)/|^sim/build\.|^sim/(package(-lock)?|tsconfig)\.json|^sim/index\.html|^examples/|$toolchain" <<<"$changed" && areas+=(bundle)
      grep -qE "^site/|^sim/|^examples/|^flake-modules/devshells/site\.nix$|^lean/Determinize/Theorems\.lean$|$toolchain" <<<"$changed" && areas+=(site)
      grep -qE '^tex/.*\.(tex|bib|cls|bst|sty)$' <<<"$changed" && areas+=(tex)
      grep -qE '^lean/.*\.lean$|^lean/(lakefile\.toml|lean-toolchain|lake-manifest\.json)$' <<<"$changed" && areas+=(lean)
      grep -qE '^tests/|^examples/|^tools/|^(test|run)\.sh$|^lean/test\.sh$' <<<"$changed" && areas+=(det)
      ;;
    sim|bundle|site|tex|lean|det) areas+=("$1") ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
  shift
done
[[ ${#areas[@]} -eq 0 ]] && exit 0
# de-duplicate, keep order
unique_areas=()
while IFS= read -r area; do unique_areas+=("$area"); done < <(printf '%s\n' "${areas[@]}" | awk '!seen[$0]++')
areas=("${unique_areas[@]}")

fail=0
report() { # name status detail
  printf '%s: %s\n' "$1" "$2"
  [[ -n "${3:-}" ]] && printf '%s\n' "$3"
}
tail_of() { tail -n 40; }

for area in "${areas[@]}"; do
  case "$area" in
    sim)
      out="$(cd sim && in_shell sim biome ci --colors=off . 2>&1)"
      if [[ $? -eq 0 ]]; then report sim "biome ci OK"
      else fail=1; report sim "biome ci FAILED (cd sim && biome check --write . applies the safe fixes)" "$(tail_of <<<"$out")"; fi
      out="$(cd sim && in_shell sim npm run typecheck 2>&1)"
      if [[ $? -eq 0 ]]; then report sim "npm run typecheck OK"
      else fail=1; report sim "npm run typecheck FAILED" "$(tail_of <<<"$out")"; fi
      out="$(cd sim && in_shell sim npm test 2>&1)"
      if [[ $? -eq 0 ]]; then report sim "npm test OK ($(grep -oE 'pass [0-9]+' <<<"$out" | head -1))"
      else fail=1; report sim "npm test FAILED" "$(grep -vE '^\s*(at |\||ℹ (start|duration|suites|cancelled|skipped|todo))' <<<"$out" | tail_of)"; fi
      ;;
    bundle)
      out="$(cd sim && in_shell sim npm run build 2>&1)"
      if [[ $? -eq 0 ]]; then report bundle "npm run build OK"
      else fail=1; report bundle "npm run build FAILED" "$(tail_of <<<"$out")"; fi
      ;;
    site)
      # The site's scripts and CSS, its quotes of the theorems, then the site as pages.yml
      # assembles it, without the documentation and the paper, in a browser and for its links.
      out="$(cd site && in_shell site biome ci --colors=off . 2>&1 && in_shell site tsc -p tsconfig.json 2>&1 \
        && in_shell site node theorems.mts --check 2>&1)"
      if [[ $? -eq 0 ]]; then report site "biome ci, tsc and the theorem quotes OK"
      else fail=1; report site "biome ci, tsc or the theorem quotes FAILED" "$(tail_of <<<"$out")"; fi
      out="$(cd sim && in_shell site npm run build 2>&1 && cd .. && rm -rf _preview \
        && site/assemble.sh --out _preview/determinize 2>&1)"
      if [[ $? -ne 0 ]]; then fail=1; report site "assembling _preview FAILED" "$(tail_of <<<"$out")"
      else
        out="$(cd sim && in_shell site playwright test --reporter=line 2>&1)"
        if [[ $? -eq 0 ]]; then report site "playwright test OK ($(grep -oE '[0-9]+ passed' <<<"$out" | tail -1))"
        else fail=1; report site "playwright test FAILED" "$(grep -v '^\[WebServer\]' <<<"$out" | tail_of)"; fi
        out="$(in_shell site html-validate site 2>&1)"
        if [[ $? -eq 0 ]]; then report site "html-validate OK"
        else fail=1; report site "html-validate FAILED" "$(tail_of <<<"$out")"; fi
        out="$(in_shell site lychee --offline --no-progress --include-fragments --root-dir "$ROOT/_preview" \
          --exclude '^file://.*/_preview/determinize/docs(/|$)' _preview 2>&1)"
        if [[ $? -eq 0 ]]; then report site "lychee --offline OK (docs/ excluded: the preview has none)"
        else fail=1; report site "lychee --offline FAILED" "$(tail_of <<<"$out")"; fi
      fi
      ;;
    tex)
      out="$(cd tex && in_shell tex latexmk -pdf -interaction=nonstopmode -file-line-error -silent main.tex 2>&1)"
      rc=$?
      log="tex/main.log"
      errors="$(grep -E '^(! |\./.*\.tex:[0-9]+: )' "$log" 2>/dev/null | head -20)"
      undefined="$(grep -E "LaTeX Warning: (Reference|Citation) .* undefined" "$log" 2>/dev/null | sort -u | head -20)"
      multiply="$(grep -E "multiply[- ]defined" "$log" 2>/dev/null | sort -u | head -10)"
      overfull="$(grep -c '^Overfull' "$log" 2>/dev/null || true)"
      if [[ $rc -ne 0 || -n "$errors" || -n "$undefined" ]]; then
        fail=1
        report tex "latexmk FAILED (exit $rc)" "${errors}${errors:+$'\n'}${undefined}"
        [[ -n "$out" ]] && printf '%s\n' "$(tail -n 15 <<<"$out")"
      else
        report tex "latexmk OK (overfull boxes: $overfull)"
      fi
      [[ -n "$multiply" ]] && report tex "warning: multiply-defined labels" "$multiply"
      ;;
    lean)
      # Never let lake compile Mathlib from source (hours): require the downloaded cache first.
      if [[ ! -d lean/.lake/packages/mathlib/.lake/build ]]; then
        fail=1; report lean "Mathlib cache not fetched: run 'cd lean && lake exe cache get' (downloads prebuilt .olean files, once), then 'lake build'"
      else
        # The formalization is complete: any warning (a `sorry` included) fails the build, and
        # Theorems.lean fails it unless every theorem is proved using the standard axioms only and
        # its statement relies on Proof only for proofs.
        out="$(cd lean && in_shell lean lake build --wfail 2>&1)"
        if [[ $? -eq 0 ]]; then
          report lean "lake build --wfail OK ($(grep -oE '[0-9]+ theorems proved' <<<"$out"); $(grep -oE 'statements rely on [0-9]+ declarations other than proofs, none from Proof' <<<"$out"))"
        else fail=1; report lean "lake build --wfail FAILED" "$(grep -vE '^(✔|⚠) \[' <<<"$out" | tail_of)"; fi
      fi
      ;;
    det)
      # test.sh starts with `lake build`, which must not compile Mathlib either (see lean).
      if [[ ! -d lean/.lake/packages/mathlib/.lake/build ]]; then
        fail=1; report det "Mathlib cache not fetched: run 'cd lean && lake exe cache get' (downloads prebuilt .olean files, once), then './test.sh --all'"
      else
        out="$(in_shell lean ./test.sh --all 2>&1)"
        if [[ $? -ne 0 ]]; then fail=1; report det "test.sh FAILED" "$(tail_of <<<"$out")"
        elif [[ -n "${STORM_PYTHON:-}" ]]; then
          report det "Lean corpus and certificate tests passed, including real Storm comparisons"
        else
          report det "Lean corpus and certificate tests passed; real Storm skipped (set STORM_PYTHON)"
        fi
      fi
      ;;
  esac
done
exit $fail
