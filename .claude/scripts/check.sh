#!/usr/bin/env bash
# Build/test runner shared by the Stop hook and the /check-all skill.
#
#   check.sh [--quiet] AREA...      AREA in: ocaml sim bundle tex det
#   check.sh --changed              pick areas from `git status` (what the Stop hook does)
#   check.sh --all                  everything except `det` (which rewrites golden files)
#
# Exit 0 = all selected checks passed, 1 = at least one failed. A human-readable
# summary goes to stdout; the last ~40 lines of any failing tool go there too.
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=../hooks/lib.sh
source "$HERE/../hooks/lib.sh"
cd "$ROOT"

areas=()
quiet=0
while [[ $# -gt 0 ]]; do
  case "$1" in
    --quiet) quiet=1 ;;
    --all) areas+=(ocaml sim bundle tex) ;;
    --changed)
      changed="$(git status --porcelain --untracked-files=all | cut -c4-)"
      grep -qE '^ocaml/' <<<"$changed" && areas+=(ocaml)
      grep -qE '^sim/(src|test)/|^sim/package(-lock)?\.json' <<<"$changed" && areas+=(sim bundle)
      grep -qE '^tex/.*\.(tex|bib|cls|bst|sty)$' <<<"$changed" && areas+=(tex)
      ;;
    ocaml|sim|bundle|tex|det) areas+=("$1") ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
  shift
done
[[ ${#areas[@]} -eq 0 ]] && exit 0
# de-duplicate, keep order
mapfile -t areas < <(printf '%s\n' "${areas[@]}" | awk '!seen[$0]++')

fail=0
report() { # name status detail
  printf '%s: %s\n' "$1" "$2"
  [[ -n "${3:-}" ]] && printf '%s\n' "$3"
}
tail_of() { tail -n 40; }

for area in "${areas[@]}"; do
  case "$area" in
    ocaml)
      out="$(cd ocaml && in_shell ocaml dune build 2>&1)"
      if [[ $? -eq 0 ]]; then report ocaml "dune build OK"
      else fail=1; report ocaml "dune build FAILED" "$(tail_of <<<"$out")"; fi
      ;;
    sim)
      out="$(cd sim && in_shell sim npm test 2>&1)"
      if [[ $? -eq 0 ]]; then report sim "npm test OK ($(grep -oE 'pass [0-9]+' <<<"$out" | head -1))"
      else fail=1; report sim "npm test FAILED" "$(grep -vE '^\s*(at |\||ℹ (start|duration|suites|cancelled|skipped|todo))' <<<"$out" | tail_of)"; fi
      ;;
    bundle)
      # The bundle is committed: if sources changed but the bundle did not, it is stale.
      src_changed="$(git status --porcelain --untracked-files=all -- sim/src sim/package.json sim/package-lock.json | head -1)"
      bundle_changed="$(git status --porcelain -- sim/app.bundle.js | head -1)"
      if [[ -n "$src_changed" && -z "$bundle_changed" ]]; then
        fail=1; report bundle "STALE: sim/src changed but sim/app.bundle.js was not rebuilt. Run: cd sim && npm run build"
      else report bundle "OK"; fi
      ;;
    tex)
      out="$(cd tex && in_shell tex latexmk -pdf -interaction=nonstopmode -file-line-error -silent main.tex 2>&1)"
      rc=$?
      log="tex/main.log"
      errors="$(grep -E '^(! |\./.*\.tex:[0-9]+: )' "$log" 2>/dev/null | head -20)"
      undefined="$(grep -E "LaTeX Warning: (Reference|Citation) .* undefined" "$log" 2>/dev/null | sort -u | head -20)"
      multiply="$(grep -E "multiply[- ]defined" "$log" 2>/dev/null | sort -u | head -10)"
      overfull="$(grep -c '^Overfull' "$log" 2>/dev/null || echo 0)"
      if [[ $rc -ne 0 || -n "$errors" || -n "$undefined" ]]; then
        fail=1
        report tex "latexmk FAILED (exit $rc)" "${errors}${errors:+$'\n'}${undefined}"
        [[ -n "$out" ]] && printf '%s\n' "$(tail -n 15 <<<"$out")"
      else
        report tex "latexmk OK (overfull boxes: $overfull)"
      fi
      [[ -n "$multiply" ]] && report tex "warning: multiply-defined labels" "$multiply"
      ;;
    det)
      out="$(in_shell ocaml ./det.sh 2>&1)"
      if [[ $? -ne 0 ]]; then fail=1; report det "det.sh FAILED" "$(tail_of <<<"$out")"
      else
        diff="$(git status --porcelain -- det/ | grep -E '\.dout$' || true)"
        if [[ -n "$diff" ]]; then report det "golden outputs changed (review with git diff det/)" "$diff"
        else report det "all det/*.det.dout unchanged"; fi
      fi
      ;;
  esac
done
exit $fail
