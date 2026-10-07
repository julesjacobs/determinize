#!/usr/bin/env bash
# PostToolUse (Edit|Write): fast feedback for the file that was just changed.
#   sim/**    -> biome check of that file (files Biome skips pass)
#   sim/src, sim/test, sim/build.ts, sim/tsconfig*.json -> npm run typecheck, npm test
#   site/*.css, site/*.mts -> biome check of that file; site/*.html -> html-validate
#   site/index.html -> its theorem quotes against lean/Determinize/Theorems.lean
#   tex/*.tex -> chktex lint of that file
#   lean/**.lean -> lake build (fails fast with a hint if the Mathlib cache is absent)
#   toolchain files -> remind to git add new files
#   *.det     -> remind to update corpus expectations
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
read_hook_input
file="$(rel_path "$(jfield tool_input.file_path)")"
[[ -z "$file" ]] && exit 0
cd "$ROOT"

if [[ "$file" == sim/* ]]; then
  out="$(cd sim && in_shell sim biome check --colors=off --files-ignore-unknown=true --no-errors-on-unmatched "${file#sim/}" 2>&1)" || {
    echo "biome check failed after editing $file ('biome check --write ${file#sim/}' in sim applies the safe fixes):" >&2
    tail -n 40 <<<"$out" >&2
    exit 2
  }
fi

case "$file" in
  site/*.css|site/*.mts)
    out="$(cd site && in_shell sim biome check --colors=off "${file#site/}" 2>&1)" || {
      echo "biome check failed after editing $file ('biome check --write ${file#site/}' in site applies the safe fixes):" >&2
      tail -n 40 <<<"$out" >&2
      exit 2
    }
    ;;
  site/*.html)
    out="$(in_shell sim html-validate "$file" 2>&1)" || {
      echo "html-validate failed after editing $file:" >&2
      tail -n 40 <<<"$out" >&2
      exit 2
    }
    if [[ "$file" == site/index.html ]]; then
      out="$(in_shell sim node site/theorems.mts --check 2>&1)" || {
        echo "$out" >&2
        exit 2
      }
    fi
    ;;
  sim/src/*|sim/test/*|sim/build.ts|sim/tsconfig*.json)
    out="$(cd sim && in_shell sim npm run typecheck 2>&1)" || {
      echo "npm run typecheck failed after editing $file:" >&2
      tail -n 40 <<<"$out" >&2
      exit 2
    }
    out="$(cd sim && in_shell sim npm test 2>&1)" || {
      echo "npm test failed after editing $file:" >&2
      grep -vE '^\s*(at |\||ℹ (start|duration|suites|cancelled|skipped|todo))' <<<"$out" | tail -n 40 >&2
      exit 2
    }
    ;;
  tex/*.tex)
    lint="$(cd tex && in_shell tex chktex -q -n1 -n3 -n8 -n13 -n24 -n36 -n44 -n46 "${file#tex/}" 2>/dev/null | head -n 25)"
    [[ -n "$lint" ]] && emit_context PostToolUse "chktex on $file (style hints, not errors; fix the ones that are real):"$'\n'"$lint"
    ;;
  lean/*.lean|lean/*/*.lean|lean/*/*/*.lean)
    out="$("$ROOT/check.sh" --quiet lean 2>&1)" || {
      echo "lake build failed after editing $file:" >&2
      tail -n 40 <<<"$out" >&2
      exit 2
    }
    ;;
  flake.nix|flake-modules/*|sim/package.json|*.envrc|lean/lakefile.toml|lean/lean-toolchain)
    emit_context PostToolUse "Toolchain definition changed ($file). New files must be 'git add'ed before Nix can see them."
    ;;
  tests/*.det|tests/*/*.det|tests/*/*/*.det|tests/*/*/*/*.det|examples/*.det|examples/*/*.det)
    emit_context PostToolUse "$file changed: register its expectations in tests/cases.toml and run ./lean/test.sh (or --all for statistical cases)."
    ;;
esac
exit 0
