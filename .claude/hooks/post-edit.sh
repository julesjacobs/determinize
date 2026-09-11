#!/usr/bin/env bash
# PostToolUse (Edit|Write): fast feedback for the file that was just changed.
#   sim/*     -> node --test
#   tex/*.tex -> chktex lint of that file
#   lean/**.lean -> lake build (fails fast with a hint if the Mathlib cache is absent)
#   toolchain files -> remind to run /learn-tool
#   *.det     -> remind to update corpus expectations
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
read_hook_input
file="$(rel_path "$(jfield tool_input.file_path)")"
[[ -z "$file" ]] && exit 0
cd "$ROOT"

case "$file" in
  sim/src/*|sim/test/*)
    out="$(cd sim && in_shell sim node --test 2>&1)" || {
      echo "node --test failed after editing $file:" >&2
      grep -vE '^\s*(at |\||ℹ (start|duration|suites|cancelled|skipped|todo))' <<<"$out" | tail -n 40 >&2
      exit 2
    }
    ;;
  tex/*.tex)
    lint="$(cd tex && in_shell tex chktex -q -n1 -n3 -n8 -n13 -n24 -n36 -n44 -n46 "${file#tex/}" 2>/dev/null | head -n 25)"
    [[ -n "$lint" ]] && emit_context PostToolUse "chktex on $file (style hints, not errors; fix the ones that are real):"$'\n'"$lint"
    ;;
  lean/*.lean|lean/*/*.lean|lean/*/*/*.lean)
    out="$("$ROOT/.claude/scripts/check.sh" --quiet lean 2>&1)" || {
      echo "lake build failed after editing $file:" >&2
      tail -n 40 <<<"$out" >&2
      exit 2
    }
    ;;
  flake.nix|flake-modules/*|sim/package.json|*.envrc|lean/lakefile.toml|lean/lean-toolchain)
    emit_context PostToolUse "Toolchain definition changed ($file). If this adds a new tool or dependency, run the learn-tool skill for it (/learn-tool <name>) so its best practices get captured in .claude/rules/ before you rely on it. New files must be 'git add'ed before Nix can see them."
    ;;
  tests/*.det|tests/*/*.det|tests/*/*/*.det|tests/*/*/*/*.det|examples/*.det|examples/*/*.det)
    emit_context PostToolUse "$file changed: register its expectations in tests/cases.toml and run ./lean/test.sh (or --all for statistical cases)."
    ;;
esac
exit 0
