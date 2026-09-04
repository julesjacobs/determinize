#!/usr/bin/env bash
# PostToolUse (Edit|Write): fast feedback for the file that was just changed.
#   ocaml/*   -> dune build (type errors surface immediately)
#   sim/*     -> node --test
#   tex/*.tex -> chktex lint of that file
#   toolchain files -> remind to run /learn-tool
#   *.det     -> remind to regenerate .dout
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
read_hook_input
file="$(rel_path "$(jfield tool_input.file_path)")"
[[ -z "$file" ]] && exit 0
cd "$ROOT"

case "$file" in
  ocaml/*.ml|ocaml/*.mli|ocaml/*.mll|ocaml/*.mly|ocaml/dune|ocaml/dune-project)
    out="$(cd ocaml && in_shell ocaml dune build 2>&1)" || {
      echo "dune build failed after editing $file:" >&2
      tail -n 40 <<<"$out" >&2
      exit 2
    }
    ;;
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
  flake.nix|flake-modules/*|sim/package.json|ocaml/dune|ocaml/dune-project|*.envrc)
    emit_context PostToolUse "Toolchain definition changed ($file). If this adds a new tool or dependency, run the learn-tool skill for it (/learn-tool <name>) so its best practices get captured in .claude/rules/ before you rely on it. New files must be 'git add'ed before Nix can see them."
    ;;
  det/*.det|examples/*.det|examples/*/*.det)
    emit_context PostToolUse "$file changed: regenerate its golden output with './run.sh $file' (or ./det.sh for all of det/) and review the .dout diff before finishing."
    ;;
esac
exit 0
