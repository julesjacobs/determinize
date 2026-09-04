#!/usr/bin/env bash
# SessionStart: a few lines of live context (stdout is added to Claude's context).
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
read_hook_input
cd "$ROOT"

have() { command -v "$1" >/dev/null 2>&1 && echo "$1 ✓" || echo "$1 ✗"; }
echo "Toolchain on PATH right now: $(have dune) $(have node) $(have latexmk) $(have storm) $(have direnv) $(have nix)"
echo "Missing tools are reachable via 'direnv exec <dir> CMD' or 'nix develop .#<ocaml|sim|tex> --command CMD' (see CLAUDE.md)."
branch="$(git branch --show-current 2>/dev/null)"
dirty="$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ')"
echo "git: branch $branch, $dirty uncommitted path(s); last commit: $(git log -1 --format='%h %s' 2>/dev/null)"
open_todos="$(grep -cE '^\[\]' TODO.md 2>/dev/null || echo 0)"
echo "TODO.md: $open_todos open item(s):"
grep -E '^\[\]' TODO.md 2>/dev/null | head -5 | sed 's/^/  /'
exit 0
