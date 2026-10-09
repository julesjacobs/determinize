#!/usr/bin/env bash
# Source after setting ROOT to the repository root.
# Use installed tools first, then an available direnv or Nix development shell. The command runs
# in the caller's directory. Simulator commands also need sim/node_modules, which only the sim and
# site shells link, and the site shell's commands Playwright's browsers, so without them they run
# in that shell. The site shell has no .envrc, so that its browser is downloaded only when needed.

# Directory whose .envrc loads a given devshell; none for the site shell.
shell_dir() {
  case "$1" in
    sim) echo "$ROOT/sim" ;;
    tex) echo "$ROOT/tex" ;;
    lean) echo "$ROOT/lean" ;;
    site) ;;
    *) echo "$ROOT" ;;
  esac
}

# Whether the shell's tools are on PATH: lake for the lean shell, whose scripts (./test.sh) exist
# without it, and on Linux also the shell's STORM_PYTHON, which a lake of a global elan lacks; the
# command itself for the others.
has_shell() {
  local name="$1" command="$2"
  case "$name" in
    lean) command -v lake && [[ "$OSTYPE" != linux* || -n "${STORM_PYTHON:-}" ]] ;;
    sim) command -v "$command" && [[ -e "$ROOT/sim/node_modules" ]] ;;
    site) command -v "$command" && [[ -e "$ROOT/sim/node_modules" && -n "${PLAYWRIGHT_BROWSERS_PATH:-}" ]] ;;
    *) command -v "$command" ;;
  esac >/dev/null 2>&1
}

in_shell() {
  local name="$1"; shift
  local dir; dir="$(shell_dir "$name")"
  if has_shell "$name" "$1"; then
    "$@"
  elif [[ -n "$dir" ]] && command -v direnv >/dev/null 2>&1 && direnv exec "$dir" true >/dev/null 2>&1; then
    direnv exec "$dir" "$@"
  elif command -v nix >/dev/null 2>&1; then
    nix develop "$ROOT#$name" --command "$@"
  else
    "$@"
  fi
}
