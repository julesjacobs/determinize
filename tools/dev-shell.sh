#!/usr/bin/env bash
# Source after setting ROOT to the repository root.
# Use installed tools first, then an available direnv or Nix development shell.

# Directory whose .envrc loads a given devshell.
shell_dir() {
  case "$1" in
    sim) echo "$ROOT/sim" ;;
    tex) echo "$ROOT/tex" ;;
    lean) echo "$ROOT/lean" ;;
    *) echo "$ROOT" ;;
  esac
}

in_shell() {
  local name="$1"; shift
  local dir; dir="$(shell_dir "$name")"
  if command -v "$1" >/dev/null 2>&1; then
    "$@"
  elif command -v direnv >/dev/null 2>&1 && direnv exec "$dir" true >/dev/null 2>&1; then
    direnv exec "$dir" "$@"
  elif command -v nix >/dev/null 2>&1; then
    (cd "$ROOT" && nix develop ".#$name" --command "$@")
  else
    "$@"
  fi
}
