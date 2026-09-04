#!/usr/bin/env bash
# Shared helpers for Claude Code hooks in this repo. Source, don't execute.
#
# Design: hooks run in whatever environment Claude Code was launched from,
# which usually has only the root devshell (.#ocaml) loaded via direnv.
# `in_shell NAME CMD...` runs CMD with the toolchain of flake devShell NAME:
#   1. if the command is already on PATH, run it directly (fast path);
#   2. else if direnv is installed, `direnv exec <dir>` (uses nix-direnv cache, ~0.2s);
#   3. else `nix develop .#NAME --command` (cold: several seconds);
#   4. else run bare and let it fail with a clear message.

ROOT="${CLAUDE_PROJECT_DIR:-$(git -C "$(dirname "${BASH_SOURCE[0]}")" rev-parse --show-toplevel 2>/dev/null || pwd)}"

# Directory whose .envrc loads a given devshell.
shell_dir() {
  case "$1" in
    ocaml) echo "$ROOT" ;;
    sim) echo "$ROOT/sim" ;;
    tex) echo "$ROOT/tex" ;;
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

# Read the hook's JSON payload from stdin into $HOOK_INPUT and expose a
# `jfield PATH` accessor (dot-separated, e.g. tool_input.file_path).
read_hook_input() {
  HOOK_INPUT="$(cat)"
}

jfield() {
  printf '%s' "$HOOK_INPUT" | python3 -c '
import json, sys
path = sys.argv[1].split(".")
try:
    v = json.load(sys.stdin)
    for p in path:
        v = v[p]
    if isinstance(v, bool):
        print("true" if v else "false")
    elif v is None:
        print("")
    else:
        print(v)
except Exception:
    print("")
' "$1"
}

# Emit a PostToolUse/SessionStart-style additionalContext JSON object.
emit_context() {
  local event="$1" text="$2"
  python3 -c '
import json, sys
print(json.dumps({"hookSpecificOutput": {"hookEventName": sys.argv[1], "additionalContext": sys.argv[2]}}))
' "$event" "$text"
}

# Path of the edited file relative to the repo root (empty if not under it).
rel_path() {
  local p="$1"
  case "$p" in
    "$ROOT"/*) printf '%s\n' "${p#"$ROOT"/}" ;;
    /*) printf '' ;;
    *) printf '%s\n' "$p" ;;
  esac
}

# Cross-platform user notification: macOS `say` (per AGENTS.md), Linux notify-send,
# otherwise a terminal bell. Never fails.
notify_user() {
  local msg="$1"
  if command -v say >/dev/null 2>&1; then
    say "$msg" >/dev/null 2>&1 &
  elif command -v notify-send >/dev/null 2>&1; then
    notify-send "Claude Code" "$msg" >/dev/null 2>&1 || true
  elif command -v osascript >/dev/null 2>&1; then
    osascript -e "display notification \"$msg\" with title \"Claude Code\"" >/dev/null 2>&1 || true
  fi
  printf '\a' > /dev/tty 2>/dev/null || true
}
