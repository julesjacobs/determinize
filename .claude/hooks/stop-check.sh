#!/usr/bin/env bash
# Stop: enforce AGENTS.md "always make sure that code / tex builds after making changes".
# Runs the checks for the areas that have uncommitted changes; blocks the stop
# (exit 2) with the failure summary so Claude fixes it before finishing.
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
read_hook_input
# Avoid infinite loops: if this hook already blocked once, let the turn end.
[[ "$(jfield stop_hook_active)" == "true" ]] && exit 0
[[ "${CLAUDE_SKIP_STOP_CHECK:-0}" == "1" ]] && exit 0
cd "$ROOT"

out="$("$ROOT/.claude/scripts/check.sh" --changed 2>&1)"
rc=$?
if [[ $rc -ne 0 ]]; then
  {
    echo "Build/test check failed for files with uncommitted changes. Fix the cause (do not suppress it), then finish:"
    echo "$out"
  } >&2
  exit 2
fi
exit 0
