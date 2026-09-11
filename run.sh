#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
(cd "$ROOT/lean" && lake build determinize >&2)
if [[ "${1:-}" == --storm ]]; then
  shift
  exec "${STORM_PYTHON:-python3}" "$ROOT/tools/storm.py" "$@"
fi
exec "$ROOT/lean/.lake/build/bin/determinize" "$@"
