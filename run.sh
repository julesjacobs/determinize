#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"

usage() {
  echo "Usage: $0 [--storm] [--limit N] path/to/file.det" >&2
  exit 2
}

RUN_STORM=0
LIMIT=""

# Parse args
case "${1:-}" in
  --storm)
    RUN_STORM=1
    shift
    ;;
esac

case "${1:-}" in
  --limit)
    [[ "$RUN_STORM" -eq 1 ]] || usage
    [[ $# -ge 2 ]] || usage
    LIMIT="$2"
    shift 2
    ;;
esac

[[ $# -eq 1 ]] || usage

DET_IN="$1"

if [[ "$DET_IN" != /* ]]; then
  DET_IN="$ROOT/$DET_IN"
fi

[[ -f "$DET_IN" ]] || { echo "Error: .det file not found: $DET_IN" >&2; exit 1; }

TRA="${DET_IN}.tra"
LAB="${DET_IN}.lab"
STATE_REW="${DET_IN}.state.rew"

cd "$ROOT/ocaml"
dune build ./determinize_main.exe

if [[ "$RUN_STORM" -eq 1 ]]; then
  if [[ -n "$LIMIT" ]]; then
    dune exec ./determinize_main.exe -- --storm --limit "$LIMIT" "$DET_IN"
  else
    dune exec ./determinize_main.exe -- --storm "$DET_IN"
  fi

  [[ -f "$TRA" ]] || { echo "Error: missing transition file: $TRA" >&2; exit 1; }
  [[ -f "$LAB" ]] || { echo "Error: missing label file: $LAB" >&2; exit 1; }
  [[ -f "$STATE_REW" ]] || { echo "Error: missing state rewards file: $STATE_REW" >&2; exit 1; }

  storm --explicit "$TRA" "$LAB" \
    --staterew "$STATE_REW" \
    --prop 'R=? [ F "done" || F "accept" ]'
else
  dune exec ./determinize_main.exe -- "$DET_IN"
fi
