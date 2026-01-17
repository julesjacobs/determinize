#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"

usage() {
  echo "Usage: $0 path/to/<file.det>" >&2
  exit 2
}

[[ $# -eq 1 ]] || usage

DET_IN="$1"

if [[ "$DET_IN" != /* ]]; then
  DET_IN="$ROOT/$DET_IN"
fi

[[ -f "$DET_IN" ]] || { echo "Error: .det file not found: $DET_IN" >&2; exit 1; }

# Compute expected output stems:
# If input is /.../foo.det then base is /.../foo
BASE="${DET_IN%.det}"

TRA="${BASE}.det.tra"
LAB="${BASE}.det.lab"
STATE_REW="${BASE}.det.state.rew"

# Run OCaml generator from /ocaml
cd "$ROOT/ocaml"
dune build ./determinize_main.exe
dune exec ./determinize_main.exe -- "$DET_IN"

[[ -f "$TRA" ]] || { echo "Error: missing transition file: $TRA" >&2; exit 1; }
[[ -f "$LAB" ]] || { echo "Error: missing label file: $LAB" >&2; exit 1; }
[[ -f "$STATE_REW" ]] || { echo "Error: missing label file: $STATE_REW" >&2; exit 1; }

# Run STORM
storm --explicit "$TRA" "$LAB" \
  --staterew "$STATE_REW" \
  --prop 'R=? [ F "done" ]'
