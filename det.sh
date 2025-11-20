#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
cd "$ROOT/ocaml"

dune build determinize_main.exe

for f in "$ROOT"/det/*.det; do
  echo "==> $f"
  dune exec ./determinize_main.exe -- "$f"
done
