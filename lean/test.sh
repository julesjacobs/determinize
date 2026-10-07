#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
lake build --wfail
# The test modules (every ../tests/test_*.py) and the corpus are independent and each keeps one
# core busy, so they run at once, on the executables built above; none of them builds anything
# that is out of date. Each one's output is printed whole, in that order, and any failure fails
# the script.
logs="$(mktemp -d)"
trap 'rm -rf "$logs"' EXIT
names=()
pids=()
for module in ../tests/test_*.py; do
  name="$(basename "$module" .py)"
  python3 -m unittest discover -s ../tests -p "$name.py" >"$logs/$name" 2>&1 &
  names+=("$name")
  pids+=($!)
done
names+=(corpus)
python3 ../tests/run.py "$@" >"$logs/corpus" 2>&1 &
pids+=($!)
status=0
for i in "${!names[@]}"; do
  wait "${pids[$i]}" || status=1
  cat "$logs/${names[$i]}"
done
exit "$status"
