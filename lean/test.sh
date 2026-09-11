#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
lake build --wfail
python3 -m unittest discover -s ../tests -p test_runner.py
python3 -m unittest discover -s ../tests -p test_export.py
python3 -m unittest discover -s ../tests -p test_results.py
python3 -m unittest discover -s ../tests -p test_workflows.py
python3 ../tests/run.py "$@"
