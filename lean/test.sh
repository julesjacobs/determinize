#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
lake build --wfail
files=()
while IFS= read -r path; do
  if [[ "$path" != ../examples/loops/example.det ]]; then
    files+=("$path")
  fi
done < <(rg --files ../det ../examples Tests/fixtures -g '*.det' | sort)
.lake/build/bin/det-tests "${files[@]}"
if .lake/build/bin/determinize --check ../examples/loops/example.det >/dev/null 2>&1; then
  echo 'The non-executable sketch unexpectedly parsed' >&2
  exit 1
fi
scratch="$(mktemp -d "${TMPDIR:-/tmp}/determinize-tests.XXXXXX")"
trap 'rm -rf "$scratch"' EXIT
for input in ../det/foldr.det ../det/factorial.det ../det/observe.det ../det/lambda.det Tests/fixtures/primitives.det Tests/fixtures/identity.det; do
  .lake/build/bin/determinize --check --certificate "$scratch/Certificate.lean" "$input"
  lake env lean "$scratch/Certificate.lean" > "$scratch/axioms.log"
  if rg -q 'sorryAx|ofReduceBool|lean4Lean|trustCompiler' "$scratch/axioms.log"; then
    cat "$scratch/axioms.log" >&2
    exit 1
  fi
done
printf '%s\n' 'Independent kernel certificate checks passed.'
