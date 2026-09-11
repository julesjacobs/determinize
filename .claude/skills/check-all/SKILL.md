---
name: check-all
description: Run Lean proofs, corpus and certificate tests, optional real Storm comparisons, simulator checks, bundle freshness, and the paper build.
argument-hint: "[sim|bundle|tex|lean|det ...] (default: --all)"
allowed-tools: Bash, Read, Grep
---

Run `.claude/scripts/check.sh --all`, or pass selected areas. `--changed` selects
areas from Git status. Checks run sequentially; never run competing Lake builds.

- `lean`: warning-free Lake build and standard-axiom reports.
- `det`: `./det.sh --all`, covering unit/corpus/statistical/certificate/workflow tests.
  Set `STORM_PYTHON` to the pinned stormpy environment to include real Storm runs.
- `sim`: Node tests.
- `bundle`: checks whether changed simulator sources have a rebuilt bundle.
- `tex`: latexmk; errors and undefined references fail, layout warnings are reported.

Use installed tools or the corresponding Nix devshell. Fetch the Mathlib cache
with `lake exe cache get` before the first build. Fix failures without weakening
expectations; rerun affected checks. State explicitly when real Storm was skipped
or a required toolchain was unavailable. Local checks must not deploy or push.
