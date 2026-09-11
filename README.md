# Determinize

A probabilistic language with E/G sampling modes, a Lean implementation and
formalization, exact expected-reward certificates, and a browser simulator.

- `lean/`: parser, inference, checked typing, determinization, numerical execution,
  exact finite-state exploration, and kernel-checkable certificates.
- `tests/`, `examples/`: shared `.det` programs and analytical expectations.
- `tools/storm.py`: exact Storm comparison against independently checked certificates.
- `sim/`: browser visualization of ordinary, symbolic, and determinized execution.
- `tex/`: paper.

## Setup

Install [elan](https://github.com/leanprover/elan) and Python 3.11 or newer. Lean
and Mathlib versions are pinned in `lean/`. Fetch the Mathlib cache once:

```sh
cd lean
lake exe cache get
lake build --wfail
```

Alternatively, `nix develop .#lean` supplies elan, Python, and uv. The root `.envrc`
selects that shell. `nix develop` provides the combined Lean, simulator, and paper
shell; `.#sim` and `.#tex` select individual toolchains.

## Run and check

From the repository root:

```sh
./run.sh tests/execution/legacy/arith.det
./run.sh --samples 20000 --seed 1 tests/statistical/gaussian.det
./run.sh --result /tmp/model --subject source tests/statistical/discrete.det
(cd lean && lake env lean /tmp/model.result.lean)
./det.sh --all
```

`run.sh` builds and invokes the Lean CLI; arguments and relative paths are passed
through. `det.sh` runs the full Lean test entry point, including the shared corpus
and independent certificates. It accepts `--all` and `--statistical`.

An E draw is replaced by its distribution's mean; a G draw remains stochastic.
Finite-model certificates prove the selected core program's integrability and
exact expected terminal reward. Rejection contributes zero, without conditioning.
The default subject is `determinized`; use `--subject source` for a source model.
Transferring a determinized answer to the source requires the determinization
theorem's premises. See [the contract](lean/finite-model-contract.md).

## Storm

```sh
uv venv --python 3.12 /tmp/determinize-storm
uv pip install --python /tmp/determinize-storm/bin/python -r tools/storm-requirements.txt
STORM_PYTHON=/tmp/determinize-storm/bin/python ./run.sh --storm \
  tests/statistical/discrete.det --prefix /tmp/model --subject source
STORM_PYTHON=/tmp/determinize-storm/bin/python ./det.sh --all
```

Storm uses exact rational matrices. The wrapper checks `.result.lean` with Lean's
kernel and requires exact agreement with Storm. It records versions, commands,
status, and failures in `.storm.json`. Without `STORM_PYTHON`, the ordinary test
suite explicitly skips real Storm comparisons; the Lean certificate tests still run.

## Simulator and paper

```sh
(cd sim && npm ci && npm test && npm run build)
(cd tex && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex)
```

Open `sim/index.html` to explore coupled traces. The simulator is an unverified
visualization and has documented differences from Lean. Do not deploy it as part
of local verification.

[Migration audit](migration-audit.md) records replacement coverage, deliberate
behavioral changes, and the retirement of the old symbolic-coupling CLI.
