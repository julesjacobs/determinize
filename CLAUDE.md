# CLAUDE.md

@AGENTS.md

## Project

Determinize is a probabilistic `.det` language with E/G sampling modes, a Lean
implementation and formalization, a browser simulator, and a paper.

| Directory | Artifact | Nix devshell |
|---|---|---|
| `lean/` | Frontend, checked typing, determinization, runtime, exact models and certificates; specifications in `Statement/` and `Traces/`, proofs in `Proof/` | `.#lean` (also root `.envrc`) |
| `tests/`, `examples/` | Shared corpus and analytical expectations in `tests/cases.toml` | `.#lean` |
| `tools/` | Exact Storm comparison through pinned stormpy | `.#lean` plus a separate Python environment |
| `sim/` | Unverified JavaScript compiler and coupled-trace visualization | `.#sim` |
| `tex/` | Paper | `.#tex` |
| `flake-modules/` | Automatically imported flake-parts modules | Default shell combines Lean, sim, and tex |

OCaml is retired. `migration-audit.md` records coverage, intentional differences,
and recovery from Git history. The simulator is not a certified implementation
and is not identical to Lean; see the audit and `lean/mul-div-typing.md`.

## Commands

- `./run.sh [LEAN OPTIONS] FILE.det`: build and run Lean. `--samples N --seed S` samples both programs.
- `./run.sh --result PREFIX --subject source FILE.det`: exact expected-reward certificate.
- `(cd lean && lake env lean PREFIX.result.lean)`: independently check the exported theorem.
- `STORM_PYTHON=/path/to/python ./run.sh --storm FILE.det --prefix PREFIX`: exact Storm comparison; install `tools/storm-requirements.txt` into that Python environment first.
- `./det.sh --all`: full Lean/corpus/certificate tests. Set `STORM_PYTHON` to include real Storm tests.
- `(cd sim && npm ci && npm test && npm run build)`: simulator checks and committed bundle.
- `(cd tex && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex)`: paper build.
- `.claude/scripts/check.sh --all`: run all areas sequentially. `--changed` selects affected areas.
- `(cd lean && lake exe cache get)`: fetch Mathlib's prebuilt cache once; do not build Mathlib from source.
- `nix flake show`, `nix flake check`: evaluate the shells when Nix is installed.

Use installed tools directly, or `direnv exec lean`, `direnv exec sim`,
`direnv exec tex`, or `nix develop .#NAME --command ...` when needed. Do not assume
a particular tool is on PATH. The Lean shell includes Python 3 and uv.

## Workflow

- Keep Lean warning-free, without `sorry`, and with only standard axioms. Run
  `lake build --wfail`; `check.sh lean` audits the printed theorem axiom reports.
- After semantic changes, compare Lean, the paper, and simulator. Record actual
  differences rather than claiming automatic parity. See the `sync-sim` skill.
- Preserve the reviewed specification and theorem premises; inference, exploration,
  and solving supply evidence to proved checkers.
- Register every new `.det` file under tests/examples in `tests/cases.toml`.
  Use analytical expectations, not regenerated random-output baselines.
- Do not hand-edit generated certificates/model files, `sim/app.bundle.js`,
  lockfiles, or vendored ACM files. Rebuild the simulator bundle after source edits
  and bump its cache-buster in `sim/index.html` when the bundle changes.
- Match surrounding code style; avoid unrelated formatting.
- Do not run `sim/deploy-to-website.sh` during local checks. Commit or push only
  when authorized; this migration is organized into local commits awaiting review.
- Preserve pinned dependencies. New dependencies need documented setup and checks.
- Update `TODO.md` and `migration-plan.md` as work completes.
- Hooks run fast checks after edits and check changed areas at Stop. Their output
  is feedback to investigate, not permission to weaken checks.

## Boundaries

A model certificate covers unbounded execution, including rejection and divergence.
A result certificate additionally checks absorption and proves an exact unnormalized
expected terminal reward. The default subject is determinized; a source claim
requires either a source model or the determinization theorem's premises.
The Float sampler and simulator are unverified. `.result.lean` is the independent
kernel-checkable artifact; `.result.json` and Storm logs are reports.

The simulator's examples are copied strings in `sim/src/examples.js`; tests run
all of them. The coupling migration tests additionally read shared corpus files.
`examples/baselines/*.sgcl` belong to another tool, and
`examples/loops/example.det` is an explicitly excluded informal sketch.
