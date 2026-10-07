# Determinize

A probabilistic language with E/G sampling modes, a Lean implementation and
formalization, exact expected-reward certificates, and a browser simulator.

[Simulator](https://julesjacobs.github.io/determinize/) ·
[API documentation](https://julesjacobs.github.io/determinize/docs/)

- `lean/`: parser, verified affinity inference, determinization, numerical execution,
  exact finite-state exploration, and kernel-checkable certificates.
- `tests/`, `examples/`: shared `.det` programs and analytical expectations.
- `tools/storm.py`: exact Storm comparison against independently checked certificates.
- `sim/`: browser visualization of ordinary, symbolic, and determinized execution.
- `tex/`: new paper; `tex/archive/` preserves the previous draft.

The theorem statements and the definitions they use can be read as
[API documentation](https://julesjacobs.github.io/determinize/docs/Determinize/Theorems.html),
where every name links to its definition.

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
./run.sh tests/execution/arith.det
./run.sh --samples 20000 --seed 1 tests/statistical/gaussian.det
./run.sh --result /tmp/model --subject source tests/statistical/discrete.det
(cd lean && lake env lean /tmp/model.result.lean)
./test.sh --all
./check.sh --all
```

`run.sh` builds and invokes the Lean CLI; arguments and relative paths are passed
through. `test.sh` runs the full Lean test entry point, including the shared corpus
and independent certificates. It accepts `--all` and `--statistical`.
`check.sh --all` additionally checks theorem axioms, the simulator's formatting, lint,
types, tests and build, the site in a browser, and the paper build. Select individual areas with
`./check.sh lean tex`, or use `./check.sh --changed` for areas affected by uncommitted
changes. The scripts use tools on `PATH` first; Nix is optional.

On GitHub, `.github/workflows/lean.yml` runs `lake build --wfail` and `./test.sh --all`
in the `.#lean` shell, and `.github/workflows/sim.yml` runs `biome ci`, the type check and
the simulator tests in the `.#sim` shell, builds the simulator into `sim/dist/` and
attaches it to the run as the `simulator` artifact. Both run for pull requests. For `main`,
`.github/workflows/pages.yml` runs both and then publishes that build of the
[simulator](https://julesjacobs.github.io/determinize/) to GitHub Pages, with the
[API documentation](https://julesjacobs.github.io/determinize/docs/) under `docs/`.
`.github/workflows/tex.yml` builds the paper in the `.#tex` shell for pull requests and for
`main`, fails on unresolved references and citations, and attaches `main.pdf` to the run as
the `paper` artifact.

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
STORM_PYTHON=/tmp/determinize-storm/bin/python ./test.sh --all
```

Storm uses exact rational matrices. The wrapper checks `.result.lean` with Lean's
kernel and requires exact agreement with Storm. It records versions, commands,
status, and failures in `.storm.json`. Without `STORM_PYTHON`, the ordinary test
suite explicitly skips real Storm comparisons; the Lean certificate tests still run.

Use `--additive` with `--result`, `--export`, or `--storm` for loops whose
pending outer additions would otherwise produce infinitely many machine states:

```sh
./run.sh --check --additive --result /tmp/geometric --subject source \
  examples/loops/geometric-addition.det
STORM_PYTHON=/tmp/determinize-storm/bin/python ./run.sh --storm \
  examples/loops/geometric-addition.det --additive --prefix /tmp/geometric --subject source --compare
```

The geometric example has 45 states, mean 1 and variance 2. Computed or random
left operands work once evaluated. This mode extracts only outer additions;
`f()+1`, scaling around a recursive call, and growing environment accumulators
can still exceed the exploration limit. See [the additive contract](lean/finite-model-contract.md#additive-output-models).

Evaluation examples with unbounded recursion:

| Example | Return mass | First moment | Second moment | Conditional variance |
| --- | ---: | ---: | ---: | ---: |
| [Geometric addition](examples/loops/geometric-addition.det) | 1 | 1 | 3 | 2 |
| [Random increments](examples/loops/geometric-random-increment.det) | 1 | 2 | 13 | 9 |
| [Rejection](examples/loops/geometric-rejection.det) | 1/2 | 1/2 | 3/2 | 2 |

## Simulator and paper

```sh
(cd sim && biome ci . && npm run typecheck && npm test && npm run build)
(cd tex && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex)
```

Biome formats and lints the simulator as configured in `sim/biome.json`;
`biome check --write .` in `sim/` applies its formatting and safe fixes. The simulator is
written in TypeScript, whose types Node and esbuild strip. `npm run typecheck` checks
`src/` against the browser's types (`tsconfig.json`), and the tests and the build script
against Node's (`tsconfig.node.json`).

The `.#sim` shell, which direnv loads in `sim/`, links `sim/node_modules` to packages that
Nix builds from `sim/package-lock.json`; the combined shell does not. To add or update a
dependency, run `npm install <pkg>@<version>` in `sim/`, which changes only `package.json`
and `package-lock.json` because `sim/.npmrc` sets `package-lock-only`, and reload the
shell. A `sim/node_modules` left by an earlier `npm ci` blocks the links: remove it once
with `rm -rf sim/node_modules`. Without Nix, `npm ci --package-lock-only=false` installs
the packages.

The project page is `site/`: a landing page without JavaScript, `404.html`, and
`site/assemble.sh`, which puts it together with the built simulator under `sim/`, the API
documentation under `docs/` and the paper as `determinize.pdf`, as GitHub Pages serves them.
`./check.sh site` runs in the `.#site` shell, which adds nixpkgs' headless Chromium and lychee
to the simulator's shell and has no `.envrc`. It builds the simulator, assembles the site into
`_preview/determinize`, and runs the Playwright tests in `sim/e2e/` (axe, JavaScript disabled,
390 px, other origins, size), html-validate and lychee. To look at that preview, run
`esbuild --servedir=_preview` in the shell and open `http://127.0.0.1:8000/determinize/`.
`node site/figures.mts` draws the landing page's figures from runs of the simulator's runtime,
and `node site/theorems.mts` quotes the theorems from `lean/Determinize/Theorems.lean`, which
`check.sh site` compares with the page. `node site/social/render.mts` renders the link-preview
image `site/social.png` from `site/social/card.html`.

The new paper starts at `tex/main.tex`, with one file per section in `tex/sections/`,
formal figures in `tex/figures/`, and supporting material in `tex/appendix/`, following
[the paper structure](PAPER_STRUCTURE.md) and [figure plan](tex/FIGURE_PLAN.md).
Add text, notation, and references as they are reviewed; the previous draft is
reference material in `tex/archive/`.
To build that archived draft independently:

```sh
(cd tex/archive && latexmk -pdf -interaction=nonstopmode -halt-on-error main.tex)
```

After `npm run build` in `sim/`, open `sim/dist/index.html`, or
[its published copy](https://julesjacobs.github.io/determinize/), to explore coupled
traces. The simulator has its own compiler and numerical runtime; Lean certificates
apply to the core programs produced by the Lean CLI. The simulator is an unverified
visualization.

See the [Lean documentation](lean/README.md) for the language, theorem premises,
and implementation limits, and the [test guide](tests/README.md) for analytical
expectations and certificate checks. No OCaml toolchain is required.
