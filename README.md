# Determinize

A probabilistic language with E/G sampling modes, a Lean implementation and
formalization, exact expected-reward certificates, and a browser simulator.

[![Project page](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/julesjacobs/determinize/status/site.json)](https://julesjacobs.github.io/determinize/)
[![Simulator](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/julesjacobs/determinize/status/sim.json)](https://julesjacobs.github.io/determinize/sim/)
[![API documentation](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/julesjacobs/determinize/status/docs.json)](https://julesjacobs.github.io/determinize/docs/)
[![Paper (PDF)](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/julesjacobs/determinize/status/paper.json)](https://julesjacobs.github.io/determinize/determinize.pdf)
[![Anonymized artifact (ZIP)](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/julesjacobs/determinize/status/artifact.json)](https://julesjacobs.github.io/determinize/determinize-artifact.zip)

- `lean/`: parser, verified affinity inference, determinization, numerical execution,
  exact finite-state exploration, and kernel-checkable certificates.
- `tests/`, `examples/`: shared `.det` programs and analytical expectations.
- `tools/storm.py`: exact Storm comparison against independently checked certificates.
- `sim/`: browser visualization of ordinary, symbolic, and determinized execution.
- `tex/`: paper sources and figures.
- `artifact/`: the anonymized archive for the paper's double-blind review.

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
types, tests and build, the site in a browser, and the paper's Lean links and build. Select individual areas with
`./check.sh lean tex`, or use `./check.sh --changed` for areas affected by uncommitted
changes. The scripts use tools on `PATH` first; Nix is optional.

On GitHub, each workflow runs for the pull requests that change a file it reads.
`.github/workflows/lean.yml` runs `lake build --wfail` and `./test.sh --all` in the `.#lean`
shell, the comparisons with Storm included. `.github/workflows/sim.yml` runs `biome ci`, the type check and the simulator tests in
the `.#sim` shell, builds the simulator into `sim/dist/` and attaches it to the run as the
`simulator` artifact. `.github/workflows/tex.yml` checks the paper's Lean links
(`tex/check-lean-links.py`), builds the paper in the `.#tex` shell, fails on unresolved
references and citations, and attaches `main.pdf` as the `paper` artifact.
`.github/workflows/site.yml` runs `./check.sh site` in the `.#site` shell.
`.github/workflows/artifact.yml` builds the anonymized archive (see [below](#anonymized-artifact))
with the API documentation that `main` rendered last, checks it unpacked, and attaches it to the
run as the `determinize-artifact.zip` artifact, the zip itself. For `main`,
`.github/workflows/pages.yml` runs these workflows, but skips Lean's tests when their inputs
passed them before and reuses the API documentation when the same sources were rendered
before. It then assembles the [project page](https://julesjacobs.github.io/determinize/), the
[simulator](https://julesjacobs.github.io/determinize/sim/), the
[API documentation](https://julesjacobs.github.io/determinize/docs/), the paper
(`determinize.pdf`) and the [anonymized artifact](https://julesjacobs.github.io/determinize/determinize-artifact.zip)
with `site/assemble.sh`, checks the links of the landing page and the simulator against the
result, and publishes it to GitHub Pages. `site/status.py` keeps the chips at the top of this file
in the branch `status`. As a run starts, each part that changed since the site was published last
becomes building. At the run's end, also after a failure, a part is failing when its build or
check failed, timed out or was cancelled, outdated when it changed but a failure elsewhere kept
the site from being published, as each deployment replaces the whole site, and up to date
otherwise. A run waits for the one before it to finish, and until it starts, the chips describe
the earlier one. Through shields.io, a chip reaches the README within about five minutes.

An E draw is replaced by its distribution's mean; a G draw remains stochastic.
Finite-model certificates prove the selected core program's integrability and
exact expected terminal reward. Rejection contributes zero, without conditioning.
The default subject is `determinized`; use `--subject source` for a source model.
Transferring a determinized answer to the source requires the determinization
theorem's premises. See [the contract](lean/finite-model-contract.md).

## Anonymized artifact

`artifact/build.py` builds `determinize-artifact.zip`, the supplementary material for the paper's
double-blind review, from the tracked files that its `SHIPPED` list selects, the built simulator
and the API documentation:

```sh
(cd sim && npm run build)
artifact/build.py --out determinize-artifact.zip --docs lean/docbuild/.lake/build/doc
```

The archive's `README.md` is `artifact/README.md`. Its `website/` opens from disk: the landing
page, the simulator, the documentation, and every shipped file as a page with line numbers, which
the documentation's source links point to. Links to the repository and to the published site
become links into the archive; the landing page loses the metadata and footer that name them,
the simulator its source maps, recorded paths their home directory, and `lean/README.md` its
comparison with drafts of the paper. The script then fails if any file names a pattern of
`artifact/identifying.txt` (the authors, their accounts and institutions), a commit of this
repository or a home directory, if the website loads from other hosts than the documentation's
MathJax CDN, or if a link of the website or of a Markdown file leaves the archive.
`artifact/build.py --check DIR` checks an unpacked archive the same way. The script refuses
documentation rendered from other Lean sources unless given `--allow-stale-docs`, which the pull
requests' runs use. `sim/e2e/artifact.spec.ts` opens the unpacked archive from disk in a browser
(`ARTIFACT_DIR=… playwright test e2e/artifact.spec.ts` in `sim/`). `./check.sh artifact` does all
of this in `_artifact/`, with the documentation that `lake build Determinize:docs` in
`lean/docbuild` rendered last.

## Storm

On Linux, the `.#lean` shell sets `STORM_PYTHON` to a Python with stormpy, the version in
`tools/storm-requirements.txt`, so `./test.sh --all` includes the comparisons with Storm, in CI
too:

```sh
./run.sh --storm tests/statistical/discrete.det --prefix /tmp/model --subject source
./test.sh --all
```

Elsewhere, install stormpy and point `STORM_PYTHON` at it:

```sh
uv venv --python 3.12 /tmp/determinize-storm
uv pip install --python /tmp/determinize-storm/bin/python -r tools/storm-requirements.txt
export STORM_PYTHON=/tmp/determinize-storm/bin/python
```

Storm uses exact rational matrices. The wrapper checks `.result.lean` with Lean's
kernel and requires exact agreement with Storm. It records versions, commands,
status, and failures in `.storm.json`. Without `STORM_PYTHON`, the ordinary test
suite explicitly skips real Storm comparisons; the Lean certificate tests still run.

`--result`, `--export` and `--storm` extract pending outer additions as transition
rewards, so that loops which accumulate a sum have finitely many machine states
(`--no-additive` turns this off):

```sh
./run.sh --check --result /tmp/geometric --subject source \
  examples/loops/geometric-addition.det
./run.sh --storm examples/loops/geometric-addition.det --prefix /tmp/geometric \
  --subject source --compare
```

The geometric example has 45 states, mean 1 and variance 2. Computed or random
left operands work once evaluated. Only outer additions are extracted;
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
`src/` against the browser's types (`tsconfig.json`), and the tests, the browser tests, the
scripts, the build script and the Playwright configuration against Node's (`tsconfig.node.json`).

`sim/src/core/` holds the compiler, the runtime and what the step table and the statistics
compute. A Biome override keeps it free of the DOM, CodeMirror and signals, so that Node's tests
and the sampling worker run it. The page (`sim/src/ui/`) keeps its state in signals of
`@preact/signals-core`. It samples in a worker (`sim/src/worker.ts`), computes the step
table in another (`sim/src/trace-worker.ts`), which keeps the run and sends it a page at a time,
and explores both programs' finite models in a third (`sim/src/exact-worker.ts`); where no worker
can start, as when `sim/dist/index.html` is opened from disk, it does all three on its own thread,
sampling and exploring in slices of about 50 ms. The exact worker also contracts each complete
model's probability-1 steps (`sim/src/core/finite/chain.ts`), and a fourth worker
(`sim/src/chain-worker.ts`, with Graphviz from `@viz-js/viz`), loaded only when a reader opens
the chains, lays them out; from disk, the same bundle runs as a script on the page. Sampling,
which starts on load and after each edit, evaluates with ports of Lean's evaluator and samplers,
SplitMix64 included
(`sim/src/core/runtime/eval.ts` and `sampling.ts`, after `Runtime/Eval.lean` and
`Runtime/Sampling.lean`): run i of a program and of its determinization starts at seed s + i, so
its statistics are those of `./run.sh --seed s --samples N`. `sim/test/fixtures/lean-cli.json`
records what the Lean CLI prints for every case of `tests/cases.toml`, and the tests compare the
ports with it; `node scripts/lean-fixtures.ts` in `sim/` records it again. The step table follows
run 0 for at most 20000 symbolic steps, in pages of at most 200 steps.

The `.#sim` shell, which direnv loads in `sim/`, links `sim/node_modules` to packages that
Nix builds from `sim/package-lock.json`; the combined shell does not. To add or update a
dependency, run `npm install <pkg>@<version>` in `sim/`, which changes only `package.json`
and `package-lock.json` because `sim/.npmrc` sets `package-lock-only`, and reload the
shell. A `sim/node_modules` left by an earlier `npm ci` blocks the links: remove it once
with `rm -rf sim/node_modules`. Without Nix, `npm ci --package-lock-only=false` installs
the packages.

The project page is `site/`: a landing page that works without JavaScript, `404.html`, and
`site/assemble.sh`, which puts it together with the built simulator under `sim/`, the API
documentation under `docs/` and the paper as `determinize.pdf`, as GitHub Pages serves them.
`site/tokens.css` holds the colours, type and spacing of the landing page and the simulator,
whose build bundles it with the fonts of `site/fonts/` into `sim/dist/`.
`./check.sh site` runs in the `.#site` shell, which adds nixpkgs' headless Chromium and lychee
to the simulator's shell and has no `.envrc`. It builds the simulator, assembles the site into
`_preview/determinize`, and runs the Playwright tests in `sim/e2e/` (for the landing page axe,
JavaScript disabled, 390 px, other origins and size; for the simulator every example, the worker
and `file://`, long tasks, links and axe), html-validate and lychee. To look at that preview, run
`esbuild --servedir=_preview` in the shell and open `http://127.0.0.1:8000/determinize/`.
`node site/figures.mts` draws the landing page's figures from runs of the simulator's runtime,
and `node site/theorems.mts` quotes the theorems from `lean/Determinize/Theorems.lean`, with links
to the documentation of the names that its modules outside `Proof/` declare; `check.sh site` compares
the quotes with the page. `node site/social/render.mts` renders the link-preview
image `site/social.png` from `site/social/card.html`.

The paper starts at `tex/main.tex`, with one file per section in `tex/sections/`,
figures in `tex/figures/`, and the appendix in `tex/sections/9_appendix.tex`.
[Open issues](OPEN_ISSUES.md) records unresolved paper work and deferred implementation.
[Paper deviations](lean/PAPER_DEVIATIONS.md) records the paper–Lean comparison.

After `npm run build` in `sim/`, open `sim/dist/index.html`, or
[its published copy](https://julesjacobs.github.io/determinize/sim/), to follow a run of a
program and of its determinization step by step, beside the G draws they share and the symbolic
semantics of the paper's proof, and to compare the output distributions of many runs of both
programs, with the exact values of each one that has a finite model and its Markov chain, drawn
with its probability-1 steps contracted: a source branches at its E and its G draws, its
determinized program only at the G draws. The address's fragment holds the program, the seed,
the example and how the page shows them, so a copied link opens the same run. The simulator's
front end, runtime and finite models are ports of Lean's, held to the Lean CLI by the fixtures
in `sim/test/fixtures/`; Lean's certificates and theorems apply to the core programs that the
Lean CLI produces.

See the [Lean documentation](lean/README.md) for the language, theorem premises,
and implementation limits, and the [test guide](tests/README.md) for analytical
expectations and certificate checks. No OCaml toolchain is required.
