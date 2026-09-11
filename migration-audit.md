# OCaml retirement audit

The maintained implementation is now Lean. No OCaml toolchain is needed by the
CLI, test suite, Storm integration, simulator, or paper build.

## Coverage and behavioral changes

| Previous workflow or feature | Replacement and coverage |
|---|---|
| Parse, infer, annotate, determinize | `Frontend/`, `Checking/`, and the formal core transform; parser/inference/checker tests plus the shared corpus |
| Functions, recursion, pairs, sums, lists, arithmetic, comparisons | Original programs retained in `tests/execution/legacy/`, with explicit typing/execution expectations; exact machine tests include captured closures and recursion |
| Uniform, Gaussian, exponential, gamma, beta, Poisson | Lean numerical samplers and analytical statistical tests; exact mean execution after determinization |
| Flip, Bernoulli, finite discrete distributions | Checked finite laws, E/G support, exact export and result tests; `flip` produces a Boolean and retains G sampling |
| Observations | Explicit rejection in the runtime and zero output mass in the semantics; rejection tests and exact expected reward of `observe(flip(0.5)); 3` = `3/2` |
| Sampled source/target means | `./run.sh --samples N --seed S FILE.det`; reports returned-value means, rejection counts, and failures explicitly |
| `.dout` regression files | Analytical expectations in `tests/cases.toml`; no automatic report files or byte-for-byte matching of random samples |
| Storm export | `./run.sh --export PREFIX FILE.det`, using exact rationals and checked paper correspondence |
| Storm results | `./run.sh --storm FILE.det --prefix PREFIX`; exact rational Storm comparison and independently checked Lean theorem |
| Exploration limits | `--max-states`, `--max-edges`, `--max-state-bytes`; incomplete exploration fails without emitting a truncated model |
| Boolean terminal rewards | Encode the reward explicitly: `if condition then 1 else 0`; the exact exporter requires numeric terminals |
| Nonterminating finite graphs | Model certificates cover divergence. Result certificates currently require uniform absorption and reject nonabsorbing graphs |
| Golden test runner | `./det.sh` delegates to `./lean/test.sh`; `--all` includes statistical tests. `STORM_PYTHON` enables real Storm comparisons |

Boolean sampling displays Boolean values; encode `if b then 1 else 0` to estimate
a numeric probability. The numerical runtime uses its own seeded RNG and Float arithmetic. It is not
expected to reproduce OCaml's seed-to-sample sequence. Exact execution uses rational
arithmetic, including decimal literals, and follows Lean's total division `x / 0 = 0`.
Distribution domain failures are explicit. Discrete literal probabilities must be nonnegative and sum to one exactly.
Typing uses silent structural subtyping and the current Lean multiplication rule;
see `lean/mul-div-typing.md` for its relationship to the paper and simulator.

The old `--limit` truncation flag and automatic `.dout` output are retired.
The Storm wrapper requires an explicit output prefix and uses the fixed expected
terminal reward query; it is not a general PRISM property CLI. The formal result
concerns the exported core program; parsing/desugaring, the numerical sampler,
and the browser simulator remain outside that theorem's trust boundary.

## Symbolic-coupling prototype

The standalone `ocaml/symbolic_coupling.ml` CLI was outside the Dune build. It
compared finite-fuel symbolic measure representations for source/target pairs.
That textual experiment and its supplied-target CLI are retired. We do not claim
that a sampled simulator trace replaces equality of measures.

The maintained replacements serve two distinct purposes:

- Lean's symbolic/trace soundness proofs establish the formal relationships.
- `sim/` visualizes seeded ordinary, symbolic, and determinized traces.
  `sim/test/coupling-migration.test.js` checks sampled/mean projections and every
  synchronized frame for the prototype's three motivating examples.

Those examples are shared `.det` files: `tests/statistical/nested.det`,
`coupling-branch.det`, and `coupling-mixed.det`. The Lean corpus checks their
source/target analytical moments, and the two new fixtures also export typing
certificates. The simulator generates its own determinized program rather than
accepting a separately supplied target program.

The simulator retains its existing typing and distribution behavior. In particular,
its multiplication convention uses a G right operand, whereas Lean uses a G left
operand; Lean's frontend reorders eligible products. Simulator support is not a
substitute for a Lean typing or result certificate.

## Recovery

Before removal, every tracked OCaml file and all 25 archived output/model files
were compared byte-for-byte with commit
`a0c8167b3a0edd2a57707052e4d2618549556b15`. All matched. Source files are recoverable
under that commit's `ocaml/`; archived output files are under its `det/`. The 31 generated reports/model files
removed from `examples/` were also checked against that commit and are recoverable
at their original paths. The
original `.det` programs remain unchanged in the shared corpus. No unique source
or baseline data was discarded.

## Validation

Run `STORM_PYTHON=/path/to/python .claude/scripts/check.sh --all` for the Lean,
corpus, Storm, simulator, bundle, and paper workflows. Python must have the pinned
`tools/storm-requirements.txt` installed. Local checks do not deploy or push.

The retirement run passed all of those workflows: 112 corpus cases (21 statistical),
20 typing certificates, model/result certificate regressions, eight real Storm
comparisons, two wrapper tests, and 51 simulator tests. The regenerated bundle was
unchanged. The paper builds with its pre-existing overfull-box and duplicate-label
warnings. Shell syntax and JSON configuration checks pass. Nix is absent on this
host, so the updated devshells have not been evaluated locally.


## Integration with main (2026-09-11)

The migration is rebased onto `b3995d4`. Upstream's nine theorem statements and
assertions are retained, including replay soundness, output mass, variance, and
conditional expectations. Their proofs use silent subtyping and the migration's
rejection representation. The frontend adopts upstream's unit-sum discrete domain;
checked rational probabilities remain the internal representation.

The four new upstream `.det` programs (`beta-shape`, `div`, `flip-prob`, and
`gauss-variance`) are preserved byte-for-byte in the shared corpus and checked by
independent typing certificates. Their OCaml output archives are retired with the
other generated reports. Upstream's theorem-assertion coverage check, paper typing
updates, and simulator updates are retained.
