Review these three entry points and the definitions they import:

- `Determinize/Spec/Main.lean` defines the expectation-preservation propositions directly: `mainThm` (finite expectations), `extendedExpectationThm` (expectations in the extended reals, infinite values included), `jensenThm` (Jensen's inequality between the two output laws), `outputMassThm` (equal output mass), `varianceThm` (non-increasing second moment and variance) and `conditionalExpectationThm` (equal expectations conditioned on acceptance, the statement behind `observe`). `Spec` contains ordinary syntax, typing, primitive distributions and means, determinization, and semantics.
- `Determinize/Spec/Traces/Main.lean` defines trace erasure (`correspondenceThm`), trace soundness (`soundnessThm`) and the law of total variance along traces (`Spec.Traces.varianceThm`). `Spec/Traces/Semantics.lean` defines the operational traces, the joint law `traceAndOutputLaw` of a program's trace and output, and `outputGivenTrace`, the program replayed along a trace.
- `Determinize/Theorems.lean` proves all nine propositions without additional hypotheses and prints their axioms.

Run `lake build --wfail` from this directory; the build is warning-free and contains no `sorry`. Check that all nine axiom reports contain only `propext`, `Classical.choice`, and `Quot.sound` (`.claude/scripts/check.sh lean` performs both checks). With Lean's kernel and these standard axioms trusted, reviewers can omit the proof bodies in `Proof`. `Spec` contains the specification; any proof imports there supply proof-irrelevant evidence. `Spec/Traces` imports no proof modules.

The typed determinization theorems assume `PrimitiveDomainSafe`: every distribution call reached at a finite execution depth has valid parameters almost surely. Typing supplies structural progress; it does not prove argument bounds. The expectation and trace theorems also establish target domain safety. The proof derives full non-stuckness from typing and this premise. Finite replay certificates do not require typing, so their separate contract retains full `DoesNotGetStuck` in `Spec/FiniteModel/Safety.lean`.

Source expressions need not be in ANF. Each primitive distribution is its own constructor with the paper's operands (`uniform action lower upper`, `gaussian action mean variance`, and so on). `DistributionAction` is either `sample affinity` or `mean`, where `Affinity` is E or G. Mean expressions carry no affinity annotation; their result affinity follows the operand typing rules. Operands may contain nested sampling and are evaluated left to right; a mean site also evaluates every operand exactly once, including a Gaussian's variance. `Expr.sourceForm` excludes mean sites. `Spec/Primitives.lean` gives each primitive one fiber: its law at a stochastic site, the Dirac mass at its mean at a mean site, and the zero measure outside the parameter domain. Expressions have no type annotations; `Typed` assigns types separately. Expressions carry E/G labels only on sample sites; literals and arithmetic are unannotated and a literal types at either affinity, as in the paper's `FloatLit`; subtyping is silent; variables are de Bruijn indices. `Typed` enforces the affinity restrictions: E multiplication requires a G left operand, division a G denominator, and comparisons G operands. Arithmetic uses real numbers, with `x / 0 = 0`. The theorems quantify over closed float programs of either affinity; the proof uses subsumption to assign an E result type to the same program.

Output laws are defined directly by recursion over reduction depth. Deterministic actions continue evaluation; sampling actions integrate the continuation over the primitive measure on reals. Expressions of every type may occur during evaluation, but only terminal reals contribute output. Neither evaluator requires a measurable structure on expressions. `Proof` introduces one internally to establish measurability of the evaluators.

A trace is a list of `(primitive, value)` pairs recording only stochastic G draws. Deterministic steps and E draws add no entry. Trace soundness factors the actual joint trace/output measures over `traceLaw source`, the trace marginal of the source joint law: the source output is a Markov kernel indexed by the trace, and the target output is a measurable function of the trace. For almost every trace, the source fiber is an integrable probability measure and the target output equals its mean. The target therefore has the same trace law and the same termination probability as the source. No global integrability is required. Detailed traces with one entry per reduction step live in `Proof/Internal/StepTraces.lean`; `Proof/CompactTrace.lean` proves that replaying a determinized program's G draws reconstructs those detailed traces almost surely.

The output-mass theorem preserves acceptance/termination mass; dividing the preserved integral by that mass gives the conditional-expectation theorem. The variance theorems bound the target variance and decompose source variance along replay traces. The finite expectation theorem adds source integrability and proves target integrability and equal integrals. The extended-real theorem only assumes that one of `∫ v⁺` and `∫ v⁻` under the source output law is finite, and concludes the same for the target and equal extended-real expectations. Jensen's inequality bounds `∫ φ` under the target output law by `∫ φ` under the source output law for every nonnegative convex `φ : ℝ → ℝ`. Both corollaries are derived from trace soundness in `Proof/Corollaries.lean`. Output measures are unnormalized: divergence contributes no output mass, and expectations are not conditioned on termination.

The default build checks nested sampling, the `x + 1/y` example, a G draw scaling an E draw from the left, and a sampled value captured by a function. `Proof/InterfaceChecks.lean` checks the direct evaluator using only public imports and verifies that these imports provide no measurable structure on expressions.

## Deviations from the paper

The statements follow the paper's theorems, not its letter. Reviewers comparing against `tex/` should know:

- **Affinity labels on sample sites.** The paper's transformation `⟦e : τ⟧` is type-directed; here `Expr.determinize` is a function on terms, so every sample site carries its affinity and that label decides whether the site is switched to its mean (`DistributionAction.determinize`). Literals and arithmetic carry no labels.
- **Multiplication and division.** Lean, the paper, and the simulator use a G left operand for multiplication and a G denominator for division. The other operand carries the result affinity. A G factor may depend on the G trace, while the expression remains affine in E draws. Lean allows silent structural subtyping; the frontend also puts a literal scaling factor on the left.

- **Primitive domains.** Sampling outside a primitive's parameter domain (`uniform(a, b)` with `a > b`, a negative Gaussian variance, and so on) yields the zero measure and counts as stuck, and so does a mean site outside the same domain; the paper's mean table is unconditional. `uniform(a, a)` is the Dirac measure at `a`; the paper's table has no such row.
- **Validity hypothesis.** The typed determinization theorems assume `PrimitiveDomainSafe program`: almost surely, at every reduction depth, no off-domain primitive call occurs, E draws included. Typing supplies structural progress. The paper's "valid G-trace" is informal. The hypothesis is necessary: a source that loses mass with positive probability on an off-domain E parameter can have a different expectation than its determinization.
- **Expectations.** `Traces.soundnessThm` needs no integrability hypothesis and proves that almost every fiber is integrable, which the paper assumes as "integrable σ" and states as the open lemma "Mean valuation correctness". `mainThm` covers finite expectations; `extendedExpectationThm` is the paper's extended-real global theorem; `jensenThm` is the paper's global Jensen corollary restricted to real-valued convex functions.

## Lean command-line implementation

Build with `lake build --wfail`, then run from `lean/`:

```sh
.lake/build/bin/determinize ../tests/execution/legacy/foldr.det
.lake/build/bin/determinize --samples 1000 --seed 42 ../tests/execution/legacy/foldr.det
.lake/build/bin/determinize --check --certificate /tmp/Certificate.lean ../tests/execution/legacy/foldr.det
lake env lean /tmp/Certificate.lean
./test.sh
```

The CLI reads the existing `.det` grammar, with optional `[E]` or `[G]` after a
sampling primitive. Unannotated sites are inferred; explicit affinities are constraints.
It prints the annotated source and the result of the existing `Expr.determinize`.
`mean_uniform`, `mean_gauss`, etc. in the output denote atomic mean operations:
they evaluate every operand exactly once and check the primitive domain. They are
output notation, not additional source primitives. `--fuel` bounds each numerical
run; `--samples` defaults to zero, so compilation does not execute the program.

The implementation is separated as follows:

- `Frontend/`: unverified parsing, desugaring/name resolution, constraint inference,
  pretty printing, orchestration, and certificate export.
- `Checking/`: certificate data, a total proof-producing typing checker, and checks
  that inference preserves the elaborated expression and explicit sampling affinities.
- `Proof/Checking/`: checker soundness, rational/real determinization correspondence,
  and application of the existing trace and finite-expectation theorems.
- `Runtime/`: an unverified floating-point interpreter and seeded numerical samplers.
- `Tests/`: parsing, inference, certificate rejection, runtime, and kernel proof tests.
- `Main.lean`: the CLI.

`Spec`, `Spec/Traces`, and the existing soundness proofs do not import the front end
or runtime. The CLI uses the formalization's syntax and determinization, generalized
over literal types. Decimal input is parsed exactly as `Rat`; the mathematical
interpretation embeds each rational into `ℝ`. A proved commuting equation connects
rational determinization to the existing real-literal theorem.

### What is checked

`Frontend.Surface` represents parsed syntax with named binders and dedicated
constructors. Elaboration resolves names to de Bruijn indices and removes syntax
sugar, producing `Checking.Input`. Its sample nodes retain optional E/G affinities;
it has no mean nodes. Inference fills these annotations and produces `Core` plus
a tree of proposed types. There is no positional annotation list.
`check` verifies every node against the existing `Typed` constructors and returns
an actual proof in `PLift`. It does not use `unsafe`, `sorry`, or inference as an
oracle. `certify` additionally requires stochastic source form and structural
correspondence between the resolved input and the inferred core. It checks each
constructor, payload, and optional sampling affinity at the same AST node. Thus inference cannot silently change literals,
operators,
binders, or distribution kinds. The certificate identifies the **resolved input
expression**; parsing, name resolution, and desugaring from source bytes remain
outside the checked boundary.

The executable runs the verified checker as compiled Lean code. An exported
`.lean` certificate independently reconstructs the checks using kernel reduction
(`by decide +kernel`, not `native_decide`); it does not import the inference algorithm.
For a float-valued program it includes `traceGuarantee`, conditional on
`PrimitiveDomainSafe`. The generic `certified_expectation` theorem additionally requires
integrability. Typing alone proves neither hypothesis. Non-float programs receive
typing and input-preservation certificates without a float-output theorem.

### Surface extensions and inference limits

Subtraction lowers to addition and negation. `<=` evaluates both operands once,
in source order, and negates the reversed strict comparison. Multiplication by a
right-hand numeric literal is rearranged to use the core's left-G multiplication
rule. These transformations belong to the unverified desugaring stage.

`bernoulli[E](p)` and `bernoulli[G](p)` are core numeric draws with outcomes 0 and 1.
The probability is evaluated once and must be in `[0,1]`; an invalid probability
has zero mass in the formal semantics and raises a runtime error. E-affinity
probabilities may depend affinely on E values; G-affinity probabilities must have G
type. Typing does not establish domain safety. Determinization changes an E draw
to a mean site with the same probability expression. `flip(p)` lowers to a G-affinity
Bernoulli comparison and therefore returns a Boolean; `flip[E]` is rejected.

`discrete[E](w0,...,wn)` and `discrete[G](w0,...,wn)` require nonnegative literal
rational probabilities whose exact sum is one, matching the domain on `main`. Core terms store the checked probabilities
for numeric outcomes `0,...,n`, including zero-weight positions. Determinization
changes an E draw to a mean site with the same distribution. Its result is the
weighted outcome index. Omitted affinities use ordinary affinity inference. Pretty printing
prints probabilities directly, preserving the unit-sum domain when reparsed.

`observe(c)` lowers to `if c then () else reject`. The explicit core rejection term
has zero output mass: formally it is an absorbing non-value, as proved in
`Proof/Rejection.lean`. The numerical runtime returns a distinct rejection outcome
immediately; ordinary divergence still exhausts fuel. Conditions are evaluated once,
and rejected executions do not evaluate their continuation. The CLI reports rejected
observations separately from execution failures. `conditionalExpectationPreservation`
proves equality of the normalized expectations under its stated premises;
empirical means use returned values and differ from unnormalized expectations.

Inference is monomorphic, uses an occurs check and structural subtyping constraints,
and defaults unconstrained affinities to E and unused type variables to `unit`.
Products, sums, and lists are covariant; function arguments are contravariant and
results covariant. `Float[G]` is a subtype of `Float[E]`; the reverse is not allowed.
Using a G draw at type E leaves its sample annotation G, so determinization still
retains the draw. Subsumption appears in certificates, never in the expression.
The checker validates every subsumption step against `Ty.Sub`. Inference
completeness and optimality are not claimed; some valid programs can be rejected
depending on conditional branch order.

The shared corpus and analytical expectations live in [`../tests/`](../tests/README.md).
Run `./test.sh` for unit tests, all corpus compilation/typing checks, exact execution
checks, and exported kernel certificates. Run `./test.sh --statistical` for sampled
source/target moment checks, or `./test.sh --all` for both. Python 3.11+ reads TOML;
the compiled Lean test runner performs all language evaluation and assertions.

### Numerical runtime

The runtime evaluates the checked core, with closures, recursive functions, sums,
lists, and left-to-right evaluation. It uses SplitMix64 with separate E/G streams,
Box–Muller Gaussian draws, Marsaglia–Tsang gamma draws, gamma-ratio beta draws,
inverse-CDF exponential draws, and sums of bounded-rate Poisson draws. Identical seeds
replay identical runs. These algorithms, Lean's compiler, floating-point rounding,
and the PRNG are not covered by the measure-theoretic soundness proof.

Primitive domains follow the formalization: reversed uniform bounds fail instead of
being swapped; zero-variance Gaussians and point uniforms are allowed; division by
zero yields zero. Nonfinite arithmetic and sampling results fail explicitly. Gamma
rejection and Poisson iterations are bounded, and Poisson rates above 1,000,000 are
rejected by this numerical runtime. Failed and exhausted runs are reported.

Statistical tests cover representative parameters, not the entire numerical range.
Overflow and underflow in intermediate calculations can distort finite outputs:
for example, `beta[E](1e308, 1e308)` currently produces a numerical mean of zero
instead of one half. The exact rational evaluator and real-measure theorems do
not use these Float calculations.

### Discrete-distribution migration

`Spec/FiniteDistribution.lean` defines checked rational probabilities and
weighted expectation. `Spec/FiniteDistributionMeasure.lean` and
`Spec/Primitives.lean` define the real finite law, Bernoulli fiber, and mean
fibers. Their probability, integrability, expectation, and Bernoulli variance and
measurability proofs are in the corresponding `Proof/` modules. The shared primitive kernels,
affine-mean laws, moment bounds, and domain-convexity proofs cover both distributions.
Both primitives pass through core typing, symbolic and trace semantics, checked
certificates, and the numerical runtime. The runtime remains unverified.

### Finite-model contract

[finite-model-contract.md](finite-model-contract.md) specifies exact rational
models, one-time terminal rewards, rejection, the initial primitive policy, and
certificates with value equations and finite-step absorption bounds. Definitions
live in `Spec/FiniteModel/`; proofs and theorems composing checker
correctness guarantees live in `Proof/FiniteModel/`. The unverified explorer is implemented in `Finite/`. The verified
model checker is in `Checking/FiniteModel.lean`. `replay_matches` supplies the correspondence proof carried by each extracted `CheckedModel`.
`Checking/Result.lean` proves result-checker soundness and the composed program
expected-reward theorem. `Tests/Results.lean` checks signed rewards, absorption,
and a nonterminating model with spurious equation solutions.


### Exact finite-state export

From `lean/`:

```sh
lake exe determinize --check --export /tmp/model --subject source ../tests/statistical/discrete.det
```

The default subject is `determinized`. Exploration uses exact rational arithmetic
and structural machine states, including closures and continuation stacks.
Stochastic Bernoulli and discrete draws have finite successors; mean draws of
all supported primitives are rational. Residual stochastic continuous and
Poisson draws are rejected. Terminal results must be numeric.

Successful exploration is checked against the executable machine before writing
`.candidate.lean`, `.replay.lean`, `.tra`, `.lab`, `.positive.state.rew`, and
`.negative.state.rew`. The replay checker takes the requested source and subject
separately from the candidate and checks their alignment, exact transitions,
complete positive successor coverage, rewards, and absorbing terminal states.
Acceptance constructs a `CheckedModel` with a `Spec.FiniteModel.Model` and
a proof of paper safety and complete output-law equality. Stored states must be unique.

The `.candidate.lean` file contains raw data. The `.replay.lean` file additionally
contains `machineReplay`, proved by `decide +kernel`, the resulting `model`, and
`modelMatches`, which certifies the selected paper program.
Check it independently with:

```sh
lake env lean /tmp/model.replay.lean
```

These are program-level model certificates: they cover unbounded execution,
rejection, and divergence. Expected-reward certificates additionally require a
uniform absorption bound. The replay checker checks the full transition matrix;
checking large graphs can cost substantially more than exploration.

Limits default to `--max-states 10000 --max-edges 100000
--max-state-bytes 1000000`. A limit or execution failure exits unsuccessfully
without writing exports; existing files at the same path are left untouched.
The byte limit measures each serialized machine state after construction, not
process memory. Structural exploration can find finite cycles, but does not
abstract infinite state spaces. Full captured environments may distinguish
states with equivalent future behavior.

`Tests/Explorer.lean` checks exact rewards, recursion, cycles, limits, and errors.
`tests/test_export.py` independently solves the emitted rational transition
equations for known answers, recompiles generated data, and kernel-checks
replay certificates (including rejection of changed initial states).
Both run through `./test.sh`.


The semantic bridge in `Proof/FiniteModel/` now reifies closures and continuation
stacks into paper expressions. It proves the binding equations, deterministic
root reductions, bookkeeping equalities, and contextual rejection laws.
The replay checker also requires a closed source (all variables bound), so
`replay_initial_reification` identifies the initial paper program directly from
an accepted replay certificate. These results are exercised in
`Tests/SemanticBridge.lean`. Sampling correspondence, reachable continuation
invariants, and a bookkeeping bound feed into the unbounded execution proof.
`Graph.lean` connects the graph to machine output, and `Soundness.lean` establishes
`Model.Matches`, which is carried by the extracting checker’s result.

### Certified expected rewards and Storm

From `lean/`:

```sh
lake exe determinize --result /tmp/model --subject source ../tests/statistical/discrete.det
lake env lean /tmp/model.result.lean
```

`--result` exports the model and an exact expected-reward certificate. It checks
all terminal/transient equations and a positive finite-step absorption bound.
The exported `expectedReward` theorem establishes integrability and the exact
answer for the selected core program. Rejected paths contribute zero; the answer
is unnormalized. Nonabsorbing models are rejected by this initial result checker.
The dense exact solver defaults to `--max-result-states 256`.

For Storm comparison, from the repository root:

```sh
python3 -m venv /tmp/determinize-storm
/tmp/determinize-storm/bin/pip install -r tools/storm-requirements.txt
/tmp/determinize-storm/bin/python tools/storm.py tests/statistical/discrete.det --prefix /tmp/model --subject source
```

The wrapper independently kernel-checks the certificate and runs Storm on the
positive and negative reward files. It records version/options/status in
`/tmp/model.storm.json`. The adapter loads rational explicit data into Storm's
exact sparse-matrix API and requires exact agreement with the Lean-checked answer.
Storm is not trusted by the theorem. Each subprocess has a
120-second timeout, adjustable with `--timeout`.
When overriding the wrapper's `--binary`, use an absolute path; relative executable
paths currently resolve against the repository root rather than the caller's directory.

`lean/test.sh` includes exact ground truth and independent result-certificate
replay with tampered values and horizons. Set `STORM_PYTHON` to an interpreter
with the pinned `stormpy` dependency to include real Storm integration tests.
