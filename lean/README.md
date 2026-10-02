Review these entry points and the definitions they import:

- `Determinize/Theorems.lean` states every theorem in full and proves it by one term from `Proof`. The paper's theorems come first, under the paper's names and with the paper's hypotheses and conclusions: `inference_correctness`, `trace_preservation`, `tracewise_soundness`, `output_mass_preservation`, `expectation_preservation`, `variance_non_increase`, `convex_function_inequality`, `probability_preservation` and `trace_variance_decomposition`. Expectation and variance are those of the output conditioned on returning. `tracewise_soundness` and `trace_variance_decomposition` use Mathlib's `Measure.condKernel`. The results the paper does not state follow: `return_or_diverge` at either affinity, `finite_expectation_preservation`, `conditional_expectation_preservation` (equal expectations conditioned on acceptance, the statement behind `observe`), and the versions for the unnormalized output laws, `unnormalized_expectation_preservation` (finite expectations), `unnormalized_extended_expectation_preservation` (expectations in the extended reals, infinite values included), `unnormalized_variance_non_increase` (non-increasing second moment and variance) and `unnormalized_trace_variance_decomposition` (the law of total variance along traces). The file's last command fails the build unless each theorem depends only on `propext`, `Classical.choice`, and `Quot.sound`. It also fails if a theorem's statement relies on a declaration from a `Proof` module that is not a proof, following the statements through every definition they use.
- `Determinize/Spec/` contains the definitions the theorems are about: ordinary syntax, typing, primitive distributions and means, determinization, and semantics. `Spec/Expectation.lean` defines expectations in the extended reals (`HasExpectation`, `extendedExpectation`), the return probability (`returnProbability`), the output law conditioned on returning (`returnedLaw`), and the expectation and variance conditioned on returning (`returnedExpectation`, `returnedVariance`). `Spec/Traces/Semantics.lean` defines the operational traces, the joint law `traceAndOutputLaw` of a program's trace and output, and its trace marginal `traceLaw`; operational replay lives in `Proof/Traces/ReplaySemantics.lean`.
- `Determinize/Spec/Inference.lean` defines what affinity inference is measured against. `Frontend.infer` fills the omitted sample affinities of a resolved program `input : Input`. A *completion* of `input` fills exactly those affinities (`Input.Matches`) and is closed and typed at some type; `AffinityLE` compares programs site by site in the order G ≤ E. `Theorems.inference_correctness`: if `infer` fails, no completion exists; if it succeeds, its program is a completion, typed at the returned type, and every completion lies below it, so it is the greatest completion. `Theorems.lean` imports `Frontend/Infer.lean` because `infer` is the subject of that statement; its body need not be read. `Spec/Inference.lean` reuses the program types and `Input.Matches` from `Spec/Frontend.lean`: `Input` and `Annotated` are `Expr` with rational literals, whose sites carry a requested affinity or `none` (a placeholder), respectively an affinity. `Input.Matches` and `AffinityLE` are both `Expr.Sitewise`, which relates two programs with the same constructors, literals and indices site by site. An annotated program is coerced to a `Core` program (`Annotated.toCore`) where `interpret` expects one.

For normalized output laws, `finite_expectation_preservation` states positive target return probability, probability-law and integrability facts for both `returnedLaw`s, and equality of their finite means. `expectation_preservation`, `variance_non_increase`, and `trace_variance_decomposition` cover extended means and normalized variance results.

For the executable backend, also review `Spec/Frontend.lean` (the rational core `Core`, with sample and mean sites, and its embedding `interpret` into real-literal expressions), `Spec/FiniteModel/Model.lean`, `Spec/FiniteModel/Statistics.lean`, `Spec/RewardModel/Model.lean`, and `Spec/RewardModel/Results.lean`. The general `MomentCertificate` and reward `Solution` routes handle closed divergent regions; the older absorption certificate is a sufficient special case. `finite_reward_integrability` derives finite moments for every finite additive model without user-supplied integrability bounds. Applying determinization to infer source expectations still requires the source hypotheses; finite target exploration does not establish source integrability.

Run `lake build --wfail` from this directory; the build is warning-free and contains no `sorry`, and it checks the public theorems' axioms. With Lean's kernel and these standard axioms trusted, reviewers can omit the proof bodies in `Proof`. `Theorems.lean` and `Spec` are the specification. A theorem cannot be stated in `Theorems.lean` without being proved, so no separate check that every statement has a proof is needed. No `Spec` file imports a `Proof` module or the front end. `Theorems.lean` imports `Proof`, but its statements may use it only for proofs: Lean treats any two proofs of a proposition as equal, so they cannot change what a statement means, and the build fails if a statement relies on anything else declared in `Proof`. The one proof the statements themselves use is finiteness evidence from `Proof/Traces/Mass.lean`: Mathlib defines `condKernel` only for finite measures, and the joint law of trace and output is one. The check sees what the statements use, not how an import changes the way `Theorems.lean` elaborates: a coercion, a notation or a changed instance priority from `Proof` could do that without leaving anything from `Proof` in a statement. `Theorems.lean` also imports the front end: `Frontend/Infer.lean`, and through it `Frontend/Syntax.lean`, the shape unifier `Frontend/Unify.lean` and the affinity solver `Frontend/Affinity.lean`.

The typed determinization theorems assume `DomainSafe`: every reached operation has valid arguments almost surely at every finite execution depth. For typed programs, this requires valid distribution parameters and nonzero divisors. Rejection and divergence remain possible. `return_or_diverge` proves that returned mass plus divergence probability is one for domain-safe programs of real type. Divergence is defined as the infimum of finite-depth running probabilities and includes rejection. `Proof.Paper.domainSafe_iff_return_or_diverge` proves the converse under real typing, so the mass-balance equation characterizes domain safety. `trace_preservation`, `probability_preservation` and `unnormalized_expectation_preservation` also establish target domain safety. Finite replay certificates use the same predicate, defined in `Spec/Semantics.lean`.

Source expressions need not be in ANF. Each primitive distribution is its own constructor with the paper's operands (`uniform action lower upper`, `gaussian action mean variance`, and so on). `DistributionAction` is either `sample affinity` or `mean`, where `Affinity` is E or G. Mean expressions carry no affinity annotation; their result affinity follows the operand typing rules. Operands may contain nested sampling and are evaluated left to right; a mean site also evaluates every operand exactly once, including a Gaussian's variance. The soundness theorems also cover source expressions containing mean sites. `Spec/Primitives.lean` gives each primitive one fiber: its law at a stochastic site, the Dirac mass at its mean at a mean site, and the zero measure outside the parameter domain. Expressions have no type annotations; `Typed` assigns types separately. Expressions carry E/G labels only on sample sites; literals and arithmetic are unannotated and a literal types at either affinity, as in the paper's rule Real; subtyping is silent; variables are de Bruijn indices. `Typed` enforces the affinity restrictions: E multiplication requires a G left operand, division a G denominator, and comparisons G operands. Arithmetic uses real numbers; division by zero gets stuck. The theorems quantify over closed float programs of either affinity; the proof uses subsumption to assign an E result type to the same program.

`DistributionAction.determinize` changes `sample E` to `mean` and retains `sample G`. A mean site still evaluates its operands and checks its parameter domain; G draws nested in those operands therefore still execute and appear in the trace.

Output laws are defined directly by recursion over reduction depth. Deterministic actions continue evaluation; sampling actions integrate the continuation over the primitive measure on reals. Expressions of every type may occur during evaluation, but only terminal reals contribute output. Neither evaluator requires a measurable structure on expressions. `Proof` introduces one internally to establish measurability of the evaluators.

A trace is a list of `(primitive, value)` pairs recording only stochastic G draws. Deterministic steps and E draws add no entry. Trace soundness factors the actual joint trace/output measures over `traceLaw source`, the trace marginal of the source joint law: the source output is a Markov kernel indexed by the trace, and the target output is a measurable function of the trace. For almost every trace, the source fiber is an integrable probability measure and the target output equals its mean. The target therefore has the same trace law and the same termination probability as the source. No global integrability is required. Detailed traces with one entry per reduction step live in `Proof/Traces/Detailed.lean`. The proof uses them to connect symbolic execution to compact traces; `Proof/Traces/CompactFiberSoundness.lean` and `Proof/Traces/CompactSoundness.lean` establish the trace factorization directly using compact replay.

The output-mass theorem preserves acceptance/termination mass; dividing the preserved integral by that mass gives the conditional-expectation theorem. The variance theorems bound the target variance and decompose source variance along replay traces. The finite expectation theorem adds source integrability and proves target integrability and equal integrals. The extended-real theorem only assumes that one of `∫ v⁺` and `∫ v⁻` under the source output law is finite, and concludes the same for the target and equal extended-real expectations. Jensen's inequality bounds `∫ φ` under the target output law by `∫ φ` under the source output law for every nonnegative convex `φ : ℝ → ℝ`. Both corollaries are derived from trace soundness in `Proof/Corollaries.lean`. Output measures are unnormalized: divergence contributes no output mass, and expectations are not conditioned on termination.

Symbolic mean steps evaluate affine formulas over the existing E samples, preserving parameter checks and reduction depth.

The default build checks mean sites with affine-dependent operands, invalid mean parameters, nested sampling, the `x + 1/y` example, a G draw scaling an E draw from the left, and a sampled value captured by a function. `Proof/InterfaceChecks.lean` checks the direct evaluator using only public imports and verifies that these imports provide no measurable structure on expressions.

## Deviations from the paper

These notes compare the Lean development with the paper in `tex/` as of `976ef7c`. The previous
draft in `tex/archive/` is no longer compared.

- **Names.** The paper's modes are affinities here: `real^m` is `Ty.float m`, the mean
  annotation `M` is `DistributionAction.mean`, and `Determinize(e)` is `Expr.determinize`.
  The paper's `μ_e^(n)`, `μ_e`, `d_e`, `J_e^(n)`, `J_e` and `η_e` are `outputMeasureAt n`,
  `bigStepMeasure`, `divergenceProbability`, `traceAndOutputLawAt n`, `traceAndOutputLaw` and
  `traceLaw`. The conditional law `μ_e(· | τ)` is Mathlib's `condKernel` of
  `traceAndOutputLaw`, and the law conditioned on returning is `returnedLaw`. The return
  probability `q_e` is `returnProbability`, and `𝔼_ret[e]` and `Var_ret[e]` are
  `returnedExpectation` and `returnedVariance`.
- **Syntax.** Variables are de Bruijn indices. `rec f x. e` is `Expr.fix`, whose body sees the
  argument at index 0 and the function at index 1; a `case` on a list binds the head at index
  0 and the tail at index 1.
  `Expr` takes the type of its literals and of its sites as parameters; the paper's syntax is
  the default, with real literals and `DistributionAction` sites.
- **Theorems.** `Theorems.lean` states the paper's theorems under the paper's names:
  `inference_correctness` (Section 3), `trace_preservation`, `tracewise_soundness`,
  `output_mass_preservation`, `expectation_preservation`, `variance_non_increase`,
  `convex_function_inequality`, `probability_preservation` (Section 4) and
  `trace_variance_decomposition` (appendix). They follow the statements of Section 4, and differ
  from them as follows.
  - The paper's definition of expectation is for probability distributions. `HasExpectation`
    and `extendedExpectation` take any measure on the reals and are used at the unnormalized
    output law. Where the paper says that `𝔼_ret[e]` is well-defined, Lean says
    `HasExpectation (bigStepMeasure program)`, which is the same when the return probability
    is positive.
  - `tracewise_soundness` does not go through `𝔼[e | τ]` in the extended reals. It states that
    the source's conditional law is integrable and that the target's is the Dirac mass at its
    Bochner integral, which is the paper's equation together with its "in particular" clause.
  - A finite second moment is `MemLp id 2`, and `Var_ret[e]` is Mathlib's `variance` of
    `returnedLaw`. `trace_variance_decomposition` also states that the average variance within
    a trace is finite.
  - The appendix restates the theorems, not always as Section 4 does. Its probability
    preservation adds `d_Determinize(e) = d_e`, which no Lean theorem states; it follows from
    `probability_preservation` and `return_or_diverge` for the target, which is typed by
    `typed_determinize`. Its expectation preservation and variance non-increase omit
    `q_Determinize(e) > 0`, which Section 4 and Lean conclude.
  - The theorems are about closed programs of type `real^E`, and an output law has mass only
    at real results. The introduction's Gaussian random walk returns a list of pairs: the
    front end infers and determinizes it, but no theorem applies to it as written.

  The paper does not state `return_or_diverge` at affinity G, `finite_expectation_preservation`,
  `conditional_expectation_preservation`, the unnormalized versions
  (`unnormalized_expectation_preservation`, `unnormalized_extended_expectation_preservation`,
  `unnormalized_variance_non_increase`, `unnormalized_trace_variance_decomposition`), or the
  finite-model and reward-model theorems.
- **Domain safety.** The paper's remark in Section 2 is informal and carries a note to define
  it. `DomainSafe` is the definition: for every depth, almost every execution reaches no stuck
  state within that many steps, and every sampling or mean site it reaches has parameters in
  its domain. This excludes every stuck state, also an ill-typed one or a free variable. For a
  typed closed program the only stuck states are the paper's two: distribution parameters
  outside the domain and division by zero. The build checks that `1 / bernoulli[G](1/2)`, the
  appendix's example without its `let`, is not domain-safe (`Proof/InterfaceChecks.lean`).
- **Evaluation contexts and mean steps.** `Spec` has no evaluation contexts: `reduce` recurses
  into the leftmost unevaluated operand and wraps the result (`Action.wrap`), which realizes
  the contexts of the paper's semantics figure. That figure's evaluation-context link points to
  `Proof/FiniteModel/Contexts.lean`, a proof about finite-model frames. A mean site is not a
  deterministic reduction, as in the paper, but a sampling action whose fiber is the Dirac mass
  at the mean, or the zero measure outside the domain. The output law, the reduction depth and
  the G trace are the same as under the paper's rule.
- **Primitive distributions.** Domains and means agree with the paper's table. Lean has no
  separate `Domain_D` and `Mean_D`: each primitive has one fiber (`uniformFiber` and so on),
  which is its law at a sampling site, the Dirac mass at its mean at a mean site, and the zero
  measure outside the domain. The paper leaves the law at degenerate parameters implicit:
  `uniform(a, a)` is the Dirac mass at `a`, and so is Mathlib's Gaussian with variance 0.
- **Traces.** A trace entry records an `Op`, whose discrete label also records the number of
  supplied probabilities (`Op.discrete arity`); the paper's entry records only the
  distribution. The paper does not say which measurable sets `Trace` has. Lean generates them
  from the length of a trace and its entries (`Spec/Traces/Semantics.lean`), with every set of
  primitive labels measurable.
- **Subtyping.** The paper lists reflexivity and transitivity as rules. `Ty.Sub` has the
  structural rules and reflexivity at base types only; general reflexivity and transitivity are
  lemmas (`Ty.Sub.refl` and `Ty.Sub.trans` in `Proof/Semantics/Subtyping.lean`).
- **Gaussian variance.** Section 3 says the distribution rules allow E parameters wherever the
  mean is affine in them. That would allow an E Gaussian variance, since the mean does not
  depend on it, but the typing figure and `Typed` both require the variance to be G.
- **Inference algorithm.** The paper solves subtype constraints by constructor expansion with
  deferred variable constraints, then propagates E forward and G backward. `Frontend.infer`
  instead decorates the shape that the most general unifier gives each type variable with a
  fresh affinity variable at every float position, decomposes the constraints into atomic ones,
  sets the variables forced below a G to G and all others to E, and checks the result. Both
  compute the greatest completion, and inference correctness speaks only about the result.
  Inference takes an `Input`, a program with rational literals and no mean sites, and
  `interpret` embeds the result into the real-literal `Expr` of the theorems.
- **Proof structure.** Section 5's symbolic state `(σ | ê : τ)` is a `Symbolic.SampleEnv` for
  `σ` and an `AffineExpr n` for `ê`: an `Expr` whose literals are affine expressions in `n`
  samples. `lift`, `realize_ρ`, `A_σ` and the sequential mean `ρ̄_σ` are `AffineExpr.ofExpr`,
  `AffineExpr.realize`, `SampleEnv.actualMeasure` and `SampleEnv.meanEnvironment`; the typing
  of residual expressions is `AffineExpr.WellTyped`, and a well-typed state that is domain-safe
  through a given depth is a `SafeConfigAt`. The differences:
  - The symbolic step is not a relation but a function, `symbolicReduce`, which returns a
    `SymbolicAction`. A mean site is an action of its own there, not a residual reduction.
  - `Act_n` and `Exp_n` are `actualTraceLaw` and `targetTraceLaw`
    (`Proof/Symbolic/TraceLaws.lean`), but over detailed traces with one entry per reduction
    step (`Proof/Traces/Detailed.lean`). The G trace is read off a detailed trace by `retain`.
  - The paper's agreement theorems speak of conditional laws at each depth. Lean's invariant
    at each depth is `FiberSound` (`Proof/Traces/Fibers.lean`), with the compact replay of a
    G trace as an explicit fiber (`compact_exactDepth_fiberSound`). It identifies that fiber
    with Mathlib's `condKernel` only for the laws summed over all depths
    (`Proof/Traces/ConditionalLaw.lean`).
  - The appendix's affine-means lemma is `SampleEnv.integral_affine`. It takes finite
    moments of the primitives as a premise, which `primitiveMomentBounds_primitiveLaws` proves.
- **Surface syntax.** The paper's examples use `fun`, `rec f x =>`, `flip`, subtraction,
  tuples and `match … with` on lists; its grammar has `λ`, `rec`, pairs and `case` instead, and
  no `flip` or subtraction. The unverified front end desugars them, for example `flip(p)` into
  `0 < bernoulli[G](p)` and `a - b` into `a + -b`. The grammar has `reject` but no `observe`;
  the front end lowers `observe(c)` to `if c then () else reject`. The front end also accepts
  `<=`, moves a literal right factor to the left of a multiplication, and reads
  `discrete(p0, …, pn)` with all n + 1 probabilities, dropping the last. The CLI prints `gauss`
  for `gaussian` and `discrete_list` for `discrete`. The introduction's random-walk figure
  calls a `reduce` that it does not define (`examples/paper/gauss-random-walk.det` does), and
  its right-hand program is simplified by hand: the CLI prints mean sites such as
  `mean_gauss(…)` and removes nothing.
- **Implementation (Section 6).** The trust list places the parser and the floating-point
  interpreter in the trusted surface. The trusted Lean sources, `Spec/` and the statements of
  `Theorems.lean`, contain neither: they live in `Frontend/` and `Runtime/`, and no theorem
  depends on the interpreter. Storm does not produce
  a certificate: it returns exact value vectors, and `tools/storm.py` adds the divergent states
  and a rank and next-state witness, then writes a `MomentCertificate` that Lean's kernel checks.
  The checked result covers return mass, first and second moments and rejection probability
  (`checked_statistics`, `checked_conditionalVariance`), not only the expected value. The
  paper does not describe the additive reward models (`--additive`, `Spec/RewardModel`) that
  Section 7 relies on.
- **Lean links.** `tex/lean-links.tex` pins the paper's GitHub links to `29ee1e7`. They
  resolve, but ten of them show sources that have since moved, and four of those show a
  statement that has since been reworded:
  - `expectation-defined`, `trace-law`: the definitions are now in `Spec/Expectation.lean`
    and `Spec/Traces/Semantics.lean`.
  - `return-or-diverge`, `output-mass`, `trace-erasure`, `inference`: the statements are now
    theorems in `Theorems.lean` (`return_or_diverge`, `output_mass_preservation`, `trace_erasure`,
    `inference_correctness`).
  - `expectation-preservation`, `variance-non-increase`, `trace-soundness`, `trace-variance`:
    also in `Theorems.lean` (`expectation_preservation`, `variance_non_increase`,
    `tracewise_soundness`, `trace_variance_decomposition`), and no longer worded as at the pinned
    revision.

  The trace-preservation lemma, the convex function inequality and Section 5 have no link
  yet. The anonymous review build links to a snapshot on
  apndx.org instead. That snapshot predates the verified inference (it has no
  `Spec/Inference.lean`), so the review build's links are off until the snapshot is
  regenerated.

## Lean command-line implementation

Build with `lake build --wfail`, then run from `lean/`:

```sh
.lake/build/bin/determinize ../tests/execution/foldr.det
.lake/build/bin/determinize --samples 1000 --seed 42 ../tests/execution/foldr.det
.lake/build/bin/determinize --check ../tests/execution/foldr.det
./test.sh
```

The CLI reads the existing `.det` grammar, with optional `[E]` or `[G]` after a
sampling primitive. Unannotated sites are inferred; explicit affinities are constraints.
It prints the annotated source and the result of the existing `Expr.determinize`.
`mean_uniform`, `mean_gauss`, etc. in the output denote atomic mean operations:
they evaluate every operand exactly once and check the primitive domain. They are
output notation, not additional source primitives. `--fuel` bounds each numerical
run; `--samples` defaults to zero, so compilation does not execute the program.

## Source layout

Under `Determinize/`:

- `Spec/`: mathematical definitions and assumptions; `Traces/` and `FiniteModel/` contain
  their respective specifications.
- `Proof/Primitives/`: distribution laws, kernels, masses, and moments.
- `Proof/Semantics/`: expression measurability, evaluator kernels, and type safety.
- `Proof/Symbolic/`: affine expressions, symbolic reduction, and its invariants.
- `Proof/Traces/`: detailed and compact traces, replay, and conditional laws.
- `Proof/FiniteModel/`: finite-model correspondence and certificate soundness.
- `Proof/Frontend/`: soundness, optimality and completeness of affinity inference.
  - `Unify.lean`: the shape unifier returns a most general unifier (`unify_mgu`).
  - `Affinity.lean`: the affinity solver returns the greatest solution (`solveAffinities_spec`).
  - `Typing.lean`: inversion of `Typed` for each constructor, up to subsumption.
  - `Decompose.lean`: erasure of types to shapes; decomposing subtyping between decorated
    types into affinity constraints is sound and complete.
  - `Ground.lean`: ground substitutions, the relations they solve, monotone read-back, and
    tactics shared by the two proofs below.
  - `Soundness.lean`: every solution of the generated constraints reads back to a typed
    program (`generate_sound`).
  - `Completeness.lean`: every typed completion comes from a solution of the generated
    constraints (`generate_complete`).
  - `Inference.lean`: the proof of `Theorems.inference_correctness`.
- `Proof/Soundness.lean`, `Proof/Corollaries.lean`: global results derived from traces.
- `Theorems.lean`: the theorems, each stated in full and proved by a term from `Proof/`.

The executable and its checkers are separated as follows:

- `Frontend/`: affinity inference (`Infer.lean`) with the shape unifier (`Unify.lean`)
  and the affinity solver (`Affinity.lean`), verified in `Proof/Frontend/`; unverified
  parsing, desugaring/name resolution, and pretty printing; and `compile`
  (`Compile.lean`), which runs them in order.
- `Checking/`: executable checkers for finite distributions and for finite-model result,
  moment, and termination certificates, with their soundness theorems.
- `Finite/`: verified graph construction and expected-reward solving, with model export.
- `Proof/LinearAlgebra/`: executable Gaussian elimination with a proof of the original equations.
- `Runtime/`: an unverified floating-point interpreter and seeded numerical samplers.
- `Tests/`: parsing, inference and input preservation, completions, mean-site typing,
  runtime, finite-model, and kernel proof tests.
- `Main.lean`: the CLI.

`Spec` and the semantic soundness proofs do not import the front end or runtime. The CLI uses the formalization's syntax and determinization,
generalized over literal and site types. Decimal input is parsed exactly as `Rat`; the mathematical
interpretation `interpret` embeds each rational into `ℝ`. A proved commuting equation
(`interpret_determinize` in `Proof/FiniteModel/Initial.lean`) connects rational
determinization to the existing real-literal theorem.

### What is verified

`Frontend.Surface` represents parsed syntax with named binders and dedicated
constructors. Elaboration resolves names to de Bruijn indices and removes syntax
sugar, producing an `Input`: an `Expr` with rational literals whose sample sites carry
an optional E/G affinity, `none` being a placeholder for an affinity to infer. `infer`
fills the placeholders and returns an `Annotated` program, whose sites all carry an
affinity, so that it has no mean sites, and a type. There is no positional annotation
list. `compile` converts the annotated program once into a `Core` program
(`Program.source`) for the runtime and the finite models, which also run determinized
programs with mean sites.

`Theorems.inference_correctness` holds for every `Input`. The returned program
keeps every constructor, payload, and requested sampling affinity of the input at the
same AST node (`Input.Matches`), so inference cannot silently change literals,
operators, binders, or distribution kinds. It is typed at the returned type, it has
the most E sites among all completions, and `infer` rejects an input only if no
completion exists. `compile` takes the typing and input-preservation proofs of its
`Program` from `Theorems.inference_correctness`; no checker runs afterwards. The
guarantees start at the **resolved input expression**: parsing, name resolution, and
desugaring from source bytes are outside the verified boundary, and so is the Lean
compiler that runs `infer` in the executable.

If the inferred type is a float, the program also has type `float E` by subsumption.
The soundness theorems of `Theorems.lean` then apply to the
compiled program under their own hypotheses: `DomainSafe` and, for expectations,
integrability. Typing alone proves neither hypothesis.

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

`discrete[E](p0,...,pn,*)` supplies probabilities for outcomes `0,...,n`;
the final outcome `n+1` has the remaining probability `1 - (p0 + ... + pn)`.
The supplied probabilities may be computed expressions. They must be nonnegative
and sum to at most one. `discrete[E](*)` always returns zero.
`discrete_list[E](ps)` takes an ordinary list expression with the same implied-final-probability
convention. G sampling requires G elements; E sampling accepts E elements.
The mean is affine in the supplied probabilities, and determinization recursively
transforms the operand before computing this mean.

The formal semantics uses exact reals and returns zero mass outside the domain.
Exact execution completes the rational list and checks it with `FiniteDistribution`;
certificates retain exact arithmetic. Float execution permits accumulation rounding
at the upper boundary (eight machine epsilons per supplied probability plus one),
clamps a slightly negative remainder to zero for its mean, and samples with cumulative
thresholds, falling through to the final outcome. This numerical approximation is
outside the theorem. Probabilities are not normalized by their sum.

The existing `discrete[E](p0,...,pn)` form still requires literal rational probabilities
summing exactly to one. It lowers to the new representation without changing its
outcomes. Pretty printing uses `discrete_list` to preserve arbitrary list operands.

`observe(c)` lowers to `if c then () else reject`. The explicit core rejection term
has zero output mass: formally it is an absorbing non-value, as proved in
`Proof/Semantics/Rejection.lean`. The numerical runtime returns a distinct rejection outcome
immediately; ordinary divergence still exhausts fuel. Conditions are evaluated once,
and rejected executions do not evaluate their continuation. The CLI reports rejected
observations separately from execution failures. `conditional_expectation_preservation`
proves equality of the normalized expectations under its stated premises;
empirical means use returned values and differ from unnormalized expectations.

Inference is monomorphic, uses an occurs check and structural subtyping constraints,
and defaults unconstrained affinities to E and unused type variables to `unit`.
Products, sums, and lists are covariant; function arguments are contravariant and
results covariant. `Float[G]` is a subtype of `Float[E]`; the reverse is not allowed.
Using a G draw at type E leaves its sample annotation G, so determinization still
retains the draw. Subsumption appears in typing derivations, never in the expression.
Inference is sound, optimal, and complete (`Theorems.inference_correctness`): it rejects a
program only if no assignment of its unannotated affinities is typable, and otherwise
returns the typable assignment with the most E sites.

The shared corpus and analytical expectations live in [`../tests/`](../tests/README.md).
Run `./test.sh` for unit tests, all corpus compilation/typing checks, and exact execution
checks. Run `./test.sh --statistical` for sampled
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
Both primitives pass through core typing, symbolic and trace semantics, affinity
inference, and the numerical runtime. The runtime remains unverified.

### Finite-model contract

[finite-model-contract.md](finite-model-contract.md) specifies exact rational
models, one-time terminal rewards, rejection, the initial primitive policy, and
certificates with value equations and finite-step absorption bounds. Definitions
live in `Spec/FiniteModel/`; proofs and theorems composing checker
correctness guarantees live in `Proof/FiniteModel/`. The verified explorer is implemented in `Finite/`, with builder invariants in `Proof/FiniteModel/`. The verified
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

Successful exploration carries a proof of correspondence with the executable machine before writing
`.candidate.lean`, `.replay.lean`, `.tra`, `.lab`, `.positive.state.rew`, and
`.negative.state.rew`. The replay checker takes the requested source and subject
separately from the candidate and checks their alignment, exact transitions,
complete positive successor coverage, rewards, and absorbing terminal states.
Acceptance constructs a `CheckedModel` with a `Spec.FiniteModel.Model` and
a proof of paper safety and complete output-law equality. Stored states must be unique.

The internal export path uses the builder’s proof without replaying the graph.
The `.candidate.lean` file contains raw data. The `.replay.lean` file additionally
contains `machineReplay`, proved by `decide +kernel`, the resulting `model`, and
`modelMatches`, which certifies the selected paper program.
Check it independently with:

```sh
lake env lean /tmp/model.replay.lean
```

These are program-level model certificates: they cover unbounded execution,
rejection, and divergence. Moment certificates also cover finite graphs with
divergence. Portable replay checks sparse rows and successor-index witnesses;
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

### Certified output moments and Storm

From `lean/`:

```sh
lake exe determinize --result /tmp/model --subject source ../tests/statistical/discrete.det
lake env lean /tmp/model.result.lean
```

`--result` exports the model, `.result.json`, and a standalone `.result.lean`
certificate. The JSON reports return, rejection, and divergence probabilities,
the first two unnormalized output moments, and conditional mean and variance.
Conditional quantities are `null` when return probability is zero.
For example, equal chances of diverging and returning 2 give return mass 1/2,
first moment 1, second moment 2, conditional mean 2, and conditional variance 0.

The verified builder constructs the graph correspondence proof. Terminal
reachability identifies a closed divergent region, which contributes no output.
One positive-probability edge of decreasing rank per remaining transient state
proves uniqueness of the equations. `Finite.solveStatistics` and
`Finite.solveTermination` use verified rational Gaussian elimination and return
proofs directly. No graph replay or result checker runs on this internal path.
The dense solver defaults to `--max-result-states 256`.

Portable exports check saved graph data, path witnesses, and value equations in
Lean's kernel, using per-state proofs and sparse equations. `outputStatistics` certifies the selected core program's output
law; `expectedReward` and `conditionalVariance` give its first moment and
conditional variance. `terminationProbabilities` certifies return, rejection,
and divergence probabilities for the finite model. These probabilities sum to
one by `Proof.FiniteModel.mass_balance`, with divergence defined as the limit of
survival probabilities. The paper semantics counts rejection among executions
with no output; the finite model distinguishes rejection from divergence.

For an independent Storm certificate, from the repository root:

```sh
python3 -m venv /tmp/determinize-storm
/tmp/determinize-storm/bin/pip install -r tools/storm-requirements.txt
/tmp/determinize-storm/bin/python tools/storm.py tests/statistical/discrete.det --prefix /tmp/model --subject source
```

The adapter invokes `--export`, obtains full exact rational state-value vectors
from Storm, and writes `.storm.lean`. Lean checks the vectors against the original
model, including boundary conditions. `reportedStatistics` additionally binds every reported rational statistic (including optional conditional quantities) to the certified initial-state statistics; changing the exported initial label cannot silently select a different answer. This route never calls our solver unless
`--compare` is supplied and has no 256-state solver limit. Both routes certify
the same quantities. Storm, rational decoding, and serialization are outside the
proof; incorrect vectors fail checking.

`.storm.json` records the certified values, versions, commands, stage durations,
and completion or failure. A kernel-checked report also requires axiom reports using
only `propext`, `Classical.choice`, and `Quot.sound`. Each subprocess has a 120-second timeout, adjustable
with `--timeout`; large portable kernel checks can require more time.
When overriding `--binary`, use an absolute path; relative paths resolve against
the repository root.

`lean/test.sh` includes exact ground truth, independent kernel replay, and
certificates with tampered values, boundaries, ranks, and dimensions.
Set `STORM_PYTHON` to an interpreter with the pinned `stormpy` dependency to
include real Storm integration tests. The precise contract and proof boundaries
are described in [finite-model-contract.md](finite-model-contract.md).
