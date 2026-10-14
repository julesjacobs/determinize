Review these entry points and the definitions they import:

- `Determinize/Theorems.lean` states every theorem in full and proves it by one term from `Proof`. The paper's theorems come first, under the paper's names and with the paper's hypotheses and conclusions: `inference_correctness`, `trace_preservation`, `tracewise_soundness`, `output_mass_preservation`, `expectation_preservation`, `variance_non_increase`, `convex_function_inequality`, `probabilistic_type_safety`, `trace_variance_decomposition`, `determinization_preserves_typing`, `equality_of_total_masses` and `returned_expectation_along_traces`. Expectation and variance are those of the output conditioned on returning. `tracewise_soundness` and `trace_variance_decomposition` use Mathlib's `Measure.condKernel`. Further results follow: `return_or_diverge` (the paper states it at affinity E only), `finite_expectation_preservation`, `conditional_expectation_preservation` (equal expectations conditioned on acceptance, the statement behind `observe`), `determinized_output_law` (the target's output law is the law of the source's conditional mean given the trace, under typing and domain safety alone; the output marginal of the appendix's big-step symbolic soundness lemma). `trace_erasure`, among the paper's theorems, proves that the output law is the output marginal of the joint law, which Section 4 states without a theorem. The file's last command fails the build unless each theorem depends only on `propext`, `Classical.choice`, and `Quot.sound`. It also fails if a theorem's statement relies on a declaration from a `Proof` module that is not a proof, following the statements through every definition they use.
- `Determinize/Spec/` contains the definitions the theorems are about: ordinary syntax, typing, primitive distributions and means, determinization, and semantics. `Spec/Expectation.lean` defines expectations in the extended reals (`HasExpectation`, `extendedExpectation`), the return probability (`returnProbability`), the output law conditioned on returning (`returnedLaw`), and the expectation and variance conditioned on returning (`returnedExpectation`, `returnedVariance`). `Spec/Traces/Semantics.lean` defines the operational traces, the joint law `traceAndOutputLaw` of a program's trace and output, and its trace marginal `traceLaw`; operational replay lives in `Proof/Traces/ReplaySemantics.lean`.
- `Determinize/Spec/Inference.lean` defines what affinity inference is measured against. `Frontend.infer` fills the omitted sample affinities of a resolved program `input : Input`. A *completion* of `input` fills exactly those affinities (`Input.Matches`) and is closed and typed at some type; `AffinityLE` compares programs site by site in the order G ≤ E. `Theorems.inference_correctness`: if `infer` fails, no completion exists; if it succeeds, its program is a completion, typed at the returned type, and every completion lies below it, so it is the greatest completion. `Theorems.lean` imports `Frontend/Infer.lean` because `infer` is the subject of that statement; its body need not be read. `Spec/Inference.lean` reuses the program types and `Input.Matches` from `Spec/Frontend.lean`: `Input` and `Annotated` are `Expr` with rational literals, whose sites carry a requested affinity or `none` (a placeholder), respectively an affinity. `Input.Matches` and `AffinityLE` are both `Expr.Sitewise`, which relates two programs with the same constructors, literals and indices site by site. An annotated program is coerced to a `Core` program (`Annotated.toCore`) where `interpret` expects one.

For normalized output laws, `finite_expectation_preservation` states positive target return probability, probability-law and integrability facts for both `returnedLaw`s, and equality of their finite means. `expectation_preservation`, `variance_non_increase`, and `trace_variance_decomposition` cover extended means and normalized variance results.

For the executable backend, also review `Spec/Frontend.lean` (the rational core `Core`, with sample and mean sites, and its embedding `interpret` into real-literal expressions), `Spec/FiniteModel/Model.lean`, `Spec/FiniteModel/Statistics.lean`, `Spec/RewardModel/Model.lean`, and `Spec/RewardModel/Results.lean`. The `MomentCertificate` and reward `Solution` certificates handle closed divergent regions. `finite_reward_integrability` derives finite moments for every finite additive model without user-supplied integrability bounds. Applying determinization to infer source expectations still requires the source hypotheses; finite target exploration does not establish source integrability.

Run `lake build --wfail` from this directory; the build is warning-free and contains no `sorry`, and it checks the public theorems' axioms. With Lean's kernel and these standard axioms trusted, reviewers can omit the proof bodies in `Proof`. `Theorems.lean` and `Spec` are the specification. A theorem cannot be stated in `Theorems.lean` without being proved, so no separate check that every statement has a proof is needed. No `Spec` file imports a `Proof` module or the front end. `Theorems.lean` imports `Proof`, but its statements may use it only for proofs: Lean treats any two proofs of a proposition as equal, so they cannot change what a statement means, and the build fails if a statement relies on anything else declared in `Proof`. The one proof the statements themselves use is finiteness evidence from `Proof/Traces/Mass.lean`: Mathlib defines `condKernel` only for finite measures, and the joint law of trace and output is one. The check sees what the statements use, not how an import changes the way `Theorems.lean` elaborates: a coercion, a notation or a changed instance priority from `Proof` could do that without leaving anything from `Proof` in a statement. `Theorems.lean` also imports the front end: `Frontend/Infer.lean`, and through it `Frontend/Syntax.lean`, the shape unifier `Frontend/Unify.lean` and the affinity solver `Frontend/Affinity.lean`.

The entry points can also be read as [API documentation](https://julesjacobs.github.io/determinize/docs/Determinize/Theorems.html), in which every name in a statement links to its definition and every declaration to its source. It covers the modules `Determinize.lean` imports, and CI publishes it from `main`, next to the simulator. To build and browse it locally:

```sh
cd docbuild
lake build Determinize:docs
python3 -m http.server -d .lake/build/doc
```

The first build also documents Lean and the imported part of Mathlib, which takes about two CPU-hours. `docbuild` pins doc-gen4 and shares `.lake/packages` with this directory. After changing `lean-toolchain` or a dependency, set doc-gen4's `rev` in `docbuild/lakefile.toml` to the new Lean version and run `MATHLIB_NO_CACHE_ON_UPDATE=1 lake update` in `docbuild`; CI fails while the two directories pin different revisions.

The typed determinization theorems assume `DomainSafe`: every reached operation has valid arguments almost surely at every finite execution depth. For typed programs, this requires valid distribution parameters and nonzero divisors. Rejection and divergence remain possible. `return_or_diverge` proves that returned mass plus divergence probability is one for domain-safe programs of real type. Divergence is defined as the infimum of finite-depth running probabilities and includes rejection. `Proof.Paper.domainSafe_iff_return_or_diverge` proves the converse under real typing, so the mass-balance equation characterizes domain safety. `trace_preservation` and `probabilistic_type_safety` also establish target domain safety. Finite replay certificates use the same predicate, defined in `Spec/Semantics.lean`.

Source expressions need not be in ANF. Each primitive distribution is its own constructor with the paper's operands (`uniform action lower upper`, `gaussian action mean variance`, and so on). `DistributionAction` is either `sample affinity` or `mean`, where `Affinity` is E or G. Mean expressions carry no affinity annotation; their result affinity follows the operand typing rules. Operands may contain nested sampling and are evaluated left to right; a mean site also evaluates every operand exactly once, including a Gaussian's variance. The soundness theorems also cover source expressions containing mean sites. `Spec/Primitives.lean` gives each primitive one fiber: its law at a stochastic site, the Dirac mass at its mean at a mean site, and the zero measure outside the parameter domain. Expressions have no type annotations; `Typed` assigns types separately. Expressions carry E/G labels only on sample sites; literals and arithmetic are unannotated and a literal types at either affinity, as in the paper's rule Real; subtyping is silent; variables are de Bruijn indices. `Typed` enforces the affinity restrictions: E multiplication requires a G left operand, division a G denominator, and comparisons G operands. Arithmetic uses real numbers; division by zero gets stuck. The theorems quantify over closed float programs of either affinity; the proof uses subsumption to assign an E result type to the same program.

`DistributionAction.determinize` changes `sample E` to `mean` and retains `sample G`. A mean site still evaluates its operands and checks its parameter domain; G draws nested in those operands therefore still execute and appear in the trace.

Output laws are defined directly by recursion over reduction depth. Deterministic actions continue evaluation; sampling actions integrate the continuation over the primitive measure on reals. Expressions of every type may occur during evaluation, but only terminal reals contribute output. Neither evaluator requires a measurable structure on expressions. `Proof` introduces one internally to establish measurability of the evaluators.

A trace is a list of `(primitive, value)` pairs recording only stochastic G draws. Deterministic steps and E draws add no entry. Trace soundness factors the actual joint trace/output measures over `traceLaw source`, the trace marginal of the source joint law: the source output is a Markov kernel indexed by the trace, and the target output is a measurable function of the trace. For almost every trace, the source fiber is an integrable probability measure and the target output equals its mean. The target therefore has the same trace law and the same termination probability as the source. No global integrability is required. Detailed traces with one entry per reduction step live in `Proof/Traces/Detailed.lean`. The proof uses them to connect symbolic execution to compact traces; `Proof/Traces/CompactFiberSoundness.lean` and `Proof/Traces/CompactSoundness.lean` establish the trace factorization directly using compact replay.

The output-mass theorem preserves the probability of returning; dividing the preserved integral by it gives `conditional_expectation_preservation`. `variance_non_increase` bounds the target variance, and `trace_variance_decomposition` decomposes the source variance along traces. `finite_expectation_preservation` adds source integrability and proves target integrability and equal means. `expectation_preservation` only assumes that one of `∫ v⁺` and `∫ v⁻` under the source output law is finite, and concludes the same for the target and equal extended-real expectations. Jensen's inequality (`convex_function_inequality`) bounds `∫ φ` under the target output law by `∫ φ` under the source output law for every nonnegative convex `φ : ℝ → ℝ`. The results are derived from trace soundness in `Proof/Corollaries.lean` and `Proof/Normalization.lean`. Output measures are unnormalized: divergence contributes no output mass. The theorems' expectations and variances are conditioned on returning, except in `convex_function_inequality`.

Symbolic mean steps evaluate affine formulas over the existing E samples, preserving parameter checks and reduction depth.

The default build checks mean sites with affine-dependent operands, invalid mean parameters, nested sampling, the `x + 1/y` example, a G draw scaling an E draw from the left, and a sampled value captured by a function. `Proof/InterfaceChecks.lean` checks the direct evaluator using only public imports and verifies that these imports provide no measurable structure on expressions.

## Deviations from the paper

These notes compare the Lean development with the paper in `tex/` as of `e87171c`, whose Lean
links are pinned to `25d2cb6`. Later changes require rechecking the comparison.
[PAPER_DEVIATIONS.md](PAPER_DEVIATIONS.md) summarizes them in plain language and marks which
are open. [Open issues](../OPEN_ISSUES.md) records remaining paper work and deferred implementation.

- **Names.** The paper's set of modes `𝒜 = {G, E, M}` is `DistributionAction`: `sample
  affinity` for G and E, `mean` for M. Result modes are affinities: `real^m` is `Ty.float m`,
  and `Determinize(e)` is `Expr.determinize`. Section 3's completions and their order are
  `Completion` and `AffinityLE`. The paper's `μ_e^(n)`, `μ_e`, `d_e`, `J_e^(n)`, `J_e` and
  `η_e` are `outputMeasureAt n`, `bigStepMeasure`, `divergenceProbability`,
  `traceAndOutputLawAt n`, `traceAndOutputLaw` and `traceLaw`. Sections 2 and 4 type
  `μ_e^(n)`, `μ_e`, `J_e^(n)` and `J_e` as subprobability measures; the Lean definitions are
  `Measure ℝ` and `Measure (Trace × ℝ)`, and their mass bound is a lemma
  (`traceAndOutputLaw_mass_le_one` with `trace_erasure`). The paper defines `d_e` in words, as
  the probability of diverging or rejecting; `divergenceProbability` is the infimum over
  depths of the probability of not yet being a value (`runningProbabilityAt`), to which stuck
  executions add nothing. The conditional law `μ_e(· | τ)` is Mathlib's `condKernel` of
  `traceAndOutputLaw`. `returnedLaw` is the output law conditioned on returning, which the
  paper uses only through `𝔼_ret[e]` and `Var_ret[e]`. The return probability `q_e` is
  `returnProbability`, and `𝔼_ret[e]` and `Var_ret[e]` are `returnedExpectation` and
  `returnedVariance`.
- **Syntax.** Variables are de Bruijn indices. `rec f x. e` is `Expr.fix`, whose body sees the
  argument at index 0 and the function at index 1; a `case` on a list binds the head at index
  0 and the tail at index 1.
  `Expr` takes the type of its literals and of its sites as parameters; the paper's syntax is
  the default, with real literals and `DistributionAction` sites.
- **Theorems.** `Theorems.lean` states the paper's theorems under the paper's names:
  `inference_correctness` (Section 3), `trace_preservation`, `tracewise_soundness`,
  `equality_of_total_masses`, `output_mass_preservation` (an equation in Section 4, a lemma in
  the appendix), `expectation_preservation`, `variance_non_increase`,
  `probabilistic_type_safety` (Section 4), `determinization_preserves_typing`,
  `convex_function_inequality` and `trace_variance_decomposition` (appendix). They follow the
  statements of Section 4, and differ from them as follows.
  - The paper's definition of expectation is for probability distributions. `HasExpectation`
    and `extendedExpectation` take any measure on the reals and are used at the unnormalized
    output law. Where the paper says that `𝔼_ret[e]` is well-defined, Lean says
    `HasExpectation (bigStepMeasure program)`, which is the same when the return probability
    is positive.
  - Section 4 defines `𝔼_ret[e]` by an integral over `μ_e`, which `returnedExpectation` is, and
    derives `𝔼_ret[e] = (1/q_e) ∫ 𝔼[e | τ] η_e(dτ)` from the disintegration identity.
    `returned_expectation_along_traces` states it for every program, with the integral of
    `𝔼[e | τ] = ∫ r⁺ μ_e(dr | τ) - ∫ r⁻ μ_e(dr | τ)` split into the nonnegative integrals of its
    two terms over `η_e`; the paper's form follows by linearity when one of them is finite. The
    paper does not name the identity.
  - Section 4 says without proof that the output marginal of `J_e` is the `μ_e` of Section 2;
    `trace_erasure` proves it.
  - `tracewise_soundness` takes the target's Dirac mass at the Bochner integral of the source's
    conditional law, where Section 4 writes `𝔼[e | τ]`, defined in the extended reals. Under the
    theorem's integrability clause, which both state, the two agree.
  - A finite second moment is `MemLp id 2`, and `Var_ret[e]` is Mathlib's `variance` of
    `returnedLaw`. `trace_variance_decomposition` also states that the average variance within
    a trace is finite.
  - The theorems are about closed programs of type `real^E`, and an output law has mass only
    at real results. The introduction's Gaussian random walk returns a list of pairs: the
    front end infers and determinizes it, but no theorem applies to it as written.
 `determinized_output_law` is the output marginal of the appendix's big-step symbolic soundness
lemma; the lemma's joint form is `traceAndOutputLaw_determinize`
(`Proof/Traces/CompactSoundness.lean`), with the replay mean `replayMean` in place of `𝔼[e |
τ]`. Section 5 derives tracewise soundness in prose from its whole-program factorization,
through `m(τ)`, the mean of `K_e`, which equals `replayMean` almost everywhere. The paper does
not state `return_or_diverge` at affinity G, `finite_expectation_preservation`,
`conditional_expectation_preservation`, or `finite_reward_integrability`, which Section 6 implies
by calling the certified numbers the exact moments; Section 6 states `finite_mass_balance` in
prose.
- **Domain safety.** Section 2's `Safe_n` is `DomainSafeAt n`, and domain safety is
  `DomainSafe`. `DomainSafeAt` recurses through `reduce` (see below) instead of evaluation
  contexts. Its sampling case, which also covers mean sites, requires the fiber to have mass
  one, which holds exactly inside the primitive's domain; `Safe_n` requires `Domain_D(v̄)` at G
  and E sites and reaches a mean site outside its domain as a stuck expression. Both exclude
  every stuck state and every off-domain call, a zero fiber in Lean (see below); for a typed
  closed program these failures are distribution parameters outside the domain and division by
  zero. The build checks that `1 / bernoulli[G](1/2)`, the appendix's example without its `let`,
  is not domain-safe (`Proof/InterfaceChecks.lean`).
- **Evaluation contexts, mean steps and off-domain calls.** `Spec` has no evaluation contexts:
  `reduce` recurses into the leftmost unevaluated operand and wraps the result
  (`Action.wrap`), which realizes the contexts of the paper's semantics figure. A mean site is
  not a deterministic reduction, as in the paper, but a sampling action whose fiber is the
  Dirac mass at the mean. A primitive call outside its domain, at a sampling or a mean site,
  is not stuck as in Section 2, but a sampling action whose fiber is the zero measure; like a
  stuck state, it contributes no output and no running probability, and it is not domain-safe.
  The output law, the reduction depth and the G trace are the same as under the paper's rules.
- **Primitive distributions.** Domains and means agree with the paper's table. Lean has no
  separate `Domain_D` and `Mean_D`: each primitive has one fiber (`uniformFiber` and so on),
  which is its law at a sampling site, the Dirac mass at its mean at a mean site, and the zero
  measure outside the domain. At degenerate parameters both say Dirac mass: `uniform(a, a)` is
  the Dirac mass at `a`, and so is Mathlib's Gaussian with variance 0.
- **Traces.** A trace entry records an `Op`, whose discrete label also records the number of
  supplied probabilities (`Op.discrete arity`); the paper's entry records only the primitive.
  Section 4's `Trace` is the disjoint union of the `(Prim × ℝ)^n` with the disjoint-union Borel
  σ-algebra. Lean's `Trace` is `List (Op × ℝ)`, whose measurable sets are generated by the
  length of a trace and its entries, with every set of primitive labels measurable
  (`Spec/Traces/Semantics.lean`); this is the same σ-algebra, with `Op` for `Prim`.
- **Subtyping.** The paper lists reflexivity and transitivity as rules. `Ty.Sub` has the
  structural rules and reflexivity at base types only; general reflexivity and transitivity are
  lemmas (`Ty.Sub.refl` and `Ty.Sub.trans` in `Proof/Semantics/Subtyping.lean`).
- **Distribution typing.** Each distribution rule of the typing figure, with its side condition
  `α = M ∨ α = m`, is two constructors of `Typed`: one for a sampling site, at `m = α`
  (`Typed.uniform`, `Typed.gaussian`, …), and one for a mean site, at either affinity
  (`Typed.uniformMean`, …).
- **Inference algorithm.** The paper checks type shapes, then solves the subtype constraints
  by expanding a type variable one constructor at a time, deferring constraints between two
  unresolved type variables, and propagates E forward and G backward through the remaining
  mode inequalities. `Frontend.infer` unifies the shapes of both sides of every constraint
  (`unify`), decorates the shape that the most general unifier gives each type variable with a
  fresh affinity variable at every float position, decomposes the constraints into atomic ones
  (`decompose`), sets the variables forced below a G to G and all others to E
  (`solveAffinities`), and checks the result. An explicit annotation is the affinity itself,
  not a fresh mode variable `a` with `a = m`. Both compute the greatest completion, and
  inference correctness speaks only about the result. Inference takes an `Input`, a program
  with rational literals, no mean sites and `none` at each site the paper leaves unannotated,
  and `interpret` embeds the result into the real-literal `Expr` of the theorems. The
  appendix's constraint-generation rules also cover mean sites (`A(M, a) = ∅`).
- **Proof structure.** Section 5's symbolic state `(σ | ê)`, the appendix's `(σ | ê : τ)`, is a
  `Symbolic.SampleEnv` for `σ` and an `AffineExpr n` for `ê`: an `Expr` whose literals are
  affine expressions `Symbolic.Affine n` in `n` samples. `lift`, `realize_ρ`, `A_σ`, the
  sequential mean `ρ̄_σ` and `Mean^#_D` are `AffineExpr.ofExpr`, `AffineExpr.realize`,
  `SampleEnv.actualMeasure`, `SampleEnv.meanEnvironment` and `meanAffine`, and
  `realize_()(lift(e)) = e` is `AffineExpr.realize_ofExpr`. The typing of residual expressions
  is `AffineExpr.WellTyped`, and a state well-typed at `real^E` and domain-safe through a given
  depth is a `SafeConfigAt`. Section 5's finite-step factorization is
  `compact_exactDepth_fiberSound`, with `compactFiber` as `K_{n,s}`, and its whole-program
  factorization is `compact_normalized_fiberSound`, with `normalizedKernel` as `K_e`. Both
  conclude `FiberSound` (`Proof/Traces/Fibers.lean`): the factorization, and for almost every
  point a fiber that is a probability measure with a finite mean equal to the point's output.
  The differences:
  - Samples are de Bruijn indices: `Env n` holds the newest sample at index 0, where the paper
    numbers `u_1, …, u_k` in declaration order. A declaration `u ∼ D(ā)` keeps as affine
    expressions only the parameters in which the mean is affine (`affineArity`); the others
    (`generalArity`, such as a Gaussian's variance) are reals, where the paper's typing makes
    them constant affine expressions.
  - The symbolic step is not a relation but a function, `symbolicReduce`, which returns a
    `SymbolicAction`. Section 5's deterministic actions are `next`, `sampleE` and, at a mean
    site, `mean`, which the paper counts as a residual reduction; its sampling action is
    `sampleG`, whose continuation returns a residual expression rather than a state. Like
    `reduce`, `symbolicReduce` has no evaluation contexts (`SymbolicAction.wrap`). It is `stuck`
    where Sample-E's structural premise `Params^#_D(ā)` fails, but checks no parameter domain:
    each action is realized with the primitive's fiber, which is zero outside the domain, where
    the Sample-G rule has `Domain_D(r̄)` as a premise.
  - `Act_n` and `Exp_n` are `actualTraceLaw` and `targetTraceLaw`
    (`Proof/Symbolic/TraceLaws.lean`), but over detailed traces with one entry per reduction
    step (`Proof/Traces/Detailed.lean`). The G trace is read off a detailed trace by `retain`,
    through which `compactFiber` reads its trace. The appendix's commuting lemmas therefore hold
    up to the entry each step prepends; they are `actualTraceLaw_next`,
    `actualTraceLaw_sampleE`, `actualTraceLaw_mean`, `actualTraceLaw_sampleG` and their
    `targetTraceLaw` counterparts, one per `SymbolicAction` other than `stuck`. The
    initialization equations, prose in Section 5 and a theorem in the appendix, are not a lemma
    of their own: `compact_exactDepth_source_fiberSound` proves them from `realize_ofExpr`.
  - Section 5 leaves the definitions of `K_{n,s}` and `K_e` and the compatibility lemma to the
    appendix, whose section for them is a to-do. `compactFiber` replays the G trace and
    integrates over `A_σ` (`compactHistoryReplay`). `K_e` is the replay summed over all depths
    (`outputGivenTrace`), made a Markov kernel by the Dirac mass at 0 where that sum has mass
    other than 1 (`normalizedOutputGivenTrace`). The compatibility lemma is
    `outputGivenTrace_eq_outputGivenTraceAt`. The finite-step factorizations are summed over
    depths on detailed traces (`joint_normalized_fiberSound`), then mapped along `retain`.
  - The appendix's agreement theorems speak of conditional laws at each depth. Lean, like
    Section 5, identifies the replay summed over all depths, `outputGivenTrace`, which equals
    `K_e` almost everywhere, with Mathlib's `condKernel`, and no per-depth fiber
    (`outputGivenTrace_ae_eq_condKernel` in `Proof/Traces/ConditionalLaw.lean`).
  - The affine-means equation of Section 5 and the appendix is `SampleEnv.integral_affine`, and
    the appendix's finiteness claim is `SampleEnv.integrable_affine`
    (`Proof/Symbolic/Moments.lean`). Both take a linear bound on the primitives' first absolute
    moments (`PrimitiveMomentBounds`) as a premise, which `primitiveMomentBounds_primitiveLaws`
    proves.
- **Surface syntax.** The paper's examples use `fun`, `rec f x =>`, `flip`, subtraction,
  tuples and `match … with` on lists; its grammar has `λ`, `rec`, pairs and `case` instead, and
  no `flip` or subtraction. The unverified front end desugars them, for example `flip(p)` into
  `0 < bernoulli[G](p)` and `a - b` into `a + -b`. The grammar has `reject` but no `observe`;
  the front end lowers `observe(c)` to `if c then () else reject`. The front end also accepts
  `<=`, moves a literal right factor to the left of a multiplication, and reads
  `discrete(p0, …, pn)` with all n + 1 probabilities, dropping the last. The CLI prints `gauss`
  for `gaussian` and `discrete_list` for `discrete`. The right-hand programs of the
  introduction's four figures are simplified by hand, as their headings say: the CLI prints
  mean sites such as `mean_gauss(…)`, names binders by position (`x0`, `f1`) and removes
  nothing.
- **Implementation (Section 6).** Section 6 trusts Lean's kernel, the three axioms, the
  specification with the Mathlib definitions it uses, and the Lean compiler for the output of
  type inference, and calls the parser and the floating-point interpreter unverified. The
  pretty printer that shows `e_src` and `e_det` (`Frontend/Pretty.lean`) is unverified too,
  and the CLI computes `e_det` with the compiled `Expr.determinize`. Section 6 counts `Spec/`
  as reviewed against Sections 2–4; `Spec/FiniteModel` and `Spec/RewardModel`, in which the
  exact results are stated, have no counterpart there.
- **Exact results.** The paper mentions the additive reward models (`Spec/RewardModel`, the
  CLI's default, on which Section 7 relies) in one sentence. With Storm, `tools/storm.py`
  writes the certificate from Storm's exact value vectors, adding the divergent states and a
  rank and next-state witness: a `Finite.Reward.Solution` in the additive mode and a
  `MomentCertificate` with `--no-additive`. The kernel checks a certificate only when its
  `.lean` file is compiled: `--result` writes `.result.lean` and reports `"kernel_checked":
  false`, and `tools/storm.py --skip-certificate` does not compile it. Every certificate also
  proves the finite model's rejection probability, and on all four routes it ends with
  `printedStatistics`: the reported return mass and moments as literals, in a statement that
  names only `Spec/` definitions and the literal program (see
  [below](#certified-output-moments-and-storm)). The CLI prints the first moment of the
  unnormalized output law; the return probability, the second moment and the conditional mean
  and variance are in the `.result.json` and `.storm.json` files. Section 6 and the CLI's note
  next to the result transfer an exact mean from `e_det` to `e_src` by
  `expectation_preservation`.
- **Evaluation (Section 7).** Section 7 says the exact values come with certificates.
  `tools/bench.py` passes `--skip-certificate` unless `--check-certificates` is given, and the
  recorded run `results/finite-after-det.json` reports every certificate as "generated,
  unchecked"; its `dreckon` export exceeded the 180-second timeout. Table 1's determinization
  times come from `determinize --det-only` (`tools/bench.py --exact --det-only`), which times
  `compile` and `Expr.determinize`; the recorded run `results/exact-determinization-only.json`
  differs from the table by up to 16% (`swap`).
- **Lean links.** `tex/lean-links.tex` pins the paper's GitHub links to `25d2cb6`, and the
  theorems' links point to `Theorems.lean`. Each linked range holds its declaration at that
  revision. The architecture figure's links follow the additive route; its certificate-checker
  link points to `Finite.Reward.Solution`, whose equations the kernel decides
  (`decide +kernel`), as the additive route has no separate checking function. Section 5's
  links point into `Proof/`. `main.tex` builds the anonymous review version, which links to the
  snapshot `sca95063a7c` on apndx.org instead. That snapshot predates the verified inference:
  it has no `Spec/Inference.lean`, `Spec/Expectation.lean` or `Frontend/Affinity.lean`, and its
  66-line `Theorems.lean` uses other names, so the review build's links are off until the
  snapshot is regenerated.

## Style

The Lean sources follow Mathlib's conventions for naming and layout. The deliberate exceptions:

- **Paper theorems.** The theorems in `Theorems.lean` are named after the paper's theorems, in
  `snake_case`, and not after the shape of their statements. Each states the conclusions of
  its paper theorem as one conjunction.
- **Hypotheses and variables.** A hypothesis is named after what it says (`typed`, `safe`,
  `notValue`) and a variable is written out (`program`, `expression`), where Mathlib writes
  `h`, `ht` and `e`.
- **Explicit arguments.** A theorem takes the program it is about as an explicit argument,
  also when a later hypothesis determines it.
- **Affinities.** The constructors `Affinity.E` and `Affinity.G` are capitalised as in the
  paper.
- **Headers.** Files carry no copyright or author header.

Three things do not follow Mathlib and are not deliberate: definitions outside `Spec` are
not all documented; the `reduce…` families in `Proof/Semantics/Measurability.lean` and the
equality deciders in `Proof/FiniteModel/Equality.lean` are built by tactics; and some
proofs and files are much longer than Mathlib allows.

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
zero fails. Nonfinite arithmetic and sampling results fail explicitly. Gamma
rejection and Poisson iterations are bounded, and Poisson rates above 1,000,000 are
rejected by this numerical runtime. Failed and exhausted runs are reported.

Statistical tests cover representative parameters, not the entire numerical range.
Overflow and underflow in intermediate calculations can distort finite outputs:
for example, `beta[E](1e308, 1e308)` currently produces a numerical mean of zero
instead of one half. The exact rational evaluator and real-measure theorems do
not use these Float calculations.

### Discrete-distribution migration

`Checking/FiniteDistribution.lean` defines checked rational probabilities and
weighted expectation, and `Proof/Primitives/FiniteDistributionMeasure.lean` their real
finite law; no theorem statement uses either. `Spec/Primitives.lean` defines the Bernoulli
fiber and the mean fibers. Their probability, integrability, expectation, and Bernoulli variance and
measurability proofs are in the corresponding `Proof/` modules. The shared primitive kernels,
affine-mean laws, moment bounds, and domain-convexity proofs cover both distributions.
Both primitives pass through core typing, symbolic and trace semantics, affinity
inference, and the numerical runtime. The runtime remains unverified.

### Finite-model contract

[finite-model-contract.md](finite-model-contract.md) specifies exact rational
models, one-time terminal rewards, rejection, the initial primitive policy, and
certificates with value equations, a closed divergent boundary and descending paths. Definitions
live in `Spec/FiniteModel/`; proofs and theorems composing checker
correctness guarantees live in `Proof/FiniteModel/`. The verified explorer is implemented in `Finite/`, with builder invariants in `Proof/FiniteModel/`. The verified
model checker is in `Checking/FiniteModel.lean`. `replay_matches` supplies the correspondence proof carried by each extracted `CheckedModel`.
`Checking/Statistics.lean` proves moment- and termination-checker soundness.
`Tests/FiniteModel.lean` checks signed rewards and a nonterminating model whose value
equations have spurious solutions.


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

Every result certificate, built-in or Storm, additive or not, ends with the reported numbers
as literals:

```lean
theorem printedStatistics :
    (⟨1, 5/4, 9/4⟩ : Determinize.Spec.FiniteModel.OutputStatistics).Matches
      (Determinize.Spec.Paper.bigStepMeasure (checkedSubject.program checkedSource)) :=
  (by decide +kernel : statistics = ⟨1, 5/4, 9/4⟩) ▸ outputStatistics
```

The three literals are the return mass and the first and second moments. The statement names
only `Spec/` definitions and `checkedSource` and `checkedSubject`, whose bodies are literals in
the file. When the return mass is positive, `printedConditionalMoments` states the program's
`returnedExpectation` and `returnedVariance` as literals, proved from `printedStatistics` by
`Proof.FiniteModel.statistics_returned_moments`. A changed literal fails the kernel check.

For an independent Storm certificate, from the repository root:

```sh
python3 -m venv /tmp/determinize-storm
/tmp/determinize-storm/bin/pip install -r tools/storm-requirements.txt
/tmp/determinize-storm/bin/python tools/storm.py tests/statistical/discrete.det --prefix /tmp/model --subject source
```

The adapter invokes `--export`, obtains full exact rational state-value vectors
from Storm, and writes `.storm.lean`. Lean checks the vectors against the original
model, including boundary conditions. `reportedStatistics` additionally binds every reported rational statistic (including optional conditional quantities) to the certified initial-state statistics; changing the exported initial label cannot silently select a different answer. The certificate then ends with `printedStatistics`. This route never calls our solver unless
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
`tests/test_printed_statistics.py` checks the closing theorems on every route, including that
a changed literal is rejected; it finds the routes by the files that generate
`outputStatistics`.
Real Storm integration tests run when `STORM_PYTHON` names an interpreter with the
pinned `stormpy` dependency, which the `.#lean` shell sets on Linux. The precise
contract and proof boundaries are described in
[finite-model-contract.md](finite-model-contract.md).

### Storm tactic

Import `Determinize.Tactic.Storm` and use `by storm` to prove exact output statistics,
returned expectation, or returned variance for a closed rational core program:

```lean
import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel

def coin : Core := .bernoulli (.sample .G) (.real (1/3))

theorem coin_statistics : (⟨1, 1/3, 1/3⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.source.program coin)) := by storm

theorem coin_mean : Determinize.Spec.returnedExpectation (Subject.source.program coin) =
    ((1/3 : Rat) : ℝ) := by storm
```

Set `STORM_PYTHON` to a Python with `tools/storm-requirements.txt` installed. The tactic
finds `tools/storm.py` above its working directory; `set_option determinize.storm.worker
"/absolute/path/to/tools/storm.py"` overrides that search. It supports `Subject.source.program`,
`Subject.determinized.program`, `interpret`, and the determinization of `interpret`. Other
program representations must first be reduced to one of these forms. Conditional moments
require positive return mass. The certified statistics are unnormalized.
Each invocation checks a fresh model; prove output statistics once when several results
are needed, then derive conditional moments with `Proof.FiniteModel.statistics_returned_moments`.

Storm supplies exact rational value vectors, a divergent boundary, and descending-path
witnesses. The tactic encodes the vectors as one string literal, decodes them into arrays,
and reconstructs the graph from the program inside one compiled verifier. It makes one
`native_decide` check of sparse equations and the witnesses, then applies
the existing correspondence and moment theorems. This trusts Lean's compiler and runtime;
the proof's axiom report includes the native check. Storm and JSON decoding supply candidate
data that the checker verifies. Reopening a compiled proof requires no Storm installation.
Set `determinize.storm.native false` for kernel-only checking with no additional axioms;
this builds separate per-state proofs and is substantially slower on large graphs.

The checked-in `dreckon` benchmark (`n = 2`, three iterations) has 43,218 states and
43,498 edges. The native tactic completed its statistics theorem in 67.9 seconds with
3.60 GiB peak process RSS on the development Mac, including imports, exploration, and
Storm. The kernel-only attempt had not finished after 15 minutes.

From `lean/`, run standalone examples with `lake lean /absolute/path/to/example.lean`.
Lake loads the precompiled tactic modules with this command; `lake env lean` alone
uses interpreted tactic code and can be slower.

The native check uses array lookup and iterates over sparse edges. Kernel-only checking
shares syntax, environments, values, and continuation stacks through auxiliary Lean
definitions and uses balanced lookup tables. The tactic does not write
Lean certificate source files or invoke the dense internal solver. `determinize.storm.maxStates`
defaults to 100000; `determinize.storm.timeout` bounds the Storm subprocess and defaults to
120 seconds. Lean's heartbeat limit bounds proof construction; increase `maxHeartbeats` for
large models. `set_option trace.Determinize.storm true` reports the checking stages.

Portable exports also share repeated structures. Additive result certificates check sparse
edge equations and assemble the existing solution theorem. `--storm-export PREFIX` writes
only the transition, label, and reward files. `tools/storm.py --skip-certificate` uses this
mode and skips Lean certificate generation as well as compilation.
