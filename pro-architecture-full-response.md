# Pro full response (rendered text)

Overall assessment

The highest-priority changes are to the meaning and usability of the public interfaces, and to duplication in the proof architecture—not to the number of fields in certificates.

The central mathematical result is stronger and more specific than “replacing samples by means preserves expectation”: on almost every successful generation trace, the target returns the mean of the source replay law. The expectation, output-mass, Jensen, and variance results are consequences of that structure. Conversely, the development’s observable semantics is narrower than a complete account of execution: it records successful real outputs, while rejection and divergence both contribute zero output mass. Those two facts should organize the specification and its presentation. 

determinize-pro-architecture-so… +1

I did not identify a false equation in the nine exported mathematical statements. I did identify substantial gaps between a natural reading of some names and what their definitions establish, avoidable proof duplication, overly broad import dependencies, and concrete frontend/statistics problems. The findings below distinguish those categories.

I revise two recommendations from my first response:

Retain the supplied absorption horizon. Replacing a small hint by model.size can substantially increase checking work.

Retain explicit terminal rows in the current representation. Their semantic redundancy does not outweigh a simpler uniform matrix interface, especially given your discarded dependent-row experiment.

The source-safety, source-integrability, finite-second-moment, and defined-extended-expectation premises should remain where currently required.

Scope of this inspection

I read all eleven Spec files, Theorems.lean, all six Checking files, seven Frontend files, five Finite files, both Runtime files, Main.lean, and all fourteen supplemental test files. Across the fifty-nine Proof modules, I inspected the module/import organization, central definitions and contract signatures, and the proof steps establishing the main semantic bridges. Inspection of the large tactic scripts was selective, not a line-by-line independent verification of every internal lemma.

I did not compile or run Lean. I also did not inspect unbundled Mathlib, the external .det corpus and manifests, Python export/Storm scripts, shell build scripts, or the paper and ancillary documents referenced by the README. Your independently checked example is additional evidence supplied by you, not an execution I performed.

Paths below are relative to lean/Determinize/, except where stated otherwise. Proposed Lean interfaces are sketches, not compiled patches.

Priorities
Priority	Recommendation	Classification
1	Expose acceptance-normalized expectation and variance, and state precisely what rejection and traces observe.	Specification/interface improvement; current equations are not thereby false.
1	Publish the canonical replay facts already established internally, including almost-everywhere probability mass and the target pushforward law.	Public-interface improvement.
1	Reuse Expr (Affine n) for symbolic syntax; remove genuinely phantom parameters; separate elementary syntax and probability lemmas from global soundness.	Architecture change with a substantial maintenance payoff.
2	Consolidate the detailed/compact replay proof paths instead of maintaining two lockstep arguments.	Proof-architecture experiment.
2	Improve the end-to-end certificate facade and measure replay/result checking before changing representations.	Architecture and performance work.
2–3	Investigate Gaussian E-dependent variance, parameter-sensitive finite support, and nonabsorbing result certificates.	Deliberate extensions, not fixes to false current theorems.
1. The human-review boundary needs a more precise definition

“Read Spec, trust Proof” works reasonably well for the nine mathematical propositions, because their syntax, typing, primitive laws, transformation, and output semantics are fixed in the public definitions. The concrete primitiveLaws implementation is connected to those fixed primitive measures by equalities; it is not an unmentioned hypothesis of the exported theorem. 

determinize-pro-architecture-so… +1

It does not describe the whole review boundary of the executable certification system.

There are three different contracts to review:

Mathematical transformation contract. This is principally the eleven Spec files and the nine theorem aliases.

Certification acceptance contract. This additionally includes Checking.Certified, affinity alignment, Candidate.ReplayValid, source scoping, supported finite primitive calls, and ResultCertificate.Valid. Some of these definitions live under Proof/FiniteModel/, despite determining what the checker accepts.

External interpretation contract. This includes which core program the frontend/exporter emits, what the numerical runtime estimates, and what the external transition/reward files mean. These remain separate from the theorem about the embedded core expression.

Directory names do not determine which definitions require human review. Transitive references in the claimed contract do.

Recommended boundary

Keep the mathematical specification small, but explicitly identify two additional review surfaces:

Mathematical specification:
    syntax, typing, primitive fibers, determinization, observable laws

Certificate specification:
    input alignment, replay acceptance, model correspondence,
    result equations and uniqueness/absorption condition

Unverified implementation boundaries:
    source translation, numerical execution, exploration/solving,
    external serialization and tool integration

Move ReplayValid and its structural predicates to a certificate-contract module, or document them as part of that surface despite their current path. Conversely, do not move every kernel construction, arbitrary null-trace completion, or reification helper into Spec: where its relationship to the public semantics is proved, its implementation is not another independent semantic choice.

Benefit: a reviewer knows what must be understood to validate each advertised guarantee.

Cost: maintaining a small explicit contract inventory. This is preferable to treating the entire import closure as the human-review surface.

2. Mathematical statements: the main issues are observable meaning and missing public consequences
2.1 The exported variance is not the variance most users will infer

Spec.varianceThm applies ProbabilityTheory.variance directly to an unnormalized real-output measure. The documentation acknowledges this, and Proof/Corollaries.lean: variance_id_eq_moments establishes the exact convention:

V(μ)=M
2
	​

−2M
1
2
	​

+mM
1
2
	​

,m=μ(R),M
k
	​

=∫x
k
dμ.

Thus it is the integral around the raw first moment, not around the conditional mean M
1
	​

/m. 

determinize-pro-architecture-so… +1

A decisive example is:

let _ = observe(flip(0.5)) in 3

Its output law is μ=
2
1
	​

δ
3
	​

. There are three different quantities:

Interpretation	Variance
Current variance id μ	9/8
Output conditioned on successful return	0
Output with rejected runs assigned numeric zero	9/4

The current inequality and trace decomposition can nevertheless be correct: source and target have equal mass and equal raw first moment, so the extra first-moment terms cancel when comparing their variances. That is also how TraceFactorization.variance_decomposition proves the result. 

determinize-pro-architecture-so…

Recommendation. Keep the current result, but make the acceptance-normalized theorem the ordinary user-facing variance result:

lean
noncomputable def acceptedOutputLaw (p : Expr) : Measure ℝ :=
  (bigStepMeasure p Set.univ)⁻¹ • bigStepMeasure p

Then expose, under the existing typing/source/safety/second-moment premises and positive output mass, probability-measure, expectation-preservation, variance-non-increase, and trace-variance-decomposition results for this law.

The definition need not take a positivity proof as a computational argument. Positivity belongs in the theorems that interpret it as a probability law.

Classification: important specification-interface improvement, not a refutation of the existing variance theorem.

Benefit: the theorem matches the CLI’s “among returned values” statistics and the natural statistical interpretation.

Cost: normalization and scaling lemmas, including the zero-mass boundary. Do not silently redefine the existing raw theorem, because its current formula is useful and already proved.

2.2 “Conditional expectation” currently means a quotient identity

conditionalExpectationThm states equality of

μ(R).toReal
∫xdμ
	​

.

It has no positive-mass premise and deliberately uses Lean’s zero-division convention when the output mass is zero. It does not construct a conditional-expectation random variable or establish the correctness or termination of a rejection-sampling implementation. 

determinize-pro-architecture-so…

This distinction matters in the presence of divergence. Positive acceptance probability does not make a naïve “run again after rejection” implementation almost surely finish: an individual attempt can diverge rather than reject.

Recommendation. Retain the total quotient lemma as a convenience, with a name such as acceptanceMeanQuotient_preserved. Add the positive-mass theorem about acceptedOutputLaw above, and describe it as expectation conditioned on successful return.

There is also stale terminology in the README: it advertises conditionalExpectationThm near the beginning, but later says “No conditioning theorem is claimed.” That should be replaced by the precise distinction between the proved quotient/normalized-law result and an unverified sampling procedure. 

determinize-pro-architecture-so… +1

Classification: interface clarification and documentation repair.

Cost: small for the quotient naming; moderate for a complete normalized-law API.

2.3 Rejection and divergence are identified by the mathematical observation

The paper reducer gives:

lean
reduce .reject = .next .reject

and .reject is not a value. Consequently, rejection is represented as an absorbing non-returning computation. Both its real-output law and its successful trace/output law are zero. The numerical runtime and finite machine, however, have explicit rejection outcomes. 

determinize-pro-architecture-so… +2

This is a coherent abstraction for output expectations, but it has a concrete consequence:

Model.Matches certifies safety and the successful real-output law. It does not certify rejection probability, rejection traces, completion probability including rejection, or execution time.

Its definition contains only those first two properties. The finite execution bridge’s rejected case likewise establishes zero output and safety. 

determinize-pro-architecture-so… +1

The public trace theorem also does not directly state “the same traces are rejected”: rejected executions contribute no trace to traceAndOutputLaw.

There are two defensible designs.

Keep the current abstraction. Rename or document Matches as matching the real-output semantics. Use “successful-return mass,” not unqualified “termination probability,” wherever rejection makes that distinction relevant. This requires no proof redesign.

Make rejection an observed outcome. Add a rejection-sensitive outcome law—returned real versus rejected—with divergence represented by missing mass. Prove that projecting away rejection recovers the current bigStepMeasure, and strengthen the finite correspondence accordingly.

I would choose the first now unless rejection-sensitive reasoning is an intended contribution. The second changes the semantic contract and touches the reducer/action interface, trace kernels, symbolic argument, and finite bridge. It is not a cosmetic replacement of one constructor.

Classification: an intentional but underemphasized modeling limitation; a richer rejection semantics would be a substantial extension.

2.4 outputGivenTrace is replay, not a conditional law for arbitrary inputs

At a G sample, outputGivenTraceAt consumes a matching primitive/value pair and forces that value. It neither checks support nor multiplies by the primitive’s sampling likelihood. 

determinize-pro-architecture-so…

For a typed uniform[G](0,1), replaying the trace [(uniform, 2)] can return 2, although that trace lies outside the actual trace law. This is harmless because the soundness assertion is almost everywhere.

More importantly, outside the affinity discipline, replay need not be conditioning at all. Consider the core shape

let x = bernoulli[E](0.5) in
let y = bernoulli[G](x) in
x

which the current typing rules correctly exclude. Conditioning the actual execution on y = 1 determines x = 1; simply forcing the G result during replay leaves the original E distribution on x. This illustrates why the typing hypotheses are doing essential work, rather than merely proving that replay terminates.

Recommendation. Prefer replayOutputLaw as the underlying definition’s name, and reserve “conditional output law” for the theorem under its hypotheses.

The public theorem should also expose facts already proved internally:

lean
-- With the current typing, source-form, and safety hypotheses:
∀ᵐ t ∂traceLaw p,
  IsProbabilityMeasure (outputGivenTrace p t)

traceAndOutputLaw p.determinize =
  (traceLaw p).map (fun t => (t, replayMean p t))

The second is already Proof.Traces.targetLaw. The proof also establishes almost-everywhere equality between raw replay and its Markov completion, but the public soundnessThm exposes neither the source replay’s mass-one property nor this direct target pushforward equation. 

determinize-pro-architecture-so… +1

Keep the existing target-replay Dirac statement as a named consequence; it is useful operationally. There is no need to replace the concrete replay definition by an existential kernel.

Benefit: users can apply the actual conditional-mean theorem without reconstructing probability and measurability facts from proof internals.

Cost: a few public bridge lemmas and more disciplined separation between canonical replay and generic factorization.

2.5 Infinite expectations and zero mass are handled with the right substantive premises

HasExpectation requires at least one of the positive and negative parts to be finite. That is the correct protection against interpreting a totalized extended-real subtraction as a meaningful expectation when both parts are infinite. The finite theorem separately requires Bochner integrability. These are different domains and should remain different. 

determinize-pro-architecture-so…

Likewise:

jensenThm correctly uses nonnegative convex test functions and lower integrals, avoiding an unnecessary global-integrability premise.

The lack of an explicit measurability premise on those globally convex real-valued functions is not a gap; the proof derives continuity.

varianceThm needs a finite second moment for its current real-valued formulation.

Zero output mass makes the expectation and successful-trace statements trivial, but that is the chosen semantics of rejection/divergence, not evidence of a vacuous specification. 

determinize-pro-architecture-so… +1

Recommendation. Add public compatibility lemmas connecting the finite and extended expectation interfaces, and explicit zero-mass lemmas. Do not collapse them into one total numeric function whose validity domain callers must remember informally.

3. E/G, means, typing, and primitive extensibility
3.1 Keep the three-way sampling action, but explain what affinities do not mean

DistributionAction now has exactly the three relevant cases:

sample E
sample G
mean

That is preferable to independent mode/kind fields with redundant or questionable combinations. A mean site does not need another affinity label; its result type follows its operands. 

determinize-pro-architecture-so…

However, neither G nor mean means “pure.”

For example,

let _ = uniform[E](0,1) in 1

can have result type float[G], although evaluating it performs an E draw. G describes allowed dependence of the resulting numeric value, not the absence of effects while computing it. Similarly, an atomic mean operation can evaluate arguments that sample, reject, or diverge.

This is especially important for Gaussian variance. Replacing

gauss[E](3, divergent_computation)

by literal 3 would be wrong even when the source is well typed and does not get stuck: divergence is safe in the specified sense. The current transformation correctly retains evaluation of the variance operand.

Remain unchanged: mean sites must evaluate every operand once, in source order, and retain the primitive’s domain check. The runtime test checking a G draw in a Gaussian variance operand is valuable, but add rejection and divergence variants too. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

3.2 Gaussian variance being G is a restriction worth challenging

The current rules require a G-typed variance for both Typed.gaussian and Typed.gaussianMean; Tests/MeanTyping.lean explicitly rejects an E variance. The analytic parameter tables correspondingly classify Gaussian mean as affine-position data and variance as general-position data. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

That restriction is not justified merely by the mean formula. The mean does not depend on variance at all.

A promising extension is to allow an E Gaussian’s variance to depend affinely on previous E values, while continuing to require G parameters for a G Gaussian. For example:

let v = uniform[E](1,2) in gauss[E](0,v)

would then be admissible.

The relevant obligations are concrete:

The valid variance domain v≥0 is convex under the affine substitutions used by the proof.

The conditional mean remains the Gaussian mean parameter.

The first absolute moment has the necessary growth bound; mathematically,

E∣N(m,v)∣≤∣m∣+
v
	​

≤∣m∣+1+v(v≥0).

The joint kernel remains measurable after reclassifying the variance coordinate.

Operand evaluation and domain checking remain strict.

This suggests Gaussian affine arity two, general arity zero, and mean coefficients (1,0). I have not implemented or verified that extension in Lean.

Classification: worthwhile language-generalization experiment, not a correctness defect in the current restricted theorem.

Benefit: a more principled account of which parameters may depend on eliminated samples.

Cost: changes to typing, symbolic parameter extraction, moment bounds, and the primitive-specific proof cases. Changing only the typing rule is insufficient.

3.3 Silent subtyping is appropriate; do not replace it with runtime casts

The direction G ≤ E, covariance of immutable products/sums/lists, and contravariance of function arguments are consistent across Ty.Sub, checkSubtype, and the type-safety proof. The higher-order canonical-form lemma typed_arr_value explicitly handles the subtype relationships needed when applying a lambda or recursive function. 

determinize-pro-architecture-so… +2

Subsumption belongs in the derivation/certificate, not in the expression semantics. Keep that design.

Two qualifications should be clearer.

Higher-order language support is not a theorem about arbitrary higher-order outputs. The expectation theorems concern closed real-output programs, which may internally use functions, recursive functions, lists, sums, and products. They do not provide a distributional semantics for returned closures or an observational-equivalence theorem at every type. Only terminal reals contribute to bigStepMeasure.

The symbolic typing relation is genuinely different from ordinary typing. AffineExpr.WellTyped.realG requires zero E coefficients. Replacing it by “every realization is ordinarily typed” would lose that condition, because an ordinary real literal can be typed G regardless of how it was obtained. 

determinize-pro-architecture-so…

The right simplification is to share syntax and binding machinery, not to erase the symbolic dependency invariant.

3.4 The asymmetric multiplication rule is a design tradeoff, not a mathematical necessity

The core permits G-left multiplication. The symbolic arithmetic already supports either operand being constant in the E coordinates: Affine.mul? checks both possibilities. 

determinize-pro-architecture-so… +1

A right-G typing rule could therefore preserve source evaluation order without commuting expressions. But it has a real cost: inference would acquire alternatives. For fun x => fun y => x*y, neither the G/E nor E/G assignment subsumes the other as the uniquely intended choice. In sums of products between sampled variables, maximizing eliminated sites becomes a selection problem over which pairs may simultaneously be E, rather than the current straightforward propagation of affinity constraints.

Recommendation. Treat symmetric multiplication as an optional expressiveness experiment. A certificate can cheaply record/check the chosen orientation, but that does not make inference-choice policy free.

Do not reorder arbitrary effectful operands to simulate the extra rule. Fix the literal-reordering/printing issue discussed below independently.

3.5 sourceForm is a phase restriction, not an analytic assumption

Expr.sourceForm excludes means everywhere, including dead branches and function bodies. The syntax, typing checker, exact machine, and semantics can nevertheless represent mean sites. 

determinize-pro-architecture-so…

The restriction is therefore primarily about the domain of the transformation theorem and the source-only symbolic typing relation. It prevents direct reuse of the theorem on mixed source/mean programs, which would matter for staged or incremental elimination.

I would retain the premise for the current theorem. Removing it requires extending the symbolic invariant and checking the mean cases, not deleting a hypothesis from a wrapper. For a one-shot frontend, this is a reasonable restriction. For a reusable transformation library, a later mixed-program theorem would be valuable.

Also publish elementary transformation laws—especially idempotence and commuting laws with literal maps and substitutions—in a lightweight module rather than leaving them dispersed through the soundness proof.

3.6 Primitive extensibility currently has a misleadingly partial abstraction

PrimitiveLaws packages probability, measurability-related kernel information, integrability, and the mean law for the fixed primitive catalog. But the complete argument also relies on:

mean affinity through meanConstant, meanCoeff, and meanValue_eq_affine;

domain stability, proved via domain_valueSet_convex and domain_at_meanEnvironment;

PrimitiveMomentBounds, establishing linear growth of first absolute moments in affine-position arguments.

Those last obligations are essential to the general argument but are outside the main law bundle. 

determinize-pro-architecture-so… +2

This matters when adding a distribution: having a finite mean at every fixed parameter is not enough to justify arbitrary nested E-dependent parameters. The moment-growth condition is what lets the proof integrate over the earlier samples.

Recommendation. First document a complete primitive-extension contract:

ordered arguments and E-admissible positions
valid parameter domain and its mean-stability property
measurable stochastic law, probability mass on-domain, zero off-domain
mean formula affine in the E-admissible parameters
first-absolute-moment growth sufficient for nested sampling
optional exact rational finite implementation

Keep the rational backend optional. A primitive can satisfy the mathematical theorem without having rational means or finite support suitable for the current explorer.

For implementation structure, I favor an incremental change over replacing every named constructor by a dependent generic prim node:

Introduce canonical per-primitive domain and mean helpers.

Make public fibers and generic analytic tables delegate to those helpers.

Keep explicit named primitive syntax and typing rules readable.

Isolate the exact rational implementation and prove its correspondence.

If a genuinely open primitive framework is a goal, then parameterize the signature itself, including ordered argument roles. Merely passing different PrimitiveLaws records whose kernels must equal the same fixed paperMeasure is not such a framework.

Cost: a genuine generic signature brings dependent arities and more involved pattern matching. The current concrete catalog avoids those costs; do not pay them solely to shorten case splits.

Finite distributions and trace labels

FiniteDistribution is a checked rational categorical law on outcome indices. Retaining zero-probability positions is meaningful, and the nonnegative/unit-sum fields are not expendable bookkeeping. The contrast with dynamically parameterized real Bernoulli probabilities should be stated explicitly. 

determinize-pro-architecture-so… +1

Op.discrete carries the entire distribution, whereas Op.uniform and Op.gaussian do not carry their parameters. That asymmetry affects trace equality and keeps the tag space countable because the stored probabilities are rational. Before adding dynamically parameterized categorical distributions, decide whether traces should identify a primitive family, a static distribution specification, or a source site. Do not accidentally put arbitrary real-valued parameters into a tag type currently equipped with the discrete measurable structure.

This is an extensibility issue, not a defect in the current fixed-program trace theorem.

4. Proof architecture: where the substantial simplifications are
4.1 Reuse Expr (Affine n) instead of maintaining a second syntax

This is the clearest structural opportunity.

Proof/Symbolic.lean: AffineExpr repeats the constructors of Spec.Syntax.Expr, with affine literals replacing ordinary literals. It then repeats realization, skeleton extraction, literal mapping, shifting, substitution, and other traversals. 

determinize-pro-architecture-so… +1

The conceptual interface is:

lean
abbrev AffineExpr (n : Nat) := Expr (Affine n)

def realize (ρ : Env n) (e : AffineExpr n) : Expr :=
  e.mapLiteral (fun a => a.eval ρ)

Likewise, ofExpr, mapAffine, and skeleton extraction should be instances of generic literal mapping. Binding operations should be the existing literal-polymorphic operations.

What should not be shared: the symbolic typing judgment and symbolic reducer’s arithmetic policy. Those encode actual additional invariants.

Benefit: adding a syntax constructor stops requiring another largely identical language definition and another collection of binding traversals.

Cost: existing constructor namespaces, recursors, induction scripts, and simp normal forms will change. I would first prove an isomorphism between the current AffineExpr n and Expr (Affine n), then migrate one group of structural lemmas. Do not combine this with changes to primitive syntax or the mathematical theorem.

Classification: high-value architecture change.

4.2 Remove phantom parameters before redesigning useful bundles

Two particularly concrete cases are:

Proof/Internal/Semantics.lean: Symbolic.SampleEnv laws n;

Proof/Symbolic.lean: SymbolicAction laws n.

Their constructors do not contain data depending on laws. The interpretation of a sample history depends on laws, but the history’s syntax does not. meanEnvironment is determined by the fixed mean formulas rather than by the kernel record. SymbolicAction.realize similarly uses the fixed primitive fiber. 

determinize-pro-architecture-so… +1

Use:

SampleEnv n
SampleEnv.actualMeasure laws history
SampleEnv.meanEnvironment history

SymbolicAction n

This removes type-level dependence without removing any invariant or requiring dependent reconstruction of data.

By contrast, SFiniteKernel is a useful package: it carries the instance required for composition. I would not remove it just because its second field is a proposition.

There are smaller redundancies:

MeasurableFamily.coordinate_count follows from skeleton_eq and the preceding realCoordinates_length.

StepKernel.terminal_measurable and terminal_value_measurable concern fixed global definitions, not the chosen kernel.

Several fixed-kernel bundles carry both an equality to the canonical law and facts derivable from that equality.

These can become derived lemmas or smart-constructor obligations. But they are lower priority than phantom parameters: redundant proof fields may be a good ergonomic cache. 

determinize-pro-architecture-so… +1

4.3 Consider arity-indexed parameters only at the narrow internal boundary where they help

SymbolicAction.sampleE stores two lists and then its WellTyped predicate stores their length equalities. Downstream proofs repeatedly convert those lists to Fin-indexed parameter functions using getD, and prove those conversions agree with checked indexing. 

determinize-pro-architecture-so… +1

A narrow alternative is:

sampleE
  (op : Op)
  (affine : Fin (affineArity op) → Affine n)
  (general : Fin (generalArity op) → ℝ)
  (continuation : AffineExpr (n+1))

Here the dependency is on primitive data, not on a validity proof. The rest of the proof already uses these indexed parameter functions.

Benefit: removes repeated arity witnesses and list-to-parameter conversions.

Cost: dependent pattern matching may still make inversion and simplification worse. This should be a separate pilot, with the current list representation as the baseline. I would not generalize this recommendation to intrinsically typed expressions, all machine states, or terminal rows.

4.4 The detailed and compact replay developments repeat the central argument

SymbolicTraceSoundness.exactDepth_fiberSound proves the detailed-trace lockstep result. CompactFiberSoundness.compact_exactDepth_fiberSound repeats that induction for compact replay; the latter file’s introductory comment explicitly describes the repetition. Both use the same symbolic safety extension and mean-environment argument. 

determinize-pro-architecture-so… +2

Furthermore, CompactSoundness.soundnessDataE invokes the detailed soundness theorem for target safety and separately constructs the compact factorization. Target safety already has an independent theorem in SymbolicSoundness. 

determinize-pro-architecture-so… +1

Recommendation. Aim for one central lockstep proof.

My first experiment would be conservative: keep detailed joint traces as an internal device for exact-depth accounting, but make the compact replay proof the principal factorization path and obtain target safety directly. Then determine which detailed replay/factorization modules are still needed.

A second possibility is a shared induction theorem parameterized by the trace observation/replay operations. That could unify both developments, but a highly abstract trace algebra may be harder to audit than one concrete proof plus a projection theorem.

Benefit: one place to update the mathematical argument when adding means, primitives, or syntax.

Cost: exact-depth bookkeeping and measurability cannot simply disappear. I am not recommending deleting detailed traces merely because the public trace omits silent steps.

Classification: major proof-architecture experiment, with a plausible payoff but a real abstraction risk.

4.5 Move generic probability facts out of language-specific proof modules

Examples include:

SFiniteKernel and its composition helpers in Proof/Measurability.lean;

sfiniteKernel_mapWithInput_apply and bind_bind_const_swap at the end of SymbolicSoundness.TargetSafety;

map_bind and bind_map in TraceSemantics;

the first-/second-moment calculations in Corollaries.

These facts are not about symbolic target safety or a particular programming language. Their current placement creates dependencies unrelated to their meaning. 

determinize-pro-architecture-so… +2

The same applies at the conceptual level. Most of Corollaries needs a finite trace measure ν, a Markov kernel κ, and the two laws

μ
source
	​

=κ∘
m
	​

ν,μ
target
	​

=ν.map(t↦∫xdκ(t)),

with almost-everywhere fiber integrability. It does not fundamentally need Expr, reduce, or the particular Trace datatype.

Recommendation. Separate a generic mean-factorization probability lemma layer from the language-specific theorem that constructs the factorization.

Benefit: normalized-law results, variance identities, and finite-law examples can be developed without importing the symbolic compiler proof.

Cost: some explicit finite-measure/kernel instances and adapters. This is genuine abstraction: unlike the current arbitrary laws parameters tied to a fixed paperMeasure, it captures mathematical data that can actually vary.

4.6 Cut the import dependencies before attempting a new semantics

The problematic dependencies are broader than the two identified initially.

Current dependency	Why it is misplaced	Suggested destination
Spec.Syntax → Spec.Primitives	Syntax needs primitive names and finite-distribution data, not all distribution measures.	A lightweight primitive-name/signature module.
Proof.Typing → Proof.Measurability	Type safety uses elementary reduce_*_eq equations housed in the large measurability module.	Proof/ReductionEquations.lean.
Proof.FiniteModel.Initial → Proof.Checking.Elaboration → Theorems	Initial reification needs literal-map/determinization correspondence, not all global soundness.	Lightweight literal/syntax laws.
Proof.FiniteModel.MeasureLaws → OrdinarySemantics → SymbolicSoundness	Direct evaluator measurability should not require the determinization proof.	Separate ordinary semantic facts from determinization typing/transport.
Proof.FiniteModel.Replay → Finite.Explore	Candidate data is defined with the unverified explorer.	Finite/Data.lean.
Checking.Result → Theorems	Generic result checking imports source-result transfer.	Move transfer to an application/composition module.
Finite.Export → Frontend.Pretty	Kernel constructor serialization is not frontend pretty printing.	A shared Export/LeanSyntax.lean.

The typing dependency is particularly concrete: reduce_typed_closed repeatedly uses MeasurableActionFamily.reduce_app_eq, reduce_fst_eq, and related equations, although those equalities are elementary operational facts. 

determinize-pro-architecture-so…

Benefit: finite certification becomes a client of ordinary semantics, not of the entire determinization development. Basic typing and syntax laws become usable without large analytic proof imports.

Cost: mostly declaration movement and import repair. This is the safest substantial architecture change to perform first.

Keep the readable direct output semantics in Spec. The equality proofs connecting it to internal kernels are useful; not every second presentation of a semantics is harmful duplication. The target should be fewer independently maintained semantic recursions and better-separated bridges, not one enormous generic evaluator.

5. Finite replay, absorption, and exact-result interfaces
5.1 The replay contract has the important safeguards; preserve them

RowReplays checks complete positive successor coverage and exact transition weights. ReplayValid additionally checks initial-state alignment, source scoping, unique stored states, dimensions, matrix validity, and sparse-edge validity. The unbounded coverage theorem is not merely an exploration-horizon claim. 

determinize-pro-architecture-so… +1

Several safeguards become especially important because the reifier is total:

Binding.close replaces a missing environment entry by .reject.

primitiveExpr replaces malformed argument lists by .reject.

valueExpr_closed therefore holds even for arbitrary raw machine values.

The scoping and reachable-shape proofs prevent those defaults from silently changing the accepted source program. The supplemental test with a free variable in an unused closure is an important specification regression: exploration finishes, but replay rejects the candidate. 

determinize-pro-architecture-so… +2

 

determinize-pro-cli-tests

Remain unchanged: complete positive coverage, exact weights, initial-source/subject alignment, a scoping safeguard, and the no-infinite-bookkeeping argument.

Also retain the separation between typing and finite replay. The exact checker can establish safety and the output law of a closed core program without a typing derivation. Forcing every generic checkModel call through Certified would unnecessarily narrow that interface.

5.2 Explicit terminal rows are a reasonable representation choice

The model semantics ignores terminal transition rows; the raw candidate format requires absorbing self-loops. The supplemental terminalWithUnusedRow example confirms that these are deliberately different levels of the design. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

After your experiment, I would leave this representation alone.

A harmless convenience is a raw constructor that creates #[⟨i,1⟩] for terminal candidate rows. That does not require defining transition data through dependent validity proofs.

Similarly, initial_lt follows from successful initial lookup, and some matrix facts follow from positive validated edges. But these small redundant proofs can make toModel straightforward. Their removal is not a worthwhile goal unless an equivalent checker demonstrably becomes simpler or cheaper.

Classification: retain the current design; only optional local conveniences.

5.3 Optimize checking algorithms without first changing the model contract

There is a more consequential issue than redundant fields: sparse candidate rows are repeatedly viewed as a dense matrix.

Candidate.weight i j scans the sparse row, and replay/matrix checks quantify over all state pairs. State uniqueness and successor matching also compare potentially large structural machine states. 

determinize-pro-architecture-so…

Recommendation. Preserve ReplayValid as the declarative contract and experiment with a faster Boolean checker proved equivalent to it.

For example, supplied successor-index hints can avoid searching every stored state for each successor, provided the checker independently:

computes the machine step;

checks the indexed state equals the actual successor;

accounts for every positive outcome;

aggregates weights correctly;

retains the necessary global uniqueness/representation checks.

The hints are untrusted data, not replacements for coverage.

Benefit: performance work can be validated against a stable logical specification.

Cost: proof of equivalence, especially around duplicate outcomes, zero weights, and index lookup. Do not assume that compiled speedups automatically translate into faster decide +kernel.

The manually implemented equality machinery in Proof/FiniteModel/Equality.lean is explicitly intended to avoid problematic reduction behavior. It should not be replaced by a derived instance merely for stylistic uniformity. 

determinize-pro-architecture-so…

5.4 Keep a small horizon hint; model.size is a bound, not a good default

checkResult uses survivalVector so that each horizon stage materializes the preceding vector. The solver searches for a small horizon that already has positive absorption probability from every state. 

determinize-pro-architecture-so… +1

For a finite graph in which every state can reach a terminal state, a simple-path argument supplies a state-count bound. But that does not justify computing to that bound.

The definition performs a dense matrix-vector stage for each horizon increment. In an implementation retaining vectors, that is roughly hN
2
 arithmetic operations before accounting for rational bit sizes, transition lookup, and kernel reduction. Replacing h=1 or h=12 by a large N can be an expensive way to eliminate one natural-number field.

Recommendation. Keep horizon. Prove a canonical upper-bound lemma for completeness/documentation, not as a mandate to use it during checking.

A small local improvement is to consider horizon zero for all-terminal models: the checker accepts it, but solve starts its search at one. This is not a significant architectural concern. 

determinize-pro-cli-tests

A more promising alternative, if absorption checking is expensive

Use a positive-edge escape ranking certificate. For every transient state, provide a successor with positive transition probability and strictly smaller natural-number rank. Other edges may cycle.

This witnesses a finite positive-probability route to a terminal state without computing survival probabilities. Checking it requires only rank comparisons and selected-edge checks in addition to the existing value equations.

There are two proof routes: derive the existing finite-horizon absorption condition, or prove uniqueness directly by propagating a maximal absolute difference of two equation solutions along the selected positive edges until reaching a terminal state.

Benefit: potentially much cheaper kernel replay than repeated rational matrix-vector multiplication.

Cost: a new certificate form and uniqueness proof. It contains more data than a horizon, which is precisely why field count is the wrong optimization metric.

Treat this as an isolated experiment after profiling, not an immediate replacement.

5.5 Absorption is for uniqueness, not integrability

outputMeasure_integrable proves that every finite model’s real-output law is integrable, including nonabsorbing models. resultCertificate_sound uses absorption to exclude spurious solutions of the value equations. 

determinize-pro-architecture-so… +1

That distinction should be explicit in the result API. The value equations alone are insufficient; the transient self-loop tests correctly demonstrate this.

There are two limitations worth separating from soundness.

All-state absorption is stronger than needed for the initial answer. Replay permits unique unreachable states, but adding an unreachable nonabsorbing component can prevent a result certificate. The explorer normally generates reachable states, so this is mostly an issue for the generic model/certificate API. 

determinize-pro-cli-tests

Nonabsorbing models can still have exact, finite expected output. Rejecting them is a limitation of the current result certificate, not an analytic necessity.

A principled extension is to certify a closed set of states with zero output: it contains no returned states and no positive transition leaves it. Set their values to zero, and certify escape from the remaining states to either genuine terminals or that zero-output set. This would accommodate closed divergent components without accepting arbitrary harmonic solutions.

Cost: additional graph analysis in the unverified solver and a checked closure/uniqueness argument. Keep the current absorbing certificate as the simpler baseline.

5.6 The endpoint should expose an answer, not certificate mechanics

The current composition theorem is mathematically appropriate, but callers must thread a CheckedModel, its dependent ResultCertificate, acceptance, numeric typing, source form, safety, and integrability. Some are genuine new premises; others were already established elsewhere. 

determinize-pro-architecture-so…

A useful application-facing result is:

lean
structure ExactResult (p : Expr) where
  value : Rat
  safe : DoesNotGetStuck p
  integrable : Integrable id (bigStepMeasure p)
  integral_eq :
    (∫ x, x ∂bigStepMeasure p) = (value : ℝ)

Construct this from the existing model/result checkers. Separately provide a numeric view of Certified that supplies typing at .float .E—using subsumption for G results—and source form.

That numeric view must retain access to the original alignment and explicit-affinity guarantees. It is not a replacement that discards provenance.

The source-transfer interface should then require only the genuinely additional source safety and source integrability facts. The generic exact checker should remain usable independently of the frontend.

Benefit: a client proving a source answer sees precisely the unresolved mathematics, not state-vector and type-tag bookkeeping.

Cost: adapters and a small stable application module; no change to the core theorem.

A related extension would certify a chosen observable of terminal outputs rather than only identity. Acceptance mass, first moment, and second moment would support exact conditional means and variances using the same model and absorption evidence. Keep the observable distinct from the model’s returned numeric value; changing terminal values to encode rewards would otherwise obscure which output law the model matches.

6. Frontend, runtime, CLI, and exports
6.1 The positional affinity representation remains the wrong long-term boundary

The elaborated Input stores a core expression with concrete sample labels plus a separate preorder list of optional requests. Inference consumes that list, and Certified checks the resulting list against it. This is implemented consistently in the inspected cases, but the correspondence is structural bookkeeping spread across several traversals. 

determinize-pro-architecture-so… +2

Recommendation. Put optional requests at their sites in the elaboration-input syntax and define a structural Annotates input core relation. A site-parameterized syntax could share the generic tree rather than introducing another fully duplicated AST.

Benefit: changing traversal order or adding a primitive cannot silently desynchronize an external positional list.

Cost: a syntax-parameterization/migration step. Coordinate it with the generic-syntax work rather than introducing a second temporary representation.

Do not mistake this improvement for verified source parsing: the text-to-input correspondence remains a separate boundary.

6.2 Keep proof-producing checking; do not make inference part of the mathematical trust argument

check returns a proof of the existing Typed judgment, and certify checks source form and input preservation. This is the correct separation. The tests exercise changed literals, affinity violations, malformed typing evidence, and contravariant function subtyping. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

The certificate’s per-node types are verbose, but some annotation data is necessary for erased higher-order syntax. I would not replace this with an intrinsically typed AST or demand inference completeness as part of this audit.

More useful improvements are better error localization, avoiding unnecessary reconstruction, and publishing a theorem facade that does not ask users to re-prove the inferred result type is numeric.

6.3 There is a concrete structural pretty-printing roundtrip failure

Frontend.Elaborate.lower always swaps a multiplication whose right operand is a literal. Frontend.Pretty.render prints the resulting multiplication normally. Therefore, by unfolding those definitions:

compile "2 * 3"       produces core 3 * 2
pretty that core      prints       3 * 2
compile the printing produces core 2 * 3

The erased core syntax changes on each roundtrip. The tested property in Tests/Parsing.lean would fail on this additional fixture, although the two programs have the same numeric meaning. 

determinize-pro-architecture-so… +1

 

determinize-pro-cli-tests

There are other scope limits: arbitrary rational categorical probabilities may print as division expressions, while the discrete frontend expects literal weights; a standalone polymorphic .reject prints as unit-valued observe(false).

Recommendation. Specify separate contracts for display, source roundtrip, and exact Lean constructor serialization. Either make source normalization idempotent and test that contract, or test semantic roundtrip rather than accidental syntactic equality. Tests of explicit annotations should not erase the very annotations whose preservation matters.

Classification: a concrete structural-roundtrip defect relative to the tested property; not an exact-certificate soundness defect. leanExpression is a separate constructor serializer.

6.4 The CLI statistics can fail even when every sample is finite

Main.summarize accumulates sum and squares, then computes variance as

squares / count - mean * mean

and clamps below at zero. Finite individual samples do not imply finite accumulated squares, and subtracting large nearby quantities can destroy the variance estimate. A constant sufficiently large finite value already makes the squared calculation nonfinite. 

determinize-pro-cli-tests

Tests/Corpus.lean already uses an online centered-moment calculation. Reuse that approach and check accumulator finiteness rather than relying on the final clamp. 

determinize-pro-cli-tests

Classification: numerical implementation defect/robustness problem, separate from the verified mathematical theorem.

The runtime’s explicit distinction between rejection and execution failure should remain. Its PRNG streams, Float arithmetic, bounded rejection algorithms, and fuel are not established by the real-measure theorem. Sharing basic value syntax might eventually help, but unifying the exact CEK engine and numerical recursive evaluator is not an obvious simplification: their stepping and failure contracts differ.

6.5 The CLI now resolves the earlier wiring uncertainty

The supplied Main.lean compiles through the checked frontend, explores the requested source/target, calls the checked export/result path, and labels numerical estimates separately. It does not independently invoke Lean on the generated certificate. The generated file is kernel-checkable; writing it is a different event from replaying it. 

determinize-pro-cli-tests

There is avoidable repeated work:

certificateText recompiles the input instead of exporting the already compiled certificate data.

writeResult checks the model, then calls write, which checks it again.

Survival information is recomputed for metadata after result checking.

These are good local refactoring targets, without changing the independent kernel replay of exported data. 

determinize-pro-architecture-so… +1

The external .tra/reward serialization is another boundary. render adds a fresh sink so terminal rewards are paid once and splits signed rewards into positive and negative files. The theorem is about the embedded candidate/model and selected core, not directly about those external bytes. The sink test is useful, but it is not a serialization-correctness theorem. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

Finally, the writes are sequential rather than transactional. Validation failures occur before export, but an IO failure can leave a partial set of files. That is an optional robustness improvement, not a mathematical issue.

7. Coverage inventory
Every specification file
File	Declarations reviewed	Assessment
Spec/Types.lean	Affinity, DistributionAction, Ty, Ty.Sub	Keep the three action states and silent contravariant subtyping. Explain dependency versus effects. float denotes mathematical reals here, not IEEE Float.
Spec/Syntax.lean	Expr, values, source form, variable/literal maps, substitution, determinization, HasVar, Typed	Clear concrete syntax, but combines syntax and typing and imports analytic primitive definitions. Share this syntax with the symbolic layer. Source-only restriction and asymmetric multiplication are deliberate scope choices.
Spec/Primitives.lean	Op, all eight primitive fibers, uniformMeasure	Domains and parameterizations are explicit. Keep strict mean evaluation/domain checks and degenerate laws. Reconsider Gaussian variance admissibility and separate primitive tags from analytic imports.
Spec/Semantics.lean	Action, reduce, cumulative/output laws, safety predicates	Correctly separates cumulative output from exact-depth counting. Its observation forgets rejection versus divergence and non-real terminal values. Keep the direct reviewer-facing evaluator.
Spec/FiniteDistribution.lean	Checked probabilities, expectation, mean	Rational categorical-index semantics is specific and coherent. Preserve zeros and unit-sum checks; do not silently reinterpret weights as unnormalized scores.
Spec/FiniteDistributionMeasure.lean	Real measure of the finite law	Appropriate explicit Rat-to-real bridge. The arbitrary Nat → ℝ value function is harmless outside the supported index range; changing to Fin is optional ergonomics.
Spec/Main.lean	All six propositions; positive/negative parts, HasExpectation, extended expectation	Genuine premises should remain. Main improvements are normalized-law interfaces, precise conditional terminology, and finite/extended compatibility lemmas.
Spec/Traces/Semantics.lean	Trace measurable structure, recording, exact-depth joint law, replay	Successful finite G traces only; replay is not unconditional conditioning. Preserve exact-depth guards and almost-everywhere interpretation.
Spec/Traces/Main.lean	Erasure, trace law, composition, replay mean, soundness, variance decomposition	Canonical operational definitions are good. Expose source replay probability, measurability, and target pushforward directly.
Spec/FiniteModel/Model.lean	State kinds, model, survival/output laws, expectation, Matches	A real-output model with terminal rows ignored. Keep the representation; make correspondence scope explicit.
Spec/FiniteModel/Certificates.lean	Subject selection, values/horizon, equations, absorption, validity	Soundness-oriented absorbing certificate. Keep the hint; separate mathematical uniqueness from implementation of its checker.
Every headline exported theorem

All nine aliases in Theorems.lean were checked against their propositions and proof entry points. 

determinize-pro-architecture-so…

Export	What it establishes	Main recommendation
expectationPreservation	Source safety/integrability imply target safety/integrability and equal raw first moments.	Keep; improve certified-source adapter.
extendedExpectationPreservation	Defined possibly infinite expectation is preserved.	Keep HasExpectation; expose finite compatibility.
jensenInequality	Lower-integral inequality for nonnegative real-valued convex tests.	Keep this no-global-integrability result.
outputMassPreservation	Equal successful real-return mass.	Avoid calling this rejection-sensitive completion probability.
varianceNonIncrease	Target finite second moment and raw second-moment/variance inequalities.	Add acceptance-normalized variance.
conditionalExpectationPreservation	Equality of totalized first-moment/mass quotients.	Distinguish the quotient convention from conditioning on a positive-probability event.
traceErasure	Erasing successful trace information recovers the real-output law, for all expressions.	Keep its hypothesis-free scope.
traceSoundness	Canonical replay factorization and almost-everywhere target Dirac mean.	Make this the central public theorem; expose its probability and pushforward consequences.
traceVarianceDecomposition	Raw variance decomposes into target variance plus integrated replay variance.	Add the normalized version and expose the relevant fiber facts.

The other public certification contracts reviewed were Certified/certify, proof-producing typing/subtyping checks, their soundness adapters, checkModelReplay, CheckedModel/checkModel, checkResult_valid, checkResult_sound, checked_expectedReward, checked_sourceExpectedReward, and the generated machineReplay, modelMatches, resultAccepted, expectedReward, typing/alignment, and trace-guarantee exports.

Major subsystems
Subsystem	Coverage and principal conclusion
Proof/Internal/*	All five modules’ central definitions inspected. The expression coding is an internal measurable presentation; the history/action law parameters and duplicated safety interfaces deserve simplification.
Ordinary semantics and typing proofs	Measurability, OrdinarySemantics, Typing, Subtyping, Rejection inspected at architecture/contracts and relevant proof steps. Separate reduction equations, generic kernels, and ordinary semantic facts.
Primitive analysis	PrimitiveKernels, PrimitiveMoments, DiscreteLaws, FiniteDistributionMeasure: law construction and bridge statements inspected, with selected analytic proof bodies. No independent audit of imported Mathlib analysis.
Symbolic development	Symbolic, SymbolicSoundness, SymbolicMoments, and all five SymbolicTrace* modules: central invariants, transitions, and soundness paths inspected. Share syntax, retain symbolic dependency typing, consolidate the lockstep proof.
Detailed/compact traces and corollaries	Replay, Trace*, all four Compact* modules, Soundness, Corollaries: semantic definitions, factorization interfaces, and main bridges inspected. Keep canonical public replay; remove duplication and misleading normalization names.
Proof/FiniteModel/*	All twenty modules examined for definitions, contracts, and bridge structure, including reification, scoping, local correspondence, reachable shapes, bookkeeping progress, graph replay, and result uniqueness. Preserve the safeguards; reduce import coupling.
Proof/Checking/*, examples/interface checks	Both adapters and the example/interface-check modules inspected. Their public-import checks are useful; elementary safety/moment lemmas should not remain private example infrastructure.
Checking/*	All six files read. Keep proof-producing checking; separate generic result certification from determinization-specific transfer.
Frontend/*	All seven files read. Positional metadata, stringly tagged surface syntax, repeated compilation, and roundtrip contracts are the main issues.
Finite/*	All five files read. Exact execution is distinct from numerical runtime; representation/certificate cost needs measurement. Parameter-sensitive finite support would remove artificial exclusions.
Runtime/*, Main.lean	Read in full. Keep explicit trust boundaries and rejection outcomes; fix statistics accumulation and redundant orchestration.
Supplemental tests	All fourteen read: Checking, Corpus, Explorer, FiniteDistribution, FiniteModel, Inference, Main, MeanTyping, ModelReplay, Parsing, PrimitiveLaws, Results, Runtime, SemanticBridge. Strong mutation coverage; add semantic-boundary fixtures rather than only more positive compilation tests.
8. Concrete next experiments
Experiment 1: strengthen the public observable contract without changing the engine

Add acceptance-normalized laws and named replay probability/pushforward lemmas. Prove examples covering 
2
1
	​

δ
3
	​

, zero output mass, a safe divergent program, a genuinely infinite expectation, and an undefined signed expectation.

Success criterion: clients can state ordinary conditional expectation and variance claims without importing symbolic proof modules or informally interpreting zero-division conventions. Existing nine propositions remain unchanged.

Experiment 2: cut imports with no representation changes

Move candidate data, literal/determinization commuting laws, reduction equations, generic kernel lemmas, and source-result transfer to the appropriate layers.

Success criterion: the generic finite checker/result path no longer imports global determinization soundness; theorem statements and accepted certificate behavior remain unchanged. Measure build/import effects rather than assuming them.

Experiment 3: remove phantom parameters, then pilot shared symbolic syntax

First remove laws from SampleEnv and SymbolicAction. Separately migrate structural symbolic operations to Expr (Affine n) through a proved correspondence.

Success criterion: fewer duplicated binding/traversal lemmas without replacing the meaningful symbolic typing invariant or increasing coercion-heavy proof scripts. Reject a migration that merely exchanges obvious duplication for difficult dependent transports.

Experiment 4: establish one factorization proof path

Expose the compact canonical facts, obtain target safety directly, and test whether the detailed replay/factorization path can be removed from the production dependency chain while retaining detailed joint traces for depth accounting.

Success criterion: one substantive lockstep argument, with the same public trace theorem. Do not count moving duplicate arguments to different files as success.

Experiment 5: profile certificate checking before altering certificate data

Measure separately state uniqueness, machine replay, dense weight checks, result equations, and absorption checking. Compare the current small horizon against model.size only as an ablation. If absorption dominates, prototype positive-edge rankings; if replay dominates, prototype a sparse checker proved equivalent to ReplayValid.

Success criterion: independently kernel-checked certificates with unchanged semantic guarantees and a measured reduction in checking cost. Keep explicit terminal rows.

Experiment 6: test one principled primitive extension

Use E-dependent Gaussian variance as the first extension. It tests whether the proposed primitive contract really accounts for domain stability, affine means, moment growth, and joint measurability. Keep symmetric multiplication separate because it changes inference-choice policy.

A smaller independent finite-backend extension is to accept evaluated degenerate stochastic laws such as point uniforms, zero-variance Gaussians, and zero-rate Poisson calls. supportedDraw currently rejects these by primitive name even though their evaluated laws can be finite; the semantic bridge test explicitly records that limitation. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

What should remain unchanged

The main premises are substantive. Retain source safety, source integrability for finite expectation, HasExpectation for extended expectation, and MemLp for the current finite-variance statement. Retain almost-everywhere reasoning rather than demanding validity at every forced real trace value.

Keep the three-way sampling action, silent structural subtyping, real-valued mathematical arithmetic, strict operand evaluation at mean sites, and explicit primitive domains. Keep the readable ordinary and compact-trace definitions as the mathematical specification.

Keep independent proof-producing typing and exact finite replay. Keep full output-law correspondence, complete positive successor coverage, initial-program alignment, scoping, reachable-shape invariants, and bookkeeping progress. Keep the small absorption hint and the straightforward terminal-row representation until measurements justify a genuinely better alternative.

The best sequence is therefore: clarify and expose the mathematical contract; cut accidental dependencies; share structural syntax and probability infrastructure; then optimize certificate checking and broaden the language. That addresses the development’s largest auditability and maintenance costs without confusing a smaller datatype with a better specification.

Sources
Is this conversation helpful so far?
ChatGPT can make mistakes. Check important info.

6
Pro