# Independent Pro full adjudication (rendered text)

Overall adjudication

The review identifies several real opportunities, but its proposed priorities and the certainty of some architectural benefits need revision. The strongest immediate work is the two executable repairs, narrower documentation, a few public replay and premise-discharge lemmas, and coordinated import cuts. Shared symbolic syntax, trace-proof consolidation, and faster certificate checking survive as bounded experiments, not as changes whose overall benefit has already been demonstrated.

I use P0 for incorrect executable behavior or reporting; P1 for small, directly useful interface or dependency repairs; P2 for maintenance/performance changes requiring a measured trial; and P3 for deliberate extensions. These are priorities within this review, not a recommendation to implement everything.

This is a source-level assessment. I inspected the relevant declarations and the proof bodies discussed below and traced the bundled module imports. I did not compile Lean, rerun your executable reproductions, or implement any proposed change.

1. The two executable findings
1.1 Literal multiplication roundtrip — KEEP, P0

The diagnosis is exact: Frontend.Elaborate.lower swaps whenever the right elaborated expression is .real, including when the left is also .real; Frontend.Pretty.render prints the resulting multiplication in its stored order. The existing test contract compares the two compiled sources after affinity erasure. Your reproduction exposes a missing case in that test, not a failure of the typing certificate. 

determinize-pro-architecture-so… +1

 

determinize-pro-cli-tests

Minimal repair: move a right literal left only when the left operand is not already a literal. Retain the order when both are literals. This makes this particular normalization idempotent while preserving the reason for it: a literal can supply the core multiplication rule’s G-left operand.

Add repeated roundtrips for literal–literal multiplication, E-expression–literal multiplication, and nested examples; check the sample affinities as well as erased syntax. Do not replace the structural test with equality of numeric answers: that would weaken a useful check and could conceal changes to binders or evaluation order.

Cost and dependency: local elaboration and regression-test changes; no new typing rule, symbolic arithmetic, or soundness proof. Symmetric multiplication is not a prerequisite.

The broader printer recommendation needs qualification. KEEP separate contracts for display, source roundtrip, and exact Lean serialization; DROP an unrestricted structural-roundtrip promise for every Core. For example, .reject prints as observe(false), and a nonterminating-decimal rational prints as a division expression, whereas discrete elaboration requires literal weights. leanExpression is a separate constructor serializer and should remain the exact-export route. 

determinize-pro-architecture-so… +2

1.2 CLI variance — REVISE the proposed fix, P0

Replacing raw squares by a centered online accumulator is appropriate, but “use Welford” is not a complete repair. Main.summarize currently calculates squares / count - mean * mean and then clamps with max 0.0; nothing checks these intermediate results. Your nonconstant fixture demonstrates incorrect reporting, rather than merely an unnecessarily unstable computation. 

determinize-pro-cli-tests

The smallest adequate repair has two parts:

Use centered accumulation rather than subtracting two large second-moment terms.

Check the accumulator and final result for nonfinite values before any clamp, and report the statistic as unavailable because of floating-point overflow or invalid accumulation.

Keep counting all returned numeric samples. Do not silently discard the samples that make a statistic overflow, and do not turn an unavailable variance into zero. A valid mean can still be reported when variance accumulation fails.

There is an important compatibility detail: the CLI currently reports empirical population variance, dividing by count; Tests/Corpus.statistical divides by samples - 1. Reusing its accumulation method should not silently copy its denominator and change the CLI statistic. 

determinize-pro-cli-tests

Your two-valued example has a true empirical variance beyond Float range. More generally, an intermediate overflow does not itself prove that the final mathematical variance is unrepresentable. Therefore the general diagnostic should be “unavailable: nonfinite accumulation,” not a claim that the mathematical variance is infinite.

Cost and dependency: a small statistics helper and focused tests: a large constant, your large two-valued sample, and ordinary small variation around a large mean. No probability API redesign or formal Float-correctness project is required.

2. Public mathematical interface and review boundary
2.1 Acceptance normalization — REVISE; DROP default-interface promotion; DEFER the suite

The focused conclusion stands. The current second-moment inequality, equal first moment, equal mass, and quotient theorem already supply the mathematics. The exact CLI result intentionally certifies unnormalized terminal reward. A normalized-law endpoint would be a convenience, not a repair to the determinization theorem. 

determinize-pro-architecture-so… +1

Minimal change: repair the interpretation and rejection-sampling wording. Retain conditionalExpectationPreservation; a migration to acceptanceMeanQuotient_preserved is not justified. Add a generic normalization bridge only when a client needs its scaled-measure conclusion.

Cost and priority: documentation is P1. A full parallel program-indexed expectation/variance/decomposition API remains deferred; it should not precede the concrete repairs above.

2.2 Public replay probability, measurability, and pushforward facts — KEEP, P1

This recommendation survives more strongly than normalization because it exposes the central operational result directly.

Proof.Traces.targetLaw already states the target joint law as the pushforward through replayMean. soundnessDataE supplies almost-everywhere equality of source replay with its Markov completion. However, Spec.Traces.soundnessThm does not explicitly expose source replay probability. 

determinize-pro-architecture-so… +2

Minimal useful change: additive, named public lemmas for:

almost-everywhere IsProbabilityMeasure (outputGivenTrace p t);

the existing canonical target pushforward equation;

replay/replay-mean measurability needed to use those statements.

The existing outputGivenTraceKernel, its application equality, and its s-finiteness instance provide the implementation bridge for measurability. No measurable structure on public Expr needs to be introduced. 

determinize-pro-architecture-so…

Keep the target-replay Dirac statement: it answers a different operational question from the pushforward equation. Do not replace concrete replay with an existential kernel.

Qualification: targetLaw is already a usable Lean theorem, not missing mathematics. The improvement is a stable, discoverable public entry point rather than requiring clients to select declarations from proof internals. Do not change the shape of the nine existing propositions merely to add these consequences.

Cost: small proof adapters and naming decisions; no new lockstep argument and no dependency on normalization.

2.3 Replay and “normalization” names — REVISE, P1 when touching the interface

outputGivenTraceAt forces matching G values without checking their support or multiplying by their likelihood. Its definition is replay; its conditional-law interpretation requires the typing, source-form, safety, and almost-everywhere qualifications. The reviewer is correct about that distinction. 

determinize-pro-architecture-so…

Nevertheless, a breaking rename to replayOutputLaw is not necessary. Prefer clearer documentation and, if useful, an alias.

The more misleading names are the internal normalizedOutputGivenTrace and normalizedReplay. They do not divide by mass. They retain a replay law where its mass is one and substitute δ₀ elsewhere. “Markov completion” describes that construction accurately. 

determinize-pro-architecture-so… +1

Minimal change: use completion terminology internally, with compatibility aliases where needed. Do not expose the arbitrary off-support δ₀ choice as another principal public semantic object.

Cost: mostly names and references. Its benefit is preventing confusion with acceptance normalization, not reducing the number of kernels.

2.4 Successful-output semantics and certification review surface — KEEP the inventory; REVISE mandatory relocation, P1

The proposed distinction between mathematical, certificate-acceptance, and external-interpretation contracts is useful. But it should not imply that every intermediate checker predicate becomes an independently trusted mathematical assumption.

Model.Matches already says exactly what it contains: source safety and equality of the successful real-output law. CheckedModel.correct proves that proposition for the caller-selected core program. ReplayValid determines which certificates are accepted, but is not an unchecked premise secretly added to the final semantic theorem. 

determinize-pro-architecture-so… +1

The review boundary depends on the claim:

For the meaning of a certified answer, review the embedded core expression, public output semantics, and final theorem statement.

For claims about accepted inputs and certificate behavior, additionally review Certified, affinity alignment, ReplayValid, scoping, supported finite calls, and result validity.

For claims about source text, numerical execution, or external files, review those separate, currently unverified correspondences.

That is more precise than either “only read Spec” or “read every definition in the import closure.”

Minimal change: maintain an explicit contract inventory. Moving ReplayValid into a clearly named certificate-contract module is reasonable during dependency cleanup, but relocating all reification and kernel helpers into Spec would enlarge the human-review surface without strengthening the final guarantee.

KEEP the successful-output abstraction; DEFER rejection-sensitive outcome semantics. The current Matches does not certify rejection probability, rejected traces, rejection-inclusive completion probability, or execution time. Narrow unqualified “termination probability” language accordingly. A richer outcome law is an extension, not a correction to the existing equality. Rejection already has explicit zero-output and safety lemmas. 

determinize-pro-architecture-so… +1

2.5 Finite/extended compatibility, zero mass, and premise-discharge helpers — REVISE to a small client-facing layer

Finite/extended compatibility: KEEP, P2. Two generic facts are useful: integrability implies HasExpectation, and under integrability extendedExpectation agrees with the real integral embedded into EReal. Keep the two validity domains distinct. Do not collapse them into a total numerical function or remove HasExpectation. The existing definition explicitly protects the signed extended expectation from the both-parts-infinite case. 

determinize-pro-architecture-so…

Zero-mass lemmas: REVISE, low priority. Reuse the generic measure fact “zero total mass implies zero measure” and existing rejection lemmas before adding program-specific wrappers. reject_zero, let_reject_zero, and failed_observation_zero already exist. There is no need for a family of duplicate zero-expectation, zero-variance, and zero-quotient APIs. 

determinize-pro-architecture-so…

Safety and moment helpers: KEEP selectively, P1. These help clients discharge genuine premises. Examples contains private safe_next, safe_sample, and safe_real; the uniform integrability and mean proofs are also private. But not all useful moment facts are hidden: uniform_abs_bound is already public. 

determinize-pro-architecture-so… +2

Do not simply expose the existing private safe_sample unchanged as the main API: its continuation premise is pointwise ∀ value. The useful general helper should accept almost-everywhere continuation safety under the sampling law. Preserve domain and integrability premises in the moment helpers.

Cost: small lemmas with direct client benefit; no need for a large automation framework. This is a better immediate investment than another normalization vocabulary.

3. Import cuts and generic probability
3.1 Every proposed import cut

The direction of the review is correct, but the cuts must be assessed together. A declaration move that leaves another transitive route to the same large proof does not achieve the advertised isolation.

Proposed cut	Verdict and minimal useful change	Benefit, cost, and dependency
Spec.Syntax → Spec.Primitives	REVISE, P1. Expr needs DistributionAction, types, finite-distribution data, and the default real literal type. It does not itself use Op or primitive measures. Remove the analytic import using those smaller dependencies; make Spec.Semantics import the fibers explicitly.	A lightweight Op module is useful for other clients, but is not itself the necessary fix for Syntax. Repair clients that currently obtain primitive names transitively. No mathematical proof change.
Proof.Typing → Proof.Measurability	KEEP, P1. Move the elementary reduce_*_eq declarations to an ordinary reduction-equations module.	reduce_typed_closed explicitly uses them. Their inspected proofs are unfolding/case analysis, not measure theory. The safety-equivalence portion still needs the internal domain-safety predicate; do not promise a completely syntax-only typing module without addressing that dependency.
FiniteModel.Initial → Proof.Checking.Elaboration → Theorems	KEEP, P1. Move interpret_determinize and related literal-map laws to a lightweight module.	initial_reification uses the commuting equation, not global determinization soundness. Preserve the existing declaration names or aliases.
FiniteModel.MeasureLaws → OrdinarySemantics → SymbolicSoundness	KEEP, with a coordinated split, P1. Separate direct evaluator/kernel facts from symbolic source transport.	MeasureLaws itself does not use the ordinary-semantic results in its bodies, but downstream Execution uses direct_cumulative_mono. Give that consumer an explicit lightweight import rather than merely deleting the transitive import.
FiniteModel.Replay → Finite.Explore	KEEP, P1. Move Edge, Row, and Candidate to a data module; let both explorer and replay import it.	Removes a proof-to-explorer dependency. It does not automatically make the resulting data layer analysis-free: machine states and StateKind retain their own dependencies.
Checking.Result → Theorems	KEEP, P1. Move checked_sourceExpectedReward to a determinization-specific composition module.	Generic result validation and selected-program reward certification need no global determinization theorem. This cut alone is insufficient because the finite-model path also reaches Theorems through Initial.
Finite.Export → Frontend.Pretty	KEEP narrowly, lower P1. Extract exact constructor serialization, including its helpers, from display printing.	Clarifies two different contracts. It does not imply a large import-time saving for the whole exporter, whose solver/checker dependencies remain substantial.

The declaration evidence is direct: syntax imports the fibers despite its constructor definitions requiring only their data; typing calls the elementary equations; initial reification calls interpret_determinize; and result transfer alone invokes expectationPreservation. 

determinize-pro-architecture-so… +4

OrdinarySemantics is particularly amenable to a split: its direct measurability and monotonicity proofs precede typed_determinize and sourceTags_of_sourceForm. The former typing theorem is itself an ordinary typing induction; the latter needs symbolic source syntax, not the whole symbolic soundness proof. 

determinize-pro-architecture-so…

My static import traversal confirms two separate routes that matter:

Checking.FiniteModel → FiniteModel.Soundness → Initial
  → Proof.Checking.Elaboration → Theorems

Checking.FiniteModel → FiniteModel.Soundness → Graph → Execution
  → MeasureLaws → OrdinarySemantics → SymbolicSoundness

Thus the result-transfer, initial-reification, and ordinary-semantics cuts belong in one dependency-isolation task. Removing only the direct Checking.Result → Theorems edge does not suffice. Execution’s actual use of ordinary monotonicity is visible in its proof, so that dependency must be retained at the correct lower layer. 

determinize-pro-architecture-so…

Acceptance criterion: the generic finite checker/result modules no longer import global determinization soundness; their theorem statements and acceptance behavior remain unchanged. Check this with actual builds and dependency inspection. Static edge analysis does not establish that every required declaration and instance has been relocated successfully.

3.2 Generic probability layer — REVISE: extract existing generic material before designing another abstraction

Several proposed moves are well supported. SFiniteKernel, its basic constructors, bind_bind_const_swap, and sfiniteKernel_mapWithInput_apply have genuinely language-independent signatures and proof bodies. Their current placement under expression measurability or symbolic target safety is accidental. 

determinize-pro-architecture-so… +1

KEEP extracting these helpers, P1/P2 as needed by the import cuts. Include the generic averaging and bind/map lemmas they depend on. Do not move a helper into a nominally generic file that still imports the old symbolic umbrella.

There is already a substantial generic abstraction: FiberGood, FiberSound, and their transport, mixture, sum, and factorization lemmas quantify over an arbitrary trace-index space. Do not build a second generic trace framework before considering this existing one. 

determinize-pro-architecture-so… +1

DEFER a wholesale generic mean-factorization redesign. A small probability layer over finite ν, a Markov kernel κ, and a measurable function equal almost everywhere to the fiber mean is mathematically appropriate. But extracting Corollaries requires keeping the language-specific joint-to-output correspondence in an adapter: the present proofs obtain output laws through trace erasure, not solely through generic probability algebra. 

determinize-pro-architecture-so…

The benefit is independently usable probability results and a smaller dependency direction. The cost is explicit instance/transport work and another interface to maintain. That cost is justified when it removes an actual dependency or serves a client—not merely because the equations can be stated more abstractly.

4. Representation changes and proof consolidation
4.1 Phantom laws parameters — KEEP narrowly, P2

The two identified parameters are genuinely phantom at the datatype level.

SampleEnv stores primitive names and affine/general arguments; its constructors do not depend on a law implementation. actualMeasure and DomainSafe do depend on laws, while meanEnvironment uses the fixed meanValue table. SymbolicAction likewise contains no constructor-level law constraint; its G constructor already accepts an arbitrary measure, and its realization uses the fixed primitive fibers. 

determinize-pro-architecture-so… +1

Minimal change: remove laws from these datatype parameters; retain it explicitly on interpretations and predicates that use it. Related operations whose only dependence is inherited from the phantom index can then be simplified.

However, “harmless” is too strong as a compatibility claim. The change alters types, recursors, inferred arguments, and theorem applications. The old index also supplied nominal same-record bookkeeping; after removal, statements must still consistently use the intended law in actualMeasure, DomainSafe, and moment assumptions.

No substantive law constraint is lost if those statements remain intact. No open primitive framework is gained either: PrimitiveLaws.kernel_eq_paperMeasure still fixes the underlying catalog and measures. 

determinize-pro-architecture-so…

Cost and dependency: a mechanical but broad proof-source migration, independent of shared syntax. Keep it separate so failures are attributable.

4.2 Expr (Affine n) and shared binding traversals — REVISE to a bounded pilot, P2

The duplication is real. AffineExpr repeats the ordinary expression constructors, and its realization, embedding, affine mapping, shifting, and substitution perform the corresponding structural traversals. This is more than similar-looking code. 

determinize-pro-architecture-so… +1

But the review’s “clearest structural opportunity” should not be read as a demonstrated net improvement.

A useful pilot should cover the operations that actually stress the representation: realization through literal mapping; sample weakening; literal-map composition; commutation with shifting and substitution; and the two binders of fix and the list-cons branch. The existing realization lemmas use strong sizeOf induction and carefully selected simplification rules. Replacing the inductive type changes recursors, constructor namespaces, equation lemmas, and the normal forms used by those scripts. 

determinize-pro-architecture-so…

Two qualifications matter:

Binding operations are reusable, but the typing proofs are not automatically reusable. Ordinary typed_shift and symbolic WellTyped preservation prove different judgments.

coordinates is a fold, not merely another mapLiteral. Sharing the AST does not by itself eliminate every structural traversal identified in the review. 

determinize-pro-architecture-so…

Minimal experiment: establish correspondence, migrate one coherent structural lemma group, then a consumer that exercises sample weakening and substitution. Judge actual proof complexity, elaboration behavior, and auditability. Do not retain two ASTs plus permanent conversion bureaucracy unless that architecture has a separate justification.

Cost: potentially substantial proof churn despite no intended semantic change. Do not combine this pilot with optional site annotations, primitive-node redesign, or trace consolidation.

4.3 Symbolic WellTyped — KEEP; DROP replacement by pointwise ordinary typing

WellTyped.realG requires zero E coefficients. Ordinary Typed.real permits any real literal at G, and WellTyped.realize_typed discards the coefficient condition when it produces ordinary literal typing. Therefore “every realization is ordinarily typed” cannot recover the symbolic invariant. 

determinize-pro-architecture-so… +1

The symbolic judgment also enforces stochastic source tags, whereas ordinary typing includes means. Its sourceTags theorem is substantive phase information. 

determinize-pro-architecture-so…

Minimal change: none to the invariant. Sharing its underlying tree is reasonable; erasing its additional information is not.

The same caution applies to the two safety predicates. PrimitiveDomainSafeAt makes the .stuck case true; public DoesNotGetStuckAt makes it false. Their reverse implication requires typing and structural progress. DROP unconditional unification as “duplicate safety interfaces.” An internal domain-only invariant and a public full non-stuckness predicate serve different purposes. 

determinize-pro-architecture-so… +1

4.4 Arity-indexed symbolic arguments — DEFER; separate P2 experiment

SymbolicAction.sampleE stores lists, and its WellTyped constructor proves their lengths. Downstream history construction converts them to Fin-indexed arguments. An indexed representation could remove this repeated conversion. 

determinize-pro-architecture-so… +1

That is a plausible local simplification, not a reason to make all syntax intrinsically typed. The cost moves from length equalities and getD lemmas to dependent elimination, function extensionality, and potentially less convenient simplification.

Minimal trial: change only the sampleE argument boundary and its realization/well-typedness bridge. Retain the current list representation unless actual consumers become clearer. No dependency on the larger AST migration is necessary.

4.5 Site-local optional affinities — DEFER

The current representation has a real invariant to maintain: Input.expression contains concrete labels while Input.affinities records optional requests in traversal order. But the inspected implementation consumes the list, checks excess/missing metadata, and validates affinity preservation against the inferred source. The attachments do not demonstrate a desynchronization bug. 

determinize-pro-architecture-so… +2

Consequently, “the wrong long-term boundary” is stronger than the evidence warrants.

Minimal current change: retain the representation and strengthen nested-site/annotation regressions where needed. A structural Annotates input core relation becomes attractive if source-location diagnostics or further elaboration transformations create concrete maintenance pressure.

A site-parameterized syntax is also a different generalization from Expr (Affine n): the latter changes literals, not sampling actions. Making them one migration adds scope rather than merely sharing work.

Cost: frontend, inference, checker, and exported-certificate changes; possible proof and format compatibility costs. It still does not verify parsing.

4.6 Detailed versus compact trace paths — REVISE: keep the experiment, not a deletion mandate

The two exact-depth lockstep proofs really do repeat next/E/G reasoning and the same symbolic safety extension. The duplication is supported by their bodies, not just file names. 

determinize-pro-architecture-so… +1

There is also a concrete small improvement: obtain target safety directly rather than projecting it from detailed StepTraces.soundness. The detailed theorem already obtains safety from determinize_primitiveDomainSafe_of_typed_source, followed by the typed safety equivalence. Extracting that composition avoids an unnecessarily strong theorem dependency. 

determinize-pro-architecture-so… +1

But this alone does not cut the detailed proof path. CompactTrace imports TraceSoundness; common averaging, trace-law, generation, and replay helpers are intermixed across the symbolic trace modules. Those dependencies must be separated before a production import can stop loading the detailed induction. 

determinize-pro-architecture-so… +1

The distinctions that must survive are:

detailed replay uses a tape whose length supplies reduction depth;

compact replay omits silent entries and is summed over return depths;

the compact proof identifies an exact-depth mass-one replay with the whole replay law;

target self-replay establishes a concrete operational fact, not just an arbitrary factorization.

In particular, a projection of a joint law does not automatically identify its fiber with the specified compact replay. The existing transport lemmas require an explicit equality of fibers, pointwise or almost everywhere. 

determinize-pro-architecture-so… +2

Minimal experiment: retain detailed joint traces for depth accounting, make compact replay the principal factorization path, and remove the detailed lockstep theorem from that path only after relocating shared helpers.

DROP removing exact-depth guards or replacing all semantic recursions with one evaluator. Cumulative output counts an already returned value at every later bound; exact-depth output counts it once. Summing the former as though it were the latter would repeatedly count the same return.

Cost: a significant proof-dependency migration. Success means one substantive lockstep argument with the same canonical replay theorem—not merely fewer files or a more abstract induction interface.

4.7 Redundant proof fields and bundles — DEFER trimming; KEEP useful packages

MeasurableFamily.coordinate_count is derivable from its skeleton equality and coordinate-length theorem. Fixed terminal measurability fields in StepKernel and several canonical-law facts can likewise become derived lemmas. These are genuine logical redundancies. 

determinize-pro-architecture-so… +1

They are not necessarily maintenance defects. Derived projections can improve constructor simplicity, but cached proof fields can improve construction and instance use. Keep SFiniteKernel: its packaged instance is used directly for composition.

Minimal change is opportunistic smart constructors or derived projections when a touched client benefits. Do not undertake a bundle-minimization pass independently of client or checking costs.

5. Finite certification and application interfaces
5.1 Replay safeguards and independent finite checking — KEEP

The existing checks encode different invariants:

RowReplays checks the computed transition law, including missing positive successors and exact aggregated weights. MatrixValid checks the induced stochastic matrix and terminal behavior. EdgesValid rejects duplicate, nonpositive, and out-of-range sparse edges even when the induced matrix might look acceptable. Aligned checks the requested initial program, scoping, and uniqueness of stored states. 

determinize-pro-architecture-so…

These are not interchangeable duplicate representations.

The scoping safeguard is particularly important because Binding.close replaces missing environment entries by rejection; malformed primitive argument lists are also reified as rejection. The proofs prevent these defaults from changing the accepted source program. The unused-closure free-variable test is therefore substantive. 

determinize-pro-architecture-so… +1

 

determinize-pro-cli-tests

Minimal change: retain these conditions and make their roles explicit in the contract inventory. Do not force generic checkModel through Certified: finite replay establishes a selected program’s safety and output law without requiring a typing derivation.

Also retain reachable-shape and bookkeeping progress. The machine/paper bridge uses different operational clocks and lexicographic recursion on paper fuel and bookkeeping rank. This is not expendable administrative duplication. 

determinize-pro-architecture-so…

5.2 Numeric Certified adapter and ExactResult — KEEP the adapter; REVISE the facade, P1/P2

The numeric adapter has a clear client: source-answer transfer. Certified already carries typing, source form, alignment, and requested-affinity preservation. The existing certified trace/expectation wrappers repeatedly recover E typing from a numeric type tag, using G-to-E subsumption. 

determinize-pro-architecture-so… +1

Minimal change: a numeric view or adapter that retains the original Certified object and supplies E typing plus interpreted source form. Do not copy the expression into an independent object that loses provenance.

An ExactResult p containing a rational answer, safety, integrability, and the integral equation is also sensible, but not required for every client. The generated expectedReward theorem already provides a simple selected-program conclusion; the facade mainly benefits library clients composing results repeatedly.

Construct it from CheckedModel.correct and checked_expectedReward. Keep it indexed by the exact core expression. Do not replace CheckedModel, which also exposes full output-law correspondence, with a weaker answer-only object. 

determinize-pro-architecture-so… +1

Source transfer must still require source safety and integrability. A finite determinized model supplies neither automatically. The adapter removes repeated typing bookkeeping, not substantive mathematics.

Cost: small composition lemmas; a new stable public record only if actual clients benefit. Place determinization-specific adapters above the generic finite checker after the import split.

5.3 Certificate profiling and sparse checking — KEEP profiling; DEFER replacement algorithms

The source establishes an opportunity, not a measured bottleneck. Candidate.weight scans a sparse row, while several checks quantify over state pairs; state equality can inspect closures and environments. These are sensible profiling targets. 

determinize-pro-architecture-so…

Measure compiled checking and independent decide +kernel separately, including state equality/uniqueness, successor coverage, dense matrix predicates, value equations, absorption, and rational bit sizes.

Minimal algorithm experiment, only after profiling: a Boolean checker proved equivalent to ReplayValid, possibly using untrusted successor-index hints. It must recompute the machine step, check hinted states, cover every positive outcome, aggregate duplicate outcomes, and reject spurious edges. Preserve the current declarative acceptance contract.

Initial-index validity and matrix nonnegativity can be derived from other accepted facts, but an optimized checker can derive those proofs without deleting the fields from ReplayValid. This separates checking cost from interface churn.

KEEP the custom equality implementation pending evidence. Its comment specifically identifies casts between Decidable types as a kernel-reduction concern. Replacing it with derivation for stylistic uniformity is not justified. 

determinize-pro-architecture-so…

5.4 Terminal rows and absorption horizons — KEEP; DROP the withdrawn alternatives

The full review already withdrew “derive terminal rows” and “replace the hint by model.size.” Those withdrawals are correct.

The model semantics ignores terminal rows, while replay/export requires a uniform absorbing representation. The terminalWithUnusedRow test deliberately distinguishes those levels. A raw terminal-row constructor is harmless convenience, not a reason for dependent transition data. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

The supplied horizon controls actual work in survivalVector. A state-count existence bound is not a reason to evaluate to that bound when a smaller certificate works. solve searches for a small horizon and then calls the checker. 

determinize-pro-architecture-so… +1

Minimal changes: none required. Considering horizon zero for an all-terminal model is a small optional improvement—the checker already accepts it—but not architectural work. A canonical state-count upper-bound theorem is worth adding only for a completeness claim or a client that uses it. 

determinize-pro-cli-tests

5.5 Positive-edge escape rankings — DEFER, P2 after absorption profiling

The proposal is mathematically credible. Require one positive-probability lower-rank successor for each transient state; other edges may cycle. Requiring every edge to decrease would wrongly exclude retry models.

It can either imply the existing absorption condition or support a uniqueness proof based on maximal absolute differences. That second route fits the existing homogeneous_unique argument, which uses a maximum over finitely many states. 

determinize-pro-architecture-so…

Minimal experiment: retain value equations and compare an alternative escape witness against the current horizon checker. Check all successor indices, positivity, and rank decreases independently.

Cost: new certificate data and a uniqueness/absorption proof. Potentially less rational arithmetic, but no demonstrated kernel speedup. The existing bookkeeping rank is not a substitute: it excludes infinite administrative machine steps, not probabilistic nonabsorption.

5.6 Nonabsorbing models and all-state absorption — DEFER, P3

The reviewer correctly separates integrability from uniqueness. outputMeasure_integrable holds for every finite model because its output is dominated by finitely many terminal Dirac measures. Absorption enters resultCertificate_sound to rule out spurious equation solutions. 

determinize-pro-architecture-so… +1

There is also a genuine interface limitation: replay permits unique unreachable states, whereas result validity demands absorption from every state. An unreachable nonabsorbing component can obstruct certification of an otherwise straightforward initial answer. 

determinize-pro-cli-tests

 

determinize-pro-architecture-so…

Minimal extension when needed: a checked closed zero-output set with no returned states and no positive outgoing transition; fix its values to zero and certify escape from the remaining relevant states to terminals or that set. Alternatively, formulate an initial-reachable-submodel result.

Do not merely choose a solution of singular equations, drop absorption, or tighten replay reachability to hide the limitation.

Cost: solver graph analysis, a new checked witness, and a uniqueness argument. Keep the current absorbing certificate as the baseline until nonabsorbing examples are an actual requirement.

5.7 Observable rewards — DEFER, P3

Mass and second-moment certificates are plausible extensions, but preserve the distinction between returned value and observable applied to that value.

A small interface can use a rational observable on terminal rational outputs, or a checked terminal-value table, with equations using that observable. Keep Model.kind and Model.Matches unchanged. For observable 1, rejected paths still contribute zero because rejection is not a return.

The existing finite-support argument already proves integrability for arbitrary real functions on the finite terminal support at intermediate points, so the analytic extension is less daunting than a new general integration theory. 

determinize-pro-architecture-so… +1

However, an exact target observable does not automatically transfer to the source. Identity expectation and mass are preserved; second moments generally are not. Do not hide that distinction inside a generic source-result facade.

Cost: additional equation/result adapters and explicit observable correspondence; justified by clients needing exact conditional statistics, not by normalization terminology alone.

6. Primitive and language recommendations
6.1 Primitive extension contract and canonical helpers — KEEP documentation; REVISE structural changes

The complete extension contract genuinely exceeds PrimitiveLaws: affine mean structure, domain stability, and moment growth are essential. SymbolicMoments.integrable_affine explicitly invokes PrimitiveMomentBounds to integrate over earlier E draws. Fixed-parameter integrability alone is insufficient for that proof. 

determinize-pro-architecture-so…

KEEP a concise extension checklist, P1: ordered arguments and E-admissible positions; domain and mean stability; measurable on-domain probability law and zero off-domain; affine mean; sufficient first-absolute-moment growth; optional exact rational implementation.

REVISE canonical-helper refactoring to be opportunistic, P2. The public fibers and internal tables are not unrelated definitions that merely happen to agree: explicit *_Fiber_eq theorems connect them. Shared domain/mean helpers can reduce maintenance, but must leave the named primitive specification easy to inspect. 

determinize-pro-architecture-so…

Do not force real semantics, rational execution, and Float algorithms through one arithmetic implementation. Keep the exact backend optional.

DEFER an open primitive signature; DROP replacing named constructors by a dependent generic primitive node merely to shorten cases. The current PrimitiveLaws is deliberately a bundle of facts about canonical measures, not a defective attempt at arbitrary plug-in semantics.

6.2 E-dependent Gaussian variance — DEFER, P3; a credible extension, not a correction

The proposed affine arity two, general arity zero, and mean coefficients (1,0) fit the intended dependency idea. But the current rule and symbolic reducer deliberately require the variance to be G/constant in E coordinates. 

determinize-pro-architecture-so… +1

The genuine new analytic obligation is a first-absolute-moment bound uniform over the now-affine variance. The existing Gaussian bound chooses a constant depending on the fixed general variance; it cannot simply be reused unchanged. A bound of the form

E∣N(m,v)∣≤∣m∣+
v
	​

≤∣m∣+1+v(v≥0)

would supply the required growth mathematically. That proposed bound has not been compiled here. 

determinize-pro-architecture-so…

One cost in the review is overstated: joint Gaussian measurability is already established using measurable_gaussianReal on the mean/variance pair. Reclassifying coordinates still requires transport and proof repair, but not Gaussian joint measurability from scratch. 

determinize-pro-architecture-so…

Minimal experiment: the new bound, domain-stability case, argument reindexing, typing/reducer changes, and one nested-variance example. Keep G Gaussian parameters G and preserve strict evaluation/domain checks.

Cost: a genuine language/proof extension. It is not needed to validate the current theorem.

6.3 Symmetric multiplication — DEFER, P3

Affine.mul? already handles either operand having zero E coefficients, so the mathematical proposal has support. But ordinary typing and inference deliberately choose the left-G discipline. 

determinize-pro-architecture-so… +1

A right-G rule can preserve source evaluation order. It introduces alternative affinity assignments and therefore an inference policy problem; checker simplicity does not eliminate that problem.

Minimal experiment: an additional derivation/checking case with a separately stated inference-choice policy. Do not commute arbitrary effectful expressions. Do not make this a prerequisite for fixing literal normalization.

6.4 Finite degenerate stochastic distributions — DEFER, P3, but smaller than the Gaussian extension

The restriction is real: finiteLaw validates parameters and then rejects unsupported stochastic primitive names before considering whether their evaluated law is a singleton. The point-uniform exclusion is explicitly tested. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

Minimal extension: exact singleton outcomes for evaluated point uniforms, zero-variance Gaussians, and zero-rate Poisson calls, after domain/arity validation. Prove the corresponding FiniteLawMatches facts and update the supported-call policy and tests.

This must establish the entire sampling law, not merely its mean. The existing finiteLaw_sound theorem is the right integration point. 

determinize-pro-architecture-so… +1

No approximations or blanket approval of those primitive families should be introduced.

6.5 Source form, transformation laws, and retained semantic choices

KEEP sourceForm; DEFER mixed source/mean soundness. The current symbolic judgment is source-only. Removing the premise requires extending that invariant and its mean cases, not deleting a wrapper hypothesis.

KEEP a lightweight transformation-law module, P1/P2. Literal-map, shift, and substitution commuting laws are genuine reusable facts currently dispersed through proof modules. Add idempotence where useful; do not require another semantic framework to publish elementary syntax algebra. The existing substitution proofs are structural. 

determinize-pro-architecture-so…

KEEP the three sampling actions, silent structural subtyping, mathematical real arithmetic, and strict mean-operand evaluation. Add focused rejection/divergence-in-mean-operand regressions alongside the existing sampled-variance test; do not treat G affinity as effect purity. 

determinize-pro-architecture-so… +1

 

determinize-pro-cli-tests

KEEP finite categorical indices, zero-weight positions, and nonnegative/unit-sum evidence. DEFER changing Nat → value to Fin → value as an ergonomic choice. Likewise defer redesigning trace tags: .discrete d carries static distribution data while other tags do not. That distinction should be revisited when adding dynamic categorical parameters or site identities, not changed speculatively now. 

determinize-pro-architecture-so… +1

7. Orchestration, serialization, and other implementation advice
7.1 Repeated compilation and model checking — KEEP, P1

Both repeated computations are present, but their fixes need more care than “pass the existing object.”

Main calls compile, then certificateText parses, elaborates, infers, and certifies again. However, Program currently stores only Input and Certified; it does not retain the raw Certificate needed by the exporter. 

determinize-pro-cli-tests

 

determinize-pro-architecture-so… +1

Minimal repair: an internal compilation result that retains certificate data for serialization, with the existing compile interface preserved as a wrapper if needed. Export the already checked source/input/requests and matching certificate. Do not turn inference into a trusted oracle.

writeResult checks the candidate and then calls write, which checks it again. Minimal repair: a shared internal emission path carrying evidence for that particular candidate. A plain CheckedModel plus an unrelated raw Candidate is not sufficient bookkeeping for safely bypassing validation. 

determinize-pro-architecture-so… +1

Preserve independent kernel replay in generated files. That repetition is intentional verification, not orchestration waste.

DEFER caching survival metadata until profiling. The generator’s computed survival vector, checked acceptance, and later metadata have different roles. Avoid changing public certificates merely to save a small unmeasured recomputation.

7.2 External serialization and partial writes — KEEP boundary documentation; DEFER stronger guarantees

render creates a fresh sink and splits signed rewards; the sink test checks the intended once-only encoding. Neither Model.Matches nor the generated reward theorem directly states correctness of the .tra and reward-file bytes. The review is correct to distinguish those contracts. 

determinize-pro-architecture-so…

 

determinize-pro-cli-tests

Minimal change: document the boundary and retain independent serialization regressions. Formal serialization correctness is warranted only if external-tool results become part of the claimed certified pipeline.

Sequential writes can leave partial output after an IO failure. DEFER transactional export unless operational use requires it; staging a bundle or using a completion marker is a separate robustness feature. Validation-before-writing does not imply atomic multi-file output.

Also retain the distinction between writing a kernel-checkable file and actually kernel-checking it. Main does the former, not the latter. 

determinize-pro-cli-tests

7.3 Diagnostics, checking architecture, and evaluator unification

KEEP proof-producing checking. DROP intrinsically typed syntax, runtime casts, or inference completeness as remedies for the issues found here. The current checker produces evidence for the existing declarative judgment and separately validates source preservation.

REVISE error-localization advice to a client-driven improvement, P2. Better frontend/checker diagnostics can be useful, but source spans and structured rejection reasons require plumbing. They are not free consequences of optional site annotations, and do not justify redesigning proof-producing check.

KEEP the exact and numerical engines separate. The exact machine supports structural replay and paper-step/bookkeeping correspondence; the numerical evaluator has Float arithmetic, PRNG state, fuel, and distinct failures. Similar value constructors do not establish that one evaluator would be clearer or cheaper to verify. 

determinize-pro-architecture-so… +1

Likewise retain the readable direct public semantics and its internal measurable-kernel presentation. InterfaceChecks explicitly verifies that the public imports do not install a measurable structure on expressions. Preserve that property through refactoring. 

determinize-pro-architecture-so…

Surviving work sequence

Repair the two executable findings, preserving the existing statistical denominator and adding overflow diagnostics and targeted roundtrip tests.

Make the small public-contract repairs: successful-return/replay wording, named replay probability and pushforward facts, and selected safety/numeric adapters. Do not add the normalized-law suite by default.

Perform the coordinated import cuts, preserving declarations and checker behavior. Extract existing generic kernel helpers where they enable those cuts.

Then choose a bounded maintenance or performance trial from actual need: phantom removal as a separate mechanical change; shared syntax or trace consolidation as isolated proof-maintenance experiments; certificate profiling before sparse or ranking certificates. Language and nonabsorbing-model extensions remain outside this immediate sequence.

Explicitly discarded: making acceptance normalization the ordinary public interface now; renaming the quotient theorem merely because it is totalized; replacing symbolic typing by pointwise ordinary typing; blindly merging the two safety predicates; removing exact-depth machinery; replacing the horizon by model.size; deriving terminal rows through a more dependent representation; deleting proof caches solely for field count; commuting arbitrary effectful multiplication operands; and unifying the exact and numerical evaluators merely because their syntax overlaps. The horizon and terminal-row alternatives were already withdrawn in the full review; this assessment agrees with those withdrawals rather than treating them as new disagreements.

Coverage and remaining uncertainty

This adjudication covers the recommendations in every substantive section of the full review and the local synthesis: public mathematics and naming; all seven import cuts; generic probability; phantom and shared syntax proposals; symbolic invariants; compact/detailed traces; certificate facades, safeguards, horizons, rankings and nonabsorption; primitive contracts and extensions; positional affinities; executable fixes; repeated work; serialization, diagnostics, and IO robustness.

The evidence includes all eleven specification files and nine headline exports; the relevant Checking, Frontend, Finite, Runtime, Main, and test contracts; and the proof bodies for the proposed simplifications, especially ordinary measurability, typing/safety equivalence, symbolic realization and moments, both lockstep inductions, compact replay identification, finite execution correspondence, and result uniqueness.

The remaining uncertainties are engineering outcomes, not reasons to weaken the mathematics: whether moved declarations build with the intended smaller import closure; whether shared syntax or one trace path actually improves proofs; whether sparse or ranking checks reduce kernel cost; and whether the proposed normalization or extension lemmas elaborate against the pinned Mathlib. None was compiled here. The unbundled Mathlib sources, external corpus/scripts, paper, and serializer/tool integrations were not independently inspected.

