# Pro first response (rendered text)

Main finding

For this example, checked typing, a checked expectation for the determinized program, and a checked expectation for the source are three different guarantees. The development has components connecting them, but the exports I inspected do not finish the connection automatically: the typing export leaves a source-safety premise; the exact-result export proves a result for the selected program, normally the target; and transferring that result back requires source safety and source integrability. Those last two requirements are not redundant. 

determinize-pro-architecture-so… +1

I followed the declarations in Frontend, Checking, the relevant Spec modules, the expectation-proof wrapper and uniform-law proofs, and the finite machine/replay/result/export chain. Paths below are relative to lean/Determinize/. This is a source inspection and a reconstruction of the executions, not a Lean rebuild. One scope limitation: the bundle contains 91 Lean library files, but not lean/Main.lean or the lean/Tests/ tree, although the build configuration names both executable roots. Thus I can assess the callable library path, not independently establish the CLI wiring or test results. Mathlib is also not supplied. 

determinize-pro-architecture-so…

1. From the input string to checked typing
What the frontend constructs

For

let x = uniform[E](0,1) in x + 1

Frontend.parse constructs the named surface tree, with some .E on the uniform node. Frontend.elaborate, through lower, resolves x to de Bruijn index zero and produces the rational core

lean
def pQ : Checking.Core :=
  .letE
    (.uniform (.sample .E) (.real 0) (.real 1))
    (.add (.bvar 0) (.real 1))

together with the separate metadata list [some .E]. In particular, draw2 stores the explicit annotation both in the expression and in that positional list. Numbers are parsed as exact rationals, not floating-point values. See Frontend/Parser.lean (decimal, expr, parse) and Frontend/Elaborate.lean (draw2, lower, elaborate). 

determinize-pro-architecture-so… +3

Frontend.infer then proposes an annotated core and a typing-certificate tree. Here the source remains pQ, the result type is .float .E, and all seven certificate nodes can have that same type. There is no subsumption needed. The underlying declarative derivation is simply

lean
Typed.letE
  (Typed.uniform Typed.real Typed.real)
  (Typed.add (Typed.bvar HasVar.head) Typed.real)

The certificate is considerably more verbose than this mathematical derivation because it repeats a proposed type at every syntax node. That is not inherently wrong—general unannotated higher-order terms need typing information—but this example makes the overhead visible. See Checking/Certificate.lean (Certificate) and Spec/Syntax.lean (Typed.letE, Typed.add, Typed.uniform). 

determinize-pro-architecture-so… +1

What is actually checked

Checking.check is proof-producing:

lean
check Γ e τ c :
  Option (PLift (Typed Γ (interpret e) τ))

It does not merely accept the inference algorithm’s verdict. Its uniform case checks that the site affinity matches the proposed numeric type and checks both operands; its let and addition cases build the corresponding Typed proofs. 

determinize-pro-architecture-so… +1

Checking.certify additionally establishes that the proposed source contains only stochastic sites, agrees with the supplied original core after erasing affinities, and respects the separately supplied affinity requests. These are exactly the fields of Checking.Certified. 

determinize-pro-architecture-so…

There are two important limits.

First, this checks correspondence to the elaborated core, not to the input string. The exported sameProgram theorem is an equality between two affinity-erased core expressions. Neither parsing, name resolution, nor the translation from explicit annotations to the request list is established by that equality. In Frontend/Certificate.lean, certificateText emits original, requested, annotated, and the certificate as Lean data, then reconstructs checked using by decide +kernel. A faulty exporter could emit a different, internally consistent program and still obtain a valid certificate for that different program. The source identity therefore remains a boundary requiring either trusted translation or a separately verified connection. 

determinize-pro-architecture-so…

Second, typing checks neither the uniform domain nor integrability. The same typing derivation accepts reversed literal bounds. For the present example, 0≤1 is true, but the checker has not proved it.

Design criticism: the expression-plus-positional-request-list representation creates an avoidable consistency obligation. The declared Certified contract treats requested as authoritative; it does not establish that this list accurately represents annotations in the original text. I would store optional affinity requests at their sites in the elaboration input, rather than storing concrete annotations in one tree and a parallel list that determines which annotations may change. This is a representation redesign, not a discovered unsoundness in certify’s stated contract.

2. Determinization and mathematical semantics
The target is a mean operation, not the literal 3/2

DistributionAction.determinize changes .sample .E to .mean. Expr.determinize applies this transformation structurally. Consequently, the target is

lean
.letE
  (.uniform .mean (.real 0) (.real 1))
  (.add (.bvar 0) (.real 1))

It has not been constant-folded or reduced. The mean site has no affinity annotation; the current three-way representation—E sample, G sample, mean—does not have a redundant fourth “mean affinity” state. 

determinize-pro-architecture-so… +1

There is also no need for a fresh target typing certificate. Proof.Paper.typed_determinize proves typing preservation. Proof.Checking.interpret_determinize proves that determinizing rational syntax and then embedding literals into the reals agrees with determinizing the real interpretation. Neither is an additional caller assumption. 

determinize-pro-architecture-so… +1

The concrete laws

Write p=interpret pQ, t=p.determinize, and
μ=uniformMeasure01.

Unfolding Spec.Semantics.reduce gives the source execution

p
u∼μ
	​

let x=u in x+1⟶u+1⟶real(u+1).

The target has the same reduction pattern, but the first fiber is δ
1/2
	​

. In the operational Action datatype, even this mean operation is represented as a .sample action; its fiber is simply a Dirac measure. The mean fiber still checks the original parameter domain. 

determinize-pro-architecture-so… +2

Thus, by unfolding the supplied semantics,

bigStepMeasure(p)=μ.map(u↦u+1),bigStepMeasure(t)=δ
3/2
	​

.

These are whole output laws, not distributions conditioned on termination. Here both have mass one. Source safety follows because the only primitive parameters are valid and every continuation performs defined real addition. Source integrability follows because its output lies in [1,2]. The definition of DoesNotGetStuck requires probability-one fibers and almost-everywhere continuation safety at every finite depth; it does not mean termination. 

determinize-pro-architecture-so…

The analytic fact that a uniform sample has the stated mean is not merely postulated in a record. Proof/PrimitiveKernels.lean proves the private lemmas uniform_integrable_id and uniform_mean, and uses them to construct the concrete primitiveLaws. The exact evaluator’s treatment of .mean and the analytic justification for replacing .sample .E by .mean are separate pieces of reasoning. 

determinize-pro-architecture-so… +1

What the expectation theorem adds

Theorems.expectationPreservation, through Spec.mainThm, takes source typing, source form, source safety, and source integrability. It returns target safety, target integrability, and equality of the two integrals. For this program, supplying the facts above yields expectation 3/2 on both sides. Proof.Checking.certified_expectation is the existing adapter from a Certified source. 

determinize-pro-architecture-so… +1

Internally, Proof/Soundness.lean obtains this through trace factorization and integration over trace fibers. In this example there are no G draws, so the only terminating generation trace is []; the target output is the mean of the source replay law on that trace. That machinery need not appear in the example’s client interface. 

determinize-pro-architecture-so… +1

Interface criticism: the convenient uniform moment lemmas are private, and the elementary safety helpers in Proof/Examples.lean—safe_next, safe_sample, safe_real, safe_let_uniform—are also private. A client trying to close this tiny example is pushed toward operational unfolding or proof-internal parameter tables. Exposing a small compositional API for primitive laws, let-binding, and safety would be more valuable here than another general-purpose wrapper theorem. 

determinize-pro-architecture-so…

The numerical runtime is a separate branch

Runtime.runOutcome converts rational literals to Float; Runtime.eval and Runtime.sample then execute a different, fuel-bounded numerical implementation. For this input, the source performs a numerical uniform draw and adds one; the target computes a / 2.0 + b / 2.0 and adds one. The inspected expectation theorem does not establish the distribution of these PRNG outputs or correctness of floating-point execution. A checked core is not a verified numerical sampler. 

determinize-pro-architecture-so… +3

3. The finite certificate for the target
Exact execution

The finite path does not use that numerical runtime. Finite.finiteLaw .uniform .mean [0,1] checks the domain and returns the exact rational outcome list [(1, 1/2)]. Stochastic uniform draws are unsupported by this finite machine, so this example must be explored as .determinized, not .source. 

determinize-pro-architecture-so… +1

Unfolding Finite.step by hand gives 13 distinct CEK states: the initial let; uniform setup; evaluation and delivery of each bound; delivery of the mean; entry into the let body; evaluation and delivery of the variable and literal; and final delivery of 3/2. The resulting model has the form

s₀ → s₁ → ⋯ → s₁₂

kind(s₀), …, kind(s₁₁) = transient
kind(s₁₂)               = returned(3/2)

P(sᵢ, sᵢ₊₁) = 1       for i < 12
P(s₁₂, s₁₂) = 1

This count is a reconstruction from step, not a reported execution of the project. The extra states are administrative evaluation states, not additional probabilistic choices. 

determinize-pro-architecture-so…

A concrete result certificate for that model is

values(sᵢ) = 3/2        for every state
horizon    = 12

Every value equation holds, and survivalWithin 12 is zero at every state.

What replay establishes

The explorer’s output is only a Candidate. Candidate.ReplayValid checks dimensions, initial-state alignment, source scoping, distinct stored states, matrix and edge validity, and local replay. Crucially, RowReplays checks coverage of every positive-probability successor, not just correctness of edges the candidate happened to include. Its transition weights must equal the complete weights returned by Finite.step. 

determinize-pro-architecture-so… +1

The semantic bridge is substantive:

uniform_mean_matches and finiteLaw_sound connect the exact rational outcome list to the real primitive fiber.

The execution proof connects CEK execution to paper reduction, including administrative progress.

replay_matches concludes Model.Matches, namely target non-stuckness and equality of the entire output law. 

determinize-pro-architecture-so… +2

This stage does not require a typing certificate: a complete successful replay independently establishes safety and the output law of the selected closed core program.

What result checking establishes

Checking.checkResult checks the value equations and the absorption condition. checked_expectedReward combines result correctness with model correspondence to conclude

Integrable(id,bigStepMeasure(t))∧∫vdbigStepMeasure(t)=3/2.

The generated .result.lean file reconstructs machineReplay and resultAccepted with decide +kernel, then applies this theorem. Generating that text is not itself independent kernel checking; the generated file still has to be checked. The resulting theorem is about the embedded checkedSource and checkedSubject. 

determinize-pro-architecture-so… +2

The absorption condition has a specific role. Finite terminal rewards already make every such model’s output integrable, even when the model can diverge. Absorption instead rules out spurious solutions to the value equations: on a transient self-loop, v=v admits any proposed value although the output law is zero. This distinction is reflected in outputMeasure_integrable and resultCertificate_sound; it should remain explicit in the interface documentation. 

determinize-pro-architecture-so… +1

The concrete integration gap: Finite.resultCertificateText exports the target result, not an unconditional source result. Checking.checked_sourceExpectedReward supplies the bridge, but asks again for source typing, source form, safety, and integrability rather than consuming the frontend’s checked-source package. Some of this is recoverable bookkeeping; some is genuinely new mathematical work.

4. Assumptions that cannot safely disappear
Source safety cannot be recovered from target safety

Consider the nearby typed program

let x = uniform[E](0,1) in uniform[E](x,0.5)

Its target evaluates both bounds of the inner uniform to 0.5, so it is safe and returns 0.5. The source encounters reversed bounds whenever x>0.5. Under the specified zero-fiber semantics, its output mass is 1/2, and its unnormalized expectation is 3/16, not 1/2.

This is a counterexample to removing source safety even when the target has an exact finite certificate and the source output is integrable. It follows directly from Typed.uniform, uniformFiber, and the bind-based output semantics. 

determinize-pro-architecture-so… +2

Source integrability cannot be recovered from a finite target

There is also a small language-level counterexample. Start with X∼uniform[E](−1,1). Repeatedly toss a fair G coin: on success return x + 1; otherwise recurse with 2 * x. This uses the permitted G-left multiplication and G-controlled branching rules. 

determinize-pro-architecture-so… +1

The source returns 1+2
N
X, where
Pr(N=n)=2
−(n+1)
. Its absolute first moment is infinite. The target starts with x=0, repeatedly revisits the same finite collection of machine states, and returns 1; it therefore admits a finite absorbing target model.

Thus even source safety, almost-sure termination, and a finite absorbing target do not justify dropping source integrability. The original example’s boundedness proves that premise; target certification does not.

sourceForm, by contrast, is currently a syntactic scope restriction already supplied by Certified.sourceOnly. It should not require renewed user proof. Extending the theorem to mixed source/mean syntax is a separate generalization, not something established by this case.

5. The cleanest interface I would aim for

I would keep the existing mathematical semantics and initially change the application-facing interfaces.

One authoritative checked numeric source. Provide a numeric-source view containing the rational core, its source-form proof, and its real interpretation typed at .float .E. A G-result program can enter through the existing subtyping rule. Define the target from that source, rather than carrying another target value plus an equality witness. This removes the repeated m/hTy adapter arguments and lets source-result transfer reuse facts already established by Certified.

One checked exact-result object. Expose the selected program, rational answer, safety, integrability, and correctness theorem. Keep candidate states, transition rows, value vectors, and checker acceptance proofs beneath that interface. The value vector is legitimate untrusted certificate data; making clients manipulate it is unnecessary. The source-transfer operation should require only the additional source-safety and source-integrability proofs.

A small public semantic API. Export direct uniform-law and safety lemmas, plus compositional let/arithmetic lemmas, so this example can close its two analytic premises without reconstructing private kernel parameter tables. The final artifact for this case should state, with no remaining premises,

∫vdbigStepMeasure(interpret pQ)=3/2,

while keeping the target’s exact-result certificate separately identifiable.

There are also several concrete simplifications visible in this path:

Derive redundant replay obligations. ReplayValid.initial_lt follows from Aligned.initial, which asserts a successful array lookup. Matrix nonnegativity follows from positive validated edges. These are redundant checking/proof obligations, not extra mathematical assumptions. Terminal transition rows are ignored by Model.outputWithin, so canonical terminal self-loops could be supplied by construction rather than included as meaningful certificate data. 

determinize-pro-architecture-so… +1

Make the horizon optional bookkeeping, not essential supplied data. For a finite model, positive-probability reachability of a terminal state has a simple-path witness. Consequently, whenever some uniform finite absorption horizon exists, model.size is a sufficient canonical horizon. Proving this equivalence would let the interface omit the horizon, or retain a smaller one only as a checking-performance hint. The absorption condition must remain; the caller-chosen number need not.

Untangle the imports. Proof/FiniteModel/Replay.lean imports Finite.Explore to obtain candidate data, pulling the unverified exploration module into the checker’s import path. Separate graph/certificate data from exploration. Likewise, Proof/FiniteModel/Initial.lean imports Proof.Checking.Elaboration, which imports the global theorem module, although the relevant bridge here is the elementary rational/real determinization equation. Move that equation into a lightweight syntax-law module. These changes reduce architectural coupling without changing the mathematical claim. 

determinize-pro-architecture-so… +2

Finally, the uniform mean and domain appear in the public fiber, the generic domain/meanValue tables, and the rational finite evaluator. The real representations are connected by uniformFiber_eq, and the rational evaluator by uniform_mean_matches, so this is proved duplication, not an unbridged semantic mismatch. It remains a maintenance target: canonical domain/mean helpers could reduce repeated definitions. The floating-point implementation should remain explicitly separate rather than inheriting an unjustified exactness claim. 

determinize-pro-architecture-so… +1

My priority would be to complete and export this one source-level 3/2 theorem through the proposed facade before undertaking a large internal redesign. It would force the development to expose exactly which facts are already certified, which follow from canonical definitions, and which require a real mathematical argument—without weakening any of those boundaries.

Sources
ChatGPT can make mistakes. Check important info.

6
Pro