# Development review

Baseline snapshot: `a4199193b529ddc15fbf1eca2c8a026dc0d995b6`. This document preserves the pre-refactor review; completed changes and before/after decisions are recorded in [refactor-plan.md](refactor-plan.md). Pro's independent full-surface review is tracked in [pro-spec-review.md](pro-spec-review.md).

## Recommended order

1. Remove the top-level result-mode parameter and use canonical trace-output functions.
2. Make the extracting model checker the primary interface; remove duplicate certificate data incrementally.
3. Expose unconditional finite-model integrability and simplify absorption certificates.
4. Consolidate sampling-site classification, then decide whether to restrict the raw syntax to three site cases.
5. Narrow the distribution constructor and tidy the specification boundary.

The first two mathematical interface changes were checked in Lean in a scratch experiment. Other proposals below require implementation and proof checking; they are not verified refactors.

## 1. Result mode is redundant; sampling mode is not

[Statement/Main.lean](lean/Determinize/Statement/Main.lean) quantifies `mode` in each main theorem. Silent subtyping proves:

```lean
(∃ mode, Typed [] program (.float mode)) ↔ Typed [] program (.float .E)
```

The G case follows from `.sub typed .general`. Consequently each top-level theorem can take `Typed [] program (.float .E)` and omit `mode`, without narrowing its domain. Apply the same change to trace statements and `SourceEndToEnd`. This changes the result-typing premise, not sampling annotations or the transform.

**Verified:** the equivalence compiled, with only standard Lean axioms. Proof impact should be small: adapt exported wrappers and callers first.

`Mode` and `Kind` encode different decisions. In [Primitives](lean/Determinize/Statement/Primitives.lean) and [Syntax](lean/Determinize/Statement/Syntax.lean), mode controls typing and which draws determinization removes; kind chooses the stochastic law or its atomic mean.

| Site | Source form? | Transform behavior |
| --- | --- | --- |
| E / stochastic | Yes | Becomes E / mean |
| G / stochastic | Yes | Remains stochastic |
| E / mean | No | Already a mean |
| G / mean | No | Admitted raw syntax, not produced from source form |

A representation `random Mode | mean` expresses the three cases needed for source programs and their transforms. It does **not** preserve every current raw term and typing judgment automatically: G/mean is currently admitted and its mode can matter to typing even though its primitive law agrees with E/mean. Choose deliberately between restricting raw syntax and proving a translation preserving the relevant semantics and typing.

Start with a named sampling-site record and a single classification function. This removes repeated tuple projections and repeated tests in `Traces.record`, `outputGivenTraceAt`, `generationEvent`, and `siteOp`. It improves clarity without pretending the two distinctions are synonyms. A phase-specific source/target syntax is a larger alternative, not necessary for this cleanup.

## 2. Canonical trace data can replace existential packaging

The current public `Statement/` and `Traces/` files contain no direct existential conclusions. The public refined theorem already uses `outputGivenTrace`. The remaining avoidable witnesses are mainly in proof interfaces:

- [TraceFibers.lean](lean/Determinize/Proof/TraceFibers.lean), `FiberSound.factorization`, returns `∃ ν output`; its proof immediately chooses the target trace marginal and the fiber integral.
- [CompactSoundness.lean](lean/Determinize/Proof/CompactSoundness.lean), `soundnessDataE` and `soundnessData`, package an existential output with a fixed replay kernel.
- [TraceFactorization.lean](lean/Determinize/Proof/TraceFactorization.lean), `MeanOnTraces`, existentially packages fiber and output.
- [Corollaries.lean](lean/Determinize/Proof/Corollaries.lean), `MeanOnTraces.output_laws`, repackages this data again.

Define the output once:

```lean
noncomputable def replayMean (program : Expr) (trace : Trace) : ℝ :=
  ∫ value : ℝ, value ∂outputGivenTrace program trace
```

Then expose, under source-form, typing, and safety premises:

```lean
traceAndOutputLaw program.determinize =
  (traceLaw program).map (fun trace => (trace, replayMean program trace))
```

**Verified:** this equality follows from the current development. A generic factorization's output can also be replaced by `fun trace => ∫ value, value ∂fiber trace`; the existing almost-everywhere mean equality proves the replacement preserves the target map law.

Keep arbitrary fibers where the generic proof abstraction needs them. Disintegrations can be nonunique off a null set; replacing them by `Classical.choose` merely conceals a witness. For the concrete program theorem, use the already-defined replay kernel. Likewise, existential intermediate typing information is not necessarily unique under subtyping.

## 3. Extract the checked model instead of supplying it twice

[Checking/FiniteModel.lean](lean/Determinize/Checking/FiniteModel.lean) already has the desired interface:

```lean
checkModel (source : Core) (subject : Subject) (candidate : Candidate) :
  Option (CheckedModel source subject)
```

`CheckedModel` contains a model and its `Matches` proof. The parallel `checkModelCertificate` accepts a separate model, reconstructs the candidate model, and compares them. This requires [ModelEquality.lean](lean/Determinize/Proof/FiniteModel/ModelEquality.lean), largely to support the generic Boolean checker contracts.

Make extraction primary throughout the end-to-end API. State the result theorem over the extracted model and its result certificate. Retain an equality adapter only if an actual consumer needs to certify an independently supplied model. Move arbitrary-checker composition in [Certificates.lean](lean/Determinize/Statement/FiniteModel/Certificates.lean) into proof support, while retaining a direct public theorem for the implemented pipeline.

**Invariant to preserve:** the caller chooses source and subject. A candidate must be checked against that request, not against its own metadata.

[Finite/Explore.lean](lean/Determinize/Finite/Explore.lean) also stores `Row.evidence`, `Row.kind`, and edges. [Replay.lean](lean/Determinize/Proof/FiniteModel/Replay.lean) recomputes the machine step and compares its evidence/kind. Remove the redundant evidence first; it is data describing a step, not a proof. Candidate source/subject can also be omitted if the initial-state check remains tied to the explicit request.

Keep sparse edges where they support efficient export and replay. Reconstructing the entire graph inside the checker is a separate performance decision. Replace long conjunctions in `ReplayValid` with named Prop fields. Where injective state enumeration supplies a unique successor index, expose a lookup function rather than another existential.

## 4. Model integrability and certificate bounds

[Proof/FiniteModel/Result.lean](lean/Determinize/Proof/FiniteModel/Result.lean) already proves `outputAt_integrable` privately, for every finite model and state, without an absorption certificate. Finite terminal rewards bound the output law even when execution can diverge.

Export integrability of `model.outputMeasure`, define its expected reward by an integral, and simplify `HasExpectedReward` to equality if this makes consumers clearer. This is a model fact, not a guarantee that requires checking candidate values.

`ResultCertificate.escape` is derivable from the model and horizon:

```text
escape = 1 - max_state (model.survivalWithin horizon state)
```

A simpler certificate stores `values` and `horizon`; its absorption condition is:

```lean
∀ state, model.survivalWithin certificate.horizon state < 1
```

Finiteness gives a uniform positive escape bound. `escape ≤ 1` follows from nonnegative survival. Positive horizon is unnecessary for an all-terminal model. The model is nonempty because it stores an initial state.

**Do retain an absorption argument:** Bellman equations alone admit spurious answers on nonabsorbing recurrent classes. Values and horizon remain useful untrusted search results; a checker need not solve the linear system itself. This is a moderate proof refactor, not just deleting certificate fields.

**Do retain source integrability in `SourceEndToEnd`:** integrability of a determinized finite model does not imply source integrability. Random heavy-tailed Gaussian scale can disappear under mean replacement while the source lacks an absolute first moment.

## 5. Distribution construction repeats checks and normalization

[Frontend/Elaborate.lean](lean/Determinize/Frontend/Elaborate.lean) rejects weights whose sum is not one. [Checking/FiniteDistribution.lean](lean/Determinize/Checking/FiniteDistribution.lean) then checks nonnegativity/positive total and divides all weights by that total. The frontend and Bernoulli production callers already supply unit-sum weights.

Use one checked unit-sum constructor, with nonnegative and sum-one evidence, and remove the frontend's duplicated validation. This preserves `.det` behavior but narrows the internal helper API: existing helper tests deliberately exercise normalization. Keep general normalization only if there is a real consumer; its proof machinery is otherwise avoidable.

## 6. Reduce the review surface without hiding semantics

- [Supported.lean](lean/Determinize/Statement/FiniteModel/Supported.lean) specifies exporter support policy; put it with the finite implementation, while documenting the actual checker's accepted domain.
- [DiscreteLaws.lean](lean/Determinize/Statement/DiscreteLaws.lean) is an import-only shim and can be removed.
- `Model.rewardWithin` is an auxiliary reward interpretation. Keep the output law and certificate validity clear; move auxiliary recurrences out of the public surface when no exported claim needs them.
- A model node representation `returned reward | rejected | transient distribution` can derive terminal self-loops for export and eliminate stored terminal rows plus absorbing-row obligations. Output semantics already ignores terminal transition rows. This is a larger model refactor.
- Primitive laws are described both in the public surface and by generic internal `domain`, `meanValue`, and `paperMeasure` machinery. Equality lemmas currently connect them. A common descriptor could reduce duplication, but should preserve readable per-primitive mathematical definitions; this is lower priority than the concrete redundant fields above.

## Semantic distinctions to preserve

- Rejection and returned zero have different output laws: zero measure versus a Dirac mass. Equal expected reward does not identify them.
- Source safety is not implied by target safety: replacing random parameters by means can turn an invalid source primitive call into a valid target call.
- Rational executable syntax versus real mathematical semantics is an intentional bridge, not duplicate numeric state.
- The quotient in `conditionalExpectationThm` equals zero at zero mass by Lean's division convention. Conditioning on acceptance has its probabilistic interpretation only at positive mass. The documentation should qualify its rejection-sampling claim accordingly.
- The existing variance comparison is meaningful for the stated unnormalized convention; normalizing both equal-mass laws gives the corresponding comparison when the common mass is positive. Do not silently change the convention during cleanup.

## Validation and next experiment

Four scratch declarations compiled against this snapshot: `float_mode_redundant`, `canonical_output`, `concrete_trace_factorization`, and `canonical_target_law`. Axiom reports contain only `propext`, `Classical.choice`, and `Quot.sound`. The experiment was run from `lean/` with `lake env lean /tmp/determinize-review-experiments.lean`; no production files were changed.

The first implementation experiment should change the exported result-mode premises and add `replayMean`, adapting the concrete trace-factorization interface. Build all Lean targets and check the exported axioms. Keep sampling representation and finite certificates for separate reviewable changes.

## Sampling representation: focused statement-surface comparison

After comparing `Statement/Types`, `Statement/Syntax`, `Statement/Primitives`, `Statement/Semantics`, `Traces/Semantics`, both public theorem surfaces, and the theorem exports, the preferred design is **sampling with an affinity, or mean without one**, keeping explicit primitive constructors:

```lean
inductive Affinity where
  | E | G

inductive DistributionAction where
  | sample (affinity : Affinity)
  | mean

-- Within Expr:
| uniform (action : DistributionAction) (lower upper : Expr Literal)
| gaussian (action : DistributionAction) (mean variance : Expr Literal)
```

This is three cases, expressed as a sum rather than three nullary constructors. It allows one sample typing rule quantified over affinity. A flat `sampleE | sampleG | mean` has the same information, but requires either splitting that rule or an auxiliary function to construct a sample from its affinity.

### Why this fits the actual statements

- **Syntax:** all primitives retain their current explicit arities; there is no recursive distribution descriptor and no new mutual recursion.
- **Typing:** unlike samples, a mean does not require its result affinity to be stored in syntax. Use two direct rules per primitive rather than a compatibility predicate. For Gaussian, the sample rule types `.gaussian (.sample a) μ v` at `.float a`; the mean rule types `.gaussian .mean μ v` at `.float a`. Both require μ at `.float a` and v at `.float .G`. This is the same principle as the existing mode-free arithmetic constructors. All other primitive parameter premises remain unchanged.
- **Primitive laws:** `uniformFiber`, `gaussianFiber`, and the other fibers already depend on kind and evaluated parameters, not affinity. They would distinguish `sample _` and `mean`. Keep domain checks and evaluation of all parameters, including Gaussian variance when taking its mean.
- **Determinization:** `.sample .E` becomes `.mean`; `.sample .G` and `.mean` stay unchanged. Operand expressions are recursively transformed in every case. The sampling annotation is necessary for this decision; an affinity on mean is not.
- **Traces:** `record` and `outputGivenTraceAt` currently recognize only `(G, stochastic, op)`. They would recognize `(.sample .G, op)`. Both existing mean cases already behave alike here.
- **Theorems:** exported claims use typed source programs, source form, determinization, and laws. They do not require an affinity annotation on means. Source syntax remains unchanged up to renaming/encoding. A preservation argument needs to move an E-sample typing derivation to the corresponding mean rule, retaining the parameter premises.

### What changes, and what does not

The two current mean syntaxes collapse to one. Their typing derivations do not collapse: the result affinity and parameter requirements remain explicit in `Typed`. For example, the erased form of an old E-annotated mean with G arguments can now also be typed G. Thus this is not a bijective renaming of raw syntax preserving every negative typing judgment; it removes a syntactic restriction imposed by the old redundant label.

Every old typed term should translate to a new typed term of the same type, and source programs should retain their typing behavior. These are proof obligations for an implementation, not results established by this review. For arbitrary new mean-containing terms, re-establish the relevant internal lemmas using their actual typing derivations; do not assume an arbitrary fixed choice of old mean affinity suffices to translate them back.

The product representation saves eight typing constructors (one rule per primitive instead of sample/mean rules), but requires explaining a syntactic distinction that primitive execution and trace semantics do not observe. For a small, explicit reviewer-facing typing judgment, two direct rules are the clearer tradeoff. No compatibility relation or generic arity machinery is recommended.

This refines the earlier conservative recommendation: retaining the product minimizes immediate proof edits; removing mean affinity gives the clearer final statement surface. The recommendation is based on inspection, not a completed kernel-checked refactor.
