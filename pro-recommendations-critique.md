# Independent Pro critique of the recommendations

- Status: complete; full response saved and locally assessed on 2026-09-12.
- Conversation: https://chatgpt.com/c/6aa4802b-7ff4-83eb-b2e8-4fe5bac94ee0
- Browser: in-app browser 1, tab 9 (reopen canonical URL if absent).
- Source snapshot: a725b701380236eca30a7fc80c918983af4af95b (still current).
- Attachments: all 91 library files, Main and 14 Lean tests, pro-architecture-review.md, pro-architecture-full-response.md.
- Objective: independently challenge the previous recommendations and decide which survive; review only, no implementation/commits/pushes.

## Checklist

- [x] Open a separate conversation, upload sources and prior recommendations, verify 6 Pro.
- [x] Submit focused critique of acceptance-normalized expectation/variance APIs using the half-Dirac example; verify generation.
- [x] Save focused response, independently assess claims against source.
- [x] Ask for adjudication of the entire recommendation list.
- [x] Save comprehensive response; assess disagreements and produce keep/revise/defer/drop decisions with reasons.
- [x] Update TODO.md, report surviving recommendations, pause five-minute monitor, and notify with say.

## Full follow-up after first assessment

Ask this independent reviewer to critique every recommendation in the attached prior review and local synthesis against actual source. Require a keep/revise/defer/drop table, concrete evidence and minimal alternative, expected benefit versus proof/maintenance/compatibility cost, dependencies, and priorities. Challenge whether alleged duplicates represent genuinely different invariants; whether proposed module movements remove actual dependencies; whether Expr (Affine n) shares usable traversals without expensive proof transports; whether removing laws parameters is truly harmless; and whether the detailed/compact trace proof paths can be consolidated without losing exact-depth reasoning. Examine the certificate facade, review boundary, primitive abstraction, site-local affinities, normalized laws, replay names/public facts, generic probability infrastructure, performance proposals, terminal rows/horizon, Gaussian variance and other extensions. Explicitly include the reproduced roundtrip and Float statistics bugs and distinguish a real fix from an oversized redesign. A classification based on field count or line count alone is insufficient. Do not assume the previous Pro is right, and do not manufacture disagreement. Give a short surviving work sequence and say which recommendations should be discarded outright. Preserve actual mathematical premises; require evidence for weakening them. Do not claim compilation. Do not stop after the focused normalization example.

## Monitoring

Use the pro skill and CUA browser UI. Do not interrupt or duplicate generation. Save rendered DOM text rather than relying on the clipboard (previous clipboard reads returned stale prompts). Once focused answer is complete, read and assess before sending full follow-up. While generation is unchanged, stay quiet. After coherent full adjudication, pause the monitor. Keep recommendations distinct from locally verified conclusions. Only ask another focused follow-up if a material unresolved issue prevents synthesis.

## Focused response and assessment

Saved in [pro-recommendations-focused-response.md](pro-recommendations-focused-response.md). Pro recommends REVISE: keep the existing raw and quotient theorems, correct observable/rejection-sampling documentation, defer a full normalized-law API, and drop its default-interface/highest-priority status.

Locally confirmed: `varianceThm` explicitly exposes the second-moment inequality; its documentation already states the normalized consequence. `conditionalExpectationThm` exports equality of first-moment/mass quotients. `--result` deliberately certifies unnormalized expectation, whereas sampling summaries use returned values. The half-Dirac example has no E draw to eliminate and diagnoses differing statistical conventions, not a failure of the determinization theorem. With common positive mass and equal first moments, the difference of normalized variances is the raw variance difference divided by mass; this is direct algebra, not a new determinization argument. A generic scaling bridge may improve convenience but has not been compiled here.

The critique convincingly downgrades the proposed API expansion. Preserve the distinction between law-level conditional means (already expressed for positive mass) and correctness/termination of a retrying sampler (not established). The existing conditional theorem comment about rejection sampling and rejected traces overstates its exported contract; the README also has inconsistent wording. These documentation recommendations survive. No implementation changes.

Full follow-up asks for a decision on every prior recommendation, including transitive import costs, representation invariants, proof consolidation risks, certificate interfaces/performance, primitive extensions, and minimal fixes for both reproduced executable issues. It explicitly notes that Welford alone cannot represent a variance exceeding Float range.

## Final adjudication

Full evidence and recommendation-by-recommendation decisions: [independent full response](pro-recommendations-full-response.md). This supersedes the priorities in `pro-architecture-review.md`; that earlier document remains the historical first assessment.

The independent critique supports a smaller immediate scope. The mathematical claims do not need redesign to address the findings. Concrete repairs and additive public lemmas survive; larger representation changes require separate before/after experiments.

| Recommendation | Decision | Surviving scope |
|---|---|---|
| Literal multiplication roundtrip | KEEP | Swap a right literal only when the left is not already literal; preserve structural regression tests and explicit affinities. Do not promise arbitrary-Core roundtrips. |
| Numerical statistics | REVISE | Centered accumulation plus nonfinite checks and an unavailable/overflow diagnostic. Welford alone cannot represent variance beyond Float range. Preserve the returned-value denominator. |
| Successful-return and conditioning documentation | KEEP | Explain positive-mass interpretation, zero-mass convention, rejection/divergence scope, and the absence of a retrying-sampler theorem. |
| Normalized-law suite as the default interface | DROP / DEFER | Drop the promotion and urgency; defer convenience wrappers. Existing moment/mass theorems imply the normalized results. Keep the conditional expectation theorem name. |
| Replay probability, measurability, target pushforward | KEEP | Add stable public lemmas without changing the nine existing proposition shapes. |
| Replay renaming | REVISE | Documentation or alias for `outputGivenTrace`; internal `normalizedOutputGivenTrace` is more misleading because it performs Markov completion, not division by mass. |
| Certificate review boundary | KEEP / REVISE | Explicit inventory by claim. Acceptance predicates matter to acceptance behavior but are not unchecked assumptions in the final semantic guarantee. Avoid moving all helpers into Spec. |
| Safety/moment and numeric Certified adapters | KEEP | Expose selected useful lemmas, preferably almost-everywhere continuation safety; retain source provenance and genuine safety/integrability premises. |
| ExactResult record | REVISE | Optional library facade; generated reward theorems already have simple conclusions. Do not replace full output-law correspondence with an answer-only object. |
| Seven import cuts | KEEP / REVISE | Coordinate the transitive cuts below; repair actual consumers. Syntax does not need an Op module just to remove its analytic import. |
| Generic probability layer | REVISE | Extract existing language-independent helpers; consider existing FiberGood/FiberSound before introducing another abstraction. Defer wholesale factorization redesign. |
| Phantom laws parameters | KEEP narrowly | Remove only datatype-level phantom indices; retain laws on actualMeasure, DomainSafe, and relevant assumptions. Broad source migration still needs builds. |
| Shared Expr (Affine n) | REVISE | Bounded pilot of structural lemmas plus a real substitution/sample-weakening consumer. No presumed net benefit. |
| Symbolic WellTyped and distinct safety predicates | KEEP | Do not replace symbolic typing by pointwise ordinary typing or conflate domain safety with full non-stuckness. |
| Detailed/compact proof consolidation | REVISE | Isolated experiment after shared-helper separation. Keep exact-depth accounting and concrete compact-replay identification. |
| Site-local optional affinities | DEFER | No demonstrated desynchronization bug; action parameterization is separate from literal parameterization. Do not combine with the shared-AST pilot. |
| Arity-indexed arguments and proof-field trimming | DEFER | Opportunistic separate trials; witness reduction may merely move complexity to dependent elimination. |
| Repeated compilation/model checking | KEEP | Retain raw typing certificate in an internal compilation result; tie emission evidence to the exact candidate. Preserve independent kernel replay. |
| Finite/extended expectation compatibility | KEEP, lower priority | Small generic compatibility facts; reuse existing generic zero-mass and rejection lemmas. |
| Certificate optimization | KEEP profiling / DEFER algorithms | Measure compiled and kernel checking separately before sparse checks, hints, or escape rankings. Preserve the declarative contract. |
| Terminal rows, horizon hints, custom equality | KEEP | Drop field-count-driven removal, canonical size horizons, and stylistic equality replacement. |
| Primitive contract | KEEP documentation / REVISE helpers | Document domain stability, affine means, measurable laws, moment growth, and optional exact backend. Refactor helpers opportunistically. |
| Gaussian E variance, symmetric multiplication, finite degeneracies | DEFER | Deliberate extensions with new proof/policy obligations. Joint Gaussian measurability already exists; uniform moment growth is the key new analytic work. |
| Nonabsorbing models, observable rewards, mixed source/mean theorem | DEFER | New supported cases, not repairs. Second-moment answers do not transfer to source by equality. |
| Serialization and transactionality | KEEP boundary / DEFER new guarantees | Preserve tests and distinguish generated certificate from kernel replay. Stronger external-byte proofs and atomic bundle output need a client requirement. |
| Diagnostics, categorical indices, trace-tag redesign | DEFER | Address demonstrated client needs; avoid speculative generalization. |
| Exact/numerical evaluator unification, runtime casts, intrinsically typed replacement | DROP as remedies | Different contracts and invariants; none addresses the findings proportionately. |

### Locally checked disagreements

The first-pass executable reproductions and focused mathematical assessment remain valid. In this second pass I additionally checked the following source facts:

- `Spec/Syntax.lean` imports Primitives, but explicit named constructors do not use `Op`; the necessary data dependencies are smaller than the prior proposed primitive-signature split suggested.
- `Proof/FiniteModel/Execution.lean` uses `direct_cumulative_mono`. `OrdinarySemantics.lean` defines it before `typed_determinize` and `sourceTags_of_sourceForm`. Removing an import from MeasureLaws requires giving Execution its actual lower-level dependency.
- `Proof/CompactTrace.lean` imports `TraceSoundness`. Changing the target-safety call in CompactSoundness alone cannot remove the detailed proof path.
- `PrimitiveDomainSafeAt` treats `.stuck` as true; `DoesNotGetStuckAt` treats it as false. Their equivalence needs typing. These predicates are not unconditional duplicates.
- `normalizedOutputGivenTrace` keeps mass-one replay and substitutes a Dirac kernel elsewhere; it does not normalize by mass. Completion terminology is more accurate.
- `Frontend.Program` retains Input and Certified, but not the raw Certificate used by serialization. Avoiding recompilation needs retained certificate data, not merely passing the existing Program.

The two routes to large determinization proofs are the initial-reification path through Checking.Elaboration/Theorems and the execution path through OrdinarySemantics/SymbolicSoundness. Their individual import edges were checked in this and the previous pass; successful isolation still requires an actual build after coordinated changes.

No proposed refactor, new theorem, or numerical fix was implemented or compiled in this review. Pro inspected source and relevant proof bodies, not Mathlib or the external scripts/corpus/paper. Its detailed coverage inventory is in the saved response. Claims of lower build cost, simpler symbolic proofs, and faster certificates remain hypotheses.

### Surviving work sequence

1. Fix the two reproduced executable issues in separate changes.
2. Repair observable/conditioning documentation; publish replay facts and selected premise-discharge/numeric adapters.
3. Cut imports as a coordinated dependency-isolation task; extract existing generic helpers where required.
4. Remove phantom parameters separately. Choose shared syntax, trace consolidation, or certificate profiling as subsequent bounded work according to need; compare before/after before retaining any redesign.

No new language or certificate extensions belong to the immediate repair sequence. The independent critique is complete; the five-minute monitor is paused. No code changes, commits, or pushes.
