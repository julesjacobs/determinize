# Pro review of the Lean design and architecture

- Snapshot: `a725b701380236eca30a7fc80c918983af4af95b`.
- Model: 6 Pro, verified before submission.
- Input: all 91 project Lean source files (22,892 lines), specification first, plus `lean/README.md`, Lake configuration, and toolchain version. Mathlib was not bundled.
- Source bundle: `/tmp/determinize-pro-architecture-source.md`.
- Status: complete; both responses collected and locally assessed (2026-09-11). Conversation: https://chatgpt.com/c/6aa46cb2-5ea8-83eb-b811-35ee9c16141c.
- Objective: critically assess the entire current mathematical specification, exported theorem statements, design, and module architecture. Challenge the current design rather than defend the completed refactor.

## Review sequence

- [x] Upload the complete current development and verify 6 Pro.
- [x] Ask for a concrete end-to-end review of `let x = uniform[E](0,1) in x + 1`, covering checked typing, determinization, mathematical semantics, theorem premises, and finite target certification.
- [x] Save and independently assess the first response.
- [x] Ask Pro to generalize to every specification file, exported claim, and architectural boundary.
- [x] Assess concrete findings against the source; kernel-check uncertain proposed statements when useful.
- [x] Record a prioritized synthesis and next experiment, then pause the monitor.

## Full-review follow-up

After reading and assessing the first response, ask Pro to extend its critique to the entire attached development. Cover every `Spec/` file and exported theorem, plus the architecture of `Proof/`, `Checking/`, `Frontend/`, `Runtime/`, and `Finite/`. Inspect whether the human-review boundary contains precisely the semantic choices and assumptions; whether names and module dependencies express their actual responsibilities; whether redundant representations or witnesses remain; whether theorem statements are needlessly weak, strong, indirect, or misleading; and whether the pipeline establishes the desired relation to parsed and executed programs.

Ask it to examine representative hard cases: E/G sampling and means, Gaussian variance, higher-order programs and silent subtyping, divergence, observation rejection, zero output mass, infinite expectations, finite replay, absorption, and result certification. Preserve genuine premises unless it can justify their removal. Request precise declaration references, concrete alternative Lean interfaces, tradeoffs, prioritized recommendations, and a coverage inventory. Distinguish specification defects, architectural improvements, optional stylistic preferences, and uncertainties. Require it to challenge its own recommendations and say what should remain unchanged. Ask a further focused question only if a material uncertainty prevents a coherent synthesis.

## Findings

The full review found no false equation in the nine exported mathematical statements. This is a source-review conclusion, not a proof that the specification captures every intended claim. Pro inspected all 11 Spec files, the nine theorem exports, Checking, Frontend, Finite, Runtime, Main, and 14 Lean test files. Its inspection of the 59 Proof modules covered architecture and central bridges, with selective reading of tactic scripts. It did not run Lean or inspect Mathlib, the external corpus/scripts, or the paper.

The original bundle contained 91 library files; the follow-up supplied 15 CLI/test files. Raw responses: [focused case](pro-architecture-first-response.md), [full critique and coverage inventory](pro-architecture-full-response.md). These are rendered browser text, so mathematical formatting and attachment citation labels are imperfect.

This request authorizes review only. No production code, theorem statements, commits, or pushes were changed.

### Prioritized local synthesis

1. **Clarify successful-return observables and add normalized-law theorems.** `Spec/Main.lean:varianceThm` uses Mathlib variance directly on the unnormalized output measure. Its own documentation acknowledges this. For `μ = ½δ₃`, this gives `9/8`, whereas variance conditioned on successful return is zero. These numbers follow directly from the definition; the example has not been formalized locally. Add `acceptedOutputLaw p := (bigStepMeasure p Set.univ)⁻¹ • bigStepMeasure p` and probability, expectation, and variance results under positive mass. Preserve existing raw results. `conditionalExpectationThm` is currently a totalized quotient identity, including zero-mass division. The README also says “No conditioning theorem is claimed,” despite advertising that theorem earlier. Clarify the distinction from an executable rejection-sampling guarantee.

2. **Expose the central trace facts.** `Proof/CompactSoundness.lean:targetLaw` already gives the target joint law as the pushforward of source traces through their replay means. `soundnessDataE` supplies almost-everywhere equality with the Markov replay completion. Export source replay probability and the direct pushforward equation. `outputGivenTrace` forces supplied G values, so `replayOutputLaw` describes its unconditional definition more precisely; its conditional interpretation requires the theorem hypotheses and almost-everywhere qualification. The public semantics records successful real outputs: rejection and divergence both contribute zero. `Model.Matches` therefore does not certify rejection probabilities or rejection traces. Document that scope before considering a richer outcome semantics.

3. **Cut accidental imports before changing representations.** Locally confirmed dependencies include `FiniteModel.Replay → Finite.Explore`, `FiniteModel.Initial → Checking.Elaboration → Theorems`, `FiniteModel.MeasureLaws → OrdinarySemantics → SymbolicSoundness`, `Typing → Measurability`, and `Checking.Result → Theorems`. Separate candidate data from exploration; move literal/determinization and reduction equations into lightweight modules; separate ordinary semantic facts from determinization proofs; move source-result composition out of generic result checking. Success means the generic finite checker no longer imports global determinization soundness, with unchanged statements and acceptance behavior. Generic probability lemmas can also move out of language-specific proof modules.

4. **Remove phantom parameters and pilot shared symbolic syntax.** `SampleEnv laws n` and `SymbolicAction laws n` have constructors independent of `laws`; interpretation can take laws separately. `AffineExpr` duplicates the literal-polymorphic `Expr` constructors and binding traversals. Pilot `Expr (Affine n)` with a correspondence proof and compare proof complexity before retaining it. Keep symbolic `WellTyped`: its G-literal condition records zero E coefficients, which ordinary typing of each realization cannot recover. Site-local optional affinity requests could later replace the core-plus-positional-list frontend representation, but coordinate that migration rather than adding another duplicate AST.

5. **Consolidate trace proof paths only after an experiment.** The detailed and compact developments both contain an exact-depth lockstep induction. `CompactSoundness.soundnessDataE` currently calls detailed `StepTraces.soundness` for target safety before constructing compact factorization. Try obtaining target safety directly and making compact replay the principal path. Detailed traces may still be needed for depth accounting. A generic trace algebra is not automatically clearer than the current duplication; no deletion or compilation experiment was performed in this review.

6. **Improve the application-facing result interface; profile checking.** A small `ExactResult p` exposing a rational answer, safety, integrability, and its integral equation would hide state-vector bookkeeping. Reuse `Certified` typing/source-form/alignment through a numeric view; source transfer must still request source safety and integrability. Keep generic finite checking independent of typing. Profile kernel replay, state equality, dense weight checks, equations, and absorption separately before attempting sparse checkers or positive-edge escape rankings.

The acceptance contract needs its own explicit review inventory alongside mathematical Spec: `Certified`, affinity alignment, `ReplayValid`, scoping, supported finite calls, and result validity. Acceptance predicates under Proof affect which certificates pass, but helpers whose relationship to the public semantics is proved need not all move into Spec. Parsing, Float execution, and external file serialization remain separate unverified boundaries.

### Reproduced executable issues

- **Structural pretty-print roundtrip:** a scratch Lean program importing `Frontend.Compile` and `Frontend.Pretty` compiled `2 * 3`, printed `(3 * 2)`, recompiled that, and printed `(2 * 3)`. Equality after affinity erasure was `false`, violating the roundtrip property used in `Tests/Parsing.lean`. Literal-right multiplication is unconditionally swapped in elaboration. This changes syntax, not numeric meaning or certificate soundness. Make normalization idempotent and specify the printer contract separately from exact Lean serialization.
- **Misleading CLI variance:** `determinize --samples 20` on `1e200 + bernoulli[G](0.5) * 1e200` returned 20 finite values, with empirical mean about `1.45e200`, but printed variance `0.000000` for both source and target. Squared accumulation overflows and the final clamp conceals the invalid calculation. The true empirical variance here exceeds Float range, so report that limitation instead of zero. Reuse a centered online accumulator like `Tests/Corpus.lean` and check finiteness. A constant `1e200` also overflowed the intermediate formula, but its printed zero alone was not a convincing wrong-result example; the two-valued fixture is.

Scratch reproduction: `/tmp/pro-roundtrip.lean` checked and ran with `lake env lean --run`; `/tmp/pro-large.det` ran with the existing built CLI. Production code was unchanged, so no rebuild was needed for this review. The first-pass exported exact theorem was independently kernel-checked as recorded below.

### Optional extensions, not accepted fixes

- **E-dependent Gaussian variance:** Pro proposes affine arity two with mean coefficients `(1,0)`. This requires domain stability, joint measurability, and moment-growth proofs, not just a typing-rule edit. It is plausible but unimplemented and unverified locally. Preserve strict evaluation of every mean operand, including operands whose values do not affect the mean.
- **Finite degenerate stochastic laws:** evaluated point uniforms, zero-variance Gaussians, and zero-rate Poisson calls could be accepted by the finite backend. Current restrictions are supported-language limitations.
- **Symmetric multiplication:** mathematically plausible but introduces inference choices between which operand must be G; do not commute arbitrary effectful operands.
- **Nonabsorbing certificates and observable rewards:** closed zero-output components and certificates for mass/second moments could broaden exact results, with new uniqueness arguments.
- **Narrow arity-indexed symbolic arguments:** may remove list-length witnesses but can increase dependent pattern-matching costs; assess independently.

### Keep unchanged; revisions to the first response

Keep `sample E | sample G | mean`, silent structural subtyping, strict mean operand evaluation and domain checks, real-valued mathematical arithmetic, and almost-everywhere trace reasoning. Affinity describes dependence of results, not absence of sampling effects.

Keep source safety, source integrability, `HasExpectation`, and finite-second-moment premises where required. Keep replay coverage, exact weights, alignment, scoping, reachable-shape invariants, and bookkeeping progress.

Pro explicitly withdrew its first-pass recommendations to replace the supplied horizon with `model.size` and derive terminal rows. Keep the small horizon hint and uniform terminal-row representation. Redundant proof fields may be useful caches; removing them is not a goal in itself. The later assessment supersedes the tentative suggestions below.

### Recommended next work

Fix the two reproduced executable issues separately. Then add normalized observable results and public replay facts without changing existing statements. Follow with import cleanup and phantom-parameter removal. Pilot shared symbolic syntax and trace-proof consolidation separately, retaining each only after a before/after assessment. Profile certificates before redesigning them. No further Pro follow-up is needed to choose this sequence; pause the monitor.

### First response and local assessment

Raw rendered response: [pro-architecture-first-response.md](pro-architecture-first-response.md). Pro correctly noted that the original bundle contained library files only. The follow-up supplies `Main.lean` and all 14 Lean test files in `/tmp/determinize-pro-cli-tests.md`.

Verified against the code:

- The source-result bridge genuinely requires safety and integrability; typing and source form already exist in `Certified`, so a numeric-source adapter could reuse those facts. This is an interface improvement, not a discovered unsound theorem.
- `Frontend.Elaborate.draw2` stores the concrete sample label and a parallel list of optional affinity requests. An input syntax with optional site annotations could remove that representation duplication, but its overall cost remains to be assessed.
- `Proof/FiniteModel/Replay.lean` imports `Finite.Explore`; `Proof/FiniteModel/Initial.lean` imports `Proof.Checking.Elaboration`, which imports all exported theorems. Splitting candidate data from exploration and basic interpretation laws from end-to-end results would reduce coupling.
- Uniform moment lemmas and compositional safety helpers are private. A public semantic lemma API would help clients discharge actual source premises.
- Initial-index validity follows from the successful lookup in `Aligned.initial`; matrix nonnegativity follows mathematically from positive validated edges and the sum definition of `Candidate.weight`. These potential deletions have not been implemented or kernel-checked as standalone derivations.

Independent executable check: the example exports 13 states, horizon 12, and target expectation `3/2`. `/tmp/pro-review-example.result.lean` passed an independent `lake env lean` check; `machineReplay`, `modelMatches`, `resultAccepted`, and `expectedReward` report only standard axioms.

Pro's safety counterexample has the claimed unnormalized expectation: integrating `(x + 1/2)/2` over `0 ≤ x ≤ 1/2` gives `3/16`. Its geometric-amplification example also has an infinite absolute first moment by direct mathematical reasoning. Neither counterexample has been formalized locally in Lean; they support keeping existing premises.

Proposed canonical `model.size` absorption horizons and derived terminal rows are not yet accepted recommendations. The full-review follow-up explicitly requests checker-cost and proof-complexity comparisons, and explains why the previous dependent terminal-row experiment was discarded. No implementation changes made.

