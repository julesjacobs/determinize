# Refactor checklist

Each retained step gets its own commit. For each step: inspect consumers, implement, build the Lean library/CLI/tests, run relevant checks, compare before/after, and record whether to keep or discard it. No push. Preserve the mathematical claims and caller-selected certificate subjects. Larger proof edits may be split into preparation and completion commits only when independently useful.

- [x] 1. Remove redundant result-affinity parameters from public theorem statements.
- [x] 2. Replace concrete existential trace outputs with canonical functions.
- [x] 3. Use model extraction throughout the checker and end-to-end interface.
- [x] 4. Remove reconstructed certificate evidence and duplicated request metadata.
- [x] 5. Replace nested replay-validity conjunctions with named structures.
- [x] 6. Expose unconditional model integrability and simplify expected reward.
- [x] 7. Derive absorption escape from the model and certificate horizon.
- [x] 8. Consolidate unit-sum finite-distribution validation.
- [x] 9. Trim and organize the specification/proof boundary.
- [x] 10. Evaluate deriving terminal rows instead of storing absorbing-row evidence.
- [ ] 11. Replace Mode/Kind with Affinity and sample-affinity-or-mean syntax, using direct typing rules.
- [ ] 12. Run complete regression checks and review the final stack.

## Before/after decisions

Record the concrete interface difference, checks, and keep/discard decision under each step as it finishes. Discard unsuccessful experiments without disturbing earlier commits.

### 1. Public result affinity — KEEP

Before: each public claim quantified an unused result mode. After: the premise is `Typed [] program (.float .E)`; G results are covered by silent subtyping. No safety, integrability, or source-form premise changed. The frontend retains its inferred-type equality and converts at the boundary. The simpler exported signatures justify the small conversion cost. `lake build --wfail` and all Lean frontend/runtime/checker tests passed; exported theorem axiom reports remain standard.

### 2. Canonical trace outputs — KEEP

Before: concrete soundness and `MeanOnTraces` existentially packaged the output, and output-law corollaries repackaged the trace marginal. After: `kernelMean` and `replayMean` name the canonical outputs, the marginal is `traceLaw source`, and `targetLaw` gives the direct map equality. Generic factorizations still allow arbitrary output functions with a canonicalization lemma; arbitrary kernels remain existential where appropriate. This removes witness bookkeeping without choosing arbitrary disintegrations. `lake build --wfail`, exported axiom reports, and Lean tests passed.

### 3. Extracting checker — KEEP

Before: a candidate and a separately supplied model were replayed and compared, with generic Boolean checker contracts and a model equality module. After: `checked_expectedReward` consumes `CheckedModel source subject`; `checked_sourceExpectedReward` retains all source premises. Removed the equality checker, equality module, and unused abstract contract composition. About 90 net lines removed; the actual certified pipeline is easier to state. The full warning-free build, standard axiom reports, and Lean tests passed.

### 4. Candidate data — KEEP

Before: candidates stored the request and every recomputed step evidence tag. After: candidates contain initial index, states, and rows (kind/edges); the request is supplied separately. Replay still checks initial-state alignment, source scope, injectivity, successor coverage, exact weights, and outcomes. Removing the duplicate metadata reduces certificate size and makes authority explicit. The warning-free build and Lean mutation tests passed. Python export ground-truth, independent kernel replay, and rejection tests passed; the round-trip test was updated to the explicit request and passed on rerun. Step 3’s Python result suite also passed (optional Storm skipped).

### 5. Named replay invariants — KEEP

Before: consumers selected matrix and alignment facts through positional conjunction projections. After: `ReplayValid`, `Aligned`, and `MatrixValid` are Prop structures with named obligations. This adds explicit decidability adapters but removes brittle positional dependencies from consumers; no assumptions or runtime candidate fields were added. Full warning-free build, Lean tests, and independent generated-certificate kernel replay passed with standard axioms.

### 6. Finite-model expectation — KEEP

Before: `HasExpectedReward` bundled integrability with answer equality, although integrability was already proved privately for every model. After: `Model.expectedReward` is the canonical integral, `outputMeasure_integrable` is public, and certificate soundness states direct equality. Source-program integrability premises remain unchanged. The nonabsorbing loop example explicitly uses the unconditional theorem. Warning-free build, Lean tests, and a generated result theorem checked independently by the kernel all passed with standard axioms.

### 7. Absorption certificate — KEEP

Before: certificates carried an escape bound and required a positive horizon plus several inequalities. After: they carry values and a horizon, and check survival below one at every state. The uniqueness proof uses this inequality directly at the maximum-error state, so no maximum-survival proof or stored bound is needed. Horizon zero now works for terminal models; loops still fail. JSON escape diagnostics are derived. Warning-free build, Lean tests (including both boundary cases), and Python generated-result/mutation tests passed; optional Storm skipped.

### 8. Unit-sum distributions — KEEP

Before: elaboration checked probabilities, then the checked constructor rechecked and normalized them. After: one constructor checks nonnegativity and sum one and preserves the exact list. Removed the unused normalization proof module and changed internal test fixtures to probabilities. `.det` acceptance requirements remain unchanged. Roughly 40 net lines removed. Warning-free build, all Lean tests, the fast corpus, and independent generated typing certificates passed.

### 9. Specification boundary — KEEP

Before: reviewer-facing material was split between `Statement/` and `Traces/`, with exporter policy, an import-only shim, and an auxiliary reward recurrence mixed in. After: 11 files under `Spec/` contain the mathematical surface; `Theorems.lean` exports it. Exporter policy moved to `Finite/`, reward recursion to its proof module, and the shim was removed. Generic checker plumbing was already eliminated in step 3. Imports, namespaces, certificate output, current docs, and theorem-coverage tooling were updated. The conditional-expectation documentation now qualifies its probabilistic interpretation by positive output mass. Full warning-free rebuild, Lean tests, independent generated result theorem, and the repository theorem/axiom check all passed. The larger rename diff buys one accurate review boundary without changing the claims.

### 10. Terminal rows — REVISE AND KEEP

Built and tested a representation storing only transient rows, with terminal self-loops derived by an accessor. Discarded it: it added dependent proof arguments, an extra kind branch per matrix lookup, and 18 lines of generic matrix-property proofs to remove one field. The patch is not part of the stack.

The retained alternative keeps the uniform matrix representation and removes `Model.absorbing`, which no output-law proof needed. Terminal behavior is already fixed by `outputWithin`; the specification now says terminal rows are ignored. This broadens admissible raw models without changing any prior model’s output law. A terminal-row-to-divergence example proves the intended stopping behavior. Sparse export validation still checks its explicit terminal self-loops as a file-format policy. Both experimental and retained versions passed the full warning-free build and Lean tests; the retained version also passed independent generated-result kernel replay with standard axioms.
