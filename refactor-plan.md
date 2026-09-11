# Refactor checklist

Each retained step gets its own commit. For each step: inspect consumers, implement, build the Lean library/CLI/tests, run relevant checks, compare before/after, and record whether to keep or discard it. No push. Preserve the mathematical claims and caller-selected certificate subjects. Larger proof edits may be split into preparation and completion commits only when independently useful.

- [x] 1. Remove redundant result-affinity parameters from public theorem statements.
- [x] 2. Replace concrete existential trace outputs with canonical functions.
- [x] 3. Use model extraction throughout the checker and end-to-end interface.
- [ ] 4. Remove reconstructed certificate evidence and duplicated request metadata.
- [ ] 5. Replace nested replay-validity conjunctions with named structures.
- [ ] 6. Expose unconditional model integrability and simplify expected reward.
- [ ] 7. Derive absorption escape from the model and certificate horizon.
- [ ] 8. Consolidate unit-sum finite-distribution validation.
- [ ] 9. Trim and organize the specification/proof boundary.
- [ ] 10. Evaluate deriving terminal rows instead of storing absorbing-row evidence.
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
