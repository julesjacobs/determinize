# Domain-safety premise trial

Baseline: `a725b701380236eca30a7fc80c918983af4af95b`.

## Before and after

Before, typed determinization statements assumed `DoesNotGetStuck source`; expectation and trace soundness also concluded `DoesNotGetStuck target`.

After, they assume `PrimitiveDomainSafe source` and conclude `PrimitiveDomainSafe target`. This says exactly what typing does not supply: every distribution call reached at a finite depth has valid parameters almost surely. Typing still supplies structural progress; source integrability and source-form requirements are unchanged.

The definition already existed in Proof. It now replaces full non-stuckness in `Spec/Semantics.lean`. The recursion is the same size; the structural-stuck case is true because this predicate only checks distribution domains. No generic safety framework or proof-oriented syntax was added to the mathematical specification.

`Typing.primitiveDomainSafe_iff_doesNotGetStuck` already proves equivalence on typed closed expressions. The proof entry points use this bridge. Target typing preservation supplies the same equivalence for the result, so the change does not reduce the typed theorem's mathematical guarantee. Frontend theorem exports and source-result transfer now use the same premise.

## The finite-certificate exception

`Model.Matches` guarantees full non-stuckness without requiring typing. Replacing that condition by domain safety would weaken its guarantee. The existing full safety definition therefore moves to `Spec/FiniteModel/Safety.lean`, with `Model.Matches` and its proofs unchanged. It cannot honestly become proof-internal while remaining part of that public certificate contract.

Main/trace specification imports now expose just domain safety. The whole development still contains two predicates, just as before; each has a distinct purpose. The full specification includes both because the typed theorem and untyped certificate contracts differ. This corrects the earlier suggestion that full safety could disappear from the entire specification.

## Assessment

KEEP: the assumption names the remaining obligation directly, with the same mathematical scope under typing. The specification pays no extra abstraction cost. Proof changes are bridge applications, not changes to symbolic reasoning or finite replay. The benefit is clearer statements, not fewer total definitions or a larger class of typed programs.

## Validation

- [x] Scratch Lean checks: structural failure alone satisfies domain safety; reversed uniform bounds do not.
- [x] Kernel-check recovery of the old full-safety expectation conclusion using the new theorem and typing preservation.
- [x] All 117 corpus cases (including statistical checks) and Lean front-end tests pass.
- [x] `lake build --wfail` passes; all 41 reported axiom sets contain only propext, Classical.choice, and Quot.sound.
- [x] Lean tests, all 117 corpus cases, and all 24 independent generated typing certificates pass.
- [x] Review final diff and finalize keep/discard assessment: KEEP.

The first certificate run was interrupted by a concurrent final rebuild temporarily removing an imported `.olean`. The reproduced error was a missing `Proof/Measurability.olean`, not a failed generated proof. After the build finished, all 24 certificate checks were rerun successfully.
