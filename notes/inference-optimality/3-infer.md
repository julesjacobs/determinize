# Step 3: `infer`, restructured for the proofs

`infer` now runs the reference algorithm of `1-claims.md` phase by phase, and every definition
it uses is total. Shape unification and the affinity solver are separate modules with complete
proofs: the unifier terminates, is sound and complete, and returns a most general unifier;
Lemma L and Lemma P hold for the solver. The three main theorems are not started. The only
`sorry`s are the six statements in `Theorems.lean`.

Behaviour is unchanged. On 2.5 million inputs the new `inferWithCertificate` returns exactly the
same annotated program and certificate as the old one, and the step-1 checker prints the same
output as before the rewrite (see "Tests").

## Where everything lives

| What | Where |
| --- | --- |
| `inferFloatTypedThm`, the sixth statement from the review | `Spec/Inference.lean`; `inferenceFloatTyping := sorry` in `Theorems.lean` |
| `Shape`, substitution, `unify` (Robinson's algorithm) and its termination proof | `Frontend/Unify.lean` |
| `Unifies`, `unify_sound`, `unify_complete`, `unify_mostGeneral`, `unify_idempotent`, `unify_mgu` | `Proof/Frontend/Unify.lean` |
| `AffinityTerm`, `AffinityConstraint`, `forcedGeneral`, `solveAffinities` and termination | `Frontend/Affinity.lean` |
| `Satisfies`, Lemma L (`satisfies_join`, `satisfies_meet`), Lemma P (`solveAffinities_spec` and its parts) | `Proof/Frontend/Affinity.lean` |
| Generation, decoration, decomposition, read-back, `infer`, `inferWithCertificate` | `Frontend/Infer.lean` |
| Old and new `inferWithCertificate` compared on every checker input | `notes/inference-optimality/Differential.lean` |

`Determinize.lean` imports both proof files, so `lake build` checks them. `Theorems.lean` does
not import them yet.

## The new structure of `infer`

```lean
def solveInput (input : Input) : Except String Solution := do
  let (draft, _) ← (generate [] input).run 0                              -- phase 1
  let relations := draft.relations
  let some shapes := unify (relations.map fun (s, t) => (s.shape, t.shape))  -- phase 2
    | throw "incompatible or infinite type shapes"
  let constraints :=
    relations.flatMap fun (s, t) => decompose (s.decorate shapes) (t.decorate shapes)
  let some affinities := solveAffinities constraints                      -- phase 3
    | throw "inconsistent E/G constraints"
  return { draft, shapes, affinities }

def infer (input : Input) : Except String (Core × Ty) := do               -- phase 4
  let s ← solveInput input
  return (s.draft.program s.affinities, s.type s.draft.ty)
```

`inferWithCertificate` shares `solveInput` and returns `s.draft.certificate s.type` instead of the
type. The certificate's type is always `s.type s.draft.ty`, so `infer` is still
`inferWithCertificate` followed by `Certificate.ty`, as in step 2.

| Reference algorithm (`1-claims.md`) | Implementation |
| --- | --- |
| 1. `gen Γ e = (τ, R, F)` | `generate Γ e : StateT Nat (Except String) Draft`. `τ` is `draft.ty`, `R` is `draft.relations` (one pair per cast). There is no `F`: see below |
| type variable `α`, affinity variable `a` | `UType.var n`; `AffinityTerm.var (.generated n)`. Both come from one counter `n` |
| 2. mgu `θ` of the shape-erased `R` | `unify (relations.map …)` on `UType.shape`; `θ : Nat → Shape` |
| decoration `Θ α` | `UType.decorate θ`, via `Shape.decorate (.leaf α)`. The float at position `p` of `θ α` gets `AffinityVar.leaf α p`, where `p` lists child indices from the root (0 and 1 for the two children of `prod`, `sum`, `arr`, 0 for a `list` element); a shape variable becomes `unit` |
| decomposition into `A` | `decompose` on decorated types: covariant, flipped in function arguments, `[]` on shapes that do not match |
| 3. `ρmax` | `solveAffinities`: G on `forcedGeneral constraints ∅`, E elsewhere, `none` if that violates a constraint |
| 4. read-back | `Draft.program ρ` (the input with site `s` annotated by `ρ`), `Solution.type` (`Θ t` under `ρ`), `Draft.certificate` |

Differences from the reference algorithm and from the old code:

* **No separate fixed values `F`.** An affinity term is a variable or a fixed affinity
  (`AffinityTerm.fixed`). An operand that must be G is cast to `general = float (fixed G)`, and a
  site with a requested affinity `m` has type `float (fixed m)` instead of a fresh variable forced
  to `m`. So the constraint system is just `R`, and Lemmas S and C need no `F`: Lemma C no longer
  sets a fresh `g := G`, and only unannotated sites have an affinity variable.
* **Unify, decorate, decompose** replace the lazy `lowerRelations`. The old code ran the same
  shape unification only as a check and discarded its result. It then expanded each type
  variable to a fresh head whenever a relation met a constructor, which builds the same decorated
  shapes one level at a time.
* **The solver** computes the variables that every solution sets to G (the closure of "below a
  G") to a fixpoint, sets them to G and the rest to E, and checks every constraint. The old
  solver made `size + 1` passes of forward E and backward G propagation. Both compute the greatest
  solution.
* **Read-back is total.** `Draft.program` rebuilds each node with the non-recursive
  `Draft.rebuild`, which returns `.unit` for a node whose children do not fit its constructor.
  `generate` never produces such a node. The old `finish` threw "internal annotation shape
  mismatch" there.
* **Error messages.** `infer` fails in three places: `generate` ("unbound core variable i"),
  `unify` ("incompatible or infinite type shapes", which replaces both "infinite type (occurs
  check)" and "incompatible type shapes"), and `solveAffinities` ("inconsistent E/G
  constraints", which now also covers the old "inconsistent E/G requirements"). The old messages
  "incompatible types", "expected type constructor" and "internal annotation shape mismatch"
  were unreachable and are gone. The corpus tests compare only the rejection stage, which is unchanged.
* **Fresh numbering** changed: one counter for everything, and decorated leaves are named by
  type variable and position rather than numbered.

No definition in `Frontend/Infer.lean`, `Frontend/Unify.lean` or `Frontend/Affinity.lean` is
`partial` or `private` (only some lemmas of the termination proofs are private): the proofs in
step 4 will live in other files and must refer to every definition.
The remaining `partial def`s in `Frontend/` are in the parser and pretty printer, outside the
statements.

## What is proved

No `sorry`; every theorem depends only on `propext`, `Classical.choice` and `Quot.sound`
(Lemma L only on `propext`).

**Shape unification** (`Proof/Frontend/Unify.lean`). `Unifies θ E` means `s.subst θ = t.subst θ`
for every `(s, t) ∈ E`.

* Termination: `unify` is defined by well-founded recursion on (number of variables in the
  equations, total size). Elimination removes a variable (`eliminate_decreases`), decomposition
  keeps the variables and shrinks the equations (`Shape.children_vars`).
* `unify_sound`: `unify E = some θ → Unifies θ E`.
* `unify_complete`: `Unifies δ E → ∃ θ, unify E = some θ`, and `unify_eq_none` is its
  contrapositive.
* `unify_mostGeneral`: `unify E = some θ → Unifies δ E → ∀ i, (θ i).subst δ = δ i`. Every unifier
  factors through `θ`, with itself as the second factor (the usual characterisation of an
  idempotent most general unifier).
* `unify_idempotent`: `(θ i).subst θ = θ i`.
* `unify_mgu` collects the above.

**Affinity solver** (`Proof/Frontend/Affinity.lean`). `Satisfies ρ C` means
`Ty.Sub (.float (lower.eval ρ)) (.float (upper.eval ρ))` for every constraint, which is the order
that `AffinityLE` in the statements uses.

* Lemma L: `satisfies_join`, `satisfies_meet` (pointwise `affinityJoin` and `affinityMeet`).
* Lemma P: `solveAffinities_sound` (the result is a solution), `solveAffinities_greatest` (it is
  above every solution), `solveAffinities_complete` (it exists whenever a solution does), and
  `solveAffinities_spec` combining them. They rest on `forcedGeneral_general` (every solution is G
  on the collected variables) and `below_forcedGeneral` (the collected set is closed).
* `affinityLE_iff`, `sub_float_iff`: the Boolean order in the solver against `Ty.Sub`.

## Tests

All runs are on the final tree. "Before" is commit `465895c`, the last commit before the rewrite.

| Run | Before | After |
| --- | --- | --- |
| `lake build` | only the `sorry` warnings of `Theorems.lean` | same (six warnings now) |
| test suite (`lean/test.sh` without `--wfail`) | passes | passes; same tests, the five Python suites take 8.3, 36.7, 88.6, 65.8 and 4.7 s (before: 8.4, 36.7, 88.9, 66.1 and 4.5 s) |
| checker, core grammar ≤ 6 | all claims hold, 21.3 s | all claims hold, 19.0 s |
| checker, full grammar ≤ 5 | all claims hold, 35.5 s | all claims hold, 29.9 s |
| checker, targeted | all claims hold, 3.0 s | all claims hold, 3.0 s |
| checker, corpus | all claims hold, 9.7 s | all claims hold, 9.3 s |

The checker's output is identical before and after, line for line: the statistics per size, and
the inferred affinities of every targeted and corpus program.

`Differential.lean` runs the old implementation (copied from `465895c`) and the new one on the
same inputs: every program of each enumeration together with every completion of it. It compares
the annotated program and the certificate exactly, and requires both to fail together.

| Inputs | Programs | Inputs compared | Accepted | Mismatches |
| --- | ---: | ---: | ---: | ---: |
| targeted | 67 | 319 | 188 | 0 |
| corpus (completions only up to 10 free sites) | 120 | 874 | 549 | 0 |
| core grammar ≤ 6 | 255,021 | 871,574 | 81,127 | 0 |
| full grammar ≤ 5 | 497,171 | 1,601,678 | 147,956 | 0 |

The new implementation is somewhat faster. Run back to back, the checker takes 19.2 s on the core
grammar up to size 6 against 24.3 s with the old implementation substituted, and
`inferWithCertificate` alone on the 255,021 programs takes 5.3 s against 7.8 s (all interpreted,
`lake env lean --run`). The test suite runs compiled code; its times did not change.

The post-edit hook runs `check.sh lean`, which builds with `--wfail`, so it reports a failure after
every Lean edit until step 4 removes the `sorry`s. During this step I checked single files with
`lake env lean` and ran the full build separately.

## For step 4

What is easier than `1-claims.md` predicted:

* The mgu module, the critical path, is done: about 170 lines of definitions and 240 of proofs.
* Lemmas L and P are done.
* Fixed affinities are part of the types, so Lemmas S and C have one kind of constraint.
* Read-back cannot fail, so claim 3 has no "`finish` cannot fail" step.
* The solver's order is `Ty.Sub` on floats, the same as `AffinityLE`, so optimality needs no
  translation between orders.

What to know, and what may be harder than predicted:

1. **Unfolding.** I checked in a scratch file that the definitions unfold per constructor.
   `simp [generate, …]` works for single-pattern cases such as `app`, but not for the cases that
   share an or-pattern (`uniform`, `add`, …); use `rw [generate]` first, then
   `simp [StateT.run, bind, StateT.bind, pure, StateT.pure, Except.bind, Except.pure, fresh,
   freshFloat, modifyGet, MonadStateOf.modifyGet, StateT.modifyGet]`. Read-back and relations
   unfold with `simp [Draft.program, Draft.rebuild, UType.affinity]` and
   `simp [Draft.relations]`. `decompose` has `decompose.induct`.
2. **Induct on the input, not on the draft.** `Draft` is generic (an `Input` node and a list of
   children), so an induction over drafts would meet the `rebuild` fallback and the nested list.
   Following `generate` by induction on `Input` computes each draft concretely.
3. **Counter invariants for Lemma C** are not proved yet: `generate` only increases the counter,
   and every type variable and generated affinity variable in its draft (types, relations, site
   types) is below the final counter or occurs in `Γ`. Lemma C needs both to extend `σ` on the
   fresh variables of each subterm.
4. **Lemma S′** needs `(s.decorate θ).shape = (s.shape.subst θ).subst fun _ => .unit` (so the two
   sides of a relation have equal decorated shapes once `θ` unifies them) and soundness of
   `decompose` under equal shapes, since `decompose` returns `[]` on a mismatch.
5. **Lemma C′** needs the erasure `Ty → Shape`, "`Ty.Sub` preserves shapes", and an affinity
   lookup `Ty → List Nat → Affinity` that follows the position convention of `Shape.decorate`, to
   define `ρ (.leaf α p)`. Set `ρ (.generated n)` from the completion.
6. **`inferFloatTypedThm` needs a lemma the three main theorems do not.** The root type's shape
   under `θ` is `float` or a shape variable (then `infer` returns `unit`). In the second case one
   must extend `θ` by that variable to `float` and show that the new leaves meet only each other
   in the decomposed constraints, so that setting them all to E keeps every constraint. This is the
   argument from `2-surface.md`, point 7. It needs its own statement about `decompose` and
   `decorate`, perhaps 100 to 200 lines.
7. **No kernel evaluation.** `unify`, `forcedGeneral` and `decompose` use well-founded recursion,
   so `decide` cannot evaluate `infer`. The `#guard`s in `Spec/Inference.lean` run compiled code and
   still pass; `2-surface.md`'s idea of turning them into `decide` examples no longer applies.
   Step 5 was going to drop kernel evaluation of the certificate anyway.

`lean/README.md` still describes `Frontend/` as unverified; `Frontend/Unify.lean` and
`Frontend/Affinity.lean` now have proofs in `Proof/Frontend/`. That belongs to the README update
that `2-surface.md` already leaves to step 4. `Differential.lean` can go with
`inferWithCertificate` in step 5.
