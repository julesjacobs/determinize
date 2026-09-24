# Step 4: the proofs (in progress)

Status: no statement is proved yet; the six theorems in `Theorems.lean` are still `sorry`. This
commit only changes `Shape.decorate` so that the proofs get simpler; `infer`'s outputs are
unchanged (see "The change to `Infer.lean`").

## Plan

All proofs go to `lean/Determinize/Proof/Frontend/`, next to step 3's `Unify.lean` and
`Affinity.lean`, in namespace `Determinize.Proof.Frontend`.

1. `Typing.lean`: inversion of `Typed` modulo `sub`, one lemma per constructor that occurs in a
   completion (all but `reject`, which needs none).
2. `Decompose.lean`: the shape erasure `shapeOf : Ty → Shape` and `Ty.Sub` preserving it; shapes of
   decorated and instantiated types; Lemma S′ (`decompose_sound`: equal shapes and a solution of
   the decomposition give `Ty.Sub` of every instance) and Lemma C′(b) (`decompose_complete`: `Ty.Sub`
   of instances gives a solution, with the affinities read off at the leaves of the decorated
   shapes).
3. `Ground.lean`: ground substitutions (a `Ty` per type variable, an `Affinity` per affinity
   variable), agreement below a counter, the monad lemmas for unfolding `generate`, and
   monotonicity of read-back.
4. `Soundness.lean`, Lemma S: every ground solution of the draft's relations types the read-back
   program. Induction on `Input`.
5. `Completeness.lean`, Lemma C: a typed completion makes `generate` succeed and extends to a
   solution whose read-back is the completion. Induction on `Input`. The conclusion is to be
   *robust* (it holds for every substitution that agrees below the final counter), which should
   make a separate "variables are below the counter" invariant unnecessary.
6. `Inference.lean`: the six theorems. The two corollaries follow from the main three as
   `2-surface.md` says.

## The change to `Infer.lean`

`Shape.decorate` used to map a shape variable to `unit`; it now keeps it as `UType.var i`. The
remaining shape variables are unconstrained: `decompose` returns `[]` on them exactly as it did on
`unit`, and `Solution.type` instantiates them with `unit`, so the constraints, the solution, the
program, the type and the certificate are all the same as before. `./test.sh` (without `--wfail`),
`Checker.lean --targeted` and `Checker.lean --corpus` pass.

The point is `inferFloatTypedThm`. `3-infer.md` (item 6) expected an argument that extends `θ` by
`β ↦ float` and shows that the new leaves meet only each other. With shape variables kept, the
program is typed at the decorated root type under *every* instance of the remaining variables
(`decompose` constrains none of them, and `Ty.Sub` is reflexive), and choosing `float E` for all of
them gives a float type whenever the decorated root type is a variable. It also gives the clean
equation `(u.decorate θ).shape = u.shape.subst θ`.
