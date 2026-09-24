# Step 4: the proofs (in progress)

Status: the inversion lemmas and the decomposition lemmas (S′, C′(b)) are proved. Lemmas S and C
and the assembly are left; the six theorems in `Theorems.lean` are still `sorry`.

## Done

| File | Contents | Lemma in `1-claims.md` |
| --- | --- | --- |
| `Proof/Frontend/Typing.lean` | `typed_<constructor>_inv` for every constructor that occurs in completions except `reject` (31), `hasVar_iff` | "Typed inversion modulo `sub`" |
| `Proof/Frontend/Decompose.lean` | `shapeOf` (erasure `Ty → Shape`), `shapeOf_sub`, shapes of decorated and instantiated types, `instantiate_decorate`; `decompose_sound`; `affinityAt`, `Approx`, `approx_decorate`, `decompose_complete` | S′, C′(b) |
| `Frontend/Infer.lean` | `Shape.decorate` keeps shape variables (previous commit; outputs unchanged) | |

`Approx ρ u T` says that `T` has the structure of the decorated type `u` wherever `u` is a
constructor other than a base type, with the affinity `ρ` gives at each float of `u`. It replaces
explicit position paths: `approx_decorate` shows that decorated types approximate their instances
when `θ` factors the shapes and the leaf variables read off the affinities (`affinityAt`), and
`decompose_complete` is an induction on the `Ty.Sub` derivation, which handles the contravariant
argument of `arr` for free. `decompose_sound` uses `decompose.induct`.

## Left

1. `Ground.lean`: ground substitutions, agreement below a counter, monad lemmas for `generate`,
   monotonicity of read-back.
2. Lemma S (`Soundness.lean`) and Lemma C (`Completeness.lean`), by induction on `Input`. Lemma C
   with a robust conclusion (for every substitution that agrees below the final counter).
3. `Inference.lean`: the six theorems, and wiring into `Theorems.lean`.

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
