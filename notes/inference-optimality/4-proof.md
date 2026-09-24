# Step 4: the proofs (in progress)

Status: Lemmas S and C are proved, together with the inversion and decomposition lemmas. Left is
the assembly into the six theorems; they are still `sorry` in `Theorems.lean`.

## Done

| File | Contents | Lemma in `1-claims.md` |
| --- | --- | --- |
| `Proof/Frontend/Typing.lean` | `typed_<constructor>_inv` for every constructor that occurs in completions except `reject` (31), `hasVar_iff` | "Typed inversion modulo `sub`" |
| `Proof/Frontend/Decompose.lean` | `shapeOf`, `shapeOf_sub`, shapes of decorated and instantiated types, `instantiate_decorate`; `decompose_sound`; `affinityAt`, `Approx`, `approx_decorate`, `decompose_complete` | S′, C′(b) |
| `Proof/Frontend/Ground.lean` | `Ground`, `Solves`, `Agree`, `Robust`, `Context`; monad lemmas, tactic macros `unfold_generate`, `relations`, `read_back`; `program_le` | infrastructure |
| `Proof/Frontend/Soundness.lean` | `generate_sound` | S |
| `Proof/Frontend/Completeness.lean` | `generate_complete`, `site_complete`, `fresh₁`–`fresh₃`, `freshAffinity` | C |

**Lemma S** (`generate_sound`): every `σ : Ground` that solves the draft's relations types the
read-back program `d.program σ.affinities` at `σ.inst d.ty` in the context `Γ.map σ.inst`, and the
program matches the input. No counter invariant is needed.

**Lemma C** (`generate_complete`): if `ê` matches `e`, `Typed Γ' (interpret ê) T`, and the instance
of `Γ` is below `Γ'` for every substitution that agrees with `σ` below the counter `n`, then
`generate Γ e` succeeds from `n` with `d` and `n' ≥ n`, and `σ` extends to `σ₁` such that every `σ'`
that agrees with `σ₁` below `n'` solves the relations of `d`, puts `d.ty` below `T`, and reads back
`ê`. The robust form replaces the counter invariants of `3-infer.md` (item 3): a later subterm
changes only variables at or above its starting counter, and `Agree.step` carries the earlier
conclusions over. The context hypothesis is up to `Sub`, so no narrowing lemma is needed.

## Left

`Inference.lean`: `solveInput_typed` (Lemma S with the decorated most general shapes, any
instance of the remaining shape variables, and the greatest solution; Lemma S′ for the
relations), `solveInput_complete` (Lemma C in the empty context, then C′(a) with `unify_complete`
and `unify_mostGeneral`, C′(b) with the leaf affinities of the completion's types, Lemma P,
`program_le`), the six theorems, and `Theorems.lean`.

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
