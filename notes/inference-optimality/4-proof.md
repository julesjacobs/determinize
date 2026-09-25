# Step 4: the proofs

All six statements of `Spec/Inference.lean` are proved. `lake build --wfail` passes with no
`sorry` anywhere, and the axiom report of each of the six theorems in `Theorems.lean` lists only
`propext`, `Classical.choice` and `Quot.sound`. The statements, their definitions and the
`example`s are unchanged.

`infer` changed in one place (`Shape.decorate` keeps shape variables instead of turning them
into `unit`). Its outputs are unchanged; see "The change to `Infer.lean`".

**After review.** The six statements were replaced by one, `inferCorrectThm`, as suggested in
the review of PR #10: if `infer` fails, the input has no completion; if it succeeds, its program
is a completion, typed at the returned type, and every completion lies below it. The three
`float E` statements were dropped, since the returned type does not matter for determinization.
`inferCorrect` in `Proof/Frontend/Inference.lean` now follows from `solveInput_typed` and
`solveInput_complete`. The float part of `solveInput_complete` is gone, and with it
`eq_var_of_shape` and `eq_float_of_shape`. Keeping shape variables in `Shape.decorate` is no
longer needed, but it does no harm. The rest of this file describes the proofs before the change.

## Where everything lives

All proofs are in `lean/Determinize/Proof/Frontend/`, namespace `Determinize.Proof.Frontend`.

| File | Contents | Lemma in `1-claims.md` |
| --- | --- | --- |
| `Unify.lean`, `Affinity.lean` | unchanged from step 3: `unify_mgu`, `solveAffinities_spec` | mgu theorem, L, P |
| `Typing.lean` | `typed_<constructor>_inv` for every constructor that occurs in completions except `reject` (31), `hasVar_iff` | "Typed inversion modulo `sub`" |
| `Decompose.lean` | `shapeOf` (erasure `Ty → Shape`), `shapeOf_sub`, shapes of decorated and instantiated types, `instantiate_decorate`; `decompose_sound`; `affinityAt`, `Approx`, `approx_decorate`, `decompose_complete` | S′, C′(b) |
| `Ground.lean` | `Ground` substitutions, `Solves`, `Agree`, `Robust`, `Context`; monad lemmas and the tactic macros `unfold_generate`, `relations`, `read_back`; `program_le` (read-back is monotone) | infrastructure |
| `Soundness.lean` | `generate_sound` | S |
| `Completeness.lean` | `generate_complete`, `site_complete`, `fresh₁`–`fresh₃`, `freshAffinity` | C |
| `Inference.lean` | `solveInput_typed`, `solveInput_complete`, and `inferSound`, `inferOptimal`, `inferComplete`, `inferFloatSound`, `inferFloatComplete`, `inferFloatTyped` | assembly, C′(a) |

`Theorems.lean` imports `Proof/Frontend/Inference.lean` and proves the six public theorems by
these names.

## The proof

### Soundness

`generate_sound` (Lemma S) takes any ground substitution `σ : Ground` (a `Ty` for every type
variable, an `Affinity` for every affinity variable) that `Solves` the draft's relations, and
concludes that the read-back program `d.program σ.affinities` matches the input and is typed at
`σ.inst d.ty` in the context `Γ.map σ.inst`. Induction on the `Input`; each cast is one
`Typed.sub`. No counter invariant is needed.

`solveInput_typed` instantiates Lemma S with the substitution that sends a type variable `α` to
the instance of its decoration `(θ α).decorate (.leaf α)`, with the greatest solution `ρ` as
affinities, and **any** `v : Nat → Ty` for the shape variables that remain after decoration.
`decompose_sound` (Lemma S′) shows that it solves every relation: the two sides have the same
decorated shape because `θ` unifies them (`shape_decorate`), and `ρ` satisfies their
decomposition. So the program is typed at `(d.ty.decorate θ).instantiate v ρ` for every `v`.
With `v = fun _ => .unit` that is `infer`'s type (`inferSound`).

### Completeness and optimality

`generate_complete` (Lemma C): if `ê` matches `e` and `Typed Γ' (interpret ê) T`, and
`Context Γ Γ' σ'` (the instance of `Γ` is pointwise below `Γ'`) holds for every `σ'` that agrees
with `σ` below the counter `n`, then `generate Γ e` succeeds from `n` with some draft `d` and
counter `n' ≥ n`, and `σ` extends to some `σ₁` (agreeing with it below `n`) such that for
**every** `σ'` that agrees with `σ₁` below `n'`:

* `σ'` solves the relations of `d`;
* `σ'.inst d.ty` is a subtype of `T`;
* `d.program σ'.affinities = ê` (the read-back is the completion itself).

Induction on the `Input`, inverting the typing derivation with the lemmas of `Typing.lean` and
choosing each fresh variable from the derivation. The context hypothesis quantifies over `Sub`,
so no narrowing lemma is needed (`letE` adds the value's draft type, which is only below the
derivation's type).

The robust form ("for every `σ'` that agrees below `n'`") replaces the counter invariants that
`3-infer.md` expected (item 3 of "For step 4"): there is no lemma saying that a draft's variables
are below the counter. A later subterm only changes variables at or above the counter at which
it starts, and the robust conclusions of earlier subterms survive such changes by `Agree.step`.
The only counter fact is `n ≤ n'`, which is part of the conclusion.

`solveInput_complete` takes a completion typed at `T` in the empty context:

1. Lemma C with `Γ = Γ' = []` gives `d` and `σ₁`.
2. The erasure `δ α := shapeOf (σ₁.types α)` unifies the shape equations (`shapeOf_sub`,
   `shape_instantiate`), so `unify` succeeds with `θ` (`unify_complete`) and
   `(θ α).subst δ = δ α` (`unify_mostGeneral`). This is C′(a).
3. Define `ρ` by `ρ (.generated j) := σ₁.affinities (.generated j)` and
   `ρ (.leaf α p) := affinityAt (σ₁.types α) p`. The substitution `⟨σ₁.types, ρ⟩` agrees with
   `σ₁` on every type variable and generated affinity variable, so by the robust conclusion it
   solves the relations and reads back `ê`. `approx_decorate` shows that every decorated type
   `Approx`imates its instance under it, and `decompose_complete` turns each `Sub` between
   instances into a solution of the decomposition. This is C′(b). The robust form is what makes
   this step work without knowing that drafts contain no leaf variables.
4. `solveAffinities` succeeds with `ρmax ≥ ρ` (Lemma P), so `solveInput` succeeds, and
   `program_le` gives `AffinityLE ê (d.program ρmax)`.
5. If `T` is a float, then `(d.ty.decorate θ).shape.subst δ = .float`, so the decorated root type
   is a float or a shape variable (`eq_float_of_shape`, `eq_var_of_shape`), and its instance with
   `v = fun _ => .float .E` is a float.

`inferComplete` and `inferOptimal` read off steps 1 to 4. `inferFloatSound` and
`inferFloatComplete` are the corollaries that `2-surface.md` predicted (`Typed.sub` with
`sub_float_E`; completeness plus optimality). `inferFloatTyped` combines step 5 with
`solveInput_typed` at `v = fun _ => .float .E`.

### The change to `Infer.lean`

`Shape.decorate` used to map a shape variable to `unit`; it now keeps it as `UType.var i`. The
remaining shape variables are unconstrained: `decompose` returns `[]` on them exactly as it did on
`unit`, and `Solution.type` instantiates them with `unit`, so the constraints, the solution, the
program, the type and the certificate are all the same as before. The docstrings of
`Infer.lean` say so.

The point is `inferFloatTypedThm`. `3-infer.md` (item 6) expected an argument that extends `θ` by
`β ↦ float` and shows that the new leaves meet only each other in the decomposed constraints.
With shape variables kept, that argument disappears: `solveInput_typed` holds for every
instance `v` of the remaining variables, because `decompose` puts no constraint on them and
`Ty.Sub` is reflexive. Choosing `v = fun _ => .float .E` types the program at a float whenever
the decorated root type is a variable. It also gives the clean `shape_decorate`:
`(u.decorate θ).shape = u.shape.subst θ`.

## Checks on the final tree

| Check | Result |
| --- | --- |
| `lake build --wfail`, `./check.sh lean` | pass; 74 theorems, all on `propext`, `Classical.choice`, `Quot.sound` only |
| `./test.sh` (with `--wfail` again) | passes: the five Python suites and the corpus run |
| `Checker.lean --targeted` | all claims hold |
| `Checker.lean --corpus` | all claims hold (119 programs, `clickGraph.det` skipped as before) |
| `Checker.lean --enumerate --data --size 5` | all claims hold; the counts equal those of `1-claims.md` (497,171 programs, 58,558 accepted, 1,671 with a `float E` completion and a non-float inferred type) |
| `Differential.lean` against the pre-step-3 `infer` | no mismatches: targeted 319 inputs, corpus 874, core grammar ≤ 6 871,574, full grammar ≤ 5 1,601,678; the same counts as in `3-infer.md` |

The differential test compares the annotated program and the certificate exactly, so the
`Shape.decorate` change did not alter any output.

## Practical notes and dead ends

* Unfolding `generate`: `rw [generate]` and then a fixed `simp only` set (`StateT.run_bind`,
  `StateT.run_pure`, `StateT.run_modifyGet`, `fresh`, `freshFloat`, `pure_bind`, plus
  `bind_eq_ok` and `pure_eq_ok` in hypotheses). The macro `unfold_generate` does this. In Lemma S
  the site cases first split `cases r` and unfold `site`; in Lemma C, `site_complete` provides
  the run of `site r` and the macro keeps `site` folded.
* In Lemma C the run equation is proved by `unfold_generate; simp only [<runs of the subterms>,
  ok_bind]; rfl`, inside `refine ⟨_, _, by …, …⟩`. The `by` block assigns the draft and the
  counter; every other component that mentions the draft must be a `by` block or `?_` too
  (the `unit` and `bool` cases failed with a plain term).
* `Draft.program` and `Draft.relations` are well-founded, hence irreducible: rewrite with their
  equation lemmas (`simp only [Draft.program, …]`), never rely on `rfl`. Even the relations of a
  leaf node are not definitionally `[]`.
* Do not put `UType.affinity` into the `read_back` simp set: it turns the affinity of a site type
  `t` into a `match` on `t`, and then `rw` with `t.affinity σ'.affinities = m` fails. Soundness
  adds it only in the site cases.
* The unused-simp-argument linter does not look inside macros, so the macros may carry lemmas
  that some cases do not need. Outside macros, `--wfail` turns an unused simp argument into an
  error.
* Fresh variables: `fresh₁`, `fresh₂`, `fresh₃` and `freshAffinity` hide the updated substitution
  behind an existential, with the counters written `n + 1 + 1` as `generate` produces them.
  `n + 2` does not match the unfolded goals syntactically.

## Left for step 5

* `lean/README.md` (untouched here): the items listed in `2-surface.md` ("Stale documentation")
  and `3-infer.md`, plus the new proof files and the fact that `Frontend/` inference is now
  verified end to end.
* `Differential.lean` and `inferWithCertificate` can go together, as `3-infer.md` says. The
  checker `Checker.lean` still calls `inferWithCertificate` and `certify`.
* `Determinize.lean` still imports `Proof/Frontend/Unify.lean` and `Proof/Frontend/Affinity.lean`
  directly; `Theorems.lean` now imports them too, through `Proof/Frontend/Inference.lean`.
