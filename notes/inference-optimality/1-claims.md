# Step 1: are the optimality claims true?

Verdict in one line: **all three claims hold for the current `infer`**, provided
"completion" means *typable at some closed type* rather than *typable at
`float E`*. With the prompt's literal `float E` reading, claim 1 is false for
trivial reasons (non-float programs, and programs whose result type is
unconstrained such as `reject`); claims 2 and 3 survive either reading. No
change to `infer`'s behaviour is needed. A brute-force checker agrees on
3.6 million core-grammar programs up to size 7, on 0.5 million full-grammar
programs up to size 5, on 67 hand-written higher-order programs, and on the
whole `.det` corpus.

The checker is `notes/inference-optimality/Checker.lean`; see "Brute-force
results" for how to run it.

## Definitions

Everything is under `lean/Determinize/`. `Input` (`Spec/Frontend.lean`) is the
resolved source; `Core = Expr Rat` (`Checking/Certificate.lean`) is the
annotated term, and `interpret : Core → Expr` turns rational literals into
reals for `Typed`.

* **Sites.** The sample sites of `e : Input`, in preorder (the order of
  `sampleAffinities` in `Checking/Elaboration.lean`). A site is *free* if its
  annotation is `none`.
* **Completion.** `Completion e ê := e.matches ê = true ∧ ∃ τ, Typed [] (interpret ê) τ`.
  This is the definition to formalise (see "Exact claims"). The prompt's
  version has `Typed [] ê (.float .E)` instead of `∃ τ`; call that
  `FloatCompletion e ê`.
* **Order.** For two completions of the same `e`, `ê' ≤ ê` iff they agree
  everywhere except that at every site the affinity of `ê'` is below that of
  `ê` in the order `G ≤ E` (`Ty.Sub.general`). Explicit sites are equal in
  both, so the order is really pointwise on the free sites.

Reminder of the direction: `G ≤ E` means a `G` value may be used where an `E`
value is expected. The greatest completion is the one with the most `E`s,
which is the one determinization replaces by the most means.

## Verdict per claim

### Claim 1, soundness: `infer e = .ok (ê, c)` implies `Completion e ê`

**True**, and more precisely `e.matches ê = true ∧ Typed [] (interpret ê) c.ty`,
which is exactly what `certify` checks after the fact. Evidence: on every
enumerated program on which `infer` succeeds, `certify` accepts its output and
the independent oracle finds a typing of it.

**False as literally stated in the prompt** (`FloatCompletion`), for two
uninteresting reasons:

1. `infer` does not require the program to have a float type. `unit`, `true`,
   `[]`, `fun x => x`, `(1, true)` all infer fine (the CLI prints
   `Checked: ...` with the non-float type and only skips the trace theorem).
   They have no `float E` typing.
2. Even when a `float E` typing exists, the certificate type may not be a
   float: `infer` defaults an unconstrained type variable to `unit`
   (`finalType`), so `reject` (surface `observe(false)`) infers at `unit`
   although `Typed [] reject (float E)` holds, and so do `fst reject`,
   `let x = reject in x`, `reject 1`, and so on. The checker counts these:
   1,671 of the 497,171 full-grammar programs up to size 5. In all of them the
   inferred *term* `ê` is still typable at `float E`; only `c.ty` is not.

Neither is a defect of the inference. The fix is to state the claim with
`∃ τ` (or with `c.ty`), and to derive the `float E` version as a corollary
under the hypothesis `c.ty = .float m`.

### Claim 2, optimality: `infer e = .ok (ê, _)` and `Completion e ê'` imply `ê' ≤ ê`

**True**, with the greatest completion, not merely a maximal one. The set of
completions of `e` is closed under pointwise join *and* meet (both checked by
brute force), so when it is nonempty it has a greatest element, and `infer`
returns it.

Why join-closure holds despite binders: the typing relation is captured
exactly by a system of constraints `a ≤ b` between affinity variables over the
two-point lattice `G ≤ E`, plus fixed values (`G` from `mul`/`div`/`lt`/
variance-like positions, explicit annotations). Solutions of such a system are
closed under join and meet. Binder types (`lam`, `fix`, `let`, `matchSum`,
`matchList`, `reject`, `nil`, `inl`/`inr`) are just more affinity variables in
that system; two typings that disagree on a binder's affinities are two
solutions, and their join is another solution whose site projection is the
join of the two completions. Binder *shapes* cannot disagree where it
matters: structural subtyping relates only types of the same shape, so the
shape of every constrained position is fixed by unification, and an
unconstrained shape variable takes part in no affinity constraint (it can be
set to `unit`, which is what `finalType` does).

This also holds when `c.ty` is not a float, and it holds for
`FloatCompletion` (a `float E` completion is a completion).

### Claim 3, completeness: `infer e` fails implies no `Completion e ê`

**True.** `infer` fails only in these places, each of which implies the
constraint system has no solution: the shape pre-check `unifyShape` (no shape
unifier, so no typing at all, since `Ty.Sub` preserves shapes), `lowerRelations`
"incompatible types" (same), `force` / the final check in `solve` (an `E`-fixed
variable is below a `G`-fixed one along a chain of `≤`), and `bvar` out of
scope (`HasVar` fails). The `finish` shape mismatch cannot occur. Evidence: on
every enumerated program, `infer` succeeds if and only if the oracle finds a
typable completion; the counts "infer ok" and "typable" agree at every size.

`lean/README.md` currently says "Inference completeness and optimality are not
claimed; some valid programs can be rejected depending on conditional branch
order." That sentence predates commit `8e4698f` (relations are now solved
after a shape check instead of being unified eagerly). The branch-order
programs are in the checker's targeted list and pass in both orders. Update
the README when the theorems land.

## Exact claims to formalise

Reformulated only in the completion relation (`∃ τ` instead of `float E`) and
in stating soundness against the certificate type; everything else is the
prompt's claim. Suggested shapes (names are suggestions; step 2 decides):

```lean
open Determinize Determinize.Checking Determinize.Spec.Paper Determinize.Frontend

/-- `ê` fills exactly the omitted affinities of `e` and is closed and well-typed. -/
def Completion (e : Input) (ê : Core) : Prop :=
  e.matches ê = true ∧ ∃ τ, Typed [] (interpret ê) τ

/-- `ê' ≤ ê`: identical except that every sample site of `ê'` is below the
corresponding site of `ê` in the order `G ≤ E`. Define it structurally, like
`Input.matches`, with the site case
`(.uniform (.sample a') l' u') (.uniform (.sample a) l u) := a' ≤ a ∧ …`,
where `Affinity.le G _ = true`, `Affinity.le E E = true`, `Affinity.le E G = false`.
Equivalently `List.Forall₂ Affinity.le (sampleAffinities ê') (sampleAffinities ê)`
together with equality of the erasures; the structural version needs no such
side condition. -/
def Core.Below (ê' ê : Core) : Prop := …

/-- Claim 1. Implies `Completion e ê`. -/
def inferSoundThm : Prop :=
  ∀ (e : Input) (ê : Core) (c : Certificate),
    infer e = .ok (ê, c) → e.matches ê = true ∧ Typed [] (interpret ê) c.ty

/-- Claim 2: the inferred completion is the greatest one. -/
def inferOptimalThm : Prop :=
  ∀ (e : Input) (ê ê' : Core) (c : Certificate),
    infer e = .ok (ê, c) → Completion e ê' → Core.Below ê' ê

/-- Claim 3. -/
def inferCompleteThm : Prop :=
  ∀ (e : Input) (ê : Core), Completion e ê → ∃ ê' c, infer e = .ok (ê', c)
```

Corollaries worth stating (cheap once the three are proved), because the
paper's theorems are about `Typed [] program (.float .E)`:

* If `infer e = .ok (ê, c)` and `c.ty = .float m` then
  `Typed [] (interpret ê) (.float .E)` (claim 1 plus `Ty.Sub.general`).
* If `e.matches ê' = true` and `Typed [] (interpret ê') (.float .E)` then
  `infer e = .ok (ê, c)` for some `ê, c` with `Core.Below ê' ê` (claims 3 and 2).

Do **not** state "if `infer e = .ok (ê, c)` then `Typed [] ê (.float .E)`" without
the `c.ty` hypothesis; see claim 1 above.

Two remarks on the definitions, so that step 2 does not pick a subtly weaker
reading:

* `Completion` must quantify the type existentially, not fix `c.ty`: otherwise
  claim 2 would only compare completions that happen to use `infer`'s type,
  which is weaker than intended (a completion of `reject` at `float E` must
  count).
* The order must be on `Core` terms (after `matches`), not on `Input`; the
  `mean` action never occurs in a completion, so `Core.Below` may return
  `false` on it.

## Proof sketch

The proof is a standard "type inference with structural subtyping over a
finite lattice of base types" argument (Fuh–Mishra 1988; the shape trick is
the one from Traytel, Berghofer, Nipkow, APLAS 2011, which the code cites).
The steps below name the lemmas the Lean proof needs. The hard parts are
marked.

### 0. The reference algorithm

`infer` is `inferExpr` (constraint generation), `solveRelations` (shape check
and reduction of subtype relations to atomic affinity constraints), `solve`
(affinity propagation), `finish` (read back the term and certificate). Read
it as the following three-phase reference algorithm; step 3 should make the
implementation *be* this algorithm (see "Fix `infer` needs").

1. **Generation.** `gen Γ e = (τ, R, F)`: a type `τ` over type variables `α`
   and affinity variables `a`; relations `R ⊆ UType × UType` (each `(s, t)`
   means `s <: t`); fixed values `F` (`a = G` from `general`, `a = m` from an
   explicit annotation). Every site gets its own affinity variable; the site
   variables are a distinguished subset. This is `inferExpr` verbatim; the
   `Draft` records where casts (`require`) were inserted.
2. **Shapes.** Erase affinities from `R` to get a first-order unification
   problem over shapes (`unit | bool | float | prod | sum | list | arr | var`).
   Let `θ` be its most general unifier (fails ⇒ `infer` fails). *Decorate*:
   for every type variable `α`, put a fresh affinity variable at every
   `float` leaf of `θ α` (indexed by `α` and the position), and `unit` at
   every remaining shape variable; call the result `Θ α`. Note that two
   variables `α ≠ β` with `θ α = θ β` get *different* affinity variables (the
   comment "its substitution must stay separate" in `solveRelations`).
   Decompose every `(s, t) ∈ R` structurally under `Θ` into atomic constraints
   `a ≤ b` (`lowerRelations` rules: covariant in `prod`, `sum`, `list` and in
   the result of `arr`, contravariant in the argument of `arr`). Call the set
   `A`.
3. **Affinities.** A solution of `(A, F)` is an assignment `ρ` of `G`/`E`
   to affinity variables satisfying `F` and every `a ≤ b`. `solve` computes
   `ρmax`: `G` on the downward closure of the `G`-fixed variables, `E`
   elsewhere; fails iff some `E`-fixed variable lies below a `G`-fixed one.
4. **Read-back.** `ê := e` with site `s` filled by `ρmax(a_s)`; `c.ty = Θ τ`
   under `ρmax`; casts become `.sub` nodes.

### 1. Lemmas about `Ty.Sub` and `Typed`

* `Ty.Sub.refl`, `Ty.Sub.trans` exist in `Proof/Semantics/Subtyping.lean`.
* Sub inversion: `Sub s t` with `s` (or `t`) constructor-headed forces the
  same constructor on the other side and `Sub` on the components (flipped for
  the argument of `arr`). Corollary: `Sub s t` implies equal shapes.
* **Typed inversion modulo `sub`**, one lemma per constructor, e.g.
  `Typed Γ (.app f x) T → ∃ A R, Typed Γ f (.arr A R) ∧ Typed Γ x A ∧ Sub R T`,
  `Typed Γ (.lam b) T → ∃ A R, Typed (A :: Γ) b R ∧ Sub (.arr A R) T`,
  `Typed Γ (.bvar i) T → ∃ T', HasVar Γ i T' ∧ Sub T' T`,
  `Typed Γ (.real q) T → ∃ m, T = .float m`,
  `Typed Γ (.mul l r) T → ∃ m, Typed Γ l (.float .G) ∧ Typed Γ r (.float m) ∧ Sub (.float m) T`,
  and so on for all 40 constructors. Proof: induction on the derivation; the
  `sub` case is `Sub.trans`. This is long but mechanical; it is the bulk of
  the completeness proof.
* **Context narrowing**: `Typed Γ e T → Forall₂ Sub Γ' Γ → Typed Γ' e T`.
  Induction on the derivation; `bvar` uses `sub`. (Alternatively build the
  narrowing into the induction hypothesis of lemma 3 below, quantifying over
  contexts above `σ Γ`.)

### 2. Soundness of the constraint system

**Lemma S.** If `gen Γ e = (τ, R, F)` and a ground substitution `σ` (type
variables to `Ty`, affinity variables to `Affinity`) satisfies `F` and
`Sub (σ s) (σ t)` for all `(s, t) ∈ R`, then
`Typed (σ Γ) (interpret (fill e σ)) (σ τ)` and `e.matches (fill e σ) = true`,
where `fill` puts `σ a_s` at each free site and the explicit annotation at the
others (`F` makes `σ a_s` equal the annotation there).

Induction on `e`. Each `require d t` becomes one `Typed.sub` with the
relation `(d.ty, t)`. The `.real` case uses `Typed.real` at `σ a`; the site
cases use `Typed.uniform` etc. at `σ a_s`. Straightforward.

**Lemma S′ (decomposition, soundness direction).** If `ρ` satisfies the
atomic constraints obtained by decomposing `(s, t)` under `Θ`, then
`Sub (Θ s ρ) (Θ t ρ)`. Induction on the shape `θ s = θ t`; parked pairs of
unresolved variables give `Sub unit unit`. Together with Lemma S this gives
claim 1: `σ := Θ ∘ ρmax` solves `R` and `F`, so `ê = fill e σ` is typed at
`c.ty = σ τ`; `finish` produces exactly that term and certificate type.

### 3. Completeness of the constraint system

**Lemma C.** Let `gen Γ e = (τ, R, F)`, let `σ₀` be a ground substitution on
the variables of `Γ`, and let `ê` with `e.matches ê = true` and
`Typed Γ' (interpret ê) T` for some `Γ'` with `Forall₂ Sub (σ₀ Γ) Γ'`. Then
`σ₀` extends to a ground `σ` on the fresh variables of `gen` such that `σ`
satisfies `R` and `F`, `Sub (σ τ) T`, and `σ a_s` is the affinity of `ê` at
every site `s`.

Induction on `e`, using the inversion lemmas. Representative cases:

* `letE v b`: inversion gives `Typed Γ' v̂ V` and `Typed (V :: Γ') b̂ T`. IH on
  `v` gives `σ₁` with `Sub (σ₁ v.ty) V`. Then `Forall₂ Sub (σ₁ (v.ty :: Γ)) (V :: Γ')`,
  so IH on `b` applies in the larger context (this is why the lemma
  quantifies over `Γ' ≥ σ Γ`; without it one needs narrowing here).
* `lam b`: inversion gives `A, R` with `Typed (A :: Γ') b̂ R` and
  `Sub (.arr A R) T`. Set `σ α := A` for the fresh argument variable, IH on
  `b`, then `Sub (.arr A (σ b.ty)) (.arr A R)` by covariance and `Sub.trans`.
* `app f x`: inversion gives `A, R`; IH on `f` and `x`; set `σ α := A`,
  `σ ρ := R`; the relations `(f.ty, arr α ρ)` and `(x.ty, α)` hold by the
  IH's `Sub` conclusions and `Sub.trans`.
* `fix b`: like `lam` with the extra relation `(b.ty, ρ)`.
* `mul l r`: inversion gives `m`; IH on `l` at `float G` and on `r` at
  `float m`; set the fresh `g := G` (fixed by `F` anyway) and `a := m`.
* Sites: inversion gives `Typed` of the operands at the affinities demanded by
  the rule and the site's affinity `m` (which equals the explicit annotation
  if any, because `matches` forces it); set `σ a_s := m`.
* `bvar i`: `HasVar Γ' i T'`, `Sub T' T`, and `Sub (σ₀ Γ)[i] Γ'[i]` give
  `Sub (σ₀ Γ[i]) T` by `Sub.trans`.
* `reject`, `nil`, `inl`, `inr`: set the fresh variable to the type the
  derivation used.

**Lemma C′ (factoring through the shapes).** If `σ` satisfies `R`, then (a)
the shape erasure `‖σ‖` unifies the shape-erased `R`, hence the mgu `θ` exists
and `‖σ‖ = θ′ ∘ θ` for some `θ′` (**the mgu theorem**); (b) define `ρ` on the
decorated variable at leaf `(α, p)` as the affinity of `σ α` at position `p`
(well-defined by (a): `θ α` is a prefix of `‖σ α‖`); then `ρ` satisfies `A`
and `F`, and `ρ` agrees with `σ` on site variables (a site variable is its own
leaf). Part (b) is the completeness direction of the decomposition lemma:
`Sub (σ s) (σ t)` implies every atomic constraint produced by decomposing
`(s, t)`, by induction on the common shape with the `arr` argument flipped.

### 4. The solver

**Lemma L (lattice).** Solutions of `(A, F)` are closed under pointwise join
and meet. Immediate: `a ≤ b` and fixed values are preserved by `max` and
`min`.

**Lemma P (`solve`).** Let `Down` be the set of variables from which a
`G`-fixed variable is reachable along `≤`, and `Up` the set reachable *from* an
`E`-fixed variable. `solve` fails iff `Down ∩ Up ≠ ∅` (equivalently, iff
`(A, F)` has no solution: any solution is `G` on `Down` and `E` on `Up`);
otherwise it returns `ρmax := G on Down, E elsewhere`, which is a solution and
is above every solution. The loop in `solve` runs `size + 1` passes over the
constraint list; one pass advances every propagation front by at least one
edge, and paths have fewer than `size` edges, so the fixpoint is reached. (If
the proof is easier with an explicit fixpoint iteration, replace the loop by
one with fuel `size`; same behaviour.)

### 5. Assembling the theorems

* **Claim 1**: Lemmas S′ and S with `ρmax`, plus the observation that `finish`
  returns `fill e (Θ ∘ ρmax)` and `c.ty = Θ τ ρmax`.
* **Claim 2**: given `Completion e ê'`, Lemma C with `Γ = Γ' = []` gives `σ`,
  Lemma C′ gives `ρ` solving `(A, F)` with `ρ a_s = affinity of ê' at s`,
  Lemma P gives `ρ ≤ ρmax`, and `ρmax a_s` is the affinity of `ê` at `s`.
  `Core.Below ê' ê` follows since both are `fill e` of something.
* **Claim 3**: from `Completion e ê'`, Lemma C′(a) shows the shape check
  succeeds and `θ` exists; Lemma C′(b) shows `(A, F)` is solvable, so `solve`
  succeeds by Lemma P; `finish` cannot fail (child counts match by
  construction). Hence `infer e = .ok _`.

### Where it will be hardest

1. **The current `Infer.lean` cannot be reasoned about as written.**
   `resolve`, `occurs`, `unifyShape`, `lowerRelations`, `inferExpr`,
   `finalType` and `finish` are `partial def`s, which Lean treats as opaque
   constants: no equation lemmas, no induction. Step 3 must restructure.
   `inferExpr`, `finalType` and `finish` are structurally recursive in the
   term or the draft and only need `termination_by` or a `fuel` argument.
   The unifier is the real problem.
2. **The mgu theorem** (Lemma C′(a)): termination and most-generality of
   first-order unification over the shape signature. This is the single
   biggest new formal development (a few hundred lines to a thousand). It is
   needed for both claim 2 (factoring an arbitrary derivation's shapes through
   the inferred ones) and claim 3 (the shape check never rejects a typable
   program). There is no way around it: without it, one cannot relate a
   completion that types `reject` at `list (float E)` to `infer`'s `unit`.
   Recommendation: implement shape unification as its own small verified
   module (shape terms, substitution, `unify` by well-founded recursion on
   `(number of unbound variables, total size)`, soundness, and "if a unifier
   exists then `unify` succeeds and every unifier factors through it"). Then
   replace the lazy `lowerRelations` expansion by "unify shapes, decorate, then
   decompose" as in step 2 of the reference algorithm. Outputs are identical
   up to the names of fresh variables, which do not affect `ê` or `c`.
3. **Lemma C** is long (forty constructors, each with an inversion lemma), but
   every case is the same three lines. Budget for it; do not underestimate the
   inversion lemmas for the eight sampling primitives and their `mean`
   siblings (the `mean` cases are impossible because `matches` excludes them,
   which the inversion lemma must still say).
4. **The decomposition lemmas** (S′ and C′(b)) need a clean statement of
   "affinity at position `p` with polarity". The cleanest formulation is
   structural: define `decompose : Shape → (UType) → (UType) → List (Nat × Nat)`
   by recursion on the *shape*, and prove both directions by the same
   induction. Avoid explicit position paths.
5. Order of attack that keeps partial progress useful: (i) Lemmas L and P
   (self-contained, easy); (ii) inversion lemmas and Lemma S; (iii) Lemma C
   assuming shapes are given by *any* unifier; (iv) the mgu module; (v) the
   plumbing to `infer`. If (iv) stalls, claims 1 and 2 relative to a
   *hypothesised* mgu are still meaningful intermediate theorems; claim 3
   genuinely needs (iv).

## Brute-force results

`notes/inference-optimality/Checker.lean` is a standalone Lean script over
the project's own definitions. For each enumerated `Input` `e` it

1. enumerates every completion (every `E`/`G` assignment to the free sites);
2. decides typability of each completion independently of `infer`: an
   affinity-free shape unifier (`shapes`) computes the most general shape of
   every binder node (`lam`, `fix`, `reject`, `nil`, `inl`, `inr`), and
   `synth` tries every `E`/`G` decoration of those shapes, computing the least
   type of every subterm under each decoration (joins at `ite`, `match`,
   `cons`, `add`; literals at `float G`; `let` binds the least type, which is
   complete by narrowing);
3. validates every positive answer with the verified checker `certify`, so a
   "typable" verdict is backed by a `Typed` proof term, and cross-checks every
   verdict against `infer` run on the fully annotated completion;
4. checks claim 1 (`infer`'s output certifies and is a typable completion;
   and if `c.ty` is a float, the output is typable at `float E`), claim 2
   (every typable completion is pointwise below the inferred one), claim 3
   (`infer` fails only when no completion is typable), and closure of the
   typable completions under pointwise join and meet;
5. separately decides typability at `float E` by first unifying the result
   shape with `float`, to count the programs where `FloatCompletion` and
   `c.ty` disagree (the `reject`-like ones above).

The oracle shares one idea with `infer` (shapes come from unification and an
unconstrained shape becomes `unit`); everything else (binder decorations,
least types, join/meet) is independent, and a wrong shape would show up as a
disagreement with `infer` on a fully annotated program, or as a certificate
`certify` rejects.

Grammar. Terms are counted by constructor nodes. The core grammar has leaves
`x` (every variable in scope), `1`, `true`; unary `-e`, `poisson(e)`,
`exponential(e)`, `fun x => e`, `rec f x => e`; binary `e e`, `e + e`,
`e * e`, `e / e`, `e < e`, `let x = e in e`, `uniform(e,e)`, `uniform[E](e,e)`,
`uniform[G](e,e)`, `gauss(e,e)`; and `if e then e else e`. The full grammar
(`--data`) adds the leaves `[]` and `observe(false)` (core `reject`), the
unary `fst`, `snd`, `inl`, `inr`, `discrete_list`, `bernoulli`, `poisson[G]`,
`exponential[E]`, the binary pairs, `::`, `beta`, `gamma`, `gauss[E]`, and
both `match` forms. The targeted list has 67 hand-written programs: forcing
through first-order and higher-order functions (`apply`, `twice`, `compose`,
a function taking a function taking a function), recursion, branch order,
functions in branches, pairs, lists, sums, `discrete_list`, every primitive
as a sink, explicit annotations, the occurs check, and non-float results.

Results (all with zero problems; "infer ok" equals "typable" at every size):

| run | programs | `infer` ok = typable | ≥2 typable completions | some site forced `G` | time |
| --- | ---: | ---: | ---: | ---: | ---: |
| core grammar, size ≤ 5 (default) | 19,061 | 3,506 | 1,614 | 508 | 7 s |
| core grammar, size ≤ 6 | 255,021 | 30,358 | 15,363 | 6,003 | 28 s |
| core grammar, size ≤ 7 | 3,618,865 | 282,033 | 152,788 | 70,360 | 5 min |
| full grammar, size ≤ 5 | 497,171 | 58,558 | 23,912 | 11,920 | 37 s |
| targeted (67 programs) | 67 | 59 | | | 5 s |
| corpus (`tests/`, `examples/`, ≤ 12 free sites) | 119 checked, 1 skipped | | | | 10 s |
| corpus with `--max-free 16` | not completed | | | | > 10 min |

"≥2 typable completions" is the number of programs on which optimality is not
vacuous. Higher-order programs (containing `lam` or `fix`) are the majority
of every enumeration (e.g. 2,942,350 of the 3,363,844 programs of size 7;
203,868 of them typable). In the full grammar up to size 5, a `float E`
completion exists for 23,484 programs, and for 1,671 of them `infer`'s
certificate type is not a float; all 1,671 are `reject`-shaped as described
under claim 1.

To rerun, from `lean/` (the interpreter is fast enough; no build target):

```sh
lake env lean --run ../notes/inference-optimality/Checker.lean             # core ≤ 5, then targeted
lake env lean --run ../notes/inference-optimality/Checker.lean --enumerate --size 7      # ~5 min
lake env lean --run ../notes/inference-optimality/Checker.lean --enumerate --data --size 5
lake env lean --run ../notes/inference-optimality/Checker.lean --targeted
lake env lean --run ../notes/inference-optimality/Checker.lean --corpus
lake env lean --run ../notes/inference-optimality/Checker.lean --max-free 16 --corpus   # clickGraph: hours
lake env lean --run ../notes/inference-optimality/Checker.lean --enumerate --data --size 3 --verbose
```

`--verbose` prints every typable program with free sites and its inferred
affinities, and every program where a `float E` completion exists but the
inferred type is not a float. The exit code is 0 iff no claim was violated.
Memory is modest (the enumeration table only stores each size at the depths
it can still be used at); do not run several size-7 runs at once.

The one skipped corpus file, `examples/baselines/clickGraph.det`, has 16 free
sites and 11 binders; with `--max-free 16` the oracle (65,536 completions,
every decoration of every binder) had not finished after ten minutes and was
stopped. `infer` accepts the file (`./test.sh` covers it); only the
brute-force comparison is missing for it.

What the checker does not cover: literals other than `1` and `true`; sizes
above 7 (core) and 5 (full); the surface desugarings (`flip`, `observe`,
`discrete(...)`, `-`, `<=`, `discrete` with remainder) except through the
targeted list and the corpus; programs with more than 16 free sites.

## Fix `infer` needs

None for the claims to hold: `infer` computes the greatest completion and
fails exactly when there is none, on every program checked.

Changes that step 2 or 3 should make anyway:

* `lean/README.md`, "Surface extensions and inference limits": replace the
  sentence claiming that completeness and optimality are not claimed and that
  branch order can cause rejections. It is stale.
* For provability (step 3), restructure `Frontend/Infer.lean` as described
  under "Where it will be hardest": total `inferExpr`/`finalType`/`finish`, a
  verified shape unifier, and "unify, decorate, decompose" in place of the lazy
  `lowerRelations`. Behaviour must not change; the tests compare
  `sampleAffinities` and the pretty-printed type, and `certify` the
  certificate, so fresh-variable numbering is free to change.
* Optional cosmetic point that the `float E` corollary depends on: `finalType`
  maps an unconstrained type variable to `unit`. Leave it; the corollary is
  stated with the hypothesis `c.ty = .float m`.

## Handoff summary

* Claims 1, 2, 3 are true for the current `infer` with
  `Completion e ê := e.matches ê = true ∧ ∃ τ, Typed [] (interpret ê) τ`.
* The greatest completion exists whenever any does (typable completions are a
  lattice), so claim 2 needs no weakening to maximality.
* The prompt's `float E` reading breaks claim 1 only for non-float and
  `reject`-shaped programs; use `∃ τ` and derive the `float E` versions.
* No behavioural fix to `infer`; a structural rewrite is required for the
  Lean proof because of `partial def`, and a verified shape unifier is the
  critical path.
