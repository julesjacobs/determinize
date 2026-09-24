import Determinize.Spec.Inference
import Determinize.Proof.Semantics.Subtyping

/-!
# Ground substitutions for generation

The drafts that `generate` produces have types over type variables and affinity variables. A
`Ground` substitution replaces all of them; `Solves` says that it satisfies every subtyping
constraint of a draft. Lemma S (`Proof/Frontend/Soundness.lean`) types the read-back program
under every solution, and Lemma C (`Proof/Frontend/Completeness.lean`) builds a solution from a
typed completion.

Generation draws fresh variables from a counter, so Lemma C extends a substitution on the
variables at or above the counter, and must know that this does not disturb what is already
established. `Agree n σ σ'` says that two substitutions agree on the variables below `n`, and
`Robust n σ P` that `P` holds for every substitution that agrees with `σ` below `n`.

The file also collects the monad lemmas for unfolding `generate`, and shows that the read-back
program is monotone in the affinities (`program_le`).
-/

namespace Determinize.Proof.Frontend
open Determinize.Frontend Spec Spec.Paper

/-! ## Unfolding `generate` -/

theorem bind_eq_ok {ε α β : Type} {x : Except ε α} {f : α → Except ε β} {b : β} :
    x >>= f = .ok b ↔ ∃ a, x = .ok a ∧ f a = .ok b := by
  cases x <;> simp [bind, Except.bind]

theorem ok_bind {ε α β : Type} {a : α} {f : α → Except ε β} :
    (Except.ok a : Except ε α) >>= f = f a := rfl

theorem pure_eq_ok {ε α : Type} {a b : α} : (pure a : Except ε α) = .ok b ↔ a = b := by
  simp [pure, Except.pure]

/-- Unfold one call of `generate` in a hypothesis `(generate Γ e).run n = .ok (d, n')` into the
successful runs of its steps. -/
macro "unfold_generate" h:ident : tactic => `(tactic| (
  rw [generate] at $h:ident
  simp only [site, StateT.run_bind, StateT.run_pure, fresh, freshFloat, StateT.run_modifyGet,
    pure_bind, bind_eq_ok, pure_eq_ok, Prod.mk.injEq] at $h:ident))

/-- Unfold one call of `generate` in a goal `(generate Γ e).run n = …`, keeping `site`. -/
macro "unfold_generate" : tactic => `(tactic| (
  rw [generate]
  simp only [StateT.run_bind, StateT.run_pure, fresh, freshFloat, StateT.run_modifyGet,
    pure_bind]))

/-! ## Ground substitutions -/

/-- A type for every type variable and an affinity for every affinity variable. -/
structure Ground where
  types : Nat → Ty
  affinities : AffinityVar → Affinity

namespace Ground

def inst (σ : Ground) (u : UType) : Ty := u.instantiate σ.types σ.affinities

/-- `σ` makes the first type of every relation a subtype of the second. -/
def Solves (σ : Ground) (relations : List (UType × UType)) : Prop :=
  ∀ p ∈ relations, Ty.Sub (σ.inst p.1) (σ.inst p.2)

theorem solves_nil (σ : Ground) : σ.Solves [] := fun _ h => nomatch h

theorem solves_cons {σ : Ground} {p : UType × UType} {rest : List (UType × UType)} :
    σ.Solves (p :: rest) ↔ Ty.Sub (σ.inst p.1) (σ.inst p.2) ∧ σ.Solves rest := by
  simp [Solves]

theorem solves_append {σ : Ground} {front rest : List (UType × UType)} :
    σ.Solves (front ++ rest) ↔ σ.Solves front ∧ σ.Solves rest := by
  simp only [Solves, List.mem_append]
  exact ⟨fun h => ⟨fun c m => h c (.inl m), fun c m => h c (.inr m)⟩,
    fun h c m => m.elim (h.1 c) (h.2 c)⟩

@[simp] theorem inst_var (σ : Ground) (i : Nat) : σ.inst (.var i) = σ.types i := rfl
@[simp] theorem inst_unit (σ : Ground) : σ.inst .unit = .unit := rfl
@[simp] theorem inst_bool (σ : Ground) : σ.inst .bool = .bool := rfl
@[simp] theorem inst_float (σ : Ground) (a : AffinityTerm AffinityVar) :
    σ.inst (.float a) = .float (a.eval σ.affinities) := rfl
@[simp] theorem inst_prod (σ : Ground) (a b : UType) :
    σ.inst (.prod a b) = .prod (σ.inst a) (σ.inst b) := rfl
@[simp] theorem inst_sum (σ : Ground) (a b : UType) :
    σ.inst (.sum a b) = .sum (σ.inst a) (σ.inst b) := rfl
@[simp] theorem inst_list (σ : Ground) (a : UType) : σ.inst (.list a) = .list (σ.inst a) := rfl
@[simp] theorem inst_arr (σ : Ground) (a b : UType) :
    σ.inst (.arr a b) = .arr (σ.inst a) (σ.inst b) := rfl
@[simp] theorem inst_general (σ : Ground) : σ.inst general = .float .G := rfl

def setType (σ : Ground) (i : Nat) (A : Ty) : Ground :=
  ⟨Function.update σ.types i A, σ.affinities⟩

def setAffinity (σ : Ground) (i : Nat) (m : Affinity) : Ground :=
  ⟨σ.types, Function.update σ.affinities (.generated i) m⟩

/-- `σ` and `σ'` agree on the type variables and generated affinity variables below `n`. -/
def Agree (n : Nat) (σ σ' : Ground) : Prop :=
  ∀ i < n, σ'.types i = σ.types i ∧ σ'.affinities (.generated i) = σ.affinities (.generated i)

theorem Agree.refl (n : Nat) (σ : Ground) : σ.Agree n σ := fun _ _ => ⟨rfl, rfl⟩

theorem Agree.type {n : Nat} {σ σ' : Ground} (h : σ.Agree n σ') {i : Nat} (hi : i < n) :
    σ'.types i = σ.types i := (h i hi).1

theorem Agree.affinity {n : Nat} {σ σ' : Ground} (h : σ.Agree n σ') {i : Nat} (hi : i < n) :
    σ'.affinities (.generated i) = σ.affinities (.generated i) := (h i hi).2

/-- Agreement below `n` followed by agreement below a larger `n₁`. -/
theorem Agree.step {n n₁ : Nat} {σ σ₁ σ' : Ground} (a : σ.Agree n σ₁) (h : σ₁.Agree n₁ σ')
    (le : n ≤ n₁) : σ.Agree n σ' := fun i hi =>
  ⟨(h.type (by omega)).trans (a.type hi), (h.affinity (by omega)).trans (a.affinity hi)⟩

theorem Agree.setType {n i : Nat} {σ σ' : Ground} (h : σ.Agree n σ') (hi : n ≤ i) (A : Ty) :
    σ.Agree n (σ'.setType i A) := fun j hj => by
  have : j ≠ i := by omega
  simpa [Ground.setType, Function.update_of_ne this] using h j hj

theorem Agree.setAffinity {n i : Nat} {σ σ' : Ground} (h : σ.Agree n σ') (hi : n ≤ i)
    (m : Affinity) : σ.Agree n (σ'.setAffinity i m) := fun j hj => by
  have : AffinityVar.generated j ≠ .generated i := by simp; omega
  simpa [Ground.setAffinity, Function.update_of_ne this] using h j hj

@[simp] theorem setType_types (σ : Ground) (i : Nat) (A : Ty) (j : Nat) :
    (σ.setType i A).types j = if j = i then A else σ.types j := by
  simp [Ground.setType, Function.update_apply]

@[simp] theorem setType_affinities (σ : Ground) (i : Nat) (A : Ty) :
    (σ.setType i A).affinities = σ.affinities := rfl

@[simp] theorem setAffinity_types (σ : Ground) (i : Nat) (m : Affinity) :
    (σ.setAffinity i m).types = σ.types := rfl

@[simp] theorem setAffinity_generated (σ : Ground) (i : Nat) (m : Affinity) (j : Nat) :
    (σ.setAffinity i m).affinities (.generated j) =
      if j = i then m else σ.affinities (.generated j) := by
  simp [Ground.setAffinity, Function.update_apply]

end Ground

open Ground

/-- Split the relations of a draft node into those of its children and casts, and instantiate
the cast types. -/
syntax "relations" (Lean.Parser.Tactic.location)? : tactic
macro_rules
  | `(tactic| relations $[$loc]?) => `(tactic|
    simp only [Draft.relations, List.flatMap_cons, List.flatMap_nil, List.append_nil, solves_cons,
      solves_append, solves_nil, Draft.ty, inst_var, inst_unit, inst_bool, inst_float, inst_prod,
      inst_sum, inst_list, inst_arr, inst_general, AffinityTerm.eval] $[$loc]?)

/-- The read-back of a draft node. -/
macro "read_back" : tactic => `(tactic|
  simp only [Draft.program, List.map, Draft.rebuild, Draft.ty])

/-- `P` holds for every substitution that agrees with `σ` below `n`. -/
def Robust (n : Nat) (σ : Ground) (P : Ground → Prop) : Prop := ∀ σ', σ.Agree n σ' → P σ'

theorem Robust.extend {n n₁ : Nat} {σ σ₁ : Ground} {P : Ground → Prop} (h : Robust n σ P)
    (a : σ.Agree n σ₁) (le : n ≤ n₁) : Robust n₁ σ₁ P :=
  fun σ' h' => h σ' (a.step h' le)

/-- The instances of a context are pointwise subtypes of `Γ'`. -/
def Context (Γ : List UType) (Γ' : List Ty) (σ : Ground) : Prop :=
  List.Forall₂ Ty.Sub (Γ.map σ.inst) Γ'

theorem Robust.cons {n : Nat} {σ : Ground} {Γ : List UType} {Γ' : List Ty} {t : UType} {A : Ty}
    (hΓ : Robust n σ (Context Γ Γ')) (ht : Robust n σ fun σ' => Ty.Sub (σ'.inst t) A) :
    Robust n σ (Context (t :: Γ) (A :: Γ')) :=
  fun σ' h => List.Forall₂.cons (ht σ' h) (hΓ σ' h)

/-- A variable of a context is found in the corresponding position. -/
theorem context_getElem? {Γ : List UType} {Γ' : List Ty} {σ : Ground} (h : Context Γ Γ' σ)
    {i : Nat} {A : Ty} (hA : Γ'[i]? = some A) : ∃ t, Γ[i]? = some t ∧ Ty.Sub (σ.inst t) A := by
  induction Γ generalizing Γ' i with
  | nil => cases h; simp at hA
  | cons t Γ ih =>
    change List.Forall₂ _ (σ.inst t :: Γ.map σ.inst) Γ' at h
    cases h with
    | cons head rest =>
      cases i with
      | zero =>
        simp only [List.getElem?_cons_zero, Option.some.injEq] at hA
        subst hA
        exact ⟨t, rfl, head⟩
      | succ i => exact ih rest (by simpa using hA)

/-! ## Monotonicity of read-back -/

/-- Induction on drafts, with a hypothesis for every child of a node. -/
theorem Draft.induction {P : Draft → Prop}
    (node : ∀ e t children, (∀ c ∈ children, P c) → P (.node e t children))
    (cast : ∀ body t, P body → P (.cast body t)) (d : Draft) : P d :=
  Draft.rec (motive_1 := P) (motive_2 := fun children => ∀ c ∈ children, P c)
    (fun e t children ih => node e t children ih) (fun body t ih => cast body t ih)
    (fun _ h => nomatch h)
    (fun _ _ head tail c h => (List.mem_cons.1 h).elim (· ▸ head) (tail c)) d

theorem affinity_le {ρ ρ' : AffinityVar → Affinity}
    (h : ∀ v, Ty.Sub (.float (ρ v)) (.float (ρ' v))) (t : UType) :
    Ty.Sub (.float (t.affinity ρ)) (.float (t.affinity ρ')) := by
  cases t with
  | float a => cases a with
    | var v => exact h v
    | fixed m => exact .refl _
  | _ => exact .refl _

theorem rebuild_le (e : Input) {m m' : Affinity} (hm : Ty.Sub (.float m) (.float m'))
    {children children' : List Annotated} (h : List.Forall₂ AffinityLE children children') :
    AffinityLE (Draft.rebuild e m children) (Draft.rebuild e m' children') := by
  unfold AffinityLE at *
  match h with
  | .nil => cases e <;> simp [Draft.rebuild, Expr.Sitewise]
  | .cons ha .nil => cases e <;> simp [Draft.rebuild, Expr.Sitewise, ha, hm]
  | .cons ha (.cons hb .nil) => cases e <;> simp [Draft.rebuild, Expr.Sitewise, ha, hb, hm]
  | .cons ha (.cons hb (.cons hc .nil)) =>
    cases e <;> simp [Draft.rebuild, Expr.Sitewise, ha, hb, hc]
  | .cons _ (.cons _ (.cons _ (.cons _ _))) => simp [Draft.rebuild, Expr.Sitewise]

/-- Read-back is monotone: larger affinities give a larger program. -/
theorem program_le {ρ ρ' : AffinityVar → Affinity}
    (h : ∀ v, Ty.Sub (.float (ρ v)) (.float (ρ' v))) (d : Draft) :
    AffinityLE (d.program ρ) (d.program ρ') := by
  induction d using Draft.induction with
  | node e t children ih =>
    rw [Draft.program, Draft.program]
    refine rebuild_le e (affinity_le h t) ?_
    rw [List.forall₂_map_left_iff, List.forall₂_map_right_iff]
    exact List.forall₂_same.2 ih
  | cast body t ih => rw [Draft.program, Draft.program]; exact ih

end Determinize.Proof.Frontend
