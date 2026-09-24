import Determinize.Frontend.Unify

/-!
# Correctness of shape unification

`unify` succeeds exactly when the equations have a unifier (`unify_complete`, `unify_sound`), and
then returns a most general one: every unifier `δ` factors through the result `θ`, as
`(θ i).subst δ = δ i` for every variable `i` (`unify_mostGeneral`). In particular `θ` is
idempotent. `unify_mgu` collects these facts. Termination is part of the definition.
-/

namespace Determinize.Proof.Frontend
open Determinize.Frontend

/-- `θ` makes the two sides of every equation equal. -/
def Unifies (θ : Nat → Shape) (equations : List (Shape × Shape)) : Prop :=
  ∀ e ∈ equations, e.1.subst θ = e.2.subst θ

/-! ## Substitutions -/

theorem Shape.subst_subst (σ τ : Nat → Shape) (s : Shape) :
    (s.subst σ).subst τ = s.subst fun i => (σ i).subst τ := by
  induction s <;> simp_all [Shape.subst]

theorem Shape.subst_congr {σ τ : Nat → Shape} {s : Shape} (h : ∀ i ∈ s.vars, σ i = τ i) :
    s.subst σ = s.subst τ := by
  induction s <;> simp_all [Shape.subst, Shape.vars]

theorem Shape.subst_var (s : Shape) : s.subst .var = s := by
  induction s <;> simp_all [Shape.subst]

theorem Shape.subst_single_of_not_mem {i : Nat} {s t : Shape} (h : i ∉ s.vars) :
    s.subst (Shape.single i t) = s := by
  rw [← Shape.subst_var s, Shape.subst_subst]
  refine Shape.subst_congr fun j hj => ?_
  have : j ≠ i := fun e => h (e ▸ hj)
  simp [Shape.single, this, Shape.subst]

/-- A variable is no larger than any shape it occurs in, after any substitution. -/
theorem Shape.size_le_subst {σ : Nat → Shape} {i : Nat} {t : Shape} (h : i ∈ t.vars) :
    (σ i).size ≤ (t.subst σ).size := by
  induction t with
  | var j => simp [Shape.vars] at h; subst h; rfl
  | unit | bool | float => simp [Shape.vars] at h
  | prod a b iha ihb | sum a b iha ihb | arr a b iha ihb =>
    simp only [Shape.vars, Finset.mem_union] at h
    simp only [Shape.subst, Shape.size]
    rcases h with h | h
    · have := iha h; omega
    · have := ihb h; omega
  | list a ih => simp only [Shape.vars] at h; simp only [Shape.subst, Shape.size]; have := ih h; omega

/-- The occurs check: a variable is strictly smaller than any other shape it occurs in. -/
theorem Shape.size_lt_subst {σ : Nat → Shape} {i : Nat} {t : Shape} (h : i ∈ t.vars)
    (ne : t ≠ .var i) : (σ i).size < (t.subst σ).size := by
  cases t with
  | var j => simp [Shape.vars] at h; exact absurd (h ▸ rfl) ne
  | unit | bool | float => simp [Shape.vars] at h
  | prod a b | sum a b | arr a b =>
    simp only [Shape.vars, Finset.mem_union] at h
    simp only [Shape.subst, Shape.size]
    rcases h with h | h
    · have := Shape.size_le_subst (σ := σ) h; omega
    · have := Shape.size_le_subst (σ := σ) h; omega
  | list a => simp only [Shape.vars] at h; simp only [Shape.subst, Shape.size]
              have := Shape.size_le_subst (σ := σ) h; omega

/-! ## Unifiers -/

theorem unifies_nil (θ : Nat → Shape) : Unifies θ [] := by simp [Unifies]

theorem unifies_cons {θ : Nat → Shape} {s t : Shape} {rest : List (Shape × Shape)} :
    Unifies θ ((s, t) :: rest) ↔ s.subst θ = t.subst θ ∧ Unifies θ rest := by
  simp [Unifies]

theorem unifies_append {θ : Nat → Shape} {front rest : List (Shape × Shape)} :
    Unifies θ (front ++ rest) ↔ Unifies θ front ∧ Unifies θ rest := by
  simp only [Unifies, List.mem_append]
  exact ⟨fun h => ⟨fun e m => h e (.inl m), fun e m => h e (.inr m)⟩,
    fun h e m => m.elim (h.1 e) (h.2 e)⟩

theorem unifies_substEquations {θ σ : Nat → Shape} {equations : List (Shape × Shape)} :
    Unifies θ (substEquations σ equations) ↔ Unifies (fun i => (σ i).subst θ) equations := by
  simp only [Unifies, substEquations, List.forall_mem_map, Shape.subst_subst]

theorem unifies_children {s t : Shape} {children : List (Shape × Shape)}
    (h : s.children t = some children) (θ : Nat → Shape) :
    s.subst θ = t.subst θ ↔ Unifies θ children := by
  cases s <;> cases t <;> simp only [Shape.children, Option.some.injEq, reduceCtorEq] at h <;>
    subst h <;> simp [Unifies, Shape.subst]

theorem not_unifies_children {s t : Shape} (h : s.children t = none)
    (hs : ∀ i, s = .var i → False) (ht : ∀ i, t = .var i → False) (θ : Nat → Shape) :
    s.subst θ ≠ t.subst θ := by
  cases s <;> cases t <;> simp_all [Shape.children, Shape.subst]

/-- A unifier of `i = t` is unchanged by first substituting `t` for `i`. -/
theorem single_absorbed {δ : Nat → Shape} {i : Nat} {t : Shape} (h : δ i = t.subst δ) (j : Nat) :
    (Shape.single i t j).subst δ = δ j := by
  by_cases e : j = i
  · subst e; simp [Shape.single, h]
  · simp [Shape.single, e, Shape.subst]

/-- Both orientations of the equation between a variable and a shape. -/
theorem unifies_var_cons {θ : Nat → Shape} {i : Nat} {t : Shape} {rest : List (Shape × Shape)} :
    Unifies θ ((t, .var i) :: rest) ↔ Unifies θ ((.var i, t) :: rest) := by
  simp only [unifies_cons, eq_comm]

/-! ## The algorithm -/

/-- The elimination step: a most general unifier of the substituted equations, composed with
the substitution of `t` for `i`, is one of the original equations. -/
private theorem eliminate_sound {i : Nat} {t : Shape} {rest : List (Shape × Shape)}
    (fresh : i ∉ t.vars) {θ : Nat → Shape} (h : Unifies θ (substEquations (.single i t) rest)) :
    Unifies (fun j => (Shape.single i t j).subst θ) ((.var i, t) :: rest) := by
  refine unifies_cons.2 ⟨?_, unifies_substEquations.1 h⟩
  show (Shape.single i t i).subst θ = t.subst fun j => (Shape.single i t j).subst θ
  rw [← Shape.subst_subst, Shape.subst_single_of_not_mem fresh, Shape.single, if_pos rfl]

theorem unify_sound {equations : List (Shape × Shape)} {θ : Nat → Shape}
    (h : unify equations = some θ) : Unifies θ equations := by
  induction equations using unify.induct generalizing θ with
  | case1 => simp only [unify, Option.some.injEq] at h; subst h; exact unifies_nil _
  | case2 i rest ih | case5 i rest _ ih =>
    rw [unify.eq_2, if_pos rfl] at h
    exact unifies_cons.2 ⟨rfl, ih h⟩
  | case3 i t rest ne occurs =>
    rw [unify.eq_2, if_neg ne, if_pos occurs] at h; cases h
  | case4 i t rest ne fresh ih =>
    rw [unify.eq_2, if_neg ne, if_neg fresh] at h
    obtain ⟨θ', h', rfl⟩ := Option.map_eq_some_iff.1 h
    exact eliminate_sound fresh (ih h')
  | case6 t i rest notVar ne occurs =>
    rw [unify.eq_3 _ _ _ notVar, if_neg ne, if_pos occurs] at h; cases h
  | case7 t i rest notVar ne fresh ih =>
    rw [unify.eq_3 _ _ _ notVar, if_neg ne, if_neg fresh] at h
    obtain ⟨θ', h', rfl⟩ := Option.map_eq_some_iff.1 h
    exact unifies_var_cons.2 (eliminate_sound fresh (ih h'))
  | case8 s t rest hs ht children eq ih =>
    rw [unify.eq_4 _ _ _ hs ht, eq] at h
    obtain ⟨front, back⟩ := unifies_append.1 (ih h)
    exact unifies_cons.2 ⟨(unifies_children eq θ).2 front, back⟩
  | case9 s t rest hs ht eq =>
    rw [unify.eq_4 _ _ _ hs ht, eq] at h; cases h

/-- A unifier of the equations also unifies them after the elimination of `i`. -/
private theorem eliminate_preserves {δ : Nat → Shape} {i : Nat} {t : Shape}
    {rest : List (Shape × Shape)} (h : Unifies δ ((.var i, t) :: rest)) :
    Unifies δ (substEquations (.single i t) rest) := by
  obtain ⟨head, rest⟩ := unifies_cons.1 h
  refine unifies_substEquations.2 ?_
  rwa [funext (single_absorbed head)]

/-- Every unifier `δ` factors through the result `θ` of `unify`: applying `θ` first and then `δ`
is the same as applying `δ`. -/
theorem unify_mostGeneral {equations : List (Shape × Shape)} {θ δ : Nat → Shape}
    (h : unify equations = some θ) (hδ : Unifies δ equations) : ∀ i, (θ i).subst δ = δ i := by
  induction equations using unify.induct generalizing θ with
  | case1 => simp only [unify, Option.some.injEq] at h; subst h; intro i; rfl
  | case2 i rest ih | case5 i rest _ ih =>
    rw [unify.eq_2, if_pos rfl] at h
    exact ih h (unifies_cons.1 hδ).2
  | case3 i t rest ne occurs =>
    rw [unify.eq_2, if_neg ne, if_pos occurs] at h; cases h
  | case4 i t rest ne fresh ih =>
    rw [unify.eq_2, if_neg ne, if_neg fresh] at h
    obtain ⟨θ', h', rfl⟩ := Option.map_eq_some_iff.1 h
    intro j
    rw [Shape.subst_subst, funext (ih h' (eliminate_preserves hδ))]
    exact single_absorbed (unifies_cons.1 hδ).1 j
  | case6 t i rest notVar ne occurs =>
    rw [unify.eq_3 _ _ _ notVar, if_neg ne, if_pos occurs] at h; cases h
  | case7 t i rest notVar ne fresh ih =>
    rw [unify.eq_3 _ _ _ notVar, if_neg ne, if_neg fresh] at h
    obtain ⟨θ', h', rfl⟩ := Option.map_eq_some_iff.1 h
    have hδ := unifies_var_cons.1 hδ
    intro j
    rw [Shape.subst_subst, funext (ih h' (eliminate_preserves hδ))]
    exact single_absorbed (unifies_cons.1 hδ).1 j
  | case8 s t rest hs ht children eq ih =>
    rw [unify.eq_4 _ _ _ hs ht, eq] at h
    obtain ⟨head, rest⟩ := unifies_cons.1 hδ
    exact ih h (unifies_append.2 ⟨(unifies_children eq δ).1 head, rest⟩)
  | case9 s t rest hs ht eq =>
    rw [unify.eq_4 _ _ _ hs ht, eq] at h; cases h

/-- `unify` succeeds on every equations that have a unifier. -/
theorem unify_complete {equations : List (Shape × Shape)} {δ : Nat → Shape}
    (hδ : Unifies δ equations) : ∃ θ, unify equations = some θ := by
  induction equations using unify.induct with
  | case1 => exact ⟨_, unify.eq_1⟩
  | case2 i rest ih | case5 i rest _ ih =>
    rw [unify.eq_2, if_pos rfl]
    exact ih (unifies_cons.1 hδ).2
  | case3 i t rest ne occurs =>
    exact absurd (unifies_cons.1 hδ).1 fun e => by
      have := Shape.size_lt_subst (σ := δ) occurs ne
      simp only [Shape.subst] at e; rw [e] at this; exact lt_irrefl _ this
  | case4 i t rest ne fresh ih =>
    rw [unify.eq_2, if_neg ne, if_neg fresh]
    obtain ⟨θ', h'⟩ := ih (eliminate_preserves hδ)
    exact ⟨_, by rw [h']; rfl⟩
  | case6 t i rest notVar ne occurs =>
    exact absurd (unifies_cons.1 (unifies_var_cons.1 hδ)).1 fun e => by
      have := Shape.size_lt_subst (σ := δ) occurs ne
      simp only [Shape.subst] at e; rw [e] at this; exact lt_irrefl _ this
  | case7 t i rest notVar ne fresh ih =>
    rw [unify.eq_3 _ _ _ notVar, if_neg ne, if_neg fresh]
    obtain ⟨θ', h'⟩ := ih (eliminate_preserves (unifies_var_cons.1 hδ))
    exact ⟨_, by rw [h']; rfl⟩
  | case8 s t rest hs ht children eq ih =>
    rw [unify.eq_4 _ _ _ hs ht, eq]
    obtain ⟨head, rest⟩ := unifies_cons.1 hδ
    exact ih (unifies_append.2 ⟨(unifies_children eq δ).1 head, rest⟩)
  | case9 s t rest hs ht eq =>
    exact absurd (unifies_cons.1 hδ).1 (not_unifies_children eq hs ht δ)

/-- The result of `unify` is idempotent. -/
theorem unify_idempotent {equations : List (Shape × Shape)} {θ : Nat → Shape}
    (h : unify equations = some θ) (i : Nat) : (θ i).subst θ = θ i :=
  unify_mostGeneral h (unify_sound h) i

/-- `unify` fails only on equations without a unifier. -/
theorem unify_eq_none {equations : List (Shape × Shape)} (h : unify equations = none)
    (δ : Nat → Shape) : ¬ Unifies δ equations := fun hδ => by
  obtain ⟨θ, h'⟩ := unify_complete hδ
  rw [h] at h'; cases h'

/-- The most general unifier theorem: `unify` succeeds exactly on the equations that have a
unifier, and every unifier factors through its result. -/
theorem unify_mgu (equations : List (Shape × Shape)) :
    ((∃ δ, Unifies δ equations) ↔ ∃ θ, unify equations = some θ) ∧
      ∀ θ, unify equations = some θ →
        Unifies θ equations ∧ ∀ δ, Unifies δ equations → ∀ i, (θ i).subst δ = δ i :=
  ⟨⟨fun ⟨_, hδ⟩ => unify_complete hδ, fun ⟨θ, h⟩ => ⟨θ, unify_sound h⟩⟩,
    fun _ h => ⟨unify_sound h, fun _ hδ => unify_mostGeneral h hδ⟩⟩

end Determinize.Proof.Frontend
