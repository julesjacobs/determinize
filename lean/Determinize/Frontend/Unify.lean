import Mathlib.Data.Finset.Card

/-!
# Unification of type shapes

A shape is a type without its affinities. Structural subtyping relates only types of the same
shape, so inference first unifies the shapes of the two sides of every subtyping constraint.
`unify` is Robinson's algorithm. It terminates because every elimination removes a variable from
the equations and every decomposition makes them smaller. `Proof/Frontend/Unify.lean` proves that
it returns a most general unifier whenever there is a unifier.
-/

namespace Determinize.Frontend

inductive Shape where
  | var (index : Nat)
  | unit | bool | float
  | prod (left right : Shape)
  | sum (left right : Shape)
  | list (element : Shape)
  | arr (argument result : Shape)
deriving DecidableEq, Repr, Inhabited

namespace Shape

/-- Replace every variable `i` by `σ i`. -/
def subst (σ : Nat → Shape) : Shape → Shape
  | var i => σ i
  | unit => unit
  | bool => bool
  | float => float
  | prod a b => prod (a.subst σ) (b.subst σ)
  | sum a b => sum (a.subst σ) (b.subst σ)
  | list a => list (a.subst σ)
  | arr a b => arr (a.subst σ) (b.subst σ)

/-- The substitution of `t` for the variable `i`. -/
def single (i : Nat) (t : Shape) (j : Nat) : Shape :=
  if j = i then t else var j

def vars : Shape → Finset Nat
  | var i => {i}
  | unit | bool | float => ∅
  | prod a b | sum a b | arr a b => a.vars ∪ b.vars
  | list a => a.vars

/-- The number of constructors. -/
def size : Shape → Nat
  | var _ | unit | bool | float => 1
  | prod a b | sum a b | arr a b => a.size + b.size + 1
  | list a => a.size + 1

/-- For two shapes that are not variables, the equations between their children, or `none` if
their head constructors differ. -/
def children : Shape → Shape → Option (List (Shape × Shape))
  | unit, unit | bool, bool | float, float => some []
  | prod a b, prod c d | sum a b, sum c d | arr a b, arr c d => some [(a, c), (b, d)]
  | list a, list b => some [(a, b)]
  | _, _ => none

theorem vars_subst_single {i : Nat} {t : Shape} (fresh : i ∉ t.vars) :
    ∀ s : Shape, (s.subst (single i t)).vars ⊆ (s.vars ∪ t.vars).erase i
  | var j => by
    by_cases h : j = i
    · subst h
      intro k hk
      simp only [subst, single, if_pos] at hk
      exact Finset.mem_erase.2 ⟨fun e => fresh (e ▸ hk), Finset.mem_union_right _ hk⟩
    · simp [subst, single, h, vars]
  | unit | bool | float => by simp [subst, vars]
  | prod a b | sum a b | arr a b => by
    have ha := vars_subst_single fresh a
    have hb := vars_subst_single fresh b
    simp only [subst, vars]
    intro k hk
    rcases Finset.mem_union.1 hk with hk | hk
    · have := ha hk; simp only [Finset.mem_erase, Finset.mem_union] at this ⊢; tauto
    · have := hb hk; simp only [Finset.mem_erase, Finset.mem_union] at this ⊢; tauto
  | list a => by simpa [subst, vars] using vars_subst_single fresh a

end Shape

/-- Apply a substitution to both sides of every equation. -/
def substEquations (σ : Nat → Shape) (equations : List (Shape × Shape)) : List (Shape × Shape) :=
  equations.map fun e => (e.1.subst σ, e.2.subst σ)

/-- The variables of a list of equations. -/
def equationVars (equations : List (Shape × Shape)) : Finset Nat :=
  equations.foldr (fun e acc => e.1.vars ∪ e.2.vars ∪ acc) ∅

/-- The total size of a list of equations. -/
def equationSize (equations : List (Shape × Shape)) : Nat :=
  (equations.map fun e => e.1.size + e.2.size).sum

/-- Decomposition keeps the variables and makes the equations smaller. -/
theorem Shape.children_vars {s t : Shape} {equations : List (Shape × Shape)}
    (h : s.children t = some equations) :
    equationVars equations = s.vars ∪ t.vars ∧ equationSize equations < s.size + t.size := by
  cases s <;> cases t <;> simp only [children, Option.some.injEq, reduceCtorEq] at h <;> subst h <;>
    refine ⟨?_, ?_⟩ <;> simp [equationVars, equationSize, vars, size] <;>
    first | omega | (ext; simp; tauto)

private theorem equationVars_map_single {i : Nat} {t : Shape} (fresh : i ∉ t.vars)
    (rest : List (Shape × Shape)) :
    equationVars (substEquations (.single i t) rest) ⊆ (equationVars rest ∪ t.vars).erase i := by
  induction rest with
  | nil => simp [equationVars, substEquations]
  | cons e rest ih =>
    simp only [equationVars, substEquations, List.map_cons, List.foldr_cons] at ih ⊢
    have h₁ := Shape.vars_subst_single fresh e.1
    have h₂ := Shape.vars_subst_single fresh e.2
    intro k hk
    simp only [Finset.mem_union] at hk
    rcases hk with (hk | hk) | hk
    · have := h₁ hk; simp only [Finset.mem_erase, Finset.mem_union] at this ⊢; tauto
    · have := h₂ hk; simp only [Finset.mem_erase, Finset.mem_union] at this ⊢; tauto
    · have := ih hk; simp only [Finset.mem_erase, Finset.mem_union] at this ⊢; tauto

/-- Eliminating a variable removes it from the equations. -/
private theorem eliminate_decreases {i : Nat} {t : Shape} (fresh : i ∉ t.vars)
    {rest : List (Shape × Shape)} {all : Finset Nat} (mem : i ∈ all)
    (contains : t.vars ∪ equationVars rest ⊆ all) :
    (equationVars (substEquations (.single i t) rest)).card < all.card := by
  refine lt_of_le_of_lt (Finset.card_le_card ((equationVars_map_single fresh rest).trans ?_))
    (Finset.card_lt_card (Finset.erase_ssubset mem))
  exact Finset.erase_subset_erase i (by rwa [Finset.union_comm])

private theorem equationSize_cons (s t : Shape) (rest : List (Shape × Shape)) :
    equationSize ((s, t) :: rest) = s.size + t.size + equationSize rest := by
  simp [equationSize]

private theorem equationSize_append (front rest : List (Shape × Shape)) :
    equationSize (front ++ rest) = equationSize front + equationSize rest := by
  simp [equationSize]

private theorem equationVars_append (front rest : List (Shape × Shape)) :
    equationVars (front ++ rest) = equationVars front ∪ equationVars rest := by
  induction front with
  | nil => simp [equationVars]
  | cons e front ih =>
    simp only [List.cons_append, equationVars, List.foldr_cons] at ih ⊢
    rw [ih]; simp only [Finset.union_assoc]

/-- Robinson's unification algorithm. The result `θ` is a most general unifier: every unifier
`δ` of the equations factors through it, as `(θ i).subst δ = δ i` for every `i`. -/
def unify (equations : List (Shape × Shape)) : Option (Nat → Shape) :=
  match equations with
  | [] => some .var
  | (.var i, t) :: rest | (t, .var i) :: rest =>
    if t = .var i then unify rest
    else if i ∈ t.vars then none
    else
      (unify (substEquations (.single i t) rest)).map fun θ j => (Shape.single i t j).subst θ
  | (s, t) :: rest =>
    match _h : s.children t with
    | some children => unify (children ++ rest)
    | none => none
termination_by ((equationVars equations).card, equationSize equations)
decreasing_by
  all_goals first
    | -- `t = var i`: the equation disappears
      exact Prod.Lex.right' _ (Finset.card_le_card Finset.subset_union_right)
        (by simp only [equationSize_cons, Shape.size]; omega)
    | -- elimination of `i`
      exact Prod.Lex.left _ _ (eliminate_decreases ‹_› (by simp [equationVars, Shape.vars])
        (fun k => by simp only [equationVars, List.foldr_cons, Finset.mem_union]; tauto))
    | -- decomposition
      obtain ⟨vars, size⟩ := Shape.children_vars _h
      refine Prod.Lex.right' _ (le_of_eq ?_) ?_
      · rw [equationVars_append, vars]; rfl
      · rw [equationSize_append, equationSize_cons]; exact Nat.add_lt_add_right size _

end Determinize.Frontend
