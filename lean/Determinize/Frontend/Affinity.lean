import Determinize.Spec.Types
import Mathlib.Data.Finset.Card

/-!
# Affinity constraints

After shape unification and decomposition, inference is left with atomic constraints `a ≤ b`
between affinities in the order `G ≤ E`. Each side is a variable or an affinity fixed by the
program. A variable must be G in every solution if it lies below a G, directly or through other
such variables; `solveAffinities` collects these variables, sets them to G and every other
variable to E, and checks the result. `Proof/Frontend/Affinity.lean` proves that it returns the
greatest solution, and fails exactly when there is none.
-/

namespace Determinize.Frontend
open Spec.Paper

/-- One side of an affinity constraint. -/
inductive AffinityTerm (V : Type) where
  | var (name : V)
  | fixed (affinity : Affinity)
deriving DecidableEq, Repr

def AffinityTerm.eval {V : Type} (ρ : V → Affinity) : AffinityTerm V → Affinity
  | var v => ρ v
  | fixed m => m

/-- `lower ≤ upper` in the order `G ≤ E`. -/
abbrev AffinityConstraint (V : Type) := AffinityTerm V × AffinityTerm V

/-- `G ≤ E`, the order of `Ty.Sub.general`. -/
def affinityLE : Affinity → Affinity → Bool
  | .E, .G => false
  | _, _ => true

variable {V : Type} [DecidableEq V]

/-- The variables on the lower side of some constraint. -/
def lowerVars (constraints : List (AffinityConstraint V)) : Finset V :=
  (constraints.filterMap fun c => match c.1 with
    | .var v => some v
    | .fixed _ => none).toFinset

/-- The term is G, or a variable in `general`. -/
def AffinityTerm.isGeneral (general : Finset V) : AffinityTerm V → Bool
  | var v => v ∈ general
  | fixed m => m == .G

/-- The variables `v` with a constraint `v ≤ t` where `t` is G or a variable in `general`. -/
def below (constraints : List (AffinityConstraint V)) (general : Finset V) : Finset V :=
  (constraints.filterMap fun c => match c.1 with
    | .var v => if c.2.isGeneral general then some v else none
    | .fixed _ => none).toFinset

theorem below_subset_lowerVars (constraints : List (AffinityConstraint V)) (general : Finset V) :
    below constraints general ⊆ lowerVars constraints := by
  intro v hv
  simp only [below, lowerVars, List.mem_toFinset, List.mem_filterMap] at hv ⊢
  obtain ⟨c, mem, hc⟩ := hv
  refine ⟨c, mem, ?_⟩
  cases h : c.1 <;> simp_all

/-- Close `general` under `below`. Started from `∅`, this gives the variables that every
solution sets to G. -/
def forcedGeneral (constraints : List (AffinityConstraint V)) (general : Finset V) : Finset V :=
  if below constraints general ⊆ general then general
  else forcedGeneral constraints (general ∪ below constraints general)
termination_by (lowerVars constraints \ general).card
decreasing_by
  obtain ⟨v, hv, notMem⟩ := Finset.not_subset.1 ‹_›
  refine Finset.card_lt_card (Finset.ssubset_iff_of_subset ?_ |>.2 ⟨v, ?_, ?_⟩)
  · exact Finset.sdiff_subset_sdiff (le_refl _) Finset.subset_union_left
  · exact Finset.mem_sdiff.2 ⟨below_subset_lowerVars _ _ hv, notMem⟩
  · simp [hv]

/-- The greatest solution: G on the variables that every solution sets to G, E elsewhere; or
`none` if that assignment, and hence every assignment, violates a constraint. -/
def solveAffinities (constraints : List (AffinityConstraint V)) : Option (V → Affinity) :=
  let general := forcedGeneral constraints ∅
  let ρ := fun v => if v ∈ general then Affinity.G else .E
  if constraints.all fun c => affinityLE (c.1.eval ρ) (c.2.eval ρ) then some ρ else none

end Determinize.Frontend
