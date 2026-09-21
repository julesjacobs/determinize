import Determinize.Proof.FiniteModel.Boundary
import Determinize.Proof.FiniteModel.Paths

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel

private def CanStop (model : Model) : Nat → Fin model.size → Prop
  | 0, i => model.kind i ≠ .transient
  | n+1, i => ∃ j, 0 < model.transition i j ∧ CanStop model n j

/-- Every finite model admits a closed divergence boundary and descending paths
from the remaining transient states to a terminal boundary. -/
theorem boundary_paths_exist (model : Model) :
    ∃ dead : Fin model.size → Bool, ClosedDivergence model dead ∧
      ∃ paths : Paths (cut model dead), paths.Valid (cut model dead) := by
  classical
  let reachable := fun i => ∃ n, CanStop model n i
  let dead := fun i => decide (¬ reachable i)
  let rank := fun i => if h : reachable i then Nat.find h else 0
  have terminal (i) (h : model.kind i ≠ .transient) : reachable i := ⟨0, h⟩
  have closed : ClosedDivergence model dead := by
    intro i hi
    have unreachable : ¬ reachable i := of_decide_eq_true hi
    refine ⟨by_contra fun h => unreachable (terminal i h), ?_⟩
    intro j positive
    apply decide_eq_true
    rintro ⟨n, hn⟩
    exact unreachable ⟨n+1, j, positive, hn⟩
  have descend (i) (hi : (cut model dead).kind i = .transient) :
      ∃ j, 0 < model.transition i j ∧ rank j < rank i := by
    have alive : dead i = false := by
      cases h : dead i
      · rfl
      · simp [h] at hi
    have reachable_i : reachable i := by simpa [dead] using alive
    have transient : model.kind i = .transient := by simpa [cut, alive] using hi
    have witness := Nat.find_spec reachable_i
    have positive : 0 < Nat.find reachable_i := by
      by_contra h
      have zero : Nat.find reachable_i = 0 := by omega
      rw [zero] at witness
      exact witness transient
    obtain ⟨n, hn⟩ := Nat.exists_eq_succ_of_ne_zero (Nat.ne_of_gt positive)
    rw [hn] at witness
    obtain ⟨j, edge, rest⟩ := witness
    have reachable_j : reachable j := ⟨n, rest⟩
    refine ⟨j, edge, ?_⟩
    simp only [rank, dif_pos reachable_i, dif_pos reachable_j]
    have minimal := Nat.find_min' reachable_j rest
    omega
  let next := fun i => if h : (cut model dead).kind i = .transient then
    Classical.choose (descend i h) else i
  refine ⟨dead, closed, ⟨rank, next⟩, ?_⟩
  intro i hi
  change 0 < model.transition i (next i) ∧ rank (next i) < rank i
  simpa only [next, dif_pos hi] using Classical.choose_spec (descend i hi)

end Determinize.Proof.FiniteModel
