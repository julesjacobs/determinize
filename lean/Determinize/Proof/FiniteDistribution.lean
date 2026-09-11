import Determinize.Statement.FiniteDistribution
import Mathlib.Tactic

namespace Determinize.Proof.FiniteDistribution
open Statement.Paper

theorem normalized_nonnegative (weights : List Rat)
    (nonnegative : ∀ p ∈ weights, 0 ≤ p) (positive : 0 < weights.sum) :
    ∀ p ∈ weights.map (· / weights.sum), 0 ≤ p := by
  intro p hp
  obtain ⟨w, hw, rfl⟩ := List.mem_map.mp hp
  exact div_nonneg (nonnegative w hw) positive.le

theorem normalized_total (weights : List Rat) (positive : 0 < weights.sum) :
    (weights.map (· / weights.sum)).sum = 1 := by
  simp_rw [div_eq_mul_inv]
  rw [List.sum_map_mul_right]
  simp [ne_of_gt positive]

theorem expectation_normalized (weights : List Rat)
    (d : Statement.Paper.FiniteDistribution)
    (normalized : d.probabilities = weights.map (· / weights.sum)) (value : Nat → Rat) :
    d.expectation value =
      (weights.zipIdx.map fun (w, i) => w * value i).sum / weights.sum := by
  unfold Statement.Paper.FiniteDistribution.expectation
  rw [normalized]
  have aux (ws : List Rat) (index : Nat) :
      (((ws.map (· / weights.sum)).zipIdx index).map fun (p, i) => p * value i).sum =
        ((ws.zipIdx index).map fun (p, i) => p * value i).sum / weights.sum := by
    induction ws generalizing index with
    | nil => simp
    | cons w ws ih =>
        simp only [List.map_cons, List.zipIdx_cons, List.sum_cons]
        rw [ih, add_div]
        congr 1
        ring
  exact aux weights 0

end Determinize.Proof.FiniteDistribution
