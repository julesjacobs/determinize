import Determinize.Spec.RewardModel.Model

namespace Determinize.Proof.RewardModel
open Spec.RewardModel

theorem controlWeight_nonnegative (model : Model) (i j : Fin model.size) :
    0 ≤ controlWeight model i j := by
  apply List.sum_nonneg
  intro x hx
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hx
  split_ifs
  · exact model.nonnegative i e he
  · rfl

private theorem sum_weights {n : Nat} (edges : List (Edge n)) :
    (∑ j : Fin n, (edges.map fun e => if e.target = j then e.probability else 0).sum) =
      (edges.map Edge.probability).sum := by
  induction edges with
  | nil => simp
  | cons e es ih =>
    simp only [List.map_cons, List.sum_cons, Finset.sum_add_distrib, ih]
    simp

theorem controlWeight_normalized (model : Model) (i : Fin model.size) :
    ∑ j, controlWeight model i j = 1 := by
  unfold controlWeight
  rw [sum_weights, model.normalized]

theorem edge_le_controlWeight (model : Model) (i : Fin model.size) (e : Edge model.size)
    (member : e ∈ model.edges i) : e.probability ≤ controlWeight model i e.target := by
  apply List.single_le_sum
  · intro x hx
    obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hx
    split_ifs
    · exact model.nonnegative i a ha
    · rfl
  · exact List.mem_map.mpr ⟨e, member, by simp⟩

theorem control_sum (model : Model) (i : Fin model.size) (v : Fin model.size → Rat) :
    (∑ j, controlWeight model i j * v j) =
      ((model.edges i).map fun e => e.probability * v e.target).sum := by
  unfold controlWeight
  generalize model.edges i = edges
  induction edges with
  | nil => simp
  | cons e es ih =>
    simp only [List.map_cons, List.sum_cons, add_mul, Finset.sum_add_distrib, ih]
    congr 1
    simp

theorem control_sum_real (model : Model) (i : Fin model.size) (v : Fin model.size → ℝ) :
    (∑ j, (controlWeight model i j : ℝ) * v j) =
      ((model.edges i).map fun e => (e.probability : ℝ) * v e.target).sum := by
  unfold controlWeight
  generalize model.edges i = edges
  induction edges with
  | nil => simp
  | cons e es ih =>
    simp only [List.map_cons, List.sum_cons, Rat.cast_add, add_mul, Finset.sum_add_distrib, ih]
    congr 1
    have cast (j : Fin model.size) : ((if e.target = j then e.probability else 0 : Rat) : ℝ) =
        if e.target = j then (e.probability : ℝ) else 0 := by split_ifs <;> simp
    simp_rw [cast]
    simp

end Determinize.Proof.RewardModel

namespace Determinize.Spec.RewardModel

abbrev Model.control (model : Model) : FiniteModel.Model where
  size := model.size
  initial := model.initial
  kind := model.kind
  transition := controlWeight model
  nonnegative := Proof.RewardModel.controlWeight_nonnegative model
  normalized := Proof.RewardModel.controlWeight_normalized model

end Determinize.Spec.RewardModel
