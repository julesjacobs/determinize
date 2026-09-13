import Determinize.Proof.FiniteModel.Replay

namespace Determinize.Finite.Builder
open Spec.FiniteModel

def sparseEdges {n : Nat} (weight : Fin n → Rat) : Array Edge :=
  (((List.finRange n).filter fun j => decide (0 < weight j)).map fun j => ⟨j.val, weight j⟩).toArray

theorem sparseEdges_weight {n : Nat} (weight : Fin n → Rat) (nonnegative : ∀ j, 0 ≤ weight j)
    (j : Fin n) :
    ((sparseEdges weight).toList.map fun edge => if edge.target = j.val then edge.probability else 0).sum =
      weight j := by
  simp only [sparseEdges, List.toList_toArray, List.map_map]
  rw [← List.sum_toFinset _ ((List.nodup_finRange n).filter _), List.toFinset_filter]
  simp only [List.toFinset_finRange, decide_eq_true_eq]
  change (∑ k ∈ Finset.univ.filter (fun k => 0 < weight k),
    if k.val = j.val then weight k else 0) = weight j
  simp only [← Fin.ext_iff, Finset.sum_ite_eq', Finset.mem_filter, Finset.mem_univ, true_and]
  split
  · rfl
  · rename_i h
    exact (le_antisymm (le_of_not_gt h) (nonnegative j)).symm

theorem sparseEdges_valid {n : Nat} (weight : Fin n → Rat) :
    ((sparseEdges weight).toList.map Edge.target).Nodup ∧
      ∀ edge ∈ (sparseEdges weight).toList, edge.target < n ∧ 0 < edge.probability := by
  constructor
  · simp only [sparseEdges, List.toList_toArray, List.map_map]
    exact ((List.nodup_finRange n).filter _).map Fin.val_injective
  · intro edge member
    simp only [sparseEdges, List.toList_toArray, List.mem_map, List.mem_filter,
      List.mem_finRange, true_and, decide_eq_true_eq] at member
    obtain ⟨j, positive, rfl⟩ := member
    exact ⟨j.isLt, positive⟩

theorem sum_list_comm {n : Nat} (outcomes : List (Rat × State)) (f : Fin n → Rat × State → Rat) :
    (∑ j, (outcomes.map (f j)).sum) = (outcomes.map fun out => ∑ j, f j out).sum := by
  induction outcomes with
  | nil => simp
  | cons out rest ih => simp [Finset.sum_add_distrib, ih]

end Determinize.Finite.Builder
