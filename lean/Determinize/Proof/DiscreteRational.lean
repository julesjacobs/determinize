import Determinize.Proof.DiscreteLaws
import Determinize.Proof.FiniteDistributionMeasure

namespace Determinize.Proof.DiscreteLaws
open Spec.Paper MeasureTheory

private theorem sum_zipIdx {α β : Type*} [AddCommMonoid β] (xs : List α) (f : α × Nat → β) :
    (xs.zipIdx.map f).sum = ∑ i : Fin xs.length, f (xs[i], i) := by
  rw [← List.sum_ofFn]
  congr 1
  apply List.ext_getElem <;> simp

private theorem sum_getElem (p : List ℝ) : (∑ i : Fin p.length, p[i]) = p.sum := by
  simp

theorem completed_domain (p : List Rat) (d : Spec.Paper.FiniteDistribution)
    (completed : d.probabilities = p ++ [1 - p.sum]) :
    (∀ i : Fin (p.map (Rat.cast : Rat → ℝ)).length, 0 ≤ (p.map (Rat.cast : Rat → ℝ))[i]) ∧
      ∑ i : Fin (p.map (Rat.cast : Rat → ℝ)).length, (p.map (Rat.cast : Rat → ℝ))[i] ≤ 1 := by
  have nonnegative : ∀ x ∈ p, 0 ≤ x := by
    intro x hx
    exact d.nonnegative x (by rw [completed]; simp [hx])
  have bounded : p.sum ≤ 1 := sub_nonneg.mp (d.nonnegative (1 - p.sum) (by rw [completed]; simp))
  constructor
  · intro i
    simpa using nonnegative (p[i.val]'(by simpa using i.isLt)) (List.getElem_mem _)
  · rw [sum_getElem]
    simpa using (show (p.sum : ℝ) ≤ 1 by exact_mod_cast bounded)

theorem completed_sample (affinity : Affinity) (p : List Rat) (d : Spec.Paper.FiniteDistribution)
    (completed : d.probabilities = p ++ [1 - p.sum]) :
    discreteFiber (.sample affinity) (p.map (Rat.cast : Rat → ℝ)) = d.measure (fun i => (i : ℝ)) := by
  rw [discreteFiber, if_pos (completed_domain p d completed), sum_getElem,
    Spec.Paper.FiniteDistribution.measure, completed,
    List.zipIdx_append, List.map_append, List.sum_append]
  rw [← sum_zipIdx (p.map (Rat.cast : Rat → ℝ))
    (fun entry => ENNReal.ofReal entry.1 • Measure.dirac (entry.2 : ℝ))]
  simp [List.zipIdx_map, List.map_map, Function.comp_def]

theorem completed_mean (p : List Rat) (d : Spec.Paper.FiniteDistribution)
    (completed : d.probabilities = p ++ [1 - p.sum]) :
    discreteFiber .mean (p.map (Rat.cast : Rat → ℝ)) = Measure.dirac (d.mean : ℝ) := by
  have expectation := discrete_mean (.sample .G) (p.map (Rat.cast : Rat → ℝ)) (completed_domain p d completed)
  rw [completed_sample .G p d completed, FiniteDistribution.mean] at expectation
  rw [discreteFiber, if_pos (completed_domain p d completed), ← expectation]

end Determinize.Proof.DiscreteLaws
