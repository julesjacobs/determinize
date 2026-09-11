import Determinize.Statement.FiniteDistributionMeasure
import Mathlib.Tactic

namespace Determinize.Proof.FiniteDistribution
open Statement.Paper MeasureTheory

private theorem list_integrable (weights : List Rat) (value : Nat → ℝ) (index : Nat)
    (f : ℝ → ℝ) :
    Integrable f ((weights.zipIdx index).map fun entry : Rat × Nat =>
      ENNReal.ofReal (entry.1 : ℝ) • Measure.dirac (value entry.2)).sum := by
  induction weights generalizing index with
  | nil => simp
  | cons p ps ih =>
      simp only [List.zipIdx_cons, List.map_cons, List.sum_cons]
      exact ((integrable_dirac (by simp)).smul_measure (by simp)).add_measure (ih _)

theorem integrable (d : Statement.Paper.FiniteDistribution) (value : Nat → ℝ) (f : ℝ → ℝ) :
    Integrable f (d.measure value) := list_integrable _ _ _ _

private theorem list_mass (weights : List Rat) (value : Nat → ℝ) (index : Nat)
    (nonnegative : ∀ p ∈ weights, 0 ≤ p) :
    (((weights.zipIdx index).map fun entry : Rat × Nat =>
      ENNReal.ofReal (entry.1 : ℝ) • Measure.dirac (value entry.2)).sum) Set.univ =
      ENNReal.ofReal (weights.sum : ℝ) := by
  induction weights generalizing index with
  | nil => simp
  | cons p ps ih =>
      have hp : 0 ≤ (p : ℝ) := by exact_mod_cast nonnegative p (by simp)
      have hps : ∀ q ∈ ps, 0 ≤ q := fun q hq => nonnegative q (by simp [hq])
      have hs : 0 ≤ (ps.sum : ℝ) := by exact_mod_cast List.sum_nonneg hps
      simp only [List.zipIdx_cons, List.map_cons, List.sum_cons,
        Measure.add_apply, Measure.smul_apply, Measure.dirac_apply_of_mem (Set.mem_univ _),
        smul_eq_mul, mul_one]
      rw [ih _ hps, Rat.cast_add, ENNReal.ofReal_add hp hs]

instance probability (d : Statement.Paper.FiniteDistribution) (value : Nat → ℝ) :
    IsProbabilityMeasure (d.measure value) where
  measure_univ := by
    rw [Statement.Paper.FiniteDistribution.measure, list_mass _ _ _ d.nonnegative, d.total]
    simp

private theorem list_integral (weights : List Rat) (value : Nat → ℝ) (index : Nat)
    (nonnegative : ∀ p ∈ weights, 0 ≤ p) (f : ℝ → ℝ) :
    (∫ x, f x ∂((weights.zipIdx index).map fun entry : Rat × Nat =>
      ENNReal.ofReal (entry.1 : ℝ) • Measure.dirac (value entry.2)).sum) =
        ((weights.zipIdx index).map fun entry : Rat × Nat => (entry.1 : ℝ) * f (value entry.2)).sum := by
  induction weights generalizing index with
  | nil => simp
  | cons p ps ih =>
      have hp : 0 ≤ (p : ℝ) := by exact_mod_cast nonnegative p (by simp)
      have hps : ∀ q ∈ ps, 0 ≤ q := fun q hq => nonnegative q (by simp [hq])
      simp only [List.zipIdx_cons, List.map_cons, List.sum_cons]
      rw [integral_add_measure
        ((integrable_dirac (by simp)).smul_measure (by simp)) (list_integrable _ _ _ _)]
      rw [ih _ hps]
      simp [ENNReal.toReal_ofReal hp]

theorem integral (d : Statement.Paper.FiniteDistribution) (value : Nat → ℝ) (f : ℝ → ℝ) :
    (∫ x, f x ∂d.measure value) =
      (d.probabilities.zipIdx.map fun entry : Rat × Nat => (entry.1 : ℝ) * f (value entry.2)).sum :=
  list_integral _ _ _ d.nonnegative _

theorem expectation (d : Statement.Paper.FiniteDistribution) (value : Nat → Rat) :
    (∫ x, x ∂d.measure (fun i => (value i : ℝ))) = (d.expectation value : ℝ) := by
  rw [integral]
  simp [Statement.Paper.FiniteDistribution.expectation, List.map_map, Function.comp_def]

theorem mean (d : Statement.Paper.FiniteDistribution) :
    (∫ x, x ∂d.measure (fun i => (i : ℝ))) = (d.mean : ℝ) := by
  simpa [Statement.Paper.FiniteDistribution.mean] using expectation d (fun i => (i : Rat))

end Determinize.Proof.FiniteDistribution
