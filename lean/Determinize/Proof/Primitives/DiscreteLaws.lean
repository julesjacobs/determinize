import Determinize.Proof.Primitives.Laws
import Determinize.Proof.Primitives.FiniteDistributionMeasure
import Mathlib.MeasureTheory.Measure.GiryMonad

namespace Determinize.Proof.DiscreteLaws
open Spec.Paper MeasureTheory

theorem bernoulli_off_domain (kind : DistributionAction) (p : ℝ) (h : ¬ (0 ≤ p ∧ p ≤ 1)) :
    bernoulliFiber kind p = 0 := by simp [bernoulliFiber, h]

theorem bernoulli_measurable (kind : DistributionAction) : Measurable (bernoulliFiber kind) := by
  unfold bernoulliFiber
  apply Measurable.ite
    ((measurableSet_le measurable_const measurable_id).inter
      (measurableSet_le measurable_id measurable_const))
  · cases kind with
    | sample affinity =>
        exact (((measurable_const.sub measurable_id).ennreal_ofReal).smul_measure _).add
          (measurable_id.ennreal_ofReal.smul_measure _)
    | mean => exact Measure.measurable_dirac
  · exact measurable_const

theorem bernoulli_integrable (kind : DistributionAction) (p : ℝ) (f : ℝ → ℝ) :
    Integrable f (bernoulliFiber kind p) := by
  unfold bernoulliFiber
  split
  · cases kind with
    | sample affinity =>
        exact ((integrable_dirac (by simp)).smul_measure (by simp)).add_measure
          ((integrable_dirac (by simp)).smul_measure (by simp))
    | mean => exact integrable_dirac (by simp)
  · simp

theorem bernoulli_probability (kind : DistributionAction) (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    IsProbabilityMeasure (bernoulliFiber kind p) := by
  constructor
  cases kind with
  | sample affinity =>
      simp only [bernoulliFiber, if_pos h, Measure.add_apply, Measure.smul_apply,
        Measure.dirac_apply_of_mem (Set.mem_univ _), smul_eq_mul, mul_one]
      rw [← ENNReal.ofReal_add (sub_nonneg.mpr h.2) h.1]
      simp
  | mean => simp [bernoulliFiber, h]

theorem bernoulli_integral {affinity : Affinity} (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) (f : ℝ → ℝ) :
    (∫ x, f x ∂bernoulliFiber (.sample affinity) p) = (1 - p) * f 0 + p * f 1 := by
  rw [bernoulliFiber, if_pos h]
  rw [integral_add_measure
    ((integrable_dirac (by simp)).smul_measure (by simp))
    ((integrable_dirac (by simp)).smul_measure (by simp))]
  simp [ENNReal.toReal_ofReal (sub_nonneg.mpr h.2), ENNReal.toReal_ofReal h.1]

theorem bernoulli_mean (kind : DistributionAction) (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    (∫ x, x ∂bernoulliFiber kind p) = p := by
  cases kind with
  | sample affinity => simp [bernoulli_integral p h]
  | mean => simp [bernoulliFiber, h]

theorem bernoulli_variance {affinity : Affinity} (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    (∫ x, (x - p)^2 ∂bernoulliFiber (.sample affinity) p) = p * (1 - p) := by
  rw [bernoulli_integral p h]
  ring

theorem bernoulli_zero (kind : DistributionAction) : bernoulliFiber kind 0 = Measure.dirac 0 := by
  cases kind <;> simp [bernoulliFiber]

theorem bernoulli_one (kind : DistributionAction) : bernoulliFiber kind 1 = Measure.dirac 1 := by
  cases kind <;> simp [bernoulliFiber]

/-- The unguarded finite law, used only to prove facts about the public fiber. -/
noncomputable def remainderMeasure (n : Nat) (p : Fin n → ℝ) : Measure ℝ :=
  (∑ i : Fin n, ENNReal.ofReal (p i) • Measure.dirac ((i : ℕ) : ℝ)) +
    ENNReal.ofReal (1 - ∑ i, p i) • Measure.dirac (n : ℝ)

theorem remainder_integrable (n : Nat) (p : Fin n → ℝ) (f : ℝ → ℝ) :
    Integrable f (remainderMeasure n p) := by
  exact (integrable_finsetSum_measure.mpr fun _ _ =>
    (integrable_dirac (by simp)).smul_measure (by simp)).add_measure
      ((integrable_dirac (by simp)).smul_measure (by simp))

theorem remainder_mass (n : Nat) (p : Fin n → ℝ)
    (valid : (∀ i, 0 ≤ p i) ∧ ∑ i, p i ≤ 1) :
    remainderMeasure n p Set.univ = 1 := by
  simp only [remainderMeasure, Measure.add_apply, Measure.finsetSum_apply,
    Measure.smul_apply, smul_eq_mul, measure_univ, mul_one]
  rw [← ENNReal.ofReal_sum_of_nonneg (fun i _ => valid.1 i),
    ← ENNReal.ofReal_add (Finset.sum_nonneg fun i _ => valid.1 i) (sub_nonneg.mpr valid.2)]
  simp

theorem remainder_integral (n : Nat) (p : Fin n → ℝ)
    (valid : (∀ i, 0 ≤ p i) ∧ ∑ i, p i ≤ 1) (f : ℝ → ℝ) :
    (∫ x, f x ∂remainderMeasure n p) =
      (∑ i : Fin n, p i * f (i : ℕ)) + (1 - ∑ i, p i) * f n := by
  rw [remainderMeasure, integral_add_measure
    (integrable_finsetSum_measure.mpr fun _ _ =>
      (integrable_dirac (by simp)).smul_measure (by simp))
    ((integrable_dirac (by simp)).smul_measure (by simp)),
    integral_finsetSum_measure (fun _ _ =>
      (integrable_dirac (by simp)).smul_measure (by simp))]
  simp [integral_smul_measure, ENNReal.toReal_ofReal (valid.1 _),
    ENNReal.toReal_ofReal (sub_nonneg.mpr valid.2)]

theorem remainder_mean (n : Nat) (p : Fin n → ℝ)
    (valid : (∀ i, 0 ≤ p i) ∧ ∑ i, p i ≤ 1) :
    (∫ x, x ∂remainderMeasure n p) =
      (n : ℝ) + ∑ i : Fin n, (((i : ℕ) : ℝ) - n) * p i := by
  rw [remainder_integral n p valid]
  simp only [sub_mul, Finset.sum_sub_distrib, ← Finset.mul_sum]
  rw [show (∑ i : Fin n, p i * (i : ℕ)) = ∑ i : Fin n, (i : ℕ) * p i by
    apply Finset.sum_congr rfl; intros; ring]
  ring

theorem discrete_probability (action : DistributionAction) (p : List ℝ)
    (valid : (∀ i : Fin p.length, 0 ≤ p[i]) ∧ ∑ i : Fin p.length, p[i] ≤ 1) :
    IsProbabilityMeasure (discreteFiber action p) := by
  constructor
  cases action with
  | sample affinity =>
      rw [discreteFiber, if_pos valid]
      exact remainder_mass _ _ valid
  | mean => rw [discreteFiber, if_pos valid]; simp

theorem discrete_integrable (action : DistributionAction) (p : List ℝ) (f : ℝ → ℝ) :
    Integrable f (discreteFiber action p) := by
  unfold discreteFiber
  split
  · cases action with
    | sample affinity => exact remainder_integrable _ _ f
    | mean => exact integrable_dirac (by simp)
  · simp

theorem discrete_mean (action : DistributionAction) (p : List ℝ)
    (valid : (∀ i : Fin p.length, 0 ≤ p[i]) ∧ ∑ i : Fin p.length, p[i] ≤ 1) :
    (∫ x, x ∂discreteFiber action p) =
      (p.length : ℝ) + ∑ i : Fin p.length, (((i : ℕ) : ℝ) - p.length) * p[i] := by
  cases action with
  | sample affinity =>
      rw [discreteFiber, if_pos valid]
      exact remainder_mean p.length (fun i => p[i]) valid
  | mean => rw [discreteFiber, if_pos valid]; simp

end Determinize.Proof.DiscreteLaws
