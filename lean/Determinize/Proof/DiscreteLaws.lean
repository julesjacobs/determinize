import Determinize.Spec.Primitives
import Determinize.Proof.FiniteDistributionMeasure
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

instance discrete_probability (kind : DistributionAction) (d : Spec.Paper.FiniteDistribution) :
    IsProbabilityMeasure (discreteFiber kind d) := by
  cases kind <;> unfold discreteFiber <;> infer_instance

theorem discrete_integrable (kind : DistributionAction) (d : Spec.Paper.FiniteDistribution) (f : ℝ → ℝ) :
    Integrable f (discreteFiber kind d) := by
  cases kind with
  | sample affinity => exact FiniteDistribution.integrable d _ f
  | mean => exact integrable_dirac (by simp)

theorem discrete_mean (kind : DistributionAction) (d : Spec.Paper.FiniteDistribution) :
    (∫ x, x ∂discreteFiber kind d) = (d.mean : ℝ) := by
  cases kind with
  | sample affinity => exact FiniteDistribution.mean d
  | mean => simp [discreteFiber]

end Determinize.Proof.DiscreteLaws
