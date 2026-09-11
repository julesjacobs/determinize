import Determinize.Spec.Semantics
import Determinize.Proof.DiscreteLaws
import Mathlib.Tactic

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper

theorem uniformMeasure_mass_le_one (lower upper : ℝ) :
    uniformMeasure lower upper Set.univ ≤ 1 := by
  unfold uniformMeasure
  split_ifs with hDomain hPoint
  · simp
  · rw [Measure.smul_apply, Measure.restrict_apply_univ, Real.volume_Icc, smul_eq_mul,
      ENNReal.inv_mul_cancel
        (ENNReal.ofReal_pos.mpr (sub_pos.mpr (lt_of_le_of_ne hDomain hPoint))).ne'
        ENNReal.ofReal_ne_top]
  · simp

theorem uniformFiber_mass_le_one (kind : DistributionAction) (lower upper : ℝ) :
    uniformFiber kind lower upper Set.univ ≤ 1 := by
  cases kind <;> simp only [uniformFiber] <;> split_ifs
  · exact uniformMeasure_mass_le_one lower upper
  all_goals simp

theorem gaussianFiber_mass_le_one (kind : DistributionAction) (mean variance : ℝ) :
    gaussianFiber kind mean variance Set.univ ≤ 1 := by
  cases kind <;> simp only [gaussianFiber] <;> split_ifs with hDomain
  · have := instIsProbabilityMeasureGaussianReal mean ⟨variance, hDomain⟩
    exact prob_le_one
  all_goals simp

theorem poissonFiber_mass_le_one (kind : DistributionAction) (rate : ℝ) :
    poissonFiber kind rate Set.univ ≤ 1 := by
  cases kind <;> simp only [poissonFiber] <;> split_ifs with hDomain
  · rw [Measure.map_apply measurable_from_nat MeasurableSet.univ, Set.preimage_univ]
    have probability (r : NNReal) : IsProbabilityMeasure (poissonMeasure r) := inferInstance
    have := probability ⟨rate, hDomain⟩
    exact prob_le_one
  all_goals simp

theorem exponentialFiber_mass_le_one (kind : DistributionAction) (rate : ℝ) :
    exponentialFiber kind rate Set.univ ≤ 1 := by
  cases kind <;> simp only [exponentialFiber] <;> split_ifs with hDomain
  · have := isProbabilityMeasure_expMeasure hDomain
    exact prob_le_one
  all_goals simp

theorem betaFiber_mass_le_one (kind : DistributionAction) (alpha beta : ℝ) :
    betaFiber kind alpha beta Set.univ ≤ 1 := by
  cases kind <;> simp only [betaFiber] <;> split_ifs with hDomain
  · have := isProbabilityMeasureBeta hDomain.1 hDomain.2
    exact prob_le_one
  all_goals simp

theorem gammaFiber_mass_le_one (kind : DistributionAction) (shape rate : ℝ) :
    gammaFiber kind shape rate Set.univ ≤ 1 := by
  cases kind <;> simp only [gammaFiber] <;> split_ifs with hDomain
  · have := isProbabilityMeasure_gammaMeasure hDomain.1 hDomain.2
    exact prob_le_one
  all_goals simp

theorem bernoulliFiber_mass_le_one (kind : DistributionAction) (probability : ℝ) :
    bernoulliFiber kind probability Set.univ ≤ 1 := by
  cases kind <;> simp only [bernoulliFiber] <;> split_ifs with hDomain
  · simp only [Measure.add_apply, Measure.smul_apply, smul_eq_mul, measure_univ, mul_one]
    rw [← ENNReal.ofReal_add (by linarith [hDomain.2]) hDomain.1, sub_add_cancel,
      ENNReal.ofReal_one]
  all_goals simp

theorem discreteFiber_mass_le_one (action : DistributionAction) (d : FiniteDistribution) :
    discreteFiber action d Set.univ ≤ 1 := by
  exact prob_le_one

theorem Action.wrap_eq_sample {context : Expr → Expr} {action : Action}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (equality : action.wrap context = .sample site fiber continuation) :
    ∃ inner, action = .sample site fiber inner ∧ continuation = context ∘ inner := by
  cases action with
  | next expression => simp [Action.wrap] at equality
  | stuck => simp [Action.wrap] at equality
  | sample actualSite actualFiber inner =>
      simp only [Action.wrap, Action.sample.injEq] at equality
      rcases equality with ⟨rfl, rfl, continuationEq⟩
      exact ⟨inner, rfl, continuationEq.symm⟩

/-- Every fiber a reduction step draws from is one of the eight primitive fibers, so its mass
is at most one (`PrimitiveMass.lean`). -/
theorem reduce_sample_mass_le_one {expression : Expr} {site : DistributionAction × Op}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (reduction : reduce expression = .sample site fiber continuation) : fiber Set.univ ≤ 1 := by
  induction expression generalizing site fiber continuation <;>
    simp only [reduce] at reduction <;>
    (repeat split at reduction) <;>
    first
      | cases reduction
      | (obtain ⟨_, reduction, _⟩ := Action.wrap_eq_sample reduction; solve_by_elim)
  all_goals first
    | exact uniformFiber_mass_le_one _ _ _
    | exact gaussianFiber_mass_le_one _ _ _
    | exact poissonFiber_mass_le_one _ _
    | exact exponentialFiber_mass_le_one _ _
    | exact betaFiber_mass_le_one _ _ _
    | exact gammaFiber_mass_le_one _ _ _
    | exact bernoulliFiber_mass_le_one _ _
    | exact discreteFiber_mass_le_one _ _

end Determinize.Proof.Paper
