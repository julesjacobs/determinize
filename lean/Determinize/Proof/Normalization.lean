import Determinize.Proof.Corollaries
import Determinize.Proof.Semantics.Termination

namespace Determinize.Proof
open MeasureTheory ProbabilityTheory Spec Spec.Paper
open scoped ENNReal

theorem normalized_probability {α : Type*} [MeasurableSpace α] (μ : Measure α)
    [IsFiniteMeasure μ] (positive : μ Set.univ ≠ 0) :
    IsProbabilityMeasure ((μ Set.univ)⁻¹ • μ) := by
  constructor
  simp [Measure.smul_apply, ENNReal.inv_mul_cancel positive (measure_ne_top μ Set.univ)]

theorem normalized_variance (μ : Measure ℝ) [IsFiniteMeasure μ]
    (positive : μ Set.univ ≠ 0) (moment : MemLp id 2 μ) :
    variance id ((μ Set.univ)⁻¹ • μ) =
      (∫ x, x ^ 2 ∂μ) / (μ Set.univ).toReal -
        ((∫ x, x ∂μ) / (μ Set.univ).toReal) ^ 2 := by
  have := normalized_probability μ positive
  rw [variance_eq_sub (moment.smul_measure (ENNReal.inv_ne_top.mpr positive))]
  simp only [integral_smul_measure, ENNReal.toReal_inv, smul_eq_mul, Pi.pow_apply, id_eq]
  simp only [div_eq_mul_inv, mul_comm]

local instance (program : Expr) : IsFiniteMeasure (Spec.Paper.bigStepMeasure program) := by
  rw [← Traces.correspondence program]
  infer_instance

namespace Paper

theorem returnedExpectationSoundness (program : Expr) (typed : Typed [] program (.float .E))
    (safe : DomainSafe program) (positive : 0 < Spec.Paper.bigStepMeasure program Set.univ)
    (integrable : Integrable id (Spec.Paper.bigStepMeasure program)) :
    0 < Spec.Paper.bigStepMeasure program.determinize Set.univ ∧
      IsProbabilityMeasure (returnedLaw program) ∧
      IsProbabilityMeasure (returnedLaw program.determinize) ∧
      Integrable id (returnedLaw program) ∧
      Integrable id (returnedLaw program.determinize) ∧
      (∫ x, x ∂returnedLaw program.determinize) = ∫ x, x ∂returnedLaw program := by
  have positive := positive.ne'
  have mass := outputMassSoundness program typed safe
  have targetPositive : Spec.Paper.bigStepMeasure program.determinize Set.univ ≠ 0 := by
    rwa [mass]
  obtain ⟨_, targetIntegrable, mean⟩ := finiteExpectationSoundness program typed safe integrable
  refine ⟨pos_iff_ne_zero.mpr targetPositive, normalized_probability _ positive,
    normalized_probability _ targetPositive,
    integrable.smul_measure (ENNReal.inv_ne_top.mpr positive),
    targetIntegrable.smul_measure (ENNReal.inv_ne_top.mpr targetPositive), ?_⟩
  simp only [returnedLaw, returnProbability, integral_smul_measure, mass, ← mean]

theorem conditionalVarianceSoundness (program : Expr) (typed : Typed [] program (.float .E))
    (safe : DomainSafe program) (positive : 0 < Spec.Paper.bigStepMeasure program Set.univ)
    (moment : MemLp id 2 (Spec.Paper.bigStepMeasure program)) :
    0 < Spec.Paper.bigStepMeasure program.determinize Set.univ ∧
      MemLp id 2 (Spec.Paper.bigStepMeasure program.determinize) ∧
      variance id (returnedLaw program.determinize) ≤ variance id (returnedLaw program) := by
  have positive := positive.ne'
  have mass := outputMassSoundness program typed safe
  have targetPositive : Spec.Paper.bigStepMeasure program.determinize Set.univ ≠ 0 := by
    rwa [mass]
  obtain ⟨targetMoment, second, -⟩ := varianceSoundness program typed safe moment
  obtain ⟨-, -, mean⟩ := finiteExpectationSoundness program typed safe
    (moment.integrable one_le_two)
  refine ⟨pos_iff_ne_zero.mpr targetPositive, targetMoment, ?_⟩
  unfold returnedLaw returnProbability
  rw [normalized_variance _ targetPositive targetMoment,
    normalized_variance _ positive moment, mass, ← mean]
  exact sub_le_sub_right (div_le_div_of_nonneg_right second ENNReal.toReal_nonneg) _

theorem conditionalExtendedExpectationSoundness (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : 0 < Spec.Paper.bigStepMeasure program Set.univ)
    (defined : HasExpectation (Spec.Paper.bigStepMeasure program)) :
    0 < Spec.Paper.bigStepMeasure program.determinize Set.univ ∧
      HasExpectation (Spec.Paper.bigStepMeasure program.determinize) ∧
      ((Spec.Paper.bigStepMeasure program.determinize Set.univ).toReal⁻¹ : EReal) *
          extendedExpectation (Spec.Paper.bigStepMeasure program.determinize) =
        ((Spec.Paper.bigStepMeasure program Set.univ).toReal⁻¹ : EReal) *
          extendedExpectation (Spec.Paper.bigStepMeasure program) := by
  have mass := outputMassSoundness program typed safe
  obtain ⟨targetDefined, expectation⟩ := extendedExpectationSoundness program typed safe defined
  exact ⟨by rwa [mass], targetDefined, by rw [mass, ← expectation]⟩

/-- Probability preservation: the target is domain-safe, the source returns or diverges, and
both return with the same probability. -/
theorem probabilityPreservation (program : Expr) (typed : Typed [] program (.float .E))
    (safe : DomainSafe program) :
    DomainSafe program.determinize ∧
      Spec.Paper.bigStepMeasure program Set.univ + divergenceProbability program = 1 ∧
      Spec.Paper.bigStepMeasure program.determinize Set.univ =
        Spec.Paper.bigStepMeasure program Set.univ :=
  ⟨(Traces.meanOnTraces .E program typed safe).1, returnOrDiverge .E program typed safe,
    outputMassSoundness program typed safe⟩

end Paper

namespace Traces

theorem conditionalVarianceSoundness (program : Expr) (typed : Typed [] program (.float .E))
    (safe : DomainSafe program) (positive : 0 < Spec.Paper.bigStepMeasure program Set.univ)
    (moment : MemLp id 2 (Spec.Paper.bigStepMeasure program)) :
    Integrable
        (fun trace => variance id ((Spec.Traces.traceAndOutputLaw program).condKernel trace))
        ((Spec.Paper.bigStepMeasure program Set.univ)⁻¹ • Spec.Traces.traceLaw program) ∧
      variance id (returnedLaw program) =
        variance id (returnedLaw program.determinize) +
          ∫ trace, variance id ((Spec.Traces.traceAndOutputLaw program).condKernel trace)
            ∂((Spec.Paper.bigStepMeasure program Set.univ)⁻¹ • Spec.Traces.traceLaw program) := by
  have positive := positive.ne'
  have mass := Paper.outputMassSoundness program typed safe
  have targetPositive : Spec.Paper.bigStepMeasure program.determinize Set.univ ≠ 0 := by rwa [mass]
  obtain ⟨targetMoment, -, -⟩ := Paper.varianceSoundness program typed safe moment
  obtain ⟨-, -, mean⟩ := Paper.finiteExpectationSoundness program typed safe
    (moment.integrable one_le_two)
  obtain ⟨integrable, decomposition⟩ := varianceSoundness program typed safe moment
  refine ⟨integrable.smul_measure (ENNReal.inv_ne_top.mpr positive), ?_⟩
  rw [variance_id_eq_moments moment, variance_id_eq_moments targetMoment,
    ← mean, measureReal_def, measureReal_def, mass] at decomposition
  unfold returnedLaw returnProbability
  rw [normalized_variance _ positive moment,
    normalized_variance _ targetPositive targetMoment, mass, ← mean,
    integral_smul_measure, ENNReal.toReal_inv, smul_eq_mul]
  have second : (∫ x : ℝ, x ^ 2 ∂Spec.Paper.bigStepMeasure program) =
      (∫ x : ℝ, x ^ 2 ∂Spec.Paper.bigStepMeasure program.determinize) +
        ∫ trace, variance id ((Spec.Traces.traceAndOutputLaw program).condKernel trace)
          ∂Spec.Traces.traceLaw program := by linarith
  rw [second]
  ring

end Traces

end Determinize.Proof
