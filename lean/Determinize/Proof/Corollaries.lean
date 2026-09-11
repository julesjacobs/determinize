import Determinize.Proof.Soundness
import Mathlib.Analysis.Convex.Continuous
import Mathlib.Analysis.Convex.Integral
import Mathlib.Data.EReal.Operations

/-!
# Corollaries of trace soundness

Jensen's inequality between the source and target output laws, preservation of expectations
in the extended reals, preservation of the output mass and hence of the expectation conditioned
on acceptance, and the law of total variance along traces with its consequence that
determinization does not increase the variance. All follow from a `TraceFactorization`
alone: trace by trace, the target output is the mean of the source output fiber.
-/

namespace Determinize.Proof.Traces

open MeasureTheory ProbabilityTheory Determinize.Statement Determinize.Statement.Paper
open Determinize.Traces
open scoped ENNReal ProbabilityTheory

/-- The source output law under a trace factorization: the mixture of the fibers over the
trace law. -/
theorem TraceFactorization.source_law {source target : Expr} {fiber : Kernel Trace ℝ}
    {output : Trace → ℝ} (factor : TraceFactorization source target fiber output) :
    bigStepMeasure source = fiber ∘ₘ traceLaw source := by
  obtain ⟨markov, -, sourceJoint, -, -⟩ := factor
  have := markov
  rw [← Proof.Traces.correspondence source, sourceJoint]
  exact Measure.snd_compProd (traceLaw source) fiber

/-- The target output law under a trace factorization: the pushforward of the trace law along
`output`. -/
theorem TraceFactorization.target_law {source target : Expr} {fiber : Kernel Trace ℝ}
    {output : Trace → ℝ} (factor : TraceFactorization source target fiber output) :
    bigStepMeasure target = (traceLaw source).map output := by
  obtain ⟨-, measurableOutput, -, targetJoint, -⟩ := factor
  rw [← Proof.Traces.correspondence target, targetJoint,
    Measure.map_map measurable_snd (show Measurable (fun trace => (trace, output trace)) from
      measurable_id.prodMk measurableOutput)]
  rfl

/-- The two output laws behind `MeanOnTraces`: the source output is the mixture of the fibers
over the trace law and the target output is the pushforward of the trace law along `output`. -/
theorem MeanOnTraces.output_laws {source target : Expr} (sound : MeanOnTraces source target) :
    ∃ fiber : Kernel Trace ℝ,
      IsMarkovKernel fiber ∧ Measurable (kernelMean fiber) ∧
      bigStepMeasure source = fiber ∘ₘ traceLaw source ∧
      bigStepMeasure target = (traceLaw source).map (kernelMean fiber) ∧
      ∀ᵐ trace ∂traceLaw source,
        Integrable id (fiber trace) ∧ kernelMean fiber trace = ∫ value : ℝ, value ∂fiber trace := by
  obtain ⟨fiber, factor⟩ := sound
  exact ⟨fiber, factor.1, factor.2.1, factor.source_law,
    factor.target_law, factor.2.2.2.2⟩

/-- Under a trace factorization the two output laws have the same mass, the mass of the trace
law: every fiber is a probability measure. -/
theorem TraceFactorization.output_mass {source target : Expr} {fiber : Kernel Trace ℝ}
    {output : Trace → ℝ} (factor : TraceFactorization source target fiber output) :
    bigStepMeasure target Set.univ = bigStepMeasure source Set.univ := by
  have := factor.1
  rw [factor.source_law, factor.target_law, Measure.map_apply factor.2.1 MeasurableSet.univ,
    Set.preimage_univ, Measure.bind_apply MeasurableSet.univ fiber.measurable.aemeasurable]
  simp [measure_univ]

/-- Output mass is preserved along the traces. -/
theorem MeanOnTraces.output_mass {source target : Expr} (sound : MeanOnTraces source target) :
    bigStepMeasure target Set.univ = bigStepMeasure source Set.univ := by
  obtain ⟨fiber, factor⟩ := sound
  exact factor.output_mass

/-- The expectations conditioned on acceptance agree: the unnormalized mean and the output
mass are both preserved, so their quotient is. -/
theorem MeanOnTraces.conditional_expectation {source target : Expr}
    (sound : MeanOnTraces source target) (integrable : Integrable id (bigStepMeasure source)) :
    (∫ value : ℝ, value ∂bigStepMeasure target) / (bigStepMeasure target Set.univ).toReal =
      (∫ value : ℝ, value ∂bigStepMeasure source) / (bigStepMeasure source Set.univ).toReal := by
  rw [(sound.finite_expectation integrable).2, sound.output_mass]

/-- Jensen's inequality along the traces. -/
theorem MeanOnTraces.lintegral_convex_le {source target : Expr}
    (sound : MeanOnTraces source target) {φ : ℝ → ℝ} (convex : ConvexOn ℝ Set.univ φ)
    (nonneg : ∀ value, 0 ≤ φ value) :
    ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure target ≤
      ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure source := by
  obtain ⟨fiber, markov, measurableOutput, sourceEq, targetEq, valid⟩ :=
    sound.output_laws
  let ν := traceLaw source
  let output := kernelMean fiber
  change Measurable output at measurableOutput
  change bigStepMeasure source = fiber ∘ₘ ν at sourceEq
  change bigStepMeasure target = ν.map output at targetEq
  change ∀ᵐ trace ∂ν, Integrable id (fiber trace) ∧
    output trace = ∫ value : ℝ, value ∂fiber trace at valid
  have := markov
  have continuous : Continuous φ := continuousOn_univ.1 (convex.continuousOn isOpen_univ)
  have measurableφ : Measurable fun value => ENNReal.ofReal (φ value) :=
    continuous.measurable.ennreal_ofReal
  rw [sourceEq, targetEq, lintegral_map measurableφ measurableOutput,
    Measure.lintegral_bind fiber.measurable.aemeasurable measurableφ.aemeasurable]
  refine lintegral_mono_ae ?_
  filter_upwards [valid] with trace good
  by_cases infinite : ∫⁻ value, ENNReal.ofReal (φ value) ∂fiber trace = ⊤
  · rw [infinite]
    exact le_top
  · have integrable : Integrable φ (fiber trace) :=
      ⟨continuous.aestronglyMeasurable,
        (hasFiniteIntegral_iff_ofReal (Filter.Eventually.of_forall nonneg)).2
          (lt_top_iff_ne_top.2 infinite)⟩
    rw [good.2, ← ofReal_integral_eq_lintegral_ofReal integrable
      (Filter.Eventually.of_forall nonneg)]
    apply ENNReal.ofReal_le_ofReal
    have jensen := convex.map_integral_le (μ := fiber trace) (f := id) continuous.continuousOn
      isClosed_univ (Filter.Eventually.of_forall fun _ => Set.mem_univ _) good.1 integrable
    simpa using jensen

/-- `∫⁻ (a - b)⁺ ≤ ∫⁻ a⁺` when `b` is nonnegative. -/
theorem lintegral_ofReal_sub_le {α : Type*} [MeasurableSpace α] (ν : Measure α) (a b : α → ℝ)
    (hb0 : ∀ x, 0 ≤ b x) :
    ∫⁻ x, ENNReal.ofReal (a x - b x) ∂ν ≤ ∫⁻ x, ENNReal.ofReal (a x) ∂ν :=
  lintegral_mono fun x => ENNReal.ofReal_le_ofReal (by linarith [hb0 x])

/-- `∫⁻ a⁺ ≤ ∫⁻ (a - b)⁺ + ∫⁻ b⁺`. -/
theorem lintegral_ofReal_le_sub_add {α : Type*} [MeasurableSpace α] (ν : Measure α)
    {a b : α → ℝ} (ha : Measurable a) (hb : Measurable b) :
    ∫⁻ x, ENNReal.ofReal (a x) ∂ν ≤
      ∫⁻ x, ENNReal.ofReal (a x - b x) ∂ν + ∫⁻ x, ENNReal.ofReal (b x) ∂ν := by
  have hab : Measurable fun x => ENNReal.ofReal (a x - b x) := (ha.sub hb).ennreal_ofReal
  rw [← lintegral_add_left hab]
  refine lintegral_mono fun x => ?_
  calc ENNReal.ofReal (a x) = ENNReal.ofReal ((a x - b x) + b x) := by rw [sub_add_cancel]
    _ ≤ ENNReal.ofReal (a x - b x) + ENNReal.ofReal (b x) := ENNReal.ofReal_add_le

/-- For nonnegative `a`, `b`, the extended-real difference `∫⁻ a - ∫⁻ b` equals
`∫⁻ (a - b)⁺ - ∫⁻ (a - b)⁻` as soon as one of the two integrals is finite. -/
theorem ereal_sub_lintegral_eq {α : Type*} [MeasurableSpace α] (ν : Measure α) {a b : α → ℝ}
    (ha : Measurable a) (hb : Measurable b) (ha0 : ∀ x, 0 ≤ a x) (hb0 : ∀ x, 0 ≤ b x)
    (finite : ∫⁻ x, ENNReal.ofReal (a x) ∂ν ≠ ⊤ ∨ ∫⁻ x, ENNReal.ofReal (b x) ∂ν ≠ ⊤) :
    ((∫⁻ x, ENNReal.ofReal (a x) ∂ν : ℝ≥0∞) : EReal) -
        (∫⁻ x, ENNReal.ofReal (b x) ∂ν : ℝ≥0∞) =
      ((∫⁻ x, ENNReal.ofReal (a x - b x) ∂ν : ℝ≥0∞) : EReal) -
        (∫⁻ x, ENNReal.ofReal (b x - a x) ∂ν : ℝ≥0∞) := by
  set A := ∫⁻ x, ENNReal.ofReal (a x) ∂ν
  set B := ∫⁻ x, ENNReal.ofReal (b x) ∂ν
  set C := ∫⁻ x, ENNReal.ofReal (a x - b x) ∂ν
  set D := ∫⁻ x, ENNReal.ofReal (b x - a x) ∂ν
  have CA : C ≤ A := lintegral_ofReal_sub_le ν a b hb0
  have DB : D ≤ B := lintegral_ofReal_sub_le ν b a ha0
  have ACB : A ≤ C + B := lintegral_ofReal_le_sub_add ν ha hb
  have BDA : B ≤ D + A := lintegral_ofReal_le_sub_add ν hb ha
  have coe_ne_top {x : ℝ≥0∞} (hx : x ≠ ⊤) : (x : EReal) ≠ ⊤ :=
    fun h => hx (EReal.coe_ennreal_eq_top_iff.1 h)
  by_cases hAtop : A = ⊤
  · have hBne : B ≠ ⊤ := finite.resolve_left (not_not.2 hAtop)
    have hCtop : C = ⊤ := by
      rw [hAtop] at ACB
      rcases ENNReal.add_eq_top.1 (top_le_iff.1 ACB) with h | h
      · exact h
      · exact absurd h hBne
    have hDne : D ≠ ⊤ := ne_top_of_le_ne_top hBne DB
    rw [hAtop, hCtop, EReal.coe_ennreal_top, EReal.top_sub (coe_ne_top hBne),
      EReal.top_sub (coe_ne_top hDne)]
  · by_cases hBtop : B = ⊤
    · have hDtop : D = ⊤ := by
        rw [hBtop] at BDA
        rcases ENNReal.add_eq_top.1 (top_le_iff.1 BDA) with h | h
        · exact h
        · exact absurd h hAtop
      rw [hBtop, hDtop, EReal.coe_ennreal_top, EReal.sub_top, EReal.sub_top]
    · have hCne : C ≠ ⊤ := ne_top_of_le_ne_top hAtop CA
      have hDne : D ≠ ⊤ := ne_top_of_le_ne_top hBtop DB
      have ia : Integrable a ν :=
        ⟨ha.aestronglyMeasurable,
          (hasFiniteIntegral_iff_ofReal (Filter.Eventually.of_forall ha0)).2
            (lt_top_iff_ne_top.2 hAtop)⟩
      have ib : Integrable b ν :=
        ⟨hb.aestronglyMeasurable,
          (hasFiniteIntegral_iff_ofReal (Filter.Eventually.of_forall hb0)).2
            (lt_top_iff_ne_top.2 hBtop)⟩
      have iab : Integrable (fun x => a x - b x) ν := ia.sub ib
      have real : A.toReal - B.toReal = C.toReal - D.toReal := by
        have hAr : A.toReal = ∫ x, a x ∂ν :=
          (integral_eq_lintegral_of_nonneg_ae (Filter.Eventually.of_forall ha0)
            ha.aestronglyMeasurable).symm
        have hBr : B.toReal = ∫ x, b x ∂ν :=
          (integral_eq_lintegral_of_nonneg_ae (Filter.Eventually.of_forall hb0)
            hb.aestronglyMeasurable).symm
        have hsub := integral_eq_lintegral_pos_part_sub_lintegral_neg_part iab
        rw [integral_sub ia ib] at hsub
        simp only [neg_sub] at hsub
        rw [hAr, hBr, hsub]
      rw [← EReal.coe_ennreal_toReal hAtop, ← EReal.coe_ennreal_toReal hBtop,
        ← EReal.coe_ennreal_toReal hCne, ← EReal.coe_ennreal_toReal hDne,
        ← EReal.coe_sub, ← EReal.coe_sub, real]

/-- Extended-real expectations are preserved along the traces. -/
theorem MeanOnTraces.extended_expectation {source target : Expr}
    (sound : MeanOnTraces source target) (defined : HasExpectation (bigStepMeasure source)) :
    HasExpectation (bigStepMeasure target) ∧
      extendedExpectation (bigStepMeasure source) =
        extendedExpectation (bigStepMeasure target) := by
  obtain ⟨fiber, markov, measurableOutput, sourceEq, targetEq, valid⟩ :=
    sound.output_laws
  let ν := traceLaw source
  let output := kernelMean fiber
  change Measurable output at measurableOutput
  change bigStepMeasure source = fiber ∘ₘ ν at sourceEq
  change bigStepMeasure target = ν.map output at targetEq
  change ∀ᵐ trace ∂ν, Integrable id (fiber trace) ∧
    output trace = ∫ value : ℝ, value ∂fiber trace at valid
  have := markov
  -- the positive and negative parts of the fibers, as real functions of the trace
  let a : Trace → ℝ := fun trace => (∫⁻ value, ENNReal.ofReal value ∂fiber trace).toReal
  let b : Trace → ℝ := fun trace => (∫⁻ value, ENNReal.ofReal (-value) ∂fiber trace).toReal
  have ha : Measurable a :=
    ((Measure.measurable_lintegral ENNReal.measurable_ofReal).comp fiber.measurable).ennreal_toReal
  have hb : Measurable b :=
    ((Measure.measurable_lintegral measurable_neg.ennreal_ofReal).comp
      fiber.measurable).ennreal_toReal
  have ha0 : ∀ trace, 0 ≤ a trace := fun _ => ENNReal.toReal_nonneg
  have hb0 : ∀ trace, 0 ≤ b trace := fun _ => ENNReal.toReal_nonneg
  have good : ∀ᵐ trace ∂ν,
      ∫⁻ value, ENNReal.ofReal value ∂fiber trace = ENNReal.ofReal (a trace) ∧
      ∫⁻ value, ENNReal.ofReal (-value) ∂fiber trace = ENNReal.ofReal (b trace) ∧
      output trace = a trace - b trace := by
    filter_upwards [valid] with trace hgood
    have finiteNorm : ∫⁻ value, ‖value‖ₑ ∂fiber trace ≠ ⊤ :=
      (hasFiniteIntegral_iff_enorm.1 hgood.1.hasFiniteIntegral).ne
    have posNe : ∫⁻ value, ENNReal.ofReal value ∂fiber trace ≠ ⊤ :=
      ne_top_of_le_ne_top finiteNorm (lintegral_ofReal_le_lintegral_enorm id)
    have negNe : ∫⁻ value, ENNReal.ofReal (-value) ∂fiber trace ≠ ⊤ := by
      have h := lintegral_ofReal_le_lintegral_enorm (μ := fiber trace) fun value : ℝ => -value
      simp only [enorm_neg] at h
      exact ne_top_of_le_ne_top finiteNorm h
    refine ⟨(ENNReal.ofReal_toReal posNe).symm, (ENNReal.ofReal_toReal negNe).symm, ?_⟩
    have h := integral_eq_lintegral_pos_part_sub_lintegral_neg_part hgood.1
    simp only [id_eq] at h
    rw [hgood.2]
    exact h
  have posSource : posPartIntegral (bigStepMeasure source) =
      ∫⁻ trace, ENNReal.ofReal (a trace) ∂ν := by
    rw [posPartIntegral, sourceEq, Measure.lintegral_bind fiber.measurable.aemeasurable
      ENNReal.measurable_ofReal.aemeasurable]
    exact lintegral_congr_ae (good.mono fun trace h => h.1)
  have negSource : negPartIntegral (bigStepMeasure source) =
      ∫⁻ trace, ENNReal.ofReal (b trace) ∂ν := by
    rw [negPartIntegral, sourceEq, Measure.lintegral_bind fiber.measurable.aemeasurable
      measurable_neg.ennreal_ofReal.aemeasurable]
    exact lintegral_congr_ae (good.mono fun trace h => h.2.1)
  have posTarget : posPartIntegral (bigStepMeasure target) =
      ∫⁻ trace, ENNReal.ofReal (a trace - b trace) ∂ν := by
    rw [posPartIntegral, targetEq, lintegral_map ENNReal.measurable_ofReal measurableOutput]
    exact lintegral_congr_ae (good.mono fun trace h => by simp only [h.2.2])
  have negTarget : negPartIntegral (bigStepMeasure target) =
      ∫⁻ trace, ENNReal.ofReal (b trace - a trace) ∂ν := by
    rw [negPartIntegral, targetEq, lintegral_map measurable_neg.ennreal_ofReal measurableOutput]
    exact lintegral_congr_ae (good.mono fun trace h => by simp only [h.2.2, neg_sub])
  rw [HasExpectation, posSource, negSource] at defined
  refine ⟨?_, ?_⟩
  · rw [HasExpectation, posTarget, negTarget]
    rcases defined with h | h
    · exact Or.inl (ne_top_of_le_ne_top h (lintegral_ofReal_sub_le ν a b hb0))
    · exact Or.inr (ne_top_of_le_ne_top h (lintegral_ofReal_sub_le ν b a ha0))
  · unfold extendedExpectation
    rw [posSource, negSource, posTarget, negTarget]
    exact ereal_sub_lintegral_eq ν ha hb ha0 hb0 defined

/-- The second moment about `m` of a real law with a finite second moment, through its first
two moments and its mass. -/
theorem integral_sub_sq_eq_moments {μ : Measure ℝ} [IsFiniteMeasure μ] (memLp : MemLp id 2 μ)
    (m : ℝ) :
    ∫ value, (value - m) ^ 2 ∂μ =
      ∫ value, value ^ 2 ∂μ - 2 * m * ∫ value, value ∂μ + μ.real Set.univ * m ^ 2 := by
  have sq : Integrable (fun value : ℝ => value ^ 2) μ := memLp.integrable_sq
  have lin : Integrable (fun value : ℝ => value) μ := memLp.integrable one_le_two
  have scaled : Integrable (fun value : ℝ => 2 * m * value) μ := lin.const_mul _
  have shifted : Integrable (fun value : ℝ => value ^ 2 - 2 * m * value) μ := sq.sub scaled
  have expand : ∀ value : ℝ, (value - m) ^ 2 = (value ^ 2 - 2 * m * value) + m ^ 2 :=
    fun value => by ring
  simp only [expand]
  rw [integral_add shifted (integrable_const _), integral_sub sq scaled, integral_const_mul,
    integral_const, smul_eq_mul]

/-- Mathlib's `variance` of a real law with a finite second moment, through its first two
moments and its mass; the law need not be normalized. -/
theorem variance_id_eq_moments {μ : Measure ℝ} [IsFiniteMeasure μ] (memLp : MemLp id 2 μ) :
    variance id μ = ∫ value, value ^ 2 ∂μ - 2 * (∫ value, value ∂μ) ^ 2 +
      μ.real Set.univ * (∫ value, value ∂μ) ^ 2 := by
  rw [variance_eq_integral aemeasurable_id]
  change ∫ value, (value - ∫ value, value ∂μ) ^ 2 ∂μ = _
  rw [integral_sub_sq_eq_moments memLp]
  ring

/-- The law of total variance along a trace factorization, through the second moments: the
target output law has a finite second moment, the fiber variances are integrable over the
trace law, and the source second moment is the target second moment plus the mean fiber
variance. -/
theorem TraceFactorization.second_moment {source target : Expr} {fiber : Kernel Trace ℝ}
    {output : Trace → ℝ} (factor : TraceFactorization source target fiber output)
    (memLp : MemLp id 2 (bigStepMeasure source)) :
    MemLp id 2 (bigStepMeasure target) ∧
      Integrable (fun trace => variance id (fiber trace)) (traceLaw source) ∧
      ∫ value, value ^ 2 ∂bigStepMeasure source =
        ∫ value, value ^ 2 ∂bigStepMeasure target +
          ∫ trace, variance id (fiber trace) ∂traceLaw source := by
  have sourceEq := factor.source_law
  have targetEq := factor.target_law
  obtain ⟨markov, measurableOutput, -, -, valid⟩ := factor
  have := markov
  have sqMeasurable : Measurable fun value : ℝ => value ^ 2 := measurable_id.pow_const 2
  -- the source second moment is the mixture of the fiber second moments
  have sqSource : Integrable (fun value : ℝ => value ^ 2) (fiber ∘ₘ traceLaw source) := by
    rw [← sourceEq]
    exact memLp.integrable_sq
  have fiberSq : ∀ᵐ trace ∂traceLaw source,
      Integrable (fun value : ℝ => value ^ 2) (fiber trace) :=
    Measure.ae_integrable_of_integrable_comp sqSource
  have secondMoment :
      Integrable (fun trace => ∫ value, value ^ 2 ∂fiber trace) (traceLaw source) := by
    simpa only [norm_pow, Real.norm_eq_abs, sq_abs] using
      Measure.integrable_integral_norm_of_integrable_comp sqSource
  have sourceSecond : ∫ value, value ^ 2 ∂bigStepMeasure source =
      ∫ trace, ∫ value, value ^ 2 ∂fiber trace ∂traceLaw source := by
    have h := sqSource
    rw [Measure.comp_eq_comp_const_apply] at h
    rw [sourceEq, Measure.comp_eq_comp_const_apply, Kernel.integral_comp h]
    simp only [Kernel.const_apply]
  -- each fiber is a probability measure whose mean is the target output
  have fiberVariance : ∀ᵐ trace ∂traceLaw source,
      variance id (fiber trace) = ∫ value, value ^ 2 ∂fiber trace - output trace ^ 2 := by
    filter_upwards [valid, fiberSq] with trace good sq
    have memLpFiber : MemLp id 2 (fiber trace) :=
      (memLp_two_iff_integrable_sq aestronglyMeasurable_id).2 sq
    rw [variance_eq_sub memLpFiber, good.2]
    rfl
  -- the squared target output is dominated by the fiber second moment
  have outputSq : Integrable (fun trace => output trace ^ 2) (traceLaw source) := by
    refine secondMoment.mono' (measurableOutput.pow_const 2).aestronglyMeasurable ?_
    filter_upwards [fiberVariance] with trace h
    rw [Real.norm_eq_abs, abs_of_nonneg (sq_nonneg _)]
    linarith [variance_nonneg id (fiber trace)]
  have memLpTarget : MemLp id 2 (bigStepMeasure target) := by
    rw [targetEq, memLp_map_measure_iff aestronglyMeasurable_id measurableOutput.aemeasurable]
    exact (memLp_two_iff_integrable_sq measurableOutput.aestronglyMeasurable).2 outputSq
  have targetSecond : ∫ value, value ^ 2 ∂bigStepMeasure target =
      ∫ trace, output trace ^ 2 ∂traceLaw source := by
    rw [targetEq, integral_map measurableOutput.aemeasurable sqMeasurable.aestronglyMeasurable]
  refine ⟨memLpTarget, ?_, ?_⟩
  · have sub : Integrable (fun trace => ∫ value, value ^ 2 ∂fiber trace - output trace ^ 2)
        (traceLaw source) := secondMoment.sub outputSq
    exact sub.congr (fiberVariance.mono fun trace h => h.symm)
  · rw [sourceSecond, targetSecond, integral_congr_ae fiberVariance,
      integral_sub secondMoment outputSq]
    ring

/-- The law of total variance along a trace factorization: the fiber variances are integrable
over the trace law and the source output variance is the target output variance plus the mean
fiber variance. Both output laws have the same mass and the same mean, so Mathlib's `variance`
needs no normalization. -/
theorem TraceFactorization.variance_decomposition {source target : Expr}
    {fiber : Kernel Trace ℝ} {output : Trace → ℝ}
    (factor : TraceFactorization source target fiber output)
    (memLp : MemLp id 2 (bigStepMeasure source)) :
    Integrable (fun trace => variance id (fiber trace)) (traceLaw source) ∧
      variance id (bigStepMeasure source) =
        variance id (bigStepMeasure target) +
          ∫ trace, variance id (fiber trace) ∂traceLaw source := by
  have := factor.1
  obtain ⟨memLpTarget, integrable, second⟩ := factor.second_moment memLp
  refine ⟨integrable, ?_⟩
  have finiteSource : IsFiniteMeasure (bigStepMeasure source) := by
    rw [factor.source_law]
    infer_instance
  have finiteTarget : IsFiniteMeasure (bigStepMeasure target) := by
    rw [factor.target_law]
    infer_instance
  obtain ⟨-, mean⟩ :=
    MeanOnTraces.finite_expectation ⟨fiber, factor.canonical⟩ (memLp.integrable one_le_two)
  have mass : (bigStepMeasure target).real Set.univ = (bigStepMeasure source).real Set.univ := by
    rw [measureReal_def, measureReal_def, factor.output_mass]
  rw [variance_id_eq_moments memLp, variance_id_eq_moments memLpTarget, second, ← mean, mass]
  ring

/-- Determinization does not increase the second moment or the variance of the output law. -/
theorem MeanOnTraces.variance_le {source target : Expr} (sound : MeanOnTraces source target)
    (memLp : MemLp id 2 (bigStepMeasure source)) :
    MemLp id 2 (bigStepMeasure target) ∧
      (∫ value, value ^ 2 ∂bigStepMeasure target) ≤ ∫ value, value ^ 2 ∂bigStepMeasure source ∧
      variance id (bigStepMeasure target) ≤ variance id (bigStepMeasure source) := by
  obtain ⟨fiber, factor⟩ := sound
  obtain ⟨memLpTarget, -, second⟩ := factor.second_moment memLp
  obtain ⟨-, decomposition⟩ := factor.variance_decomposition memLp
  have nonneg : 0 ≤ ∫ trace, variance id (fiber trace) ∂traceLaw source :=
    integral_nonneg fun trace => variance_nonneg id (fiber trace)
  exact ⟨memLpTarget, by linarith, by linarith⟩

end Determinize.Proof.Traces

namespace Determinize.Proof.Paper

/-- Extended-real expectation preservation, from operational trace soundness. -/
theorem extendedExpectationSoundness : Determinize.Statement.extendedExpectationThm := by
  intro program typed sourceForm sourceSafe defined
  exact (Determinize.Proof.Traces.meanOnTraces .E program typed sourceForm
    sourceSafe).2.extended_expectation defined

/-- Jensen's inequality between the two output laws, from operational trace soundness. -/
theorem jensenSoundness : Determinize.Statement.jensenThm := by
  intro program typed sourceForm sourceSafe φ convex nonneg
  exact (Determinize.Proof.Traces.meanOnTraces .E program typed sourceForm
    sourceSafe).2.lintegral_convex_le convex nonneg

/-- Output mass preservation, from operational trace soundness. -/
theorem outputMassSoundness : Determinize.Statement.outputMassThm := by
  intro program typed sourceForm sourceSafe
  exact (Determinize.Proof.Traces.meanOnTraces .E program typed sourceForm
    sourceSafe).2.output_mass

/-- Variance non-increase, from operational trace soundness. -/
theorem varianceSoundness : Determinize.Statement.varianceThm := by
  intro program typed sourceForm sourceSafe memLp
  exact (Determinize.Proof.Traces.meanOnTraces .E program typed sourceForm
    sourceSafe).2.variance_le memLp

/-- Preservation of the expectation conditioned on acceptance, from operational trace
soundness. -/
theorem conditionalExpectationSoundness : Determinize.Statement.conditionalExpectationThm := by
  intro program typed sourceForm sourceSafe integrable
  exact (Determinize.Proof.Traces.meanOnTraces .E program typed sourceForm
    sourceSafe).2.conditional_expectation integrable

end Determinize.Proof.Paper

namespace Determinize.Proof.Traces

open MeasureTheory ProbabilityTheory Determinize.Traces

/-- The law of total variance along traces, with `Traces.outputGivenTrace` as the fiber. -/
theorem varianceSoundness : Determinize.Traces.varianceThm := by
  intro program typed sourceForm sourceSafe memLp
  obtain ⟨_, factor, massAe, _⟩ := soundnessData .E program typed sourceForm sourceSafe
  obtain ⟨integrable, decomposition⟩ := factor.variance_decomposition memLp
  have congr : (fun trace => variance id (StepTraces.normalizedOutputGivenTrace program trace))
      =ᵐ[traceLaw program] fun trace => variance id (outputGivenTrace program trace) :=
    massAe.mono fun trace h => by simp only [h]
  exact ⟨integrable.congr congr, by rw [decomposition, integral_congr_ae congr]⟩

end Determinize.Proof.Traces
