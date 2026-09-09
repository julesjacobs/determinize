import Determinize.Proof.Soundness
import Mathlib.Analysis.Convex.Continuous
import Mathlib.Analysis.Convex.Integral
import Mathlib.Data.EReal.Operations

/-!
# Corollaries of trace soundness

Jensen's inequality between the source and target output laws, and preservation of
expectations in the extended reals. Both follow from `Traces.MeanOnTraces` alone: trace by
trace, the target output is the mean of the source output fiber.
-/

namespace Determinize.Traces

open MeasureTheory ProbabilityTheory Determinize.Statement Determinize.Statement.Paper
open scoped ENNReal ProbabilityTheory

/-- The two output laws behind `MeanOnTraces`: the source output is the mixture of the fibers
over the trace law and the target output is the pushforward of the trace law along `output`. -/
theorem MeanOnTraces.output_laws {source target : Expr} (sound : MeanOnTraces source target) :
    ∃ (traces : Measure Trace) (fiber : Kernel Trace ℝ) (output : Trace → ℝ),
      IsFiniteMeasure traces ∧ IsMarkovKernel fiber ∧ Measurable output ∧
      bigStepMeasure source = fiber ∘ₘ traces ∧
      bigStepMeasure target = traces.map output ∧
      ∀ᵐ trace ∂traces,
        Integrable id (fiber trace) ∧ output trace = ∫ value : ℝ, value ∂fiber trace := by
  rcases sound with
    ⟨fiber, output, markov, measurableOutput, sourceJoint, targetJoint, valid⟩
  have := markov
  refine ⟨traceLaw source, fiber, output, inferInstance, markov, measurableOutput, ?_, ?_, valid⟩
  · rw [← Proof.Traces.correspondence source, sourceJoint]
    exact Measure.snd_compProd (traceLaw source) fiber
  · rw [← Proof.Traces.correspondence target, targetJoint,
      Measure.map_map measurable_snd (show Measurable (fun trace => (trace, output trace)) from
        measurable_id.prodMk measurableOutput)]
    rfl

/-- Jensen's inequality along the traces. -/
theorem MeanOnTraces.lintegral_convex_le {source target : Expr}
    (sound : MeanOnTraces source target) {φ : ℝ → ℝ} (convex : ConvexOn ℝ Set.univ φ)
    (nonneg : ∀ value, 0 ≤ φ value) :
    ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure target ≤
      ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure source := by
  obtain ⟨ν, fiber, output, -, markov, measurableOutput, sourceEq, targetEq, valid⟩ :=
    sound.output_laws
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
  obtain ⟨ν, fiber, output, -, markov, measurableOutput, sourceEq, targetEq, valid⟩ :=
    sound.output_laws
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

end Determinize.Traces

namespace Determinize.Proof.Paper

/-- Extended-real expectation preservation, from operational trace soundness. -/
theorem extendedExpectationSoundness : Determinize.Statement.extendedExpectationThm := by
  intro mode program typed sourceForm sourceSafe defined
  exact (Determinize.Proof.Traces.soundness mode program typed sourceForm
    sourceSafe).2.extended_expectation defined

/-- Jensen's inequality between the two output laws, from operational trace soundness. -/
theorem jensenSoundness : Determinize.Statement.jensenThm := by
  intro mode program typed sourceForm sourceSafe φ convex nonneg
  exact (Determinize.Proof.Traces.soundness mode program typed sourceForm
    sourceSafe).2.lintegral_convex_le convex nonneg

end Determinize.Proof.Paper
