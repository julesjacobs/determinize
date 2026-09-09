import Determinize.Proof.CompactSoundness
import Determinize.Statement.Main

namespace Determinize.Traces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper
open scoped ProbabilityTheory

/-- Integrating the trace fibers preserves finite expectation. -/
theorem MeanOnTraces.finite_expectation {source target : Expr} (sound : MeanOnTraces source target)
    (integrable : Integrable id (bigStepMeasure source)) :
    Integrable id (bigStepMeasure target) ∧
      (∫ value : ℝ, value ∂bigStepMeasure source) = ∫ value : ℝ, value ∂bigStepMeasure target := by
  rcases sound with ⟨fiber, output, markov, measurableOutput, sourceJoint, targetJoint, valid⟩
  let := markov
  let ν := traceLaw source
  have sourceEq : bigStepMeasure source = fiber ∘ₘ ν := by
    rw [← Proof.Traces.correspondence source, sourceJoint]
    exact Measure.snd_compProd ν fiber
  have targetEq : bigStepMeasure target = ν.map output := by
    rw [← Proof.Traces.correspondence target, targetJoint,
      Measure.map_map measurable_snd (show Measurable (fun trace => (trace, output trace)) from
        measurable_id.prodMk measurableOutput)]
    rfl
  rw [sourceEq] at integrable ⊢
  rw [targetEq]
  have normIntegrable := Measure.integrable_integral_norm_of_integrable_comp integrable
  have outputIntegrable : Integrable output ν := by
    apply normIntegrable.mono' measurableOutput.aestronglyMeasurable
    filter_upwards [valid] with trace good
    rw [good.2]
    exact norm_integral_le_integral_norm _
  refine ⟨(integrable_map_measure aestronglyMeasurable_id measurableOutput.aemeasurable).2 outputIntegrable, ?_⟩
  change (∫ value : ℝ, id value ∂fiber ∘ₘ ν) = ∫ value : ℝ, id value ∂ν.map output
  rw [integral_map measurableOutput.aemeasurable aestronglyMeasurable_id]
  rw [Measure.comp_eq_comp_const_apply] at integrable ⊢
  rw [Kernel.integral_comp integrable]
  simp only [Kernel.const_apply]
  exact integral_congr_ae (valid.mono fun _ good => good.2.symm)

end Determinize.Traces

namespace Determinize.Proof.Paper

/-- The public expectation theorem follows from operational trace soundness. -/
theorem finiteExpectationSoundness : Determinize.Statement.mainThm := by
  intro mode program typed sourceForm sourceSafe sourceIntegrable
  rcases Determinize.Proof.Traces.soundness mode program typed sourceForm sourceSafe with ⟨targetSafe, traces⟩
  rcases traces.finite_expectation sourceIntegrable with ⟨targetIntegrable, expectation⟩
  exact ⟨targetSafe, targetIntegrable, expectation⟩

end Determinize.Proof.Paper
