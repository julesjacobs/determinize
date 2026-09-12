import Determinize.Proof.CompactSoundness
import Determinize.Spec.Main

/-!
# The expectation theorem

`Spec.mainThm` (exported as `Theorems.expectationPreservation`) follows from trace
soundness. Under a `TraceFactorization`, the source output law is the mixture of the fibers
over the trace law (`TraceFactorization.source_law`) and the target output law is the
pushforward of the trace law along the fiber means (`TraceFactorization.target_law`), so
integrating the fibers preserves a finite expectation (`MeanOnTraces.finite_expectation`);
`finiteExpectationSoundness` instantiates this with the factorization that `meanOnTraces`
provides for a typed source that does not get stuck.
-/

namespace Determinize.Proof.Traces
open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Spec.Traces
open scoped ProbabilityTheory

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

/-- Integrating the trace fibers preserves finite expectation. -/
theorem MeanOnTraces.finite_expectation {source target : Expr} (sound : MeanOnTraces source target)
    (integrable : Integrable id (bigStepMeasure source)) :
    Integrable id (bigStepMeasure target) ∧
      (∫ value : ℝ, value ∂bigStepMeasure source) = ∫ value : ℝ, value ∂bigStepMeasure target := by
  obtain ⟨fiber, factor⟩ := sound
  let output := kernelMean fiber
  have sourceEq := factor.source_law
  have targetEq := factor.target_law
  obtain ⟨markov, measurableOutput, -, -, valid⟩ := factor
  have := markov
  rw [sourceEq] at integrable ⊢
  rw [targetEq]
  have normIntegrable := Measure.integrable_integral_norm_of_integrable_comp integrable
  have outputIntegrable : Integrable output (traceLaw source) := by
    apply normIntegrable.mono' measurableOutput.aestronglyMeasurable
    filter_upwards [valid] with trace good
    rw [good.2]
    exact norm_integral_le_integral_norm _
  refine ⟨(integrable_map_measure aestronglyMeasurable_id measurableOutput.aemeasurable).2
    outputIntegrable, ?_⟩
  change (∫ value : ℝ, id value ∂fiber ∘ₘ traceLaw source) =
    ∫ value : ℝ, id value ∂(traceLaw source).map output
  rw [integral_map measurableOutput.aemeasurable aestronglyMeasurable_id]
  rw [Measure.comp_eq_comp_const_apply] at integrable ⊢
  rw [Kernel.integral_comp integrable]
  simp only [Kernel.const_apply]
  exact integral_congr_ae (valid.mono fun _ good => good.2.symm)

end Determinize.Proof.Traces

namespace Determinize.Proof.Paper

/-- The public expectation theorem follows from operational trace soundness. -/
theorem finiteExpectationSoundness : Determinize.Spec.mainThm := by
  intro program typed sourceSafe sourceIntegrable
  rcases Determinize.Proof.Traces.meanOnTraces .E program typed
    ((Typing.primitiveDomainSafe_iff_doesNotGetStuck typed).1 sourceSafe) with ⟨targetSafe, traces⟩
  rcases traces.finite_expectation sourceIntegrable with ⟨targetIntegrable, expectation⟩
  exact ⟨fun fuel => Typing.doesNotGetStuckAt_imp_primitiveDomainSafeAt (targetSafe fuel),
    targetIntegrable, expectation⟩

end Determinize.Proof.Paper
