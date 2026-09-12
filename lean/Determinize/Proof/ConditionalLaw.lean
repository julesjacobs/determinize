import Determinize.Proof.CompactSoundness
import Mathlib.Probability.Kernel.Disintegration.Unique

/-!
# The replay kernel as the regular conditional distribution

`Traces.outputGivenTrace` is a finite kernel, and by trace soundness the joint law of the source
is its trace law followed by the replay, the joint law of the target the same trace law followed
by the target's replay. Mathlib's disintegration kernel is almost everywhere unique among the
finite kernels with this factorization property (`eq_condKernel_of_measure_eq_compProd`), so
each replay is a version of the regular conditional distribution of the output given the trace,
and the almost-sure clause of trace soundness transfers to the conditional laws.
-/

namespace Determinize.Proof.Traces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Spec.Traces
open Determinize.Proof.Paper
open StepTraces (normalizedOutputGivenTrace outputGivenTraceKernel
  outputGivenTraceKernel_apply outputGivenTrace_mass_le_one)
open scoped ProbabilityTheory

/-- The compact replay is a finite kernel: its mass is at most one. -/
instance outputGivenTraceKernel_finite (program : Expr) :
    IsFiniteKernel (outputGivenTraceKernel program) :=
  ⟨1, ENNReal.one_lt_top, fun trace => by
    rw [outputGivenTraceKernel_apply]
    exact outputGivenTrace_mass_le_one program trace⟩

theorem outputGivenTraceKernel_coe (program : Expr) :
    ⇑(outputGivenTraceKernel program) = outputGivenTrace program :=
  funext (outputGivenTraceKernel_apply program)

/-- A joint law that is a trace law followed by the replay of `program` has that replay as its
disintegration kernel, for almost every trace. -/
theorem outputGivenTrace_ae_eq_condKernel (program : Expr) {traces : Measure Trace}
    [IsFiniteMeasure traces] {joint : Measure Output} [IsFiniteMeasure joint]
    (factor : joint = traceThenOutput traces (outputGivenTrace program))
    (marginal : joint.fst = traces) :
    outputGivenTrace program =ᵐ[traces] joint.condKernel := by
  have compProd : joint = joint.fst ⊗ₘ outputGivenTraceKernel program := by
    rw [marginal, compProd_eq_traceThenOutput, outputGivenTraceKernel_coe, factor]
  have unique := eq_condKernel_of_measure_eq_compProd (outputGivenTraceKernel program) compProd
  rw [marginal] at unique
  filter_upwards [unique] with trace h
  rw [← h, outputGivenTraceKernel_apply]

/-- The determinized program has the trace law of its source. -/
theorem traceLaw_determinize (affinity : Affinity) (program : Expr)
    (typed : Typed [] program (.float affinity)) (sourceForm : program.sourceForm = true)
    (safe : DoesNotGetStuck program) :
    traceLaw program.determinize = traceLaw program := by
  let output := kernelMean (normalizedOutputGivenTrace program)
  obtain ⟨-, ⟨-, measurableOutput, -, targetMap, -⟩, -, -⟩ :=
    soundnessData affinity program typed sourceForm safe
  have pairMeasurable : Measurable fun trace : Trace => (trace, output trace) :=
    measurable_id.prodMk measurableOutput
  calc traceLaw program.determinize
      = ((traceLaw program).map fun trace => (trace, output trace)).map Prod.fst := by
        rw [traceLaw, targetMap]
    _ = traceLaw program := by
        rw [Measure.map_map measurable_fst pairMeasurable]
        exact Measure.map_id

/-- The public conditional-law theorem: both replays are versions of the regular conditional
distributions of the outputs given the trace, and trace soundness holds for those. -/
theorem conditionalLaw : Determinize.Spec.Traces.conditionalLawThm := by
  intro program typed sourceForm safe
  obtain ⟨targetSafe, sourceFactor, targetFactor, ae⟩ :=
    replaySoundness program typed sourceForm safe
  have sameTraces := traceLaw_determinize .E program typed sourceForm
    ((Typing.primitiveDomainSafe_iff_doesNotGetStuck typed).1 safe)
  have sourceAe := outputGivenTrace_ae_eq_condKernel program sourceFactor rfl
  have targetAe := outputGivenTrace_ae_eq_condKernel program.determinize targetFactor
    sameTraces
  refine ⟨targetSafe, sameTraces, ?_⟩
  filter_upwards [ae, sourceAe, targetAe] with trace good sourceEq targetEq
  rw [← sourceEq, ← targetEq]
  exact good

end Determinize.Proof.Traces
