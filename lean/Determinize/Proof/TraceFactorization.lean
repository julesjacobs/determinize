import Determinize.Spec.Traces.Main
import Mathlib.Probability.Kernel.MeasurableIntegral
import Mathlib.Probability.Kernel.Composition.MeasureCompProd

/-!
# Trace factorizations

The proof-internal form of trace soundness: the source and target joint laws factor over the
source trace law through a Markov kernel and a measurable output function. The public
`Spec.Traces.soundnessThm` instantiates the kernel with `Spec.Traces.outputGivenTrace`; the corollaries
in `Proof/Corollaries.lean` are proved for an arbitrary factorization.
-/

namespace Determinize.Proof.Traces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Spec.Traces
open scoped ProbabilityTheory

/-- The canonical real output of a trace-indexed kernel. -/
noncomputable def kernelMean (fiber : Kernel Trace ℝ) (trace : Trace) : ℝ :=
  ∫ value : ℝ, value ∂fiber trace

/-- A factorization of the actual source and target joint laws over the source's own trace
law: the source output is drawn from the Markov kernel `fiber` indexed by the trace, the target
output is the measurable function `output` of the trace, and for almost every trace the fiber
is integrable and `output` is its mean. -/
def TraceFactorization (source target : Expr) (fiber : Kernel Trace ℝ) (output : Trace → ℝ) :
    Prop :=
  IsMarkovKernel fiber ∧
  Measurable output ∧
  traceAndOutputLaw source = traceLaw source ⊗ₘ fiber ∧
  traceAndOutputLaw target = (traceLaw source).map (fun trace => (trace, output trace)) ∧
  ∀ᵐ trace ∂traceLaw source,
    Integrable id (fiber trace) ∧ output trace = ∫ value : ℝ, value ∂fiber trace

/-- The output function is determined almost everywhere by the fiber. -/
theorem TraceFactorization.canonical {source target : Expr} {fiber : Kernel Trace ℝ}
    {output : Trace → ℝ} (factor : TraceFactorization source target fiber output) :
    TraceFactorization source target fiber (kernelMean fiber) := by
  obtain ⟨markov, _, sourceEq, targetEq, meanEq⟩ := factor
  have := markov
  refine ⟨markov, (stronglyMeasurable_id.integral_kernel (κ := fiber)).measurable,
    sourceEq, targetEq.trans ?_, ?_⟩
  · apply Measure.map_congr
    filter_upwards [meanEq] with trace good
    exact congrArg (fun value => (trace, value)) good.2
  · exact meanEq.mono fun _ good => ⟨good.1, rfl⟩

/-- Some trace factorization of the source and target joint laws exists. -/
def MeanOnTraces (source target : Expr) : Prop :=
  ∃ fiber : Kernel Trace ℝ, TraceFactorization source target fiber (kernelMean fiber)

end Determinize.Proof.Traces
