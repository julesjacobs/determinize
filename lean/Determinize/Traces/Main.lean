import Determinize.Traces.Semantics
import Mathlib.Probability.Kernel.Composition.MeasureCompProd
import Mathlib.MeasureTheory.Integral.Bochner.Basic

namespace Determinize.Traces

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper

/-- Erasing terminating traces recovers the ordinary output semantics. -/
def correspondenceThm : Prop :=
  ∀ program : Expr, (jointMeasure program).map Prod.snd = bigStepMeasure program

/-- The law of a program's terminating generation traces: the trace marginal of its joint law. -/
noncomputable def traceLaw (program : Expr) : Measure Trace :=
  (jointMeasure program).map Prod.fst

/-- The actual source and target joint laws factor over the source's own trace law: the
source output is drawn from a Markov kernel indexed by the trace, the target output is a
measurable function of the trace, and that function is almost surely the fiber's mean. -/
def MeanOnTraces (source target : Expr) : Prop :=
  ∃ (fiber : Kernel Trace ℝ) (output : Trace → ℝ),
    IsMarkovKernel fiber ∧
    Measurable output ∧
    jointMeasure source = traceLaw source ⊗ₘ fiber ∧
    jointMeasure target = (traceLaw source).map (fun trace => (trace, output trace)) ∧
    ∀ᵐ trace ∂traceLaw source,
      Integrable id (fiber trace) ∧ output trace = ∫ value : ℝ, value ∂fiber trace

/-- Trace soundness requires no global integrability assumption. -/
def soundnessThm : Prop :=
  ∀ (mode : Mode) (program : Expr),
    Typed [] program (.float mode) → program.sourceForm = true →
    DoesNotGetStuck program →
      DoesNotGetStuck program.determinize ∧ MeanOnTraces program program.determinize

end Determinize.Traces
