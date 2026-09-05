import Determinize.Traces.Semantics
import Mathlib.Probability.Kernel.Composition.MeasureCompProd
import Mathlib.MeasureTheory.Integral.Bochner.Basic

namespace Determinize.Traces

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper

/-- Erasing terminating traces recovers the ordinary output semantics. -/
def correspondenceThm : Prop :=
  ∀ program : Expr, (jointMeasure program).map Prod.snd = bigStepMeasure program

/-- The actual source and target joint laws factor over the same operational traces. -/
def MeanOnTraces (source target : Expr) : Prop :=
  ∃ (traces : Measure Trace) (fiber : Kernel Trace ℝ) (output : Trace → ℝ),
    traces Set.univ ≤ 1 ∧
    IsMarkovKernel fiber ∧
    Measurable output ∧
    jointMeasure source = traces ⊗ₘ fiber ∧
    jointMeasure target = traces.map (fun trace => (trace, output trace)) ∧
    ∀ᵐ trace ∂traces,
      Integrable id (fiber trace) ∧ output trace = ∫ value : ℝ, value ∂fiber trace

/-- Trace soundness requires no global integrability assumption. -/
def soundnessThm : Prop :=
  ∀ (mode : Mode) (program : Expr),
    Typed [] program (.float mode) → program.sourceForm = true →
    let source := observeFloat mode program
    DoesNotGetStuck source →
      DoesNotGetStuck source.determinize ∧ MeanOnTraces source source.determinize

end Determinize.Traces
