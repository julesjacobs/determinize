import Determinize.Spec.Traces.Semantics
import Mathlib.MeasureTheory.Integral.Bochner.Basic
import Mathlib.Probability.Moments.Variance

namespace Determinize.Spec.Traces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper

/-- Erasing terminating traces recovers the ordinary output semantics. -/
def correspondenceThm : Prop :=
  ∀ program : Expr, (traceAndOutputLaw program).map Prod.snd = bigStepMeasure program

/-- The law of a program's terminating generation traces: the trace marginal of its joint law. -/
noncomputable def traceLaw (program : Expr) : Measure Trace :=
  (traceAndOutputLaw program).map Prod.fst

/-- Draw a trace from `traces`, then an output from `outputs trace`: the joint law of the pair. -/
noncomputable def traceThenOutput (traces : Measure Trace) (outputs : Trace → Measure ℝ) :
    Measure Output :=
  traces.bind fun trace => (outputs trace).map fun value => (trace, value)

/-- The mean of the source replay law at a fixed generation trace. -/
noncomputable def replayMean (program : Expr) (trace : Trace) : ℝ :=
  ∫ value : ℝ, value ∂outputGivenTrace program trace

/-- Trace soundness. Conditioned on its G draws, the determinized program returns
the conditional mean of the source: the joint trace/output law of the source is its trace law
followed by `outputGivenTrace`, the joint law of the target is the same trace law followed by
the target's `outputGivenTrace`, and for almost every trace the source's output law given the
trace has a finite mean and the target's output law given the trace is the Dirac mass at that
mean. No global integrability assumption is required. -/
def soundnessThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) → program.sourceForm = true →
    DoesNotGetStuck program →
      DoesNotGetStuck program.determinize ∧
      traceAndOutputLaw program = traceThenOutput (traceLaw program) (outputGivenTrace program) ∧
      traceAndOutputLaw program.determinize =
        traceThenOutput (traceLaw program) (outputGivenTrace program.determinize) ∧
      ∀ᵐ trace ∂traceLaw program,
        Integrable id (outputGivenTrace program trace) ∧
        outputGivenTrace program.determinize trace =
          Measure.dirac (replayMean program trace)

/-- The law of total variance along traces. When the source output law has a finite second
moment, the variances of the source's output laws given the traces are integrable over the
trace law and the source output variance is the target output variance plus their mean: trace
by trace, determinization discards exactly the variance of the output given the trace. The
output laws are unnormalized, but they have the same mass and, by `soundnessThm`, the same mean,
so the identity holds for Mathlib's `variance` (`∫ (v - ∫ v)²`) without normalization; dividing
both output laws and the trace law by their common mass gives the same identity for the laws
conditioned on termination. -/
def varianceThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) → program.sourceForm = true →
    DoesNotGetStuck program →
    MemLp id 2 (bigStepMeasure program) →
      Integrable (fun trace => variance id (outputGivenTrace program trace)) (traceLaw program) ∧
      variance id (bigStepMeasure program) =
        variance id (bigStepMeasure program.determinize) +
          ∫ trace, variance id (outputGivenTrace program trace) ∂traceLaw program

end Determinize.Spec.Traces
