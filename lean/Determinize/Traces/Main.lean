import Determinize.Traces.Semantics
import Mathlib.Probability.Kernel.Composition.MeasureCompProd
import Mathlib.MeasureTheory.Integral.Bochner.Basic
import Mathlib.Probability.Moments.Variance

namespace Determinize.Traces

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper

/-- Erasing terminating traces recovers the ordinary output semantics. -/
def correspondenceThm : Prop :=
  ∀ program : Expr, (jointMeasure program).map Prod.snd = bigStepMeasure program

/-- The law of a program's terminating generation traces: the trace marginal of its joint law. -/
noncomputable def traceLaw (program : Expr) : Measure Trace :=
  (jointMeasure program).map Prod.fst

/-- A factorization of the actual source and target joint laws over the source's own trace
law: the source output is drawn from the Markov kernel `fiber` indexed by the trace, the target
output is the measurable function `output` of the trace, and for almost every trace the fiber
is integrable and `output` is its mean. -/
def TraceFactorization (source target : Expr) (fiber : Kernel Trace ℝ) (output : Trace → ℝ) :
    Prop :=
  IsMarkovKernel fiber ∧
  Measurable output ∧
  jointMeasure source = traceLaw source ⊗ₘ fiber ∧
  jointMeasure target = (traceLaw source).map (fun trace => (trace, output trace)) ∧
  ∀ᵐ trace ∂traceLaw source,
    Integrable id (fiber trace) ∧ output trace = ∫ value : ℝ, value ∂fiber trace

/-- The actual source and target joint laws factor over the source's own trace law: the
source output is drawn from a Markov kernel indexed by the trace, the target output is a
measurable function of the trace, and that function is almost surely the fiber's mean. -/
def MeanOnTraces (source target : Expr) : Prop :=
  ∃ (fiber : Kernel Trace ℝ) (output : Trace → ℝ), TraceFactorization source target fiber output

/-- Trace soundness requires no global integrability assumption. -/
def soundnessThm : Prop :=
  ∀ (mode : Mode) (program : Expr),
    Typed [] program (.float mode) → program.sourceForm = true →
    DoesNotGetStuck program →
      DoesNotGetStuck program.determinize ∧ MeanOnTraces program program.determinize

/-- The law of total variance along traces. For every trace factorization of a source whose
output law has a finite second moment, the fiber variances are integrable over the trace law
and the source output variance is the target output variance plus the mean fiber variance:
trace by trace, determinization discards exactly the variance of the fiber. The output laws
are unnormalized, but a factorization gives them the same mass (every fiber is a probability
measure) and, by the almost-sure mean identity, the same mean `∫ output ∂traceLaw source`, so
the identity holds for Mathlib's `variance` (`∫ (v - ∫ v)²`) without normalization; dividing
both output laws and the trace law by their common mass gives the same identity for the laws
conditioned on termination. -/
def VarianceOnTraces (source target : Expr) : Prop :=
  ∀ (fiber : Kernel Trace ℝ) (output : Trace → ℝ),
    TraceFactorization source target fiber output →
    MemLp id 2 (bigStepMeasure source) →
    Integrable (fun trace => variance id (fiber trace)) (traceLaw source) ∧
      variance id (bigStepMeasure source) =
        variance id (bigStepMeasure target) +
          ∫ trace, variance id (fiber trace) ∂traceLaw source

/-- Every trace factorization of a source program decomposes its output variance;
`soundnessThm` supplies such a factorization. -/
def varianceThm : Prop :=
  ∀ (mode : Mode) (program : Expr),
    Typed [] program (.float mode) → program.sourceForm = true →
    DoesNotGetStuck program →
      VarianceOnTraces program program.determinize

end Determinize.Traces
