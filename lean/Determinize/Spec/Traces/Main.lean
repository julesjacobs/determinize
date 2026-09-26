import Determinize.Spec.Main
import Determinize.Spec.Traces.Semantics
import Mathlib.Probability.Kernel.Disintegration.StandardBorel
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

/-- Every joint law is a finite measure. Mathlib's disintegration `Measure.condKernel` requires
this, so the statements below that use it assume it. -/
def finiteJointLawThm : Prop :=
  ∀ program : Expr, IsFiniteMeasure (traceAndOutputLaw program)

/-- The source and target have the same law of terminating G traces. For almost every such
trace, the source's conditional output law has a finite mean and the target's conditional
output law is the Dirac mass at that mean. These are Mathlib's regular conditional
distributions of the joint laws over their trace marginals, unique up to null sets.
No global integrability assumption is required. -/
def conditionalLawThm : Prop :=
  ∀ [∀ program, IsFiniteMeasure (traceAndOutputLaw program)] (program : Expr),
    Typed [] program (.float .E) → DomainSafe program →
      DomainSafe program.determinize ∧
      traceLaw program.determinize = traceLaw program ∧
      ∀ᵐ trace ∂traceLaw program,
        Integrable id ((traceAndOutputLaw program).condKernel trace) ∧
        (traceAndOutputLaw program.determinize).condKernel trace =
          Measure.dirac (∫ value : ℝ, value ∂(traceAndOutputLaw program).condKernel trace)

/-- The law of total variance along traces. When the source output law has a finite second
moment, the variances of the source's output laws given the traces are integrable over the
trace law and the source output variance is the target output variance plus their mean: trace
by trace, determinization discards exactly the variance of the output given the trace. The
output laws are unnormalized, but they have the same mass and, by `conditionalLawThm`, the same mean,
so the identity holds for Mathlib's `variance` (`∫ (v - ∫ v)²`) without normalization; dividing
both output laws and the trace law by their common mass gives the same identity for the laws
conditioned on termination. -/
def varianceThm : Prop :=
  ∀ [∀ program, IsFiniteMeasure (traceAndOutputLaw program)] (program : Expr),
    Typed [] program (.float .E) → DomainSafe program →
    MemLp id 2 (bigStepMeasure program) →
      Integrable (fun trace => variance id ((traceAndOutputLaw program).condKernel trace))
        (traceLaw program) ∧
      variance id (bigStepMeasure program) =
        variance id (bigStepMeasure program.determinize) +
          ∫ trace, variance id ((traceAndOutputLaw program).condKernel trace) ∂traceLaw program

/-- Total variance conditioned on returning, with the trace law normalized by output mass. -/
def conditionalVarianceThm : Prop :=
  ∀ [∀ program, IsFiniteMeasure (traceAndOutputLaw program)] (program : Expr),
    Typed [] program (.float .E) → DomainSafe program →
    bigStepMeasure program Set.univ ≠ 0 → MemLp id 2 (bigStepMeasure program) →
      Integrable (fun trace => variance id ((traceAndOutputLaw program).condKernel trace))
        ((bigStepMeasure program Set.univ)⁻¹ • traceLaw program) ∧
      variance id (returnedLaw program) =
        variance id (returnedLaw program.determinize) +
          ∫ trace, variance id ((traceAndOutputLaw program).condKernel trace)
            ∂((bigStepMeasure program Set.univ)⁻¹ • traceLaw program)

end Determinize.Spec.Traces
