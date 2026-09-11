import Determinize.Traces.Main
import Mathlib.Probability.Kernel.Composition.MeasureCompProd

/-!
# Trace factorizations

The proof-internal form of trace soundness: the source and target joint laws factor over the
source trace law through a Markov kernel and a measurable output function. The public
`Traces.soundnessThm` instantiates the kernel with `Traces.outputGivenTrace`; the corollaries
in `Proof/Corollaries.lean` are proved for an arbitrary factorization.
-/

namespace Determinize.Proof.Traces

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Traces
open scoped ProbabilityTheory

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

/-- Some trace factorization of the source and target joint laws exists. -/
def MeanOnTraces (source target : Expr) : Prop :=
  ∃ (fiber : Kernel Trace ℝ) (output : Trace → ℝ), TraceFactorization source target fiber output

end Determinize.Proof.Traces
