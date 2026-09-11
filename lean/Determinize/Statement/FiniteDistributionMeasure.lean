import Determinize.Statement.FiniteDistribution
import Mathlib.MeasureTheory.Integral.Bochner.Basic

namespace Determinize.Statement.Paper
open MeasureTheory

/-- Real-valued law of a rational finite distribution with the given outcome values. -/
noncomputable def FiniteDistribution.measure (d : FiniteDistribution) (value : Nat → ℝ) : Measure ℝ :=
  (d.probabilities.zipIdx.map fun (p, i) =>
    ENNReal.ofReal (p : ℝ) • Measure.dirac (value i)).sum

end Determinize.Statement.Paper
