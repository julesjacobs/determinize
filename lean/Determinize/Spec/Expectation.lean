import Determinize.Spec.Semantics
import Mathlib.Data.EReal.Operations

/-!
# Expectations of output laws

The output law `bigStepMeasure program` is unnormalized: its total mass is the probability that
the program returns a real. `extendedExpectation` is its expectation in the extended reals,
which is well-defined (`HasExpectation`) as soon as one of the positive and negative parts is
finite; it covers programs such as `1 / uniform(0, 1)` whose expectation is `+∞`. `returnedLaw`
is the output law conditioned on returning.
-/

namespace Determinize.Spec

open MeasureTheory Paper
open scoped ENNReal

/-- The positive part `∫ v⁺ dμ` of the expectation of a real law. -/
noncomputable def posPartIntegral (μ : Measure ℝ) : ℝ≥0∞ := ∫⁻ value, ENNReal.ofReal value ∂μ

/-- The negative part `∫ v⁻ dμ` of the expectation of a real law. -/
noncomputable def negPartIntegral (μ : Measure ℝ) : ℝ≥0∞ :=
  ∫⁻ value, ENNReal.ofReal (-value) ∂μ

/-- The expectation of `μ` is well-defined in `[-∞, ∞]` when at least one part is finite. -/
def HasExpectation (μ : Measure ℝ) : Prop := posPartIntegral μ ≠ ⊤ ∨ negPartIntegral μ ≠ ⊤

/-- The expectation `∫ v⁺ dμ - ∫ v⁻ dμ` in the extended reals; meaningful under
`HasExpectation μ`. -/
noncomputable def extendedExpectation (μ : Measure ℝ) : EReal :=
  (posPartIntegral μ : EReal) - (negPartIntegral μ : EReal)

/-- Output law conditioned on returning a real; used only when output mass is positive. -/
noncomputable def returnedLaw (program : Expr) : Measure ℝ :=
  (bigStepMeasure program Set.univ)⁻¹ • bigStepMeasure program

end Determinize.Spec
