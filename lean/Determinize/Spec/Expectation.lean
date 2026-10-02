import Determinize.Spec.Semantics
import Mathlib.Data.EReal.Operations
import Mathlib.Probability.Moments.Variance

/-!
# Expectation and variance of returned values

The output law `bigStepMeasure program` is unnormalized: its total mass, `returnProbability`, is
the probability that the program returns a real. `extendedExpectation` is the expectation of a
law in the extended reals, which is well-defined (`HasExpectation`) as soon as one of its positive
and negative parts is finite; it covers programs such as `1 / uniform(0, 1)` whose expectation is
`+∞`. `returnedExpectation` and `returnedVariance` are the paper's `𝔼_ret[e]` and `Var_ret[e]`:
the expectation and the variance of the output conditioned on returning.
-/

namespace Determinize.Spec

open MeasureTheory ProbabilityTheory Paper
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

/-- The probability `q_e` that the program returns a real: the total mass of its output law. -/
noncomputable def returnProbability (program : Expr) : ℝ≥0∞ := bigStepMeasure program Set.univ

/-- Output law conditioned on returning a real; used only when the return probability is
positive. -/
noncomputable def returnedLaw (program : Expr) : Measure ℝ :=
  (returnProbability program)⁻¹ • bigStepMeasure program

/-- The expectation `𝔼_ret[e]` of the output conditioned on returning, in the extended reals:
the expectation of the output law divided by the return probability. It is meaningful when the
return probability is positive and `HasExpectation (bigStepMeasure program)`. -/
noncomputable def returnedExpectation (program : Expr) : EReal :=
  ((returnProbability program).toReal⁻¹ : EReal) * extendedExpectation (bigStepMeasure program)

/-- The variance `Var_ret[e]` of the output conditioned on returning: Mathlib's `variance`
(`∫ (v - ∫ v)²`) of the returned law. -/
noncomputable def returnedVariance (program : Expr) : ℝ := variance id (returnedLaw program)

end Determinize.Spec
