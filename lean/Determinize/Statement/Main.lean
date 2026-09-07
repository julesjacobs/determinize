import Determinize.Statement.Semantics
import Mathlib.Analysis.Convex.Function
import Mathlib.Data.EReal.Operations

/-!
# Soundness of paper determinization

These are the reviewer-facing theorem statements over the fixed paper syntax,
typing judgment, determinization transformation, and measure semantics.

`mainThm` is the expectation theorem for finite (Bochner) expectations.
`extendedExpectationThm` states it in the extended reals, where the expectation of a law
is well-defined as soon as one of its positive and negative parts is finite; it covers
programs such as `1 / uniform(0, 1)` whose expectation is `+∞`. `jensenThm` compares the
two output laws against every nonnegative convex function.
-/

namespace Determinize.Statement

open MeasureTheory Paper
open scoped ENNReal

/-- Determinization preserves finite expectations and cannot introduce stuckness. -/
def mainThm : Prop :=
  ∀ (mode : Mode) (program : Expr),
    Typed [] program (.float mode) →
    program.sourceForm = true →
    let source := observeFloat mode program
    let target := source.determinize
    DoesNotGetStuck source →
    Integrable id (bigStepMeasure source) →
    DoesNotGetStuck target ∧
      Integrable id (bigStepMeasure target) ∧
      (∫ value : ℝ, value ∂bigStepMeasure source) =
        ∫ value : ℝ, value ∂bigStepMeasure target

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

/-- Determinization preserves expectations in the extended reals: whenever the source
expectation is well-defined, possibly infinite, so is the target's, and they agree. -/
def extendedExpectationThm : Prop :=
  ∀ (mode : Mode) (program : Expr),
    Typed [] program (.float mode) →
    program.sourceForm = true →
    let source := observeFloat mode program
    let target := source.determinize
    DoesNotGetStuck source →
    HasExpectation (bigStepMeasure source) →
    HasExpectation (bigStepMeasure target) ∧
      extendedExpectation (bigStepMeasure source) = extendedExpectation (bigStepMeasure target)

/-- Jensen's inequality: every nonnegative convex function integrates to at most as much
under the determinized output law as under the source output law. -/
def jensenThm : Prop :=
  ∀ (mode : Mode) (program : Expr),
    Typed [] program (.float mode) →
    program.sourceForm = true →
    let source := observeFloat mode program
    let target := source.determinize
    DoesNotGetStuck source →
    ∀ φ : ℝ → ℝ, ConvexOn ℝ Set.univ φ → (∀ value, 0 ≤ φ value) →
      ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure target ≤
        ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure source

end Determinize.Statement
