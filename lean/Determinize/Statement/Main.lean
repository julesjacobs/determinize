import Determinize.Statement.Semantics

/-!
# Soundness of paper determinization

This is the reviewer-facing theorem statement over the fixed paper syntax,
typing judgment, determinization transformation, and measure semantics.
-/

namespace Determinize.Statement

open MeasureTheory Paper

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

end Determinize.Statement
