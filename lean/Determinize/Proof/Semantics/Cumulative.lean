import Determinize.Spec.Semantics

namespace Determinize.Proof.Cumulative
open Spec.Paper MeasureTheory

/-- Real output accumulated through `fuel` reduction steps. Only terminal reals
contribute output; other types may occur during evaluation. -/
noncomputable def outputMeasure : Nat → Expr → Measure ℝ
  | 0, .real value => Measure.dirac value
  | 0, _ => 0
  | fuel + 1, expression => match reduce expression with
      | .next next => outputMeasure fuel next
      | .sample _ fiber continuation =>
          fiber.bind fun value => outputMeasure fuel (continuation value)
      | .stuck => 0

theorem absorbing_zero (expression : Expr)
    (notValue : expression.isValue = false) (absorbing : reduce expression = .next expression)
    (fuel : Nat) : outputMeasure fuel expression = 0 := by
  induction fuel with
  | zero => cases expression <;> simp_all [Expr.isValue, outputMeasure]
  | succ fuel ih => simpa [outputMeasure, absorbing] using ih

end Determinize.Proof.Cumulative
