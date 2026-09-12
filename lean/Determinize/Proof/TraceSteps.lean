import Determinize.Proof.Replay

/-!
# One-step unfoldings of the exact-depth law

How `exactMeasure (depth + 1)` unfolds for a deterministic step, a rejection and a sampling
step.
-/

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper

noncomputable section

theorem exact_succ_next (depth : Nat) (expression next : Expr)
    (notValue : expression.isValue ≠ true) (reduction : reduce expression = .next next) :
    exactMeasure (depth + 1) expression = (exactMeasure depth next).map (prepend none) := by
  simp [exactMeasure, notValue, reduction]

theorem exact_succ_sample (depth : Nat) (expression : Expr) (fiber : Measure ℝ)
    (continuation : ℝ → Expr) (notValue : expression.isValue ≠ true)
    (reduction : reduce expression = .sample site fiber continuation) :
    exactMeasure (depth + 1) expression = fiber.bind fun value =>
      (exactMeasure depth (continuation value)).map
        (prepend (entry (generationOp expression.skeleton) value)) := by
  simp only [exactMeasure, notValue, Bool.false_eq_true, ↓reduceIte, reduction]
  simp_rw [reduce_site reduction, ← generationEvent_eq_entry]

end

end Determinize.Proof.StepTraces
