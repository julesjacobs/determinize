import Determinize.Spec.Semantics
import Determinize.Spec.Semantics

namespace Determinize.Proof.Rejection
open Spec.Paper MeasureTheory

/-- A rejection never contributes a returned float at any execution depth. -/
theorem outputMeasureAt_reject (fuel : Nat) : outputMeasureAt fuel .reject = 0 := by
  induction fuel with
  | zero => rfl
  | succ fuel ih => simpa [outputMeasureAt, reduce, Expr.isValue] using ih

theorem bigStepMeasure_reject : bigStepMeasure .reject = 0 := by
  simp [bigStepMeasure, outputMeasureAt_reject]

/-- Rejection is intentional loss of output mass, not an invalid primitive call. -/
theorem domainSafe_reject : DomainSafe .reject := by
  intro fuel
  induction fuel with
  | zero => trivial
  | succ fuel ih => simpa [DomainSafeAt, Expr.isValue, reduce] using ih

theorem outputMeasureAt_let_reject (fuel : Nat) (body : Expr) :
    outputMeasureAt fuel (.letE .reject body) = 0 := by
  induction fuel with
  | zero => rfl
  | succ fuel ih => simpa [outputMeasureAt, reduce, Expr.isValue, Action.wrap] using ih

theorem bigStepMeasure_let_reject (body : Expr) : bigStepMeasure (.letE .reject body) = 0 := by
  simp [bigStepMeasure, outputMeasureAt_let_reject]

theorem bigStepMeasure_failed_observation :
    bigStepMeasure (.ite (.bool false) .unit .reject) = 0 := by
  unfold bigStepMeasure
  have h (fuel : Nat) : outputMeasureAt fuel (.ite (.bool false) .unit .reject) = 0 := by
    cases fuel with
    | zero => rfl
    | succ fuel => simp [outputMeasureAt, reduce, Expr.isValue, outputMeasureAt_reject]
  simp [h]

theorem determinize_reject : Expr.determinize (.reject : Expr) = .reject := rfl

end Determinize.Proof.Rejection
