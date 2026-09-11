import Determinize.Statement.Semantics

namespace Determinize.Proof.Rejection
open Statement.Paper MeasureTheory

/-- A rejection never contributes a returned float at any execution depth. -/
theorem cumulative_reject (fuel : Nat) : cumulativeOutputMeasure fuel .reject = 0 := by
  induction fuel with
  | zero => rfl
  | succ fuel ih => simpa [cumulativeOutputMeasure, reduce] using ih

theorem reject_zero : bigStepMeasure .reject = 0 := by
  simp [bigStepMeasure, cumulative_reject]

/-- Rejection is intentional loss of output mass, not an invalid primitive call. -/
theorem reject_safe : DoesNotGetStuck .reject := by
  intro fuel
  induction fuel with
  | zero => trivial
  | succ fuel ih => simpa [DoesNotGetStuckAt, Expr.isValue, reduce] using ih

theorem cumulative_let_reject (fuel : Nat) (body : Expr) :
    cumulativeOutputMeasure fuel (.letE .reject body) = 0 := by
  induction fuel with
  | zero => rfl
  | succ fuel ih => simpa [cumulativeOutputMeasure, reduce, Expr.isValue, Action.wrap] using ih

theorem let_reject_zero (body : Expr) : bigStepMeasure (.letE .reject body) = 0 := by
  simp [bigStepMeasure, cumulative_let_reject]

theorem failed_observation_zero : bigStepMeasure (.ite (.bool false) .unit .reject) = 0 := by
  unfold bigStepMeasure
  have h (fuel : Nat) : cumulativeOutputMeasure fuel (.ite (.bool false) .unit .reject) = 0 := by
    cases fuel with
    | zero => rfl
    | succ fuel => simp [cumulativeOutputMeasure, reduce, Expr.isValue, cumulative_reject]
  simp [h]

theorem determinize_reject : Expr.determinize (.reject : Expr) = .reject := rfl

end Determinize.Proof.Rejection
