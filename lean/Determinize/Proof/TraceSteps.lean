import Determinize.Proof.Replay

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

theorem replay_succ_next (depth : Nat) (expression next : Expr) (tape : Trace)
    (notValue : expression.isValue ≠ true) (opNone : generationOp expression.skeleton = none)
    (reduction : reduce expression = .next next) :
    replayMeasure (depth + 1) expression tape = replayMeasure depth next (List.tail tape) := by
  rw [replayMeasure, if_neg notValue, opNone, Determinize.Spec.Paper.stepMeasure, reduction]
  change (Measure.dirac next).bind (fun e => replayMeasure depth e (List.tail tape)) = _
  have familyMeasurable : Measurable (fun e => replayMeasure depth e (List.tail tape)) := by
    have eq : (fun e => replayMeasure depth e (List.tail tape)) =
        fun e => (replayKernel depth).kernel (List.tail tape, e) := by
      funext e
      exact (replayKernel_apply _ _ _).symm
    rw [eq]
    exact (replayKernel depth).kernel.measurable.comp (measurable_const.prodMk measurable_id)
  exact Measure.dirac_bind familyMeasurable next

theorem replay_succ_sampleE (depth : Nat) (expression : Expr) (fiber : Measure ℝ)
    (continuation : ℝ → Expr) (tape : Trace)
    (notValue : expression.isValue ≠ true) (opNone : generationOp expression.skeleton = none)
    (reduction : reduce expression = .sample site fiber continuation) :
    replayMeasure (depth + 1) expression tape =
      fiber.bind fun value => replayMeasure depth (continuation value) (List.tail tape) := by
  rw [replayMeasure, if_neg notValue, opNone, Determinize.Spec.Paper.stepMeasure, reduction]
  change (fiber.map continuation).bind (fun e => replayMeasure depth e (List.tail tape)) = _
  let previous := SFiniteKernel.pullback (replayKernel depth) (fun e => (List.tail tape, e))
    (measurable_const.prodMk measurable_id)
  have previousEq : ∀ e, previous.kernel e = replayMeasure depth e (List.tail tape) := by
    intro e
    rw [MeasurableActionFamily.pullback_apply, replayKernel_apply]
  simp_rw [← previousEq]
  exact bind_map _ _ ((MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable
    expression fiber continuation reduction) previous.kernel

theorem replay_succ_sampleG (depth : Nat) (expression : Expr) (fiber : Measure ℝ)
    (continuation : ℝ → Expr) (tape : Trace) (op : Op)
    (notValue : expression.isValue ≠ true) (opSome : generationOp expression.skeleton = some op)
    (reduction : reduce expression = .sample site fiber continuation) :
    replayMeasure (depth + 1) expression tape =
      replayMeasure depth (continuation (eventValue (tape.headD none))) (List.tail tape) := by
  rw [replayMeasure, if_neg notValue, opSome, sampleContinuation, reduction]

end

end Determinize.Proof.StepTraces
