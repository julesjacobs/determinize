import Determinize.Finite.Reward.Normalize
import Determinize.Proof.FiniteModel.Equality
import Determinize.Proof.FiniteModel.Progress

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Finite.Reward

private def additive : Frame → Bool
  | .right .add (.number _) => true
  | _ => false

private theorem splitStack_cons (frame : Frame) (stack : List Frame) :
    splitStack (frame :: stack) =
      if (splitStack stack).1 = [] ∧ additive frame then
        ([], (match frame with | .right .add (.number c) => c | _ => 0) ::
          (splitStack stack).2)
      else (frame :: (splitStack stack).1, (splitStack stack).2) := by
  cases h : splitStack stack with
  | mk inner offsets =>
    cases inner <;> cases frame <;> simp [splitStack, h, additive]
    case nil.right op value =>
      cases op <;> cases value <;> simp

theorem splitStack_reconstruct (stack : List Frame) :
    (splitStack stack).1 ++ ((splitStack stack).2.map fun c => Frame.right .add (.number c)) = stack := by
  induction stack with
  | nil => rfl
  | cons frame stack ih =>
    rw [splitStack_cons]
    split_ifs with h
    · rcases h with ⟨empty, add⟩
      cases frame <;> simp only [additive, Bool.false_eq_true] at add
      case right op value =>
        cases op <;> cases value <;> simp only [Bool.false_eq_true] at add
        simp_all
    · simpa using congrArg (List.cons frame) ih

private theorem splitStack_inner (stack : List Frame) :
    splitStack (splitStack stack).1 = ((splitStack stack).1, []) := by
  induction stack with
  | nil => rfl
  | cons frame stack ih =>
    rw [splitStack_cons]
    split_ifs with h
    · rfl
    · change splitStack (frame :: (splitStack stack).1) = _
      rw [splitStack_cons, ih]
      rw [if_neg h]

private theorem splitStack_append (inner : List Frame) (offsets : List Rat)
    (clean : splitStack inner = (inner, [])) :
    splitStack (inner ++ offsets.map (fun c => Frame.right .add (.number c))) =
      (inner, offsets) := by
  induction inner with
  | nil =>
    induction offsets with
    | nil => rfl
    | cons c cs ih =>
        simp only [List.nil_append] at ih ⊢
        simp [splitStack, ih]
  | cons frame inner ih =>
    rw [splitStack_cons] at clean
    split_ifs at clean with h
    · simp at clean
    · have eqs := Prod.mk.inj clean
      have hi : (splitStack inner).1 = inner := List.cons.inj eqs.1 |>.2
      have hc : splitStack inner = (inner, []) := Prod.ext hi eqs.2
      have blocked : ¬ (inner = [] ∧ additive frame) := by simpa [hi] using h
      rw [List.cons_append, splitStack_cons, ih hc]
      simp [blocked]

theorem normalizeStack_idempotent (stack : List Frame) :
    normalizeStack (normalizeStack stack).2 = (0, (normalizeStack stack).2) := by
  have clean := splitStack_inner stack
  by_cases empty : (splitStack stack).2.isEmpty = true
  · simp [normalizeStack, empty, clean]
  · have hg := splitStack_append (splitStack stack).1 [0] clean
    simp only [List.map_cons, List.map_nil] at hg
    simp [normalizeStack, empty, Reward.guard, hg]

theorem normalize_idempotent (state : State) :
    Reward.normalize (Reward.normalize state).2 = (0, (Reward.normalize state).2) := by
  cases state with
  | eval expression environment stack =>
      simp only [Reward.normalize, normalizeStack_idempotent]
  | deliver value stack =>
      simp only [Reward.normalize, normalizeStack_idempotent]
  | rejected => rfl

end Determinize.Proof.RewardModel
