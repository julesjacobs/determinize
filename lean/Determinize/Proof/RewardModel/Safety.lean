import Determinize.Proof.RewardModel.Correspondence

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Proof.FiniteModel

def failureWithin : Nat → State → Prop
  | 0, s => ∃ e, step s = .error e
  | n+1, s => match step s with
      | .error _ => True
      | .ok (.next _ xs) => ∃ x ∈ xs, 0 < x.1 ∧ failureWithin n x.2
      | _ => False

private theorem numeric_no_failure (offsets : List Rat) (b : Rat) (n : Nat) :
    ¬ failureWithin n (.deliver (.number b) (additionStack offsets)) := by
  induction n generalizing b offsets with
  | zero => cases offsets <;> simp [failureWithin, additionStack, step, binary, pure, bind, Except.bind, Except.pure]
  | succ n ih =>
    cases offsets with
    | nil => simp [failureWithin, additionStack, step]
    | cons c cs =>
      simpa [failureWithin, additionStack, step, binary, pure, bind, Except.bind, Except.pure] using ih cs (c+b)

theorem addition_failure_iff (offsets : List Rat) (n : Nat) (state : State) :
    failureWithin n (pushStack state (additionStack offsets)) ↔ failureWithin n state := by
  induction n generalizing state with
  | zero =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [failureWithin, action, herr]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp [failureWithin, action, step_push state (additionStack offsets) tag xs action]
      · simpa [pushStack, failureWithin, step] using numeric_no_failure offsets b 0
      · simp [pushStack, failureWithin, step]
  | succ n ih =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [failureWithin, action, herr]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp [failureWithin, action, step_push state (additionStack offsets) tag xs action, extend, ih]
      · simpa [pushStack, failureWithin, step] using numeric_no_failure offsets b (n+1)
      · simp [pushStack, failureWithin, step]

theorem normalize_failure_iff (state : State) (n : Nat) :
    failureWithin n (Reward.normalize state).2 ↔ failureWithin n state := by
  cases state with
  | rejected => rfl
  | eval e env stack =>
    let base := State.eval e env (Reward.splitStack stack).1
    have original := addition_failure_iff (Reward.splitStack stack).2 n base
    have canonical := addition_failure_iff [0] n base
    have reconstructed := RewardModel.splitStack_reconstruct stack
    simp only [pushStack, base, additionStack, reconstructed] at original
    by_cases empty : (Reward.splitStack stack).2 = []
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack] using original.symm
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack, Reward.guard] using
        canonical.trans original.symm
  | deliver v stack =>
    let base := State.deliver v (Reward.splitStack stack).1
    have original := addition_failure_iff (Reward.splitStack stack).2 n base
    have canonical := addition_failure_iff [0] n base
    have reconstructed := RewardModel.splitStack_reconstruct stack
    simp only [pushStack, base, additionStack, reconstructed] at original
    by_cases empty : (Reward.splitStack stack).2 = []
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack] using original.symm
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack, Reward.guard] using
        canonical.trans original.symm

def rewardFailureWithin : Nat → State → Prop
  | 0, s => ∃ e, step s = .error e
  | n+1, s => match step s with
      | .error _ => True
      | .ok (.next _ xs) => ∃ x ∈ xs, 0 < x.1 ∧ rewardFailureWithin n (Reward.normalize x.2).2
      | _ => False

theorem failure_implies_rewardFailure (n : Nat) (state : State) :
    failureWithin n state → rewardFailureWithin n state := by
  induction n generalizing state with
  | zero => exact id
  | succ n ih =>
    cases action : step state with
    | error e => simp [failureWithin, rewardFailureWithin, action]
    | ok result =>
      cases result with
      | returned b => simp [failureWithin, action]
      | rejected => simp [failureWithin, action]
      | next tag xs =>
        simp only [failureWithin, rewardFailureWithin, action]
        rintro ⟨x, member, positive, failed⟩
        exact ⟨x, member, positive, ih _ ((normalize_failure_iff x.2 n).mpr failed)⟩

end Determinize.Proof.RewardModel
