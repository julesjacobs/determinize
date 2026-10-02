import Determinize.Proof.RewardModel.Correspondence

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Proof.FiniteModel

def FailureWithin : Nat → State → Prop
  | 0, s => ∃ e, step s = .error e
  | n + 1, s => match step s with
      | .error _ => True
      | .ok (.next _ xs) => ∃ x ∈ xs, 0 < x.1 ∧ FailureWithin n x.2
      | _ => False

private theorem not_failureWithin_number (offsets : List Rat) (b : Rat) (n : Nat) :
    ¬ FailureWithin n (.deliver (.number b) (additionStack offsets)) := by
  induction n generalizing b offsets with
  | zero =>
    cases offsets <;> simp [FailureWithin, additionStack, step, binary, pure, bind, Except.bind,
      Except.pure]
  | succ n ih =>
    cases offsets with
    | nil => simp [FailureWithin, additionStack, step]
    | cons c cs =>
      simpa [FailureWithin, additionStack, step, binary, pure, bind, Except.bind, Except.pure]
        using ih cs (c + b)

theorem failureWithin_pushStack_additionStack_iff (offsets : List Rat) (n : Nat) (state : State) :
    FailureWithin n (pushStack state (additionStack offsets)) ↔ FailureWithin n state := by
  induction n generalizing state with
  | zero =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [FailureWithin, action, herr]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp [FailureWithin, action, step_pushStack state (additionStack offsets) tag xs action]
      · simpa [pushStack, FailureWithin, step] using not_failureWithin_number offsets b 0
      · simp [pushStack, FailureWithin, step]
  | succ n ih =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [FailureWithin, action, herr]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp [FailureWithin, action, step_pushStack state (additionStack offsets) tag xs action,
          extend,
          ih]
      · simpa [pushStack, FailureWithin, step] using not_failureWithin_number offsets b (n + 1)
      · simp [pushStack, FailureWithin, step]

theorem failureWithin_normalize_iff (state : State) (n : Nat) :
    FailureWithin n (Reward.normalize state).2 ↔ FailureWithin n state := by
  cases state with
  | rejected => rfl
  | eval e env stack =>
    let base := State.eval e env (Reward.splitStack stack).1
    have original := failureWithin_pushStack_additionStack_iff (Reward.splitStack stack).2 n base
    have canonical := failureWithin_pushStack_additionStack_iff [0] n base
    have reconstructed := RewardModel.splitStack_reconstruct stack
    simp only [pushStack, base, additionStack, reconstructed] at original
    by_cases empty : (Reward.splitStack stack).2 = []
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack]
        using original.symm
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack,
        Reward.guard] using
        canonical.trans original.symm
  | deliver v stack =>
    let base := State.deliver v (Reward.splitStack stack).1
    have original := failureWithin_pushStack_additionStack_iff (Reward.splitStack stack).2 n base
    have canonical := failureWithin_pushStack_additionStack_iff [0] n base
    have reconstructed := RewardModel.splitStack_reconstruct stack
    simp only [pushStack, base, additionStack, reconstructed] at original
    by_cases empty : (Reward.splitStack stack).2 = []
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack]
        using original.symm
    · simpa [Reward.normalize, Reward.normalizeStack, empty, base, pushStack, additionStack,
        Reward.guard] using
        canonical.trans original.symm

def RewardFailureWithin : Nat → State → Prop
  | 0, s => ∃ e, step s = .error e
  | n + 1, s => match step s with
      | .error _ => True
      | .ok (.next _ xs) => ∃ x ∈ xs, 0 < x.1 ∧ RewardFailureWithin n (Reward.normalize x.2).2
      | _ => False

theorem rewardFailureWithin_of_failureWithin (n : Nat) (state : State) :
    FailureWithin n state → RewardFailureWithin n state := by
  induction n generalizing state with
  | zero => exact id
  | succ n ih =>
    cases action : step state with
    | error e => simp [FailureWithin, RewardFailureWithin, action]
    | ok result =>
      cases result with
      | returned b => simp [FailureWithin, action]
      | rejected => simp [FailureWithin, action]
      | next tag xs =>
        simp only [FailureWithin, RewardFailureWithin, action]
        rintro ⟨x, member, positive, failed⟩
        exact ⟨x, member, positive, ih _ ((failureWithin_normalize_iff x.2 n).mpr failed)⟩

end Determinize.Proof.RewardModel
