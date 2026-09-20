import Determinize.Proof.RewardModel.Correspondence

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Proof.FiniteModel Spec.RewardModel MeasureTheory

noncomputable def rewardOutput : Nat → State → Measure ℝ
  | 0, state => match step state with
      | .ok (.returned b) => Measure.dirac (b : ℝ)
      | _ => 0
  | n+1, state => match step state with
      | .ok (.returned b) => Measure.dirac (b : ℝ)
      | .ok (.next _ xs) => weightedOutput xs fun s =>
          shift (Reward.normalize s).1 (rewardOutput n (Reward.normalize s).2)
      | _ => 0

theorem rewardOutput_mono (state : State) : Monotone (fun n => rewardOutput n state) := by
  apply monotone_nat_of_le_succ
  intro n
  induction n generalizing state with
  | zero =>
      cases action : step state with
      | error e => simp [rewardOutput, action]
      | ok result => cases result <;> simp [rewardOutput, action, Measure.zero_le]
  | succ n ih =>
      cases action : step state with
      | error e => simp [rewardOutput, action]
      | ok result =>
          cases result <;> simp only [rewardOutput, action]
          all_goals try exact le_rfl
          exact weightedOutput_mono _ _ _ (fun e _ _ => shift_mono _ (ih _))

theorem machine_le_reward (n : Nat) (state : State) : machineOutput n state ≤ rewardOutput n state := by
  induction n generalizing state with
  | zero => rfl
  | succ n ih =>
      cases action : step state with
      | error e => simp [machineOutput, rewardOutput, action]
      | ok result =>
          cases result <;> simp only [machineOutput, rewardOutput, action]
          all_goals try exact le_rfl
          exact weightedOutput_mono _ _ _ (fun e _ _ =>
            (normalize_output_upper e.2 n).trans (shift_mono _ (ih _)))

theorem reward_le_machine (n : Nat) (state : State) :
    ∃ horizon, rewardOutput n state ≤ machineOutput horizon state := by
  induction n generalizing state with
  | zero => exact ⟨0, le_rfl⟩
  | succ n ih =>
      cases action : step state with
      | error e => exact ⟨0, by simp [rewardOutput, machineOutput, action]⟩
      | ok result =>
        cases result with
        | returned b => exact ⟨0, by simp [rewardOutput, machineOutput, action]⟩
        | rejected => exact ⟨0, by simp [rewardOutput, machineOutput, action]⟩
        | next tag xs =>
          obtain ⟨horizon, bound⟩ := weightedOutput_uniform_bound xs
            (fun s => shift (Reward.normalize s).1 (rewardOutput n (Reward.normalize s).2))
            machineOutput machineOutput_mono (by
              intro e _ _
              obtain ⟨k, hk⟩ := ih (Reward.normalize e.2).2
              obtain ⟨h, hh⟩ := normalize_output_lower e.2 k
              exact ⟨h, (shift_mono _ hk).trans hh⟩)
          exact ⟨horizon+1, by simpa only [rewardOutput, machineOutput, action] using bound⟩

theorem reward_output_eq_machine (state : State) :
    (⨆ n, rewardOutput n state) = machineOutputMeasure state := by
  apply le_antisymm
  · apply iSup_le
    intro n
    obtain ⟨h, bound⟩ := reward_le_machine n state
    exact bound.trans (le_iSup (fun k => machineOutput k state) h)
  · apply iSup_le
    intro n
    exact (machine_le_reward n state).trans (le_iSup (fun k => rewardOutput k state) n)

end Determinize.Proof.RewardModel
