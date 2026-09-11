import Determinize.Proof.FiniteModel.Progress
import Determinize.Proof.FiniteModel.MeasureLaws

namespace Determinize.Proof.FiniteModel
open Statement.Paper Determinize.Finite MeasureTheory

noncomputable def machineOutput : Nat → State → Measure ℝ
  | 0, state => match step state with
      | .ok (.returned reward) => Measure.dirac (reward : ℝ)
      | _ => 0
  | fuel + 1, state => match step state with
      | .ok (.returned reward) => Measure.dirac (reward : ℝ)
      | .ok (.next _ successors) => weightedOutput successors (machineOutput fuel)
      | _ => 0

noncomputable def machineOutputMeasure (state : State) : Measure ℝ :=
  ⨆ fuel, machineOutput fuel state

def SameObservations (before after : State) : Prop :=
  (∀ fuel, cumulativeOutputMeasure fuel (stateExpr before) =
    cumulativeOutputMeasure fuel (stateExpr after)) ∧
  (∀ fuel, DoesNotGetStuckAt fuel (stateExpr before) ↔ DoesNotGetStuckAt fuel (stateExpr after))

def PaperStep (before : State) (successors : List (Rat × State)) : Prop :=
  cumulativeOutputMeasure 0 (stateExpr before) = 0 ∧
  (∀ fuel, cumulativeOutputMeasure (fuel+1) (stateExpr before) =
    weightedOutput successors (fun after => cumulativeOutputMeasure fuel (stateExpr after))) ∧
  (∀ fuel, DoesNotGetStuckAt (fuel+1) (stateExpr before) ↔
    ∀ outcome ∈ successors, 0 < outcome.1 → DoesNotGetStuckAt fuel (stateExpr outcome.2))

def StepMeaning (state : State) : Step → Prop
  | .returned reward =>
      (∀ fuel, cumulativeOutputMeasure fuel (stateExpr state) = Measure.dirac (reward : ℝ)) ∧
      DoesNotGetStuck (stateExpr state)
  | .rejected =>
      (∀ fuel, cumulativeOutputMeasure fuel (stateExpr state) = 0) ∧ DoesNotGetStuck (stateExpr state)
  | .next _ successors =>
      (∃ after, successors = [(1,after)] ∧ Bookkeeping state ∧ SameObservations state after) ∨
      PaperStep state successors

theorem machineOutput_mono (state : State) : Monotone (fun fuel => machineOutput fuel state) := by
  apply monotone_nat_of_le_succ
  intro fuel
  induction fuel generalizing state with
  | zero => cases h : step state <;> simp [machineOutput, h]
            rename_i result
            cases result <;> simp
            exact bot_le
  | succ fuel ih =>
      cases h : step state with
      | error failure => simp [machineOutput, h]
      | ok result =>
          cases result <;> simp only [machineOutput, h]
          · exact weightedOutput_mono _ _ _ (fun entry _ _ => ih entry.2)
          all_goals exact le_rfl

theorem machineOutput_le_paper (initial : State)
    (meaning : ∀ state, MachineReachable initial state → ∀ result,
      step state = .ok result → StepMeaning state result)
    (fuel : Nat) (state : State) (reachable : MachineReachable initial state) :
    machineOutput fuel state ≤ cumulativeOutputMeasure fuel (stateExpr state) := by
  induction fuel generalizing state with
  | zero =>
      cases action : step state with
      | error failure => simpa only [machineOutput, action] using (Measure.zero_le _)
      | ok result =>
          have localMeaning := meaning state reachable result action
          cases result with
          | returned reward => simpa [machineOutput, action] using (localMeaning.1 0).ge
          | rejected => simpa only [machineOutput, action] using (Measure.zero_le _)
          | next evidence successors => simpa only [machineOutput, action] using (Measure.zero_le _)
  | succ fuel ih =>
      cases action : step state with
      | error failure => simpa only [machineOutput, action] using (Measure.zero_le _)
      | ok result =>
          have localMeaning := meaning state reachable result action
          cases result with
          | returned reward => simpa [machineOutput, action] using (localMeaning.1 (fuel+1)).ge
          | rejected => simpa only [machineOutput, action] using (Measure.zero_le _)
          | next evidence successors =>
              rcases localMeaning with ⟨after, rfl, _, same⟩ | advance
              · have nextReach : MachineReachable initial after := MachineReachable.next reachable action (by simp) (by norm_num : (0:Rat)<1)
                have bound := ih after nextReach
                rw [← same.1 fuel] at bound
                simpa [machineOutput, action, weightedOutput] using
                  bound.trans (Proof.Paper.direct_cumulative_mono _ (Nat.le_succ fuel))
              · rw [machineOutput, action, advance.2.1 fuel]
                exact weightedOutput_mono _ _ _ (fun entry member positive =>
                  ih entry.2 (.next reachable action member positive))

theorem execution_safe (initial : State)
    (meaning : ∀ state, MachineReachable initial state → ∀ result,
      step state = .ok result → StepMeaning state result)
    (noFailure : ∀ state, MachineReachable initial state → ∀ failure, step state ≠ .error failure)
    (fuel : Nat) (state : State) (reachable : MachineReachable initial state) :
    DoesNotGetStuckAt fuel (stateExpr state) := by
  cases fuel with
  | zero => trivial
  | succ fuel =>
      cases action : step state with
      | error failure => exact False.elim (noFailure state reachable failure action)
      | ok result =>
          have localMeaning := meaning state reachable result action
          cases result with
          | returned reward => exact localMeaning.2 (fuel+1)
          | rejected => exact localMeaning.2 (fuel+1)
          | next evidence successors =>
              rcases localMeaning with ⟨after, rfl, bookkeeping, same⟩ | advance
              · have nextReach : MachineReachable initial after := MachineReachable.next reachable action (by simp) (by norm_num : (0:Rat)<1)
                have decrease := bookkeeping_decreases state bookkeeping _ _ action 1 after (by simp)
                exact (same.2 (fuel+1)).mpr
                  (execution_safe initial meaning noFailure (fuel+1) after nextReach)
              · apply (advance.2.2 fuel).mpr
                intro entry member positive
                exact execution_safe initial meaning noFailure fuel entry.2
                  (.next reachable action member positive)
termination_by (fuel, bookkeepingRank state)
decreasing_by all_goals omega

theorem paper_le_machine_horizon (initial : State)
    (meaning : ∀ state, MachineReachable initial state → ∀ result,
      step state = .ok result → StepMeaning state result)
    (noFailure : ∀ state, MachineReachable initial state → ∀ failure, step state ≠ .error failure)
    (fuel : Nat) (state : State) (reachable : MachineReachable initial state) :
    ∃ horizon, cumulativeOutputMeasure fuel (stateExpr state) ≤ machineOutput horizon state := by
  cases action : step state with
  | error failure => exact False.elim (noFailure state reachable failure action)
  | ok result =>
      have localMeaning := meaning state reachable result action
      cases result with
      | returned reward => exact ⟨0, by simp [machineOutput, action, localMeaning.1 fuel]⟩
      | rejected => exact ⟨0, by simp [machineOutput, action, localMeaning.1 fuel]⟩
      | next evidence successors =>
          rcases localMeaning with ⟨after, rfl, bookkeeping, same⟩ | advance
          · have nextReach : MachineReachable initial after := MachineReachable.next reachable action (by simp) (by norm_num : (0:Rat)<1)
            have decrease := bookkeeping_decreases state bookkeeping _ _ action 1 after (by simp)
            obtain ⟨horizon, bound⟩ := paper_le_machine_horizon initial meaning noFailure fuel after nextReach
            refine ⟨horizon+1, ?_⟩
            simpa [machineOutput, action, weightedOutput, same.1 fuel] using bound
          · cases fuel with
            | zero => exact ⟨0, by rw [advance.1]; exact bot_le⟩
            | succ fuel =>
                have bounds : ∀ entry ∈ successors, 0 < entry.1 → ∃ horizon,
                    cumulativeOutputMeasure fuel (stateExpr entry.2) ≤ machineOutput horizon entry.2 := by
                  intro entry member positive
                  exact paper_le_machine_horizon initial meaning noFailure fuel entry.2
                    (.next reachable action member positive)
                obtain ⟨horizon, bound⟩ := weightedOutput_uniform_bound successors
                  (fun after => cumulativeOutputMeasure fuel (stateExpr after)) machineOutput machineOutput_mono bounds
                exact ⟨horizon+1, by simpa [machineOutput, action, advance.2.1 fuel] using bound⟩
termination_by (fuel, bookkeepingRank state)
decreasing_by all_goals omega

theorem execution_output_eq (initial : State)
    (meaning : ∀ state, MachineReachable initial state → ∀ result,
      step state = .ok result → StepMeaning state result)
    (noFailure : ∀ state, MachineReachable initial state → ∀ failure, step state ≠ .error failure)
    (state : State) (reachable : MachineReachable initial state) :
    machineOutputMeasure state = bigStepMeasure (stateExpr state) := by
  unfold machineOutputMeasure bigStepMeasure
  apply le_antisymm
  · apply iSup_le
    intro fuel
    exact (machineOutput_le_paper initial meaning fuel state reachable).trans (le_iSup (fun n => cumulativeOutputMeasure n (stateExpr state)) fuel)
  · apply iSup_le
    intro fuel
    obtain ⟨horizon, bound⟩ := paper_le_machine_horizon initial meaning noFailure fuel state reachable
    exact bound.trans (le_iSup (fun n => machineOutput n state) horizon)

end Determinize.Proof.FiniteModel
