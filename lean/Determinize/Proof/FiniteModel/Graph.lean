import Determinize.Proof.FiniteModel.Execution
import Determinize.Proof.FiniteModel.Model

namespace Determinize.Proof.FiniteModel
open Statement.Paper Statement.FiniteModel Determinize.Finite MeasureTheory

def successorWeight {α : Type} [DecidableEq α] (outcomes : List (Rat × α)) (state : α) : Rat :=
  (outcomes.map fun entry => if entry.2 = state then entry.1 else 0).sum

theorem successorWeight_nonnegative {α : Type} [DecidableEq α] (outcomes : List (Rat × α))
    (nonnegative : ∀ entry ∈ outcomes, 0 ≤ entry.1) (state : α) :
    0 ≤ successorWeight outcomes state := by
  apply List.sum_nonneg
  intro p member
  obtain ⟨entry, entryMember, rfl⟩ := List.mem_map.mp member
  split_ifs
  · exact nonnegative entry entryMember
  · rfl

theorem weightedOutput_group {α : Type} [DecidableEq α] {n : Nat}
    (states : Fin n → α) (unique : Function.Injective states)
    (outcomes : List (Rat × α)) (nonnegative : ∀ entry ∈ outcomes, 0 ≤ entry.1)
    (covered : ∀ entry ∈ outcomes, 0 < entry.1 → ∃ i, states i = entry.2)
    (output : α → Measure ℝ) :
    weightedOutput outcomes output =
      ∑ i, ENNReal.ofReal (successorWeight outcomes (states i) : ℝ) • output (states i) := by
  induction outcomes with
  | nil => simp [weightedOutput, successorWeight]
  | cons entry rest ih =>
      have restNonnegative : ∀ e ∈ rest, 0 ≤ e.1 := fun e he => nonnegative e (by simp [he])
      have restCovered : ∀ e ∈ rest, 0 < e.1 → ∃ i, states i = e.2 :=
        fun e he => covered e (by simp [he])
      have splitWeight (i : Fin n) :
          ENNReal.ofReal (successorWeight (entry :: rest) (states i) : ℝ) =
            (if entry.2 = states i then ENNReal.ofReal (entry.1 : ℝ) else 0) +
              ENNReal.ofReal (successorWeight rest (states i) : ℝ) := by
        simp only [successorWeight, List.map_cons, List.sum_cons, Rat.cast_add]
        rw [ENNReal.ofReal_add]
        · split_ifs <;> simp
        · split_ifs
          · exact_mod_cast nonnegative entry (by simp)
          · simp
        · exact_mod_cast successorWeight_nonnegative rest restNonnegative (states i)
      simp only [weightedOutput, List.map_cons, List.sum_cons, splitWeight, add_smul,
        Finset.sum_add_distrib]
      congr 1
      · by_cases positive : 0 < entry.1
        · obtain ⟨i, hi⟩ := covered entry (by simp) positive
          have indices (j : Fin n) : entry.2 = states j ↔ j = i := by
            rw [← hi]
            exact ⟨fun eq => (unique eq).symm, fun eq => eq ▸ rfl⟩
          simp [indices, hi]
        · have zero : ENNReal.ofReal (entry.1 : ℝ) = 0 :=
            ENNReal.ofReal_eq_zero.mpr (by exact_mod_cast le_of_not_gt positive)
          simp [zero]
      · exact ih restNonnegative restCovered

theorem replay_machineOutput (candidate : Candidate) {source : Checking.Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) (fuel : Nat) (i : Fin candidate.states.size) :
    (candidate.toModel valid).outputWithin fuel i = machineOutput fuel (candidate.state i) := by
  induction fuel generalizing i with
  | zero =>
      have row := (valid.2.2.2 i).2
      cases action : step (candidate.state i) with
      | error failure => simp [Candidate.RowReplays, action] at row
      | ok result =>
          cases result <;> simp only [Candidate.RowReplays, action] at row
          all_goals simp_all [Model.outputWithin, Candidate.toModel, machineOutput]
  | succ fuel ih =>
      have row := (valid.2.2.2 i).2
      cases action : step (candidate.state i) with
      | error failure => simp [Candidate.RowReplays, action] at row
      | ok result =>
          cases result with
          | returned reward =>
              simp only [Candidate.RowReplays, action] at row
              simp [Model.outputWithin, Candidate.toModel, row, machineOutput, action]
          | rejected =>
              simp only [Candidate.RowReplays, action] at row
              simp [Model.outputWithin, Candidate.toModel, row, machineOutput, action]
          | next evidence successors =>
              simp only [Candidate.RowReplays, action] at row
              rw [Model.outputWithin.eq_2 (candidate.toModel valid) i fuel]
              have kind : (candidate.toModel valid).kind i = .transient := row.1
              rw [kind]
              change (∑ j : Fin candidate.states.size,
                ENNReal.ofReal (candidate.weight i j : ℝ) • (candidate.toModel valid).outputWithin fuel j) = _
              simp only [machineOutput, action]
              simp_rw [ih, row.2.2.2]
              exact (weightedOutput_group candidate.state valid.2.1.2.2 successors
                row.2.1 row.2.2.1 (machineOutput fuel)).symm

theorem replay_machineOutputMeasure (candidate : Candidate) {source : Checking.Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) :
    (candidate.toModel valid).outputMeasure = machineOutputMeasure (initialState source subject) := by
  unfold Model.outputMeasure machineOutputMeasure
  apply iSup_congr
  intro fuel
  simpa only [replay_initial candidate valid] using
    replay_machineOutput candidate valid fuel (candidate.toModel valid).initial

end Determinize.Proof.FiniteModel
