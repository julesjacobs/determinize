import Determinize.Proof.FiniteModel.Build
import Determinize.Proof.FiniteModel.SparseRow

namespace Determinize.Finite.Builder
open Spec.FiniteModel Checking
open Spec.Paper (Core)
variable [Hashable State]

abbrev Work.record (work : Work) (complete : work.rows.size = work.table.states.size)
    (i : Fin work.table.states.size) : Record := work.rows[i.val]'(by omega)

theorem Work.record_state (work : Work) (complete : work.rows.size = work.table.states.size)
    (i : Fin work.table.states.size) : (work.record complete i).state = work.table.states[i.val] := by
  have h := work.aligned (⟨i.val, by omega⟩ : Fin work.rows.size)
  simpa using h.symm

def Work.weight (work : Work) (complete : work.rows.size = work.table.states.size)
    (i j : Fin work.table.states.size) : Rat :=
  ((work.record complete i).action.successors.map fun outcome =>
    if outcome.2 = work.table.states[j.val] then outcome.1 else 0).sum

theorem Work.weight_nonnegative (work : Work) (complete : work.rows.size = work.table.states.size)
    (i j : Fin work.table.states.size) : 0 ≤ work.weight complete i j := by
  apply List.sum_nonneg
  intro q member
  obtain ⟨outcome, inOutcomes, rfl⟩ := List.mem_map.mp member
  split
  · exact (work.record complete i).action.nonnegative outcome inOutcomes
  · exact le_rfl

theorem Work.weight_normalized (work : Work) (complete : work.rows.size = work.table.states.size)
    (i : Fin work.table.states.size) : ∑ j, work.weight complete i j = 1 := by
  simp only [Work.weight]
  rw [sum_list_comm]
  trans ((work.record complete i).action.successors.map Prod.fst).sum
  · congr 1
    apply List.map_congr_left
    intro outcome member
    by_cases zero : outcome.1 = 0
    · simp [zero]
    · have positive : 0 < outcome.1 := lt_of_le_of_ne
        ((work.record complete i).action.nonnegative outcome member) (Ne.symm zero)
      obtain ⟨j, hj⟩ := work.closed _ (Array.getElem_mem _) outcome member positive
      have only (k : Fin work.table.states.size) : outcome.2 = work.table.states[k.val] ↔ j = k := by
        rw [← hj]
        exact work.table.injective.eq_iff
      simp [only]
  · exact (work.record complete i).action.normalized

abbrev Work.candidate (work : Work) (complete : work.rows.size = work.table.states.size) : Candidate where
  initial := 0
  states := work.table.states
  rows := Array.ofFn fun i : Fin work.table.states.size =>
    ⟨(work.record complete i).action.kind, sparseEdges (work.weight complete i)⟩

@[simp] theorem Work.candidate_row (work : Work) (complete : work.rows.size = work.table.states.size)
    (i : Fin work.table.states.size) : (work.candidate complete).row i =
      ⟨(work.record complete i).action.kind, sparseEdges (work.weight complete i)⟩ := by
  simp [Candidate.row, Work.candidate, i.isLt]

@[simp] theorem Work.candidate_weight (work : Work) (complete : work.rows.size = work.table.states.size)
    (i j : Fin work.table.states.size) : (work.candidate complete).weight i j = work.weight complete i j := by
  simp only [Candidate.weight, work.candidate_row]
  exact sparseEdges_weight _ (work.weight_nonnegative complete i) j

theorem Work.replays (work : Work) (complete : work.rows.size = work.table.states.size)
    (i : Fin work.table.states.size) : (work.candidate complete).RowReplays i := by
  have correct := (work.record complete i).action.correct
  simp only [work.record_state complete i] at correct
  change match step work.table.states[i.val] with
    | .error _ => False
    | .ok (.returned r) => _
    | .ok .rejected => _
    | .ok (.next _ _) => _
  cases h : step work.table.states[i.val] with
  | error failure => simp [h] at correct
  | ok action => cases action with
    | returned reward => simp only [h] at correct; simpa [h] using correct.1
    | rejected => simp only [h] at correct; simpa [h] using correct.1
    | next evidence outcomes =>
      simp only [h] at correct
      refine ⟨by simpa using correct.1, ?_, ?_, ?_⟩
      · rw [← correct.2]; exact (work.record complete i).action.nonnegative
      · intro outcome member positive
        exact work.closed _ (Array.getElem_mem _) outcome (correct.2.symm ▸ member) positive
      · intro j
        simp only [work.candidate_weight, Work.weight, correct.2]
        rfl

theorem Work.absorbing (work : Work) (complete : work.rows.size = work.table.states.size)
    (i : Fin work.table.states.size) (terminal : (work.record complete i).action.kind ≠ .transient)
    (j : Fin work.table.states.size) : work.weight complete i j = if i = j then 1 else 0 := by
  have correct := (work.record complete i).action.correct
  have outcomes : (work.record complete i).action.successors = [(1,work.table.states[i.val])] := by
    cases h : step (work.record complete i).state with
    | error failure => simp [h] at correct
    | ok action => cases action with
      | returned r => simp only [h] at correct; simpa only [work.record_state complete i] using correct.2
      | rejected => simp only [h] at correct; simpa only [work.record_state complete i] using correct.2
      | next evidence outcomes => simp [h] at correct; exact (terminal correct.1).elim
  simp only [Work.weight, outcomes, List.map_cons, List.map_nil, List.sum_cons, List.sum_nil,
    add_zero]
  have eq : work.table.states[i.val] = work.table.states[j.val] ↔ i = j := work.table.injective.eq_iff
  simp only [eq]

theorem Work.valid (work : Work) (complete : work.rows.size = work.table.states.size)
    (source : Core) (subject : Subject)
    (root : work.root = initialState source subject)
    (sourceScoped : Proof.FiniteModel.Binding.Scoped 0 source) :
    (work.candidate complete).ReplayValid source subject := by
  refine ⟨by simp, ?_, ⟨?_, sourceScoped, work.table.injective⟩,
    ⟨?_, ?_, ?_⟩, ?_, work.replays complete⟩
  · exact (Array.getElem?_eq_some_iff.mp work.initial).1
  · exact work.initial.trans (congrArg some root)
  · intro i j; simpa using work.weight_nonnegative complete i j
  · intro i; simpa using work.weight_normalized complete i
  · intro i terminal j
    simpa using work.absorbing complete i (by simpa using terminal) j
  · intro i
    simpa [Candidate.EdgesValid] using sparseEdges_valid (work.weight complete i)

end Determinize.Finite.Builder
