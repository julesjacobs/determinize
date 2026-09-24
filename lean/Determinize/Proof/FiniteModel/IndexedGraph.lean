import Determinize.Proof.FiniteModel.Soundness
import Determinize.Proof.FiniteModel.IndexedReplay

namespace Determinize.Proof.FiniteModel
open Spec.Paper Spec.FiniteModel Determinize.Finite MeasureTheory

/-- Push probability mass through supplied indices. Equal state labels are allowed. -/
theorem weightedOutput_indexed {n : Nat} (states : Fin n → State) (fallback : Fin n)
    (outcomes : List (Rat × State)) (indices : Nat → Nat)
    (nonnegative : ∀ outcome ∈ outcomes, 0 ≤ outcome.1)
    (covered : ∀ k : Fin outcomes.length, 0 < outcomes[k].1 →
      ∃ bound : indices k.val < n, states ⟨indices k.val, bound⟩ = outcomes[k].2)
    (output : State → Measure ℝ) :
    weightedOutput outcomes output =
      ∑ j, ENNReal.ofReal (indexedMass outcomes indices j.val : ℝ) • output (states j) := by
  let destination (k : Nat) : Fin n := if h : indices k < n then ⟨indices k, h⟩ else fallback
  let indexed := outcomes.zipIdx.map fun (outcome, k) => (outcome.1, destination k)
  have destination_eq (outcome : Rat × State) (k : Nat)
      (member : (outcome, k) ∈ outcomes.zipIdx) (positive : 0 < outcome.1) :
      (destination k).val = indices k ∧ states (destination k) = outcome.2 := by
    have position := List.mem_zipIdx' member
    obtain ⟨bound, located⟩ := covered ⟨k, position.1⟩ (by simpa [position.2] using positive)
    constructor
    · simp [destination, bound]
    · simpa [destination, bound, position.2] using located
  have law : weightedOutput outcomes output = weightedOutput indexed (fun j => output (states j)) := by
    unfold weightedOutput indexed
    conv_lhs => rw [← List.zipIdx_map_fst 0 outcomes]
    simp only [List.map_map]
    congr 1
    apply List.map_congr_left
    intro pair member
    obtain ⟨outcome, k⟩ := pair
    by_cases positive : 0 < outcome.1
    · simp only [Function.comp_apply, (destination_eq outcome k member positive).2]
    · have zero : ENNReal.ofReal (outcome.1 : ℝ) = 0 :=
        ENNReal.ofReal_eq_zero.mpr (by exact_mod_cast le_of_not_gt positive)
      simp [zero]
  have weights (j : Fin n) : successorWeight indexed j = indexedMass outcomes indices j.val := by
    unfold successorWeight indexed indexedMass
    simp only [List.map_map]
    congr 1
    apply List.map_congr_left
    intro pair member
    obtain ⟨outcome, k⟩ := pair
    by_cases positive : 0 < outcome.1
    · simp only [Function.comp_apply, Fin.ext_iff, (destination_eq outcome k member positive).1]
    · have zero : outcome.1 = 0 := le_antisymm (le_of_not_gt positive)
        (nonnegative outcome (List.fst_mem_of_mem_zipIdx member))
      simp [zero]
  rw [law, weightedOutput_group id (fun _ _ h => h) indexed]
  · simp only [id_eq, weights]
  · intro entry member
    change entry ∈ outcomes.zipIdx.map (fun (outcome, k) => (outcome.1, destination k)) at member
    obtain ⟨pair, member, equal⟩ := List.mem_map.mp member
    rw [← equal]
    exact nonnegative pair.1 (List.fst_mem_of_mem_zipIdx member)
  · intro entry _ _
    exact ⟨entry.2, rfl⟩

theorem indexedReplay_machineOutput (candidate : Candidate) {source : Spec.Paper.Core} {subject : Subject}
    (indices : Fin candidate.states.size → Nat → Nat)
    (valid : candidate.IndexedReplayValid source subject indices) (fuel : Nat)
    (i : Fin candidate.states.size) :
    (candidate.graphModel (indexedReplay_valid candidate source subject indices valid)).outputWithin fuel i =
      machineOutput fuel (candidate.state i) := by
  let graph := indexedReplay_valid candidate source subject indices valid
  have replays := valid.2.2.2.2.2.2
  induction fuel generalizing i with
  | zero =>
      have row := replays i
      cases action : step (candidate.state i) with
      | error failure => simp [Candidate.IndexedRowReplays, action] at row
      | ok result =>
          cases result <;> simp only [Candidate.IndexedRowReplays, action] at row
          all_goals simp_all [Model.outputWithin, machineOutput]
  | succ fuel ih =>
      have row := replays i
      cases action : step (candidate.state i) with
      | error failure => simp [Candidate.IndexedRowReplays, action] at row
      | ok result =>
          cases result with
          | returned reward =>
              simp only [Candidate.IndexedRowReplays, action] at row
              simp [Model.outputWithin, row, machineOutput, action]
          | rejected =>
              simp only [Candidate.IndexedRowReplays, action] at row
              simp [Model.outputWithin, row, machineOutput, action]
          | next evidence successors =>
              simp only [Candidate.IndexedRowReplays, action] at row
              rw [Model.outputWithin.eq_2 (candidate.graphModel graph) i fuel]
              have kind : (candidate.graphModel graph).kind i = .transient := row.1
              rw [kind]
              change (∑ j, ENNReal.ofReal (candidate.weight i j : ℝ) •
                (candidate.graphModel graph).outputWithin fuel j) = _
              simp only [machineOutput, action]
              simp_rw [ih, indexedRow_weights candidate (indices i) i _ evidence successors action (replays i)]
              symm
              apply weightedOutput_indexed candidate.state i successors (indices i) row.2.1
              intro k positive
              obtain ⟨bound, located⟩ := Array.getElem?_eq_some_iff.mp (row.2.2.1 k positive).1
              exact ⟨bound, located⟩

theorem indexedReplay_matches (candidate : Candidate) {source : Spec.Paper.Core} {subject : Subject}
    (indices : Fin candidate.states.size → Nat → Nat)
    (valid : candidate.IndexedReplayValid source subject indices) :
    (candidate.graphModel (indexedReplay_valid candidate source subject indices valid)).Matches
      (subject.program source) := by
  let graph := indexedReplay_valid candidate source subject indices valid
  have initial : candidate.state (candidate.graphModel graph).initial = initialState source subject := by
    simpa [Candidate.state, Candidate.graphModel, graph.initial_lt] using valid.2.2.1
  have replays := valid.2.2.2.2.2.2
  have covered : ∀ state, MachineReachable (initialState source subject) state →
      ∃ i, candidate.state i = state := by
    intro state reachable
    induction reachable with
    | initial => exact ⟨_, initial⟩
    | next previous action member positive ih =>
        obtain ⟨i, hi⟩ := ih
        have row := replays i
        rw [← hi] at action
        simp only [Candidate.IndexedRowReplays, action] at row
        obtain ⟨k, boundK, hk⟩ := List.getElem_of_mem member
        obtain ⟨bound, located⟩ := Array.getElem?_eq_some_iff.mp
          (row.2.2.1 ⟨k, boundK⟩ (by simpa [hk] using positive)).1
        exact ⟨⟨indices i k, bound⟩, by simpa [Candidate.state, hk] using located⟩
  have noFailure : ∀ state, MachineReachable (initialState source subject) state →
      ∀ failure, step state ≠ .error failure := by
    intro state reachable failure action
    obtain ⟨i, hi⟩ := covered state reachable
    have row := replays i
    simp [Candidate.IndexedRowReplays, hi, action] at row
  have meaning : ∀ state, MachineReachable (initialState source subject) state → ∀ result,
      step state = .ok result → StepMeaning state result :=
    fun state reachable result action => stepMeaning state
      (program_reachable_shape source subject state reachable) result action
  have initialEqual : stateExpr (initialState source subject) = subject.program source :=
    initial_reification source subject
      ((Binding.scoped_map source (fun q : Rat => (q : ℝ)) 0).mpr valid.2.2.2.1)
  have outputEqual : (candidate.graphModel graph).outputMeasure =
      machineOutputMeasure (initialState source subject) := by
    unfold Model.outputMeasure machineOutputMeasure
    apply iSup_congr
    intro fuel
    simpa only [initial] using indexedReplay_machineOutput candidate indices valid fuel
      (candidate.graphModel graph).initial
  constructor
  · intro fuel
    rw [← initialEqual]
    exact execution_safe _ meaning noFailure fuel _ .initial
  · rw [outputEqual, execution_output_eq _ meaning noFailure _ .initial, initialEqual]

end Determinize.Proof.FiniteModel
