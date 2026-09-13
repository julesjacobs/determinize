import Determinize.Proof.FiniteModel.Replay

namespace Determinize.Finite
open Spec.FiniteModel Checking
deriving instance DecidableEq for Edge

private def rowMass (candidate : Candidate) (i : Fin candidate.states.size) (target : Nat) : Rat :=
  ((candidate.row i).edges.toList.map fun edge => if edge.target = target then edge.probability else 0).sum

private def indexedMass (outcomes : List (Rat × State)) (indices : Nat → Nat) (target : Nat) : Rat :=
  (outcomes.zipIdx.map fun (outcome, k) => if indices k = target then outcome.1 else 0).sum

def Candidate.IndexedRowReplays (candidate : Candidate) (indices : Nat → Nat)
    (i : Fin candidate.states.size) : Prop :=
  match step (candidate.state i) with
  | .error _ => False
  | .ok (.returned reward) => (candidate.row i).kind = .returned reward
  | .ok .rejected => (candidate.row i).kind = .rejected
  | .ok (.next _ successors) =>
      (candidate.row i).kind = .transient ∧
      (∀ outcome ∈ successors, 0 ≤ outcome.1) ∧
      (∀ k : Fin successors.length, 0 < successors[k].1 →
        candidate.states[indices k.val]? = some successors[k].2 ∧
          ∃ edge ∈ (candidate.row i).edges.toList, edge.target = indices k.val) ∧
      (∀ edge ∈ (candidate.row i).edges.toList,
        rowMass candidate i edge.target = indexedMass successors indices edge.target)

instance (candidate : Candidate) (indices : Nat → Nat) (i : Fin candidate.states.size) :
    Decidable (candidate.IndexedRowReplays indices i) := by
  unfold Candidate.IndexedRowReplays
  split <;> infer_instance

theorem indexedRow_replays (candidate : Candidate)
    (injective : Function.Injective candidate.state) (indices : Nat → Nat)
    (i : Fin candidate.states.size) (valid : candidate.IndexedRowReplays indices i) :
    candidate.RowReplays i := by
  unfold Candidate.IndexedRowReplays at valid
  unfold Candidate.RowReplays
  split at valid <;> rename_i action
  · simp at valid
  · simpa [action] using valid
  · simpa [action] using valid
  · rename_i evidence successors
    simp only [action]
    refine ⟨valid.1, valid.2.1, ?_, ?_⟩
    · intro outcome member positive
      obtain ⟨k, boundK, hk⟩ := List.getElem_of_mem member
      have covered := valid.2.2.1 ⟨k, boundK⟩ (by simpa [hk] using positive)
      obtain ⟨bound, located⟩ := Array.getElem?_eq_some_iff.mp covered.1
      exact ⟨⟨indices k, bound⟩, by simpa [Candidate.state, hk] using located⟩
    · intro j
      have weights : candidate.weight i j = indexedMass successors indices j.val := by
        by_cases present : ∃ edge ∈ (candidate.row i).edges.toList, edge.target = j.val
        · obtain ⟨edge, member, target⟩ := present
          simpa only [rowMass, Candidate.weight, target] using valid.2.2.2 edge member
        · have leftZero : candidate.weight i j = 0 := by
            apply List.sum_eq_zero
            intro q member
            obtain ⟨edge, inEdges, rfl⟩ := List.mem_map.mp member
            have different : edge.target ≠ j.val := fun h => present ⟨edge, inEdges, h⟩
            simp [different]
          rw [leftZero]
          symm
          apply List.sum_eq_zero
          intro q member
          obtain ⟨⟨outcome, k⟩, inZip, rfl⟩ := List.mem_map.mp member
          have position := List.mem_zipIdx' inZip
          by_cases positive : 0 < outcome.1
          · obtain ⟨edge, memberEdge, target⟩ := (valid.2.2.1 ⟨k, position.1⟩
              (by simpa [position.2] using positive)).2
            have different : indices k ≠ j.val := fun h => present ⟨edge, memberEdge, target.trans h⟩
            simp [different]
          · have zero : outcome.1 = 0 := le_antisymm (le_of_not_gt positive)
              (valid.2.1 outcome (List.fst_mem_of_mem_zipIdx inZip))
            simp [zero]
      rw [weights]
      unfold indexedMass
      conv_rhs => rw [← List.zipIdx_map_fst 0 successors]
      simp only [List.map_map]
      congr 1
      apply List.map_congr_left
      intro pair member
      obtain ⟨outcome, k⟩ := pair
      have position := List.mem_zipIdx' member
      by_cases positive : 0 < outcome.1
      · have covered := valid.2.2.1 ⟨k, position.1⟩ (by simpa [position.2] using positive)
        obtain ⟨bound, located⟩ := Array.getElem?_eq_some_iff.mp covered.1
        have stateEq : candidate.state ⟨indices k, bound⟩ = outcome.2 := by
          simpa [Candidate.state, position.2] using located
        have eq : indices k = j.val ↔ outcome.2 = candidate.state j := by
          rw [← stateEq, injective.eq_iff, Fin.ext_iff]
        simp only [Function.comp_apply, eq]
      · have zero : outcome.1 = 0 := le_antisymm (le_of_not_gt positive)
          (valid.2.1 outcome (List.fst_mem_of_mem_zipIdx member))
        simp [zero]

private def valueFingerprint : Value → Nat
  | .number r => 5 * (r.num.natAbs + r.den)
  | .bool b => if b then 1 else 2
  | _ => 0

def stateFingerprint : State → Nat
  | .eval _ environment stack =>
      3 * ((environment.map valueFingerprint).sum + stack.length)
  | .deliver value stack => 3 * (valueFingerprint value + stack.length) + 1
  | .rejected => 2

def Candidate.FingerprintsValid (candidate : Candidate) (keys : Fin candidate.states.size → Nat) : Prop :=
  (∀ i, keys i = stateFingerprint (candidate.state i)) ∧
    (∀ i j, keys i = keys j → candidate.state i = candidate.state j → i = j)

instance (candidate : Candidate) (keys : Fin candidate.states.size → Nat) :
    Decidable (candidate.FingerprintsValid keys) := inferInstanceAs (Decidable (_ ∧ _))

theorem fingerprints_injective (candidate : Candidate) (keys : Fin candidate.states.size → Nat)
    (valid : candidate.FingerprintsValid keys) : Function.Injective candidate.state := by
  intro i j equal
  apply valid.2 i j _ equal
  rw [valid.1 i, valid.1 j, equal]

def Candidate.IndexedReplayValid (candidate : Candidate) (source : Core) (subject : Subject)
    (keys : Fin candidate.states.size → Nat) (indices : Fin candidate.states.size → Nat → Nat) : Prop :=
  candidate.rows.size = candidate.states.size ∧ candidate.initial < candidate.states.size ∧
    candidate.states[candidate.initial]? = some (initialState source subject) ∧
    Proof.FiniteModel.Binding.Scoped 0 source ∧ candidate.FingerprintsValid keys ∧
    candidate.MatrixValid ∧ (∀ i, candidate.EdgesValid i) ∧
    (∀ i, candidate.IndexedRowReplays (indices i) i)

instance (candidate : Candidate) (source : Core) (subject : Subject)
    (keys : Fin candidate.states.size → Nat) (indices : Fin candidate.states.size → Nat → Nat) :
    Decidable (candidate.IndexedReplayValid source subject keys indices) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _ ∧ _ ∧ _ ∧ _ ∧ _))

theorem indexedReplay_valid (candidate : Candidate) (source : Core) (subject : Subject)
    (keys : Fin candidate.states.size → Nat) (indices : Fin candidate.states.size → Nat → Nat)
    (valid : candidate.IndexedReplayValid source subject keys indices) : candidate.ReplayValid source subject := by
  obtain ⟨rows, initial, aligned, sourceScoped, fingerprints, matrix, edges, replays⟩ := valid
  have injective := fingerprints_injective candidate keys fingerprints
  exact ⟨rows, initial, ⟨aligned, sourceScoped, injective⟩, matrix, edges,
    fun i => indexedRow_replays candidate injective (indices i) i (replays i)⟩

private theorem sum_edge_weights {n : Nat} (edges : List Edge)
    (bounded : ∀ edge ∈ edges, edge.target < n) :
    (∑ j : Fin n, (edges.map fun edge => if edge.target = j.val then edge.probability else 0).sum) =
      (edges.map Edge.probability).sum := by
  induction edges with
  | nil => simp
  | cons edge edges ih =>
    have bound := bounded edge (by simp)
    have rest := ih (fun e h => bounded e (by simp [h]))
    simp only [List.map_cons, List.sum_cons, Finset.sum_add_distrib, rest]
    congr 1
    have eq (j : Fin n) : edge.target = j.val ↔ (⟨edge.target, bound⟩ : Fin n) = j := by
      constructor
      · intro h; apply Fin.ext; exact h
      · intro h; exact congrArg Fin.val h
    simp [eq]

private theorem matrix_of_sparse (candidate : Candidate)
    (edges : ∀ i, candidate.EdgesValid i)
    (normalized : ∀ i, ((candidate.row i).edges.toList.map Edge.probability).sum = 1)
    (terminal : ∀ i, (candidate.row i).kind ≠ .transient →
      (candidate.row i).edges = #[⟨i.val,1⟩]) : candidate.MatrixValid := by
  refine ⟨?_, ?_, ?_⟩
  · intro i j
    apply List.sum_nonneg
    intro q member
    obtain ⟨edge, present, rfl⟩ := List.mem_map.mp member
    split
    · exact le_of_lt ((edges i).2 edge present).2
    · exact le_rfl
  · intro i
    exact (sum_edge_weights (candidate.row i).edges.toList
      (fun edge present => ((edges i).2 edge present).1)).trans (normalized i)
  · intro i isTerminal j
    simp [Candidate.weight, terminal i isTerminal, Fin.ext_iff]

def Candidate.IndexedStateValid (candidate : Candidate)
    (keys : Fin candidate.states.size → Nat) (indices : Fin candidate.states.size → Nat → Nat)
    (i : Fin candidate.states.size) : Prop :=
  keys i = stateFingerprint (candidate.state i) ∧
    (∀ j, keys i = keys j → candidate.state i = candidate.state j → i = j) ∧
    (((candidate.row i).edges.toList.map Edge.probability).sum = 1) ∧
    ((candidate.row i).kind ≠ .transient → (candidate.row i).edges = #[⟨i.val,1⟩]) ∧
    candidate.EdgesValid i ∧ candidate.IndexedRowReplays (indices i) i

instance (candidate : Candidate) (keys : Fin candidate.states.size → Nat)
    (indices : Fin candidate.states.size → Nat → Nat) (i : Fin candidate.states.size) :
    Decidable (candidate.IndexedStateValid keys indices i) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _ ∧ _ ∧ _))

theorem indexedStates_valid (candidate : Candidate) (source : Core) (subject : Subject)
    (keys : Fin candidate.states.size → Nat) (indices : Fin candidate.states.size → Nat → Nat)
    (initial : candidate.rows.size = candidate.states.size ∧ candidate.initial < candidate.states.size ∧
      candidate.states[candidate.initial]? = some (initialState source subject) ∧
      Proof.FiniteModel.Binding.Scoped 0 source)
    (rows : ∀ i, candidate.IndexedStateValid keys indices i) :
    candidate.IndexedReplayValid source subject keys indices := by
  have edges := fun i => (rows i).2.2.2.2.1
  exact ⟨initial.1, initial.2.1, initial.2.2.1, initial.2.2.2,
    ⟨fun i => (rows i).1, fun i => (rows i).2.1⟩,
    matrix_of_sparse candidate edges (fun i => (rows i).2.2.1) (fun i => (rows i).2.2.2.1),
    edges, fun i => (rows i).2.2.2.2.2⟩

end Determinize.Finite
