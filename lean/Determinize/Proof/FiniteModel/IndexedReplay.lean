import Determinize.Proof.FiniteModel.Replay

namespace Determinize.Finite
open Spec.FiniteModel Checking
deriving instance DecidableEq for Edge

private def rowMass (candidate : Candidate) (i : Fin candidate.states.size) (target : Nat) : Rat :=
  ((candidate.row i).edges.toList.map fun edge => if edge.target = target then edge.probability else 0).sum

def indexedMass (outcomes : List (Rat × State)) (indices : Nat → Nat) (target : Nat) : Rat :=
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

theorem indexedRow_weights (candidate : Candidate) (indices : Nat → Nat)
    (i j : Fin candidate.states.size) (evidence : Evidence) (successors : List (Rat × State))
    (action : step (candidate.state i) = .ok (.next evidence successors))
    (replay : candidate.IndexedRowReplays indices i) :
    candidate.weight i j = indexedMass successors indices j.val := by
  have valid := replay
  simp only [Candidate.IndexedRowReplays, action] at valid
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

def Candidate.IndexedReplayValid (candidate : Candidate) (source : Core) (subject : Subject)
    (indices : Fin candidate.states.size → Nat → Nat) : Prop :=
  candidate.rows.size = candidate.states.size ∧ candidate.initial < candidate.states.size ∧
    candidate.states[candidate.initial]? = some (initialState source subject) ∧
    Proof.FiniteModel.Binding.Scoped 0 source ∧
    candidate.MatrixValid ∧ (∀ i, candidate.EdgesValid i) ∧
    (∀ i, candidate.IndexedRowReplays (indices i) i)

instance (candidate : Candidate) (source : Core) (subject : Subject)
    (indices : Fin candidate.states.size → Nat → Nat) :
    Decidable (candidate.IndexedReplayValid source subject indices) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _ ∧ _ ∧ _ ∧ _))

theorem indexedReplay_valid (candidate : Candidate) (source : Core) (subject : Subject)
    (indices : Fin candidate.states.size → Nat → Nat)
    (valid : candidate.IndexedReplayValid source subject indices) : candidate.GraphValid :=
  ⟨valid.2.1, valid.2.2.2.2.1, valid.2.2.2.2.2.1⟩

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
    (indices : Fin candidate.states.size → Nat → Nat) (i : Fin candidate.states.size) : Prop :=
  (((candidate.row i).edges.toList.map Edge.probability).sum = 1) ∧
    ((candidate.row i).kind ≠ .transient → (candidate.row i).edges = #[⟨i.val,1⟩]) ∧
    candidate.EdgesValid i ∧ candidate.IndexedRowReplays (indices i) i

instance (candidate : Candidate) (indices : Fin candidate.states.size → Nat → Nat)
    (i : Fin candidate.states.size) : Decidable (candidate.IndexedStateValid indices i) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _))

theorem indexedStates_valid (candidate : Candidate) (source : Core) (subject : Subject)
    (indices : Fin candidate.states.size → Nat → Nat)
    (initial : candidate.rows.size = candidate.states.size ∧ candidate.initial < candidate.states.size ∧
      candidate.states[candidate.initial]? = some (initialState source subject) ∧
      Proof.FiniteModel.Binding.Scoped 0 source)
    (rows : ∀ i, candidate.IndexedStateValid indices i) :
    candidate.IndexedReplayValid source subject indices := by
  have edges := fun i => (rows i).2.2.1
  exact ⟨initial.1, initial.2.1, initial.2.2.1, initial.2.2.2,
    matrix_of_sparse candidate edges (fun i => (rows i).1) (fun i => (rows i).2.1),
    edges, fun i => (rows i).2.2.2⟩

end Determinize.Finite
