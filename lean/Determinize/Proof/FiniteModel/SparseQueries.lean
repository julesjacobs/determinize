import Determinize.Proof.FiniteModel.IndexedReplay
import Determinize.Proof.FiniteModel.Termination

namespace Determinize.Finite
open Spec.FiniteModel Proof.FiniteModel

def edgeValues {n : Nat} (edges : List Edge) (values : Fin n → Rat) : Rat :=
  (edges.map fun edge => if h : edge.target < n then edge.probability * values ⟨edge.target,h⟩ else 0).sum

private theorem sum_edge_values {n : Nat} (edges : List Edge) (values : Fin n → Rat)
    (bounded : ∀ edge ∈ edges, edge.target < n) :
    (∑ j : Fin n, (edges.map fun edge => if edge.target = j.val then edge.probability else 0).sum * values j) =
      edgeValues edges values := by
  induction edges with
  | nil => simp [edgeValues]
  | cons edge edges ih =>
    have bound := bounded edge (by simp)
    have rest := ih (fun e h => bounded e (by simp [h]))
    simp only [List.map_cons, List.sum_cons, add_mul, Finset.sum_add_distrib, rest,
      edgeValues, List.map_cons, List.sum_cons, dif_pos bound]
    congr 1
    have eq (j : Fin n) : edge.target = j.val ↔ (⟨edge.target, bound⟩ : Fin n) = j := by
      constructor
      · intro h; apply Fin.ext; exact h
      · intro h; exact congrArg Fin.val h
    simp only [ite_mul, zero_mul, eq]
    simp

theorem Candidate.weighted_sum (candidate : Candidate) (i : Fin candidate.states.size)
    (edges : candidate.EdgesValid i) (values : Fin candidate.states.size → Rat) :
    (∑ j, candidate.weight i j * values j) = edgeValues (candidate.row i).edges.toList values :=
  sum_edge_values _ _ (fun edge present => (edges.2 edge present).1)

private def sparseEquation (candidate : Candidate) (kind : Fin candidate.states.size → StateKind)
    (values : Fin candidate.states.size → Rat) (state : Fin candidate.states.size) : Prop :=
  values state = match kind state with
    | .returned r => r | .rejected => 0
    | .transient => edgeValues (candidate.row state).edges.toList values

private instance (candidate : Candidate) (kind : Fin candidate.states.size → StateKind)
    (values : Fin candidate.states.size → Rat) (state : Fin candidate.states.size) :
    Decidable (sparseEquation candidate kind values state) := inferInstanceAs (Decidable (_ = _))

def Candidate.QueryStateValid (candidate : Candidate)
    (valid : candidate.GraphValid) (certificate : TerminationCertificate (candidate.graphModel valid))
    (state : Fin candidate.states.size) : Prop :=
  (certificate.output.dead state = true → (candidate.row state).kind = .transient ∧
    ∀ next, 0 < candidate.weight state next → certificate.output.dead next = true) ∧
  ((cut (candidate.graphModel valid) certificate.output.dead).kind state = .transient →
    0 < candidate.weight state (certificate.output.next state) ∧
      certificate.output.rank (certificate.output.next state) < certificate.output.rank state) ∧
  (∀ moment, sparseEquation candidate (certificate.output.model (candidate.graphModel valid) moment).kind
    (certificate.output.values moment) state) ∧
  sparseEquation candidate (rejectionQuery (candidate.graphModel valid) certificate.output.dead).kind
    certificate.rejection state

instance (candidate : Candidate)
    (valid : candidate.GraphValid) (certificate : TerminationCertificate (candidate.graphModel valid))
    (state : Fin candidate.states.size) : Decidable (candidate.QueryStateValid valid certificate state) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _))

theorem sparseResults_valid (candidate : Candidate)
    (valid : candidate.GraphValid) (certificate : TerminationCertificate (candidate.graphModel valid))
    (states : ∀ state, candidate.QueryStateValid valid certificate state) : certificate.Valid (candidate.graphModel valid) := by
  apply terminationStates_valid
  intro state
  refine ⟨(states state).1, (states state).2.1, ?_, ?_⟩
  · intro moment
    change certificate.output.values moment state = match (certificate.output.model (candidate.graphModel valid) moment).kind state with
      | .returned r => r | .rejected => 0
      | .transient => ∑ next, candidate.weight state next * certificate.output.values moment next
    rw [candidate.weighted_sum state (valid.edges state)]
    exact (states state).2.2.1 moment
  · change certificate.rejection state = match (rejectionQuery (candidate.graphModel valid) certificate.output.dead).kind state with
      | .returned r => r | .rejected => 0
      | .transient => ∑ next, candidate.weight state next * certificate.rejection next
    rw [candidate.weighted_sum state (valid.edges state)]
    exact (states state).2.2.2

end Determinize.Finite
