import Determinize.Finite.Explore
import Determinize.Proof.FiniteModel.Equality
import Determinize.Proof.FiniteModel.Substitution

/-!
Internal graph invariants for the executable machine. These are intermediate
proof obligations; they do not establish the paper's `Model.Matches` contract.
-/

namespace Determinize.Finite
open Statement.Paper Statement.FiniteModel Checking

def Candidate.state (candidate : Candidate) (i : Fin candidate.states.size) : State :=
  candidate.states[i]

def Candidate.row (candidate : Candidate) (i : Fin candidate.states.size) : Row :=
  candidate.rows[i.val]?.getD ⟨.rejected, .rejected, #[]⟩

def Candidate.weight (candidate : Candidate) (i j : Fin candidate.states.size) : Rat :=
  ((candidate.row i).edges.toList.map fun edge =>
    if edge.target = j.val then edge.probability else 0).sum

/-- Local replay checks the entire probability law, including successors with
positive mass that are absent from the supplied graph. -/
def Candidate.RowReplays (candidate : Candidate) (i : Fin candidate.states.size) : Prop :=
  match step (candidate.state i) with
  | .error _ => False
  | .ok (.returned reward) =>
      (candidate.row i).kind = .returned reward ∧ (candidate.row i).evidence = .returned
  | .ok .rejected =>
      (candidate.row i).kind = .rejected ∧ (candidate.row i).evidence = .rejected
  | .ok (.next evidence successors) =>
      (candidate.row i).kind = .transient ∧
      (candidate.row i).evidence = evidence ∧
      (∀ outcome ∈ successors, 0 ≤ outcome.1) ∧
      (∀ outcome ∈ successors, 0 < outcome.1 →
        ∃ j : Fin candidate.states.size, candidate.state j = outcome.2) ∧
      (∀ j : Fin candidate.states.size, candidate.weight i j =
        (successors.map fun outcome =>
          if outcome.2 = candidate.state j then outcome.1 else 0).sum)

instance (candidate : Candidate) (i : Fin candidate.states.size) :
    Decidable (candidate.RowReplays i) := by
  unfold Candidate.RowReplays
  split
  · infer_instance
  · infer_instance
  · infer_instance
  · infer_instance

def Candidate.MatrixValid (candidate : Candidate) : Prop :=
  (∀ i j, 0 ≤ candidate.weight i j) ∧
  (∀ i, ∑ j, candidate.weight i j = 1) ∧
  (∀ i, (candidate.row i).kind ≠ .transient →
    ∀ j, candidate.weight i j = if i = j then 1 else 0)

instance (candidate : Candidate) : Decidable candidate.MatrixValid :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _))

/-- Sparse representation checks additionally reject out-of-range, duplicate,
and nonpositive edges, even when they cancel in the induced matrix. -/
def Candidate.EdgesValid (candidate : Candidate) (i : Fin candidate.states.size) : Prop :=
  ((candidate.row i).edges.toList.map Edge.target).Nodup ∧
  ∀ edge ∈ (candidate.row i).edges.toList,
    edge.target < candidate.states.size ∧ 0 < edge.probability

instance (candidate : Candidate) (i : Fin candidate.states.size) :
    Decidable (candidate.EdgesValid i) := inferInstanceAs (Decidable (_ ∧ _))

def initialState (source : Core) (subject : Subject) : State :=
  .eval (match subject with | .source => source | .determinized => source.determinize) [] []

def Candidate.Aligned (candidate : Candidate) (source : Core) (subject : Subject) : Prop :=
  candidate.source = source ∧ candidate.subject = subject ∧
    candidate.states[candidate.initial]? = some (initialState source subject) ∧
    Determinize.Proof.FiniteModel.Binding.Scoped 0 source ∧
    Function.Injective candidate.state

instance (candidate : Candidate) (source : Core) (subject : Subject) :
    Decidable (candidate.Aligned source subject) := inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _ ∧ ∀ _ _, _))

def Candidate.ReplayValid (candidate : Candidate) (source : Core) (subject : Subject) : Prop :=
  (candidate.rows.size = candidate.states.size ∧ candidate.initial < candidate.states.size) ∧
  candidate.Aligned source subject ∧ candidate.MatrixValid ∧
  ∀ i, candidate.EdgesValid i ∧ candidate.RowReplays i

instance (candidate : Candidate) (source : Core) (subject : Subject) :
    Decidable (candidate.ReplayValid source subject) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _))

/-- This constructs a finite rational model, but supplies no `Matches` proof. -/
def Candidate.toModel (candidate : Candidate) {source : Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) : Model where
  size := candidate.states.size
  initial := ⟨candidate.initial, valid.1.2⟩
  kind := fun i => (candidate.row i).kind
  transition := candidate.weight
  nonnegative := valid.2.2.1.1
  normalized := valid.2.2.1.2.1
  absorbing := valid.2.2.1.2.2

theorem replay_successor_covered (candidate : Candidate) {source : Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) (i : Fin candidate.states.size)
    (evidence : Evidence) (successors : List (Rat × State))
    (action : step (candidate.state i) = .ok (.next evidence successors))
    (outcome : Rat × State) (member : outcome ∈ successors) (positive : 0 < outcome.1) :
    ∃ j : Fin candidate.states.size, candidate.state j = outcome.2 := by
  have localValid := (valid.2.2.2 i).2
  simp only [Candidate.RowReplays, action] at localValid
  exact localValid.2.2.2.1 outcome member positive

theorem replay_transition_weight (candidate : Candidate) {source : Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) (i j : Fin candidate.states.size)
    (evidence : Evidence) (successors : List (Rat × State))
    (action : step (candidate.state i) = .ok (.next evidence successors)) :
    (candidate.toModel valid).transition i j =
      (successors.map fun outcome =>
        if outcome.2 = candidate.state j then outcome.1 else 0).sum := by
  have localValid := (valid.2.2.2 i).2
  simp only [Candidate.RowReplays, action] at localValid
  exact localValid.2.2.2.2 j

/-- Reachability along positive-probability transitions of the executable machine. -/
inductive MachineReachable (initial : State) : State → Prop where
  | initial : MachineReachable initial initial
  | next {before after : State} {probability : Rat} {evidence : Evidence}
      {successors : List (Rat × State)} :
      MachineReachable initial before →
      step before = .ok (.next evidence successors) →
      (probability, after) ∈ successors → 0 < probability →
      MachineReachable initial after

theorem replay_initial (candidate : Candidate) {source : Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) :
    candidate.state (candidate.toModel valid).initial = initialState source subject := by
  have aligned := valid.2.1.2.2.1
  simpa [valid.1.2, Candidate.state, Candidate.toModel] using aligned

/-- Coverage holds for paths of arbitrary length, not just the explorer's horizon. -/
theorem replay_reachable_covered (candidate : Candidate) {source : Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) {state : State}
    (reachable : MachineReachable (initialState source subject) state) :
    ∃ i : Fin candidate.states.size, candidate.state i = state := by
  induction reachable with
  | initial => exact ⟨(candidate.toModel valid).initial, replay_initial candidate valid⟩
  | next previous action member positive ih =>
      obtain ⟨i, hi⟩ := ih
      exact replay_successor_covered candidate valid i _ _ (hi ▸ action) _ member positive

theorem replay_no_failure (candidate : Candidate) {source : Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) (i : Fin candidate.states.size)
    (failure : Failure) : step (candidate.state i) ≠ .error failure := by
  intro action
  have localValid := (valid.2.2.2 i).2
  simp [Candidate.RowReplays, action] at localValid

theorem replay_reachable_no_failure (candidate : Candidate) {source : Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) {state : State}
    (reachable : MachineReachable (initialState source subject) state) (failure : Failure) :
    step state ≠ .error failure := by
  obtain ⟨i, hi⟩ := replay_reachable_covered candidate valid reachable
  rw [← hi]
  exact replay_no_failure candidate valid i failure

end Determinize.Finite
