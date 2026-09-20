import Determinize.Finite.Reward.Graph
import Determinize.Proof.FiniteModel.Replay
import Determinize.Spec.RewardModel.Model

namespace Determinize.Finite.Reward
open Spec.FiniteModel Checking

def Candidate.row (c : Candidate) (i : Fin c.states.size) : Row :=
  c.rows[i.val]?.getD ⟨.rejected, []⟩

def Candidate.RowValid (c : Candidate) (i : Fin c.states.size) : Prop :=
  (∀ e ∈ (c.row i).edges, e.target < c.states.size ∧ 0 < e.probability) ∧
  ((c.row i).edges.map Edge.probability).sum = 1 ∧
  match Reward.step c.states[i] with
  | .error _ => False
  | .ok (.returned b) => (c.row i).kind = .returned b
  | .ok .rejected => (c.row i).kind = .rejected
  | .ok (.next _ outcomes) =>
      (c.row i).kind = .transient ∧
      (∀ o ∈ outcomes, 0 ≤ o.probability) ∧
      ((c.row i).edges.map fun e =>
        (e.probability, c.states[e.target]?.getD .rejected, e.reward)) =
      ((outcomes.filter fun o => 0 < o.probability).map fun o =>
        (o.probability, o.state, o.reward))

instance (c : Candidate) (i : Fin c.states.size) : Decidable (c.RowValid i) := by
  unfold Candidate.RowValid
  split <;> infer_instance

/-- This checks local normalized-machine replay, not source-semantic correspondence. -/
def Candidate.ReplayValid (c : Candidate) (source : Core) (subject : Subject) : Prop :=
  c.rows.size = c.states.size ∧
  c.initial < c.states.size ∧
  c.states[c.initial]? = some (Finite.initialState source subject) ∧
  Proof.FiniteModel.Binding.Scoped 0 source ∧
  (∀ i : Fin c.states.size, Reward.normalize c.states[i] = (0, c.states[i])) ∧
  (∀ i : Fin c.states.size, c.RowValid i)

instance (c : Candidate) (source : Core) (subject : Subject) :
    Decidable (c.ReplayValid source subject) := inferInstanceAs (Decidable (_ ∧ _))

abbrev Candidate.modelEdges (c : Candidate) {source : Core} {subject : Subject}
    (valid : c.ReplayValid source subject) (i : Fin c.states.size) :
    List (Spec.RewardModel.Edge c.states.size) :=
  (c.row i).edges.attach.map fun ⟨e, he⟩ =>
    ⟨⟨e.target, ((valid.2.2.2.2.2 i).1 e he).1⟩, e.probability, e.reward⟩

abbrev Candidate.toModel (c : Candidate) {source : Core} {subject : Subject}
    (valid : c.ReplayValid source subject) : Spec.RewardModel.Model where
  size := c.states.size
  initial := ⟨c.initial, valid.2.1⟩
  kind := fun i => (c.row i).kind
  edges := c.modelEdges valid
  nonnegative := by
    intro i e he
    obtain ⟨⟨original, member⟩, _, rfl⟩ := List.mem_map.mp he
    exact le_of_lt ((valid.2.2.2.2.2 i).1 original member).2
  normalized := by
    intro i
    simpa [Candidate.modelEdges, List.map_map] using (valid.2.2.2.2.2 i).2.1

end Determinize.Finite.Reward
