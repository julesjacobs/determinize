import Determinize.Proof.RewardModel.Replay

namespace Determinize.Finite.Reward

def Candidate.StateReplayValid (c : Candidate) (i : Fin c.states.size) : Prop :=
  normalize c.states[i] = (0, c.states[i]) ∧ c.RowValid i

instance (c : Candidate) (i : Fin c.states.size) : Decidable (c.StateReplayValid i) :=
  inferInstanceAs (Decidable (_ ∧ _))

def Candidate.InitialValid (c : Candidate) (source : Spec.Paper.Core)
    (subject : Spec.FiniteModel.Subject) : Prop :=
  c.rows.size = c.states.size ∧ c.initial < c.states.size ∧
    c.states[c.initial]? = some (initialState source subject) ∧
      Proof.FiniteModel.Binding.Scoped 0 source

instance (c : Candidate) (source : Spec.Paper.Core) (subject : Spec.FiniteModel.Subject) :
    Decidable (c.InitialValid source subject) := inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _))

theorem Candidate.replay_of_states (c : Candidate) (source : Spec.Paper.Core)
    (subject : Spec.FiniteModel.Subject) (initial : c.InitialValid source subject)
    (states : ∀ i, c.StateReplayValid i) : c.ReplayValid source subject :=
  ⟨initial.1, initial.2.1, initial.2.2.1, initial.2.2.2,
    fun i ↦ (states i).1, fun i ↦ (states i).2⟩

end Determinize.Finite.Reward
