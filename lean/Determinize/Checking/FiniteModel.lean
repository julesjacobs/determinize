import Determinize.Proof.FiniteModel.Soundness
import Determinize.Proof.FiniteModel.ModelEquality

namespace Determinize.Checking
open Statement.FiniteModel Determinize.Finite

/-- Kernel-replayable evidence of graph validity. `replay_matches` transports
this evidence to the paper semantics. -/
def checkModelReplay (source : Core) (subject : Subject) (candidate : Candidate) :
    Option (PLift (candidate.ReplayValid source subject)) :=
  if valid : candidate.ReplayValid source subject then some ⟨valid⟩ else none

structure CheckedModel (source : Core) (subject : Subject) where
  model : Model
  correct : model.Matches (subject.program source)

def checkModel (source : Core) (subject : Subject) (candidate : Candidate) :
    Option (CheckedModel source subject) := do
  let valid ← checkModelReplay source subject candidate
  return ⟨candidate.toModel valid.down, Proof.FiniteModel.replay_matches candidate valid.down⟩

/-- Checks both graph replay and equality with the separately supplied model. -/
def checkModelCertificate (source : Core) (subject : Subject) (model : Model) (candidate : Candidate) : Bool :=
  if valid : candidate.ReplayValid source subject then decide (model = candidate.toModel valid) else false

theorem checkModelCertificate_sound : ModelCheckerSound checkModelCertificate := by
  intro source subject model candidate accepted
  unfold checkModelCertificate at accepted
  split at accepted
  · have equal := of_decide_eq_true accepted
    rw [equal]
    exact Proof.FiniteModel.replay_matches candidate ‹_›
  · contradiction

end Determinize.Checking
