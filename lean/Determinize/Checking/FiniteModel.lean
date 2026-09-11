import Determinize.Proof.FiniteModel.Soundness

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

end Determinize.Checking
