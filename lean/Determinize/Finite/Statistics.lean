import Determinize.Finite.Solve
import Determinize.Proof.FiniteModel.Statistics

namespace Determinize.Finite
open Spec.FiniteModel Proof.FiniteModel

/-- Shared boundary analysis and absorption evidence for all output moments. -/
def solveStatistics (model : Model) (limits : SolveLimits := {}) :
    Except String {certificate : MomentCertificate model // certificate.Valid model} := do
  if model.size > limits.maxStates then
    throw s!"exact solver state limit exceeded ({model.size} > {limits.maxStates})"
  let boundary ← analyze model
  let stopped := cut model boundary.dead
  let paths ← findPaths stopped boundary.rank
  let mass ← solveValues (rewards stopped Moment.mass.rational) limits
  let first ← solveValues (rewards stopped Moment.first.rational) limits
  let second ← solveValues (rewards stopped Moment.second.rational) limits
  let values := fun moment => match moment with
    | .mass => mass.val | .first => first.val | .second => second.val
  return ⟨⟨boundary.dead, paths.val.rank, paths.val.next, values⟩, boundary.closed, paths.property, by
    intro moment
    cases moment
    · exact mass.property
    · exact first.property
    · exact second.property⟩

end Determinize.Finite
