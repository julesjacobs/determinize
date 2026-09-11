import Determinize.Proof.FiniteModel.Continuation
import Determinize.Proof.FiniteModel.Graph
import Determinize.Proof.FiniteModel.Initial

namespace Determinize.Proof.FiniteModel
open Spec.Paper Spec.FiniteModel Determinize.Finite

theorem replay_matches (candidate : Candidate) {source : Checking.Core} {subject : Subject}
    (valid : candidate.ReplayValid source subject) :
    (candidate.toModel valid).Matches (subject.program source) := by
  have meaning : ∀ state, MachineReachable (initialState source subject) state → ∀ result,
      step state = .ok result → StepMeaning state result :=
    fun state reachable result action => stepMeaning state
      (program_reachable_shape source subject state reachable) result action
  have noFailure := fun state reachable failure => replay_reachable_no_failure candidate valid
    (state := state) reachable failure
  have initialEqual : stateExpr (initialState source subject) = subject.program source := by
    simpa only [replay_initial candidate valid] using replay_initial_reification source subject candidate valid
  constructor
  · intro fuel
    rw [← initialEqual]
    exact execution_safe _ meaning noFailure fuel _ .initial
  · rw [replay_machineOutputMeasure candidate valid, execution_output_eq _ meaning noFailure _ .initial,
      initialEqual]

end Determinize.Proof.FiniteModel
