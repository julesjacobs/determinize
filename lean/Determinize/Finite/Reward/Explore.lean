import Determinize.Proof.RewardModel.Replay
import Determinize.Finite.Explore

namespace Determinize.Finite.Reward
open Checking Spec.FiniteModel

inductive Exploration (source : Core) (subject : Subject) where
  | complete (candidate : Candidate) (valid : candidate.ReplayValid source subject)
  | incomplete (limit : Limit) (discovered expanded edges : Nat)
  | failed (state : Nat) (failure : Failure)

private def build (source : Core) (subject : Subject) (limits : Limits)
    (fuel : Nat) (states : Array State) (indices : Std.HashMap State Nat)
    (rows : Array Row) (edgeCount : Nat) : Exploration source subject :=
  if rows.size = states.size then
    let c : Candidate := ⟨0, states, rows⟩
    if valid : c.ReplayValid source subject then .complete c valid
    else .failed rows.size (.invalid "additive graph failed local replay")
  else match fuel with
  | 0 => .incomplete .states states.size rows.size edgeCount
  | fuel + 1 => Id.run do
      let i := rows.size
      let some current := states[i]? | return .failed i (.invalid "missing additive state")
      match Reward.step current with
      | .error failure => return .failed i failure
      | .ok (.returned value) =>
          if edgeCount + 1 > limits.maxEdges then
            return .incomplete .edges states.size rows.size edgeCount
          return build source subject limits fuel states indices
            (rows.push ⟨.returned value, [⟨i, 1, 0⟩]⟩) (edgeCount + 1)
      | .ok .rejected =>
          if edgeCount + 1 > limits.maxEdges then
            return .incomplete .edges states.size rows.size edgeCount
          return build source subject limits fuel states indices
            (rows.push ⟨.rejected, [⟨i, 1, 0⟩]⟩) (edgeCount + 1)
      | .ok (.next _ outcomes) =>
          let mut states := states
          let mut indices := indices
          let mut edges := []
          for o in outcomes do
            if o.probability < 0 then return .failed i (.invalid "negative probability")
            if 0 < o.probability then
              if edgeCount + edges.length + 1 > limits.maxEdges then
                return .incomplete .edges states.size rows.size (edgeCount + edges.length)
              if (reprStr o.state).utf8ByteSize > limits.maxStateBytes then
                return .incomplete .stateBytes states.size rows.size (edgeCount + edges.length)
              let target := match indices[o.state]? with
                | some j => j
                | none => states.size
              if target = states.size then
                if states.size + 1 > limits.maxStates then
                  return .incomplete .states (states.size + 1) rows.size (edgeCount + edges.length)
                indices := indices.insert o.state target
                states := states.push o.state
              edges := ⟨target, o.probability, o.reward⟩ :: edges
          return build source subject limits fuel states indices
            (rows.push ⟨.transient, edges.reverse⟩) (edgeCount + edges.length)

def explore (source : Core) (subject : Subject := .determinized) (limits : Limits := {}) :
    Exploration source subject :=
  let root := initialState source subject
  if limits.maxStates = 0 then .incomplete .states 0 0 0
  else if ¬ Proof.FiniteModel.Binding.Scoped 0 source then
    .failed 0 (.invalid "source contains an unbound variable")
  else if (reprStr root).utf8ByteSize > limits.maxStateBytes then
    .incomplete .stateBytes 0 0 0
  else build source subject limits limits.maxStates #[root] (({} : Std.HashMap State Nat).insert root 0) #[] 0

end Determinize.Finite.Reward
