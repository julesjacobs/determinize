import Determinize.Finite.Graph
import Determinize.Proof.FiniteModel.BuildModel

namespace Determinize.Finite
open Spec.FiniteModel Checking
open Spec.Paper (Core)

structure Limits where
  maxStates : Nat := 10000
  maxEdges : Nat := 100000
  maxStateBytes : Nat := 1000000
deriving Repr

inductive Limit where
  | states | edges | stateBytes
deriving Repr, BEq

instance : Hashable State := ⟨fun state => hash (reprStr state)⟩

inductive Exploration (source : Core) (subject : Subject) where
  | complete (candidate : Candidate) (valid : candidate.ReplayValid source subject)
  | incomplete (limit : Limit) (discovered expanded edges : Nat)
  | failed (state : Nat) (failure : Failure)

private def build (source : Core) (subject : Subject)
    (sourceScoped : Proof.FiniteModel.Binding.Scoped 0 source) (limits : Limits)
    (remaining : Nat) (work : Builder.Work)
    (root : work.root = initialState source subject) (edgeCount : Nat) : Exploration source subject :=
  if complete : work.rows.size = work.table.states.size then
    .complete (work.candidate complete) (work.valid complete source subject root sourceScoped)
  else match remaining with
  | 0 => .incomplete .states work.table.states.size work.rows.size edgeCount
  | remaining + 1 =>
    have pending : work.rows.size < work.table.states.size := lt_of_le_of_ne work.size_le complete
    match Builder.readAction (work.table.states[work.rows.size]'pending) with
    | .error failure => .failed work.rows.size failure
    | .ok action =>
      let outgoing := (action.successors.filterMap fun (outcome : Rat × State) => if 0 < outcome.1 then some outcome.2 else none).eraseDups.length
      if edgeCount + outgoing > limits.maxEdges then
        .incomplete .edges work.table.states.size work.rows.size edgeCount
      else if action.successors.any (fun (p,s) => 0 < p && (reprStr s).utf8ByteSize > limits.maxStateBytes) then
        .incomplete .stateBytes work.table.states.size work.rows.size edgeCount
      else
        let next := work.expand pending action
        if next.table.states.size > limits.maxStates then
          .incomplete .states next.table.states.size next.rows.size (edgeCount + outgoing)
        else build source subject sourceScoped limits remaining next root (edgeCount + outgoing)

/-- Successful exploration constructs its correspondence evidence without replaying the graph. -/
def explore (source : Core) (subject : Subject := .determinized) (limits : Limits := {}) :
    Exploration source subject :=
  if sourceScoped : Proof.FiniteModel.Binding.Scoped 0 source then
    if limits.maxStates == 0 then .incomplete .states 0 0 0
    else if (reprStr (initialState source subject)).utf8ByteSize > limits.maxStateBytes then
      .incomplete .stateBytes 0 0 0
    else build source subject sourceScoped limits limits.maxStates
      (Builder.start (initialState source subject)) rfl 0
  else .failed 0 (.invalid "source contains an unbound variable")

end Determinize.Finite
