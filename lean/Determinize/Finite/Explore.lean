import Determinize.Finite.Machine
import Determinize.Spec.FiniteModel.Certificates
import Std.Data.HashMap

namespace Determinize.Finite
open Spec.Paper Spec.FiniteModel Checking

structure Edge where
  target : Nat
  probability : Rat
deriving Repr, BEq

structure Row where
  kind : StateKind
  edges : Array Edge
deriving Repr

/-- Complete exploration data awaiting the independent model checker. -/
structure Candidate where
  initial : Nat := 0
  states : Array State
  rows : Array Row
deriving Repr

structure Limits where
  maxStates : Nat := 10000
  maxEdges : Nat := 100000
  maxStateBytes : Nat := 1000000
deriving Repr

inductive Limit where
  | states | edges | stateBytes
deriving Repr, BEq

inductive Exploration where
  | complete (candidate : Candidate)
  | incomplete (limit : Limit) (discovered expanded edges : Nat)
  | failed (state : Nat) (failure : Failure)
deriving Repr

private def insertSuccessor (probability : Rat) (state : State) :
    List (Rat × State) → List (Rat × State)
  | [] => [(probability,state)]
  | (p,s) :: rest =>
      if s == state then (p+probability,s) :: rest
      else (p,s) :: insertSuccessor probability state rest

/-- Preserve first-discovery order, combine equal successors, and remove zero edges. -/
def aggregate (successors : List (Rat × State)) : List (Rat × State) :=
  successors.foldl (fun accumulated (p,s) =>
    if p == 0 then accumulated else insertSuccessor p s accumulated) []

instance : Hashable State := ⟨fun state => hash (reprStr state)⟩

/-- Breadth-first exploration. Limits never produce a `Candidate`. Hash collisions
are resolved by structural equality; rational values retain exact normalized form. -/
def explore (source : Core) (subject : Subject := .determinized) (limits : Limits := {}) : Exploration := Id.run do
  if limits.maxStates == 0 then return .incomplete .states 0 0 0
  let program := match subject with | .source => source | .determinized => source.determinize
  let initial := State.eval program [] []
  if (reprStr initial).utf8ByteSize > limits.maxStateBytes then return .incomplete .stateBytes 0 0 0
  let mut states := #[initial]
  let mut indices : Std.HashMap State Nat := ({} : Std.HashMap State Nat).insert initial 0
  let mut rows : Array Row := #[]
  let mut edgeCount := 0
  let mut cursor := 0
  while cursor < states.size do
    let state := states[cursor]!
    let action := step state
    if let .error failure := action then return .failed cursor failure
    let .ok action := action | unreachable!
    let (kind, successors) := match action with
      | .returned reward => (StateKind.returned reward, [(1,state)])
      | .rejected => (.rejected, [(1,state)])
      | .next _ successors => (.transient, successors)
    if successors.any (fun (p,_) => p < 0) then
      return .failed cursor (.invalid "negative transition probability")
    let successors := aggregate successors
    if (successors.map Prod.fst).sum != 1 then
      return .failed cursor (.invalid "transition probabilities do not sum to one")
    let mut edges : Array Edge := #[]
    for (probability,next) in successors do
      if edgeCount ≥ limits.maxEdges then return .incomplete .edges states.size rows.size edgeCount
      let mut target := 0
      match indices[next]? with
      | some index => target := index
      | none =>
          if states.size ≥ limits.maxStates then return .incomplete .states states.size rows.size edgeCount
          if (reprStr next).utf8ByteSize > limits.maxStateBytes then
            return .incomplete .stateBytes states.size rows.size edgeCount
          target := states.size
          states := states.push next
          indices := indices.insert next target
      edges := edges.push ⟨target,probability⟩
      edgeCount := edgeCount + 1
    rows := rows.push ⟨kind,edges⟩
    cursor := cursor + 1
  return .complete ⟨0,states,rows⟩

end Determinize.Finite
