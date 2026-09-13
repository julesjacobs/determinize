import Determinize.Finite.Machine
import Determinize.Spec.FiniteModel.Certificates

namespace Determinize.Finite
open Spec.FiniteModel

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

end Determinize.Finite
