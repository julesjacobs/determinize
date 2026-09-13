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

/-- Finite graph data shared by verified construction and external replay. -/
structure Candidate where
  initial : Nat := 0
  states : Array State
  rows : Array Row
deriving Repr

end Determinize.Finite
