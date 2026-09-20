import Determinize.Finite.Reward.Normalize
import Determinize.Finite.Graph

namespace Determinize.Finite.Reward
open Spec.FiniteModel

structure Edge where
  target : Nat
  probability : Rat
  reward : Rat
deriving Repr, BEq, DecidableEq

structure Row where
  kind : StateKind
  edges : List Edge
deriving Repr

structure Candidate where
  initial : Nat := 0
  states : Array State
  rows : Array Row
deriving Repr

end Determinize.Finite.Reward
