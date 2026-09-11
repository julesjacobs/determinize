import Determinize.Checking.Certificate

namespace Determinize.Frontend
open Spec.Paper Checking

inductive Surface where
  | var (name : String)
  | number (value : Rat)
  | node (tag : String) (names : List String) (affinity : Option Affinity) (args : List Surface)
deriving Repr

structure Input where
  expression : Core
  affinities : List (Option Affinity)
deriving Repr

end Determinize.Frontend
