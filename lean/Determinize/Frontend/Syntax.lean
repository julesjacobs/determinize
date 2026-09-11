import Determinize.Checking.Certificate

namespace Determinize.Frontend
open Spec.Paper Checking

inductive Surface where
  | var (name : String)
  | number (value : Rat)
  | node (tag : String) (names : List String) (mode : Option Mode) (args : List Surface)
deriving Repr

structure Input where
  expression : Core
  modes : List (Option Mode)
deriving Repr

end Determinize.Frontend
