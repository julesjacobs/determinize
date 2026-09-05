import Mathlib.Tactic.DeriveCountable

/-! # Paper modes and types -/

namespace Determinize.Statement.Paper

inductive Mode where
  | E | G
deriving DecidableEq, Repr, Countable

inductive Ty where
  | unit | bool | float (mode : Mode)
  | prod (left right : Ty)
  | sum (left right : Ty)
  | list (element : Ty)
  | arr (argument result : Ty)
deriving DecidableEq, Repr, Countable

end Determinize.Statement.Paper
