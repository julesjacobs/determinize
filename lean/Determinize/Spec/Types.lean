import Mathlib.Tactic.DeriveCountable

/-! # Paper modes and types -/

namespace Determinize.Spec.Paper

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

/-- Structural subtyping: G may be used as E; function arguments are contravariant. -/
inductive Ty.Sub : Ty → Ty → Prop where
  | unit : Sub .unit .unit
  | bool : Sub .bool .bool
  | float (mode) : Sub (.float mode) (.float mode)
  | general : Sub (.float .G) (.float .E)
  | prod : Sub a c → Sub b d → Sub (.prod a b) (.prod c d)
  | sum : Sub a c → Sub b d → Sub (.sum a b) (.sum c d)
  | list : Sub a b → Sub (.list a) (.list b)
  | arr : Sub c a → Sub b d → Sub (.arr a b) (.arr c d)

end Determinize.Spec.Paper
