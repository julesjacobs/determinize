import Mathlib.Tactic.DeriveCountable

/-! # Paper affinities and types -/

namespace Determinize.Spec.Paper

inductive Affinity where
  | E | G
deriving DecidableEq, Repr, Countable

/-- Sampling retains its E/G annotation; computing a mean needs none. -/
inductive DistributionAction where
  | sample (affinity : Affinity)
  | mean
deriving DecidableEq, Repr, Countable

def DistributionAction.isSample : DistributionAction → Bool
  | .sample _ => true
  | .mean => false

def DistributionAction.determinize : DistributionAction → DistributionAction
  | .sample .E => .mean
  | action => action

inductive Ty where
  | unit | bool | float (affinity : Affinity)
  | prod (left right : Ty)
  | sum (left right : Ty)
  | list (element : Ty)
  | arr (argument result : Ty)
deriving DecidableEq, Repr, Countable

/-- Structural subtyping: G may be used as E; function arguments are contravariant. -/
inductive Ty.Sub : Ty → Ty → Prop where
  | unit : Sub .unit .unit
  | bool : Sub .bool .bool
  | float (affinity) : Sub (.float affinity) (.float affinity)
  | general : Sub (.float .G) (.float .E)
  | prod : Sub a c → Sub b d → Sub (.prod a b) (.prod c d)
  | sum : Sub a c → Sub b d → Sub (.sum a b) (.sum c d)
  | list : Sub a b → Sub (.list a) (.list b)
  | arr : Sub c a → Sub b d → Sub (.arr a b) (.arr c d)

end Determinize.Spec.Paper
