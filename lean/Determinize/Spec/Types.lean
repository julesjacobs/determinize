import Mathlib.Tactic.DeriveCountable

/-! # Paper affinities and types -/

namespace Determinize.Spec.Paper

/-- The paper's modes of a real: a `G` sample must remain stochastic, an `E` sample is eligible
for determinization, and the type system restricts how reals of affinity `E` are used. -/
inductive Affinity where
  | E | G
deriving DecidableEq, Repr, Countable

/-- Sampling retains its E/G annotation; computing a mean needs none. -/
inductive DistributionAction where
  | sample (affinity : Affinity)
  | mean
deriving DecidableEq, Repr, Countable

/-- Determinization of a site: an `E` sample becomes a mean; `G` samples and means are kept. -/
def DistributionAction.determinize : DistributionAction → DistributionAction
  | .sample .E => .mean
  | action => action

/-- The paper's types. `float affinity` is the type of reals of that affinity. -/
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
