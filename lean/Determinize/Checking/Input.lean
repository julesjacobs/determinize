import Determinize.Checking.Certificate

namespace Determinize.Checking
open Spec.Paper

/-- Resolved source syntax. Each sample retains its optional requested affinity. -/
inductive Input where
  | bvar (index : Nat)
  | reject
  | unit | bool (value : Bool) | real (value : Rat)
  | lam (body : Input)
  | fix (body : Input)
  | app (function argument : Input)
  | pair (left right : Input) | fst (pair : Input)
  | snd (pair : Input) | inl (value : Input)
  | inr (value : Input)
  | matchSum (scrutinee left right : Input)
  | nil | cons (head tail : Input)
  | matchList (scrutinee nilCase consCase : Input)
  | ite (condition thenBranch elseBranch : Input)
  | letE (value body : Input)
  | neg (body : Input)
  | add (left right : Input) | mul (left right : Input)
  | div (left right : Input) | lt (left right : Input)
  | uniform (affinity : Option Affinity) (lower upper : Input)
  | gaussian (affinity : Option Affinity) (mean variance : Input)
  | poisson (affinity : Option Affinity) (rate : Input)
  | discrete (affinity : Option Affinity) (distribution : FiniteDistribution)
  | bernoulli (affinity : Option Affinity) (probability : Input)
  | exponential (affinity : Option Affinity) (rate : Input)
  | beta (affinity : Option Affinity) (alpha beta : Input)
  | gamma (affinity : Option Affinity) (shape rate : Input)

deriving Repr, DecidableEq

end Determinize.Checking
