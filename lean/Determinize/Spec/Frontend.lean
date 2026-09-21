import Determinize.Spec.Syntax

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
  | discrete (affinity : Option Affinity) (probabilities : Input)
  | bernoulli (affinity : Option Affinity) (probability : Input)
  | exponential (affinity : Option Affinity) (rate : Input)
  | beta (affinity : Option Affinity) (alpha beta : Input)
  | gamma (affinity : Option Affinity) (shape rate : Input)

deriving Repr, DecidableEq

/-- The candidate preserves every constructor and payload, and fills only omitted affinities. -/
def Input.matches : Input → Expr Rat → Bool
  | .bvar index, .bvar index' => decide (index = index')
  | .reject, .reject => true
  | .unit, .unit => true
  | .bool value, .bool value' => decide (value = value')
  | .real value, .real value' => decide (value = value')
  | .nil, .nil => true
  | .lam body, .lam body' =>
      body.matches body'
  | .fix body, .fix body' =>
      body.matches body'
  | .app fn arg, .app fn' arg' =>
      fn.matches fn' && arg.matches arg'
  | .pair left right, .pair left' right' =>
      left.matches left' && right.matches right'
  | .fst body, .fst body' =>
      body.matches body'
  | .snd body, .snd body' =>
      body.matches body'
  | .inl body, .inl body' =>
      body.matches body'
  | .inr body, .inr body' =>
      body.matches body'
  | .matchSum scrutinee left right, .matchSum scrutinee' left' right' =>
      scrutinee.matches scrutinee' && left.matches left' && right.matches right'
  | .cons head tail, .cons head' tail' =>
      head.matches head' && tail.matches tail'
  | .matchList scrutinee nilCase consCase, .matchList scrutinee' nilCase' consCase' =>
      scrutinee.matches scrutinee' && nilCase.matches nilCase' && consCase.matches consCase'
  | .ite condition thenBranch elseBranch, .ite condition' thenBranch' elseBranch' =>
      condition.matches condition' && thenBranch.matches thenBranch' && elseBranch.matches elseBranch'
  | .letE value body, .letE value' body' =>
      value.matches value' && body.matches body'
  | .neg body, .neg body' =>
      body.matches body'
  | .add left right, .add left' right' =>
      left.matches left' && right.matches right'
  | .mul left right, .mul left' right' =>
      left.matches left' && right.matches right'
  | .div left right, .div left' right' =>
      left.matches left' && right.matches right'
  | .lt left right, .lt left' right' =>
      left.matches left' && right.matches right'
  | .uniform requested lower upper, .uniform (.sample actual) lower' upper' =>
      (requested.isNone || requested == some actual) && lower.matches lower' && upper.matches upper'
  | .gaussian requested mean variance, .gaussian (.sample actual) mean' variance' =>
      (requested.isNone || requested == some actual) && mean.matches mean' && variance.matches variance'
  | .poisson requested rate, .poisson (.sample actual) rate' =>
      (requested.isNone || requested == some actual) && rate.matches rate'
  | .bernoulli requested probability, .bernoulli (.sample actual) probability' =>
      (requested.isNone || requested == some actual) && probability.matches probability'
  | .exponential requested rate, .exponential (.sample actual) rate' =>
      (requested.isNone || requested == some actual) && rate.matches rate'
  | .beta requested alpha betaArg, .beta (.sample actual) alpha' betaArg' =>
      (requested.isNone || requested == some actual) && alpha.matches alpha' && betaArg.matches betaArg'
  | .gamma requested shape rate, .gamma (.sample actual) shape' rate' =>
      (requested.isNone || requested == some actual) && shape.matches shape' && rate.matches rate'
  | .discrete requested distribution, .discrete (.sample actual) distribution' =>
      (requested.isNone || requested == some actual) && distribution.matches distribution'
  | _, _ => false


end Determinize.Checking
