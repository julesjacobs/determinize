import Determinize.Checking.Input

namespace Determinize.Frontend
open Spec.Paper Checking

inductive Surface where
  | var (name : String)
  | number (value : Rat)
  | unit | nil | bool (value : Bool)
  | lam (name : String) (body : Surface)
  | fix (function argument : String) (body : Surface)
  | letE (name : String) (value body : Surface)
  | matchList (head tail : String) (scrutinee nilCase consCase : Surface)
  | matchSum (leftName rightName : String) (scrutinee left right : Surface)
  | ite (condition thenBranch elseBranch : Surface)
  | neg (body : Surface)
  | fst (body : Surface) | snd (body : Surface)
  | inl (body : Surface) | inr (body : Surface)
  | app (fn arg : Surface) | pair (left right : Surface)
  | cons (head tail : Surface)
  | add (left right : Surface) | sub (left right : Surface)
  | mul (left right : Surface) | div (left right : Surface)
  | lt (left right : Surface) | le (left right : Surface)
  | uniform (affinity : Option Affinity) (lower upper : Surface)
  | gaussian (affinity : Option Affinity) (mean variance : Surface)
  | poisson (affinity : Option Affinity) (rate : Surface)
  | exponential (affinity : Option Affinity) (rate : Surface)
  | bernoulli (affinity : Option Affinity) (probability : Surface)
  | beta (affinity : Option Affinity) (alpha betaArg : Surface)
  | gamma (affinity : Option Affinity) (shape rate : Surface)
  | discrete (affinity : Option Affinity) (weights : List Surface)
  | flip (affinity : Option Affinity) (probability : Surface)
  | observe (condition : Surface)
deriving Repr

end Determinize.Frontend
