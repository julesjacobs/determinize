import Determinize.Frontend.Syntax
import Determinize.Frontend.Shift
import Determinize.Checking.FiniteDistribution

namespace Determinize.Frontend
open Spec.Paper Checking

private def index (env : List String) (x : String) : Except String Nat :=
  match env with
  | [] => .error s!"unbound variable '{x}'"
  | y :: ys => if x == y then .ok 0 else (· + 1) <$> index ys x

private def lower (env : List String) : Surface → Except String Input
  | .var x => do pure (.bvar (← index env x))
  | .number q => pure (.real q)
  | .lam x b => return .lam (← lower (x :: env) b)
  | .fix f x b => return .fix (← lower (x :: f :: env) b)
  | .letE x a b => return .letE (← lower env a) (← lower (x :: env) b)
  | .matchList x xs e n c =>
    return .matchList (← lower env e) (← lower env n) (← lower (x :: xs :: env) c)
  | .matchSum x y e l r =>
    return .matchSum (← lower env e) (← lower (x :: env) l) (← lower (y :: env) r)
  | .unit => return .unit
  | .nil => return .nil
  | .bool b => return (.bool b)
  | .ite c a b => return .ite (← lower env c) (← lower env a) (← lower env b)
  | .discrete affinity args => do
    let weights ← args.mapM fun a => match a with
      | .number q => pure q
      | _ => throw "discrete expects nonnegative literal weights"
    let distribution ← finiteDistribution weights
    return .discrete affinity distribution
  | .neg a => return .neg (← lower env a)
  | .fst a => return .fst (← lower env a)
  | .snd a => return .snd (← lower env a)
  | .inl a => return .inl (← lower env a)
  | .inr a => return .inr (← lower env a)
  | .poisson affinity a => return .poisson affinity (← lower env a)
  | .exponential affinity a => return .exponential affinity (← lower env a)
  | .bernoulli affinity a => return .bernoulli affinity (← lower env a)
  | .observe condition => do
    let a ← lower env condition
    return .ite a .unit .reject
  | .flip affinity probability => do
    if affinity == some .E then throw "flip produces a Boolean and requires [G]"
    let draw : Input := .bernoulli (some .G) (← lower env probability)
    return .lt (.real 0) draw
  | .app a b => return .app (← lower env a) (← lower env b)
  | .pair a b => return .pair (← lower env a) (← lower env b)
  | .cons a b => return .cons (← lower env a) (← lower env b)
  | .add a b => return .add (← lower env a) (← lower env b)
  | .div a b => return .div (← lower env a) (← lower env b)
  | .lt a b => return .lt (← lower env a) (← lower env b)
  | .sub a b => return .add (← lower env a) (.neg (← lower env b))
  | .mul a b => do
    let a ← lower env a; let b ← lower env b
    match a, b with
    | .real _, _ => return .mul a b
    | _, .real _ => return .mul b a
    | _, _ => return .mul a b
  | .le a b => do
    let a ← lower env a; let b ← lower env b
    return .letE a (.letE
      (b.shift 1 0) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))
  | .uniform affinity a b => return .uniform affinity (← lower env a) (← lower env b)
  | .gaussian affinity a b => return .gaussian affinity (← lower env a) (← lower env b)
  | .beta affinity a b => return .beta affinity (← lower env a) (← lower env b)
  | .gamma affinity a b => return .gamma affinity (← lower env a) (← lower env b)

def elaborate (e : Surface) : Except String Input := lower [] e

end Determinize.Frontend
