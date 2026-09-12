import Determinize.Frontend.Syntax
import Determinize.Checking.FiniteDistribution

namespace Determinize.Frontend
open Spec.Paper Checking

private def pureInput (e : Core) : Input := ⟨e, []⟩
private def unary (f : Core → Core) (a : Input) : Input := ⟨f a.expression, a.affinities⟩
private def binary (f : Core → Core → Core) (a b : Input) : Input :=
  ⟨f a.expression b.expression, a.affinities ++ b.affinities⟩
private def ternary (f : Core → Core → Core → Core) (a b c : Input) : Input :=
  ⟨f a.expression b.expression c.expression, a.affinities ++ b.affinities ++ c.affinities⟩
private def draw1 (f : DistributionAction → Core → Core) (m : Option Affinity) (a : Input) : Input :=
  ⟨f (.sample (m.getD .G)) a.expression, m :: a.affinities⟩
private def draw2 (f : DistributionAction → Core → Core → Core) (m : Option Affinity) (a b : Input) : Input :=
  ⟨f (.sample (m.getD .G)) a.expression b.expression, m :: (a.affinities ++ b.affinities)⟩

/-- Explicit rejection has zero output mass in the formal semantics. -/
def reject : Core := .reject

private def index (env : List String) (x : String) : Except String Nat :=
  match env with
  | [] => .error s!"unbound variable '{x}'"
  | y :: ys => if x == y then .ok 0 else (· + 1) <$> index ys x

private partial def lower (env : List String) : Surface → Except String Input
  | .var x => do pure (pureInput (.bvar (← index env x)))
  | .number q => pure (pureInput (.real q))
  | .lam x b => return unary .lam (← lower (x :: env) b)
  | .fix f x b => return unary .fix (← lower (x :: f :: env) b)
  | .letE x a b => return binary .letE (← lower env a) (← lower (x :: env) b)
  | .matchList x xs e n c =>
    return ternary .matchList (← lower env e) (← lower env n) (← lower (x :: xs :: env) c)
  | .matchSum x y e l r =>
    return ternary .matchSum (← lower env e) (← lower (x :: env) l) (← lower (y :: env) r)
  | .unit => return pureInput .unit
  | .nil => return pureInput .nil
  | .bool b => return pureInput (.bool b)
  | .ite c a b => return ternary .ite (← lower env c) (← lower env a) (← lower env b)
  | .discrete affinity args => do
    let weights ← args.mapM fun a => match a with
      | .number q => pure q
      | _ => throw "discrete expects nonnegative literal weights"
    let distribution ← finiteDistribution weights
    return ⟨.discrete (.sample (affinity.getD .G)) distribution, [affinity]⟩
  | .neg a => return unary .neg (← lower env a)
  | .fst a => return unary .fst (← lower env a)
  | .snd a => return unary .snd (← lower env a)
  | .inl a => return unary .inl (← lower env a)
  | .inr a => return unary .inr (← lower env a)
  | .poisson affinity a => return draw1 .poisson affinity (← lower env a)
  | .exponential affinity a => return draw1 .exponential affinity (← lower env a)
  | .bernoulli affinity a => return draw1 .bernoulli affinity (← lower env a)
  | .observe condition => do
    let a ← lower env condition
    return ⟨.ite a.expression .unit reject, a.affinities⟩
  | .flip affinity probability => do
    if affinity == some .E then throw "flip produces a Boolean and requires [G]"
    let draw := draw1 .bernoulli (some .G) (← lower env probability)
    return ⟨.lt (.real 0) draw.expression, draw.affinities⟩
  | .app a b => return binary .app (← lower env a) (← lower env b)
  | .pair a b => return binary .pair (← lower env a) (← lower env b)
  | .cons a b => return binary .cons (← lower env a) (← lower env b)
  | .add a b => return binary .add (← lower env a) (← lower env b)
  | .div a b => return binary .div (← lower env a) (← lower env b)
  | .lt a b => return binary .lt (← lower env a) (← lower env b)
  | .sub a b => return binary .add (← lower env a) (unary .neg (← lower env b))
  | .mul a b => do
    let a ← lower env a; let b ← lower env b
    match a.expression, b.expression with
    | .real _, _ => return binary .mul a b
    | _, .real _ => return binary .mul b a
    | _, _ => return binary .mul a b
  | .le a b => do
    let a ← lower env a; let b ← lower env b
    return ⟨.letE a.expression (.letE
      (b.expression.shift 1 0) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true))), a.affinities ++ b.affinities⟩
  | .uniform affinity a b => return draw2 .uniform affinity (← lower env a) (← lower env b)
  | .gaussian affinity a b => return draw2 .gaussian affinity (← lower env a) (← lower env b)
  | .beta affinity a b => return draw2 .beta affinity (← lower env a) (← lower env b)
  | .gamma affinity a b => return draw2 .gamma affinity (← lower env a) (← lower env b)

def elaborate (e : Surface) : Except String Input := lower [] e

end Determinize.Frontend
