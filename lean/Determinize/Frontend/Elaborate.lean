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
  | .node tag names affinity args => do
    match tag, names, args with
    | "lam", [x], [b] => return unary .lam (← lower (x :: env) b)
    | "fix", [f,x], [b] => return unary .fix (← lower (x :: f :: env) b)
    | "let", [x], [a,b] => return binary .letE (← lower env a) (← lower (x :: env) b)
    | "matchList", [x,xs], [e,n,c] =>
      return ternary .matchList (← lower env e) (← lower env n) (← lower (x :: xs :: env) c)
    | "matchSum", [x,y], [e,l,r] =>
      return ternary .matchSum (← lower env e) (← lower (x :: env) l) (← lower (y :: env) r)
    | "unit", _, [] => return pureInput .unit
    | "nil", _, [] => return pureInput .nil
    | "true", _, [] => return pureInput (.bool true)
    | "false", _, [] => return pureInput (.bool false)
    | "ite", _, [c,a,b] => return ternary .ite (← lower env c) (← lower env a) (← lower env b)
    | "discrete", _, _ =>
      let weights ← args.mapM fun a => match a with
        | .number q => pure q
        | _ => throw "discrete expects nonnegative literal weights"
      let distribution ← finiteDistribution weights
      return ⟨.discrete (.sample (affinity.getD .G)) distribution, [affinity]⟩
    | _, _, [a] =>
      let a ← lower env a
      match tag with
      | "neg" => return unary .neg a
      | "fst" => return unary .fst a
      | "snd" => return unary .snd a
      | "inl" => return unary .inl a
      | "inr" => return unary .inr a
      | "poisson" => return draw1 .poisson affinity a
      | "exponential" => return draw1 .exponential affinity a
      | "observe" =>
        if affinity.isSome then throw "observe has no sampling affinity"
        return ⟨.ite a.expression .unit reject, a.affinities⟩
      | "bernoulli" => return draw1 .bernoulli affinity a
      | "flip" =>
        if affinity == some .E then throw "flip produces a Boolean and requires [G]"
        let draw := draw1 .bernoulli (some .G) a
        return ⟨.lt (.real 0) draw.expression, draw.affinities⟩
      | _ => throw s!"unknown unary operation {tag}"
    | _, _, [a,b] =>
      let a ← lower env a; let b ← lower env b
      match tag with
      | "app" => return binary .app a b
      | "pair" => return binary .pair a b
      | "::" => return binary .cons a b
      | "+" => return binary .add a b
      | "-" => return binary .add a (unary .neg b)
      | "*" =>
        match a.expression, b.expression with
        | .real _, _ => return binary .mul a b
        | _, .real _ => return binary .mul b a
        | _, _ => return binary .mul a b
      | "/" => return binary .div a b
      | "<" => return binary .lt a b
      | "<=" => return ⟨.letE a.expression (.letE
          (b.expression.shift 1 0) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true))), a.affinities ++ b.affinities⟩
      | "uniform" => return draw2 .uniform affinity a b
      | "gauss" | "gaussian" => return draw2 .gaussian affinity a b
      | "beta" => return draw2 .beta affinity a b
      | "gamma" => return draw2 .gamma affinity a b
      | _ => throw s!"unknown binary operation {tag}"
    | _, _, _ => throw s!"wrong arguments for {tag}"

def elaborate (e : Surface) : Except String Input := lower [] e

end Determinize.Frontend
