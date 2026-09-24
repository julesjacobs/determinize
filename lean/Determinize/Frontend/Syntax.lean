import Determinize.Spec.Frontend

/-!
# Front-end syntax

`Surface` is what the parser produces, with names and surface-only constructs; `elaborate`
resolves it to an `Input`. The rest of the file lists the sites of a program and decides
`Expr.Sitewise`, so that `Input.matches` can be evaluated.
-/

namespace Determinize.Frontend
open Spec.Paper

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
  | discreteRemainder (affinity : Option Affinity) (probabilities : Surface)
  | flip (affinity : Option Affinity) (probability : Surface)
  | observe (condition : Surface)
deriving Repr

end Determinize.Frontend

namespace Determinize.Spec.Paper.Expr

/-- The sites of a program, in syntax order. -/
def sites {Literal Site : Type} : Expr Literal Site → List Site
  | .bvar _ | .reject | .unit | .bool _ | .real _ | .nil => []
  | .lam a | .fix a | .fst a | .snd a | .inl a | .inr a | .neg a => a.sites
  | .app a b | .pair a b | .cons a b | .letE a b | .add a b | .mul a b | .div a b | .lt a b =>
      a.sites ++ b.sites
  | .matchSum a b c | .matchList a b c | .ite a b c => a.sites ++ b.sites ++ c.sites
  | .poisson s a | .discrete s a | .bernoulli s a | .exponential s a => s :: a.sites
  | .uniform s a b | .gaussian s a b | .beta s a b | .gamma s a b => s :: (a.sites ++ b.sites)

/-- `Sitewise` for a Boolean relation between sites. -/
def checkSitewise {Literal Site Site' : Type} [DecidableEq Literal] (r : Site → Site' → Bool) :
    Expr Literal Site → Expr Literal Site' → Bool
  | .bvar index, .bvar index' => index == index'
  | .bool value, .bool value' => value == value'
  | .real value, .real value' => decide (value = value')
  | .reject, .reject | .unit, .unit | .nil, .nil => true
  | .lam a, .lam a' | .fix a, .fix a' | .fst a, .fst a' | .snd a, .snd a' | .inl a, .inl a'
  | .inr a, .inr a' | .neg a, .neg a' =>
      checkSitewise r a a'
  | .app a b, .app a' b' | .pair a b, .pair a' b' | .cons a b, .cons a' b'
  | .letE a b, .letE a' b' | .add a b, .add a' b' | .mul a b, .mul a' b'
  | .div a b, .div a' b' | .lt a b, .lt a' b' =>
      checkSitewise r a a' && checkSitewise r b b'
  | .matchSum a b c, .matchSum a' b' c' | .matchList a b c, .matchList a' b' c'
  | .ite a b c, .ite a' b' c' =>
      checkSitewise r a a' && checkSitewise r b b' && checkSitewise r c c'
  | .poisson s a, .poisson s' a' | .discrete s a, .discrete s' a'
  | .bernoulli s a, .bernoulli s' a' | .exponential s a, .exponential s' a' =>
      r s s' && checkSitewise r a a'
  | .uniform s a b, .uniform s' a' b' | .gaussian s a b, .gaussian s' a' b'
  | .beta s a b, .beta s' a' b' | .gamma s a b, .gamma s' a' b' =>
      r s s' && checkSitewise r a a' && checkSitewise r b b'
  | _, _ => false

theorem checkSitewise_iff {Literal Site Site' : Type} [DecidableEq Literal]
    (R : Site → Site' → Prop) [∀ s s', Decidable (R s s')] (e : Expr Literal Site)
    (e' : Expr Literal Site') : checkSitewise (fun s s' => decide (R s s')) e e' = true ↔ Sitewise R e e' := by
  induction e generalizing e' <;> cases e' <;>
    simp [checkSitewise, Sitewise, Bool.and_eq_true, and_assoc, *]

instance {Literal Site Site' : Type} [DecidableEq Literal] (R : Site → Site' → Prop)
    [∀ s s', Decidable (R s s')] (e : Expr Literal Site) (e' : Expr Literal Site') :
    Decidable (Sitewise R e e') :=
  decidable_of_iff _ (checkSitewise_iff R e e')

end Determinize.Spec.Paper.Expr

namespace Determinize.Spec.Paper

instance (input : Input) (program : Annotated) : Decidable (input.matches program) :=
  inferInstanceAs (Decidable (Expr.Sitewise _ input program))

end Determinize.Spec.Paper
