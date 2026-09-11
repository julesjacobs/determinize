import Determinize.Spec.Primitives
import Determinize.Spec.Types
import Mathlib.Tactic.DeriveCountable

/-!
# Reviewer-facing paper syntax

Only sample sites carry mode labels; subtyping is silent; literals and arithmetic
are mode-free, as in the paper's grammar, and a literal types at either mode. Types are
assigned separately by `Typed`; no expression constructor contains a type annotation.
Each primitive distribution is its own constructor with the paper's operands; a site
carries its mode and whether it still samples or already returns the primitive's mean.
`Expr.sourceForm` requires stochastic sites; their operands may be arbitrary expressions.
-/

namespace Determinize.Spec.Paper

/-- Untyped paper expressions with de Bruijn variables; only sample sites carry a mode. -/
inductive Expr (Literal : Type := ℝ) where
  | bvar (index : Nat)
  | reject
  | unit | bool (value : Bool) | real (value : Literal)
  | lam (body : Expr Literal)
  | fix (body : Expr Literal)
  | app (function argument : Expr Literal)
  | pair (left right : Expr Literal) | fst (pair : Expr Literal)
  | snd (pair : Expr Literal) | inl (value : Expr Literal)
  | inr (value : Expr Literal)
  | matchSum (scrutinee left right : Expr Literal)
  | nil | cons (head tail : Expr Literal)
  | matchList (scrutinee nilCase consCase : Expr Literal)
  | ite (condition thenBranch elseBranch : Expr Literal)
  | letE (value body : Expr Literal)
  | neg (body : Expr Literal)
  | add (left right : Expr Literal) | mul (left right : Expr Literal)
  | div (left right : Expr Literal) | lt (left right : Expr Literal)
  | uniform (mode : Mode) (kind : Kind) (lower upper : Expr Literal)
  | gaussian (mode : Mode) (kind : Kind) (mean variance : Expr Literal)
  | poisson (mode : Mode) (kind : Kind) (rate : Expr Literal)
  | discrete (mode : Mode) (kind : Kind) (distribution : FiniteDistribution)
  | bernoulli (mode : Mode) (kind : Kind) (probability : Expr Literal)
  | exponential (mode : Mode) (kind : Kind) (rate : Expr Literal)
  | beta (mode : Mode) (kind : Kind) (alpha beta : Expr Literal)
  | gamma (mode : Mode) (kind : Kind) (shape rate : Expr Literal)

deriving instance Repr, DecidableEq, Inhabited for Expr

namespace Expr

def isValue {Literal : Type} : Expr Literal → Bool
  | .unit | .bool _ | .real _ | .lam _ | .fix _ | .nil => true
  | .pair left right | .cons left right => left.isValue && right.isValue
  | .inl value | .inr value => value.isValue
  | _ => false

/-- Source expressions contain only stochastic sampling sites. -/
def sourceForm {Literal : Type} : Expr Literal → Bool
  | .bvar _ | .reject | .unit | .bool _ | .real _ | .nil => true
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .neg body => body.sourceForm
  | .app left right | .pair left right | .cons left right
  | .add left right | .mul left right | .div left right | .lt left right =>
      left.sourceForm && right.sourceForm
  | .matchSum scrutinee left right | .ite scrutinee left right =>
      scrutinee.sourceForm && left.sourceForm && right.sourceForm
  | .matchList scrutinee nilCase consCase =>
      scrutinee.sourceForm && nilCase.sourceForm && consCase.sourceForm
  | .letE value body => value.sourceForm && body.sourceForm
  | .discrete _ kind _ => kind.isStochastic
  | .uniform _ kind lower upper => kind.isStochastic && lower.sourceForm && upper.sourceForm
  | .gaussian _ kind mean variance =>
      kind.isStochastic && mean.sourceForm && variance.sourceForm
  | .poisson _ kind rate | .bernoulli _ kind rate | .exponential _ kind rate => kind.isStochastic && rate.sourceForm
  | .beta _ kind left right => kind.isStochastic && left.sourceForm && right.sourceForm
  | .gamma _ kind shape rate => kind.isStochastic && shape.sourceForm && rate.sourceForm

/-- Apply `replace depth index` to variables, increasing `depth` beneath binders. -/
def mapVars {Literal : Type} (replace : Nat → Nat → Expr Literal) (depth : Nat) :
    Expr Literal → Expr Literal
  | .bvar index => replace depth index
  | .unit => .unit
  | .reject => .reject
  | .discrete mode kind d => .discrete mode kind d
  | .bool value => .bool value
  | .real value => .real value
  | .lam body => .lam (body.mapVars replace (depth + 1))
  | .fix body => .fix (body.mapVars replace (depth + 2))
  | .app f x => .app (f.mapVars replace depth) (x.mapVars replace depth)
  | .pair l r => .pair (l.mapVars replace depth) (r.mapVars replace depth)
  | .fst x => .fst (x.mapVars replace depth)
  | .snd x => .snd (x.mapVars replace depth)
  | .inl x => .inl (x.mapVars replace depth)
  | .inr x => .inr (x.mapVars replace depth)
  | .matchSum x l r => .matchSum (x.mapVars replace depth)
      (l.mapVars replace (depth + 1)) (r.mapVars replace (depth + 1))
  | .nil => .nil
  | .cons h t => .cons (h.mapVars replace depth) (t.mapVars replace depth)
  | .matchList x n c => .matchList (x.mapVars replace depth)
      (n.mapVars replace depth) (c.mapVars replace (depth + 2))
  | .ite c t e => .ite (c.mapVars replace depth) (t.mapVars replace depth)
      (e.mapVars replace depth)
  | .letE x b => .letE (x.mapVars replace depth)
      (b.mapVars replace (depth + 1))
  | .neg x => .neg (x.mapVars replace depth)
  | .add l r => .add (l.mapVars replace depth) (r.mapVars replace depth)
  | .mul l r => .mul (l.mapVars replace depth) (r.mapVars replace depth)
  | .div l r => .div (l.mapVars replace depth) (r.mapVars replace depth)
  | .lt l r => .lt (l.mapVars replace depth) (r.mapVars replace depth)
  | .uniform m k l r => .uniform m k (l.mapVars replace depth) (r.mapVars replace depth)
  | .gaussian m k l r => .gaussian m k (l.mapVars replace depth) (r.mapVars replace depth)
  | .poisson m k x => .poisson m k (x.mapVars replace depth)
  | .bernoulli m k x => .bernoulli m k (x.mapVars replace depth)
  | .exponential m k x => .exponential m k (x.mapVars replace depth)
  | .beta m k l r => .beta m k (l.mapVars replace depth) (r.mapVars replace depth)
  | .gamma m k l r => .gamma m k (l.mapVars replace depth) (r.mapVars replace depth)

abbrev shift {Literal : Type} (amount cutoff : Nat) : Expr Literal → Expr Literal :=
  mapVars (fun cutoff index => .bvar (if cutoff ≤ index then index + amount else index)) cutoff

abbrev substAt {Literal : Type} (depth : Nat) (replacement : Expr Literal) : Expr Literal → Expr Literal :=
  mapVars (fun depth index => if index = depth then replacement.shift depth 0
    else .bvar (if depth < index then index - 1 else index)) depth

def substHead {Literal : Type} (body replacement : Expr Literal) : Expr Literal :=
  substAt 0 replacement body
def substTwo {Literal : Type} (body argument function : Expr Literal) : Expr Literal :=
  substAt 0 argument (substAt 1 function body)

/-- An expectation-mode site returns its mean after determinization; a general-mode site
keeps sampling. -/
def determinizeKind : Mode → Kind → Kind
  | .E, _ => .mean
  | .G, kind => kind

/-- Replace expectation-mode stochastic samples by their atomic means. -/
def determinize {Literal : Type} : Expr Literal → Expr Literal
  | .bvar index => .bvar index
  | .unit => .unit
  | .reject => .reject
  | .bool value => .bool value
  | .real value => .real value
  | .lam body => .lam body.determinize
  | .fix body => .fix body.determinize
  | .app function argument => .app function.determinize argument.determinize
  | .pair left right => .pair left.determinize right.determinize
  | .fst pairValue => .fst pairValue.determinize
  | .snd pairValue => .snd pairValue.determinize
  | .inl value => .inl value.determinize
  | .inr value => .inr value.determinize
  | .matchSum scrutinee left right =>
      .matchSum scrutinee.determinize left.determinize right.determinize
  | .nil => .nil
  | .cons head tail => .cons head.determinize tail.determinize
  | .matchList scrutinee nilCase consCase =>
      .matchList scrutinee.determinize nilCase.determinize consCase.determinize
  | .ite condition thenBranch elseBranch =>
      .ite condition.determinize thenBranch.determinize elseBranch.determinize
  | .letE value body => .letE value.determinize body.determinize
  | .neg body => .neg body.determinize
  | .add left right => .add left.determinize right.determinize
  | .mul left right => .mul left.determinize right.determinize
  | .div left right => .div left.determinize right.determinize
  | .lt left right => .lt left.determinize right.determinize
  | .uniform mode kind lower upper =>
      .uniform mode (determinizeKind mode kind) lower.determinize upper.determinize
  | .gaussian mode kind mean variance =>
      .gaussian mode (determinizeKind mode kind) mean.determinize variance.determinize
  | .poisson mode kind rate => .poisson mode (determinizeKind mode kind) rate.determinize
  | .bernoulli mode kind probability => .bernoulli mode (determinizeKind mode kind) probability.determinize
  | .discrete mode kind d => .discrete mode (determinizeKind mode kind) d
  | .exponential mode kind rate =>
      .exponential mode (determinizeKind mode kind) rate.determinize
  | .beta mode kind left right =>
      .beta mode (determinizeKind mode kind) left.determinize right.determinize
  | .gamma mode kind shape rate =>
      .gamma mode (determinizeKind mode kind) shape.determinize rate.determinize

def mapLiteral {α β : Type} (f : α → β) : Expr α → Expr β
  | .bvar index => .bvar index
  | .unit => .unit
  | .reject => .reject
  | .discrete mode kind d => .discrete mode kind d
  | .bool value => .bool value
  | .real value => .real (f value)
  | .lam body => .lam (body.mapLiteral f)
  | .fix body => .fix (body.mapLiteral f)
  | .app function argument => .app (function.mapLiteral f) (argument.mapLiteral f)
  | .pair left right => .pair (left.mapLiteral f) (right.mapLiteral f)
  | .fst pairValue => .fst (pairValue.mapLiteral f)
  | .snd pairValue => .snd (pairValue.mapLiteral f)
  | .inl value => .inl (value.mapLiteral f)
  | .inr value => .inr (value.mapLiteral f)
  | .matchSum scrutinee left right =>
      .matchSum (scrutinee.mapLiteral f) (left.mapLiteral f) (right.mapLiteral f)
  | .nil => .nil
  | .cons head tail => .cons (head.mapLiteral f) (tail.mapLiteral f)
  | .matchList scrutinee nilCase consCase =>
      .matchList (scrutinee.mapLiteral f) (nilCase.mapLiteral f) (consCase.mapLiteral f)
  | .ite condition thenBranch elseBranch =>
      .ite (condition.mapLiteral f) (thenBranch.mapLiteral f) (elseBranch.mapLiteral f)
  | .letE value body => .letE (value.mapLiteral f) (body.mapLiteral f)
  | .neg body => .neg (body.mapLiteral f)
  | .add left right => .add (left.mapLiteral f) (right.mapLiteral f)
  | .mul left right => .mul (left.mapLiteral f) (right.mapLiteral f)
  | .div left right => .div (left.mapLiteral f) (right.mapLiteral f)
  | .lt left right => .lt (left.mapLiteral f) (right.mapLiteral f)
  | .uniform mode kind lower upper => .uniform mode kind (lower.mapLiteral f) (upper.mapLiteral f)
  | .gaussian mode kind mean variance => .gaussian mode kind (mean.mapLiteral f) (variance.mapLiteral f)
  | .poisson mode kind rate => .poisson mode kind (rate.mapLiteral f)
  | .bernoulli mode kind probability => .bernoulli mode kind (probability.mapLiteral f)
  | .exponential mode kind rate => .exponential mode kind (rate.mapLiteral f)
  | .beta mode kind alpha betaArg => .beta mode kind (alpha.mapLiteral f) (betaArg.mapLiteral f)
  | .gamma mode kind shape rate => .gamma mode kind (shape.mapLiteral f) (rate.mapLiteral f)

end Expr

/-- Typing of de Bruijn variables in a paper context. -/
inductive HasVar : List Ty → Nat → Ty → Prop
  | head : HasVar (ty :: context) 0 ty
  | tail : HasVar context index ty → HasVar (head :: context) (index + 1) ty

/-- Core typing with mode-annotated sample sites and silent structural subtyping. -/
inductive Typed : List Ty → Expr → Ty → Prop
  | bvar : HasVar context index ty → Typed context (.bvar index) ty
  | reject : Typed context .reject ty
  | unit : Typed context .unit .unit
  | bool : Typed context (.bool value) .bool
  | real : Typed context (.real value) (.float mode)
  | lam : Typed (argument :: context) body result →
      Typed context (.lam body) (.arr argument result)
  | fix : Typed (argument :: .arr argument result :: context) body result →
      Typed context (.fix body) (.arr argument result)
  | app : Typed context function (.arr argument result) → Typed context operand argument →
      Typed context (.app function operand) result
  | pair : Typed context left leftTy → Typed context right rightTy →
      Typed context (.pair left right) (.prod leftTy rightTy)
  | fst : Typed context pair (.prod leftTy rightTy) → Typed context (.fst pair) leftTy
  | snd : Typed context pair (.prod leftTy rightTy) → Typed context (.snd pair) rightTy
  | inl : Typed context value leftTy →
      Typed context (.inl value) (.sum leftTy rightTy)
  | inr : Typed context value rightTy →
      Typed context (.inr value) (.sum leftTy rightTy)
  | matchSum : Typed context scrutinee (.sum leftTy rightTy) →
      Typed (leftTy :: context) left result → Typed (rightTy :: context) right result →
      Typed context (.matchSum scrutinee left right) result
  | nil : Typed context .nil (.list element)
  | cons : Typed context head element → Typed context tail (.list element) →
      Typed context (.cons head tail) (.list element)
  | matchList : Typed context scrutinee (.list element) → Typed context nilCase result →
      Typed (element :: .list element :: context) consCase result →
      Typed context (.matchList scrutinee nilCase consCase) result
  | ite : Typed context condition .bool → Typed context thenBranch result →
      Typed context elseBranch result → Typed context (.ite condition thenBranch elseBranch) result
  | letE : Typed context value valueTy → Typed (valueTy :: context) body result →
      Typed context (.letE value body) result
  | sub : Typed context value a → Ty.Sub a b → Typed context value b
  | neg : Typed context value (.float mode) → Typed context (.neg value) (.float mode)
  | add : Typed context left (.float mode) → Typed context right (.float mode) →
      Typed context (.add left right) (.float mode)
  | mul : Typed context left (.float .G) → Typed context right (.float mode) →
      Typed context (.mul left right) (.float mode)
  | div : Typed context left (.float mode) → Typed context right (.float .G) →
      Typed context (.div left right) (.float mode)
  | lt : Typed context left (.float .G) → Typed context right (.float .G) →
      Typed context (.lt left right) .bool
  | uniform : Typed context lower (.float mode) → Typed context upper (.float mode) →
      Typed context (.uniform mode kind lower upper) (.float mode)
  | gaussian : Typed context mean (.float mode) → Typed context variance (.float .G) →
      Typed context (.gaussian mode kind mean variance) (.float mode)
  | poisson : Typed context rate (.float mode) →
      Typed context (.poisson mode kind rate) (.float mode)
  | discrete : Typed context (.discrete mode kind d) (.float mode)
  | bernoulli : Typed context probability (.float mode) →
      Typed context (.bernoulli mode kind probability) (.float mode)
  | exponential : Typed context rate (.float .G) →
      Typed context (.exponential mode kind rate) (.float mode)
  | beta : Typed context alpha (.float .G) → Typed context beta (.float .G) →
      Typed context (.beta mode kind alpha beta) (.float mode)
  | gamma : Typed context shape (.float mode) → Typed context rate (.float .G) →
      Typed context (.gamma mode kind shape rate) (.float mode)

end Determinize.Spec.Paper
