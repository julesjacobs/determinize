import Determinize.Spec.Primitives
import Determinize.Spec.Types
import Mathlib.Tactic.DeriveCountable

/-!
# Reviewer-facing paper syntax

Only sample sites carry affinity labels; subtyping is silent; literals and arithmetic
are unannotated, as in the paper's grammar, and a literal types at either affinity. Types are
assigned separately by `Typed`; no expression constructor contains a type annotation.
Each primitive distribution is its own constructor with the paper's operands; a site
samples with an E/G affinity or computes the primitive’s mean without an affinity annotation.
Programs may contain both sampling and mean sites; their operands may be arbitrary expressions.
-/

namespace Determinize.Spec.Paper

/-- Untyped paper expressions with de Bruijn variables; only sample sites carry a affinity. -/
inductive Expr (Literal : Type := ℝ) where
  | bvar (index : Nat)
  | reject
  | unit | bool (value : Bool) | real (value : Literal)
  | lam (body : Expr Literal)
  | fix (body : Expr Literal)
  | app (function argument : Expr Literal)
  | pair (left right : Expr Literal) | fst (pair : Expr Literal)
  | snd (pair : Expr Literal) | inl (operand : Expr Literal)
  | inr (operand : Expr Literal)
  | matchSum (scrutinee left right : Expr Literal)
  | nil | cons (head tail : Expr Literal)
  | matchList (scrutinee nilCase consCase : Expr Literal)
  | ite (condition thenBranch elseBranch : Expr Literal)
  | letE (value body : Expr Literal)
  | neg (body : Expr Literal)
  | add (left right : Expr Literal) | mul (left right : Expr Literal)
  | div (left right : Expr Literal) | lt (left right : Expr Literal)
  | uniform (action : DistributionAction) (lower upper : Expr Literal)
  | gaussian (action : DistributionAction) (mean variance : Expr Literal)
  | poisson (action : DistributionAction) (rate : Expr Literal)
  | discrete (action : DistributionAction) (distribution : FiniteDistribution)
  | bernoulli (action : DistributionAction) (probability : Expr Literal)
  | exponential (action : DistributionAction) (rate : Expr Literal)
  | beta (action : DistributionAction) (alpha beta : Expr Literal)
  | gamma (action : DistributionAction) (shape rate : Expr Literal)

deriving instance Repr, DecidableEq, Inhabited for Expr

namespace Expr

def isValue {Literal : Type} : Expr Literal → Bool
  | .unit | .bool _ | .real _ | .lam _ | .fix _ | .nil => true
  | .pair left right | .cons left right => left.isValue && right.isValue
  | .inl operand | .inr operand => operand.isValue
  | _ => false

/-- Apply `replace depth index` to variables, increasing `depth` beneath binders. -/
def mapVars {Literal : Type} (replace : Nat → Nat → Expr Literal) (depth : Nat) :
    Expr Literal → Expr Literal
  | .bvar index => replace depth index
  | .unit => .unit
  | .reject => .reject
  | .discrete action d => .discrete action d
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
  | .uniform k l r => .uniform k (l.mapVars replace depth) (r.mapVars replace depth)
  | .gaussian k l r => .gaussian k (l.mapVars replace depth) (r.mapVars replace depth)
  | .poisson k x => .poisson k (x.mapVars replace depth)
  | .bernoulli k x => .bernoulli k (x.mapVars replace depth)
  | .exponential k x => .exponential k (x.mapVars replace depth)
  | .beta k l r => .beta k (l.mapVars replace depth) (r.mapVars replace depth)
  | .gamma k l r => .gamma k (l.mapVars replace depth) (r.mapVars replace depth)

abbrev shift {Literal : Type} (amount cutoff : Nat) : Expr Literal → Expr Literal :=
  mapVars (fun cutoff index => .bvar (if cutoff ≤ index then index + amount else index)) cutoff

abbrev substAt {Literal : Type} (depth : Nat) (replacement : Expr Literal) : Expr Literal → Expr Literal :=
  mapVars (fun depth index => if index = depth then replacement.shift depth 0
    else .bvar (if depth < index then index - 1 else index)) depth

def substHead {Literal : Type} (body replacement : Expr Literal) : Expr Literal :=
  substAt 0 replacement body
def substTwo {Literal : Type} (body argument function : Expr Literal) : Expr Literal :=
  substAt 0 argument (substAt 1 function body)

/-- Replace E stochastic samples by their atomic means. -/
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
  | .inl operand => .inl operand.determinize
  | .inr operand => .inr operand.determinize
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
  | .uniform action lower upper =>
      .uniform action.determinize lower.determinize upper.determinize
  | .gaussian action mean variance =>
      .gaussian action.determinize mean.determinize variance.determinize
  | .poisson action rate => .poisson action.determinize rate.determinize
  | .bernoulli action probability => .bernoulli action.determinize probability.determinize
  | .discrete action d => .discrete action.determinize d
  | .exponential action rate =>
      .exponential action.determinize rate.determinize
  | .beta action left right =>
      .beta action.determinize left.determinize right.determinize
  | .gamma action shape rate =>
      .gamma action.determinize shape.determinize rate.determinize

def mapLiteral {α β : Type} (f : α → β) : Expr α → Expr β
  | .bvar index => .bvar index
  | .unit => .unit
  | .reject => .reject
  | .discrete action d => .discrete action d
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
  | .uniform action lower upper => .uniform action (lower.mapLiteral f) (upper.mapLiteral f)
  | .gaussian action mean variance => .gaussian action (mean.mapLiteral f) (variance.mapLiteral f)
  | .poisson action rate => .poisson action (rate.mapLiteral f)
  | .bernoulli action probability => .bernoulli action (probability.mapLiteral f)
  | .exponential action rate => .exponential action (rate.mapLiteral f)
  | .beta action alpha betaArg => .beta action (alpha.mapLiteral f) (betaArg.mapLiteral f)
  | .gamma action shape rate => .gamma action (shape.mapLiteral f) (rate.mapLiteral f)

end Expr

/-- Typing of de Bruijn variables in a paper context. -/
inductive HasVar : List Ty → Nat → Ty → Prop
  | head : HasVar (ty :: context) 0 ty
  | tail : HasVar context index ty → HasVar (head :: context) (index + 1) ty

/-- Core typing with affinity-annotated sample sites and silent structural subtyping. -/
inductive Typed : List Ty → Expr → Ty → Prop
  | bvar : HasVar context index ty → Typed context (.bvar index) ty
  | reject : Typed context .reject ty
  | unit : Typed context .unit .unit
  | bool : Typed context (.bool value) .bool
  | real : Typed context (.real value) (.float affinity)
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
  | inl : Typed context operand leftTy →
      Typed context (.inl operand) (.sum leftTy rightTy)
  | inr : Typed context operand rightTy →
      Typed context (.inr operand) (.sum leftTy rightTy)
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
  | neg : Typed context operand (.float affinity) → Typed context (.neg operand) (.float affinity)
  | add : Typed context left (.float affinity) → Typed context right (.float affinity) →
      Typed context (.add left right) (.float affinity)
  | mul : Typed context left (.float .G) → Typed context right (.float affinity) →
      Typed context (.mul left right) (.float affinity)
  | div : Typed context left (.float affinity) → Typed context right (.float .G) →
      Typed context (.div left right) (.float affinity)
  | lt : Typed context left (.float .G) → Typed context right (.float .G) →
      Typed context (.lt left right) .bool
  | uniform : Typed context lower (.float affinity) → Typed context upper (.float affinity) →
      Typed context (.uniform (.sample affinity) lower upper) (.float affinity)
  | uniformMean : Typed context lower (.float affinity) → Typed context upper (.float affinity) →
      Typed context (.uniform .mean lower upper) (.float affinity)
  | gaussian : Typed context mean (.float affinity) → Typed context variance (.float .G) →
      Typed context (.gaussian (.sample affinity) mean variance) (.float affinity)
  | gaussianMean : Typed context mean (.float affinity) → Typed context variance (.float .G) →
      Typed context (.gaussian .mean mean variance) (.float affinity)
  | poisson : Typed context rate (.float affinity) →
      Typed context (.poisson (.sample affinity) rate) (.float affinity)
  | poissonMean : Typed context rate (.float affinity) →
      Typed context (.poisson .mean rate) (.float affinity)
  | discrete : Typed context (.discrete (.sample affinity) d) (.float affinity)
  | discreteMean : Typed context (.discrete .mean d) (.float affinity)
  | bernoulli : Typed context probability (.float affinity) →
      Typed context (.bernoulli (.sample affinity) probability) (.float affinity)
  | bernoulliMean : Typed context probability (.float affinity) →
      Typed context (.bernoulli .mean probability) (.float affinity)
  | exponential : Typed context rate (.float .G) →
      Typed context (.exponential (.sample affinity) rate) (.float affinity)
  | exponentialMean : Typed context rate (.float .G) →
      Typed context (.exponential .mean rate) (.float affinity)
  | beta : Typed context alpha (.float .G) → Typed context beta (.float .G) →
      Typed context (.beta (.sample affinity) alpha beta) (.float affinity)
  | betaMean : Typed context alpha (.float .G) → Typed context beta (.float .G) →
      Typed context (.beta .mean alpha beta) (.float affinity)
  | gamma : Typed context shape (.float affinity) → Typed context rate (.float .G) →
      Typed context (.gamma (.sample affinity) shape rate) (.float affinity)
  | gammaMean : Typed context shape (.float affinity) → Typed context rate (.float .G) →
      Typed context (.gamma .mean shape rate) (.float affinity)

end Determinize.Spec.Paper
