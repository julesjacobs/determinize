import Determinize.Spec.Primitives
import Determinize.Spec.Types
import Mathlib.Tactic.DeriveCountable

/-!
# Reviewer-facing paper syntax

Only sample sites carry affinity labels; subtyping is silent; literals and arithmetic
are unannotated, as in the paper's grammar, and a literal types at either affinity. Types are
assigned separately by `Typed`; no expression constructor contains a type annotation.
Each primitive distribution is its own constructor with the paper's operands and a site. In
the paper syntax (`Expr` with its default parameters), a site samples with an E/G affinity or
computes the primitive’s mean without an affinity annotation. Programs may contain both sampling
and mean sites; their operands may be arbitrary expressions. `Spec/Frontend.lean` reuses the
constructors with rational literals and other sites for the programs of the front end.
-/

namespace Determinize.Spec.Paper

/-- Untyped paper expressions with de Bruijn variables. `Site` is the annotation of a
primitive distribution; in the paper syntax it is a `DistributionAction`. -/
inductive Expr (Literal : Type := ℝ) (Site : Type := DistributionAction) where
  | bvar (index : Nat)
  | reject
  | unit | bool (value : Bool) | real (value : Literal)
  | lam (body : Expr Literal Site)
  | fix (body : Expr Literal Site)
  | app (function argument : Expr Literal Site)
  | pair (left right : Expr Literal Site) | fst (pair : Expr Literal Site)
  | snd (pair : Expr Literal Site) | inl (operand : Expr Literal Site)
  | inr (operand : Expr Literal Site)
  | matchSum (scrutinee left right : Expr Literal Site)
  | nil | cons (head tail : Expr Literal Site)
  | matchList (scrutinee nilCase consCase : Expr Literal Site)
  | ite (condition thenBranch elseBranch : Expr Literal Site)
  | letE (value body : Expr Literal Site)
  | neg (body : Expr Literal Site)
  | add (left right : Expr Literal Site) | mul (left right : Expr Literal Site)
  | div (left right : Expr Literal Site) | lt (left right : Expr Literal Site)
  | uniform (site : Site) (lower upper : Expr Literal Site)
  | gaussian (site : Site) (mean variance : Expr Literal Site)
  | poisson (site : Site) (rate : Expr Literal Site)
  | discrete (site : Site) (probabilities : Expr Literal Site)
  | bernoulli (site : Site) (probability : Expr Literal Site)
  | exponential (site : Site) (rate : Expr Literal Site)
  | beta (site : Site) (alpha beta : Expr Literal Site)
  | gamma (site : Site) (shape rate : Expr Literal Site)

deriving instance Repr, DecidableEq, Inhabited for Expr

namespace Expr

def isValue {Literal : Type} : Expr Literal → Bool
  | .unit | .bool _ | .real _ | .lam _ | .fix _ | .nil => true
  | .pair left right | .cons left right => left.isValue && right.isValue
  | .inl operand | .inr operand => operand.isValue
  | _ => false

/-- Apply `replace depth index` to variables, increasing `depth` beneath binders. -/
def mapVars {Literal Site : Type} (replace : Nat → Nat → Expr Literal Site) (depth : Nat) :
    Expr Literal Site → Expr Literal Site
  | .bvar index => replace depth index
  | .unit => .unit
  | .reject => .reject
  | .discrete action d => .discrete action (d.mapVars replace depth)
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

abbrev shift {Literal Site : Type} (amount cutoff : Nat) : Expr Literal Site → Expr Literal Site :=
  mapVars (fun cutoff index => .bvar (if cutoff ≤ index then index + amount else index)) cutoff

abbrev substAt {Literal Site : Type} (depth : Nat) (replacement : Expr Literal Site) :
    Expr Literal Site → Expr Literal Site :=
  mapVars (fun depth index => if index = depth then replacement.shift depth 0
    else .bvar (if depth < index then index - 1 else index)) depth

def substHead {Literal Site : Type} (body replacement : Expr Literal Site) : Expr Literal Site :=
  substAt 0 replacement body
def substTwo {Literal Site : Type} (body argument function : Expr Literal Site) :
    Expr Literal Site :=
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
  | .discrete action d => .discrete action.determinize d.determinize
  | .exponential action rate =>
      .exponential action.determinize rate.determinize
  | .beta action left right =>
      .beta action.determinize left.determinize right.determinize
  | .gamma action shape rate =>
      .gamma action.determinize shape.determinize rate.determinize

/-- Apply `literal` to every literal and `site` to every site. -/
def map {Literal Literal' Site Site' : Type} (literal : Literal → Literal') (site : Site → Site') :
    Expr Literal Site → Expr Literal' Site'
  | .bvar index => .bvar index
  | .unit => .unit
  | .reject => .reject
  | .bool value => .bool value
  | .real value => .real (literal value)
  | .lam body => .lam (body.map literal site)
  | .fix body => .fix (body.map literal site)
  | .app function argument => .app (function.map literal site) (argument.map literal site)
  | .pair left right => .pair (left.map literal site) (right.map literal site)
  | .fst pairValue => .fst (pairValue.map literal site)
  | .snd pairValue => .snd (pairValue.map literal site)
  | .inl value => .inl (value.map literal site)
  | .inr value => .inr (value.map literal site)
  | .matchSum scrutinee left right =>
      .matchSum (scrutinee.map literal site) (left.map literal site) (right.map literal site)
  | .nil => .nil
  | .cons head tail => .cons (head.map literal site) (tail.map literal site)
  | .matchList scrutinee nilCase consCase =>
      .matchList (scrutinee.map literal site) (nilCase.map literal site) (consCase.map literal site)
  | .ite condition thenBranch elseBranch =>
      .ite (condition.map literal site) (thenBranch.map literal site) (elseBranch.map literal site)
  | .letE value body => .letE (value.map literal site) (body.map literal site)
  | .neg body => .neg (body.map literal site)
  | .add left right => .add (left.map literal site) (right.map literal site)
  | .mul left right => .mul (left.map literal site) (right.map literal site)
  | .div left right => .div (left.map literal site) (right.map literal site)
  | .lt left right => .lt (left.map literal site) (right.map literal site)
  | .uniform s lower upper => .uniform (site s) (lower.map literal site) (upper.map literal site)
  | .gaussian s mean variance =>
      .gaussian (site s) (mean.map literal site) (variance.map literal site)
  | .poisson s rate => .poisson (site s) (rate.map literal site)
  | .discrete s probabilities => .discrete (site s) (probabilities.map literal site)
  | .bernoulli s probability => .bernoulli (site s) (probability.map literal site)
  | .exponential s rate => .exponential (site s) (rate.map literal site)
  | .beta s alpha betaArg => .beta (site s) (alpha.map literal site) (betaArg.map literal site)
  | .gamma s shape rate => .gamma (site s) (shape.map literal site) (rate.map literal site)

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
  | discrete : Typed context probabilities (.list (.float affinity)) →
      Typed context (.discrete (.sample affinity) probabilities) (.float affinity)
  | discreteMean : Typed context probabilities (.list (.float affinity)) →
      Typed context (.discrete .mean probabilities) (.float affinity)
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
