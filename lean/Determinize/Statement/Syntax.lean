import Determinize.Statement.Primitives
import Determinize.Statement.Types
import Mathlib.Tactic.DeriveCountable

/-!
# Reviewer-facing paper syntax

Only sample sites carry mode labels, and promotion is explicit; literals and arithmetic
are mode-free, as in the paper's grammar, and a literal types at either mode. Types are
assigned separately by `Typed`; no expression constructor contains a type annotation.
`Expr.sourceForm` requires stochastic source samples; their operands may be
arbitrary expressions.
-/

namespace Determinize.Statement.Paper

inductive Tag where
  | stochastic (op : Op)
  | mean (op : Op)
deriving DecidableEq, Repr, Countable

abbrev Tag.base : Tag → Op
  | .stochastic op | .mean op => op

/-- Untyped paper expressions with de Bruijn variables; only sample sites carry a mode. -/
inductive Expr (Literal : Type := ℝ) where
  | bvar (index : Nat)
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
  | promote (body : Expr Literal) | neg (body : Expr Literal)
  | add (left right : Expr Literal) | mul (left right : Expr Literal)
  | div (left right : Expr Literal) | lt (left right : Expr Literal)
  | sample (mode : Mode) (op : Tag) (affineArgs generalArgs : List (Expr Literal))

namespace Expr

def isValue {Literal : Type} : Expr Literal → Bool
  | .unit | .bool _ | .real _ | .lam _ | .fix _ | .nil => true
  | .pair left right | .cons left right => left.isValue && right.isValue
  | .inl value | .inr value => value.isValue
  | _ => false

/-- Source expressions contain only stochastic sampling tags. -/
def sourceForm : Expr → Bool
  | .bvar _ | .unit | .bool _ | .real _ | .nil => true
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .promote body | .neg body => body.sourceForm
  | .app left right | .pair left right | .cons left right
  | .add left right | .mul left right | .div left right | .lt left right =>
      left.sourceForm && right.sourceForm
  | .matchSum scrutinee left right | .ite scrutinee left right =>
      scrutinee.sourceForm && left.sourceForm && right.sourceForm
  | .matchList scrutinee nilCase consCase =>
      scrutinee.sourceForm && nilCase.sourceForm && consCase.sourceForm
  | .letE value body => value.sourceForm && body.sourceForm
  | .sample _ (.stochastic _) affine general =>
      (affine.map sourceForm).all id && (general.map sourceForm).all id
  | .sample _ (.mean _) _ _ => false

/-- Apply `replace depth index` to variables, increasing `depth` beneath binders. -/
def mapVars (replace : Nat → Nat → Expr) (depth : Nat) : Expr → Expr
  | .bvar index => replace depth index
  | .unit => .unit
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
  | .promote x => .promote (x.mapVars replace depth)
  | .neg x => .neg (x.mapVars replace depth)
  | .add l r => .add (l.mapVars replace depth) (r.mapVars replace depth)
  | .mul l r => .mul (l.mapVars replace depth) (r.mapVars replace depth)
  | .div l r => .div (l.mapVars replace depth) (r.mapVars replace depth)
  | .lt l r => .lt (l.mapVars replace depth) (r.mapVars replace depth)
  | .sample m op affine general => .sample m op
      (affine.map (mapVars replace depth)) (general.map (mapVars replace depth))

abbrev shift (amount cutoff : Nat) : Expr → Expr :=
  mapVars (fun cutoff index => .bvar (if cutoff ≤ index then index + amount else index)) cutoff

abbrev substAt (depth : Nat) (replacement : Expr) : Expr → Expr :=
  mapVars (fun depth index => if index = depth then replacement.shift depth 0
    else .bvar (if depth < index then index - 1 else index)) depth

def substHead (body replacement : Expr) : Expr := substAt 0 replacement body
def substTwo (body argument function : Expr) : Expr :=
  substAt 0 argument (substAt 1 function body)

/-- Replace expectation-mode stochastic samples by their atomic means. -/
def determinize : Expr → Expr
  | .bvar index => .bvar index
  | .unit => .unit
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
  | .promote body => .promote body.determinize
  | .neg body => .neg body.determinize
  | .add left right => .add left.determinize right.determinize
  | .mul left right => .mul left.determinize right.determinize
  | .div left right => .div left.determinize right.determinize
  | .lt left right => .lt left.determinize right.determinize
  | .sample .E (.stochastic op) affine general =>
      .sample .E (.mean op) (affine.map determinize) (general.map determinize)
  | .sample mode op affine general =>
      .sample mode op (affine.map determinize) (general.map determinize)

end Expr

/-- Typing of de Bruijn variables in a paper context. -/
inductive HasVar : List Ty → Nat → Ty → Prop
  | head : HasVar (ty :: context) 0 ty
  | tail : HasVar context index ty → HasVar (head :: context) (index + 1) ty

/-- The explicitly mode- and coercion-annotated core typing judgment. -/
inductive Typed : List Ty → Expr → Ty → Prop
  | bvar : HasVar context index ty → Typed context (.bvar index) ty
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
  | promote : Typed context value (.float .G) → Typed context (.promote value) (.float .E)
  | neg : Typed context value (.float mode) → Typed context (.neg value) (.float mode)
  | add : Typed context left (.float mode) → Typed context right (.float mode) →
      Typed context (.add left right) (.float mode)
  | mul : Typed context left (.float .G) → Typed context right (.float mode) →
      Typed context (.mul left right) (.float mode)
  | div : Typed context left (.float mode) → Typed context right (.float .G) →
      Typed context (.div left right) (.float mode)
  | lt : Typed context left (.float .G) → Typed context right (.float .G) →
      Typed context (.lt left right) .bool
  | sample (op : Tag) :
      affine.length = affineArity op.base → general.length = generalArity op.base →
      (∀ expression ∈ affine, Typed context expression (.float mode)) →
      (∀ expression ∈ general, Typed context expression (.float .G)) →
      Typed context (.sample mode op affine general) (.float mode)

end Determinize.Statement.Paper
