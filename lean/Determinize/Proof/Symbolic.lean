import Determinize.Proof.Typing

/-!
# Symbolic language and reduction

Expressions carry affine functions of earlier expectation-mode samples. The
symbolic reducer records those samples while preserving a fixed residual shape.
-/

set_option aesop.warn.nonterminal false
set_option linter.unusedSimpArgs false
set_option linter.unusedTactic false
set_option linter.unreachableTactic false
set_option linter.unnecessarySeqFocus false
set_option linter.unnecessarySimpa false
set_option linter.style.haveILetI false
set_option linter.unusedVariables false

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory
open Determinize.Statement.Paper

attribute [local simp] Determinize.Statement.Paper.reduce

namespace Symbolic

inductive AffineExpr (sampleCount : Nat) where
  | bvar (index : Nat) | unit | bool (value : Bool)
  | real (value : Affine sampleCount)
  | lam (body : AffineExpr sampleCount)
  | fix (body : AffineExpr sampleCount)
  | app (function argument : AffineExpr sampleCount)
  | pair (left right : AffineExpr sampleCount)
  | fst (pair : AffineExpr sampleCount)
  | snd (pair : AffineExpr sampleCount)
  | inl (value : AffineExpr sampleCount)
  | inr (value : AffineExpr sampleCount)
  | matchSum (scrutinee left right : AffineExpr sampleCount)
  | nil | cons (head tail : AffineExpr sampleCount)
  | matchList (scrutinee nilCase consCase : AffineExpr sampleCount)
  | ite (condition thenBranch elseBranch : AffineExpr sampleCount)
  | letE (value body : AffineExpr sampleCount)
  | promote (body : AffineExpr sampleCount)
  | neg (body : AffineExpr sampleCount)
  | add (left right : AffineExpr sampleCount)
  | mul (left right : AffineExpr sampleCount)
  | div (left right : AffineExpr sampleCount)
  | lt (left right : AffineExpr sampleCount)
  | uniform (mode : Mode) (kind : Kind) (lower upper : AffineExpr sampleCount)
  | gaussian (mode : Mode) (kind : Kind) (mean variance : AffineExpr sampleCount)
  | poisson (mode : Mode) (kind : Kind) (rate : AffineExpr sampleCount)
  | exponential (mode : Mode) (kind : Kind) (rate : AffineExpr sampleCount)
  | beta (mode : Mode) (kind : Kind) (alpha beta : AffineExpr sampleCount)
  | gamma (mode : Mode) (kind : Kind) (shape rate : AffineExpr sampleCount)
  | bernoulli (mode : Mode) (kind : Kind) (probability : AffineExpr sampleCount)
  | discrete (mode : Mode) (kind : Kind) (weights : List ℝ)

namespace AffineExpr

def realize (environment : Env sampleCount) : AffineExpr sampleCount → Expr
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real value => .real (value.eval environment)
  | .lam body => .lam (body.realize environment)
  | .fix body =>
      .fix (body.realize environment)
  | .app function argument =>
      .app (function.realize environment) (argument.realize environment)
  | .pair left right =>
      .pair (left.realize environment) (right.realize environment)
  | .fst pairValue => .fst (pairValue.realize environment)
  | .snd pairValue => .snd (pairValue.realize environment)
  | .inl value => .inl (value.realize environment)
  | .inr value => .inr (value.realize environment)
  | .matchSum scrutinee left right => .matchSum (scrutinee.realize environment)
      (left.realize environment) (right.realize environment)
  | .nil => .nil
  | .cons head tail =>
      .cons (head.realize environment) (tail.realize environment)
  | .matchList scrutinee nilCase consCase =>
      .matchList (scrutinee.realize environment)
        (nilCase.realize environment) (consCase.realize environment)
  | .ite condition thenBranch elseBranch =>
      .ite (condition.realize environment) (thenBranch.realize environment)
        (elseBranch.realize environment)
  | .letE value body =>
      .letE (value.realize environment) (body.realize environment)
  | .promote body => .promote (body.realize environment)
  | .neg body => .neg (body.realize environment)
  | .add left right =>
      .add (left.realize environment) (right.realize environment)
  | .mul left right =>
      .mul (left.realize environment) (right.realize environment)
  | .div left right =>
      .div (left.realize environment) (right.realize environment)
  | .lt left right => .lt (left.realize environment) (right.realize environment)
  | .uniform mode kind lower upper =>
      .uniform mode kind (lower.realize environment) (upper.realize environment)
  | .gaussian mode kind mean variance =>
      .gaussian mode kind (mean.realize environment) (variance.realize environment)
  | .poisson mode kind rate => .poisson mode kind (rate.realize environment)
  | .exponential mode kind rate => .exponential mode kind (rate.realize environment)
  | .beta mode kind left right =>
      .beta mode kind (left.realize environment) (right.realize environment)
  | .gamma mode kind shape rate =>
      .gamma mode kind (shape.realize environment) (rate.realize environment)
  | .bernoulli mode kind probability => .bernoulli mode kind (probability.realize environment)
  | .discrete mode kind weights => .discrete mode kind weights

def skeleton : AffineExpr sampleCount → Skeleton
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real _ => .real
  | .lam body => .lam body.skeleton
  | .fix body => .fix body.skeleton
  | .app function argument => .app function.skeleton argument.skeleton
  | .pair left right => .pair left.skeleton right.skeleton
  | .fst pairValue => .fst pairValue.skeleton
  | .snd pairValue => .snd pairValue.skeleton
  | .inl value => .inl value.skeleton
  | .inr value => .inr value.skeleton
  | .matchSum scrutinee left right =>
      .matchSum scrutinee.skeleton left.skeleton right.skeleton
  | .nil => .nil
  | .cons head tail => .cons head.skeleton tail.skeleton
  | .matchList scrutinee nilCase consCase =>
      .matchList scrutinee.skeleton nilCase.skeleton consCase.skeleton
  | .ite condition thenBranch elseBranch =>
      .ite condition.skeleton thenBranch.skeleton elseBranch.skeleton
  | .letE value body => .letE value.skeleton body.skeleton
  | .promote body => .promote body.skeleton
  | .neg body => .neg body.skeleton
  | .add left right => .add left.skeleton right.skeleton
  | .mul left right => .mul left.skeleton right.skeleton
  | .div left right => .div left.skeleton right.skeleton
  | .lt left right => .lt left.skeleton right.skeleton
  | .uniform mode kind lower upper => .uniform mode kind lower.skeleton upper.skeleton
  | .gaussian mode kind mean variance => .gaussian mode kind mean.skeleton variance.skeleton
  | .poisson mode kind rate => .poisson mode kind rate.skeleton
  | .exponential mode kind rate => .exponential mode kind rate.skeleton
  | .beta mode kind left right => .beta mode kind left.skeleton right.skeleton
  | .gamma mode kind shape rate => .gamma mode kind shape.skeleton rate.skeleton
  | .bernoulli mode kind probability => .bernoulli mode kind probability.skeleton
  | .discrete mode kind weights => .discrete mode kind (weights.map fun _ => ())

def coordinates : AffineExpr sampleCount → List (Affine sampleCount)
  | .real value => [value]
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .promote body | .neg body => body.coordinates
  | .app left right | .pair left right | .cons left right
  | .add left right | .mul left right | .div left right | .lt left right =>
      left.coordinates ++ right.coordinates
  | .matchSum scrutinee left right | .ite scrutinee left right =>
      scrutinee.coordinates ++ left.coordinates ++ right.coordinates
  | .matchList scrutinee nilCase consCase =>
      scrutinee.coordinates ++ nilCase.coordinates ++ consCase.coordinates
  | .letE value body => value.coordinates ++ body.coordinates
  | .uniform _ _ left right | .gaussian _ _ left right | .beta _ _ left right
  | .gamma _ _ left right => left.coordinates ++ right.coordinates
  | .poisson _ _ body | .exponential _ _ body | .bernoulli _ _ body => body.coordinates
  | .discrete _ _ weights => weights.map fun weight => (weight, 0)
  | _ => []

def ofExpr : Expr → AffineExpr 0
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real value => .real (value, Fin.elim0)
  | .lam body => .lam (ofExpr body)
  | .fix body => .fix (ofExpr body)
  | .app function argument => .app (ofExpr function) (ofExpr argument)
  | .pair left right => .pair (ofExpr left) (ofExpr right)
  | .fst pairValue => .fst (ofExpr pairValue)
  | .snd pairValue => .snd (ofExpr pairValue)
  | .inl value => .inl (ofExpr value)
  | .inr value => .inr (ofExpr value)
  | .matchSum scrutinee left right =>
      .matchSum (ofExpr scrutinee) (ofExpr left) (ofExpr right)
  | .nil => .nil
  | .cons head tail => .cons (ofExpr head) (ofExpr tail)
  | .matchList scrutinee nilCase consCase =>
      .matchList (ofExpr scrutinee) (ofExpr nilCase) (ofExpr consCase)
  | .ite condition thenBranch elseBranch =>
      .ite (ofExpr condition) (ofExpr thenBranch) (ofExpr elseBranch)
  | .letE value body => .letE (ofExpr value) (ofExpr body)
  | .promote body => .promote (ofExpr body)
  | .neg body => .neg (ofExpr body)
  | .add left right => .add (ofExpr left) (ofExpr right)
  | .mul left right => .mul (ofExpr left) (ofExpr right)
  | .div left right => .div (ofExpr left) (ofExpr right)
  | .lt left right => .lt (ofExpr left) (ofExpr right)
  | .uniform mode kind lower upper => .uniform mode kind (ofExpr lower) (ofExpr upper)
  | .gaussian mode kind mean variance => .gaussian mode kind (ofExpr mean) (ofExpr variance)
  | .poisson mode kind rate => .poisson mode kind (ofExpr rate)
  | .exponential mode kind rate => .exponential mode kind (ofExpr rate)
  | .beta mode kind left right => .beta mode kind (ofExpr left) (ofExpr right)
  | .gamma mode kind shape rate => .gamma mode kind (ofExpr shape) (ofExpr rate)
  | .bernoulli mode kind probability => .bernoulli mode kind (ofExpr probability)
  | .discrete mode kind weights => .discrete mode kind weights

def mapAffine (transform : Affine n → Affine m) : AffineExpr n → AffineExpr m
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real value => .real (transform value)
  | .lam body => .lam (body.mapAffine transform)
  | .fix body =>
      .fix (body.mapAffine transform)
  | .app function argument =>
      .app (function.mapAffine transform) (argument.mapAffine transform)
  | .pair left right =>
      .pair (left.mapAffine transform) (right.mapAffine transform)
  | .fst pairValue => .fst (pairValue.mapAffine transform)
  | .snd pairValue => .snd (pairValue.mapAffine transform)
  | .inl value => .inl (value.mapAffine transform)
  | .inr value => .inr (value.mapAffine transform)
  | .matchSum scrutinee left right => .matchSum (scrutinee.mapAffine transform)
      (left.mapAffine transform) (right.mapAffine transform)
  | .nil => .nil
  | .cons head tail =>
      .cons (head.mapAffine transform) (tail.mapAffine transform)
  | .matchList scrutinee nilCase consCase =>
      .matchList (scrutinee.mapAffine transform)
        (nilCase.mapAffine transform) (consCase.mapAffine transform)
  | .ite condition thenBranch elseBranch =>
      .ite (condition.mapAffine transform) (thenBranch.mapAffine transform)
        (elseBranch.mapAffine transform)
  | .letE value body =>
      .letE (value.mapAffine transform) (body.mapAffine transform)
  | .promote body => .promote (body.mapAffine transform)
  | .neg body => .neg (body.mapAffine transform)
  | .add left right =>
      .add (left.mapAffine transform) (right.mapAffine transform)
  | .mul left right =>
      .mul (left.mapAffine transform) (right.mapAffine transform)
  | .div left right =>
      .div (left.mapAffine transform) (right.mapAffine transform)
  | .lt left right => .lt (left.mapAffine transform) (right.mapAffine transform)
  | .uniform mode kind lower upper =>
      .uniform mode kind (lower.mapAffine transform) (upper.mapAffine transform)
  | .gaussian mode kind mean variance =>
      .gaussian mode kind (mean.mapAffine transform) (variance.mapAffine transform)
  | .poisson mode kind rate => .poisson mode kind (rate.mapAffine transform)
  | .exponential mode kind rate => .exponential mode kind (rate.mapAffine transform)
  | .beta mode kind left right =>
      .beta mode kind (left.mapAffine transform) (right.mapAffine transform)
  | .gamma mode kind shape rate =>
      .gamma mode kind (shape.mapAffine transform) (rate.mapAffine transform)
  | .bernoulli mode kind probability =>
      .bernoulli mode kind (probability.mapAffine transform)
  | .discrete mode kind weights => .discrete mode kind weights

def Affine.weaken (expression : Affine n) : Affine (n + 1) :=
  (expression.1, Fin.cases 0 expression.2)

def Affine.fresh (n : Nat) : Affine (n + 1) :=
  (0, Fin.cases 1 (fun _ => 0))

@[simp] theorem Affine.eval_const (constant : ℝ) (environment : Env n) :
    Symbolic.Affine.eval ((constant, 0) : Affine n) environment = constant := by
  simp [Symbolic.Affine.eval]

def weakenSamples (expression : AffineExpr n) : AffineExpr (n + 1) :=
  expression.mapAffine Affine.weaken

@[simp] theorem kind_isStochastic_iff (kind : Kind) :
    kind.isStochastic = true ↔ kind = .stochastic := by
  cases kind <;> simp [Kind.isStochastic]

/-- Pending source E and G sites retain stochastic tags. -/
def SourceTags : AffineExpr sampleCount → Prop
  | .uniform _ kind left right | .gaussian _ kind left right | .beta _ kind left right
  | .gamma _ kind left right => kind = .stochastic ∧ left.SourceTags ∧ right.SourceTags
  | .poisson _ kind body | .exponential _ kind body | .bernoulli _ kind body =>
      kind = .stochastic ∧ body.SourceTags
  | .discrete _ kind _ => kind = .stochastic
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .promote body | .neg body => body.SourceTags
  | .app left right | .pair left right | .cons left right
  | .add left right | .mul left right | .div left right | .lt left right =>
      left.SourceTags ∧ right.SourceTags
  | .matchSum scrutinee left right | .ite scrutinee left right =>
      scrutinee.SourceTags ∧ left.SourceTags ∧ right.SourceTags
  | .matchList scrutinee nilCase consCase =>
      scrutinee.SourceTags ∧ nilCase.SourceTags ∧ consCase.SourceTags
  | .letE value body => value.SourceTags ∧ body.SourceTags
  | _ => True

inductive WellTyped : List Ty → AffineExpr sampleCount → Ty → Prop
  | bvar : Determinize.Statement.Paper.HasVar context index ty → WellTyped context (.bvar index) ty
  | unit : WellTyped context .unit .unit
  | bool : WellTyped context (.bool value) .bool
  | realE : WellTyped context (.real value) (.float .E)
  | realG : value.2 = 0 → WellTyped context (.real value) (.float .G)
  | lam : WellTyped (argument :: context) body result →
      WellTyped context (.lam body) (.arr argument result)
  | fix : WellTyped (argument :: .arr argument result :: context) body result →
      WellTyped context (.fix body) (.arr argument result)
  | app : WellTyped context function (.arr argument result) → WellTyped context operand argument →
      WellTyped context (.app function operand) result
  | pair : WellTyped context left leftTy → WellTyped context right rightTy →
      WellTyped context (.pair left right) (.prod leftTy rightTy)
  | fst : WellTyped context pairValue (.prod leftTy rightTy) →
      WellTyped context (.fst pairValue) leftTy
  | snd : WellTyped context pairValue (.prod leftTy rightTy) →
      WellTyped context (.snd pairValue) rightTy
  | inl : WellTyped context value leftTy →
      WellTyped context (.inl value) (.sum leftTy rightTy)
  | inr : WellTyped context value rightTy →
      WellTyped context (.inr value) (.sum leftTy rightTy)
  | matchSum : WellTyped context scrutinee (.sum leftTy rightTy) →
      WellTyped (leftTy :: context) left result → WellTyped (rightTy :: context) right result →
      WellTyped context (.matchSum scrutinee left right) result
  | nil : WellTyped context .nil (.list element)
  | cons : WellTyped context head element → WellTyped context tail (.list element) →
      WellTyped context (.cons head tail) (.list element)
  | matchList : WellTyped context scrutinee (.list element) →
      WellTyped context nilCase result →
      WellTyped (element :: .list element :: context) consCase result →
      WellTyped context (.matchList scrutinee nilCase consCase) result
  | ite : WellTyped context condition .bool → WellTyped context thenBranch result →
      WellTyped context elseBranch result →
      WellTyped context (.ite condition thenBranch elseBranch) result
  | letE : WellTyped context value valueTy → WellTyped (valueTy :: context) body result →
      WellTyped context (.letE value body) result
  | promote : WellTyped context value (.float .G) →
      WellTyped context (.promote value) (.float .E)
  | negE : WellTyped context value (.float .E) →
      WellTyped context (.neg value) (.float .E)
  | negG : WellTyped context value (.float .G) →
      WellTyped context (.neg value) (.float .G)
  | addE : WellTyped context left (.float .E) → WellTyped context right (.float .E) →
      WellTyped context (.add left right) (.float .E)
  | addG : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.add left right) (.float .G)
  | mulGE : WellTyped context left (.float .G) → WellTyped context right (.float .E) →
      WellTyped context (.mul left right) (.float .E)
  | mulGG : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.mul left right) (.float .G)
  | divEG : WellTyped context left (.float .E) → WellTyped context right (.float .G) →
      WellTyped context (.div left right) (.float .E)
  | divGG : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.div left right) (.float .G)
  | lt : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.lt left right) .bool
  | uniform : WellTyped context lower (.float mode) → WellTyped context upper (.float mode) →
      WellTyped context (.uniform mode .stochastic lower upper) (.float mode)
  | gaussian : WellTyped context mean (.float mode) → WellTyped context spread (.float .G) →
      WellTyped context (.gaussian mode .stochastic mean spread) (.float mode)
  | poisson : WellTyped context rate (.float mode) →
      WellTyped context (.poisson mode .stochastic rate) (.float mode)
  | exponential : WellTyped context rate (.float .G) →
      WellTyped context (.exponential mode .stochastic rate) (.float mode)
  | beta : WellTyped context alpha (.float .G) → WellTyped context betaArg (.float .G) →
      WellTyped context (.beta mode .stochastic alpha betaArg) (.float mode)
  | gamma : WellTyped context shape (.float mode) → WellTyped context rate (.float .G) →
      WellTyped context (.gamma mode .stochastic shape rate) (.float mode)
  | bernoulli : WellTyped context probability (.float mode) →
      WellTyped context (.bernoulli mode .stochastic probability) (.float mode)
  | discrete : WellTyped context (.discrete mode .stochastic weights) (.float mode)

theorem WellTyped.realize_typed {sampleCount : Nat} {expression : AffineExpr sampleCount}
    (typed : WellTyped context expression ty)
    (environment : Env sampleCount) :
    Determinize.Statement.Paper.Typed context (expression.realize environment) ty := by
  induction typed <;> simp only [realize]
  case bvar hvar => exact Determinize.Statement.Paper.Typed.bvar hvar
  case «unit» => exact Determinize.Statement.Paper.Typed.unit
  case bool => exact Determinize.Statement.Paper.Typed.bool
  case realE => exact Determinize.Statement.Paper.Typed.real
  case realG => exact Determinize.Statement.Paper.Typed.real
  case lam ih => exact Determinize.Statement.Paper.Typed.lam ih
  case fix ih => exact Determinize.Statement.Paper.Typed.fix ih
  case app function operand => exact Determinize.Statement.Paper.Typed.app function operand
  case pair left right => exact Determinize.Statement.Paper.Typed.pair left right
  case fst pairValue => exact Determinize.Statement.Paper.Typed.fst pairValue
  case snd pairValue => exact Determinize.Statement.Paper.Typed.snd pairValue
  case inl value => exact Determinize.Statement.Paper.Typed.inl value
  case inr value => exact Determinize.Statement.Paper.Typed.inr value
  case matchSum scrutinee left right => exact Determinize.Statement.Paper.Typed.matchSum scrutinee left right
  case nil => exact Determinize.Statement.Paper.Typed.nil
  case cons head tail => exact Determinize.Statement.Paper.Typed.cons head tail
  case matchList scrutinee nilCase consCase =>
    exact Determinize.Statement.Paper.Typed.matchList scrutinee nilCase consCase
  case ite condition thenBranch elseBranch =>
    exact Determinize.Statement.Paper.Typed.ite condition thenBranch elseBranch
  case letE value body => exact Determinize.Statement.Paper.Typed.letE value body
  case promote value => exact Determinize.Statement.Paper.Typed.promote value
  case negE value => exact Determinize.Statement.Paper.Typed.neg value
  case negG value => exact Determinize.Statement.Paper.Typed.neg value
  case addE left right => exact Determinize.Statement.Paper.Typed.add left right
  case addG left right => exact Determinize.Statement.Paper.Typed.add left right
  case mulGE left right => exact Determinize.Statement.Paper.Typed.mul left right
  case mulGG left right => exact Determinize.Statement.Paper.Typed.mul left right
  case divEG left right => exact Determinize.Statement.Paper.Typed.div left right
  case divGG left right => exact Determinize.Statement.Paper.Typed.div left right
  case lt left right => exact Determinize.Statement.Paper.Typed.lt left right
  case uniform lower upper => exact Determinize.Statement.Paper.Typed.uniform lower upper
  case gaussian mean variance => exact Determinize.Statement.Paper.Typed.gaussian mean variance
  case poisson rate => exact Determinize.Statement.Paper.Typed.poisson rate
  case exponential rate => exact Determinize.Statement.Paper.Typed.exponential rate
  case beta alpha betaTyped => exact Determinize.Statement.Paper.Typed.beta alpha betaTyped
  case gamma shape rate => exact Determinize.Statement.Paper.Typed.gamma shape rate
  case bernoulli probability => exact Determinize.Statement.Paper.Typed.bernoulli probability
  case discrete => exact Determinize.Statement.Paper.Typed.discrete

theorem WellTyped.sourceTags (typed : WellTyped context expression ty) :
    expression.SourceTags := by
  induction typed <;> simp_all [SourceTags]

theorem WellTyped.mapAffine {n m : Nat} {expression : AffineExpr n}
    (typed : WellTyped context expression ty)
    (transform : Affine n → Affine m)
    (preservesZero : ∀ affine, affine.2 = 0 → (transform affine).2 = 0) :
    WellTyped context (expression.mapAffine transform) ty := by
  induction typed
  all_goals rw [AffineExpr.mapAffine]
  case realG =>
    rename_i value _ zero
    exact .realG (preservesZero value zero)
  all_goals aesop (add safe constructors WellTyped)

theorem WellTyped.weakenSamples (typed : WellTyped context expression ty) :
    WellTyped context expression.weakenSamples ty := by
  apply WellTyped.mapAffine typed Affine.weaken
  intro affine zero
  change (Fin.cases 0 affine.2 : Fin (_ + 1) → ℝ) = 0
  rw [zero]
  funext index
  refine Fin.cases ?_ (fun tail => ?_) index <;> rfl

def shift (amount cutoff : Nat) : AffineExpr sampleCount → AffineExpr sampleCount
  | .bvar index => .bvar (if cutoff ≤ index then index + amount else index)
  | .unit => .unit
  | .bool value => .bool value
  | .real value => .real value
  | .lam body => .lam (body.shift amount (cutoff + 1))
  | .fix body => .fix (body.shift amount (cutoff + 2))
  | .app f x => .app (f.shift amount cutoff) (x.shift amount cutoff)
  | .pair l r => .pair (l.shift amount cutoff) (r.shift amount cutoff)
  | .fst x => .fst (x.shift amount cutoff)
  | .snd x => .snd (x.shift amount cutoff)
  | .inl x => .inl (x.shift amount cutoff)
  | .inr x => .inr (x.shift amount cutoff)
  | .matchSum x l r => .matchSum (x.shift amount cutoff)
      (l.shift amount (cutoff + 1)) (r.shift amount (cutoff + 1))
  | .nil => .nil
  | .cons h t => .cons (h.shift amount cutoff) (t.shift amount cutoff)
  | .matchList x n c => .matchList (x.shift amount cutoff)
      (n.shift amount cutoff) (c.shift amount (cutoff + 2))
  | .ite c t e => .ite (c.shift amount cutoff) (t.shift amount cutoff)
      (e.shift amount cutoff)
  | .letE x b => .letE (x.shift amount cutoff)
      (b.shift amount (cutoff + 1))
  | .promote x => .promote (x.shift amount cutoff)
  | .neg x => .neg (x.shift amount cutoff)
  | .add l r => .add (l.shift amount cutoff) (r.shift amount cutoff)
  | .mul l r => .mul (l.shift amount cutoff) (r.shift amount cutoff)
  | .div l r => .div (l.shift amount cutoff) (r.shift amount cutoff)
  | .lt l r => .lt (l.shift amount cutoff) (r.shift amount cutoff)
  | .uniform m k lower upper => .uniform m k (lower.shift amount cutoff) (upper.shift amount cutoff)
  | .gaussian m k mean variance =>
      .gaussian m k (mean.shift amount cutoff) (variance.shift amount cutoff)
  | .poisson m k rate => .poisson m k (rate.shift amount cutoff)
  | .exponential m k rate => .exponential m k (rate.shift amount cutoff)
  | .beta m k left right => .beta m k (left.shift amount cutoff) (right.shift amount cutoff)
  | .gamma m k shape rate => .gamma m k (shape.shift amount cutoff) (rate.shift amount cutoff)
  | .bernoulli m k probability => .bernoulli m k (probability.shift amount cutoff)
  | .discrete m k weights => .discrete m k weights

def substAt (depth : Nat) (replacement : AffineExpr sampleCount)
    (expression : AffineExpr sampleCount) : AffineExpr sampleCount := match expression with
  | .bvar index => if index = depth then replacement.shift depth 0
      else .bvar (if depth < index then index - 1 else index)
  | .unit => .unit
  | .bool value => .bool value
  | .real value => .real value
  | .lam body => .lam (substAt (depth + 1) replacement body)
  | .fix body => .fix (substAt (depth + 2) replacement body)
  | .app f x => .app (substAt depth replacement f) (substAt depth replacement x)
  | .pair l r => .pair (substAt depth replacement l) (substAt depth replacement r)
  | .fst x => .fst (substAt depth replacement x)
  | .snd x => .snd (substAt depth replacement x)
  | .inl x => .inl (substAt depth replacement x)
  | .inr x => .inr (substAt depth replacement x)
  | .matchSum x l r => .matchSum (substAt depth replacement x)
      (substAt (depth + 1) replacement l) (substAt (depth + 1) replacement r)
  | .nil => .nil
  | .cons h t => .cons (substAt depth replacement h) (substAt depth replacement t)
  | .matchList x n c => .matchList (substAt depth replacement x)
      (substAt depth replacement n) (substAt (depth + 2) replacement c)
  | .ite c t e => .ite (substAt depth replacement c)
      (substAt depth replacement t) (substAt depth replacement e)
  | .letE x b => .letE (substAt depth replacement x)
      (substAt (depth + 1) replacement b)
  | .promote x => .promote (substAt depth replacement x)
  | .neg x => .neg (substAt depth replacement x)
  | .add l r => .add (substAt depth replacement l) (substAt depth replacement r)
  | .mul l r => .mul (substAt depth replacement l) (substAt depth replacement r)
  | .div l r => .div (substAt depth replacement l) (substAt depth replacement r)
  | .lt l r => .lt (substAt depth replacement l) (substAt depth replacement r)
  | .uniform m k lower upper =>
      .uniform m k (substAt depth replacement lower) (substAt depth replacement upper)
  | .gaussian m k mean variance =>
      .gaussian m k (substAt depth replacement mean) (substAt depth replacement variance)
  | .poisson m k rate => .poisson m k (substAt depth replacement rate)
  | .exponential m k rate => .exponential m k (substAt depth replacement rate)
  | .beta m k left right =>
      .beta m k (substAt depth replacement left) (substAt depth replacement right)
  | .gamma m k shape rate =>
      .gamma m k (substAt depth replacement shape) (substAt depth replacement rate)
  | .bernoulli m k probability => .bernoulli m k (substAt depth replacement probability)
  | .discrete m k weights => .discrete m k weights

def substHead (body replacement : AffineExpr sampleCount) : AffineExpr sampleCount :=
  substAt 0 replacement body

def substTwo (body argument function : AffineExpr sampleCount) : AffineExpr sampleCount :=
  substAt 0 argument (substAt 1 function body)

theorem wellTyped_shift (h : WellTyped (before ++ suffix) expression ty) :
    WellTyped (before ++ inserted ++ suffix)
      (expression.shift inserted.length before.length) ty := by
  generalize hcontext : before ++ suffix = context at h
  induction h generalizing before suffix with
  | bvar hvar =>
      rw [← hcontext] at hvar
      rw [shift]
      exact .bvar (Typing.hasVar_shift hvar)
  | unit => rw [shift]; exact .unit
  | bool => rw [shift]; exact .bool
  | realE => rw [shift]; exact .realE
  | realG zero => rw [shift]; exact .realG zero
  | lam h ih =>
      rw [shift]
      exact .lam (ih (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
  | fix h ih =>
      rw [shift]
      exact .fix (ih (before := _ :: _ :: before) (suffix := suffix) (by simpa using hcontext))
  | app hf hx ihf ihx =>
      rw [shift]
      exact .app (ihf (before := before) (suffix := suffix) hcontext)
        (ihx (before := before) (suffix := suffix) hcontext)
  | pair hl hr ihl ihr =>
      rw [shift]
      exact .pair (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | fst hp ih =>
      rw [shift]
      exact .fst (ih (before := before) (suffix := suffix) hcontext)
  | snd hp ih =>
      rw [shift]
      exact .snd (ih (before := before) (suffix := suffix) hcontext)
  | inl hv ih =>
      rw [shift]
      exact .inl (ih (before := before) (suffix := suffix) hcontext)
  | inr hv ih =>
      rw [shift]
      exact .inr (ih (before := before) (suffix := suffix) hcontext)
  | matchSum hs hl hr ihs ihl ihr =>
      rw [shift]
      exact .matchSum (ihs (before := before) (suffix := suffix) hcontext)
        (ihl (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
        (ihr (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
  | nil => rw [shift]; exact .nil
  | cons hh ht ihh iht =>
      rw [shift]
      exact .cons (ihh (before := before) (suffix := suffix) hcontext)
        (iht (before := before) (suffix := suffix) hcontext)
  | matchList hs hn hc ihs ihn ihc =>
      rw [shift]
      exact .matchList (ihs (before := before) (suffix := suffix) hcontext)
        (ihn (before := before) (suffix := suffix) hcontext)
        (ihc (before := _ :: _ :: before) (suffix := suffix) (by simpa using hcontext))
  | ite hc ht he ihc iht ihe =>
      rw [shift]
      exact .ite (ihc (before := before) (suffix := suffix) hcontext)
        (iht (before := before) (suffix := suffix) hcontext)
        (ihe (before := before) (suffix := suffix) hcontext)
  | letE hv hb ihv ihb =>
      rw [shift]
      exact .letE (ihv (before := before) (suffix := suffix) hcontext)
        (ihb (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
  | promote hv ih =>
      rw [shift]
      exact .promote (ih (before := before) (suffix := suffix) hcontext)
  | negE hv ih =>
      rw [shift]
      exact .negE (ih (before := before) (suffix := suffix) hcontext)
  | negG hv ih =>
      rw [shift]
      exact .negG (ih (before := before) (suffix := suffix) hcontext)
  | addE hl hr ihl ihr =>
      rw [shift]
      exact .addE (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | addG hl hr ihl ihr =>
      rw [shift]
      exact .addG (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | mulGE hl hr ihl ihr =>
      rw [shift]
      exact .mulGE (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | mulGG hl hr ihl ihr =>
      rw [shift]
      exact .mulGG (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | divEG hl hr ihl ihr =>
      rw [shift]
      exact .divEG (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | divGG hl hr ihl ihr =>
      rw [shift]
      exact .divGG (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | lt hl hr ihl ihr =>
      rw [shift]
      exact .lt (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | uniform hl hr ihl ihr =>
      rw [shift]
      exact .uniform (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | gaussian hl hr ihl ihr =>
      rw [shift]
      exact .gaussian (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | beta hl hr ihl ihr =>
      rw [shift]
      exact .beta (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | gamma hl hr ihl ihr =>
      rw [shift]
      exact .gamma (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | poisson hv ih =>
      rw [shift]
      exact .poisson (ih (before := before) (suffix := suffix) hcontext)
  | bernoulli hv ih =>
      rw [shift]
      exact .bernoulli (ih (before := before) (suffix := suffix) hcontext)
  | discrete => rw [shift]; exact .discrete
  | exponential hv ih =>
      rw [shift]
      exact .exponential (ih (before := before) (suffix := suffix) hcontext)

theorem aexprHasVar_subst (h : Determinize.Statement.Paper.HasVar (before ++ binder :: suffix) index ty) :
    (index = before.length ∧ ty = binder) ∨
      (index ≠ before.length ∧ Determinize.Statement.Paper.HasVar (before ++ suffix)
        (if before.length < index then index - 1 else index) ty) := by
  induction before generalizing index with
  | nil =>
      cases h with
      | head => exact .inl ⟨rfl, rfl⟩
      | tail h =>
          right
          exact ⟨by simp only [List.length_nil]; omega, by simpa using h⟩
  | cons head before ih =>
      cases h with
      | head =>
          right
          simp only [List.cons_append, List.length_cons, Nat.zero_lt_succ,
            ↓reduceIte]
          exact ⟨by omega, .head⟩
      | tail h =>
          rcases ih h with equal | shifted
          · left
            exact ⟨congrArg Nat.succ equal.1, equal.2⟩
          · right
            rcases shifted with ⟨notEqual, shifted⟩
            rename_i index
            constructor
            · simp only [List.length_cons]
              omega
            simp only [List.cons_append, List.length_cons, Nat.succ_lt_succ_iff]
            by_cases condition : before.length < index
            · simp only [condition, ↓reduceIte]
              have shifted := shifted
              simp only [condition, ↓reduceIte] at shifted
              have shifted' := Determinize.Statement.Paper.HasVar.tail (head := head) shifted
              convert shifted' using 1 <;> omega
            · simp only [condition, ↓reduceIte]
              have shifted := shifted
              simp only [condition, ↓reduceIte] at shifted
              exact Determinize.Statement.Paper.HasVar.tail (head := head) shifted

theorem wellTyped_substAt (h : WellTyped (before ++ binder :: suffix) expression ty)
    (replacementTyped : WellTyped suffix replacement binder) :
    WellTyped (before ++ suffix)
      (substAt before.length replacement expression) ty := by
  generalize hcontext : before ++ binder :: suffix = context at h
  induction h generalizing before suffix with
  | bvar hvar =>
      rw [← hcontext] at hvar
      rcases aexprHasVar_subst hvar with equal | shifted
      · rcases equal with ⟨rfl, rfl⟩
        rw [substAt, if_pos rfl]
        simpa only [List.nil_append, List.append_assoc, List.length_nil] using
          (wellTyped_shift (before := []) (suffix := suffix) (inserted := before)
            replacementTyped)
      · rcases shifted with ⟨notEqual, shifted⟩
        rw [substAt, if_neg notEqual]
        exact .bvar shifted
  | unit => rw [substAt]; exact .unit
  | bool => rw [substAt]; exact .bool
  | realE => rw [substAt]; exact .realE
  | realG zero => rw [substAt]; exact .realG zero
  | lam h ih =>
      rw [substAt]
      exact .lam (ih replacementTyped (before := _ :: before) (suffix := suffix)
        (by simpa using hcontext))
  | fix h ih =>
      rw [substAt]
      exact .fix (ih replacementTyped (before := _ :: _ :: before) (suffix := suffix)
        (by simpa using hcontext))
  | app hf hx ihf ihx =>
      rw [substAt]
      exact .app (ihf replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihx replacementTyped (before := before) (suffix := suffix) hcontext)
  | pair hl hr ihl ihr =>
      rw [substAt]
      exact .pair (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | fst hp ih =>
      rw [substAt]
      exact .fst (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | snd hp ih =>
      rw [substAt]
      exact .snd (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | inl hv ih =>
      rw [substAt]
      exact .inl (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | inr hv ih =>
      rw [substAt]
      exact .inr (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | matchSum hs hl hr ihs ihl ihr =>
      rw [substAt]
      exact .matchSum
        (ihs replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihl replacementTyped (before := _ :: before) (suffix := suffix)
          (by simpa using hcontext))
        (ihr replacementTyped (before := _ :: before) (suffix := suffix)
          (by simpa using hcontext))
  | nil => rw [substAt]; exact .nil
  | cons hh ht ihh iht =>
      rw [substAt]
      exact .cons (ihh replacementTyped (before := before) (suffix := suffix) hcontext)
        (iht replacementTyped (before := before) (suffix := suffix) hcontext)
  | matchList hs hn hc ihs ihn ihc =>
      rw [substAt]
      exact .matchList
        (ihs replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihn replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihc replacementTyped (before := _ :: _ :: before) (suffix := suffix)
          (by simpa using hcontext))
  | ite hc ht he ihc iht ihe =>
      rw [substAt]
      exact .ite (ihc replacementTyped (before := before) (suffix := suffix) hcontext)
        (iht replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihe replacementTyped (before := before) (suffix := suffix) hcontext)
  | letE hv hb ihv ihb =>
      rw [substAt]
      exact .letE (ihv replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihb replacementTyped (before := _ :: before) (suffix := suffix)
          (by simpa using hcontext))
  | promote hv ih =>
      rw [substAt]
      exact .promote (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | negE hv ih =>
      rw [substAt]
      exact .negE (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | negG hv ih =>
      rw [substAt]
      exact .negG (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | addE hl hr ihl ihr =>
      rw [substAt]
      exact .addE (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | addG hl hr ihl ihr =>
      rw [substAt]
      exact .addG (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | mulGE hl hr ihl ihr =>
      rw [substAt]
      exact .mulGE (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | mulGG hl hr ihl ihr =>
      rw [substAt]
      exact .mulGG (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | divEG hl hr ihl ihr =>
      rw [substAt]
      exact .divEG (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | divGG hl hr ihl ihr =>
      rw [substAt]
      exact .divGG (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | lt hl hr ihl ihr =>
      rw [substAt]
      exact .lt (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | uniform hl hr ihl ihr =>
      rw [substAt]
      exact .uniform (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | gaussian hl hr ihl ihr =>
      rw [substAt]
      exact .gaussian (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | beta hl hr ihl ihr =>
      rw [substAt]
      exact .beta (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | gamma hl hr ihl ihr =>
      rw [substAt]
      exact .gamma (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | poisson hv ih =>
      rw [substAt]
      exact .poisson (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | bernoulli hv ih =>
      rw [substAt]
      exact .bernoulli (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | discrete => rw [substAt]; exact .discrete
  | exponential hv ih =>
      rw [substAt]
      exact .exponential (ih replacementTyped (before := before) (suffix := suffix) hcontext)

theorem wellTyped_substHead (bodyTyped : WellTyped (binder :: suffix) body ty)
    (replacementTyped : WellTyped suffix replacement binder) :
    WellTyped suffix (substHead body replacement) ty := by
  simpa [substHead] using
    wellTyped_substAt (before := []) bodyTyped replacementTyped

theorem wellTyped_substTwo
    (bodyTyped : WellTyped (argumentTy :: functionTy :: suffix) body resultTy)
    (argumentTyped : WellTyped suffix argument argumentTy)
    (functionTyped : WellTyped suffix function functionTy) :
    WellTyped suffix (substTwo body argument function) resultTy := by
  rw [substTwo]
  apply wellTyped_substHead
  · exact wellTyped_substAt (before := [argumentTy]) bodyTyped functionTyped
  · exact argumentTyped

set_option maxHeartbeats 800000 in
theorem realize_skeleton (expression : AffineExpr sampleCount)
    (environment : Env sampleCount) :
    (expression.realize environment).skeleton = expression.skeleton := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
    have recurse (child : AffineExpr sampleCount) (smaller : sizeOf child < sizeOf expression) :
        (child.realize environment).skeleton = child.skeleton :=
      ih (sizeOf child) (by rwa [← sizeEq]) child rfl
    cases expression with
    | _ =>
        simp (disch := simp_wf) only [realize, skeleton, Expr.skeleton, recurse]
        all_goals repeat' first | rfl | rw [recurse _ (by simp_wf <;> omega)]

set_option maxHeartbeats 800000 in
theorem realize_coordinates (expression : AffineExpr sampleCount)
    (environment : Env sampleCount) :
    (expression.realize environment).realCoordinates =
      expression.coordinates.map (Symbolic.Affine.eval · environment) := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
    have recurse (child : AffineExpr sampleCount) (smaller : sizeOf child < sizeOf expression) :
        (child.realize environment).realCoordinates =
          child.coordinates.map (Symbolic.Affine.eval · environment) :=
      ih (sizeOf child) (by rwa [← sizeEq]) child rfl
    cases expression with
    | discrete _ _ weights =>
        simp [realize, coordinates, Expr.realCoordinates, Function.comp_def]
    | _ =>
        simp (disch := simp_wf) only [realize, coordinates, Expr.realCoordinates,
          List.map_append, List.map_nil, recurse]
        all_goals repeat' first | rfl | rw [recurse _ (by simp_wf <;> omega)]

set_option maxHeartbeats 800000 in
theorem realize_shift (expression : AffineExpr sampleCount)
    (environment : Env sampleCount) (amount cutoff : Nat) :
    (expression.shift amount cutoff).realize environment =
      (expression.realize environment).shift amount cutoff := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression cutoff with
  | h size ih =>
    have recurse (child : AffineExpr sampleCount) (childCutoff : Nat)
        (smaller : sizeOf child < sizeOf expression) :
        (child.shift amount childCutoff).realize environment =
          (child.realize environment).shift amount childCutoff :=
      ih (sizeOf child) (by rwa [← sizeEq]) child childCutoff rfl
    cases expression with
    | _ =>
        simp (disch := simp_wf) only [shift, realize, Expr.shift, Expr.mapVars, recurse]
        all_goals repeat' first | rfl | rw [recurse _ _ (by simp_wf <;> omega)]

set_option maxHeartbeats 800000 in
theorem realize_substAt (expression replacement : AffineExpr sampleCount)
    (environment : Env sampleCount) (depth : Nat) :
    (substAt depth replacement expression).realize environment =
      Expr.substAt depth (replacement.realize environment) (expression.realize environment) := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression depth with
  | h size ih =>
    have recurse (child : AffineExpr sampleCount) (childDepth : Nat)
        (smaller : sizeOf child < sizeOf expression) :
        (substAt childDepth replacement child).realize environment =
          Expr.substAt childDepth (replacement.realize environment) (child.realize environment) :=
      ih (sizeOf child) (by rwa [← sizeEq]) child childDepth rfl
    cases expression with
    | bvar index =>
        simp only [substAt, realize, Expr.substAt, Expr.mapVars]
        split <;> simp_all only [realize, realize_shift]
    | _ =>
        simp (disch := simp_wf) only [substAt, realize, Expr.substAt, Expr.mapVars, recurse]
        all_goals repeat' first | rfl | rw [recurse _ _ (by simp_wf <;> omega)]

theorem realize_substHead (body replacement : AffineExpr sampleCount)
    (environment : Env sampleCount) :
    (body.substHead replacement).realize environment =
      (body.realize environment).substHead (replacement.realize environment) :=
  realize_substAt body replacement environment 0

theorem realize_substTwo (body argument function : AffineExpr sampleCount)
    (environment : Env sampleCount) :
    (body.substTwo argument function).realize environment =
      (body.realize environment).substTwo (argument.realize environment)
        (function.realize environment) := by
  simp only [substTwo, Expr.substTwo, realize_substAt]

@[simp] theorem Affine.eval_weaken (expression : Affine n) (head : ℝ)
    (environment : Env n) :
    (Affine.weaken expression).eval (Env.cons head environment) = expression.eval environment := by
  simp only [Affine.weaken, Symbolic.Affine.eval, Fin.sum_univ_succ, Env.cons_zero,
    Env.cons_succ, Fin.cases_zero, Fin.cases_succ, zero_mul, zero_add]

@[simp] theorem Affine.eval_fresh (n : Nat) (head : ℝ) (environment : Env n) :
    (Affine.fresh n).eval (Env.cons head environment) = head := by
  simp only [Affine.fresh, Symbolic.Affine.eval, Fin.sum_univ_succ, Env.cons_zero,
    Env.cons_succ, Fin.cases_zero, Fin.cases_succ, one_mul, zero_mul, Finset.sum_const_zero,
    add_zero, zero_add]

set_option maxHeartbeats 800000 in
theorem realize_mapAffine (expression : AffineExpr n) (transform : Affine n → Affine m)
    (sourceEnvironment : Env n) (targetEnvironment : Env m)
    (eval_transform : ∀ affine,
      (transform affine).eval targetEnvironment = affine.eval sourceEnvironment) :
    (expression.mapAffine transform).realize targetEnvironment =
      expression.realize sourceEnvironment := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
    have recurse (child : AffineExpr n) (smaller : sizeOf child < sizeOf expression) :
        (child.mapAffine transform).realize targetEnvironment =
          child.realize sourceEnvironment :=
      ih (sizeOf child) (by rwa [← sizeEq]) child rfl
    cases expression with
    | real value => simp only [mapAffine, realize, eval_transform]
    | _ =>
        simp (disch := simp_wf) only [mapAffine, realize, recurse]
        all_goals repeat' first | rfl | rw [recurse _ (by simp_wf <;> omega)]

@[simp] theorem realize_weakenSamples (expression : AffineExpr n) (head : ℝ)
    (environment : Env n) :
    expression.weakenSamples.realize (Env.cons head environment) =
      expression.realize environment :=
  realize_mapAffine expression Affine.weaken environment (Env.cons head environment)
    (fun affine => Affine.eval_weaken affine head environment)

set_option maxHeartbeats 800000 in
@[simp] theorem realize_ofExpr (expression : Expr) :
    (ofExpr expression).realize Env.empty = expression := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
    have recurse (child : Expr) (smaller : sizeOf child < sizeOf expression) :
        (ofExpr child).realize Env.empty = child :=
      ih (sizeOf child) (by rwa [← sizeEq]) child rfl
    cases expression with
    | real value =>
        simp [ofExpr, realize, Symbolic.Affine.eval]
    | _ =>
        simp (disch := simp_wf) only [ofExpr, realize, recurse]
        all_goals repeat' first | rfl | rw [recurse _ (by simp_wf <;> omega)]

def isValue : AffineExpr n → Bool
  | .unit | .bool _ | .real _ | .lam _ | .fix _ | .nil => true
  | .pair left right | .cons left right => left.isValue && right.isValue
  | .inl value | .inr value => value.isValue
  | _ => false

@[simp] theorem realize_isValue (expression : AffineExpr n) (environment : Env n) :
    (expression.realize environment).isValue = expression.isValue := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
    have recurse (child : AffineExpr n) (smaller : sizeOf child < sizeOf expression) :
        (child.realize environment).isValue = child.isValue :=
      ih (sizeOf child) (by rwa [← sizeEq]) child rfl
    cases expression <;> simp (disch := simp_wf) only [realize, isValue, Expr.isValue,
      recurse]
    all_goals repeat' first | rfl | rw [recurse _ (by simp_wf <;> omega)]

def affineValue? : AffineExpr n → Option (Affine n)
  | .real value => some value
  | _ => none

noncomputable def constantValue? : AffineExpr n → Option ℝ
  | .real (constant, coefficients) => if coefficients = 0 then some constant else none
  | _ => none

theorem wellTyped_arr_value (typed : WellTyped context expression (.arr argument result))
    (value : expression.isValue = true) :
    (∃ body, expression = .lam body) ∨
      ∃ body, expression = .fix body := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_arr_value_typed
    (typed : WellTyped context expression (.arr argument result))
    (value : expression.isValue = true) :
    (∃ body, expression = .lam body ∧
      WellTyped (argument :: context) body result) ∨
    ∃ body, expression = .fix body ∧
      WellTyped (argument :: .arr argument result :: context) body result := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_prod_value_typed
    (typed : WellTyped context expression (.prod leftTy rightTy))
    (value : expression.isValue = true) :
    ∃ left right, expression = .pair left right ∧
      WellTyped context left leftTy ∧ WellTyped context right rightTy := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_sum_value_typed
    (typed : WellTyped context expression (.sum leftTy rightTy))
    (value : expression.isValue = true) :
    (∃ child, expression = .inl child ∧
      WellTyped context child leftTy) ∨
    ∃ child, expression = .inr child ∧
      WellTyped context child rightTy := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_list_value_typed
    (typed : WellTyped context expression (.list element))
    (value : expression.isValue = true) :
    expression = .nil ∨
      ∃ head tail, expression = .cons head tail ∧
        WellTyped context head element ∧ WellTyped context tail (.list element) := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_pair_inv
    (typed : WellTyped context (.pair left right)
      (.prod leftTy rightTy)) :
    WellTyped context left leftTy ∧ WellTyped context right rightTy := by
  cases typed
  exact ⟨by assumption, by assumption⟩

theorem wellTyped_inl_inv
    (typed : WellTyped context (.inl value)
      (.sum leftTy rightTy)) : WellTyped context value leftTy := by
  cases typed
  assumption

theorem wellTyped_inr_inv
    (typed : WellTyped context (.inr value)
      (.sum leftTy rightTy)) : WellTyped context value rightTy := by
  cases typed
  assumption

theorem wellTyped_cons_inv
    (typed : WellTyped context (.cons head tail) (.list element)) :
    WellTyped context head element ∧ WellTyped context tail (.list element) := by
  cases typed
  exact ⟨by assumption, by assumption⟩

theorem wellTyped_prod_value (typed : WellTyped context expression (.prod leftTy rightTy))
    (value : expression.isValue = true) :
    ∃ left right, expression = .pair left right := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_sum_value (typed : WellTyped context expression (.sum leftTy rightTy))
    (value : expression.isValue = true) :
    (∃ child, expression = .inl child) ∨
      ∃ child, expression = .inr child := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_list_value (typed : WellTyped context expression (.list element))
    (value : expression.isValue = true) :
    expression = .nil ∨
      ∃ head tail, expression = .cons head tail := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_bool_value (typed : WellTyped context expression .bool)
    (value : expression.isValue = true) :
    ∃ result, expression = .bool result := by
  cases typed <;> simp_all [isValue]

theorem wellTyped_real_value (typed : WellTyped context expression (.float mode))
    (value : expression.isValue = true) :
    ∃ coordinate, expression = .real coordinate := by
  cases typed <;> simp_all [isValue]

/-- A general-mode literal has no coefficients on the expectation-mode samples. -/
theorem wellTyped_realG_coefficients
    (typed : WellTyped context (.real value) (.float .G)) : value.2 = 0 := by
  cases typed
  assumption

theorem constantValue?_eq_some_of_wellTypedG
    (typed : WellTyped context expression (.float .G))
    (value : expression.isValue = true) :
    ∃ result, constantValue? expression = some result := by
  obtain ⟨affine, rfl⟩ := wellTyped_real_value typed value
  rcases affine with ⟨constantTerm, coefficients⟩
  have constant : coefficients = 0 := wellTyped_realG_coefficients typed
  exact ⟨constantTerm, by simp [constantValue?, constant]⟩

def Affine.neg (expression : Affine n) : Affine n :=
  (-expression.1, fun index => -expression.2 index)

def Affine.add (left right : Affine n) : Affine n :=
  (left.1 + right.1, fun index => left.2 index + right.2 index)

noncomputable def Affine.mul? (left right : Affine n) : Option (Affine n) :=
  if right.2 = 0 then some (right.1 • left)
  else if left.2 = 0 then some (left.1 • right)
  else none

noncomputable def Affine.div? (left right : Affine n) : Option (Affine n) :=
  if right.2 = 0 then some ((right.1)⁻¹ • left) else none

@[simp] theorem Affine.eval_neg (expression : Affine n) (environment : Env n) :
    (Affine.neg expression).eval environment = -expression.eval environment := by
  simp only [Affine.neg, Symbolic.Affine.eval]
  have sumRule : (∑ index, -expression.2 index * environment index) =
      -(∑ index, expression.2 index * environment index) := by
    rw [← Finset.sum_neg_distrib]
    apply Finset.sum_congr rfl
    intro index _
    ring
  rw [sumRule]
  ring

@[simp] theorem Affine.eval_add (left right : Affine n) (environment : Env n) :
    (Affine.add left right).eval environment = left.eval environment + right.eval environment := by
  simp only [Affine.add, Symbolic.Affine.eval]
  have sumRule : (∑ index, (left.2 index + right.2 index) * environment index) =
      (∑ index, left.2 index * environment index) +
        ∑ index, right.2 index * environment index := by
    rw [← Finset.sum_add_distrib]
    apply Finset.sum_congr rfl
    intro index _
    ring
  rw [sumRule]
  ring

theorem Affine.eval_mul_of_eq_some {left right result : Affine n}
    (equality : Affine.mul? left right = some result) (environment : Env n) :
    result.eval environment = left.eval environment * right.eval environment := by
  unfold Affine.mul? at equality
  split at equality
  · rename_i constant
    simp only [Option.some.injEq] at equality
    subst result
    rcases right with ⟨rightConstant, rightCoefficients⟩
    simp only at constant
    subst rightCoefficients
    simp only [Symbolic.Affine.eval, Pi.zero_apply, zero_mul, Finset.sum_const_zero,
      add_zero, Prod.smul_fst, Prod.smul_snd, Pi.smul_apply, smul_eq_mul]
    have sumRule : (∑ index, rightConstant * left.2 index * environment index) =
        rightConstant * ∑ index, left.2 index * environment index := by
      rw [Finset.mul_sum]
      apply Finset.sum_congr rfl
      intro index _
      ring
    rw [sumRule]
    ring
  · split at equality
    · rename_i constant
      simp only [Option.some.injEq] at equality
      subst result
      rcases left with ⟨leftConstant, leftCoefficients⟩
      simp only at constant
      subst leftCoefficients
      simp only [Symbolic.Affine.eval, Pi.zero_apply, zero_mul, Finset.sum_const_zero,
        add_zero, Prod.smul_fst, Prod.smul_snd, Pi.smul_apply, smul_eq_mul]
      have sumRule : (∑ index, leftConstant * right.2 index * environment index) =
          leftConstant * ∑ index, right.2 index * environment index := by
        rw [Finset.mul_sum]
        apply Finset.sum_congr rfl
        intro index _
        ring
      rw [sumRule]
      ring
    · contradiction

theorem Affine.mul?_eq_some_of_left {left right : Affine n} (constant : left.2 = 0) :
    ∃ result, Affine.mul? left right = some result := by
  unfold Affine.mul?
  split_ifs <;> first | exact ⟨_, rfl⟩ | exact absurd constant (by assumption)

theorem Affine.eval_div_of_eq_some {left right result : Affine n}
    (equality : Affine.div? left right = some result) (environment : Env n) :
    result.eval environment = left.eval environment / right.eval environment := by
  unfold Affine.div? at equality
  split at equality
  · rename_i constant
    simp only [Option.some.injEq] at equality
    subst result
    rcases right with ⟨rightConstant, rightCoefficients⟩
    simp only at constant
    subst rightCoefficients
    simp only [Symbolic.Affine.eval, Pi.zero_apply, zero_mul, Finset.sum_const_zero,
      add_zero, Prod.smul_fst, Prod.smul_snd, Pi.smul_apply, smul_eq_mul,
      div_eq_mul_inv]
    have sumRule : (∑ index, rightConstant⁻¹ * left.2 index * environment index) =
        rightConstant⁻¹ * ∑ index, left.2 index * environment index := by
      rw [Finset.mul_sum]
      apply Finset.sum_congr rfl
      intro index _
      ring
    rw [sumRule]
    ring
  · contradiction

inductive SymbolicAction
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (sampleCount : Nat) where
  | next (expression : AffineExpr sampleCount)
  | sampleE (op : Determinize.Statement.Paper.Op)
      (affineArgs : List (Affine sampleCount))
      (generalArgs : List ℝ) (continuation : AffineExpr (sampleCount + 1))
  | sampleG (site : Mode × Kind × Op) (fiber : Measure ℝ)
      (continuation : ℝ → AffineExpr sampleCount)
  | stuck

namespace SymbolicAction

noncomputable def realize (environment : Env n) : SymbolicAction laws n → Action
  | .next expression => .next (expression.realize environment)
  | .sampleE op affine general continuation =>
      .sample (.E, .stochastic, op) (primitiveFiber .stochastic op
        (affine.map (Symbolic.Affine.eval · environment)) general)
        (fun value => continuation.realize (Env.cons value environment))
  | .sampleG site fiber continuation =>
      .sample site fiber (fun value => (continuation value).realize environment)
  | .stuck => .stuck

def wrap (context : AffineExpr n → AffineExpr n) (liftedContext : AffineExpr (n + 1) → AffineExpr (n + 1)) :
    SymbolicAction laws n → SymbolicAction laws n
  | .next expression => .next (context expression)
  | .sampleE op affine general continuation =>
      .sampleE op affine general (liftedContext continuation)
  | .sampleG site fiber continuation => .sampleG site fiber (context ∘ continuation)
  | .stuck => .stuck

theorem realize_wrap (action : SymbolicAction laws n) (environment : Env n)
    (context : AffineExpr n → AffineExpr n) (liftedContext : AffineExpr (n + 1) → AffineExpr (n + 1))
    (context_realize : ∀ expression,
      (context expression).realize environment = ExprContext (expression.realize environment))
    (lifted_realize : ∀ expression value,
      (liftedContext expression).realize (Env.cons value environment) =
        ExprContext (expression.realize (Env.cons value environment))) :
    (action.wrap context liftedContext).realize environment =
      (action.realize environment).wrap ExprContext := by
  cases action with
  | next expression => simp [wrap, realize, Action.wrap, context_realize]
  | stuck => rfl
  | sampleE op affine general continuation =>
      simp only [wrap, realize, Action.wrap, Action.sample.injEq, true_and]
      funext value
      exact lifted_realize continuation value
  | sampleG site fiber continuation =>
      simp only [wrap, realize, Action.wrap, Action.sample.injEq, true_and,
        Function.comp_apply]
      funext value
      exact context_realize (continuation value)

inductive WellTyped (ty : Ty) : SymbolicAction laws n → Prop
  | next : AffineExpr.WellTyped [] expression ty → WellTyped ty (.next expression)
  | sampleE : affine.length = Determinize.Statement.Paper.affineArity op →
      general.length = Determinize.Statement.Paper.generalArity op →
      AffineExpr.WellTyped [] continuation ty →
      WellTyped ty (.sampleE op affine general continuation)
  | sampleG : (∀ value, AffineExpr.WellTyped [] (continuation value) ty) →
      WellTyped ty (.sampleG site fiber continuation)

@[simp] theorem wellTyped_next_iff :
    WellTyped ty (.next expression : SymbolicAction laws n) ↔
      AffineExpr.WellTyped [] expression ty := by
  constructor
  · intro typed; cases typed; assumption
  · exact .next

@[simp] theorem wellTyped_sampleE_iff :
    WellTyped ty (.sampleE op affine general continuation : SymbolicAction laws n) ↔
      affine.length = Determinize.Statement.Paper.affineArity op ∧
      general.length = Determinize.Statement.Paper.generalArity op ∧
      AffineExpr.WellTyped [] continuation ty := by
  constructor
  · intro typed; cases typed; exact ⟨by assumption, by assumption, by assumption⟩
  · rintro ⟨ha, hg, typed⟩; exact .sampleE ha hg typed

@[simp] theorem wellTyped_sampleG_iff :
    WellTyped ty (.sampleG site fiber continuation : SymbolicAction laws n) ↔
      ∀ value, AffineExpr.WellTyped [] (continuation value) ty := by
  constructor
  · intro typed; cases typed; assumption
  · exact .sampleG

@[simp] theorem not_wellTyped_stuck :
    ¬ WellTyped ty (.stuck : SymbolicAction laws n) := by
  intro typed
  cases typed

theorem WellTyped.wrap (typed : WellTyped childTy action)
    (contextTyped : ∀ expression, AffineExpr.WellTyped [] expression childTy →
      AffineExpr.WellTyped [] (context expression) resultTy)
    (liftedTyped : ∀ expression, AffineExpr.WellTyped [] expression childTy →
      AffineExpr.WellTyped [] (liftedContext expression) resultTy) :
    WellTyped resultTy (SymbolicAction.wrap context liftedContext action) := by
  cases typed with
  | next typed => exact .next (contextTyped _ typed)
  | sampleE ha hg typed => exact .sampleE ha hg (liftedTyped _ typed)
  | sampleG typed => exact .sampleG fun value => contextTyped _ (typed value)

end SymbolicAction

noncomputable def symbolicReduce
    (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    AffineExpr n → SymbolicAction laws n
  | expression@(.bvar _) => .stuck
  | expression@(.unit) | expression@(.bool _) | expression@(.real _)
  | expression@(.lam _) | expression@(.fix _) | expression@.nil =>
      .next expression
  | expression@(.pair left right) =>
      if left.isValue then
        if right.isValue then .next expression
        else (symbolicReduce laws right).wrap (fun next => .pair left next)
          (fun next => .pair left.weakenSamples next)
      else (symbolicReduce laws left).wrap (fun next => .pair next right)
        (fun next => .pair next right.weakenSamples)
  | expression@(.inl value) =>
      if value.isValue then .next expression
      else (symbolicReduce laws value).wrap .inl .inl
  | expression@(.inr value) =>
      if value.isValue then .next expression
      else (symbolicReduce laws value).wrap .inr .inr
  | expression@(.cons head tail) =>
      if head.isValue then
        if tail.isValue then .next expression
        else (symbolicReduce laws tail).wrap (fun next => .cons head next)
          (fun next => .cons head.weakenSamples next)
      else (symbolicReduce laws head).wrap (fun next => .cons next tail)
        (fun next => .cons next tail.weakenSamples)
  | .app function argument =>
      if function.isValue then
        if argument.isValue then
          match function with
          | .lam body => .next (body.substHead argument)
          | fix@(.fix body) => .next (body.substTwo argument fix)
          | _ => .stuck
        else (symbolicReduce laws argument).wrap (fun next => .app function next)
          (fun next => .app function.weakenSamples next)
      else (symbolicReduce laws function).wrap (fun next => .app next argument)
        (fun next => .app next argument.weakenSamples)
  | .fst pairValue =>
      if pairValue.isValue then match pairValue with
        | AffineExpr.pair left _ => .next left | _ => .stuck
      else (symbolicReduce laws pairValue).wrap .fst .fst
  | .snd pairValue =>
      if pairValue.isValue then match pairValue with
        | AffineExpr.pair _ right => .next right | _ => .stuck
      else (symbolicReduce laws pairValue).wrap .snd .snd
  | .matchSum scrutinee left right =>
      if scrutinee.isValue then
        match scrutinee with
        | .inl value => .next (left.substHead value)
        | .inr value => .next (right.substHead value)
        | _ => .stuck
      else (symbolicReduce laws scrutinee).wrap
        (fun next => .matchSum next left right)
        (fun next => .matchSum next left.weakenSamples right.weakenSamples)
  | .matchList scrutinee nilCase consCase =>
      if scrutinee.isValue then
        match scrutinee with
        | .nil => .next nilCase
        | .cons head tail => .next (consCase.substTwo head tail)
        | _ => .stuck
      else (symbolicReduce laws scrutinee).wrap
        (fun next => .matchList next nilCase consCase)
        (fun next => .matchList next nilCase.weakenSamples consCase.weakenSamples)
  | .ite condition thenBranch elseBranch =>
      if condition.isValue then
        match condition with
        | .bool true => .next thenBranch
        | .bool false => .next elseBranch
        | _ => .stuck
      else (symbolicReduce laws condition).wrap
        (fun next => .ite next thenBranch elseBranch)
        (fun next => .ite next thenBranch.weakenSamples elseBranch.weakenSamples)
  | .letE value body =>
      if value.isValue then .next (body.substHead value)
      else (symbolicReduce laws value).wrap (fun next => .letE next body)
        (fun next => .letE next body.weakenSamples)
  | .promote body =>
      if body.isValue then match body with
        | .real value => .next (.real value) | _ => .stuck
      else (symbolicReduce laws body).wrap .promote .promote
  | .neg body =>
      if body.isValue then match body with
        | .real value => .next (.real (Affine.neg value)) | _ => .stuck
      else (symbolicReduce laws body).wrap .neg .neg
  | .add left right =>
      if left.isValue then
        if right.isValue then match left.affineValue?, right.affineValue? with
          | some x, some y => .next (.real (Affine.add x y)) | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.add left)
          (.add left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .add next right)
        (fun next => .add next right.weakenSamples)
  | .mul left right =>
      if left.isValue then
        if right.isValue then match left.affineValue?, right.affineValue? with
          | some x, some y => match Affine.mul? x y with
            | some result => .next (.real result) | none => .stuck
          | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.mul left)
          (.mul left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .mul next right)
        (fun next => .mul next right.weakenSamples)
  | .div left right =>
      if left.isValue then
        if right.isValue then match left.affineValue?, right.affineValue? with
          | some x, some y => match Affine.div? x y with
            | some result => .next (.real result) | none => .stuck
          | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.div left)
          (.div left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .div next right)
        (fun next => .div next right.weakenSamples)
  | .lt left right =>
      if left.isValue then
        if right.isValue then match left.constantValue?, right.constantValue? with
          | some x, some y => .next (.bool (x < y)) | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.lt left) (.lt left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .lt next right)
        (fun next => .lt next right.weakenSamples)
  | .uniform mode kind lower upper =>
      if lower.isValue then
        if upper.isValue then match mode, kind with
          | .E, .stochastic => match lower.affineValue?, upper.affineValue? with
            | some x, some y => .sampleE .uniform [x, y] [] (.real (Affine.fresh n))
            | _, _ => .stuck
          | _, _ => match lower.constantValue?, upper.constantValue? with
            | some x, some y =>
                .sampleG (mode, kind, .uniform) (uniformFiber kind x y)
                  (fun value => .real (value, 0))
            | _, _ => .stuck
        else (symbolicReduce laws upper).wrap (.uniform mode kind lower)
          (.uniform mode kind lower.weakenSamples)
      else (symbolicReduce laws lower).wrap (fun next => .uniform mode kind next upper)
        (fun next => .uniform mode kind next upper.weakenSamples)
  | .gaussian mode kind mean variance =>
      if mean.isValue then
        if variance.isValue then match mode, kind with
          | .E, .stochastic => match mean.affineValue?, variance.constantValue? with
            | some x, some y => .sampleE .gaussian [x] [y] (.real (Affine.fresh n))
            | _, _ => .stuck
          | _, _ => match mean.constantValue?, variance.constantValue? with
            | some x, some y =>
                .sampleG (mode, kind, .gaussian) (gaussianFiber kind x y)
                  (fun value => .real (value, 0))
            | _, _ => .stuck
        else (symbolicReduce laws variance).wrap (.gaussian mode kind mean)
          (.gaussian mode kind mean.weakenSamples)
      else (symbolicReduce laws mean).wrap (fun next => .gaussian mode kind next variance)
        (fun next => .gaussian mode kind next variance.weakenSamples)
  | .poisson mode kind rate =>
      if rate.isValue then match mode, kind with
        | .E, .stochastic => match rate.affineValue? with
          | some x => .sampleE .poisson [x] [] (.real (Affine.fresh n))
          | none => .stuck
        | _, _ => match rate.constantValue? with
          | some x =>
              .sampleG (mode, kind, .poisson) (poissonFiber kind x)
                (fun value => .real (value, 0))
          | none => .stuck
      else (symbolicReduce laws rate).wrap (.poisson mode kind) (.poisson mode kind)
  | .bernoulli mode kind probability =>
      if probability.isValue then match mode, kind with
        | .E, .stochastic => match probability.affineValue? with
          | some x => .sampleE .bernoulli [x] [] (.real (Affine.fresh n))
          | none => .stuck
        | _, _ => match probability.constantValue? with
          | some x =>
              .sampleG (mode, kind, .bernoulli) (bernoulliFiber kind x)
                (fun value => .real (value, 0))
          | none => .stuck
      else (symbolicReduce laws probability).wrap (.bernoulli mode kind) (.bernoulli mode kind)
  | .discrete mode kind weights =>
      match mode, kind with
      | .E, .stochastic => .sampleE (.discrete weights.length) [] weights (.real (Affine.fresh n))
      | _, _ =>
          .sampleG (mode, kind, .discrete weights.length) (discreteFiber kind weights)
            (fun value => .real (value, 0))
  | .exponential mode kind rate =>
      if rate.isValue then match mode, kind with
        | .E, .stochastic => match rate.constantValue? with
          | some x => .sampleE .exponential [] [x] (.real (Affine.fresh n))
          | none => .stuck
        | _, _ => match rate.constantValue? with
          | some x =>
              .sampleG (mode, kind, .exponential) (exponentialFiber kind x)
                (fun value => .real (value, 0))
          | none => .stuck
      else (symbolicReduce laws rate).wrap (.exponential mode kind) (.exponential mode kind)
  | .beta mode kind alpha betaParam =>
      if alpha.isValue then
        if betaParam.isValue then match mode, kind with
          | .E, .stochastic => match alpha.constantValue?, betaParam.constantValue? with
            | some x, some y => .sampleE .beta [] [x, y] (.real (Affine.fresh n))
            | _, _ => .stuck
          | _, _ => match alpha.constantValue?, betaParam.constantValue? with
            | some x, some y =>
                .sampleG (mode, kind, .beta) (betaFiber kind x y) (fun value => .real (value, 0))
            | _, _ => .stuck
        else (symbolicReduce laws betaParam).wrap (.beta mode kind alpha)
          (.beta mode kind alpha.weakenSamples)
      else (symbolicReduce laws alpha).wrap (fun next => .beta mode kind next betaParam)
        (fun next => .beta mode kind next betaParam.weakenSamples)
  | .gamma mode kind shape rate =>
      if shape.isValue then
        if rate.isValue then match mode, kind with
          | .E, .stochastic => match shape.affineValue?, rate.constantValue? with
            | some x, some y => .sampleE .gamma [x] [y] (.real (Affine.fresh n))
            | _, _ => .stuck
          | _, _ => match shape.constantValue?, rate.constantValue? with
            | some x, some y =>
                .sampleG (mode, kind, .gamma) (gammaFiber kind x y) (fun value => .real (value, 0))
            | _, _ => .stuck
        else (symbolicReduce laws rate).wrap (.gamma mode kind shape)
          (.gamma mode kind shape.weakenSamples)
      else (symbolicReduce laws shape).wrap (fun next => .gamma mode kind next rate)
        (fun next => .gamma mode kind next rate.weakenSamples)

theorem symbolicReduce_app_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (function operand : AffineExpr n) : symbolicReduce laws (.app function operand) =
    if function.isValue then
      if operand.isValue then
        match function with
        | .lam body => .next (body.substHead operand)
        | fix@(.fix body) => .next (body.substTwo operand fix)
        | _ => .stuck
      else (symbolicReduce laws operand).wrap (fun next => .app function next)
        (fun next => .app function.weakenSamples next)
    else (symbolicReduce laws function).wrap (fun next => .app next operand)
      (fun next => .app next operand.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_fst_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (pairValue : AffineExpr n) : symbolicReduce laws (.fst pairValue) =
    if pairValue.isValue then match pairValue with
      | .pair left _ => .next left | _ => .stuck
    else (symbolicReduce laws pairValue).wrap .fst .fst := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_snd_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (pairValue : AffineExpr n) : symbolicReduce laws (.snd pairValue) =
    if pairValue.isValue then match pairValue with
      | .pair _ right => .next right | _ => .stuck
    else (symbolicReduce laws pairValue).wrap .snd .snd := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_matchSum_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (scrutinee left right : AffineExpr n) :
    symbolicReduce laws (.matchSum scrutinee left right) =
      if scrutinee.isValue then match scrutinee with
        | .inl value => .next (left.substHead value)
        | .inr value => .next (right.substHead value)
        | _ => .stuck
      else (symbolicReduce laws scrutinee).wrap
        (fun next => .matchSum next left right)
        (fun next => .matchSum next left.weakenSamples right.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_matchList_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (scrutinee nilCase consCase : AffineExpr n) :
    symbolicReduce laws (.matchList scrutinee nilCase consCase) =
      if scrutinee.isValue then match scrutinee with
        | .nil => .next nilCase
        | .cons head tail => .next (consCase.substTwo head tail)
        | _ => .stuck
      else (symbolicReduce laws scrutinee).wrap
        (fun next => .matchList next nilCase consCase)
        (fun next => .matchList next nilCase.weakenSamples
          consCase.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_ite_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (condition thenBranch elseBranch : AffineExpr n) :
    symbolicReduce laws (.ite condition thenBranch elseBranch) =
      if condition.isValue then match condition with
        | .bool true => .next thenBranch
        | .bool false => .next elseBranch
        | _ => .stuck
      else (symbolicReduce laws condition).wrap
        (fun next => .ite next thenBranch elseBranch)
        (fun next => .ite next thenBranch.weakenSamples elseBranch.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_promote_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (body : AffineExpr n) : symbolicReduce laws (.promote body) =
    if body.isValue then match body with
      | .real value => .next (.real value) | _ => .stuck
    else (symbolicReduce laws body).wrap .promote .promote := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_let_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (value body : AffineExpr n) :
    symbolicReduce laws (.letE value body) =
    if value.isValue then .next (body.substHead value)
    else (symbolicReduce laws value).wrap (fun next => .letE next body)
      (fun next => .letE next body.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_uniform_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (lower upper : AffineExpr n) :
    symbolicReduce laws (.uniform mode kind lower upper) =
          if lower.isValue then
            if upper.isValue then match mode, kind with
              | .E, .stochastic => match lower.affineValue?, upper.affineValue? with
                | some x, some y => .sampleE .uniform [x, y] [] (.real (Affine.fresh n))
                | _, _ => .stuck
              | _, _ => match lower.constantValue?, upper.constantValue? with
                | some x, some y =>
                    .sampleG (mode, kind, .uniform) (uniformFiber kind x y)
                      (fun value => .real (value, 0))
                | _, _ => .stuck
            else (symbolicReduce laws upper).wrap (.uniform mode kind lower)
              (.uniform mode kind lower.weakenSamples)
          else (symbolicReduce laws lower).wrap (fun next => .uniform mode kind next upper)
            (fun next => .uniform mode kind next upper.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_gaussian_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (mean variance : AffineExpr n) :
    symbolicReduce laws (.gaussian mode kind mean variance) =
          if mean.isValue then
            if variance.isValue then match mode, kind with
              | .E, .stochastic => match mean.affineValue?, variance.constantValue? with
                | some x, some y => .sampleE .gaussian [x] [y] (.real (Affine.fresh n))
                | _, _ => .stuck
              | _, _ => match mean.constantValue?, variance.constantValue? with
                | some x, some y =>
                    .sampleG (mode, kind, .gaussian) (gaussianFiber kind x y)
                      (fun value => .real (value, 0))
                | _, _ => .stuck
            else (symbolicReduce laws variance).wrap (.gaussian mode kind mean)
              (.gaussian mode kind mean.weakenSamples)
          else (symbolicReduce laws mean).wrap (fun next => .gaussian mode kind next variance)
            (fun next => .gaussian mode kind next variance.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_poisson_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (rate : AffineExpr n) :
    symbolicReduce laws (.poisson mode kind rate) =
          if rate.isValue then match mode, kind with
            | .E, .stochastic => match rate.affineValue? with
              | some x => .sampleE .poisson [x] [] (.real (Affine.fresh n))
              | none => .stuck
            | _, _ => match rate.constantValue? with
              | some x =>
                  .sampleG (mode, kind, .poisson) (poissonFiber kind x)
                    (fun value => .real (value, 0))
              | none => .stuck
          else (symbolicReduce laws rate).wrap (.poisson mode kind) (.poisson mode kind) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_bernoulli_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (probability : AffineExpr n) :
    symbolicReduce laws (.bernoulli mode kind probability) =
          if probability.isValue then match mode, kind with
            | .E, .stochastic => match probability.affineValue? with
              | some x => .sampleE .bernoulli [x] [] (.real (Affine.fresh n))
              | none => .stuck
            | _, _ => match probability.constantValue? with
              | some x =>
                  .sampleG (mode, kind, .bernoulli) (bernoulliFiber kind x)
                    (fun value => .real (value, 0))
              | none => .stuck
          else (symbolicReduce laws probability).wrap (.bernoulli mode kind)
            (.bernoulli mode kind) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_discrete_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (weights : List ℝ) :
    symbolicReduce laws (.discrete mode kind weights : AffineExpr n) =
      match mode, kind with
      | .E, .stochastic => .sampleE (.discrete weights.length) [] weights (.real (Affine.fresh n))
      | _, _ =>
          .sampleG (mode, kind, .discrete weights.length) (discreteFiber kind weights)
            (fun value => .real (value, 0)) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_exponential_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (rate : AffineExpr n) :
    symbolicReduce laws (.exponential mode kind rate) =
          if rate.isValue then match mode, kind with
            | .E, .stochastic => match rate.constantValue? with
              | some x => .sampleE .exponential [] [x] (.real (Affine.fresh n))
              | none => .stuck
            | _, _ => match rate.constantValue? with
              | some x =>
                  .sampleG (mode, kind, .exponential) (exponentialFiber kind x)
                    (fun value => .real (value, 0))
              | none => .stuck
          else (symbolicReduce laws rate).wrap (.exponential mode kind)
            (.exponential mode kind) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_beta_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (alpha betaParam : AffineExpr n) :
    symbolicReduce laws (.beta mode kind alpha betaParam) =
          if alpha.isValue then
            if betaParam.isValue then match mode, kind with
              | .E, .stochastic => match alpha.constantValue?, betaParam.constantValue? with
                | some x, some y => .sampleE .beta [] [x, y] (.real (Affine.fresh n))
                | _, _ => .stuck
              | _, _ => match alpha.constantValue?, betaParam.constantValue? with
                | some x, some y =>
                    .sampleG (mode, kind, .beta) (betaFiber kind x y)
                      (fun value => .real (value, 0))
                | _, _ => .stuck
            else (symbolicReduce laws betaParam).wrap (.beta mode kind alpha)
              (.beta mode kind alpha.weakenSamples)
          else (symbolicReduce laws alpha).wrap (fun next => .beta mode kind next betaParam)
            (fun next => .beta mode kind next betaParam.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_gamma_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (kind : Kind) (shape rate : AffineExpr n) :
    symbolicReduce laws (.gamma mode kind shape rate) =
          if shape.isValue then
            if rate.isValue then match mode, kind with
              | .E, .stochastic => match shape.affineValue?, rate.constantValue? with
                | some x, some y => .sampleE .gamma [x] [y] (.real (Affine.fresh n))
                | _, _ => .stuck
              | _, _ => match shape.constantValue?, rate.constantValue? with
                | some x, some y =>
                    .sampleG (mode, kind, .gamma) (gammaFiber kind x y)
                      (fun value => .real (value, 0))
                | _, _ => .stuck
            else (symbolicReduce laws rate).wrap (.gamma mode kind shape)
              (.gamma mode kind shape.weakenSamples)
          else (symbolicReduce laws shape).wrap (fun next => .gamma mode kind next rate)
            (fun next => .gamma mode kind next rate.weakenSamples) := by
  rw [symbolicReduce.eq_def]

set_option maxHeartbeats 800000 in
theorem symbolicReduce_realize
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {expression : AffineExpr n} (typed : WellTyped context expression ty)
    (environment : Env n) :
    (symbolicReduce laws expression).realize environment =
      reduce (expression.realize environment) := by
  induction typed generalizing environment with
  | bvar hvar => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | «unit» => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | bool => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | realE => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | realG => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | lam => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | fix => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | nil => simp [symbolicReduce, SymbolicAction.realize, realize, reduce]
  | pair leftTyped rightTyped ihl ihr =>
      rename_i context' left leftTy right rightTy
      rw [symbolicReduce, realize, reduce, realize_isValue]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, Bool.true_eq, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp [rightValue, SymbolicAction.realize, realize]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .pair
              (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue,
            if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .pair
            next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | inl valueTyped ih =>
      rename_i context' value leftTy rightTy
      rw [symbolicReduce, realize, reduce, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [valueIsValue, SymbolicAction.realize, realize]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .inl next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih environment]
  | inr valueTyped ih =>
      rename_i context' value rightTy leftTy
      rw [symbolicReduce, realize, reduce, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [valueIsValue, SymbolicAction.realize, realize]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .inr next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih environment]
  | cons headTyped tailTyped ihh iht =>
      rename_i context' head element tail
      rw [symbolicReduce, realize, reduce, realize_isValue]
      by_cases headValue : head.isValue = true
      · simp only [headValue, Bool.true_eq, ↓reduceIte]
        by_cases tailValue : tail.isValue = true
        · simp [tailValue, SymbolicAction.realize, realize]
        · simp only [tailValue, Bool.eq_false_of_not_eq_true tailValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .cons (head.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            iht environment, realize_isValue,
            if_neg tailValue]
      · simp only [headValue, Bool.eq_false_of_not_eq_true headValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .cons next (tail.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihh environment]
  | app functionTyped operandTyped ihf iho =>
      rename_i context' function argumentTy result operand
      rw [realize, MeasurableActionFamily.reduce_app_eq, realize_isValue]
      by_cases functionValue : function.isValue = true
      · simp only [functionValue, ↓reduceIte]
        by_cases operandValue : operand.isValue = true
        · rcases wellTyped_arr_value functionTyped functionValue with ⟨body, rfl⟩ | ⟨body, rfl⟩
          · simp [symbolicReduce, functionValue, operandValue, SymbolicAction.realize,
              realize, realize_substHead]
          · simp [symbolicReduce, functionValue, operandValue, SymbolicAction.realize,
              realize, realize_substTwo]
        · rw [symbolicReduce_app_eq]
          simp only [functionValue, operandValue,
            Bool.eq_false_of_not_eq_true operandValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .app (function.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            iho environment, realize_isValue, if_neg operandValue]
          all_goals simp_all [isValue]
      · rw [symbolicReduce_app_eq]
        simp only [functionValue, Bool.eq_false_of_not_eq_true functionValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .app next (operand.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihf environment]
        all_goals simp_all [isValue]
  | fst pairTyped ih =>
      rename_i context' pairValue leftTy rightTy
      rw [realize, MeasurableActionFamily.reduce_fst_eq, realize_isValue]
      by_cases pairIsValue : pairValue.isValue = true
      · simp only [pairIsValue, ↓reduceIte]
        obtain ⟨left, right, rfl⟩ := wellTyped_prod_value pairTyped pairIsValue
        simp [symbolicReduce, pairIsValue, SymbolicAction.realize, realize]
      · rw [symbolicReduce_fst_eq]
        simp only [pairIsValue, Bool.eq_false_of_not_eq_true pairIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .fst next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih environment]
        all_goals simp_all [isValue]
  | snd pairTyped ih =>
      rename_i context' pairValue leftTy rightTy
      rw [realize, MeasurableActionFamily.reduce_snd_eq, realize_isValue]
      by_cases pairIsValue : pairValue.isValue = true
      · simp only [pairIsValue, ↓reduceIte]
        obtain ⟨left, right, rfl⟩ := wellTyped_prod_value pairTyped pairIsValue
        simp [symbolicReduce, pairIsValue, SymbolicAction.realize, realize]
      · rw [symbolicReduce_snd_eq]
        simp only [pairIsValue, Bool.eq_false_of_not_eq_true pairIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .snd next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih environment]
        all_goals simp_all [isValue]
  | matchSum scrutineeTyped leftTyped rightTyped ihs ihl ihr =>
      rename_i context' scrutinee leftTy rightTy left result right
      rw [realize, MeasurableActionFamily.reduce_matchSum_eq, realize_isValue]
      by_cases scrutineeValue : scrutinee.isValue = true
      · simp only [scrutineeValue, ↓reduceIte]
        rcases wellTyped_sum_value scrutineeTyped scrutineeValue with ⟨child, rfl⟩ | ⟨child, rfl⟩
        · simp [symbolicReduce, scrutineeValue, SymbolicAction.realize, realize,
            realize_substHead]
        · simp [symbolicReduce, scrutineeValue, SymbolicAction.realize, realize,
            realize_substHead]
      · rw [symbolicReduce_matchSum_eq]
        simp only [scrutineeValue,
          Bool.eq_false_of_not_eq_true scrutineeValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .matchSum next
            (left.realize environment) (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihs environment]
        all_goals simp_all [isValue]
  | matchList scrutineeTyped nilTyped consTyped ihs ihn ihc =>
      rename_i context' scrutinee element nilCase result consCase
      rw [realize, MeasurableActionFamily.reduce_matchList_eq, realize_isValue]
      by_cases scrutineeValue : scrutinee.isValue = true
      · simp only [scrutineeValue, ↓reduceIte]
        rcases wellTyped_list_value scrutineeTyped scrutineeValue with equality | ⟨head, tail, equality⟩
        · subst scrutinee
          simp [symbolicReduce, scrutineeValue, SymbolicAction.realize, realize]
        · subst scrutinee
          simp [symbolicReduce, scrutineeValue, SymbolicAction.realize, realize,
            realize_substTwo]
      · rw [symbolicReduce_matchList_eq]
        simp only [scrutineeValue,
          Bool.eq_false_of_not_eq_true scrutineeValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .matchList next
            (nilCase.realize environment) (consCase.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihs environment]
        all_goals simp_all [isValue]
  | ite conditionTyped thenTyped elseTyped ihc iht ihe =>
      rename_i context' condition thenBranch result elseBranch
      rw [realize, MeasurableActionFamily.reduce_ite_eq, realize_isValue]
      by_cases conditionValue : condition.isValue = true
      · simp only [conditionValue, ↓reduceIte]
        obtain ⟨answer, rfl⟩ := wellTyped_bool_value conditionTyped conditionValue
        cases answer <;> simp [symbolicReduce, conditionValue, SymbolicAction.realize, realize]
      · rw [symbolicReduce_ite_eq]
        simp only [conditionValue,
          Bool.eq_false_of_not_eq_true conditionValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .ite next
            (thenBranch.realize environment) (elseBranch.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihc environment]
        all_goals simp_all [isValue]
  | letE valueTyped bodyTyped ihv ihb =>
      rename_i context' value valueTy body result
      rw [realize, MeasurableActionFamily.reduce_let_eq, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [symbolicReduce, valueIsValue, SymbolicAction.realize, realize_substHead]
      · rw [symbolicReduce_let_eq]
        simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .letE next (body.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihv environment]
        all_goals simp_all [isValue]
  | promote valueTyped ih =>
      rename_i context' value
      rw [realize, MeasurableActionFamily.reduce_promote_eq, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [symbolicReduce, valueIsValue, SymbolicAction.realize, realize]
      · rw [symbolicReduce_promote_eq]
        simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .promote next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih environment]
        all_goals simp_all [isValue]
  | negE valueTyped ih =>
      rename_i context' value
      rw [realize, MeasurableActionFamily.reduce_neg_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [SymbolicAction.realize, realize, Affine.eval_neg]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .neg next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih environment]
        all_goals simp_all [isValue]
  | negG valueTyped ih =>
      rename_i context' value
      rw [realize, MeasurableActionFamily.reduce_neg_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [SymbolicAction.realize, realize, Affine.eval_neg]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .neg next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih environment]
        all_goals simp_all [isValue]
  | addE leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, MeasurableActionFamily.reduce_add_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          simp [affineValue?, SymbolicAction.realize, realize, Expr.isValue,
            realValue?, Affine.eval_add]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .add (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .add next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | addG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, MeasurableActionFamily.reduce_add_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          simp [affineValue?, SymbolicAction.realize, realize, Expr.isValue,
            realValue?, Affine.eval_add]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .add (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .add next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | mulGE leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, MeasurableActionFamily.reduce_mul_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          have leftZero : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain ⟨result, product⟩ :=
            Affine.mul?_eq_some_of_left (left := (x0, xc)) (right := y) leftZero
          simp [affineValue?, product, SymbolicAction.realize, realize,
            Expr.isValue, realValue?, Affine.eval_mul_of_eq_some product]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .mul (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .mul next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | mulGG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, MeasurableActionFamily.reduce_mul_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          rcases y with ⟨y0, yc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, Affine.mul?, SymbolicAction.realize, realize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero]
          ring
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .mul (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .mul next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | divEG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, MeasurableActionFamily.reduce_div_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases y with ⟨y0, yc⟩
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, Affine.div?, SymbolicAction.realize, realize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero,
            div_eq_mul_inv]
          have sumRule : (∑ i, y0⁻¹ * x.2 i * environment i) =
              y0⁻¹ * ∑ i, x.2 i * environment i := by
            rw [Finset.mul_sum]
            apply Finset.sum_congr rfl
            intro i _
            ring
          rw [sumRule]
          ring
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .div (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .div next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | divGG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, MeasurableActionFamily.reduce_div_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          rcases y with ⟨y0, yc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, Affine.div?, SymbolicAction.realize, realize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero,
            div_eq_mul_inv]
          ring
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .div (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .div next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | lt leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, MeasurableActionFamily.reduce_lt_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          rcases y with ⟨y0, yc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue,
            realValue?, Symbolic.Affine.eval, Finset.sum_const_zero]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .lt (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .lt next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | uniform leftTyped rightTyped ihl ihr =>
      rename_i context' left mode right
      rw [realize, MeasurableActionFamily.reduce_uniform_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases mode with
          | E =>
              simp [affineValue?, SymbolicAction.realize, realize, Expr.isValue,
                realValue?, uniformFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue, realValue?]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .uniform mode .stochastic (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .uniform mode .stochastic next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | gaussian leftTyped rightTyped ihl ihr =>
      rename_i context' left mode right
      rw [realize, MeasurableActionFamily.reduce_gaussian_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases mode with
          | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [affineValue?, constantValue?, SymbolicAction.realize, realize,
                Expr.isValue, realValue?, gaussianFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue, realValue?]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .gaussian mode .stochastic (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .gaussian mode .stochastic next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | poisson valueTyped ih =>
      rename_i context' value mode
      rw [realize, MeasurableActionFamily.reduce_poisson_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        cases mode with
        | E =>
              simp [affineValue?, SymbolicAction.realize, realize, Expr.isValue,
                realValue?, poissonFiber_eq, Affine.eval_fresh]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue, realValue?]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .poisson mode .stochastic next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ih environment]
  | bernoulli valueTyped ih =>
      rename_i context' value mode
      rw [realize, MeasurableActionFamily.reduce_bernoulli_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        cases mode with
        | E =>
              simp [affineValue?, SymbolicAction.realize, realize, Expr.isValue,
                realValue?, bernoulliFiber_eq, Affine.eval_fresh]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue, realValue?]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .bernoulli mode .stochastic next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ih environment]
  | discrete =>
      rename_i context' mode weights
      rw [realize, MeasurableActionFamily.reduce_discrete_eq, symbolicReduce_discrete_eq]
      cases mode with
      | E =>
          simp [SymbolicAction.realize, realize, Expr.isValue, realValue?, discreteFiber_eq,
            Affine.eval_fresh]
      | G => simp [SymbolicAction.realize, realize]
  | exponential valueTyped ih =>
      rename_i context' value mode
      rw [realize, MeasurableActionFamily.reduce_exponential_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        cases mode with
        | E =>
              rcases x with ⟨x0, xc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
              simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue,
                realValue?, exponentialFiber_eq, Affine.eval_fresh]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue, realValue?]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .exponential mode .stochastic next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ih environment]
  | beta leftTyped rightTyped ihl ihr =>
      rename_i context' left right mode
      rw [realize, MeasurableActionFamily.reduce_beta_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases mode with
          | E =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue,
                realValue?, betaFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue, realValue?]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .beta mode .stochastic (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .beta mode .stochastic next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]
  | gamma leftTyped rightTyped ihl ihr =>
      rename_i context' left mode right
      rw [realize, MeasurableActionFamily.reduce_gamma_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases mode with
          | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [affineValue?, constantValue?, SymbolicAction.realize, realize,
                Expr.isValue, realValue?, gammaFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue, realValue?]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .gamma mode .stochastic (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .gamma mode .stochastic next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl environment]

set_option maxHeartbeats 1600000 in
set_option maxRecDepth 4000 in
theorem symbolicReduce_wellTyped
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {expression : AffineExpr n} (typed : WellTyped [] expression ty) :
    SymbolicAction.WellTyped ty (symbolicReduce laws expression) := by
  generalize hcontext : ([] : List Ty) = context at typed
  induction typed
  case uniform left mode right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
        cases mode with
        | E =>
              simp only [affineValue?]
              exact .sampleE rfl rfl .realE
        | G =>
            rcases x with ⟨x0, xc⟩
            rcases y with ⟨y0, yc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
            obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
            simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
            exact .sampleG fun value => .realG rfl
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .uniform leftTyped nextTyped)
          (fun next nextTyped => .uniform leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .uniform nextTyped rightTyped)
        (fun next nextTyped => .uniform nextTyped rightTyped.weakenSamples)
  case gaussian left mode right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
        cases mode with
        | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp only [affineValue?, constantValue?, eq_self_iff_true, ↓reduceIte]
              exact .sampleE rfl rfl .realE
        | G =>
            rcases x with ⟨x0, xc⟩
            rcases y with ⟨y0, yc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
            obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
            simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
            exact .sampleG fun value => .realG rfl
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .gaussian leftTyped nextTyped)
          (fun next nextTyped => .gaussian leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .gaussian nextTyped rightTyped)
        (fun next nextTyped => .gaussian nextTyped rightTyped.weakenSamples)
  case poisson value mode valueTyped ih =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped isValue
      cases mode with
      | E =>
            simp only [affineValue?]
            exact .sampleE rfl rfl .realE
      | G =>
          rcases x with ⟨x0, xc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
          simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
          exact .sampleG fun value => .realG rfl
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .poisson nextTyped)
        (fun next nextTyped => .poisson nextTyped)
  case bernoulli value mode valueTyped ih =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped isValue
      cases mode with
      | E =>
            simp only [affineValue?]
            exact .sampleE rfl rfl .realE
      | G =>
          rcases x with ⟨x0, xc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
          simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
          exact .sampleG fun value => .realG rfl
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .bernoulli nextTyped)
        (fun next nextTyped => .bernoulli nextTyped)
  case discrete mode weights =>
    cases hcontext
    rw [symbolicReduce_discrete_eq]
    cases mode with
    | E => exact .sampleE rfl rfl .realE
    | G => exact .sampleG fun value => .realG rfl
  case exponential value mode valueTyped ih =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped isValue
      cases mode with
      | E =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
            exact .sampleE rfl rfl .realE
      | G =>
          rcases x with ⟨x0, xc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
          simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
          exact .sampleG fun value => .realG rfl
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .exponential nextTyped)
        (fun next nextTyped => .exponential nextTyped)
  case beta left right mode leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
        cases mode with
        | E =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
              exact .sampleE rfl rfl .realE
        | G =>
            rcases x with ⟨x0, xc⟩
            rcases y with ⟨y0, yc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
            obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
            simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
            exact .sampleG fun value => .realG rfl
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .beta leftTyped nextTyped)
          (fun next nextTyped => .beta leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .beta nextTyped rightTyped)
        (fun next nextTyped => .beta nextTyped rightTyped.weakenSamples)
  case gamma left mode right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
        cases mode with
        | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp only [affineValue?, constantValue?, eq_self_iff_true, ↓reduceIte]
              exact .sampleE rfl rfl .realE
        | G =>
            rcases x with ⟨x0, xc⟩
            rcases y with ⟨y0, yc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
            obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
            simp only [constantValue?, eq_self_iff_true, ↓reduceIte]
            exact .sampleG fun value => .realG rfl
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .gamma leftTyped nextTyped)
          (fun next nextTyped => .gamma leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .gamma nextTyped rightTyped)
        (fun next nextTyped => .gamma nextTyped rightTyped.weakenSamples)

  case app functionTyped argumentTyped ihf iha =>
    cases hcontext
    rename_i function argumentTy result operand
    rw [symbolicReduce_app_eq]
    by_cases functionValue : function.isValue = true
    · simp only [functionValue, ↓reduceIte]
      by_cases argumentValue : operand.isValue = true
      · simp only [argumentValue, ↓reduceIte]
        rcases wellTyped_arr_value_typed functionTyped functionValue with function | function
        · rcases function with ⟨body, rfl, bodyTyped⟩
          exact .next (wellTyped_substHead bodyTyped argumentTyped)
        · rcases function with ⟨body, rfl, bodyTyped⟩
          exact .next (wellTyped_substTwo bodyTyped argumentTyped (.fix bodyTyped))
      · simp only [argumentValue, ↓reduceIte]
        exact (iha rfl).wrap
          (fun next nextTyped => .app functionTyped nextTyped)
          (fun next nextTyped => .app functionTyped.weakenSamples nextTyped)
    · simp only [functionValue, ↓reduceIte]
      exact (ihf rfl).wrap
        (fun next nextTyped => .app nextTyped argumentTyped)
        (fun next nextTyped => .app nextTyped argumentTyped.weakenSamples)
  case fst pairTyped ih =>
    cases hcontext
    rename_i pairValue leftTy rightTy
    rw [symbolicReduce_fst_eq]
    by_cases value : pairValue.isValue = true
    · simp only [value, ↓reduceIte]
      obtain ⟨left, right, rfl, leftTyped, rightTyped⟩ :=
        wellTyped_prod_value_typed pairTyped value
      exact .next leftTyped
    · simp only [value, ↓reduceIte]
      exact (ih rfl).wrap (fun next nextTyped => .fst nextTyped)
        (fun next nextTyped => .fst nextTyped)
  case snd pairTyped ih =>
    cases hcontext
    rename_i pairValue leftTy rightTy
    rw [symbolicReduce_snd_eq]
    by_cases value : pairValue.isValue = true
    · simp only [value, ↓reduceIte]
      obtain ⟨left, right, rfl, leftTyped, rightTyped⟩ :=
        wellTyped_prod_value_typed pairTyped value
      exact .next rightTyped
    · simp only [value, ↓reduceIte]
      exact (ih rfl).wrap (fun next nextTyped => .snd nextTyped)
        (fun next nextTyped => .snd nextTyped)
  case matchSum context scrutinee leftTy rightTy left result right
      scrutineeTyped leftTyped rightTyped ih _ _ =>
    cases hcontext
    rw [symbolicReduce_matchSum_eq]
    by_cases value : scrutinee.isValue = true
    · simp only [value, ↓reduceIte]
      rcases wellTyped_sum_value_typed scrutineeTyped value with left | right
      · rcases left with ⟨child, rfl, childTyped⟩
        exact .next (wellTyped_substHead leftTyped childTyped)
      · rcases right with ⟨child, rfl, childTyped⟩
        exact .next (wellTyped_substHead rightTyped childTyped)
    · simp only [value, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .matchSum nextTyped leftTyped rightTyped)
        (fun next nextTyped => .matchSum nextTyped leftTyped.weakenSamples
          rightTyped.weakenSamples)
  case matchList context scrutinee element nilCase result consCase
      scrutineeTyped nilTyped consTyped ih _ _ =>
    cases hcontext
    rw [symbolicReduce_matchList_eq]
    by_cases value : scrutinee.isValue = true
    · simp only [value, ↓reduceIte]
      rcases wellTyped_list_value_typed scrutineeTyped value with nil | cons
      · subst nil; exact .next nilTyped
      · rcases cons with ⟨head, tail, rfl, headTyped, tailTyped⟩
        exact .next (wellTyped_substTwo consTyped headTyped tailTyped)
    · simp only [value, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .matchList nextTyped nilTyped consTyped)
        (fun next nextTyped => .matchList nextTyped nilTyped.weakenSamples
          consTyped.weakenSamples)
  case ite context condition thenBranch result elseBranch
      conditionTyped thenTyped elseTyped ih _ _ =>
    cases hcontext
    rw [symbolicReduce_ite_eq]
    by_cases value : condition.isValue = true
    · simp only [value, ↓reduceIte]
      obtain ⟨result, rfl⟩ := wellTyped_bool_value conditionTyped value
      cases result <;> simp only <;> exact .next (by assumption)
    · simp only [value, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .ite nextTyped thenTyped elseTyped)
        (fun next nextTyped => .ite nextTyped thenTyped.weakenSamples elseTyped.weakenSamples)
  case letE context value valueTy body result valueTyped bodyTyped ih _ =>
    cases hcontext
    rw [symbolicReduce_let_eq]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      exact .next (wellTyped_substHead bodyTyped valueTyped)
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .letE nextTyped bodyTyped)
        (fun next nextTyped => .letE nextTyped bodyTyped.weakenSamples)
  case lt left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftConstant, leftEquation⟩ :=
          constantValue?_eq_some_of_wellTypedG leftTyped leftValue
        obtain ⟨rightConstant, rightEquation⟩ :=
          constantValue?_eq_some_of_wellTypedG rightTyped rightValue
        simp only [leftEquation, rightEquation]
        exact .next .bool
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .lt leftTyped nextTyped)
          (fun next nextTyped => .lt leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .lt nextTyped rightTyped)
        (fun next nextTyped => .lt nextTyped rightTyped.weakenSamples)
  case divGG left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftAffine, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨rightAffine, rfl⟩ := wellTyped_real_value rightTyped rightValue
        rcases leftAffine with ⟨leftConstant, leftCoefficients⟩
        rcases rightAffine with ⟨rightConstant, rightCoefficients⟩
        have leftZero : leftCoefficients = 0 := wellTyped_realG_coefficients leftTyped
        have rightZero : rightCoefficients = 0 := wellTyped_realG_coefficients rightTyped
        simp only [affineValue?, Affine.div?, rightZero, ↓reduceIte, Option.some.injEq]
        exact SymbolicAction.WellTyped.next (.realG (by simp [leftZero]))
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .divGG leftTyped nextTyped)
          (fun next nextTyped => .divGG leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .divGG nextTyped rightTyped)
        (fun next nextTyped => .divGG nextTyped rightTyped.weakenSamples)
  case divEG left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftAffine, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨rightAffine, rfl⟩ := wellTyped_real_value rightTyped rightValue
        rcases rightAffine with ⟨rightConstant, rightCoefficients⟩
        have rightZero : rightCoefficients = 0 := wellTyped_realG_coefficients rightTyped
        simp only [affineValue?, Affine.div?, rightZero, ↓reduceIte, Option.some.injEq]
        exact SymbolicAction.WellTyped.next .realE
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .divEG leftTyped nextTyped)
          (fun next nextTyped => .divEG leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .divEG nextTyped rightTyped)
        (fun next nextTyped => .divEG nextTyped rightTyped.weakenSamples)
  case addG left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftAffine, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨rightAffine, rfl⟩ := wellTyped_real_value rightTyped rightValue
        rcases leftAffine with ⟨leftConstant, leftCoefficients⟩
        rcases rightAffine with ⟨rightConstant, rightCoefficients⟩
        have leftZero : leftCoefficients = 0 := wellTyped_realG_coefficients leftTyped
        have rightZero : rightCoefficients = 0 := wellTyped_realG_coefficients rightTyped
        simp only [affineValue?, Affine.add]
        exact SymbolicAction.WellTyped.next (.realG (by
          rw [leftZero, rightZero]
          funext index
          simp only [Pi.zero_apply, zero_add]))
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .addG leftTyped nextTyped)
          (fun next nextTyped => .addG leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .addG nextTyped rightTyped)
        (fun next nextTyped => .addG nextTyped rightTyped.weakenSamples)
  case addE left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftAffine, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨rightAffine, rfl⟩ := wellTyped_real_value rightTyped rightValue
        simp only [affineValue?, Affine.add]
        exact SymbolicAction.WellTyped.next .realE
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .addE leftTyped nextTyped)
          (fun next nextTyped => .addE leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .addE nextTyped rightTyped)
        (fun next nextTyped => .addE nextTyped rightTyped.weakenSamples)
  case promote context value valueTyped ih =>
    cases hcontext
    rw [symbolicReduce_promote_eq]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped isValue
      exact .next .realE
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .promote nextTyped)
        (fun next nextTyped => .promote nextTyped)
  case negE context value valueTyped ih =>
    cases hcontext
    rw [symbolicReduce.eq_def]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped isValue
      exact .next .realE
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .negE nextTyped)
        (fun next nextTyped => .negE nextTyped)
  case negG context value valueTyped ih =>
    cases hcontext
    rw [symbolicReduce.eq_def]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped isValue
      rcases coordinate with ⟨constant, coefficients⟩
      have zero : coefficients = 0 := wellTyped_realG_coefficients valueTyped
      exact .next (.realG (by rw [zero]; funext index; simp [Affine.neg]))
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .negG nextTyped)
        (fun next nextTyped => .negG nextTyped)
  case mulGE left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftAffine, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨rightAffine, rfl⟩ := wellTyped_real_value rightTyped rightValue
        rcases leftAffine with ⟨leftConstant, leftCoefficients⟩
        have leftZero : leftCoefficients = 0 := wellTyped_realG_coefficients leftTyped
        obtain ⟨result, product⟩ := Affine.mul?_eq_some_of_left
          (left := (leftConstant, leftCoefficients)) (right := rightAffine) leftZero
        simp only [affineValue?, product]
        exact SymbolicAction.WellTyped.next .realE
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .mulGE leftTyped nextTyped)
          (fun next nextTyped => .mulGE leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .mulGE nextTyped rightTyped)
        (fun next nextTyped => .mulGE nextTyped rightTyped.weakenSamples)
  case mulGG left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftAffine, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨rightAffine, rfl⟩ := wellTyped_real_value rightTyped rightValue
        rcases leftAffine with ⟨leftConstant, leftCoefficients⟩
        rcases rightAffine with ⟨rightConstant, rightCoefficients⟩
        have leftZero : leftCoefficients = 0 := wellTyped_realG_coefficients leftTyped
        have rightZero : rightCoefficients = 0 := wellTyped_realG_coefficients rightTyped
        simp only [affineValue?, Affine.mul?, rightZero, ↓reduceIte, Option.some.injEq]
        exact SymbolicAction.WellTyped.next (.realG (by simp [leftZero]))
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .mulGG leftTyped nextTyped)
          (fun next nextTyped => .mulGG leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .mulGG nextTyped rightTyped)
        (fun next nextTyped => .mulGG nextTyped rightTyped.weakenSamples)
  case pair leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    rename_i left leftTy right rightTy
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        exact .next (.pair leftTyped rightTyped)
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .pair leftTyped nextTyped)
          (fun next nextTyped => .pair leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .pair nextTyped rightTyped)
        (fun next nextTyped => .pair nextTyped rightTyped.weakenSamples)
  case cons headTyped tailTyped ihHead ihTail =>
    cases hcontext
    rename_i head element tail
    simp only [symbolicReduce]
    by_cases headValue : head.isValue = true
    · simp only [headValue, ↓reduceIte]
      by_cases tailValue : tail.isValue = true
      · simp only [tailValue, ↓reduceIte]
        exact .next (.cons headTyped tailTyped)
      · simp only [tailValue, ↓reduceIte]
        exact (ihTail rfl).wrap
          (fun next nextTyped => .cons headTyped nextTyped)
          (fun next nextTyped => .cons headTyped.weakenSamples nextTyped)
    · simp only [headValue, ↓reduceIte]
      exact (ihHead rfl).wrap
        (fun next nextTyped => .cons nextTyped tailTyped)
        (fun next nextTyped => .cons nextTyped tailTyped.weakenSamples)
  case inl valueTyped ih =>
    cases hcontext
    rename_i value leftTy rightTy
    simp only [symbolicReduce]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      exact .next (.inl valueTyped)
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap (fun next nextTyped => .inl nextTyped)
        (fun next nextTyped => .inl nextTyped)
  case inr valueTyped ih =>
    cases hcontext
    rename_i value rightTy leftTy
    simp only [symbolicReduce]
    by_cases isValue : value.isValue = true
    · simp only [isValue, ↓reduceIte]
      exact .next (.inr valueTyped)
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap (fun next nextTyped => .inr nextTyped)
        (fun next nextTyped => .inr nextTyped)
  case bvar hvar => cases hcontext; cases hvar
  case unit => simp only [symbolicReduce]; exact .next .unit
  case bool => simp only [symbolicReduce]; exact .next .bool
  case realE => simp only [symbolicReduce]; exact .next .realE
  case realG value zero => simp only [symbolicReduce]; exact .next (.realG zero)
  case lam bodyTyped ih =>
    cases hcontext
    simp only [symbolicReduce]
    exact .next (.lam bodyTyped)
  case fix bodyTyped ih =>
    cases hcontext
    simp only [symbolicReduce]
    exact .next (.fix bodyTyped)
  case nil => simp only [symbolicReduce]; exact .next .nil

theorem wellTyped_ofExpr_of_typed {expression : Expr}
    (typed : Determinize.Statement.Paper.Typed context expression ty)
    (sourceTags : (AffineExpr.ofExpr expression).SourceTags) :
    WellTyped context (AffineExpr.ofExpr expression) ty := by
  induction typed <;> simp only [ofExpr, SourceTags] at sourceTags ⊢
  case uniform lowerTyped upperTyped ihl ihr =>
    obtain ⟨rfl, lowerTags, upperTags⟩ := sourceTags
    exact .uniform (ihl lowerTags) (ihr upperTags)
  case gaussian meanTyped varianceTyped ihl ihr =>
    obtain ⟨rfl, meanTags, varianceTags⟩ := sourceTags
    exact .gaussian (ihl meanTags) (ihr varianceTags)
  case poisson rateTyped ih =>
    obtain ⟨rfl, rateTags⟩ := sourceTags
    exact .poisson (ih rateTags)
  case bernoulli probabilityTyped ih =>
    obtain ⟨rfl, probabilityTags⟩ := sourceTags
    exact .bernoulli (ih probabilityTags)
  case discrete =>
    obtain rfl := sourceTags
    exact .discrete
  case exponential rateTyped ih =>
    obtain ⟨rfl, rateTags⟩ := sourceTags
    exact .exponential (ih rateTags)
  case beta alphaTyped betaTyped ihl ihr =>
    obtain ⟨rfl, alphaTags, betaTags⟩ := sourceTags
    exact .beta (ihl alphaTags) (ihr betaTags)
  case gamma shapeTyped rateTyped ihl ihr =>
    obtain ⟨rfl, shapeTags, rateTags⟩ := sourceTags
    exact .gamma (ihl shapeTags) (ihr rateTags)
  all_goals try aesop (add safe constructors WellTyped)
  all_goals try (cases ‹Mode› <;> aesop (add safe constructors WellTyped))

theorem coordinate_count (expression : AffineExpr sampleCount) :
    expression.coordinates.length = expression.skeleton.realArity := by
  have lengthRule := realCoordinates_length (expression.realize (fun _ => 0))
  rw [realize_coordinates, List.length_map, realize_skeleton] at lengthRule
  exact lengthRule

theorem affine_eval_measurable (expression : Affine sampleCount) :
    Measurable expression.eval := by
  apply Measurable.add measurable_const
  apply Finset.measurable_sum
  intro index _
  exact Measurable.mul measurable_const (measurable_pi_apply index)

def realizeFamily (expression : AffineExpr sampleCount) :
    MeasurableFamily (Env sampleCount) expression.realize where
  skeleton := expression.skeleton
  skeleton_eq := expression.realize_skeleton
  coordinate_count environment := by
    rw [realize_coordinates, List.length_map, coordinate_count]
  coordinate_measurable index := by
    rw [show (fun environment =>
        (expression.realize environment).realCoordinates.getD index 0) =
        fun environment =>
          (expression.coordinates.map (Symbolic.Affine.eval · environment)).getD index 0 by
      funext environment
      rw [realize_coordinates]]
    by_cases inBounds : index < expression.coordinates.length
    · convert affine_eval_measurable expression.coordinates[index] using 1
      funext environment
      simp [List.getD_eq_getElem?_getD, inBounds]
    · convert (measurable_const : Measurable fun _ : Env sampleCount => (0 : ℝ)) using 1
      funext environment
      simp [List.getD, inBounds]

theorem realize_measurable (expression : AffineExpr sampleCount) :
    Measurable expression.realize :=
  expression.realizeFamily.measurable

theorem actualMeasure_snoc
    (history : Symbolic.SampleEnv laws n) (op : Determinize.Statement.Paper.Op)
    (affineArgs : Fin (Determinize.Statement.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Statement.Paper.generalArity op) → ℝ) :
    Symbolic.SampleEnv.actualMeasure laws (.snoc history op affineArgs generalArgs) =
      (Symbolic.SampleEnv.actualMeasure laws history).bind fun environment =>
        (laws.kernel op
          (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)).map
            (fun value => Env.cons value environment) := by
  rfl

end AffineExpr

end Symbolic

end Determinize.Proof.Paper
