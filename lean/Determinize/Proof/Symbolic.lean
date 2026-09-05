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
  | real (mode : Mode) (value : Affine sampleCount)
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
  | neg (mode : Mode) (body : AffineExpr sampleCount)
  | add (mode : Mode) (left right : AffineExpr sampleCount)
  | mul (mode : Mode) (left right : AffineExpr sampleCount)
  | div (mode : Mode) (left right : AffineExpr sampleCount)
  | lt (left right : AffineExpr sampleCount)
  | sample (mode : Mode) (op : Tag)
      (affineArgs generalArgs : List (AffineExpr sampleCount))

namespace AffineExpr

def realize (environment : Env sampleCount) : AffineExpr sampleCount → Expr
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real mode value => .real mode (value.eval environment)
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
  | .neg mode body => .neg mode (body.realize environment)
  | .add mode left right =>
      .add mode (left.realize environment) (right.realize environment)
  | .mul mode left right =>
      .mul mode (left.realize environment) (right.realize environment)
  | .div mode left right =>
      .div mode (left.realize environment) (right.realize environment)
  | .lt left right => .lt (left.realize environment) (right.realize environment)
  | .sample mode op affine general => .sample mode op
      (affine.map (realize environment)) (general.map (realize environment))

def skeleton : AffineExpr sampleCount → Skeleton
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real mode _ => .real mode
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
  | .neg mode body => .neg mode body.skeleton
  | .add mode left right => .add mode left.skeleton right.skeleton
  | .mul mode left right => .mul mode left.skeleton right.skeleton
  | .div mode left right => .div mode left.skeleton right.skeleton
  | .lt left right => .lt left.skeleton right.skeleton
  | .sample mode op affine general =>
      .sample mode op (affine.map skeleton) (general.map skeleton)

def coordinates : AffineExpr sampleCount → List (Affine sampleCount)
  | .real _ value => [value]
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .promote body | .neg _ body => body.coordinates
  | .app left right | .pair left right | .cons left right
  | .add _ left right | .mul _ left right | .div _ left right | .lt left right =>
      left.coordinates ++ right.coordinates
  | .matchSum scrutinee left right | .ite scrutinee left right =>
      scrutinee.coordinates ++ left.coordinates ++ right.coordinates
  | .matchList scrutinee nilCase consCase =>
      scrutinee.coordinates ++ nilCase.coordinates ++ consCase.coordinates
  | .letE value body => value.coordinates ++ body.coordinates
  | .sample _ _ affine general =>
      affine.flatMap coordinates ++ general.flatMap coordinates
  | _ => []

def ofExpr : Expr → AffineExpr 0
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real mode value => .real mode (value, Fin.elim0)
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
  | .neg mode body => .neg mode (ofExpr body)
  | .add mode left right => .add mode (ofExpr left) (ofExpr right)
  | .mul mode left right => .mul mode (ofExpr left) (ofExpr right)
  | .div mode left right => .div mode (ofExpr left) (ofExpr right)
  | .lt left right => .lt (ofExpr left) (ofExpr right)
  | .sample mode op affine general =>
      .sample mode op (affine.map ofExpr) (general.map ofExpr)

def mapAffine (transform : Affine n → Affine m) : AffineExpr n → AffineExpr m
  | .bvar index => .bvar index
  | .unit => .unit
  | .bool value => .bool value
  | .real mode value => .real mode (transform value)
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
  | .neg mode body => .neg mode (body.mapAffine transform)
  | .add mode left right =>
      .add mode (left.mapAffine transform) (right.mapAffine transform)
  | .mul mode left right =>
      .mul mode (left.mapAffine transform) (right.mapAffine transform)
  | .div mode left right =>
      .div mode (left.mapAffine transform) (right.mapAffine transform)
  | .lt left right => .lt (left.mapAffine transform) (right.mapAffine transform)
  | .sample mode op affine general => .sample mode op
      (affine.map (mapAffine transform)) (general.map (mapAffine transform))

def Affine.weaken (expression : Affine n) : Affine (n + 1) :=
  (expression.1, Fin.cases 0 expression.2)

def Affine.fresh (n : Nat) : Affine (n + 1) :=
  (0, Fin.cases 1 (fun _ => 0))

def weakenSamples (expression : AffineExpr n) : AffineExpr (n + 1) :=
  expression.mapAffine Affine.weaken

/-- Every G coordinate has zero E coefficient. -/
def GZero (expression : AffineExpr sampleCount) : Prop :=
  ∀ (index : Nat) (coordinate : Affine sampleCount),
    expression.skeleton.coordinateModes[index]? = some Mode.G →
    expression.coordinates[index]? = some coordinate → coordinate.2 = 0

/-- Pending source E and G sites retain stochastic tags. -/
def SourceTags : AffineExpr sampleCount → Prop
  | .sample mode op affine general =>
      (match mode, op with
       | .E, .stochastic _ => True
       | .G, .stochastic _ => True
       | _, _ => False) ∧
      (∀ child ∈ affine, child.SourceTags) ∧
      ∀ child ∈ general, child.SourceTags
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .promote body | .neg _ body => body.SourceTags
  | .app left right | .pair left right | .cons left right
  | .add _ left right | .mul _ left right | .div _ left right | .lt left right =>
      left.SourceTags ∧ right.SourceTags
  | .matchSum scrutinee left right | .ite scrutinee left right =>
      scrutinee.SourceTags ∧ left.SourceTags ∧ right.SourceTags
  | .matchList scrutinee nilCase consCase =>
      scrutinee.SourceTags ∧ nilCase.SourceTags ∧ consCase.SourceTags
  | .letE value body => value.SourceTags ∧ body.SourceTags
  | _ => True

def GConstant : AffineExpr sampleCount → Prop
  | .real .G (_, coefficients) => coefficients = 0
  | .real .E _ => True
  | .sample _ _ affine general =>
      (∀ child ∈ affine, child.GConstant) ∧ ∀ child ∈ general, child.GConstant
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .promote body | .neg _ body => body.GConstant
  | .app left right | .pair left right | .cons left right
  | .add _ left right | .mul _ left right | .div _ left right | .lt left right =>
      left.GConstant ∧ right.GConstant
  | .matchSum scrutinee left right | .ite scrutinee left right =>
      scrutinee.GConstant ∧ left.GConstant ∧ right.GConstant
  | .matchList scrutinee nilCase consCase =>
      scrutinee.GConstant ∧ nilCase.GConstant ∧ consCase.GConstant
  | .letE value body => value.GConstant ∧ body.GConstant
  | _ => True

def Typed (context : List Ty) (expression : AffineExpr sampleCount) (ty : Ty) : Prop :=
  ∀ environment, Determinize.Statement.Paper.Typed context (expression.realize environment) ty

inductive WellTyped : List Ty → AffineExpr sampleCount → Ty → Prop
  | bvar : Determinize.Statement.Paper.HasVar context index ty → WellTyped context (.bvar index) ty
  | unit : WellTyped context .unit .unit
  | bool : WellTyped context (.bool value) .bool
  | realE : WellTyped context (.real .E value) (.float .E)
  | realG : value.2 = 0 → WellTyped context (.real .G value) (.float .G)
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
      WellTyped context (.neg .E value) (.float .E)
  | negG : WellTyped context value (.float .G) →
      WellTyped context (.neg .G value) (.float .G)
  | addE : WellTyped context left (.float .E) → WellTyped context right (.float .E) →
      WellTyped context (.add .E left right) (.float .E)
  | addG : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.add .G left right) (.float .G)
  | mulEG : WellTyped context left (.float .E) → WellTyped context right (.float .G) →
      WellTyped context (.mul .E left right) (.float .E)
  | mulGG : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.mul .G left right) (.float .G)
  | divEG : WellTyped context left (.float .E) → WellTyped context right (.float .G) →
      WellTyped context (.div .E left right) (.float .E)
  | divGG : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.div .G left right) (.float .G)
  | lt : WellTyped context left (.float .G) → WellTyped context right (.float .G) →
      WellTyped context (.lt left right) .bool
  | sampleE (op : Determinize.Statement.Paper.Op) :
      affine.length = Determinize.Statement.Paper.affineArity op →
      general.length = Determinize.Statement.Paper.generalArity op →
      (∀ expression ∈ affine, WellTyped context expression (.float .E)) →
      (∀ expression ∈ general, WellTyped context expression (.float .G)) →
      WellTyped context (.sample .E (.stochastic op) affine general) (.float .E)
  | sampleG (op : Determinize.Statement.Paper.Op) :
      affine.length = Determinize.Statement.Paper.affineArity op →
      general.length = Determinize.Statement.Paper.generalArity op →
      (∀ expression ∈ affine, WellTyped context expression (.float .G)) →
      (∀ expression ∈ general, WellTyped context expression (.float .G)) →
      WellTyped context (.sample .G (.stochastic op) affine general) (.float .G)

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
  case mulEG left right => exact Determinize.Statement.Paper.Typed.mul left right
  case mulGG left right => exact Determinize.Statement.Paper.Typed.mul left right
  case divEG left right => exact Determinize.Statement.Paper.Typed.div left right
  case divGG left right => exact Determinize.Statement.Paper.Typed.div left right
  case lt left right => exact Determinize.Statement.Paper.Typed.lt left right
  case sampleE op ha hg hAffine hGeneral ihAffine ihGeneral =>
    apply Determinize.Statement.Paper.Typed.sample (.stochastic op)
    · simpa only [List.length_map, Tag.base] using ha
    · simpa only [List.length_map, Tag.base] using hg
    · intro expression member
      rw [List.mem_map] at member
      rcases member with ⟨child, childMember, rfl⟩
      exact ihAffine child childMember
    · intro expression member
      rw [List.mem_map] at member
      rcases member with ⟨child, childMember, rfl⟩
      exact ihGeneral child childMember
  case sampleG op ha hg hAffine hGeneral ihAffine ihGeneral =>
    apply Determinize.Statement.Paper.Typed.sample (.stochastic op)
    · simpa only [List.length_map, Tag.base] using ha
    · simpa only [List.length_map, Tag.base] using hg
    · intro expression member
      rw [List.mem_map] at member
      rcases member with ⟨child, childMember, rfl⟩
      exact ihAffine child childMember
    · intro expression member
      rw [List.mem_map] at member
      rcases member with ⟨child, childMember, rfl⟩
      exact ihGeneral child childMember

theorem WellTyped.sourceTags (typed : WellTyped context expression ty) :
    expression.SourceTags := by
  induction typed <;> simp_all [SourceTags]

theorem WellTyped.gconstant (typed : WellTyped context expression ty) :
    expression.GConstant := by
  induction typed
  case realG =>
    rename_i value _ zero
    rcases value with ⟨constant, coefficients⟩
    unfold GConstant
    exact zero
  all_goals try simp only [GConstant] at * <;> aesop

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
  case sampleE op ha hg hAffine hGeneral ihAffine ihGeneral =>
    exact .sampleE op (by simpa using ha) (by simpa using hg)
      (fun child member => by
        rw [List.mem_map] at member
        rcases member with ⟨original, originalMember, rfl⟩
        exact ihAffine original originalMember)
      (fun child member => by
        rw [List.mem_map] at member
        rcases member with ⟨original, originalMember, rfl⟩
        exact ihGeneral original originalMember)
  case sampleG op ha hg hAffine hGeneral ihAffine ihGeneral =>
    exact .sampleG op (by simpa using ha) (by simpa using hg)
      (fun child member => by
        rw [List.mem_map] at member
        rcases member with ⟨original, originalMember, rfl⟩
        exact ihAffine original originalMember)
      (fun child member => by
        rw [List.mem_map] at member
        rcases member with ⟨original, originalMember, rfl⟩
        exact ihGeneral original originalMember)
  all_goals aesop (add safe constructors WellTyped) (add safe cases Tag)

theorem WellTyped.weakenSamples (typed : WellTyped context expression ty) :
    WellTyped context expression.weakenSamples ty := by
  apply WellTyped.mapAffine typed Affine.weaken
  intro affine zero
  change (Fin.cases 0 affine.2 : Fin (_ + 1) → ℝ) = 0
  rw [zero]
  funext index
  refine Fin.cases ?_ (fun tail => ?_) index <;> rfl

theorem WellTyped.typed (typed : WellTyped context expression ty) :
    Typed context expression ty :=
  typed.realize_typed

def shift (amount cutoff : Nat) : AffineExpr sampleCount → AffineExpr sampleCount
  | .bvar index => .bvar (if cutoff ≤ index then index + amount else index)
  | .unit => .unit
  | .bool value => .bool value
  | .real mode value => .real mode value
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
  | .neg m x => .neg m (x.shift amount cutoff)
  | .add m l r => .add m (l.shift amount cutoff) (r.shift amount cutoff)
  | .mul m l r => .mul m (l.shift amount cutoff) (r.shift amount cutoff)
  | .div m l r => .div m (l.shift amount cutoff) (r.shift amount cutoff)
  | .lt l r => .lt (l.shift amount cutoff) (r.shift amount cutoff)
  | .sample m op affine general => .sample m op
      (affine.map (shift amount cutoff)) (general.map (shift amount cutoff))

def substAt (depth : Nat) (replacement : AffineExpr sampleCount)
    (expression : AffineExpr sampleCount) : AffineExpr sampleCount := match expression with
  | .bvar index => if index = depth then replacement.shift depth 0
      else .bvar (if depth < index then index - 1 else index)
  | .unit => .unit
  | .bool value => .bool value
  | .real mode value => .real mode value
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
  | .neg m x => .neg m (substAt depth replacement x)
  | .add m l r => .add m (substAt depth replacement l) (substAt depth replacement r)
  | .mul m l r => .mul m (substAt depth replacement l) (substAt depth replacement r)
  | .div m l r => .div m (substAt depth replacement l) (substAt depth replacement r)
  | .lt l r => .lt (substAt depth replacement l) (substAt depth replacement r)
  | .sample m op affine general => .sample m op
      (affine.map (substAt depth replacement)) (general.map (substAt depth replacement))
termination_by expression

def substHead (body replacement : AffineExpr sampleCount) : AffineExpr sampleCount :=
  substAt 0 replacement body

def substTwo (body argument function : AffineExpr sampleCount) : AffineExpr sampleCount :=
  substAt 0 argument (substAt 1 function body)

set_option maxHeartbeats 800000 in
theorem gconstant_shift (expression : AffineExpr n) (amount cutoff : Nat)
    (constant : expression.GConstant) :
    (expression.shift amount cutoff).GConstant := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression cutoff with
  | h size ih =>
    have recurse (child : AffineExpr n) (childCutoff : Nat)
        (smaller : sizeOf child < sizeOf expression) (childConstant : child.GConstant) :
        (child.shift amount childCutoff).GConstant :=
      ih (sizeOf child) (by rwa [← sizeEq]) child childCutoff childConstant rfl
    cases expression with
    | sample mode op affine general =>
        simp only [shift, GConstant, List.forall_mem_map] at constant ⊢
        constructor
        · intro child member
          exact recurse child cutoff
            (Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega))
            (constant.1 child member)
        · intro child member
          exact recurse child cutoff
            (Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega))
            (constant.2 child member)
    | _ =>
        simp (disch := simp_wf) only [shift, GConstant, recurse] at constant ⊢
        all_goals aesop (add safe apply recurse) <;> simp_wf <;> omega

set_option maxHeartbeats 800000 in
theorem gconstant_mapAffine (expression : AffineExpr n) (transform : Affine n → Affine m)
    (preservesZero : ∀ affine, affine.2 = 0 → (transform affine).2 = 0)
    (constant : expression.GConstant) :
    (expression.mapAffine transform).GConstant := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
    have recurse (child : AffineExpr n) (smaller : sizeOf child < sizeOf expression)
        (childConstant : child.GConstant) :
        (child.mapAffine transform).GConstant :=
      ih (sizeOf child) (by rwa [← sizeEq]) child childConstant rfl
    cases expression with
    | real mode affine =>
        cases mode with
        | E => simp [mapAffine, GConstant]
        | G =>
            rcases affine with ⟨constantTerm, coefficients⟩
            simp only [GConstant] at constant
            simp only [mapAffine]
            have result := preservesZero (constantTerm, coefficients) constant
            generalize transform (constantTerm, coefficients) = transformed at result ⊢
            rcases transformed with ⟨newConstant, newCoefficients⟩
            simpa only [GConstant] using result
    | sample mode op affine general =>
        simp only [mapAffine, GConstant, List.forall_mem_map] at constant ⊢
        constructor
        · intro child member
          exact recurse child
            (Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega))
            (constant.1 child member)
        · intro child member
          exact recurse child
            (Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega))
            (constant.2 child member)
    | _ =>
        simp (disch := simp_wf) only [mapAffine, GConstant, recurse] at constant ⊢
        all_goals aesop (add safe apply recurse) <;> simp_wf <;> omega

theorem gconstant_weakenSamples (expression : AffineExpr n) (constant : expression.GConstant) :
    expression.weakenSamples.GConstant := by
  apply gconstant_mapAffine expression Affine.weaken
  · intro affine zero
    change (Fin.cases 0 affine.2 : Fin (n + 1) → ℝ) = 0
    rw [zero]
    funext index
    refine Fin.cases ?_ (fun tail => ?_) index <;> rfl
  · exact constant

set_option maxHeartbeats 800000 in
theorem gconstant_substAt (expression replacement : AffineExpr n) (depth : Nat)
    (expressionConstant : expression.GConstant)
    (replacementConstant : replacement.GConstant) :
    (substAt depth replacement expression).GConstant := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression depth with
  | h size ih =>
    have recurse (child : AffineExpr n) (childDepth : Nat)
        (smaller : sizeOf child < sizeOf expression) (childConstant : child.GConstant) :
        (substAt childDepth replacement child).GConstant :=
      ih (sizeOf child) (by rwa [← sizeEq]) child childDepth childConstant rfl
    cases expression with
    | bvar index =>
        simp only [substAt]
        split
        · exact gconstant_shift replacement depth 0 replacementConstant
        · simp [GConstant]
    | sample mode op affine general =>
        simp only [substAt, GConstant, List.forall_mem_map] at expressionConstant ⊢
        constructor
        · intro child member
          exact recurse child depth
            (Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega))
            (expressionConstant.1 child member)
        · intro child member
          exact recurse child depth
            (Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega))
            (expressionConstant.2 child member)
    | _ =>
        simp (disch := simp_wf) only [substAt, GConstant, recurse] at expressionConstant ⊢
        all_goals aesop (add safe apply recurse) <;> simp_wf <;> omega

theorem gconstant_substHead (body replacement : AffineExpr n)
    (bodyConstant : body.GConstant) (replacementConstant : replacement.GConstant) :
    (body.substHead replacement).GConstant :=
  gconstant_substAt body replacement 0 bodyConstant replacementConstant

theorem gconstant_substTwo (body argument function : AffineExpr n)
    (bodyConstant : body.GConstant) (argumentConstant : argument.GConstant)
    (functionConstant : function.GConstant) :
    (body.substTwo argument function).GConstant := by
  exact gconstant_substHead _ _
    (gconstant_substAt body function 1 bodyConstant functionConstant) argumentConstant

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
  | mulEG hl hr ihl ihr =>
      rw [shift]
      exact .mulEG (ihl (before := before) (suffix := suffix) hcontext)
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
  | sampleE op ha hg hta htg iha ihg =>
      rw [shift]
      apply WellTyped.sampleE op
      · simpa using ha
      · simpa using hg
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact iha original member (before := before) (suffix := suffix) hcontext
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact ihg original member (before := before) (suffix := suffix) hcontext
  | sampleG op ha hg hta htg iha ihg =>
      rw [shift]
      apply WellTyped.sampleG op
      · simpa using ha
      · simpa using hg
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact iha original member (before := before) (suffix := suffix) hcontext
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact ihg original member (before := before) (suffix := suffix) hcontext

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
  | mulEG hl hr ihl ihr =>
      rw [substAt]
      exact .mulEG (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
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
  | sampleE op ha hg hta htg iha ihg =>
      rw [substAt]
      apply WellTyped.sampleE op
      · simpa using ha
      · simpa using hg
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact iha original member replacementTyped (before := before) (suffix := suffix) hcontext
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact ihg original member replacementTyped (before := before) (suffix := suffix) hcontext
  | sampleG op ha hg hta htg iha ihg =>
      rw [substAt]
      apply WellTyped.sampleG op
      · simpa using ha
      · simpa using hg
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact iha original member replacementTyped (before := before) (suffix := suffix) hcontext
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact ihg original member replacementTyped (before := before) (suffix := suffix) hcontext

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
    | sample mode op affine general =>
        simp only [realize, skeleton, Expr.skeleton]
        congr 1
        · rw [List.map_map]
          apply List.map_congr_left
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
        · rw [List.map_map]
          apply List.map_congr_left
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
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
    | sample mode op affine general =>
        simp only [realize, coordinates, Expr.realCoordinates, List.flatMap_map,
          List.map_flatMap, List.map_append]
        congr 1
        · apply List.flatMap_congr
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
        · apply List.flatMap_congr
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
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
    | sample mode op affine general =>
        simp only [shift, realize, Expr.shift, Expr.mapVars, List.map_map]
        congr 1
        · apply List.map_congr_left
          intro child member
          apply recurse child cutoff
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
        · apply List.map_congr_left
          intro child member
          apply recurse child cutoff
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
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
    | sample mode op affine general =>
        simp only [substAt, realize, Expr.substAt, Expr.mapVars, List.map_map]
        congr 1
        · apply List.map_congr_left
          intro child member
          apply recurse child depth
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
        · apply List.map_congr_left
          intro child member
          apply recurse child depth
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
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
    | sample mode op affine general =>
        simp only [mapAffine, realize, List.map_map]
        congr 1
        · apply List.map_congr_left
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
        · apply List.map_congr_left
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
    | real mode value => simp only [mapAffine, realize, eval_transform]
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
    | sample mode op affine general =>
        simp only [ofExpr, realize, List.map_map]
        congr 1
        · conv_rhs => rw [← List.map_id affine]
          apply List.map_congr_left
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
        · conv_rhs => rw [← List.map_id general]
          apply List.map_congr_left
          intro child member
          apply recurse child
          exact Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)
    | real mode value =>
        simp [ofExpr, realize, Symbolic.Affine.eval]
    | _ =>
        simp (disch := simp_wf) only [ofExpr, realize, recurse]
        all_goals repeat' first | rfl | rw [recurse _ (by simp_wf <;> omega)]

theorem gzero_zero (expression : AffineExpr 0) : expression.GZero := by
  intro index coordinate _ _
  funext coefficient
  exact Fin.elim0 coefficient

def isValue : AffineExpr n → Bool
  | .unit | .bool _ | .real _ _ | .lam _ | .fix _ | .nil => true
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
  | .real _ value => some value
  | _ => none

noncomputable def constantValue? : AffineExpr n → Option ℝ
  | .real _ (constant, coefficients) => if coefficients = 0 then some constant else none
  | _ => none

theorem typed_real_value {sampleCount : Nat} {expression : AffineExpr sampleCount}
    (typed : Typed [] expression (.float mode))
    (value : expression.isValue = true) :
    ∃ affine, expression = .real mode affine := by
  let environment : Env sampleCount := fun _ => 0
  have concreteValue : (expression.realize environment).isValue = true := by
    rw [realize_isValue, value]
  obtain ⟨coordinate, equality⟩ :=
    Typing.typed_real_value (typed environment) concreteValue
  cases expression <;> simp [realize] at equality
  rename_i actualMode affine
  cases equality.1
  exact ⟨affine, rfl⟩

theorem affineValue?_eq_some_of_typed (typed : Typed [] expression (.float mode))
    (value : expression.isValue = true) :
    ∃ affine, affineValue? expression = some affine := by
  obtain ⟨affine, rfl⟩ := typed_real_value typed value
  exact ⟨affine, rfl⟩

theorem constantValue?_eq_some_of_typedG (typed : Typed [] expression (.float .G))
    (value : expression.isValue = true) (constant : expression.GConstant) :
    ∃ result, constantValue? expression = some result := by
  obtain ⟨affine, rfl⟩ := typed_real_value typed value
  rcases affine with ⟨constantTerm, coefficients⟩
  simp only [GConstant] at constant
  exact ⟨constantTerm, by simp [constantValue?, constant]⟩

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
    ∃ coordinate, expression = .real mode coordinate := by
  cases typed <;> simp_all [isValue]

theorem affineValue?_eq_some_of_wellTyped
    (typed : WellTyped context expression (.float mode))
    (value : expression.isValue = true) :
    ∃ affine, affineValue? expression = some affine := by
  obtain ⟨affine, rfl⟩ := wellTyped_real_value typed value
  exact ⟨affine, rfl⟩

theorem constantValue?_eq_some_of_wellTypedG
    (typed : WellTyped context expression (.float .G))
    (value : expression.isValue = true) (constant : expression.GConstant) :
    ∃ result, constantValue? expression = some result := by
  obtain ⟨affine, rfl⟩ := wellTyped_real_value typed value
  rcases affine with ⟨constantTerm, coefficients⟩
  simp only [GConstant] at constant
  exact ⟨constantTerm, by simp [constantValue?, constant]⟩

def allAffineValues? (expressions : List (AffineExpr n)) : Option (List (Affine n)) :=
  expressions.mapM affineValue?

noncomputable def allConstantValues? (expressions : List (AffineExpr n)) : Option (List ℝ) :=
  expressions.mapM constantValue?

def firstNonValue : List (AffineExpr n) → Option (List (AffineExpr n) × AffineExpr n × List (AffineExpr n))
  | [] => none
  | expression :: expressions =>
      if expression.isValue then
        match firstNonValue expressions with
        | none => none
        | some (front, current, suffix) => some (expression :: front, current, suffix)
      else some ([], expression, expressions)

theorem firstNonValue_current_mem {expressions : List (AffineExpr n)} {front current suffix}
    (found : firstNonValue expressions = some (front, current, suffix)) :
    current ∈ expressions := by
  induction expressions generalizing front current suffix with
  | nil => simp [firstNonValue] at found
  | cons head tail ih =>
      simp only [firstNonValue] at found
      split at found
      · split at found
        · contradiction
        · rename_i tailFront tailCurrent tailSuffix equation
          simp only [Option.some.injEq, Prod.mk.injEq] at found
          exact List.mem_cons_of_mem head (found.2.1 ▸ ih equation)
      · simp only [Option.some.injEq, Prod.mk.injEq] at found
        exact found.2.1 ▸ List.mem_cons_self

theorem firstNonValue_realize (expressions : List (AffineExpr n))
    (environment : Env n) :
    Determinize.Statement.Paper.firstNonValue
        (expressions.map (realize environment)) =
      (firstNonValue expressions).map fun result =>
        (result.1.map (realize environment), result.2.1.realize environment,
          result.2.2.map (realize environment)) := by
  induction expressions with
  | nil => rfl
  | cons head tail ih =>
      simp only [List.map_cons, Determinize.Statement.Paper.firstNonValue,
        firstNonValue, realize_isValue]
      split
      · rw [ih]
        cases htail : firstNonValue tail <;> simp [htail]
      · rfl

theorem allRealValues_realize_of_allAffineValues
    {expressions : List (AffineExpr n)} {values : List (Affine n)}
    (found : allAffineValues? expressions = some values)
    (environment : Env n) :
    allRealValues? (expressions.map (realize environment)) =
      some (values.map (Symbolic.Affine.eval · environment)) := by
  induction expressions generalizing values with
  | nil => simp [allAffineValues?] at found ⊢; subst values; rfl
  | cons head tail ih =>
      cases hhead : affineValue? head with
      | none => simp [allAffineValues?, hhead] at found
      | some headValue =>
          cases htail : allAffineValues? tail with
          | none =>
              change List.mapM affineValue? tail = none at htail
              simp [allAffineValues?, hhead, htail] at found
          | some tailValues =>
              change List.mapM affineValue? tail = some tailValues at htail
              have valuesEq : values = headValue :: tailValues := by
                simpa [allAffineValues?, hhead, htail] using found.symm
              subst values
              cases head <;> simp [affineValue?] at hhead
              rename_i mode actual
              cases hhead
              simp only [List.map_cons, realize, allRealValues?, Option.some.injEq,
                List.cons.injEq, true_and]
              rw [ih htail]
              rfl

theorem allRealValues_realize_of_allConstantValues
    {expressions : List (AffineExpr n)} {values : List ℝ}
    (found : allConstantValues? expressions = some values)
    (environment : Env n) :
    allRealValues? (expressions.map (realize environment)) = some values := by
  induction expressions generalizing values with
  | nil => simp [allConstantValues?] at found ⊢; subst values; rfl
  | cons head tail ih =>
      cases hhead : constantValue? head with
      | none => simp [allConstantValues?, hhead] at found
      | some headValue =>
          cases htail : allConstantValues? tail with
          | none =>
              change List.mapM constantValue? tail = none at htail
              simp [allConstantValues?, hhead, htail] at found
          | some tailValues =>
              change List.mapM constantValue? tail = some tailValues at htail
              have valuesEq : values = headValue :: tailValues := by
                simpa [allConstantValues?, hhead, htail] using found.symm
              subst values
              cases head <;> simp [constantValue?] at hhead
              rename_i mode actual
              rcases actual with ⟨constant, coefficients⟩
              rcases hhead with ⟨zero, rfl⟩
              change coefficients = 0 at zero
              rw [zero]
              simp only [List.map_cons, realize, Symbolic.Affine.eval, Pi.zero_apply,
                zero_mul, Finset.sum_const_zero, add_zero, allRealValues?,
                Option.some.injEq, List.cons.injEq, true_and]
              rw [ih htail]
              rfl

theorem firstNonValue_eq_none_iff (expressions : List (AffineExpr n)) :
    firstNonValue expressions = none ↔
      ∀ expression ∈ expressions, expression.isValue = true := by
  induction expressions with
  | nil => simp [firstNonValue]
  | cons head tail ih =>
      simp only [firstNonValue]
      by_cases headValue : head.isValue = true
      · simp only [headValue, ↓reduceIte, List.forall_mem_cons, true_and]
        rw [← ih]
        cases firstNonValue tail <;> simp
      · simp [headValue]

theorem firstNonValue_eq_some_append
    (found : firstNonValue expressions = some (front, current, suffix)) :
    expressions = front ++ current :: suffix := by
  induction expressions generalizing front current suffix with
  | nil => simp [firstNonValue] at found
  | cons head tail ih =>
      simp only [firstNonValue] at found
      by_cases headValue : head.isValue = true
      · simp only [headValue, ↓reduceIte] at found
        split at found
        · contradiction
        · rename_i tailFront tailCurrent tailSuffix equation
          simp only [Option.some.injEq, Prod.mk.injEq] at found
          rcases found with ⟨rfl, rfl, rfl⟩
          simp [ih equation]
      · simp [headValue] at found
        rcases found with ⟨rfl, rfl, rfl⟩
        rfl

theorem wellTyped_list_replace
    (typed : ∀ expression ∈ front ++ current :: suffix,
      WellTyped [] expression ty)
    (nextTyped : WellTyped [] next ty) :
    ∀ expression ∈ front ++ next :: suffix, WellTyped [] expression ty := by
  intro expression member
  simp only [List.mem_append, List.mem_cons] at member ⊢
  rcases member with member | rfl | member
  · exact typed expression (by simp [member])
  · exact nextTyped
  · exact typed expression (by simp [member])

theorem wellTyped_list_weakenSamples
    {n : Nat} {expressions : List (AffineExpr n)}
    (typed : ∀ expression ∈ expressions, WellTyped [] expression ty) :
    ∀ expression ∈ expressions.map weakenSamples,
      WellTyped [] expression ty := by
  intro expression member
  rw [List.mem_map] at member
  rcases member with ⟨original, originalMember, rfl⟩
  exact (typed original originalMember).weakenSamples

theorem wellTyped_list_replace_weakenSamples
    (typed : ∀ expression ∈ front ++ current :: suffix,
      WellTyped [] expression ty)
    (nextTyped : WellTyped [] next ty) :
    ∀ expression ∈ front.map weakenSamples ++ next :: suffix.map weakenSamples,
      WellTyped [] expression ty := by
  intro expression member
  simp only [List.mem_append, List.mem_cons] at member
  rcases member with member | rfl | member
  · rw [List.mem_map] at member
    rcases member with ⟨original, originalMember, rfl⟩
    exact (typed original (by simp [originalMember])).weakenSamples
  · exact nextTyped
  · rw [List.mem_map] at member
    rcases member with ⟨original, originalMember, rfl⟩
    exact (typed original (by simp [originalMember])).weakenSamples

theorem allAffineValues_length
    (found : allAffineValues? expressions = some values) :
    values.length = expressions.length := by
  induction expressions generalizing values with
  | nil => simp [allAffineValues?] at found; subst values; rfl
  | cons head tail ih =>
      cases headFound : affineValue? head with
      | none => simp [allAffineValues?, List.mapM_cons, headFound] at found
      | some headValue =>
          cases tailFound : allAffineValues? tail with
          | none =>
              change List.mapM affineValue? tail = none at tailFound
              simp [allAffineValues?, List.mapM_cons, headFound, tailFound] at found
          | some tailValues =>
              change List.mapM affineValue? tail = some tailValues at tailFound
              have valuesEq : values = headValue :: tailValues := by
                simpa [allAffineValues?, List.mapM_cons, headFound, tailFound] using found.symm
              subst values
              simp [ih tailFound]

theorem allConstantValues_length
    (found : allConstantValues? expressions = some values) :
    values.length = expressions.length := by
  induction expressions generalizing values with
  | nil => simp [allConstantValues?] at found; subst values; rfl
  | cons head tail ih =>
      cases headFound : constantValue? head with
      | none => simp [allConstantValues?, List.mapM_cons, headFound] at found
      | some headValue =>
          cases tailFound : allConstantValues? tail with
          | none =>
              change List.mapM constantValue? tail = none at tailFound
              simp [allConstantValues?, List.mapM_cons, headFound, tailFound] at found
          | some tailValues =>
              change List.mapM constantValue? tail = some tailValues at tailFound
              have valuesEq : values = headValue :: tailValues := by
                simpa [allConstantValues?, List.mapM_cons, headFound, tailFound] using found.symm
              subst values
              simp [ih tailFound]
theorem allAffineValues_of_wellTyped_values
    {n : Nat} {expressions : List (AffineExpr n)} {context : List Ty}
    {mode : Mode}
    (typed : ∀ expression ∈ expressions,
      WellTyped context expression (.float mode))
    (values : ∀ expression ∈ expressions, expression.isValue = true) :
    ∃ coordinates, allAffineValues? expressions = some coordinates := by
  induction expressions with
  | nil => exact ⟨[], rfl⟩
  | cons head tail ih =>
      obtain ⟨coordinate, rfl⟩ := wellTyped_real_value (typed head (by simp))
        (values head (by simp))
      obtain ⟨coordinates, equation⟩ := ih
        (fun expression member => typed expression (by simp [member]))
        (fun expression member => values expression (by simp [member]))
      change List.mapM affineValue? tail = some coordinates at equation
      exact ⟨coordinate :: coordinates, by simp [allAffineValues?, affineValue?, equation]⟩

theorem allConstantValues_of_wellTypedG_values
    {n : Nat} {expressions : List (AffineExpr n)} {context : List Ty}
    (typed : ∀ expression ∈ expressions,
      WellTyped context expression (.float .G))
    (values : ∀ expression ∈ expressions, expression.isValue = true)
    (constant : ∀ expression ∈ expressions, expression.GConstant) :
    ∃ coordinates, allConstantValues? expressions = some coordinates := by
  induction expressions with
  | nil => exact ⟨[], rfl⟩
  | cons head tail ih =>
      obtain ⟨coordinate, equation⟩ := constantValue?_eq_some_of_wellTypedG
        (typed head (by simp)) (values head (by simp)) (constant head (by simp))
      obtain ⟨coordinates, tailEquation⟩ := ih
        (fun expression member => typed expression (by simp [member]))
        (fun expression member => values expression (by simp [member]))
        (fun expression member => constant expression (by simp [member]))
      change List.mapM constantValue? tail = some coordinates at tailEquation
      exact ⟨coordinate :: coordinates,
        by simp [allConstantValues?, equation, tailEquation]⟩

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
  | sampleG (site : Mode × Tag) (fiber : Measure ℝ) (continuation : ℝ → AffineExpr sampleCount)
  | stuck

namespace SymbolicAction

def GConstant : SymbolicAction laws n → Prop
  | .next expression => expression.GConstant
  | .sampleE _ _ _ continuation => continuation.GConstant
  | .sampleG _ _ continuation => ∀ value, (continuation value).GConstant
  | .stuck => True

noncomputable def realize (environment : Env n) : SymbolicAction laws n → Action
  | .next expression => .next (expression.realize environment)
  | .sampleE op affine general continuation =>
      .sample (.E, .stochastic op) (primitiveFiber (.stochastic op)
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

theorem gconstant_wrap (action : SymbolicAction laws n)
    (constant : action.GConstant)
    (context : AffineExpr n → AffineExpr n) (liftedContext : AffineExpr (n + 1) → AffineExpr (n + 1))
    (contextConstant : ∀ expression, expression.GConstant →
      (context expression).GConstant)
    (liftedConstant : ∀ expression, expression.GConstant →
      (liftedContext expression).GConstant) :
    (action.wrap context liftedContext).GConstant := by
  cases action with
  | next expression => exact contextConstant expression constant
  | sampleE op affine general continuation => exact liftedConstant continuation constant
  | sampleG site fiber continuation =>
      intro value
      exact contextConstant (continuation value) (constant value)
  | stuck => trivial

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

theorem WellTyped.gconstant (typed : WellTyped ty action) : action.GConstant := by
  cases typed with
  | next typed => exact typed.gconstant
  | sampleE ha hg typed => exact typed.gconstant
  | sampleG typed => exact fun value => (typed value).gconstant

end SymbolicAction

noncomputable def symbolicReduce
    (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    AffineExpr n → SymbolicAction laws n
  | expression@(.bvar _) => .stuck
  | expression@(.unit) | expression@(.bool _) | expression@(.real _ _)
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
        | .real .G value => .next (.real .E value) | _ => .stuck
      else (symbolicReduce laws body).wrap .promote .promote
  | .neg mode body =>
      if body.isValue then match body with
        | .real _ value => .next (.real mode (Affine.neg value)) | _ => .stuck
      else (symbolicReduce laws body).wrap (.neg mode) (.neg mode)
  | .add mode left right =>
      if left.isValue then
        if right.isValue then match left.affineValue?, right.affineValue? with
          | some x, some y => .next (.real mode (Affine.add x y)) | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.add mode left)
          (.add mode left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .add mode next right)
        (fun next => .add mode next right.weakenSamples)
  | .mul mode left right =>
      if left.isValue then
        if right.isValue then match left.affineValue?, right.affineValue? with
          | some x, some y => match Affine.mul? x y with
            | some result => .next (.real mode result) | none => .stuck
          | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.mul mode left)
          (.mul mode left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .mul mode next right)
        (fun next => .mul mode next right.weakenSamples)
  | .div mode left right =>
      if left.isValue then
        if right.isValue then match left.affineValue?, right.affineValue? with
          | some x, some y => match Affine.div? x y with
            | some result => .next (.real mode result) | none => .stuck
          | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.div mode left)
          (.div mode left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .div mode next right)
        (fun next => .div mode next right.weakenSamples)
  | .lt left right =>
      if left.isValue then
        if right.isValue then match left.constantValue?, right.constantValue? with
          | some x, some y => .next (.bool (x < y)) | _, _ => .stuck
        else (symbolicReduce laws right).wrap (.lt left) (.lt left.weakenSamples)
      else (symbolicReduce laws left).wrap (fun next => .lt next right)
        (fun next => .lt next right.weakenSamples)
  | .sample mode op affine general =>
      match hAffine : firstNonValue affine with
      | some (front, current, suffix) =>
          (symbolicReduce laws current).wrap
            (fun next => .sample mode op (front ++ next :: suffix) general)
            (fun next => .sample mode op
              (front.map weakenSamples ++ next :: suffix.map weakenSamples)
              (general.map weakenSamples))
      | none => match hGeneral : firstNonValue general with
        | some (front, current, suffix) =>
            (symbolicReduce laws current).wrap
              (fun next => .sample mode op affine (front ++ next :: suffix))
              (fun next => .sample mode op (affine.map weakenSamples)
                (front.map weakenSamples ++ next :: suffix.map weakenSamples))
        | none => match mode, op, allAffineValues? affine, allConstantValues? general with
          | .E, .stochastic base, some affineValues, some generalValues =>
              .sampleE base affineValues generalValues (.real .E (Affine.fresh n))
          | _, _, _, _ => match allConstantValues? affine, allConstantValues? general with
            | some affineValues, some generalValues =>
                .sampleG (mode, op) (primitiveFiber op affineValues generalValues)
                  (fun value => .real mode (value, 0))
            | _, _ => .stuck
termination_by expression => sizeOf expression
decreasing_by
  all_goals
    first
    | decreasing_trivial
    | apply Nat.lt_trans (List.sizeOf_lt_of_mem (firstNonValue_current_mem
        (expressions := affine) (by assumption)))
      simp_wf <;> omega
    | apply Nat.lt_trans (List.sizeOf_lt_of_mem (firstNonValue_current_mem
        (expressions := general) (by assumption)))
      simp_wf <;> omega

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
      | .real .G value => .next (.real .E value) | _ => .stuck
    else (symbolicReduce laws body).wrap .promote .promote := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_let_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (value body : AffineExpr n) :
    symbolicReduce laws (.letE value body) =
    if value.isValue then .next (body.substHead value)
    else (symbolicReduce laws value).wrap (fun next => .letE next body)
      (fun next => .letE next body.weakenSamples) := by
  rw [symbolicReduce.eq_def]

theorem symbolicReduce_sample_eq (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (op : Tag) (affine general : List (AffineExpr n)) :
    symbolicReduce laws (.sample mode op affine general) =
      match hAffine : firstNonValue affine with
      | some (front, current, suffix) =>
          (symbolicReduce laws current).wrap
            (fun next => .sample mode op (front ++ next :: suffix) general)
            (fun next => .sample mode op
              (front.map weakenSamples ++ next :: suffix.map weakenSamples)
              (general.map weakenSamples))
      | none => match hGeneral : firstNonValue general with
        | some (front, current, suffix) =>
            (symbolicReduce laws current).wrap
              (fun next => .sample mode op affine (front ++ next :: suffix))
              (fun next => .sample mode op (affine.map weakenSamples)
                (front.map weakenSamples ++ next :: suffix.map weakenSamples))
        | none => match mode, op, allAffineValues? affine, allConstantValues? general with
          | .E, .stochastic base, some affineValues, some generalValues =>
              .sampleE base affineValues generalValues (.real .E (Affine.fresh n))
          | _, _, _, _ => match allConstantValues? affine, allConstantValues? general with
            | some affineValues, some generalValues =>
                .sampleG (mode, op) (primitiveFiber op affineValues generalValues)
                  (fun value => .real mode (value, 0))
            | _, _ => .stuck := by
  rw [symbolicReduce.eq_def]

set_option maxHeartbeats 800000 in
theorem symbolicReduce_realize
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {expression : AffineExpr n} (typed : WellTyped context expression ty)
    (gconstant : expression.GConstant)
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
      simp only [GConstant] at gconstant
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
            ihr gconstant.2 environment, realize_isValue,
            if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .pair
            next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | inl valueTyped ih =>
      rename_i context' value leftTy rightTy
      simp only [GConstant] at gconstant
      rw [symbolicReduce, realize, reduce, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [valueIsValue, SymbolicAction.realize, realize]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .inl next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih gconstant environment]
  | inr valueTyped ih =>
      rename_i context' value rightTy leftTy
      simp only [GConstant] at gconstant
      rw [symbolicReduce, realize, reduce, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [valueIsValue, SymbolicAction.realize, realize]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .inr next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih gconstant environment]
  | cons headTyped tailTyped ihh iht =>
      rename_i context' head element tail
      simp only [GConstant] at gconstant
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
            iht gconstant.2 environment, realize_isValue,
            if_neg tailValue]
      · simp only [headValue, Bool.eq_false_of_not_eq_true headValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .cons next (tail.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihh gconstant.1 environment]
  | app functionTyped operandTyped ihf iho =>
      rename_i context' function argumentTy result operand
      simp only [GConstant] at gconstant
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
            iho gconstant.2 environment, realize_isValue, if_neg operandValue]
          all_goals simp_all [isValue]
      · rw [symbolicReduce_app_eq]
        simp only [functionValue, Bool.eq_false_of_not_eq_true functionValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .app next (operand.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihf gconstant.1 environment]
        all_goals simp_all [isValue]
  | fst pairTyped ih =>
      rename_i context' pairValue leftTy rightTy
      simp only [GConstant] at gconstant
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
          (lifted_realize := by intros; simp only [realize]), ih gconstant environment]
        all_goals simp_all [isValue]
  | snd pairTyped ih =>
      rename_i context' pairValue leftTy rightTy
      simp only [GConstant] at gconstant
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
          (lifted_realize := by intros; simp only [realize]), ih gconstant environment]
        all_goals simp_all [isValue]
  | matchSum scrutineeTyped leftTyped rightTyped ihs ihl ihr =>
      rename_i context' scrutinee leftTy rightTy left result right
      simp only [GConstant] at gconstant
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
          ihs gconstant.1 environment]
        all_goals simp_all [isValue]
  | matchList scrutineeTyped nilTyped consTyped ihs ihn ihc =>
      rename_i context' scrutinee element nilCase result consCase
      simp only [GConstant] at gconstant
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
          ihs gconstant.1 environment]
        all_goals simp_all [isValue]
  | ite conditionTyped thenTyped elseTyped ihc iht ihe =>
      rename_i context' condition thenBranch result elseBranch
      simp only [GConstant] at gconstant
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
          ihc gconstant.1 environment]
        all_goals simp_all [isValue]
  | letE valueTyped bodyTyped ihv ihb =>
      rename_i context' value valueTy body result
      simp only [GConstant] at gconstant
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
          ihv gconstant.1 environment]
        all_goals simp_all [isValue]
  | promote valueTyped ih =>
      rename_i context' value
      simp only [GConstant] at gconstant
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
          (lifted_realize := by intros; simp only [realize]), ih gconstant environment]
        all_goals simp_all [isValue]
  | negE valueTyped ih =>
      rename_i context' value
      simp only [GConstant] at gconstant
      rw [realize, MeasurableActionFamily.reduce_neg_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [SymbolicAction.realize, realize, Affine.eval_neg]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .neg .E next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih gconstant environment]
        all_goals simp_all [isValue]
  | negG valueTyped ih =>
      rename_i context' value
      simp only [GConstant] at gconstant
      rw [realize, MeasurableActionFamily.reduce_neg_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [SymbolicAction.realize, realize, Affine.eval_neg]
      · simp only [valueIsValue, Bool.eq_false_of_not_eq_true valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .neg .G next)
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize]), ih gconstant environment]
        all_goals simp_all [isValue]
  | addE leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      simp only [GConstant] at gconstant
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
            (ExprContext := fun next => .add .E (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr gconstant.2 environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .add .E next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | addG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      simp only [GConstant] at gconstant
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
            (ExprContext := fun next => .add .G (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr gconstant.2 environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .add .G next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | mulEG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      simp only [GConstant] at gconstant
      rw [realize, MeasurableActionFamily.reduce_mul_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases y with ⟨y0, yc⟩
          simp only [GConstant] at gconstant
          rw [gconstant.2]
          simp [affineValue?, Affine.mul?, SymbolicAction.realize, realize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero]
          have sumRule : (∑ i, y0 * x.2 i * environment i) =
              y0 * ∑ i, x.2 i * environment i := by
            rw [Finset.mul_sum]
            apply Finset.sum_congr rfl
            intro i _
            ring
          rw [sumRule]
          ring
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .mul .E (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr gconstant.2 environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .mul .E next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | mulGG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      simp only [GConstant] at gconstant
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
          simp only [GConstant] at gconstant
          rcases gconstant with ⟨rfl, rfl⟩
          simp [affineValue?, Affine.mul?, SymbolicAction.realize, realize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero]
          ring
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .mul .G (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr gconstant.2 environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .mul .G next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | divEG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      simp only [GConstant] at gconstant
      rw [realize, MeasurableActionFamily.reduce_div_eq, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases y with ⟨y0, yc⟩
          simp only [GConstant] at gconstant
          rw [gconstant.2]
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
            (ExprContext := fun next => .div .E (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr gconstant.2 environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .div .E next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | divGG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      simp only [GConstant] at gconstant
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
          simp only [GConstant] at gconstant
          rcases gconstant with ⟨rfl, rfl⟩
          simp [affineValue?, Affine.div?, SymbolicAction.realize, realize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero,
            div_eq_mul_inv]
          ring
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .div .G (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr gconstant.2 environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .div .G next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | lt leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      simp only [GConstant] at gconstant
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
          simp only [GConstant] at gconstant
          rcases gconstant with ⟨rfl, rfl⟩
          simp [constantValue?, SymbolicAction.realize, realize, Expr.isValue,
            realValue?, Symbolic.Affine.eval, Finset.sum_const_zero]
        · simp only [rightValue, Bool.eq_false_of_not_eq_true rightValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .lt (left.realize environment) next)
            (context_realize := by intros; simp only [realize])
            (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
            ihr gconstant.2 environment, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.eq_false_of_not_eq_true leftValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .lt next (right.realize environment))
          (context_realize := by intros; simp only [realize])
          (lifted_realize := by intros; simp only [realize, realize_weakenSamples]),
          ihl gconstant.1 environment]
  | sampleE op ha hg hAffine hGeneral ihAffine ihGeneral =>
      rename_i affine context' general
      simp only [GConstant] at gconstant
      rw [realize, MeasurableActionFamily.reduce_sample_eq,
        firstNonValue_realize, symbolicReduce_sample_eq]
      split
      · rename_i front current suffix hA
        simp only [hA, Option.map_some]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .sample .E (.stochastic op)
            (front.map (realize environment) ++ next :: suffix.map (realize environment))
            (general.map (realize environment)))
          (context_realize := by
            intro next
            simp only [realize, List.map_append, List.map_cons])
          (lifted_realize := by
            intro next value
            simp only [realize, List.map_append, List.map_cons, List.map_map,
              Function.comp_apply]
            have mapRule (expressions : List (AffineExpr n)) :
                expressions.map (realize (Env.cons value environment) ∘ weakenSamples) =
                  expressions.map (realize environment) := by
              apply List.map_congr_left
              intro expression _
              exact realize_weakenSamples expression value environment
            rw [mapRule front, mapRule suffix, mapRule general]),
          ihAffine current (firstNonValue_current_mem hA)
            (gconstant.1 current (firstNonValue_current_mem hA)) environment]
      · split
        · rename_i hANone front current suffix hG
          simp only [hANone, Option.map_none, hG, Option.map_some]
          rw [firstNonValue_realize, hG]
          simp only [Option.map_some]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .sample .E (.stochastic op)
              (affine.map (realize environment))
              (front.map (realize environment) ++ next :: suffix.map (realize environment)))
            (context_realize := by
              intro next
              simp only [realize, List.map_append, List.map_cons])
            (lifted_realize := by
              intro next value
              simp only [realize, List.map_append, List.map_cons, List.map_map,
                Function.comp_apply]
              have mapRule (expressions : List (AffineExpr n)) :
                  expressions.map (realize (Env.cons value environment) ∘ weakenSamples) =
                    expressions.map (realize environment) := by
                apply List.map_congr_left
                intro expression _
                exact realize_weakenSamples expression value environment
              rw [mapRule affine, mapRule front, mapRule suffix]),
            ihGeneral current (firstNonValue_current_mem hG)
              (gconstant.2 current (firstNonValue_current_mem hG)) environment]
        · rename_i hANone hGNone
          have affineAreValues := (firstNonValue_eq_none_iff affine).mp hANone
          have generalAreValues := (firstNonValue_eq_none_iff general).mp hGNone
          obtain ⟨affineValues, affineFound⟩ :=
            allAffineValues_of_wellTyped_values hAffine affineAreValues
          obtain ⟨generalValues, generalFound⟩ :=
            allConstantValues_of_wellTypedG_values hGeneral generalAreValues gconstant.2
          have affineRealized := allRealValues_realize_of_allAffineValues
            affineFound environment
          have generalRealized := allRealValues_realize_of_allConstantValues
            generalFound environment
          simp only [affineFound, generalFound, SymbolicAction.realize]
          rw [hANone, firstNonValue_realize, hGNone, affineRealized, generalRealized]
          simp only [Option.map_none, Affine.eval_fresh]
          congr 1
          funext value
          simp [realize]
  | sampleG op ha hg hAffine hGeneral ihAffine ihGeneral =>
      rename_i affine context' general
      simp only [GConstant] at gconstant
      rw [realize, MeasurableActionFamily.reduce_sample_eq,
        firstNonValue_realize, symbolicReduce_sample_eq]
      split
      · rename_i front current suffix hA
        simp only [hA, Option.map_some]
        rw [SymbolicAction.realize_wrap
          (ExprContext := fun next => .sample .G (.stochastic op)
            (front.map (realize environment) ++ next :: suffix.map (realize environment))
            (general.map (realize environment)))
          (context_realize := by
            intro next
            simp only [realize, List.map_append, List.map_cons])
          (lifted_realize := by
            intro next value
            simp only [realize, List.map_append, List.map_cons, List.map_map,
              Function.comp_apply]
            have mapRule (expressions : List (AffineExpr n)) :
                expressions.map (realize (Env.cons value environment) ∘ weakenSamples) =
                  expressions.map (realize environment) := by
              apply List.map_congr_left
              intro expression _
              exact realize_weakenSamples expression value environment
            rw [mapRule front, mapRule suffix, mapRule general]),
          ihAffine current (firstNonValue_current_mem hA)
            (gconstant.1 current (firstNonValue_current_mem hA)) environment]
      · split
        · rename_i hANone front current suffix hG
          simp only [hANone, Option.map_none, hG, Option.map_some]
          rw [firstNonValue_realize, hG]
          simp only [Option.map_some]
          rw [SymbolicAction.realize_wrap
            (ExprContext := fun next => .sample .G (.stochastic op)
              (affine.map (realize environment))
              (front.map (realize environment) ++ next :: suffix.map (realize environment)))
            (context_realize := by
              intro next
              simp only [realize, List.map_append, List.map_cons])
            (lifted_realize := by
              intro next value
              simp only [realize, List.map_append, List.map_cons, List.map_map,
                Function.comp_apply]
              have mapRule (expressions : List (AffineExpr n)) :
                  expressions.map (realize (Env.cons value environment) ∘ weakenSamples) =
                    expressions.map (realize environment) := by
                apply List.map_congr_left
                intro expression _
                exact realize_weakenSamples expression value environment
              rw [mapRule affine, mapRule front, mapRule suffix]),
            ihGeneral current (firstNonValue_current_mem hG)
              (gconstant.2 current (firstNonValue_current_mem hG)) environment]
        · rename_i hANone hGNone
          have affineAreValues := (firstNonValue_eq_none_iff affine).mp hANone
          have generalAreValues := (firstNonValue_eq_none_iff general).mp hGNone
          obtain ⟨affineValues, affineFound⟩ :=
            allConstantValues_of_wellTypedG_values hAffine affineAreValues gconstant.1
          obtain ⟨generalValues, generalFound⟩ :=
            allConstantValues_of_wellTypedG_values hGeneral generalAreValues gconstant.2
          have affineRealized := allRealValues_realize_of_allConstantValues
            affineFound environment
          have generalRealized := allRealValues_realize_of_allConstantValues
            generalFound environment
          simp only [affineFound, generalFound, SymbolicAction.realize]
          rw [hANone, firstNonValue_realize, hGNone, affineRealized, generalRealized]
          simp only [Option.map_none, realize]
          congr 1
          funext value
          simp [Symbolic.Affine.eval]

set_option maxHeartbeats 1600000 in
set_option maxRecDepth 4000 in
theorem symbolicReduce_wellTyped
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {expression : AffineExpr n} (typed : WellTyped [] expression ty) :
    SymbolicAction.WellTyped ty (symbolicReduce laws expression) := by
  generalize hcontext : ([] : List Ty) = context at typed
  induction typed
  case sampleE op affineLength generalLength affineTyped generalTyped iha ihg =>
    cases hcontext
    rw [symbolicReduce_sample_eq]
    cases found : firstNonValue _ with
    | some result =>
        rcases result with ⟨front, current, suffix⟩
        let currentTyped := affineTyped current (firstNonValue_current_mem found)
        let currentAction := iha current (firstNonValue_current_mem found) rfl
        apply currentAction.wrap
        · intro next nextTyped
          apply WellTyped.sampleE op
          · rw [← affineLength, firstNonValue_eq_some_append found]; simp
          · exact generalLength
          · exact wellTyped_list_replace
              (by simpa [firstNonValue_eq_some_append found] using affineTyped) nextTyped
          · exact generalTyped
        · intro next nextTyped
          apply WellTyped.sampleE op
          · rw [← affineLength, firstNonValue_eq_some_append found]
            simp
          · simpa using generalLength
          · exact wellTyped_list_replace_weakenSamples
              (by simpa [firstNonValue_eq_some_append found] using affineTyped)
              nextTyped
          · intro child member
            rw [List.mem_map] at member
            rcases member with ⟨original, originalMember, rfl⟩
            exact (generalTyped original originalMember).weakenSamples
    | none =>
        cases foundGeneral : firstNonValue _ with
        | some result =>
          rcases result with ⟨front, current, suffix⟩
          let currentAction := ihg current (firstNonValue_current_mem foundGeneral) rfl
          apply currentAction.wrap
          · intro next nextTyped
            apply WellTyped.sampleE op
            · exact affineLength
            · rw [← generalLength, firstNonValue_eq_some_append foundGeneral]; simp
            · exact affineTyped
            · exact wellTyped_list_replace
                (by simpa [firstNonValue_eq_some_append foundGeneral] using generalTyped)
                nextTyped
          · intro next nextTyped
            apply WellTyped.sampleE op
            · simpa using affineLength
            · rw [← generalLength, firstNonValue_eq_some_append foundGeneral]; simp
            · intro child member
              rw [List.mem_map] at member
              rcases member with ⟨original, originalMember, rfl⟩
              exact (affineTyped original originalMember).weakenSamples
            · exact wellTyped_list_replace_weakenSamples
                (by simpa [firstNonValue_eq_some_append foundGeneral] using generalTyped)
                nextTyped
        | none =>
          have affineValues := (firstNonValue_eq_none_iff _).mp found
          have generalValues := (firstNonValue_eq_none_iff _).mp foundGeneral
          obtain ⟨av, ha⟩ := allAffineValues_of_wellTyped_values affineTyped affineValues
          obtain ⟨gv, hg⟩ := allConstantValues_of_wellTypedG_values generalTyped
            generalValues (fun child member => (generalTyped child member).gconstant)
          simp only [ha, hg]
          exact .sampleE ((allAffineValues_length ha).trans affineLength)
            ((allConstantValues_length hg).trans generalLength) (.realE)
  case sampleG op affineLength generalLength affineTyped generalTyped iha ihg =>
    cases hcontext
    rw [symbolicReduce_sample_eq]
    cases found : firstNonValue _ with
    | some result =>
        rcases result with ⟨front, current, suffix⟩
        let currentAction := iha current (firstNonValue_current_mem found) rfl
        apply currentAction.wrap
        · intro next nextTyped
          apply WellTyped.sampleG op
          · rw [← affineLength, firstNonValue_eq_some_append found]; simp
          · exact generalLength
          · exact wellTyped_list_replace
              (by simpa [firstNonValue_eq_some_append found] using affineTyped) nextTyped
          · exact generalTyped
        · intro next nextTyped
          apply WellTyped.sampleG op
          · rw [← affineLength, firstNonValue_eq_some_append found]; simp
          · simpa using generalLength
          · exact wellTyped_list_replace_weakenSamples
              (by simpa [firstNonValue_eq_some_append found] using affineTyped)
              nextTyped
          · intro child member
            rw [List.mem_map] at member
            rcases member with ⟨original, originalMember, rfl⟩
            exact (generalTyped original originalMember).weakenSamples
    | none =>
        cases foundGeneral : firstNonValue _ with
        | some result =>
          rcases result with ⟨front, current, suffix⟩
          let currentAction := ihg current (firstNonValue_current_mem foundGeneral) rfl
          apply currentAction.wrap
          · intro next nextTyped
            apply WellTyped.sampleG op
            · exact affineLength
            · rw [← generalLength, firstNonValue_eq_some_append foundGeneral]; simp
            · exact affineTyped
            · exact wellTyped_list_replace
                (by simpa [firstNonValue_eq_some_append foundGeneral] using generalTyped)
                nextTyped
          · intro next nextTyped
            apply WellTyped.sampleG op
            · simpa using affineLength
            · rw [← generalLength, firstNonValue_eq_some_append foundGeneral]; simp
            · intro child member
              rw [List.mem_map] at member
              rcases member with ⟨original, originalMember, rfl⟩
              exact (affineTyped original originalMember).weakenSamples
            · exact wellTyped_list_replace_weakenSamples
                (by simpa [firstNonValue_eq_some_append foundGeneral] using generalTyped)
                nextTyped
        | none =>
          have affineValues := (firstNonValue_eq_none_iff _).mp found
          have generalValues := (firstNonValue_eq_none_iff _).mp foundGeneral
          obtain ⟨av, ha⟩ := allConstantValues_of_wellTypedG_values affineTyped
            affineValues (fun child member => (affineTyped child member).gconstant)
          obtain ⟨gv, hg⟩ := allConstantValues_of_wellTypedG_values generalTyped
            generalValues (fun child member => (generalTyped child member).gconstant)
          simp only [ha, hg]
          exact .sampleG fun value => .realG rfl
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
          constantValue?_eq_some_of_wellTypedG leftTyped leftValue leftTyped.gconstant
        obtain ⟨rightConstant, rightEquation⟩ :=
          constantValue?_eq_some_of_wellTypedG rightTyped rightValue rightTyped.gconstant
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
        have leftZero : leftCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using leftTyped.gconstant
        have rightZero : rightCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using rightTyped.gconstant
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
        have rightZero : rightCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using rightTyped.gconstant
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
  case mulEG left right leftTyped rightTyped ihLeft ihRight =>
    cases hcontext
    simp only [symbolicReduce]
    by_cases leftValue : left.isValue = true
    · simp only [leftValue, ↓reduceIte]
      by_cases rightValue : right.isValue = true
      · simp only [rightValue, ↓reduceIte]
        obtain ⟨leftAffine, rfl⟩ := wellTyped_real_value leftTyped leftValue
        obtain ⟨rightAffine, rfl⟩ := wellTyped_real_value rightTyped rightValue
        rcases rightAffine with ⟨rightConstant, rightCoefficients⟩
        have rightZero : rightCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using rightTyped.gconstant
        simp only [affineValue?, Affine.mul?, rightZero, ↓reduceIte, Option.some.injEq]
        exact SymbolicAction.WellTyped.next .realE
      · simp only [rightValue, ↓reduceIte]
        exact (ihRight rfl).wrap
          (fun next nextTyped => .mulEG leftTyped nextTyped)
          (fun next nextTyped => .mulEG leftTyped.weakenSamples nextTyped)
    · simp only [leftValue, ↓reduceIte]
      exact (ihLeft rfl).wrap
        (fun next nextTyped => .mulEG nextTyped rightTyped)
        (fun next nextTyped => .mulEG nextTyped rightTyped.weakenSamples)
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
        have leftZero : leftCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using leftTyped.gconstant
        have rightZero : rightCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using rightTyped.gconstant
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
      have zero : coefficients = 0 := by
        simpa only [AffineExpr.GConstant] using valueTyped.gconstant
      exact .next (.realG (by rw [zero]; funext index; simp [Affine.neg]))
    · simp only [isValue, ↓reduceIte]
      exact (ih rfl).wrap
        (fun next nextTyped => .negG nextTyped)
        (fun next nextTyped => .negG nextTyped)
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
        have leftZero : leftCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using leftTyped.gconstant
        have rightZero : rightCoefficients = 0 := by
          simpa only [AffineExpr.GConstant] using rightTyped.gconstant
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

private def CoordinateGRelation (mode : Mode) (coordinate : Affine sampleCount) : Prop :=
  mode = .G → coordinate.2 = 0

private theorem coordinateRelation_of_gconstant (expression : AffineExpr sampleCount)
    (constant : expression.GConstant) :
    List.Forall₂ CoordinateGRelation expression.skeleton.coordinateModes
      expression.coordinates := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
    subst size
    have recurse (child : AffineExpr sampleCount) (childConstant : child.GConstant)
        (smaller : sizeOf child < sizeOf expression) :
        List.Forall₂ CoordinateGRelation child.skeleton.coordinateModes child.coordinates :=
      ih (sizeOf child) smaller child childConstant rfl
    have listRecurse (children : List (AffineExpr sampleCount))
        (childrenConstant : ∀ child ∈ children, child.GConstant)
        (smaller : ∀ child ∈ children, sizeOf child < sizeOf expression) :
        List.Forall₂ CoordinateGRelation
          (children.flatMap (fun child => child.skeleton.coordinateModes))
          (children.flatMap AffineExpr.coordinates) := by
      induction children with
      | nil => exact .nil
      | cons child children ihChildren =>
          simp only [List.flatMap_cons]
          exact List.rel_append
            (recurse child (childrenConstant child (by simp))
              (smaller child (by simp)))
            (ihChildren (fun next member => childrenConstant next (by simp [member]))
              (fun next member => smaller next (by simp [member])))
    cases expression with
    | bvar | unit | bool | nil =>
        simp only [skeleton, Expr.coordinateModes, coordinates]
        exact .nil
    | real mode value =>
        simp only [skeleton, Expr.coordinateModes, coordinates]
        cases mode
        · exact .cons (fun no => by cases no) .nil
        · unfold GConstant at constant
          exact .cons (fun _ => constant) .nil
    | lam body | fix body | fst body | snd body
    | inl body | inr body | promote body | neg _ body =>
        simp only [skeleton, Expr.coordinateModes, coordinates]
        simp only [GConstant] at constant
        exact recurse body constant (by simp_wf <;> omega)
    | app left right | pair left right | cons left right
    | add _ left right | mul _ left right | div _ left right | lt left right
    | letE left right =>
        simp only [skeleton, Expr.coordinateModes, coordinates]
        simp only [GConstant] at constant
        exact List.rel_append
          (recurse left constant.1 (by simp_wf <;> omega))
          (recurse right constant.2 (by simp_wf <;> omega))
    | matchSum first second third | matchList first second third
    | ite first second third =>
        simp only [skeleton, Expr.coordinateModes, coordinates]
        simp only [GConstant] at constant
        exact List.rel_append
          (List.rel_append
            (recurse first constant.1 (by simp_wf <;> omega))
            (recurse second constant.2.1 (by simp_wf <;> omega)))
          (recurse third constant.2.2 (by simp_wf <;> omega))
    | sample mode op affine general =>
        simp only [GConstant] at constant
        simpa only [skeleton, Expr.coordinateModes, coordinates,
          List.flatMap_map, Function.comp_def] using List.rel_append
            (listRecurse affine constant.1 (fun child member =>
              Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)))
            (listRecurse general constant.2 (fun child member =>
              Nat.lt_trans (List.sizeOf_lt_of_mem member) (by simp_wf <;> omega)))

theorem gzero_of_gconstant {expression : AffineExpr sampleCount}
    (constant : expression.GConstant) : expression.GZero := by
  intro index coordinate modeAt coordinateAt
  have related := coordinateRelation_of_gconstant expression constant
  have retrieve : ∀ {modes : List Mode} {coordinates : List (Affine sampleCount)},
      List.Forall₂ CoordinateGRelation modes coordinates →
      ∀ (index : Nat) (coordinate : Affine sampleCount),
        modes[index]? = some Mode.G →
        coordinates[index]? = some coordinate → coordinate.2 = 0 := by
    intro modes coordinates related
    induction related with
    | nil => intro index coordinate modeAt; simp at modeAt
    | cons head tail ih =>
        intro index coordinate modeAt coordinateAt
        cases index with
        | zero =>
            simp only [List.getElem?_cons_zero, Option.some.injEq] at modeAt coordinateAt
            subst coordinate
            exact head modeAt
        | succ index =>
            simp only [List.getElem?_cons_succ] at modeAt coordinateAt
            exact ih index coordinate modeAt coordinateAt
  exact retrieve related index coordinate modeAt coordinateAt

theorem wellTyped_ofExpr_of_typed {expression : Expr}
    (typed : Determinize.Statement.Paper.Typed context expression ty)
    (sourceTags : (AffineExpr.ofExpr expression).SourceTags) :
    WellTyped context (AffineExpr.ofExpr expression) ty := by
  induction typed <;> simp only [ofExpr, SourceTags] at sourceTags ⊢
  all_goals try aesop (add safe constructors WellTyped)
  all_goals try (cases ‹Mode› <;> aesop (add safe constructors WellTyped))
  case sample =>
    rename_i affine context mode general op affineLength generalLength
      affineTyped generalTyped iha ihg
    cases mode with
    | E =>
        cases op with
        | stochastic op =>
            simp only [SourceTags] at sourceTags
            apply WellTyped.sampleE op
            · simpa only [List.length_map, Tag.base] using affineLength
            · simpa only [List.length_map, Tag.base] using generalLength
            · intro child member
              rw [List.mem_map] at member
              rcases member with ⟨original, originalMember, rfl⟩
              exact iha original originalMember
                (sourceTags.2.1 _ (List.mem_map_of_mem originalMember))
            · intro child member
              rw [List.mem_map] at member
              rcases member with ⟨original, originalMember, rfl⟩
              exact ihg original originalMember
                (sourceTags.2.2 _ (List.mem_map_of_mem originalMember))
        | mean op =>
            simp only [SourceTags] at sourceTags
            exact False.elim sourceTags.1
    | G =>
        cases op with
        | stochastic op =>
            simp only [SourceTags] at sourceTags
            apply WellTyped.sampleG op
            · simpa only [List.length_map, Tag.base] using affineLength
            · simpa only [List.length_map, Tag.base] using generalLength
            · intro child member
              rw [List.mem_map] at member
              rcases member with ⟨original, originalMember, rfl⟩
              exact iha original originalMember
                (sourceTags.2.1 _ (List.mem_map_of_mem originalMember))
            · intro child member
              rw [List.mem_map] at member
              rcases member with ⟨original, originalMember, rfl⟩
              exact ihg original originalMember
                (sourceTags.2.2 _ (List.mem_map_of_mem originalMember))
        | mean op =>
            simp only [SourceTags] at sourceTags
            exact False.elim sourceTags.1

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

noncomputable def toResidual (expression : AffineExpr sampleCount) (gzero : expression.GZero) :
    Symbolic.Residual sampleCount where
  skeleton := expression.skeleton
  coordinates := expression.coordinates
  coordinate_count := expression.coordinate_count
  general_independent := gzero
  realize := expression.realize
  realize_measurable := expression.realize_measurable
  realize_skeleton := expression.realize_skeleton
  realize_coordinates := expression.realize_coordinates

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

theorem actualInterpretation_bind_stepKernel
    (stepKernel : StepKernel) (state : Symbolic.State laws) :
    (Symbolic.actualInterpretation laws state).bind stepKernel.kernel =
      (Symbolic.SampleEnv.actualMeasure laws state.samples).bind fun environment =>
        stepMeasure (state.residual.realize environment) := by
  rw [Symbolic.actualInterpretation]
  rw [← Measure.bind_dirac_eq_map _ state.residual.realize_measurable]
  rw [Measure.bind_bind]
  · apply Measure.bind_congr_right
    filter_upwards [] with environment
    rw [Measure.dirac_bind stepKernel.kernel.measurable]
    exact stepKernel.kernel_eq_stepMeasure _
  · exact (Measurable.comp Measure.measurable_dirac
      state.residual.realize_measurable).aemeasurable
  · exact stepKernel.kernel.aemeasurable

end AffineExpr

end Symbolic

end Determinize.Proof.Paper
