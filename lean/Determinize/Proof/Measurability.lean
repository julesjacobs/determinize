import Determinize.Proof.Internal.Semantics
import Mathlib.Tactic

/-!
# Measurability of the paper semantics

The expression space is decomposed into countably many fixed-skeleton fibers.
On each fiber, reduction is represented by a measurable family of actions;
these pieces assemble into the global one-step kernel.
-/

set_option linter.unusedSimpArgs false
set_option linter.unusedTactic false
set_option linter.unreachableTactic false
set_option linter.unnecessarySeqFocus false
set_option linter.unnecessarySimpa false
set_option linter.style.haveILetI false
set_option linter.unusedVariables false

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory
open Determinize.Spec.Paper

noncomputable section

attribute [local simp] Determinize.Spec.Paper.reduce

/-- Extending a finite real environment is measurable. -/
theorem measurable_envCons {n : Nat} :
    Measurable (fun input : ℝ × (Fin n → ℝ) =>
      Env.cons input.1 input.2) := by
  rw [measurable_pi_iff]
  intro i
  refine Fin.cases ?_ (fun j => ?_) i
  · simpa [Env.cons] using
      (measurable_fst : Measurable (Prod.fst : ℝ × (Fin n → ℝ) → ℝ))
  · change Measurable (fun input : ℝ × (Fin n → ℝ) => input.2 j)
    exact (measurable_pi_apply j).comp measurable_snd

/-- An s-finite kernel packaged with the instance needed for composition. -/
structure SFiniteKernel (α β : Type*) [MeasurableSpace α] [MeasurableSpace β] where
  kernel : Kernel α β
  sfinite : IsSFiniteKernel kernel

namespace SFiniteKernel

variable {α β γ : Type*} [MeasurableSpace α] [MeasurableSpace β]
  [MeasurableSpace γ]

noncomputable def zero : SFiniteKernel α β := ⟨0, inferInstance⟩

noncomputable def deterministic (function : α → β) (measurable : Measurable function) :
    SFiniteKernel α β := ⟨Kernel.deterministic function measurable, inferInstance⟩

noncomputable def piecewise {set : Set α} (measurableSet : MeasurableSet set)
    (thenKernel elseKernel : SFiniteKernel α β) : SFiniteKernel α β := by
  classical
  letI := thenKernel.sfinite
  letI := elseKernel.sfinite
  exact ⟨Kernel.piecewise measurableSet thenKernel.kernel elseKernel.kernel, inferInstance⟩

noncomputable def mapWithInput (draw : SFiniteKernel α β)
    (transform : α × β → γ) (measurableTransform : Measurable transform) :
    SFiniteKernel α γ := by
  letI := draw.sfinite
  let transformed := Kernel.deterministic transform measurableTransform
  let paired := draw.kernel ⊗ₖ transformed
  exact ⟨paired.map Prod.snd, inferInstance⟩

noncomputable def pullback (draw : SFiniteKernel β γ) (parameters : α → β)
    (measurableParameters : Measurable parameters) : SFiniteKernel α γ := by
  letI := draw.sfinite
  exact ⟨draw.kernel ∘ₖ Kernel.deterministic parameters measurableParameters, inferInstance⟩

end SFiniteKernel

deriving instance Countable for Expr

/-- The canonical expression with a given skeleton: every literal is `0`. -/
def zeroFill : Skeleton → Expr
  | .bvar index => .bvar index
  | .unit => .unit
  | .reject => .reject
  | .discrete mode kind d => .discrete mode kind d
  | .bool value => .bool value
  | .real => .real 0
  | .lam body => .lam (zeroFill body)
  | .fix body => .fix (zeroFill body)
  | .app function argument => .app (zeroFill function) (zeroFill argument)
  | .pair left right => .pair (zeroFill left) (zeroFill right)
  | .fst pair => .fst (zeroFill pair)
  | .snd pair => .snd (zeroFill pair)
  | .inl value => .inl (zeroFill value)
  | .inr value => .inr (zeroFill value)
  | .matchSum scrutinee left right =>
      .matchSum (zeroFill scrutinee) (zeroFill left) (zeroFill right)
  | .nil => .nil
  | .cons head tail => .cons (zeroFill head) (zeroFill tail)
  | .matchList scrutinee nilCase consCase =>
      .matchList (zeroFill scrutinee) (zeroFill nilCase) (zeroFill consCase)
  | .ite condition thenBranch elseBranch =>
      .ite (zeroFill condition) (zeroFill thenBranch) (zeroFill elseBranch)
  | .letE value body => .letE (zeroFill value) (zeroFill body)
  | .neg body => .neg (zeroFill body)
  | .add left right => .add (zeroFill left) (zeroFill right)
  | .mul left right => .mul (zeroFill left) (zeroFill right)
  | .div left right => .div (zeroFill left) (zeroFill right)
  | .lt left right => .lt (zeroFill left) (zeroFill right)
  | .uniform mode kind lower upper => .uniform mode kind (zeroFill lower) (zeroFill upper)
  | .gaussian mode kind mean variance => .gaussian mode kind (zeroFill mean) (zeroFill variance)
  | .poisson mode kind rate => .poisson mode kind (zeroFill rate)
  | .bernoulli mode kind probability => .bernoulli mode kind (zeroFill probability)
  | .exponential mode kind rate => .exponential mode kind (zeroFill rate)
  | .beta mode kind left right => .beta mode kind (zeroFill left) (zeroFill right)
  | .gamma mode kind shape rate => .gamma mode kind (zeroFill shape) (zeroFill rate)

@[simp] theorem zeroFill_skeleton (skeleton : Skeleton) :
    (zeroFill skeleton).skeleton = skeleton := by
  induction skeleton <;> simp [zeroFill, Expr.skeleton, *]

namespace RealCoordinates

theorem measurable_code : Measurable RealCoordinates.code :=
  comap_measurable _

theorem measurable_length :
    Measurable fun coordinates : RealCoordinates => coordinates.values.length :=
  measurable_fst.comp measurable_code

theorem measurable_getD (index : Nat) :
    Measurable fun coordinates : RealCoordinates => coordinates.values.getD index 0 :=
  (measurable_pi_apply index).comp (measurable_snd.comp measurable_code)

/-- A map into real coordinates is measurable once its length and each of its
zero-padded coordinates are. -/
theorem measurable_of_length_getD {α : Type*} [MeasurableSpace α]
    {coordinates : α → RealCoordinates}
    (lengthMeasurable : Measurable fun parameter => (coordinates parameter).values.length)
    (coordinateMeasurable : ∀ index : Nat,
      Measurable fun parameter => (coordinates parameter).values.getD index 0) :
    Measurable coordinates := by
  rw [measurable_iff_comap_le]
  change MeasurableSpace.comap coordinates
    (MeasurableSpace.comap RealCoordinates.code
      (inferInstance : MeasurableSpace (Nat × (Nat → ℝ)))) ≤ _
  rw [MeasurableSpace.comap_comp]
  exact (lengthMeasurable.prodMk (measurable_pi_lambda _ coordinateMeasurable)).comap_le

end RealCoordinates

theorem realCoordinates_length (expression : Expr) :
    expression.realCoordinates.length = expression.skeleton.realArity := by
  cases expression with
  | bvar | reject | unit | bool | real | nil | discrete =>
      simp [Expr.realCoordinates, Expr.skeleton, Expr.realArity]
  | lam body | fix body | fst body | snd body | inl body
  | inr body | neg body | poisson _ _ body | bernoulli _ _ body | exponential _ _ body =>
      simpa [Expr.realCoordinates, Expr.skeleton, Expr.realArity] using
        realCoordinates_length body
  | app left right | pair left right | cons left right | add left right
  | mul left right | div left right | lt left right | letE left right
  | uniform _ _ left right | gaussian _ _ left right | beta _ _ left right
  | gamma _ _ left right =>
      simpa [Expr.realCoordinates, Expr.skeleton, Expr.realArity] using congrArg₂ (· + ·)
          (realCoordinates_length left) (realCoordinates_length right)
  | matchSum first second third | matchList first second third
  | ite first second third =>
      simpa [Expr.realCoordinates, Expr.skeleton, Expr.realArity,
        Nat.add_assoc] using congrArg₂ (· + ·)
          (realCoordinates_length first)
          (congrArg₂ (· + ·) (realCoordinates_length second)
            (realCoordinates_length third))

/-- A family of expressions with a fixed real-free skeleton and
measurably varying real coordinates. -/
structure MeasurableFamily (α : Type*) [MeasurableSpace α] (expression : α → Expr) where
  skeleton : Skeleton
  skeleton_eq : ∀ parameter, (expression parameter).skeleton = skeleton
  coordinate_count : ∀ parameter,
    (expression parameter).realCoordinates.length = skeleton.realArity
  coordinate_measurable : ∀ index : Nat,
    Measurable fun parameter => (expression parameter).realCoordinates.getD index 0

namespace MeasurableFamily

def comp {α β : Type*} [MeasurableSpace α] [MeasurableSpace β]
    {expression : α → Expr} (family : MeasurableFamily α expression)
    (function : β → α) (measurableFunction : Measurable function) :
    MeasurableFamily β (expression ∘ function) where
  skeleton := family.skeleton
  skeleton_eq parameter := family.skeleton_eq (function parameter)
  coordinate_count parameter := family.coordinate_count (function parameter)
  coordinate_measurable index :=
    (family.coordinate_measurable index).comp measurableFunction

def of_eq {α : Type*} [MeasurableSpace α] {first second : α → Expr}
    (family : MeasurableFamily α first) (equality : first = second) :
    MeasurableFamily α second := equality ▸ family

/-- Extract a fixed-skeleton child whose coordinates occupy a contiguous block
of the parent's coordinate traversal. -/
def extractContiguous {α : Type*} [MeasurableSpace α]
    {parent child : α → Expr} (parentFamily : MeasurableFamily α parent)
    (childSkeleton : Skeleton) (offset : Nat)
    (child_skeleton : ∀ parameter, (child parameter).skeleton = childSkeleton)
    (coordinate_rule : ∀ parameter index, index < childSkeleton.realArity →
      (child parameter).realCoordinates.getD index 0 =
        (parent parameter).realCoordinates.getD (offset + index) 0) :
    MeasurableFamily α child where
  skeleton := childSkeleton
  skeleton_eq := child_skeleton
  coordinate_count parameter := by
    rw [realCoordinates_length, child_skeleton]
  coordinate_measurable index := by
    by_cases inBounds : index < childSkeleton.realArity
    · have equality :
          (fun parameter => (child parameter).realCoordinates.getD index 0) =
            fun parameter => (parent parameter).realCoordinates.getD (offset + index) 0 := by
        funext parameter
        exact coordinate_rule parameter index inBounds
      rw [equality]
      exact parentFamily.coordinate_measurable _
    · have equality :
          (fun parameter => (child parameter).realCoordinates.getD index 0) = fun _ => 0 := by
        funext parameter
        rw [List.getD_eq_getElem?_getD,
          List.getElem?_eq_none (by
            rw [realCoordinates_length, child_skeleton]
            exact Nat.le_of_not_gt inBounds)]
        rfl
      rw [equality]
      exact measurable_const

def congr {α : Type*} [MeasurableSpace α] {first second : α → Expr}
    (family : MeasurableFamily α first) (equal : first = second) :
    MeasurableFamily α second := equal ▸ family

theorem measurable_realCoordinates {α : Type*} [MeasurableSpace α]
    {expression : α → Expr} (family : MeasurableFamily α expression) :
    Measurable fun parameter => (⟨(expression parameter).realCoordinates⟩ : RealCoordinates) := by
  apply RealCoordinates.measurable_of_length_getD
  · have constantLength :
        (fun parameter => (expression parameter).realCoordinates.length) =
          fun _ => family.skeleton.realArity :=
      funext family.coordinate_count
    change Measurable fun parameter => (expression parameter).realCoordinates.length
    rw [constantLength]
    exact measurable_const
  · exact family.coordinate_measurable

theorem measurable {α : Type*} [MeasurableSpace α]
    {expression : α → Expr} (family : MeasurableFamily α expression) :
    Measurable expression := by
  rw [measurable_iff_comap_le]
  change MeasurableSpace.comap expression
    (MeasurableSpace.comap code (inferInstance : MeasurableSpace Code)) ≤
      (inferInstance : MeasurableSpace α)
  rw [MeasurableSpace.comap_comp]
  have codeMeasurable : Measurable fun parameter => code (expression parameter) := by
    simp only [code]
    apply Measurable.prod
    · convert (measurable_const : Measurable fun _ : α => family.skeleton) using 1
      funext parameter
      exact family.skeleton_eq parameter
    · exact family.measurable_realCoordinates
  exact codeMeasurable.comap_le

def constant {α : Type*} [MeasurableSpace α] (expression : Expr) :
    MeasurableFamily α (fun _ => expression) where
  skeleton := expression.skeleton
  skeleton_eq _ := rfl
  coordinate_count _ := realCoordinates_length expression
  coordinate_measurable _ := measurable_const

def realLiteral :
    MeasurableFamily ℝ (fun value => Expr.real value) where
  skeleton := .real
  skeleton_eq _ := by simp [Expr.skeleton]
  coordinate_count _ := by simp [Expr.realCoordinates, Expr.skeleton, Expr.realArity]
  coordinate_measurable index := by
    cases index with
    | zero =>
        have functionEq :
            (fun value : ℝ => (Expr.real value).realCoordinates.getD 0 0) = id := by
          funext value
          simp [Expr.realCoordinates, List.getD]
        rw [functionEq]
        exact measurable_id
    | succ index => simp [Expr.realCoordinates, List.getD]

/-- Combine two fixed-skeleton families through a constructor whose coordinate
traversal is concatenation. -/
def combine {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right)
    (constructor : Expr → Expr → Expr) (skeletonConstructor : Skeleton → Skeleton → Skeleton)
    (skeleton_rule : ∀ l r, (constructor l r).skeleton =
      skeletonConstructor l.skeleton r.skeleton)
    (coordinates_rule : ∀ l r, (constructor l r).realCoordinates =
      l.realCoordinates ++ r.realCoordinates)
    (arity_rule : ∀ l r, (skeletonConstructor l r).realArity =
      l.realArity + r.realArity) :
    MeasurableFamily α (fun parameter => constructor (left parameter) (right parameter)) where
  skeleton := skeletonConstructor leftFamily.skeleton rightFamily.skeleton
  skeleton_eq parameter := by
    rw [skeleton_rule, leftFamily.skeleton_eq, rightFamily.skeleton_eq]
  coordinate_count parameter := by
    rw [coordinates_rule, List.length_append, leftFamily.coordinate_count,
      rightFamily.coordinate_count, arity_rule]
  coordinate_measurable index := by
    by_cases beforeRight : index < leftFamily.skeleton.realArity
    · have functionEq :
          (fun parameter =>
            (constructor (left parameter) (right parameter)).realCoordinates.getD index 0) =
          (fun parameter => (left parameter).realCoordinates.getD index 0) := by
          funext parameter
          rw [coordinates_rule, List.getD_eq_getElem?_getD, List.getElem?_append,
            leftFamily.coordinate_count parameter, if_pos beforeRight,
            ← List.getD_eq_getElem?_getD]
      rw [functionEq]
      exact leftFamily.coordinate_measurable index
    · have afterLeft : leftFamily.skeleton.realArity ≤ index := Nat.le_of_not_gt beforeRight
      have functionEq :
          (fun parameter =>
            (constructor (left parameter) (right parameter)).realCoordinates.getD index 0) =
          (fun parameter =>
            (right parameter).realCoordinates.getD
              (index - leftFamily.skeleton.realArity) 0) := by
          funext parameter
          rw [coordinates_rule, List.getD_eq_getElem?_getD, List.getElem?_append,
            leftFamily.coordinate_count parameter, if_neg beforeRight,
            ← List.getD_eq_getElem?_getD]
      rw [functionEq]
      exact rightFamily.coordinate_measurable _

def app {α : Type*} [MeasurableSpace α] {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .app (left parameter) (right parameter)) :=
  combine leftFamily rightFamily .app .app
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def pair {α : Type*} [MeasurableSpace α] {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .pair (left parameter) (right parameter)) :=
  combine leftFamily rightFamily .pair .pair
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def cons {α : Type*} [MeasurableSpace α] {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .cons (left parameter) (right parameter)) :=
  combine leftFamily rightFamily .cons .cons
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def add {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .add (left parameter) (right parameter)) :=
  combine leftFamily rightFamily .add .add
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def mul {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .mul (left parameter) (right parameter)) :=
  combine leftFamily rightFamily .mul .mul
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def div {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .div (left parameter) (right parameter)) :=
  combine leftFamily rightFamily .div .div
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def lt {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .lt (left parameter) (right parameter)) :=
  combine leftFamily rightFamily Expr.lt Expr.lt
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

/-- Lift a unary constructor whose real-coordinate traversal is unchanged. -/
def wrap {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (bodyFamily : MeasurableFamily α body)
    (constructor : Expr → Expr) (skeletonConstructor : Skeleton → Skeleton)
    (skeleton_rule : ∀ expression, (constructor expression).skeleton =
      skeletonConstructor expression.skeleton)
    (coordinates_rule : ∀ expression, (constructor expression).realCoordinates =
      expression.realCoordinates)
    (arity_rule : ∀ skeleton,
      (skeletonConstructor skeleton).realArity = skeleton.realArity) :
    MeasurableFamily α (fun parameter => constructor (body parameter)) where
  skeleton := skeletonConstructor bodyFamily.skeleton
  skeleton_eq parameter := by rw [skeleton_rule, bodyFamily.skeleton_eq]
  coordinate_count parameter := by
    rw [coordinates_rule, bodyFamily.coordinate_count, arity_rule]
  coordinate_measurable index := by
    have functionEq :
        (fun parameter => (constructor (body parameter)).realCoordinates.getD index 0) =
        (fun parameter => (body parameter).realCoordinates.getD index 0) := by
      funext parameter
      rw [coordinates_rule]
    rw [functionEq]
    exact bodyFamily.coordinate_measurable index

def fst {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (family : MeasurableFamily α body) :
    MeasurableFamily α (fun parameter => .fst (body parameter)) :=
  wrap family .fst .fst
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def snd {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (family : MeasurableFamily α body) :
    MeasurableFamily α (fun parameter => .snd (body parameter)) :=
  wrap family .snd .snd
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def inl {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (family : MeasurableFamily α body) :
    MeasurableFamily α (fun parameter => .inl (body parameter)) :=
  wrap family .inl .inl
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def inr {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (family : MeasurableFamily α body) :
    MeasurableFamily α (fun parameter => .inr (body parameter)) :=
  wrap family .inr .inr
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def neg {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (family : MeasurableFamily α body) :
    MeasurableFamily α (fun parameter => .neg (body parameter)) :=
  wrap family .neg .neg
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

/-- Lift a one-hole context whose coordinate traversal has fixed coordinates
before and after the hole. -/
def surround {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (bodyFamily : MeasurableFamily α body)
    (constructor : Expr → Expr) (resultSkeleton : Skeleton)
    (skeleton_rule : ∀ parameter, (constructor (body parameter)).skeleton = resultSkeleton)
    (beforeCoordinates afterCoordinates : List ℝ)
    (coordinates_rule : ∀ parameter, (constructor (body parameter)).realCoordinates =
      beforeCoordinates ++ (body parameter).realCoordinates ++ afterCoordinates)
    (arity_rule : resultSkeleton.realArity =
      beforeCoordinates.length + bodyFamily.skeleton.realArity + afterCoordinates.length) :
    MeasurableFamily α (fun parameter => constructor (body parameter)) where
  skeleton := resultSkeleton
  skeleton_eq parameter := skeleton_rule parameter
  coordinate_count parameter := by
    rw [coordinates_rule parameter, List.length_append, List.length_append,
      bodyFamily.coordinate_count, arity_rule]
  coordinate_measurable index := by
    by_cases inPrefix : index < beforeCoordinates.length
    · have functionEq :
          (fun parameter => (constructor (body parameter)).realCoordinates.getD index 0) =
            fun _ => beforeCoordinates.getD index 0 := by
          funext parameter
          rw [coordinates_rule parameter, List.getD_eq_getElem?_getD, List.getElem?_append]
          have inCombined : index <
              (beforeCoordinates ++ (body parameter).realCoordinates).length := by
            simp only [List.length_append]
            omega
          rw [if_pos inCombined, List.getElem?_append, if_pos inPrefix,
            ← List.getD_eq_getElem?_getD]
      rw [functionEq]
      exact measurable_const
    · have afterPrefix : beforeCoordinates.length ≤ index := Nat.le_of_not_gt inPrefix
      by_cases inBody : index - beforeCoordinates.length < bodyFamily.skeleton.realArity
      · have functionEq :
            (fun parameter => (constructor (body parameter)).realCoordinates.getD index 0) =
              fun parameter => (body parameter).realCoordinates.getD
                (index - beforeCoordinates.length) 0 := by
            funext parameter
            rw [coordinates_rule parameter, List.getD_eq_getElem?_getD, List.getElem?_append]
            have inCombined : index <
                (beforeCoordinates ++ (body parameter).realCoordinates).length := by
              rw [List.length_append, bodyFamily.coordinate_count]
              omega
            rw [if_pos inCombined, List.getElem?_append, if_neg inPrefix,
              ← List.getD_eq_getElem?_getD]
        rw [functionEq]
        exact bodyFamily.coordinate_measurable _
      · have functionEq :
            (fun parameter => (constructor (body parameter)).realCoordinates.getD index 0) =
              fun _ => afterCoordinates.getD
                (index - beforeCoordinates.length - bodyFamily.skeleton.realArity) 0 := by
            funext parameter
            rw [coordinates_rule parameter, List.getD_eq_getElem?_getD, List.getElem?_append]
            have afterCombined : ¬ index <
                (beforeCoordinates ++ (body parameter).realCoordinates).length := by
              rw [List.length_append, bodyFamily.coordinate_count]
              omega
            rw [if_neg afterCombined, List.length_append, bodyFamily.coordinate_count,
              ← List.getD_eq_getElem?_getD]
            congr 1
            omega
        rw [functionEq]
        exact measurable_const

end MeasurableFamily

namespace Expr

/-- Structural child projections used on a fixed skeleton fiber.  Their
fallback is unreachable once the enclosing skeleton constructor is fixed. -/
def firstChild : Expr → Expr
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .neg body => body
  | .app left _ | .pair left _ | .cons left _ | .add left _
  | .mul left _ | .div left _ | .lt left _ => left
  | .uniform _ _ left _ | .gaussian _ _ left _ | .beta _ _ left _ | .gamma _ _ left _ => left
  | .poisson _ _ body | .bernoulli _ _ body | .exponential _ _ body => body
  | .matchSum first _ _ | .matchList first _ _ | .ite first _ _ => first
  | .letE first _ => first
  | expression => expression

def secondChild : Expr → Expr
  | .app _ right | .pair _ right | .cons _ right | .add _ right
  | .mul _ right | .div _ right | .lt _ right => right
  | .uniform _ _ _ right | .gaussian _ _ _ right | .beta _ _ _ right | .gamma _ _ _ right => right
  | .matchSum _ second _ | .matchList _ second _ | .ite _ second _ => second
  | .letE _ second => second
  | expression => expression

def thirdChild : Expr → Expr
  | .matchSum _ _ third | .matchList _ _ third | .ite _ _ third => third
  | expression => expression

end Expr

namespace Skeleton

def firstChild : Skeleton → Skeleton
  | .lam body | .fix body | .fst body | .snd body
  | .inl body | .inr body | .neg body => body
  | .app left _ | .pair left _ | .cons left _ | .add left _
  | .mul left _ | .div left _ | .lt left _ => left
  | .uniform _ _ left _ | .gaussian _ _ left _ | .beta _ _ left _ | .gamma _ _ left _ => left
  | .poisson _ _ body | .bernoulli _ _ body | .exponential _ _ body => body
  | .matchSum first _ _ | .matchList first _ _ | .ite first _ _ => first
  | .letE first _ => first
  | skeleton => skeleton

def secondChild : Skeleton → Skeleton
  | .app _ right | .pair _ right | .cons _ right | .add _ right
  | .mul _ right | .div _ right | .lt _ right => right
  | .uniform _ _ _ right | .gaussian _ _ _ right | .beta _ _ _ right | .gamma _ _ _ right => right
  | .matchSum _ second _ | .matchList _ second _ | .ite _ second _ => second
  | .letE _ second => second
  | skeleton => skeleton

def thirdChild : Skeleton → Skeleton
  | .matchSum _ _ third | .matchList _ _ third | .ite _ _ third => third
  | skeleton => skeleton

def secondOffset : Skeleton → Nat
  | .app first _ | .pair first _ | .cons first _ | .add first _
  | .mul first _ | .div first _ | .lt first _ => first.realArity
  | .uniform _ _ first _ | .gaussian _ _ first _ | .beta _ _ first _ | .gamma _ _ first _ =>
      first.realArity
  | .matchSum first _ _ | .matchList first _ _ | .ite first _ _ => first.realArity
  | .letE first _ => first.realArity
  | _ => 0

def thirdOffset : Skeleton → Nat
  | .matchSum first second _ | .matchList first second _
  | .ite first second _ => first.realArity + second.realArity
  | _ => 0

end Skeleton

theorem firstChild_skeleton (expression : Expr) :
    (Expr.firstChild expression).skeleton = Skeleton.firstChild expression.skeleton := by
  cases expression <;> simp [Expr.firstChild, Expr.skeleton, Skeleton.firstChild]

theorem secondChild_skeleton (expression : Expr) :
    (Expr.secondChild expression).skeleton = Skeleton.secondChild expression.skeleton := by
  cases expression <;> simp [Expr.secondChild, Expr.skeleton, Skeleton.secondChild]

theorem thirdChild_skeleton (expression : Expr) :
    (Expr.thirdChild expression).skeleton = Skeleton.thirdChild expression.skeleton := by
  cases expression <;> simp [Expr.thirdChild, Expr.skeleton, Skeleton.thirdChild]

theorem list_getD_append_left {α : Type*} (left right : List α) (index : Nat)
    (default : α) (inBounds : index < left.length) :
    (left ++ right).getD index default = left.getD index default := by
  rw [List.getD_eq_getElem?_getD, List.getElem?_append, if_pos inBounds,
    ← List.getD_eq_getElem?_getD]

theorem list_getD_append_right {α : Type*} (left right : List α) (index : Nat)
    (default : α) :
    (left ++ right).getD (left.length + index) default = right.getD index default := by
  rw [List.getD_eq_getElem?_getD, List.getElem?_append, if_neg (by omega),
    Nat.add_sub_cancel_left, ← List.getD_eq_getElem?_getD]

theorem list_getD_append_middle {α : Type*} (first second third : List α) (index : Nat)
    (default : α) (inBounds : index < second.length) :
    (first ++ second ++ third).getD (first.length + index) default =
      second.getD index default := by
  rw [List.append_assoc]
  rw [list_getD_append_right first (second ++ third),
    list_getD_append_left _ _ _ _ inBounds]

theorem list_getD_append_third {α : Type*} (first second third : List α) (index : Nat)
    (default : α) :
    (first ++ second ++ third).getD (first.length + second.length + index) default =
      third.getD index default := by
  rw [List.append_assoc]
  simpa [Nat.add_assoc] using
    (list_getD_append_right first (second ++ third) (second.length + index)
      default).trans (list_getD_append_right second third index default)

theorem firstChild_coordinates_decompose (expression : Expr) :
    ∃ suffix, expression.realCoordinates =
      (Expr.firstChild expression).realCoordinates ++ suffix := by
  cases expression <;>
    simp [Expr.firstChild, Expr.realCoordinates]

theorem secondChild_coordinates_decompose (expression : Expr) :
    ∃ front suffix, expression.realCoordinates =
        front ++ (Expr.secondChild expression).realCoordinates ++ suffix ∧
      front.length = Skeleton.secondOffset expression.skeleton := by
  cases expression <;>
    simp [Expr.secondChild, Expr.skeleton, Expr.realCoordinates, Skeleton.secondOffset]
  case app function argument | pair function argument | cons function argument |
      add function argument | mul function argument | div function argument |
      lt function argument | uniform _ _ function argument | gaussian _ _ function argument |
      beta _ _ function argument | gamma _ _ function argument =>
    exact ⟨function.realCoordinates, ⟨[], by simp⟩, realCoordinates_length function⟩
  case matchSum scrutinee left right | matchList scrutinee left right |
      ite scrutinee left right =>
    exact ⟨scrutinee.realCoordinates, ⟨right.realCoordinates, by simp⟩,
      realCoordinates_length scrutinee⟩
  case letE value body =>
    exact ⟨value.realCoordinates, ⟨[], by simp⟩, realCoordinates_length value⟩

theorem thirdChild_coordinates_decompose (expression : Expr) :
    ∃ front, expression.realCoordinates =
        front ++ (Expr.thirdChild expression).realCoordinates ∧
      front.length = Skeleton.thirdOffset expression.skeleton := by
  cases expression <;>
    simp [Expr.thirdChild, Expr.skeleton, Expr.realCoordinates, Skeleton.thirdOffset]
  case matchSum scrutinee left right | matchList scrutinee left right |
      ite scrutinee left right =>
    exact ⟨scrutinee.realCoordinates ++ left.realCoordinates, by simp,
      by rw [List.length_append, realCoordinates_length, realCoordinates_length]⟩

theorem firstChild_coordinate (expression : Expr) (index : Nat)
    (inBounds : index < (Expr.firstChild expression).skeleton.realArity) :
    (Expr.firstChild expression).realCoordinates.getD index 0 =
      expression.realCoordinates.getD index 0 := by
  rcases firstChild_coordinates_decompose expression with ⟨suffix, equality⟩
  rw [equality, list_getD_append_left]
  rw [realCoordinates_length]
  exact inBounds

theorem secondChild_coordinate (expression : Expr) (index : Nat)
    (inBounds : index < (Expr.secondChild expression).skeleton.realArity) :
    (Expr.secondChild expression).realCoordinates.getD index 0 =
      expression.realCoordinates.getD (Skeleton.secondOffset expression.skeleton + index) 0 := by
  rcases secondChild_coordinates_decompose expression with
    ⟨front, suffix, equality, frontLength⟩
  rw [equality, ← frontLength, list_getD_append_middle]
  rw [realCoordinates_length]
  exact inBounds

theorem thirdChild_coordinate (expression : Expr) (index : Nat)
    (inBounds : index < (Expr.thirdChild expression).skeleton.realArity) :
    (Expr.thirdChild expression).realCoordinates.getD index 0 =
      expression.realCoordinates.getD (Skeleton.thirdOffset expression.skeleton + index) 0 := by
  rcases thirdChild_coordinates_decompose expression with ⟨front, equality, frontLength⟩
  rw [equality, ← frontLength, list_getD_append_right]

namespace MeasurableFamily

def firstChild {α : Type*} [MeasurableSpace α] {parent : α → Expr}
    (family : MeasurableFamily α parent) :
    MeasurableFamily α (fun parameter => Expr.firstChild (parent parameter)) :=
  family.extractContiguous (Skeleton.firstChild family.skeleton) 0
    (fun parameter => by
      rw [firstChild_skeleton, family.skeleton_eq])
    (fun parameter index inBounds => by
      simp only [zero_add]
      apply firstChild_coordinate
      rwa [firstChild_skeleton, family.skeleton_eq])

def secondChild {α : Type*} [MeasurableSpace α] {parent : α → Expr}
    (family : MeasurableFamily α parent) :
    MeasurableFamily α (fun parameter => Expr.secondChild (parent parameter)) :=
  family.extractContiguous (Skeleton.secondChild family.skeleton)
    (Skeleton.secondOffset family.skeleton)
    (fun parameter => by
      rw [secondChild_skeleton, family.skeleton_eq])
    (fun parameter index inBounds => by
      rw [secondChild_coordinate]
      · rw [family.skeleton_eq]
      · rwa [secondChild_skeleton, family.skeleton_eq])

def thirdChild {α : Type*} [MeasurableSpace α] {parent : α → Expr}
    (family : MeasurableFamily α parent) :
    MeasurableFamily α (fun parameter => Expr.thirdChild (parent parameter)) :=
  family.extractContiguous (Skeleton.thirdChild family.skeleton)
    (Skeleton.thirdOffset family.skeleton)
    (fun parameter => by
      rw [thirdChild_skeleton, family.skeleton_eq])
    (fun parameter index inBounds => by
      rw [thirdChild_coordinate]
      · rw [family.skeleton_eq]
      · rwa [thirdChild_skeleton, family.skeleton_eq])

end MeasurableFamily


namespace MeasurableFamily

def matchSumScrutinee {α : Type*} [MeasurableSpace α] {scrutinee : α → Expr} (family : MeasurableFamily α scrutinee)
    (left right : Expr) :
    MeasurableFamily α (fun parameter => .matchSum (scrutinee parameter) left right) := by
  apply surround family (fun body => .matchSum body left right)
    (.matchSum family.skeleton left.skeleton right.skeleton)
    (by intro parameter; simp [Expr.skeleton, family.skeleton_eq parameter]) []
    (left.realCoordinates ++ right.realCoordinates)
  · intro parameter
    simp [Expr.realCoordinates, List.append_assoc]
  · simp only [Expr.realArity, List.length_nil, zero_add, List.length_append]
    rw [realCoordinates_length left, realCoordinates_length right]
    omega

def matchListScrutinee {α : Type*} [MeasurableSpace α] {scrutinee : α → Expr} (family : MeasurableFamily α scrutinee)
    (nilCase consCase : Expr) :
    MeasurableFamily α
      (fun parameter => .matchList (scrutinee parameter) nilCase consCase) := by
  apply surround family (fun body => .matchList body nilCase consCase)
    (.matchList family.skeleton nilCase.skeleton consCase.skeleton)
    (by intro parameter; simp [Expr.skeleton, family.skeleton_eq parameter]) []
    (nilCase.realCoordinates ++ consCase.realCoordinates)
  · intro parameter
    simp [Expr.realCoordinates, List.append_assoc]
  · simp only [Expr.realArity, List.length_nil, zero_add, List.length_append]
    rw [realCoordinates_length nilCase, realCoordinates_length consCase]
    omega

def iteCondition {α : Type*} [MeasurableSpace α] {condition : α → Expr} (family : MeasurableFamily α condition)
    (thenBranch elseBranch : Expr) :
    MeasurableFamily α
      (fun parameter => .ite (condition parameter) thenBranch elseBranch) := by
  apply surround family (fun body => .ite body thenBranch elseBranch)
    (.ite family.skeleton thenBranch.skeleton elseBranch.skeleton)
    (by intro parameter; simp [Expr.skeleton, family.skeleton_eq parameter]) []
    (thenBranch.realCoordinates ++ elseBranch.realCoordinates)
  · intro parameter
    simp [Expr.realCoordinates, List.append_assoc]
  · simp only [Expr.realArity, List.length_nil, zero_add, List.length_append]
    rw [realCoordinates_length thenBranch, realCoordinates_length elseBranch]
    omega

def letValue {α : Type*} [MeasurableSpace α] {value : α → Expr} (family : MeasurableFamily α value) (body : Expr) :
    MeasurableFamily α (fun parameter => .letE (value parameter) body) := by
  apply surround family (fun expression => .letE expression body)
    (.letE family.skeleton body.skeleton)
    (by intro parameter; simp [Expr.skeleton, family.skeleton_eq parameter]) []
    body.realCoordinates
  · intro parameter
    simp [Expr.realCoordinates]
  · simp only [Expr.realArity, List.length_nil, zero_add]
    rw [realCoordinates_length body]

end MeasurableFamily

theorem Action.wrap_eq_sample {context : Expr → Expr} {action : Action}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (equality : action.wrap context = .sample site fiber continuation) :
    ∃ inner, action = .sample site fiber inner ∧ continuation = context ∘ inner := by
  cases action with
  | next expression => simp [Action.wrap] at equality
  | stuck => simp [Action.wrap] at equality
  | sample actualSite actualFiber inner =>
      simp only [Action.wrap, Action.sample.injEq] at equality
      rcases equality with ⟨rfl, rfl, continuationEq⟩
      exact ⟨inner, rfl, continuationEq.symm⟩

theorem measurable_code : Measurable code := by
  rw [measurable_iff_comap_le]
  rfl

theorem measurable_skeleton : Measurable Expr.skeleton :=
  measurable_fst.comp measurable_code

theorem measurable_realCoordinates :
    Measurable fun expression : Expr =>
      (⟨expression.realCoordinates⟩ : RealCoordinates) :=
  measurable_snd.comp measurable_code

theorem measurable_realCoordinate (index : Nat) :
    Measurable fun expression : Expr => expression.realCoordinates.getD index 0 :=
  (RealCoordinates.measurable_getD index).comp measurable_realCoordinates

theorem measurable_realCoordinateAt {α : Type*} [MeasurableSpace α]
    (coordinates : α → RealCoordinates) (coordinatesMeasurable : Measurable coordinates)
    (index : α → Nat) (indexMeasurable : Measurable index) :
    Measurable fun parameter => (coordinates parameter).values.getD (index parameter) 0 := by
  have indexedGetD : Measurable fun pair : RealCoordinates × Nat =>
      pair.1.values.getD pair.2 0 :=
    measurable_from_prod_countable_left fun index =>
      RealCoordinates.measurable_getD index
  exact indexedGetD.comp (coordinatesMeasurable.prodMk indexMeasurable)

def realCoordinatesAppend (left right : RealCoordinates) : RealCoordinates :=
  ⟨left.values ++ right.values⟩

theorem realCoordinatesAppend_measurable :
    Measurable fun pair : RealCoordinates × RealCoordinates =>
      realCoordinatesAppend pair.1 pair.2 := by
  classical
  have leftLength : Measurable fun pair : RealCoordinates × RealCoordinates =>
      pair.1.values.length := RealCoordinates.measurable_length.comp measurable_fst
  have rightLength : Measurable fun pair : RealCoordinates × RealCoordinates =>
      pair.2.values.length := RealCoordinates.measurable_length.comp measurable_snd
  have sumMeasurable : Measurable fun pair : RealCoordinates × RealCoordinates =>
      pair.1.values.length + pair.2.values.length := leftLength.add rightLength
  have appendLengthMeasurable : Measurable fun pair : RealCoordinates × RealCoordinates =>
      (realCoordinatesAppend pair.1 pair.2).values.length := by
    simpa only [realCoordinatesAppend, List.length_append] using sumMeasurable
  have coordinatesMeasurable (index : Nat) :
      Measurable fun pair : RealCoordinates × RealCoordinates =>
        (realCoordinatesAppend pair.1 pair.2).values.getD index 0 := by
    let region : Set (RealCoordinates × RealCoordinates) :=
      {pair | index < pair.1.values.length}
    have regionMeasurable : MeasurableSet region := by
      exact leftLength measurableSet_Ioi
    have leftCoordinate : Measurable fun pair : RealCoordinates × RealCoordinates =>
        pair.1.values.getD index 0 :=
      (RealCoordinates.measurable_getD index).comp measurable_fst
    have rightCoordinate : Measurable fun pair : RealCoordinates × RealCoordinates =>
        pair.2.values.getD (index - pair.1.values.length) 0 :=
      measurable_realCoordinateAt
        (fun pair : RealCoordinates × RealCoordinates => pair.2) measurable_snd
        (fun pair => index - pair.1.values.length)
        (measurable_const.sub leftLength)
    have piecewiseMeasurable : Measurable (region.piecewise
        (fun pair => pair.1.values.getD index 0)
        (fun pair => pair.2.values.getD (index - pair.1.values.length) 0)) := by
      exact leftCoordinate.piecewise regionMeasurable rightCoordinate
    convert piecewiseMeasurable using 1
    funext pair
    change (pair.1.values ++ pair.2.values).getD index 0 = _
    rw [List.getD_eq_getElem?_getD, List.getElem?_append]
    by_cases member : index < pair.1.values.length
    · rw [if_pos member, ← List.getD_eq_getElem?_getD]
      simp [region, Set.piecewise, member]
    · rw [if_neg member, ← List.getD_eq_getElem?_getD]
      simp [region, Set.piecewise, member]
  exact RealCoordinates.measurable_of_length_getD appendLengthMeasurable coordinatesMeasurable

theorem measurable_expr_of_parts {α : Type*} [MeasurableSpace α]
    (expression : α → Expr)
    (skeletonMeasurable : Measurable fun parameter => (expression parameter).skeleton)
    (coordinatesMeasurable : Measurable fun parameter =>
      (⟨(expression parameter).realCoordinates⟩ : RealCoordinates)) :
    Measurable expression := by
  rw [measurable_iff_comap_le]
  change MeasurableSpace.comap expression
    (MeasurableSpace.comap code (inferInstance : MeasurableSpace Code)) ≤ _
  rw [MeasurableSpace.comap_comp]
  apply Measurable.comap_le
  exact Measurable.prod skeletonMeasurable coordinatesMeasurable

theorem measurable_binaryConstructor {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) (constructor : Expr → Expr → Expr)
    (skeletonConstructor : Skeleton → Skeleton → Skeleton)
    (skeleton_rule : ∀ l r, (constructor l r).skeleton =
      skeletonConstructor l.skeleton r.skeleton)
    (coordinates_rule : ∀ l r, (constructor l r).realCoordinates =
      l.realCoordinates ++ r.realCoordinates) :
    Measurable fun parameter => constructor (left parameter) (right parameter) := by
  apply measurable_expr_of_parts
  · have leftSkeleton := measurable_skeleton.comp leftMeasurable
    have rightSkeleton := measurable_skeleton.comp rightMeasurable
    have paired : Measurable fun parameter =>
        ((left parameter).skeleton, (right parameter).skeleton) :=
      Measurable.prod leftSkeleton rightSkeleton
    have operation : Measurable fun pair : Skeleton × Skeleton =>
        skeletonConstructor pair.1 pair.2 := measurable_of_countable _
    convert operation.comp paired using 1
    funext parameter
    simpa using skeleton_rule (left parameter) (right parameter)
  have leftCoordinates : Measurable fun parameter =>
      (⟨(left parameter).realCoordinates⟩ : RealCoordinates) :=
    measurable_realCoordinates.comp leftMeasurable
  have rightCoordinates : Measurable fun parameter =>
      (⟨(right parameter).realCoordinates⟩ : RealCoordinates) :=
    measurable_realCoordinates.comp rightMeasurable
  have paired : Measurable fun parameter =>
      ((⟨(left parameter).realCoordinates⟩ : RealCoordinates),
        (⟨(right parameter).realCoordinates⟩ : RealCoordinates)) :=
    Measurable.prod leftCoordinates rightCoordinates
  have appended := realCoordinatesAppend_measurable.comp paired
  convert appended using 1
  funext parameter
  simp [realCoordinatesAppend, coordinates_rule]

theorem measurable_unaryConstructor {α : Type*} [MeasurableSpace α]
    {body : α → Expr} (bodyMeasurable : Measurable body)
    (constructor : Expr → Expr) (skeletonConstructor : Skeleton → Skeleton)
    (skeleton_rule : ∀ expression, (constructor expression).skeleton =
      skeletonConstructor expression.skeleton)
    (coordinates_rule : ∀ expression, (constructor expression).realCoordinates =
      expression.realCoordinates) :
    Measurable fun parameter => constructor (body parameter) := by
  apply measurable_expr_of_parts
  · have operation : Measurable fun skeleton : Skeleton => skeletonConstructor skeleton :=
      measurable_of_countable _
    convert operation.comp (measurable_skeleton.comp bodyMeasurable) using 1
    funext parameter
    simpa using skeleton_rule (body parameter)
  · convert measurable_realCoordinates.comp bodyMeasurable using 1
    funext parameter
    simp [coordinates_rule]

theorem measurable_fstConstructor {α : Type*} [MeasurableSpace α] {body : α → Expr} (bodyMeasurable : Measurable body) :
    Measurable fun parameter => Expr.fst (body parameter) :=
  measurable_unaryConstructor bodyMeasurable .fst .fst
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_sndConstructor {α : Type*} [MeasurableSpace α] {body : α → Expr} (bodyMeasurable : Measurable body) :
    Measurable fun parameter => Expr.snd (body parameter) :=
  measurable_unaryConstructor bodyMeasurable .snd .snd
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_inl {α : Type*} [MeasurableSpace α] {body : α → Expr} (bodyMeasurable : Measurable body) :
    Measurable fun parameter => Expr.inl (body parameter) :=
  measurable_unaryConstructor bodyMeasurable .inl .inl
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_inr {α : Type*} [MeasurableSpace α] {body : α → Expr} (bodyMeasurable : Measurable body) :
    Measurable fun parameter => Expr.inr (body parameter) :=
  measurable_unaryConstructor bodyMeasurable .inr .inr
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_neg {α : Type*} [MeasurableSpace α]
    {body : α → Expr} (bodyMeasurable : Measurable body) :
    Measurable fun parameter => Expr.neg (body parameter) :=
  measurable_unaryConstructor bodyMeasurable .neg .neg
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_realLiteral {α : Type*} [MeasurableSpace α]
    {value : α → ℝ} (valueMeasurable : Measurable value) :
    Measurable fun parameter => Expr.real (value parameter) := by
  apply measurable_expr_of_parts
  · simpa [Expr.skeleton] using
      (measurable_const : Measurable fun _ : α => Skeleton.real)
  · have singletonCoordinates : Measurable fun parameter : α =>
        (⟨[value parameter]⟩ : RealCoordinates) := by
      apply RealCoordinates.measurable_of_length_getD
      · exact measurable_const
      · intro index
        cases index with
        | zero => simpa [List.getD] using valueMeasurable
        | succ _ =>
            simpa [List.getD] using (measurable_const : Measurable fun _ : α => (0 : ℝ))
    simpa [Expr.realCoordinates] using singletonCoordinates

theorem measurable_app {α : Type*} [MeasurableSpace α] {function argument : α → Expr} (functionMeasurable : Measurable function)
    (argumentMeasurable : Measurable argument) :
    Measurable fun parameter => Expr.app (function parameter) (argument parameter) :=
  measurable_binaryConstructor functionMeasurable argumentMeasurable .app .app
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_pair {α : Type*} [MeasurableSpace α] {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.pair (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable .pair .pair
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_cons {α : Type*} [MeasurableSpace α] {head tail : α → Expr} (headMeasurable : Measurable head)
    (tailMeasurable : Measurable tail) :
    Measurable fun parameter => Expr.cons (head parameter) (tail parameter) :=
  measurable_binaryConstructor headMeasurable tailMeasurable .cons .cons
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_add {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.add (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable .add .add
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_mul {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.mul (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable .mul .mul
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_div {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.div (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable .div .div
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_lt {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.lt (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable Expr.lt Expr.lt
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem terminalFloatSet_eq :
    terminalFloatSet = Expr.skeleton ⁻¹' {Skeleton.real} := by
  ext expression
  cases expression <;>
    simp [terminalFloatSet, Expr.skeleton]

theorem terminalFloatSet_measurable : MeasurableSet terminalFloatSet := by
  rw [terminalFloatSet_eq]
  have singletonMeasurable : MeasurableSet ({Skeleton.real} : Set Skeleton) := by
    change True
    trivial
  exact measurable_skeleton singletonMeasurable

theorem terminalFloatValue_measurable : Measurable terminalFloatValue := by
  classical
  have piecewiseEq : terminalFloatValue =
      terminalFloatSet.piecewise
        (fun expression => expression.realCoordinates.getD 0 0) (fun _ => 0) := by
    funext expression
    cases expression <;>
      simp [Set.piecewise, terminalFloatValue, terminalFloatSet,
        Expr.realCoordinates, List.getD]
  rw [piecewiseEq]
  exact (measurable_realCoordinate 0).piecewise terminalFloatSet_measurable measurable_const

def SkeletonFiber (skeleton : Skeleton) : Set Expr :=
  {expression | expression.skeleton = skeleton}

theorem skeletonFiber_measurable (skeleton : Skeleton) :
    MeasurableSet (SkeletonFiber skeleton) := by
  have singletonMeasurable : MeasurableSet ({skeleton} : Set Skeleton) := by
    change True
    trivial
  have preimageMeasurable := measurable_skeleton singletonMeasurable
  convert preimageMeasurable using 1
  ext expression
  simp [SkeletonFiber]

def MeasurableFamily.skeletonFiber (skeleton : Skeleton) :
    MeasurableFamily (SkeletonFiber skeleton) Subtype.val where
  skeleton := skeleton
  skeleton_eq expression := expression.property
  coordinate_count expression := by
    rw [realCoordinates_length expression]
    exact congrArg Expr.realArity expression.property
  coordinate_measurable index :=
    (measurable_realCoordinate index).comp measurable_subtype_coe

theorem skeletonFiber_cover :
    ⋃ skeleton, SkeletonFiber skeleton = Set.univ := by
  ext expression
  simp [SkeletonFiber]

def skeletonShift (amount cutoff : Nat) : Skeleton → Skeleton
  | .bvar index => .bvar (if cutoff ≤ index then index + amount else index)
  | .unit => .unit
  | .reject => .reject
  | .discrete mode kind d => .discrete mode kind d
  | .bool value => .bool value
  | .real => .real
  | .lam body => .lam (skeletonShift amount (cutoff + 1) body)
  | .fix body => .fix (skeletonShift amount (cutoff + 2) body)
  | .app function argument =>
      .app (skeletonShift amount cutoff function) (skeletonShift amount cutoff argument)
  | .pair left right =>
      .pair (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .fst pair => .fst (skeletonShift amount cutoff pair)
  | .snd pair => .snd (skeletonShift amount cutoff pair)
  | .inl value => .inl (skeletonShift amount cutoff value)
  | .inr value => .inr (skeletonShift amount cutoff value)
  | .matchSum scrutinee left right =>
      .matchSum (skeletonShift amount cutoff scrutinee)
        (skeletonShift amount (cutoff + 1) left) (skeletonShift amount (cutoff + 1) right)
  | .nil => .nil
  | .cons head tail =>
      .cons (skeletonShift amount cutoff head) (skeletonShift amount cutoff tail)
  | .matchList scrutinee nilCase consCase =>
      .matchList (skeletonShift amount cutoff scrutinee)
        (skeletonShift amount cutoff nilCase) (skeletonShift amount (cutoff + 2) consCase)
  | .ite condition thenBranch elseBranch =>
      .ite (skeletonShift amount cutoff condition) (skeletonShift amount cutoff thenBranch)
        (skeletonShift amount cutoff elseBranch)
  | .letE value body =>
      .letE (skeletonShift amount cutoff value)
        (skeletonShift amount (cutoff + 1) body)
  | .neg body => .neg (skeletonShift amount cutoff body)
  | .add left right =>
      .add (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .mul left right =>
      .mul (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .div left right =>
      .div (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .lt left right => .lt (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .uniform mode kind left right =>
      .uniform mode kind (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .gaussian mode kind left right =>
      .gaussian mode kind (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .poisson mode kind body => .poisson mode kind (skeletonShift amount cutoff body)
  | .bernoulli mode kind body => .bernoulli mode kind (skeletonShift amount cutoff body)
  | .exponential mode kind body => .exponential mode kind (skeletonShift amount cutoff body)
  | .beta mode kind left right =>
      .beta mode kind (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .gamma mode kind left right =>
      .gamma mode kind (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)

theorem shift_skeleton (amount cutoff : Nat) (expression : Expr) :
    (expression.shift amount cutoff).skeleton = skeletonShift amount cutoff expression.skeleton := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression cutoff with
  | h size ih =>
      cases expression with
      | uniform _ _ left right | gaussian _ _ left right | beta _ _ left right
      | gamma _ _ left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | poisson _ _ body | bernoulli _ _ body | exponential _ _ body =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | pair left right | app left right | cons left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | add left right | mul left right | div left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | lt left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | matchSum scrutinee left right | ite scrutinee left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf scrutinee) (by rw [← sizeEq]; simp_wf <;> omega)
              cutoff scrutinee rfl]
          first
          | rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega)
                (cutoff + 1) left rfl,
              ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega)
                (cutoff + 1) right rfl]
          | rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
              ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | matchList scrutinee nilCase consCase =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf scrutinee) (by rw [← sizeEq]; simp_wf <;> omega)
              cutoff scrutinee rfl,
            ih (sizeOf nilCase) (by rw [← sizeEq]; simp_wf <;> omega)
              cutoff nilCase rfl,
            ih (sizeOf consCase) (by rw [← sizeEq]; simp_wf <;> omega)
              (cutoff + 2) consCase rfl]
      | letE value body =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf value) (by rw [← sizeEq]; simp_wf <;> omega) cutoff value rfl,
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              (cutoff + 1) body rfl]
      | lam body | fst body | snd body =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          first
          | rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
                (cutoff + 1) body rfl]
          | rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | fix body =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
            (cutoff + 2) body rfl]
      | inl body | inr body =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | neg body =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | bvar index | reject | unit | bool flag | real index | nil | discrete _ _ _ =>
          simp [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]

theorem shift_realCoordinates (amount cutoff : Nat) (expression : Expr) :
    (expression.shift amount cutoff).realCoordinates = expression.realCoordinates := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression cutoff with
  | h size ih =>
      cases expression with
      | uniform _ _ left right | gaussian _ _ left right | beta _ _ left right
      | gamma _ _ left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | poisson _ _ body | bernoulli _ _ body | exponential _ _ body =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | pair left right | app left right | cons left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | add left right | mul left right | div left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | lt left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | matchSum scrutinee left right | ite scrutinee left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf scrutinee) (by rw [← sizeEq]; simp_wf <;> omega)
              cutoff scrutinee rfl]
          first
          | rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega)
                (cutoff + 1) left rfl,
              ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega)
                (cutoff + 1) right rfl]
          | rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
              ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | matchList scrutinee nilCase consCase =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf scrutinee) (by rw [← sizeEq]; simp_wf <;> omega)
              cutoff scrutinee rfl,
            ih (sizeOf nilCase) (by rw [← sizeEq]; simp_wf <;> omega)
              cutoff nilCase rfl,
            ih (sizeOf consCase) (by rw [← sizeEq]; simp_wf <;> omega)
              (cutoff + 2) consCase rfl]
      | letE value body =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf value) (by rw [← sizeEq]; simp_wf <;> omega) cutoff value rfl,
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              (cutoff + 1) body rfl]
      | lam body | fst body | snd body =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          first
          | rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
                (cutoff + 1) body rfl]
          | rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | fix body =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
            (cutoff + 2) body rfl]
      | inl body | inr body =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | neg body =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | bvar index | reject | unit | bool flag | real index | nil | discrete _ _ _ =>
          simp [Expr.shift, Expr.mapVars, Expr.realCoordinates]

def MeasurableFamily.shift {α : Type*} [MeasurableSpace α]
    {expression : α → Expr} (family : MeasurableFamily α expression)
    (amount cutoff : Nat) :
    MeasurableFamily α (fun parameter => (expression parameter).shift amount cutoff) where
  skeleton := skeletonShift amount cutoff family.skeleton
  skeleton_eq parameter := by rw [shift_skeleton, family.skeleton_eq]
  coordinate_count parameter := by
    rw [← family.skeleton_eq parameter, ← shift_skeleton]
    exact realCoordinates_length _
  coordinate_measurable index := by
    have equality :
        (fun parameter => ((expression parameter).shift amount cutoff).realCoordinates.getD index 0) =
          fun parameter => (expression parameter).realCoordinates.getD index 0 := by
      funext parameter
      rw [shift_realCoordinates]
    rw [equality]
    exact family.coordinate_measurable index

def skeletonSubstAt (depth : Nat) (replacement : Skeleton) : Skeleton → Skeleton
  | .bvar index => if index = depth then skeletonShift depth 0 replacement
      else .bvar (if depth < index then index - 1 else index)
  | .unit => .unit
  | .reject => .reject
  | .discrete mode kind d => .discrete mode kind d
  | .bool value => .bool value
  | .real => .real
  | .lam body => .lam (skeletonSubstAt (depth + 1) replacement body)
  | .fix body =>
      .fix (skeletonSubstAt (depth + 2) replacement body)
  | .app function argument => .app
      (skeletonSubstAt depth replacement function)
      (skeletonSubstAt depth replacement argument)
  | .pair left right => .pair
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .fst pair => .fst (skeletonSubstAt depth replacement pair)
  | .snd pair => .snd (skeletonSubstAt depth replacement pair)
  | .inl value => .inl (skeletonSubstAt depth replacement value)
  | .inr value => .inr (skeletonSubstAt depth replacement value)
  | .matchSum scrutinee left right => .matchSum
      (skeletonSubstAt depth replacement scrutinee)
      (skeletonSubstAt (depth + 1) replacement left)
      (skeletonSubstAt (depth + 1) replacement right)
  | .nil => .nil
  | .cons head tail => .cons
      (skeletonSubstAt depth replacement head) (skeletonSubstAt depth replacement tail)
  | .matchList scrutinee nilCase consCase => .matchList
      (skeletonSubstAt depth replacement scrutinee)
      (skeletonSubstAt depth replacement nilCase)
      (skeletonSubstAt (depth + 2) replacement consCase)
  | .ite condition thenBranch elseBranch => .ite
      (skeletonSubstAt depth replacement condition)
      (skeletonSubstAt depth replacement thenBranch)
      (skeletonSubstAt depth replacement elseBranch)
  | .letE value body => .letE
      (skeletonSubstAt depth replacement value)
      (skeletonSubstAt (depth + 1) replacement body)
  | .neg body => .neg (skeletonSubstAt depth replacement body)
  | .add left right => .add
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .mul left right => .mul
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .div left right => .div
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .lt left right => .lt
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .uniform mode kind left right =>
      .uniform mode kind (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .gaussian mode kind left right =>
      .gaussian mode kind (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .poisson mode kind body => .poisson mode kind (skeletonSubstAt depth replacement body)
  | .bernoulli mode kind body => .bernoulli mode kind (skeletonSubstAt depth replacement body)
  | .exponential mode kind body => .exponential mode kind (skeletonSubstAt depth replacement body)
  | .beta mode kind left right =>
      .beta mode kind (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .gamma mode kind left right =>
      .gamma mode kind (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)

theorem substAt_skeleton (depth : Nat) (replacement expression : Expr) :
    (Expr.substAt depth replacement expression).skeleton =
      skeletonSubstAt depth replacement.skeleton expression.skeleton := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression depth with
  | h size ih =>
      cases expression with
      | uniform _ _ left right | gaussian _ _ left right | beta _ _ left right
      | gamma _ _ left right =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) depth left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) depth right rfl]
      | poisson _ _ body | bernoulli _ _ body | exponential _ _ body =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) depth body rfl]
      | pair left right | app left right | cons left right =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) depth left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) depth right rfl]
      | add left right | mul left right | div left right =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) depth left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) depth right rfl]
      | lt left right =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) depth left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) depth right rfl]
      | matchSum scrutinee left right | ite scrutinee left right =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf scrutinee) (by rw [← sizeEq]; simp_wf <;> omega)
              depth scrutinee rfl]
          first
          | rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega)
                (depth + 1) left rfl,
              ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega)
                (depth + 1) right rfl]
          | rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) depth left rfl,
              ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) depth right rfl]
      | matchList scrutinee nilCase consCase =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf scrutinee) (by rw [← sizeEq]; simp_wf <;> omega)
              depth scrutinee rfl,
            ih (sizeOf nilCase) (by rw [← sizeEq]; simp_wf <;> omega)
              depth nilCase rfl,
            ih (sizeOf consCase) (by rw [← sizeEq]; simp_wf <;> omega)
              (depth + 2) consCase rfl]
      | letE value body =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf value) (by rw [← sizeEq]; simp_wf <;> omega) depth value rfl,
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              (depth + 1) body rfl]
      | lam body | fst body | snd body =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          first
          | rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
                (depth + 1) body rfl]
          | rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) depth body rfl]
      | fix body =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
            (depth + 2) body rfl]
      | inl body | inr body =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) depth body rfl]
      | neg body =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) depth body rfl]
      | bvar index =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          split
          · exact shift_skeleton depth 0 replacement
          · simp only [Expr.skeleton]
      | reject | unit | bool flag | real index | nil | discrete _ _ _ =>
          simp [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]

inductive CoordinateSelector where
  | body (index : Nat)
  | replacement (index : Nat)

def coordinatePlan (depth : Nat) (replacement : Skeleton) (bodyOffset : Nat) :
    Skeleton → List CoordinateSelector
  | .bvar index =>
      if index = depth then
        (List.range replacement.realArity).map CoordinateSelector.replacement
      else []
  | .real => [.body bodyOffset]
  | .lam body => coordinatePlan (depth + 1) replacement bodyOffset body
  | .fix body => coordinatePlan (depth + 2) replacement bodyOffset body
  | .fst body | .snd body | .inl body | .inr body
  | .neg body => coordinatePlan depth replacement bodyOffset body
  | .app left right | .pair left right | .cons left right
  | .add left right | .mul left right | .div left right | .lt left right =>
      coordinatePlan depth replacement bodyOffset left ++
        coordinatePlan depth replacement (bodyOffset + left.realArity) right
  | .matchSum scrutinee left right =>
      coordinatePlan depth replacement bodyOffset scrutinee ++
        coordinatePlan (depth + 1) replacement
          (bodyOffset + scrutinee.realArity) left ++
        coordinatePlan (depth + 1) replacement
          (bodyOffset + scrutinee.realArity + left.realArity) right
  | .matchList scrutinee nilCase consCase =>
      coordinatePlan depth replacement bodyOffset scrutinee ++
        coordinatePlan depth replacement
          (bodyOffset + scrutinee.realArity) nilCase ++
        coordinatePlan (depth + 2) replacement
          (bodyOffset + scrutinee.realArity + nilCase.realArity) consCase
  | .ite condition thenBranch elseBranch =>
      coordinatePlan depth replacement bodyOffset condition ++
        coordinatePlan depth replacement
          (bodyOffset + condition.realArity) thenBranch ++
        coordinatePlan depth replacement
          (bodyOffset + condition.realArity + thenBranch.realArity) elseBranch
  | .letE value body =>
      coordinatePlan depth replacement bodyOffset value ++
        coordinatePlan (depth + 1) replacement
          (bodyOffset + value.realArity) body
  | .uniform _ _ left right | .gaussian _ _ left right | .beta _ _ left right
  | .gamma _ _ left right =>
      coordinatePlan depth replacement bodyOffset left ++
        coordinatePlan depth replacement (bodyOffset + left.realArity) right
  | .poisson _ _ body | .bernoulli _ _ body | .exponential _ _ body => coordinatePlan depth replacement bodyOffset body
  | _ => []

def CoordinateSelector.eval (body replacement : List ℝ) : CoordinateSelector → ℝ
  | .body index => body.getD index 0
  | .replacement index => replacement.getD index 0

def applyCoordinatePlan (plan : List CoordinateSelector)
    (body replacement : List ℝ) : List ℝ :=
  plan.map (CoordinateSelector.eval body replacement)

theorem map_range_getD (values : List ℝ) :
    (List.range values.length).map (fun index => values.getD index 0) = values := by
  apply List.ext_getElem
  · simp
  · intro index leftBounds rightBounds
    simp [List.getD, rightBounds]

theorem applyCoordinatePlan_replacementRange (body replacement : List ℝ) :
    applyCoordinatePlan
        ((List.range replacement.length).map CoordinateSelector.replacement)
        body replacement = replacement := by
  unfold applyCoordinatePlan
  rw [List.map_map]
  change (List.range replacement.length).map
    (fun index => replacement.getD index 0) = replacement
  exact map_range_getD replacement

@[simp] theorem applyCoordinatePlan_append (left right : List CoordinateSelector)
    (body replacement : List ℝ) :
    applyCoordinatePlan (left ++ right) body replacement =
      applyCoordinatePlan left body replacement ++
        applyCoordinatePlan right body replacement := by
  simp [applyCoordinatePlan]

theorem applyCoordinatePlan_bodyAtPrefix (before suffix : List ℝ) (value : ℝ)
    (replacement : List ℝ) :
    applyCoordinatePlan [.body before.length] (before ++ value :: suffix) replacement =
      [value] := by
  simp [applyCoordinatePlan, CoordinateSelector.eval, List.getD,
    List.getElem?_append]

theorem applyCoordinatePlan_coordinatePlan (depth : Nat) (replacement expression : Expr)
    (before suffix : List ℝ) :
    applyCoordinatePlan
        (coordinatePlan depth replacement.skeleton before.length expression.skeleton)
        (before ++ expression.realCoordinates ++ suffix) replacement.realCoordinates =
      (Expr.substAt depth replacement expression).realCoordinates := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression depth before suffix with
  | h size ih =>
      cases expression with
      | bvar index =>
          simp only [coordinatePlan, Expr.substAt, Expr.mapVars, Expr.skeleton, Expr.realCoordinates]
          split
          · rw [← realCoordinates_length replacement,
              applyCoordinatePlan_replacementRange]
            exact shift_realCoordinates depth 0 replacement |>.symm
          · simp [applyCoordinatePlan, Expr.realCoordinates]
      | real value =>
          simpa [coordinatePlan, Expr.substAt, Expr.mapVars, Expr.skeleton, Expr.realCoordinates] using
            applyCoordinatePlan_bodyAtPrefix before suffix value replacement.realCoordinates
      | pair left right | app left right | cons left right =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have leftResult := ih (sizeOf left)
            (by rw [← sizeEq]; simp_wf <;> omega) depth left before
              (right.realCoordinates ++ suffix) rfl
          have rightResult := ih (sizeOf right)
            (by rw [← sizeEq]; simp_wf <;> omega) depth right
              (before ++ left.realCoordinates) suffix rfl
          simp only [List.append_assoc] at leftResult rightResult ⊢
          rw [leftResult]
          have offsetEquality : before.length + left.skeleton.realArity =
              (before ++ left.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length left]
          rw [offsetEquality, rightResult]
      | add left right | mul left right | div left right =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have leftResult := ih (sizeOf left)
            (by rw [← sizeEq]; simp_wf <;> omega) depth left before
              (right.realCoordinates ++ suffix) rfl
          have rightResult := ih (sizeOf right)
            (by rw [← sizeEq]; simp_wf <;> omega) depth right
              (before ++ left.realCoordinates) suffix rfl
          simp only [List.append_assoc] at leftResult rightResult ⊢
          rw [leftResult]
          have offsetEquality : before.length + left.skeleton.realArity =
              (before ++ left.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length left]
          rw [offsetEquality, rightResult]
      | lt left right =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have leftResult := ih (sizeOf left)
            (by rw [← sizeEq]; simp_wf <;> omega) depth left before
              (right.realCoordinates ++ suffix) rfl
          have rightResult := ih (sizeOf right)
            (by rw [← sizeEq]; simp_wf <;> omega) depth right
              (before ++ left.realCoordinates) suffix rfl
          simp only [List.append_assoc] at leftResult rightResult ⊢
          rw [leftResult]
          have offsetEquality : before.length + left.skeleton.realArity =
              (before ++ left.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length left]
          rw [offsetEquality, rightResult]
      | matchSum scrutinee left right =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have scrutineeResult := ih (sizeOf scrutinee)
            (by rw [← sizeEq]; simp_wf <;> omega) depth scrutinee before
              (left.realCoordinates ++ right.realCoordinates ++ suffix) rfl
          have leftResult := ih (sizeOf left)
            (by rw [← sizeEq]; simp_wf <;> omega) (depth + 1) left
              (before ++ scrutinee.realCoordinates)
              (right.realCoordinates ++ suffix) rfl
          have rightResult := ih (sizeOf right)
            (by rw [← sizeEq]; simp_wf <;> omega) (depth + 1) right
              (before ++ scrutinee.realCoordinates ++ left.realCoordinates) suffix rfl
          simp only [List.append_assoc] at scrutineeResult leftResult rightResult ⊢
          rw [scrutineeResult]
          have firstOffset : before.length + scrutinee.skeleton.realArity =
              (before ++ scrutinee.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length scrutinee]
          rw [firstOffset]
          rw [leftResult]
          have secondOffset : (before ++ scrutinee.realCoordinates).length +
                left.skeleton.realArity =
              (before ++ (scrutinee.realCoordinates ++ left.realCoordinates)).length := by
            simp only [List.length_append]
            rw [realCoordinates_length left]
            omega
          rw [secondOffset, rightResult]
      | matchList scrutinee nilCase consCase =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have scrutineeResult := ih (sizeOf scrutinee)
            (by rw [← sizeEq]; simp_wf <;> omega) depth scrutinee before
              (nilCase.realCoordinates ++ consCase.realCoordinates ++ suffix) rfl
          have nilResult := ih (sizeOf nilCase)
            (by rw [← sizeEq]; simp_wf <;> omega) depth nilCase
              (before ++ scrutinee.realCoordinates)
              (consCase.realCoordinates ++ suffix) rfl
          have consResult := ih (sizeOf consCase)
            (by rw [← sizeEq]; simp_wf <;> omega) (depth + 2) consCase
              (before ++ scrutinee.realCoordinates ++ nilCase.realCoordinates) suffix rfl
          simp only [List.append_assoc] at scrutineeResult nilResult consResult ⊢
          rw [scrutineeResult]
          have firstOffset : before.length + scrutinee.skeleton.realArity =
              (before ++ scrutinee.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length scrutinee]
          rw [firstOffset]
          rw [nilResult]
          have secondOffset : (before ++ scrutinee.realCoordinates).length +
                nilCase.skeleton.realArity =
              (before ++ (scrutinee.realCoordinates ++ nilCase.realCoordinates)).length := by
            simp only [List.length_append]
            rw [realCoordinates_length nilCase]
            omega
          rw [secondOffset, consResult]
      | ite condition thenBranch elseBranch =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have conditionResult := ih (sizeOf condition)
            (by rw [← sizeEq]; simp_wf <;> omega) depth condition before
              (thenBranch.realCoordinates ++ elseBranch.realCoordinates ++ suffix) rfl
          have thenResult := ih (sizeOf thenBranch)
            (by rw [← sizeEq]; simp_wf <;> omega) depth thenBranch
              (before ++ condition.realCoordinates)
              (elseBranch.realCoordinates ++ suffix) rfl
          have elseResult := ih (sizeOf elseBranch)
            (by rw [← sizeEq]; simp_wf <;> omega) depth elseBranch
              (before ++ condition.realCoordinates ++ thenBranch.realCoordinates) suffix rfl
          simp only [List.append_assoc] at conditionResult thenResult elseResult ⊢
          rw [conditionResult]
          have firstOffset : before.length + condition.skeleton.realArity =
              (before ++ condition.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length condition]
          rw [firstOffset]
          rw [thenResult]
          have secondOffset : (before ++ condition.realCoordinates).length +
                thenBranch.skeleton.realArity =
              (before ++ (condition.realCoordinates ++ thenBranch.realCoordinates)).length := by
            simp only [List.length_append]
            rw [realCoordinates_length thenBranch]
            omega
          rw [secondOffset, elseResult]
      | letE value body =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have valueResult := ih (sizeOf value)
            (by rw [← sizeEq]; simp_wf <;> omega) depth value before
              (body.realCoordinates ++ suffix) rfl
          have bodyResult := ih (sizeOf body)
            (by rw [← sizeEq]; simp_wf <;> omega) (depth + 1) body
              (before ++ value.realCoordinates) suffix rfl
          simp only [List.append_assoc] at valueResult bodyResult ⊢
          rw [valueResult]
          have offsetEquality : before.length + value.skeleton.realArity =
              (before ++ value.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length value]
          rw [offsetEquality, bodyResult]
      | lam body =>
          simpa only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars] using
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              (depth + 1) body before suffix rfl
      | fix body =>
          simpa only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars] using
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              (depth + 2) body before suffix rfl
      | fst body | snd body | inl body | inr body
      | neg body =>
          simpa only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars] using
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              depth body before suffix rfl
      | uniform _ _ left right | gaussian _ _ left right | beta _ _ left right
      | gamma _ _ left right =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have leftResult := ih (sizeOf left)
            (by rw [← sizeEq]; simp_wf <;> omega) depth left before
              (right.realCoordinates ++ suffix) rfl
          have rightResult := ih (sizeOf right)
            (by rw [← sizeEq]; simp_wf <;> omega) depth right
              (before ++ left.realCoordinates) suffix rfl
          simp only [List.append_assoc] at leftResult rightResult ⊢
          rw [leftResult]
          have offsetEquality : before.length + left.skeleton.realArity =
              (before ++ left.realCoordinates).length := by
            rw [List.length_append, realCoordinates_length left]
          rw [offsetEquality, rightResult]
      | poisson _ _ body | bernoulli _ _ body | exponential _ _ body =>
          simpa only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars] using
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              depth body before suffix rfl
      | reject | unit | bool flag | nil | discrete _ _ _ =>
          simp [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan]

theorem substAt_realCoordinates (depth : Nat) (replacement expression : Expr) :
    applyCoordinatePlan
        (coordinatePlan depth replacement.skeleton 0 expression.skeleton)
        expression.realCoordinates replacement.realCoordinates =
      (Expr.substAt depth replacement expression).realCoordinates := by
  simpa using
    applyCoordinatePlan_coordinatePlan depth replacement expression [] []

theorem coordinatePlan_length (depth : Nat) (replacement expression : Expr) :
    (coordinatePlan depth replacement.skeleton 0 expression.skeleton).length =
      (skeletonSubstAt depth replacement.skeleton expression.skeleton).realArity := by
  calc
    (coordinatePlan depth replacement.skeleton 0 expression.skeleton).length =
        (Expr.substAt depth replacement expression).realCoordinates.length := by
      simpa only [applyCoordinatePlan, List.length_map] using
        congrArg List.length (substAt_realCoordinates depth replacement expression)
    _ = (Expr.substAt depth replacement expression).skeleton.realArity :=
      realCoordinates_length _
    _ = (skeletonSubstAt depth replacement.skeleton expression.skeleton).realArity :=
      congrArg Expr.realArity (substAt_skeleton depth replacement expression)

theorem applyCoordinatePlan_getD_of_lt (plan : List CoordinateSelector)
    (body replacement : List ℝ) (index : Nat) (inBounds : index < plan.length) :
    (applyCoordinatePlan plan body replacement).getD index 0 =
      (plan[index]).eval body replacement := by
  simp [applyCoordinatePlan, List.getD, inBounds]

def MeasurableFamily.substAt {α : Type*} [MeasurableSpace α]
    {body replacement : α → Expr} (bodyFamily : MeasurableFamily α body)
    (depth : Nat) (replacementFamily : MeasurableFamily α replacement) :
    MeasurableFamily α
      (fun parameter => Expr.substAt depth (replacement parameter) (body parameter)) where
  skeleton := skeletonSubstAt depth replacementFamily.skeleton bodyFamily.skeleton
  skeleton_eq parameter := by
    rw [substAt_skeleton, replacementFamily.skeleton_eq, bodyFamily.skeleton_eq]
  coordinate_count parameter := by
    rw [realCoordinates_length, substAt_skeleton,
      replacementFamily.skeleton_eq, bodyFamily.skeleton_eq]
  coordinate_measurable index := by
    let plan := coordinatePlan depth replacementFamily.skeleton 0 bodyFamily.skeleton
    have coordinateEquality (parameter : α) :
        (Expr.substAt depth (replacement parameter) (body parameter)).realCoordinates =
          applyCoordinatePlan plan (body parameter).realCoordinates
            (replacement parameter).realCoordinates := by
      rw [← substAt_realCoordinates]
      simp only [plan, replacementFamily.skeleton_eq, bodyFamily.skeleton_eq]
    by_cases inBounds : index < plan.length
    · let selector := plan[index]
      have functionEquality :
          (fun parameter =>
              (Expr.substAt depth (replacement parameter) (body parameter)).realCoordinates.getD
                index 0) =
            fun parameter => selector.eval (body parameter).realCoordinates
              (replacement parameter).realCoordinates := by
        funext parameter
        rw [coordinateEquality parameter]
        exact applyCoordinatePlan_getD_of_lt _ _ _ index inBounds
      rw [functionEquality]
      cases selector with
      | body bodyIndex => exact bodyFamily.coordinate_measurable bodyIndex
      | replacement replacementIndex =>
          exact replacementFamily.coordinate_measurable replacementIndex
    · have functionEquality :
          (fun parameter =>
              (Expr.substAt depth (replacement parameter) (body parameter)).realCoordinates.getD
                index 0) = fun _ => 0 := by
        funext parameter
        rw [coordinateEquality parameter]
        simp [applyCoordinatePlan, List.getD, inBounds]
      rw [functionEquality]
      exact measurable_const

def MeasurableFamily.substHead {α : Type*} [MeasurableSpace α]
    {body replacement : α → Expr} (bodyFamily : MeasurableFamily α body)
    (replacementFamily : MeasurableFamily α replacement) :
    MeasurableFamily α
      (fun parameter => Expr.substHead (body parameter) (replacement parameter)) := by
  simpa only [Expr.substHead] using bodyFamily.substAt 0 replacementFamily

def MeasurableFamily.substTwo {α : Type*} [MeasurableSpace α]
    {body argument function : α → Expr} (bodyFamily : MeasurableFamily α body)
    (argumentFamily : MeasurableFamily α argument)
    (functionFamily : MeasurableFamily α function) :
    MeasurableFamily α
      (fun parameter => Expr.substTwo (body parameter) (argument parameter)
        (function parameter)) := by
  simpa only [Expr.substTwo] using
    (bodyFamily.substAt 1 functionFamily).substAt 0 argumentFamily

theorem isValue_eq_skeletonIsValue : ∀ expression : Expr,
    expression.isValue = Expr.isValue expression.skeleton
  | .bvar _ | .reject | .discrete _ _ _ | .unit | .bool _ | .real _ | .lam _ | .fix _
  | .app _ _ | .fst _ | .snd _ | .matchSum _ _ _ | .nil
  | .matchList _ _ _ | .ite _ _ _ | .letE _ _
  | .neg _ | .add _ _ | .mul _ _ | .div _ _ | .lt _ _ => by
      simp [Expr.isValue, Expr.skeleton, Expr.isValue]
  | .uniform _ _ _ _ | .gaussian _ _ _ _ | .poisson _ _ _ | .bernoulli _ _ _ | .exponential _ _ _
  | .beta _ _ _ _ | .gamma _ _ _ _ => by simp [Expr.isValue, Expr.skeleton, Expr.isValue]
  | .pair left right | .cons left right => by
      simp only [Expr.isValue, Expr.skeleton, Expr.isValue]
      rw [isValue_eq_skeletonIsValue left, isValue_eq_skeletonIsValue right]
  | .inl value | .inr value => by
      simp only [Expr.isValue, Expr.skeleton, Expr.isValue]
      exact isValue_eq_skeletonIsValue value

theorem MeasurableFamily.isValue_eq {α : Type*} [MeasurableSpace α]
    {expression : α → Expr} (family : MeasurableFamily α expression) (parameter : α) :
    (expression parameter).isValue = Expr.isValue family.skeleton := by
  rw [isValue_eq_skeletonIsValue, family.skeleton_eq]

def paramsFromCoordinates (op : Determinize.Spec.Paper.Op)
    (affine general : List ℝ) : Determinize.Spec.Paper.Params op :=
  (fun index => affine.getD index.1 0, fun index => general.getD index.1 0)

def atomicParams (op : Op) (affine general : List ℝ) : Determinize.Spec.Paper.Params op :=
  paramsFromCoordinates op affine general

theorem paramsFromCoordinates_eq_getElem
    (op : Determinize.Spec.Paper.Op) (affine general : List ℝ)
    (affineArity : affine.length =
      Determinize.Spec.Paper.affineArity op)
    (generalArity : general.length =
      Determinize.Spec.Paper.generalArity op) :
    paramsFromCoordinates op affine general =
      (fun index => affine[index.1]'(by simpa [affineArity] using index.2),
        fun index => general[index.1]'(by simpa [generalArity] using index.2)) := by
  apply Prod.ext
  · funext index
    simp [paramsFromCoordinates, List.getD,
      show index.1 < affine.length by simpa [affineArity] using index.2]
  · funext index
    simp [paramsFromCoordinates, List.getD,
      show index.1 < general.length by simpa [generalArity] using index.2]

theorem measurable_meanValue (op : Determinize.Spec.Paper.Op) :
    Measurable (Determinize.Spec.Paper.meanValue op) := by
  cases op <;> unfold Determinize.Spec.Paper.meanValue <;> fun_prop

/-- The kernel of a primitive site in its evaluated parameters: the primitive's law at a
stochastic site, the Dirac mass at its mean on the parameter domain at a mean site. -/
noncomputable def primitiveKernelPack
    (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    (kind : Kind) → (op : Op) → SFiniteKernel (Determinize.Spec.Paper.Params op) ℝ
  | .stochastic, op => ⟨laws.kernel op, laws.kernel_sfinite op⟩
  | .mean, op => SFiniteKernel.piecewise
      (Determinize.Proof.Paper.measurableSet_domain op)
      (SFiniteKernel.deterministic
        (Determinize.Spec.Paper.meanValue op) (measurable_meanValue op))
      SFiniteKernel.zero

theorem primitiveFiber_eq_atomic
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (op : Op)
    (affine general : List ℝ)
    (affineArity : affine.length =
      Determinize.Spec.Paper.affineArity op)
    (generalArity : general.length =
      Determinize.Spec.Paper.generalArity op) :
    primitiveFiber kind op affine general =
      (primitiveKernelPack laws kind op).kernel (atomicParams op affine general) := by
  classical
  cases kind with
  | stochastic =>
      simp only [atomicParams, primitiveKernelPack]
      unfold primitiveFiber
        Determinize.Spec.Paper.parseParams
      simp only
      rw [dif_pos affineArity, dif_pos generalArity]
      rw [laws.kernel_eq_paperMeasure]
      apply congrArg (Determinize.Spec.Paper.paperMeasure op)
      exact (paramsFromCoordinates_eq_getElem op affine general
        affineArity generalArity).symm
  | mean =>
      simp only [atomicParams, primitiveKernelPack]
      unfold primitiveFiber
        Determinize.Spec.Paper.parseParams
      simp only
      rw [dif_pos affineArity, dif_pos generalArity]
      let actualParams : Determinize.Spec.Paper.Params op :=
        (fun index => affine[index.1]'(by simpa [affineArity] using index.2),
          fun index => general[index.1]'(by simpa [generalArity] using index.2))
      have actualParamsEquality : actualParams =
          paramsFromCoordinates op affine general := by
        dsimp only [actualParams]
        exact (paramsFromCoordinates_eq_getElem op affine general
          affineArity generalArity).symm
      change (if Determinize.Spec.Paper.domain op actualParams then
          Measure.dirac (Determinize.Spec.Paper.meanValue op actualParams)
        else 0) =
          (SFiniteKernel.piecewise
            (Determinize.Proof.Paper.measurableSet_domain op)
            (SFiniteKernel.deterministic
              (Determinize.Spec.Paper.meanValue op)
              (measurable_meanValue op))
            SFiniteKernel.zero).kernel (paramsFromCoordinates op affine general)
      unfold SFiniteKernel.piecewise SFiniteKernel.deterministic SFiniteKernel.zero
      rw [Kernel.piecewise_apply]
      simp only [Set.mem_ofPred_eq]
      by_cases paramsDomain : Determinize.Spec.Paper.domain op
          (paramsFromCoordinates op affine general)
      · have actualDomain : Determinize.Spec.Paper.domain op actualParams :=
          actualParamsEquality.symm ▸ paramsDomain
        rw [if_pos paramsDomain, if_pos actualDomain, Kernel.deterministic_apply]
        exact congrArg Measure.dirac
          (congrArg (Determinize.Spec.Paper.meanValue op)
            actualParamsEquality)
      · have actualOutside : ¬ Determinize.Spec.Paper.domain op actualParams :=
          fun actualDomain => paramsDomain (actualParamsEquality ▸ actualDomain)
        rw [if_neg paramsDomain, if_neg actualOutside]
        simp


universe u

/-- A measurable finite-piece description of a parameter-dependent reduction
action.  Sample continuations are jointly measurable in the original parameter and
the freshly drawn real. -/
inductive MeasurableActionFamily (α : Type u) [MeasurableSpace α] :
    (α → Action) → Type (u + 1)
  | next {successor : α → Expr} (measurable : Measurable successor) :
      MeasurableActionFamily α (fun parameter => .next (successor parameter))
  | sample {site : Mode × Kind × Op} (draw : SFiniteKernel α ℝ)
      {continuation : α × ℝ → Expr}
      (measurable : Measurable continuation) :
      MeasurableActionFamily α (fun parameter =>
        .sample site (draw.kernel parameter) (fun value => continuation (parameter, value)))
  | stuck : MeasurableActionFamily α (fun _ => .stuck)
  | piecewise {region : Set α} [DecidablePred (· ∈ region)]
      (measurableRegion : MeasurableSet region)
      {whenTrue whenFalse : α → Action}
      (trueFamily : MeasurableActionFamily α whenTrue)
      (falseFamily : MeasurableActionFamily α whenFalse) :
      MeasurableActionFamily α (region.piecewise whenTrue whenFalse)

namespace MeasurableActionFamily

theorem pullback_apply {α β γ : Type*} [MeasurableSpace α] [MeasurableSpace β]
    [MeasurableSpace γ]
    (draw : SFiniteKernel β γ)
    (function : α → β) (measurableFunction : Measurable function) (parameter : α) :
    (SFiniteKernel.pullback draw function
      measurableFunction).kernel parameter = draw.kernel (function parameter) := by
  unfold SFiniteKernel.pullback
  rw [Kernel.comp_apply]
  simp only [Kernel.deterministic_apply]
  exact Measure.dirac_bind draw.kernel.measurable _

def congr {α : Type*} [MeasurableSpace α] {first second : α → Action}
    (family : MeasurableActionFamily α first) (equal : first = second) :
    MeasurableActionFamily α second := equal ▸ family

def comp {α β : Type*} [MeasurableSpace α] [MeasurableSpace β]
    {action : α → Action} (family : MeasurableActionFamily α action)
    (function : β → α) (measurableFunction : Measurable function) :
    MeasurableActionFamily β (action ∘ function) := by
  induction family with
  | next successorMeasurable =>
      exact .next (successorMeasurable.comp measurableFunction)
  | sample draw continuationMeasurable =>
      let pulled : SFiniteKernel β ℝ :=
        SFiniteKernel.pullback draw function measurableFunction
      have firstMeasurable : Measurable (fun pair : β × ℝ => pair.1) := measurable_fst
      have secondMeasurable : Measurable (fun pair : β × ℝ => pair.2) := measurable_snd
      have pairMeasurable : Measurable (fun pair : β × ℝ => (function pair.1, pair.2)) :=
        Measurable.prod (measurableFunction.comp firstMeasurable) secondMeasurable
      apply congr (.sample pulled <| continuationMeasurable.comp pairMeasurable)
      funext parameter
      rw [pullback_apply]
      rfl
  | stuck => exact .stuck
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily
      trueResult falseResult =>
      classical
      apply congr (.piecewise (measurableRegion.preimage measurableFunction)
        trueResult falseResult)
      funext parameter
      by_cases member : function parameter ∈ region <;>
        simp [Set.piecewise, member]

/-- Map every successor and sample continuation through a jointly measurable
parameter-dependent one-hole context. -/
def map {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) (context : α → Expr → Expr)
    (nextMeasurable : ∀ {body : α → Expr}, Measurable body →
      Measurable fun input => context input (body input))
    (sampleMeasurable : ∀ {body : α × ℝ → Expr}, Measurable body →
      Measurable fun input => context input.1 (body input)) :
    MeasurableActionFamily α (fun parameter => (action parameter).wrap (context parameter)) := by
  induction family with
  | next successorIsMeasurable =>
      exact .next (nextMeasurable successorIsMeasurable)
  | sample draw continuationIsMeasurable =>
      exact .sample draw (sampleMeasurable continuationIsMeasurable)
  | stuck => exact .stuck
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily
      trueResult falseResult =>
      classical
      apply congr (.piecewise measurableRegion trueResult falseResult)
      funext parameter
      by_cases member : parameter ∈ region <;>
        simp [Set.piecewise, member]

theorem measurable_getD_pair (index : Nat) :
    Measurable fun pair : ℝ × ℝ => [pair.1, pair.2].getD index 0 := by
  rcases index with _ | _ | index <;> simp [List.getD] <;> fun_prop

theorem measurable_getD_first (index : Nat) :
    Measurable fun pair : ℝ × ℝ => [pair.1].getD index 0 := by
  rcases index with _ | index <;> simp [List.getD] <;> fun_prop

theorem measurable_getD_second (index : Nat) :
    Measurable fun pair : ℝ × ℝ => [pair.2].getD index 0 := by
  rcases index with _ | index <;> simp [List.getD] <;> fun_prop

theorem measurable_getD_single (index : Nat) :
    Measurable fun value : ℝ => [value].getD index 0 := by
  rcases index with _ | index <;> simp [List.getD] <;> fun_prop

theorem measurable_getD_nil {α : Type*} [MeasurableSpace α] (index : Nat) :
    Measurable fun _ : α => ([] : List ℝ).getD index 0 := by
  simp [List.getD]

/-- The kernel of `uniform` in its evaluated operands. -/
noncomputable def uniformDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) :
    SFiniteKernel (ℝ × ℝ) ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind .uniform)
    (fun p => paramsFromCoordinates .uniform [p.1, p.2] [])
    (Measurable.prod (measurable_pi_lambda _ fun index => measurable_getD_pair index.1)
      (measurable_pi_lambda _ fun index => measurable_getD_nil index.1))

theorem uniformDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (lower upper : ℝ) :
    (uniformDraw laws kind).kernel (lower, upper) = uniformFiber kind lower upper := by
  rw [uniformDraw, pullback_apply, uniformFiber_eq,
    primitiveFiber_eq_atomic laws kind .uniform [lower, upper] [] rfl rfl]
  rfl

/-- The kernel of `gaussian` in its evaluated operands. -/
noncomputable def gaussianDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) :
    SFiniteKernel (ℝ × ℝ) ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind .gaussian)
    (fun p => paramsFromCoordinates .gaussian [p.1] [p.2])
    (Measurable.prod (measurable_pi_lambda _ fun index => measurable_getD_first index.1)
      (measurable_pi_lambda _ fun index => measurable_getD_second index.1))

theorem gaussianDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (mean variance : ℝ) :
    (gaussianDraw laws kind).kernel (mean, variance) = gaussianFiber kind mean variance := by
  rw [gaussianDraw, pullback_apply, gaussianFiber_eq,
    primitiveFiber_eq_atomic laws kind .gaussian [mean] [variance] rfl rfl]
  rfl

/-- The kernel of `beta` in its evaluated operands. -/
noncomputable def betaDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) :
    SFiniteKernel (ℝ × ℝ) ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind .beta)
    (fun p => paramsFromCoordinates .beta [] [p.1, p.2])
    (Measurable.prod (measurable_pi_lambda _ fun index => measurable_getD_nil index.1)
      (measurable_pi_lambda _ fun index => measurable_getD_pair index.1))

theorem betaDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (alpha beta : ℝ) :
    (betaDraw laws kind).kernel (alpha, beta) = betaFiber kind alpha beta := by
  rw [betaDraw, pullback_apply, betaFiber_eq,
    primitiveFiber_eq_atomic laws kind .beta [] [alpha, beta] rfl rfl]
  rfl

/-- The kernel of `gamma` in its evaluated operands. -/
noncomputable def gammaDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) :
    SFiniteKernel (ℝ × ℝ) ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind .gamma)
    (fun p => paramsFromCoordinates .gamma [p.1] [p.2])
    (Measurable.prod (measurable_pi_lambda _ fun index => measurable_getD_first index.1)
      (measurable_pi_lambda _ fun index => measurable_getD_second index.1))

theorem gammaDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (shape rate : ℝ) :
    (gammaDraw laws kind).kernel (shape, rate) = gammaFiber kind shape rate := by
  rw [gammaDraw, pullback_apply, gammaFiber_eq,
    primitiveFiber_eq_atomic laws kind .gamma [shape] [rate] rfl rfl]
  rfl

/-- The kernel of `poisson` in its evaluated operands. -/
noncomputable def poissonDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) :
    SFiniteKernel ℝ ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind .poisson)
    (fun p => paramsFromCoordinates .poisson [p] [])
    (Measurable.prod (measurable_pi_lambda _ fun index => measurable_getD_single index.1)
      (measurable_pi_lambda _ fun index => measurable_getD_nil index.1))

noncomputable def discreteDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (d : FiniteDistribution) : SFiniteKernel Unit ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind (.discrete d))
    (fun _ => paramsFromCoordinates (.discrete d) [] []) measurable_const

theorem discreteDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (d : FiniteDistribution) :
    (discreteDraw laws kind d).kernel () = discreteFiber kind d := by
  rw [discreteDraw, pullback_apply, discreteFiber_eq,
    primitiveFiber_eq_atomic laws kind (.discrete d) [] [] rfl rfl]
  rfl

noncomputable def bernoulliDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) :
    SFiniteKernel ℝ ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind .bernoulli)
    (fun p => paramsFromCoordinates .bernoulli [p] [])
    (Measurable.prod (measurable_pi_lambda _ fun index => measurable_getD_single index.1)
      (measurable_pi_lambda _ fun index => measurable_getD_nil index.1))

theorem poissonDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (rate : ℝ) :
    (poissonDraw laws kind).kernel rate = poissonFiber kind rate := by
  rw [poissonDraw, pullback_apply, poissonFiber_eq,
    primitiveFiber_eq_atomic laws kind .poisson [rate] [] rfl rfl]
  rfl

theorem bernoulliDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (probability : ℝ) :
    (bernoulliDraw laws kind).kernel probability = bernoulliFiber kind probability := by
  rw [bernoulliDraw, pullback_apply, bernoulliFiber_eq,
    primitiveFiber_eq_atomic laws kind .bernoulli [probability] [] rfl rfl]
  rfl

/-- The kernel of `exponential` in its evaluated operands. -/
noncomputable def exponentialDraw (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) :
    SFiniteKernel ℝ ℝ :=
  SFiniteKernel.pullback (primitiveKernelPack laws kind .exponential)
    (fun p => paramsFromCoordinates .exponential [] [p])
    (Measurable.prod (measurable_pi_lambda _ fun index => measurable_getD_nil index.1)
      (measurable_pi_lambda _ fun index => measurable_getD_single index.1))

theorem exponentialDraw_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (rate : ℝ) :
    (exponentialDraw laws kind).kernel rate = exponentialFiber kind rate := by
  rw [exponentialDraw, pullback_apply, exponentialFiber_eq,
    primitiveFiber_eq_atomic laws kind .exponential [] [rate] rfl rfl]
  rfl

def wrapUnary {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action)
    (constructor : Expr → Expr) (skeletonConstructor : Skeleton → Skeleton)
    (skeleton_rule : ∀ expression, (constructor expression).skeleton =
      skeletonConstructor expression.skeleton)
    (coordinates_rule : ∀ expression, (constructor expression).realCoordinates =
      expression.realCoordinates) :
    MeasurableActionFamily α
      (fun parameter => (action parameter).wrap constructor) := by
  apply family.map (fun _ => constructor)
  · intro body bodyMeasurable
    exact measurable_unaryConstructor bodyMeasurable constructor skeletonConstructor
      skeleton_rule coordinates_rule
  · intro body bodyMeasurable
    exact measurable_unaryConstructor bodyMeasurable constructor skeletonConstructor
      skeleton_rule coordinates_rule

def wrapBinaryLeft {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) (right : α → Expr)
    (rightMeasurable : Measurable right) (constructor : Expr → Expr → Expr)
    (skeletonConstructor : Skeleton → Skeleton → Skeleton)
    (skeleton_rule : ∀ left right, (constructor left right).skeleton =
      skeletonConstructor left.skeleton right.skeleton)
    (coordinates_rule : ∀ left right, (constructor left right).realCoordinates =
      left.realCoordinates ++ right.realCoordinates) :
    MeasurableActionFamily α
      (fun parameter => (action parameter).wrap
        (fun next => constructor next (right parameter))) := by
  apply family.map (fun parameter next => constructor next (right parameter))
  · intro body bodyMeasurable
    exact measurable_binaryConstructor bodyMeasurable rightMeasurable constructor
      skeletonConstructor skeleton_rule coordinates_rule
  · intro body bodyMeasurable
    exact measurable_binaryConstructor bodyMeasurable
      (rightMeasurable.comp measurable_fst) constructor skeletonConstructor
      skeleton_rule coordinates_rule

def wrapBinaryRight {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) (left : α → Expr)
    (leftMeasurable : Measurable left) (constructor : Expr → Expr → Expr)
    (skeletonConstructor : Skeleton → Skeleton → Skeleton)
    (skeleton_rule : ∀ left right, (constructor left right).skeleton =
      skeletonConstructor left.skeleton right.skeleton)
    (coordinates_rule : ∀ left right, (constructor left right).realCoordinates =
      left.realCoordinates ++ right.realCoordinates) :
    MeasurableActionFamily α
      (fun parameter => (action parameter).wrap
        (fun next => constructor (left parameter) next)) := by
  apply family.map (fun parameter next => constructor (left parameter) next)
  · intro body bodyMeasurable
    exact measurable_binaryConstructor leftMeasurable bodyMeasurable constructor
      skeletonConstructor skeleton_rule coordinates_rule
  · intro body bodyMeasurable
    exact measurable_binaryConstructor (leftMeasurable.comp measurable_fst)
      bodyMeasurable constructor skeletonConstructor skeleton_rule coordinates_rule

theorem measurableTernaryConstructor {α : Type*} [MeasurableSpace α]
    {first second third : α → Expr} (firstMeasurable : Measurable first)
    (secondMeasurable : Measurable second) (thirdMeasurable : Measurable third)
    (constructor : Expr → Expr → Expr → Expr)
    (skeletonConstructor : Skeleton → Skeleton → Skeleton → Skeleton)
    (skeleton_rule : ∀ first second third,
      (constructor first second third).skeleton =
        skeletonConstructor first.skeleton second.skeleton third.skeleton)
    (coordinates_rule : ∀ first second third,
      (constructor first second third).realCoordinates =
        first.realCoordinates ++ second.realCoordinates ++ third.realCoordinates) :
    Measurable fun parameter =>
      constructor (first parameter) (second parameter) (third parameter) := by
  apply measurable_expr_of_parts
  · have inputs : Measurable fun parameter =>
        ((first parameter).skeleton, (second parameter).skeleton,
          (third parameter).skeleton) :=
      Measurable.prod (measurable_skeleton.comp firstMeasurable)
        (Measurable.prod (measurable_skeleton.comp secondMeasurable)
          (measurable_skeleton.comp thirdMeasurable))
    have operation : Measurable fun input : Skeleton × Skeleton × Skeleton =>
        skeletonConstructor input.1 input.2.1 input.2.2 := measurable_of_countable _
    convert operation.comp inputs using 1
    funext parameter
    exact skeleton_rule _ _ _
  · have firstCoordinates : Measurable fun parameter =>
        (⟨(first parameter).realCoordinates⟩ : RealCoordinates) :=
      measurable_realCoordinates.comp firstMeasurable
    have secondCoordinates : Measurable fun parameter =>
        (⟨(second parameter).realCoordinates⟩ : RealCoordinates) :=
      measurable_realCoordinates.comp secondMeasurable
    have thirdCoordinates : Measurable fun parameter =>
        (⟨(third parameter).realCoordinates⟩ : RealCoordinates) :=
      measurable_realCoordinates.comp thirdMeasurable
    have firstPair : Measurable fun parameter =>
        ((⟨(first parameter).realCoordinates⟩ : RealCoordinates),
          (⟨(second parameter).realCoordinates⟩ : RealCoordinates)) :=
      Measurable.prod firstCoordinates secondCoordinates
    have firstTwo : Measurable fun parameter =>
        realCoordinatesAppend
          (⟨(first parameter).realCoordinates⟩ : RealCoordinates)
          (⟨(second parameter).realCoordinates⟩ : RealCoordinates) :=
      realCoordinatesAppend_measurable.comp firstPair
    have allPair : Measurable fun parameter =>
        (realCoordinatesAppend
            (⟨(first parameter).realCoordinates⟩ : RealCoordinates)
            (⟨(second parameter).realCoordinates⟩ : RealCoordinates),
          (⟨(third parameter).realCoordinates⟩ : RealCoordinates)) :=
      Measurable.prod firstTwo thirdCoordinates
    have allThree := realCoordinatesAppend_measurable.comp allPair
    convert allThree using 1
    funext parameter
    simp [realCoordinatesAppend, coordinates_rule]

def wrapTernaryFirst {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) (second third : α → Expr)
    (secondMeasurable : Measurable second) (thirdMeasurable : Measurable third)
    (constructor : Expr → Expr → Expr → Expr)
    (skeletonConstructor : Skeleton → Skeleton → Skeleton → Skeleton)
    (skeleton_rule : ∀ first second third,
      (constructor first second third).skeleton =
        skeletonConstructor first.skeleton second.skeleton third.skeleton)
    (coordinates_rule : ∀ first second third,
      (constructor first second third).realCoordinates =
        first.realCoordinates ++ second.realCoordinates ++ third.realCoordinates) :
    MeasurableActionFamily α (fun parameter => (action parameter).wrap
      (fun next => constructor next (second parameter) (third parameter))) := by
  apply family.map
    (fun parameter next => constructor next (second parameter) (third parameter))
  · intro body bodyMeasurable
    exact measurableTernaryConstructor bodyMeasurable secondMeasurable thirdMeasurable
      constructor skeletonConstructor skeleton_rule coordinates_rule
  · intro body bodyMeasurable
    exact measurableTernaryConstructor bodyMeasurable
      (secondMeasurable.comp measurable_fst) (thirdMeasurable.comp measurable_fst)
      constructor skeletonConstructor skeleton_rule coordinates_rule

def nextFamily {α : Type*} [MeasurableSpace α] {expression : α → Expr}
    (family : MeasurableFamily α expression) :
    MeasurableActionFamily α (fun parameter => .next (expression parameter)) :=
  .next family.measurable

noncomputable def reducePair {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.pair (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · apply congr (nextFamily (MeasurableFamily.pair leftFamily rightFamily))
      funext parameter
      simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, leftFamily.skeleton_eq parameter,
        rightFamily.skeleton_eq parameter, leftValue, rightValue]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable .pair .pair
          (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
      funext parameter
      simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, leftFamily.skeleton_eq parameter,
        rightFamily.skeleton_eq parameter, leftValue, rightValue]
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable .pair .pair
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, leftFamily.skeleton_eq parameter, leftValue]

noncomputable def reduceCons {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {head tail : α → Expr}
    (headFamily : MeasurableFamily α head) (tailFamily : MeasurableFamily α tail)
    (headReduce : MeasurableActionFamily α (fun parameter => reduce (head parameter)))
    (tailReduce : MeasurableActionFamily α (fun parameter => reduce (tail parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.cons (head parameter) (tail parameter))) := by
  classical
  by_cases headValue : Expr.isValue headFamily.skeleton = true
  · by_cases tailValue : Expr.isValue tailFamily.skeleton = true
    · apply congr (nextFamily (MeasurableFamily.cons headFamily tailFamily))
      funext parameter
      simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, headFamily.skeleton_eq parameter,
        tailFamily.skeleton_eq parameter, headValue, tailValue]
    · apply congr (tailReduce.wrapBinaryRight head headFamily.measurable .cons .cons
          (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
      funext parameter
      simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, headFamily.skeleton_eq parameter,
        tailFamily.skeleton_eq parameter, headValue, tailValue]
  · apply congr (headReduce.wrapBinaryLeft tail tailFamily.measurable .cons .cons
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, headFamily.skeleton_eq parameter, headValue]

noncomputable def reduceInl {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {value : α → Expr}
    (valueFamily : MeasurableFamily α value)
    (valueReduce : MeasurableActionFamily α (fun parameter => reduce (value parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.inl (value parameter))) := by
  classical
  by_cases isValue : Expr.isValue valueFamily.skeleton = true
  · apply congr (nextFamily (MeasurableFamily.inl valueFamily))
    funext parameter
    simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]
  · apply congr (valueReduce.wrapUnary .inl .inl
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]

noncomputable def reduceInr {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {value : α → Expr}
    (valueFamily : MeasurableFamily α value)
    (valueReduce : MeasurableActionFamily α (fun parameter => reduce (value parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.inr (value parameter))) := by
  classical
  by_cases isValue : Expr.isValue valueFamily.skeleton = true
  · apply congr (nextFamily (MeasurableFamily.inr valueFamily))
    funext parameter
    simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]
  · apply congr (valueReduce.wrapUnary .inr .inr
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Spec.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]

theorem reduce_app_eq
    (function argument : Expr) :
    reduce (.app function argument) =
      if function.isValue then
        if argument.isValue then
          match function with
          | .lam body => .next (body.substHead argument)
          | fix@(.fix body) => .next (body.substTwo argument fix)
          | _ => .stuck
        else (reduce argument).wrap (fun next => .app function next)
      else (reduce function).wrap (fun next => .app next argument) := by
  cases function <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceApp {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {function argument : α → Expr}
    (functionFamily : MeasurableFamily α function)
    (argumentFamily : MeasurableFamily α argument)
    (functionReduce : MeasurableActionFamily α
      (fun parameter => reduce (function parameter)))
    (argumentReduce : MeasurableActionFamily α
      (fun parameter => reduce (argument parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.app (function parameter) (argument parameter))) := by
  classical
  by_cases functionValue : Expr.isValue functionFamily.skeleton = true
  · by_cases argumentValue : Expr.isValue argumentFamily.skeleton = true
    · cases functionSkeletonEq : functionFamily.skeleton
      case lam =>
          apply congr (nextFamily (functionFamily.firstChild.substHead argumentFamily))
          funext parameter
          have fixed := functionFamily.skeleton_eq parameter
          have actualFunctionValue : (function parameter).isValue = true :=
            (functionFamily.isValue_eq parameter).trans functionValue
          have actualArgumentValue : (argument parameter).isValue = true :=
            (argumentFamily.isValue_eq parameter).trans argumentValue
          rw [functionSkeletonEq] at fixed
          cases actualEq : function parameter <;>
            rw [actualEq] at actualFunctionValue <;>
            simp [actualEq, reduce, Expr.skeleton, Expr.firstChild, Expr.isValue,
              Expr.isValue, Action.wrap, actualFunctionValue, actualArgumentValue]
              at fixed actualFunctionValue ⊢
      case fix =>
          apply congr
            (nextFamily (functionFamily.firstChild.substTwo argumentFamily functionFamily))
          funext parameter
          have fixed := functionFamily.skeleton_eq parameter
          have actualFunctionValue : (function parameter).isValue = true :=
            (functionFamily.isValue_eq parameter).trans functionValue
          have actualArgumentValue : (argument parameter).isValue = true :=
            (argumentFamily.isValue_eq parameter).trans argumentValue
          rw [functionSkeletonEq] at fixed
          cases actualEq : function parameter <;>
            rw [actualEq] at actualFunctionValue <;>
            simp [actualEq, reduce, Expr.skeleton, Expr.firstChild, Expr.isValue,
              Expr.isValue, Action.wrap, actualFunctionValue, actualArgumentValue]
              at fixed actualFunctionValue ⊢
      all_goals
        apply congr stuck
        funext parameter
        have fixed := functionFamily.skeleton_eq parameter
        have actualFunctionValue : (function parameter).isValue = true :=
          (functionFamily.isValue_eq parameter).trans functionValue
        have actualArgumentValue : (argument parameter).isValue = true :=
          (argumentFamily.isValue_eq parameter).trans argumentValue
        rw [functionSkeletonEq] at fixed
        cases actualEq : function parameter <;>
          rw [actualEq] at actualFunctionValue <;>
          simp [actualEq, Expr.skeleton] at fixed <;>
          rw [reduce_app_eq] <;>
          rw [actualFunctionValue, actualArgumentValue] <;>
          simp [Action.wrap]
    · apply congr (argumentReduce.wrapBinaryRight function functionFamily.measurable
          .app .app (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualFunctionValue : (function parameter).isValue = true :=
        (functionFamily.isValue_eq parameter).trans functionValue
      have actualArgumentValue : (argument parameter).isValue = false := by
        rw [argumentFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true argumentValue
      rw [reduce_app_eq]
      rw [actualFunctionValue, actualArgumentValue]
      simp
  · apply congr (functionReduce.wrapBinaryLeft argument argumentFamily.measurable
        .app .app (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualFunctionValue : (function parameter).isValue = false := by
      rw [functionFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true functionValue
    rw [reduce_app_eq]
    rw [actualFunctionValue]
    simp

theorem reduce_fst_eq
    (pair : Expr) :
    reduce (.fst pair) =
      if pair.isValue then
        match pair with
        | .pair left _ => .next left
        | _ => .stuck
      else (reduce pair).wrap .fst := by
  cases pair <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceFst {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {pair : α → Expr} (pairFamily : MeasurableFamily α pair)
    (pairReduce : MeasurableActionFamily α (fun parameter => reduce (pair parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.fst (pair parameter))) := by
  classical
  by_cases pairValue : Expr.isValue pairFamily.skeleton = true
  · cases pairSkeletonEq : pairFamily.skeleton
    case pair =>
      apply congr (nextFamily pairFamily.firstChild)
      funext parameter
      have fixed := pairFamily.skeleton_eq parameter
      have actualPairValue : (pair parameter).isValue = true :=
        (pairFamily.isValue_eq parameter).trans pairValue
      rw [pairSkeletonEq] at fixed
      cases actualEq : pair parameter <;>
        rw [actualEq] at actualPairValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_fst_eq, actualPairValue] <;>
        simp [Expr.firstChild]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := pairFamily.skeleton_eq parameter
      have actualPairValue : (pair parameter).isValue = true :=
        (pairFamily.isValue_eq parameter).trans pairValue
      rw [pairSkeletonEq] at fixed
      cases actualEq : pair parameter <;>
        rw [actualEq] at actualPairValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_fst_eq, actualPairValue] <;>
        simp
  · apply congr (pairReduce.wrapUnary .fst .fst
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualPairValue : (pair parameter).isValue = false := by
      rw [pairFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true pairValue
    rw [reduce_fst_eq, actualPairValue]
    simp

theorem reduce_snd_eq
    (pair : Expr) :
    reduce (.snd pair) =
      if pair.isValue then
        match pair with
        | .pair _ right => .next right
        | _ => .stuck
      else (reduce pair).wrap .snd := by
  cases pair <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceSnd {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {pair : α → Expr} (pairFamily : MeasurableFamily α pair)
    (pairReduce : MeasurableActionFamily α (fun parameter => reduce (pair parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.snd (pair parameter))) := by
  classical
  by_cases pairValue : Expr.isValue pairFamily.skeleton = true
  · cases pairSkeletonEq : pairFamily.skeleton
    case pair =>
      apply congr (nextFamily pairFamily.secondChild)
      funext parameter
      have fixed := pairFamily.skeleton_eq parameter
      have actualPairValue : (pair parameter).isValue = true :=
        (pairFamily.isValue_eq parameter).trans pairValue
      rw [pairSkeletonEq] at fixed
      cases actualEq : pair parameter <;>
        rw [actualEq] at actualPairValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_snd_eq, actualPairValue] <;>
        simp [Expr.secondChild]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := pairFamily.skeleton_eq parameter
      have actualPairValue : (pair parameter).isValue = true :=
        (pairFamily.isValue_eq parameter).trans pairValue
      rw [pairSkeletonEq] at fixed
      cases actualEq : pair parameter <;>
        rw [actualEq] at actualPairValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_snd_eq, actualPairValue] <;>
        simp
  · apply congr (pairReduce.wrapUnary .snd .snd
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualPairValue : (pair parameter).isValue = false := by
      rw [pairFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true pairValue
    rw [reduce_snd_eq, actualPairValue]
    simp

theorem reduce_matchSum_eq
    (scrutinee left right : Expr) :
    reduce (.matchSum scrutinee left right) =
      if scrutinee.isValue then
        match scrutinee with
        | .inl value => .next (left.substHead value)
        | .inr value => .next (right.substHead value)
        | _ => .stuck
      else (reduce scrutinee).wrap
        (fun next => .matchSum next left right) := by
  cases scrutinee <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceMatchSum {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {scrutinee left right : α → Expr}
    (scrutineeFamily : MeasurableFamily α scrutinee)
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (scrutineeReduce : MeasurableActionFamily α
      (fun parameter => reduce (scrutinee parameter))) :
    MeasurableActionFamily α (fun parameter =>
      reduce (.matchSum (scrutinee parameter) (left parameter) (right parameter))) := by
  classical
  by_cases scrutineeValue : Expr.isValue scrutineeFamily.skeleton = true
  · cases scrutineeSkeletonEq : scrutineeFamily.skeleton
    case inl =>
      apply congr (nextFamily (leftFamily.substHead scrutineeFamily.firstChild))
      funext parameter
      have fixed := scrutineeFamily.skeleton_eq parameter
      have actualScrutineeValue : (scrutinee parameter).isValue = true :=
        (scrutineeFamily.isValue_eq parameter).trans scrutineeValue
      rw [scrutineeSkeletonEq] at fixed
      cases actualEq : scrutinee parameter <;>
        rw [actualEq] at actualScrutineeValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_matchSum_eq, actualScrutineeValue] <;>
        simp [Expr.firstChild]
    case inr =>
      apply congr (nextFamily (rightFamily.substHead scrutineeFamily.firstChild))
      funext parameter
      have fixed := scrutineeFamily.skeleton_eq parameter
      have actualScrutineeValue : (scrutinee parameter).isValue = true :=
        (scrutineeFamily.isValue_eq parameter).trans scrutineeValue
      rw [scrutineeSkeletonEq] at fixed
      cases actualEq : scrutinee parameter <;>
        rw [actualEq] at actualScrutineeValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_matchSum_eq, actualScrutineeValue] <;>
        simp [Expr.firstChild]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := scrutineeFamily.skeleton_eq parameter
      have actualScrutineeValue : (scrutinee parameter).isValue = true :=
        (scrutineeFamily.isValue_eq parameter).trans scrutineeValue
      rw [scrutineeSkeletonEq] at fixed
      cases actualEq : scrutinee parameter <;>
        rw [actualEq] at actualScrutineeValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_matchSum_eq, actualScrutineeValue] <;>
        simp
  · apply congr (scrutineeReduce.wrapTernaryFirst left right leftFamily.measurable
        rightFamily.measurable .matchSum .matchSum
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualScrutineeValue : (scrutinee parameter).isValue = false := by
      rw [scrutineeFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true scrutineeValue
    rw [reduce_matchSum_eq, actualScrutineeValue]
    simp

theorem reduce_matchList_eq
    (scrutinee nilCase consCase : Expr) :
    reduce (.matchList scrutinee nilCase consCase) =
      if scrutinee.isValue then
        match scrutinee with
        | .nil => .next nilCase
        | .cons head tail => .next (consCase.substTwo head tail)
        | _ => .stuck
      else (reduce scrutinee).wrap
        (fun next => .matchList next nilCase consCase) := by
  cases scrutinee <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceMatchList {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {scrutinee nilCase consCase : α → Expr}
    (scrutineeFamily : MeasurableFamily α scrutinee)
    (nilFamily : MeasurableFamily α nilCase) (consFamily : MeasurableFamily α consCase)
    (scrutineeReduce : MeasurableActionFamily α
      (fun parameter => reduce (scrutinee parameter))) :
    MeasurableActionFamily α (fun parameter => reduce
      (.matchList (scrutinee parameter) (nilCase parameter)
        (consCase parameter))) := by
  classical
  by_cases scrutineeValue : Expr.isValue scrutineeFamily.skeleton = true
  · cases scrutineeSkeletonEq : scrutineeFamily.skeleton
    case nil =>
      apply congr (nextFamily nilFamily)
      funext parameter
      have fixed := scrutineeFamily.skeleton_eq parameter
      have actualScrutineeValue : (scrutinee parameter).isValue = true :=
        (scrutineeFamily.isValue_eq parameter).trans scrutineeValue
      rw [scrutineeSkeletonEq] at fixed
      cases actualEq : scrutinee parameter <;>
        rw [actualEq] at actualScrutineeValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_matchList_eq, actualScrutineeValue] <;>
        simp
    case cons =>
      apply congr (nextFamily
        (consFamily.substTwo scrutineeFamily.firstChild scrutineeFamily.secondChild))
      funext parameter
      have fixed := scrutineeFamily.skeleton_eq parameter
      have actualScrutineeValue : (scrutinee parameter).isValue = true :=
        (scrutineeFamily.isValue_eq parameter).trans scrutineeValue
      rw [scrutineeSkeletonEq] at fixed
      cases actualEq : scrutinee parameter <;>
        rw [actualEq] at actualScrutineeValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_matchList_eq, actualScrutineeValue] <;>
        simp [Expr.firstChild, Expr.secondChild]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := scrutineeFamily.skeleton_eq parameter
      have actualScrutineeValue : (scrutinee parameter).isValue = true :=
        (scrutineeFamily.isValue_eq parameter).trans scrutineeValue
      rw [scrutineeSkeletonEq] at fixed
      cases actualEq : scrutinee parameter <;>
        rw [actualEq] at actualScrutineeValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_matchList_eq, actualScrutineeValue] <;>
        simp
  · apply congr (scrutineeReduce.wrapTernaryFirst nilCase consCase nilFamily.measurable
        consFamily.measurable .matchList .matchList
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualScrutineeValue : (scrutinee parameter).isValue = false := by
      rw [scrutineeFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true scrutineeValue
    rw [reduce_matchList_eq, actualScrutineeValue]
    simp

theorem reduce_ite_eq
    (condition thenBranch elseBranch : Expr) :
    reduce (.ite condition thenBranch elseBranch) =
      if condition.isValue then
        match condition with
        | .bool true => .next thenBranch
        | .bool false => .next elseBranch
        | _ => .stuck
      else (reduce condition).wrap
        (fun next => .ite next thenBranch elseBranch) := by
  cases condition <;> simp only [reduce, Determinize.Spec.Paper.reduce]
  case bool value => cases value <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceIte {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {condition thenBranch elseBranch : α → Expr}
    (conditionFamily : MeasurableFamily α condition)
    (thenFamily : MeasurableFamily α thenBranch)
    (elseFamily : MeasurableFamily α elseBranch)
    (conditionReduce : MeasurableActionFamily α
      (fun parameter => reduce (condition parameter))) :
    MeasurableActionFamily α (fun parameter =>
      reduce (.ite (condition parameter) (thenBranch parameter)
        (elseBranch parameter))) := by
  classical
  by_cases conditionValue : Expr.isValue conditionFamily.skeleton = true
  · cases conditionSkeletonEq : conditionFamily.skeleton
    case bool value =>
      cases value with
      | false =>
          apply congr (nextFamily elseFamily)
          funext parameter
          have fixed := conditionFamily.skeleton_eq parameter
          have actualConditionValue : (condition parameter).isValue = true :=
            (conditionFamily.isValue_eq parameter).trans conditionValue
          rw [conditionSkeletonEq] at fixed
          cases actualEq : condition parameter <;>
            rw [actualEq] at actualConditionValue <;>
            simp [actualEq, Expr.skeleton] at fixed <;>
            rw [reduce_ite_eq, actualConditionValue] <;>
            simp_all
      | true =>
          apply congr (nextFamily thenFamily)
          funext parameter
          have fixed := conditionFamily.skeleton_eq parameter
          have actualConditionValue : (condition parameter).isValue = true :=
            (conditionFamily.isValue_eq parameter).trans conditionValue
          rw [conditionSkeletonEq] at fixed
          cases actualEq : condition parameter <;>
            rw [actualEq] at actualConditionValue <;>
            simp [actualEq, Expr.skeleton] at fixed <;>
            rw [reduce_ite_eq, actualConditionValue] <;>
            simp_all
    all_goals
      apply congr stuck
      funext parameter
      have fixed := conditionFamily.skeleton_eq parameter
      have actualConditionValue : (condition parameter).isValue = true :=
        (conditionFamily.isValue_eq parameter).trans conditionValue
      rw [conditionSkeletonEq] at fixed
      cases actualEq : condition parameter <;>
        rw [actualEq] at actualConditionValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_ite_eq, actualConditionValue] <;>
        simp
  · apply congr (conditionReduce.wrapTernaryFirst thenBranch elseBranch
        thenFamily.measurable elseFamily.measurable .ite .ite
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualConditionValue : (condition parameter).isValue = false := by
      rw [conditionFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true conditionValue
    rw [reduce_ite_eq, actualConditionValue]
    simp

theorem reduce_let_eq
    (value body : Expr) :
    reduce (.letE value body) =
      if value.isValue then .next (body.substHead value)
      else (reduce value).wrap (fun next => .letE next body) := by
  cases value <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceLet {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {value body : α → Expr}
    (valueFamily : MeasurableFamily α value) (bodyFamily : MeasurableFamily α body)
    (valueReduce : MeasurableActionFamily α (fun parameter => reduce (value parameter))) :
    MeasurableActionFamily α (fun parameter =>
      reduce (.letE (value parameter) (body parameter))) := by
  classical
  by_cases valueIsValue : Expr.isValue valueFamily.skeleton = true
  · apply congr (nextFamily (bodyFamily.substHead valueFamily))
    funext parameter
    have actualValueIsValue : (value parameter).isValue = true :=
      (valueFamily.isValue_eq parameter).trans valueIsValue
    rw [reduce_let_eq, actualValueIsValue]
    simp
  · apply congr (valueReduce.wrapBinaryLeft body bodyFamily.measurable
        (fun value body => .letE value body)
        (fun value body => .letE value body)
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualValueIsValue : (value parameter).isValue = false := by
      rw [valueFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true valueIsValue
    rw [reduce_let_eq, actualValueIsValue]
    simp

def realCoordinateResult {α : Type*} [MeasurableSpace α]
    {expression : α → Expr} (family : MeasurableFamily α expression)
    (value : α → ℝ) (valueMeasurable : Measurable value) :
    MeasurableFamily α (fun parameter => Expr.real (value parameter)) :=
  MeasurableFamily.realLiteral.comp value valueMeasurable

theorem reduce_neg_eq
    (body : Expr) :
    reduce (.neg body) =
      if body.isValue then
        match body with
        | .real value => .next (.real (-value))
        | _ => .stuck
      else (reduce body).wrap .neg := by
  cases body <;> simp only [reduce, Determinize.Spec.Paper.reduce]

noncomputable def reduceNeg {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {body : α → Expr} (bodyFamily : MeasurableFamily α body)
    (bodyReduce : MeasurableActionFamily α (fun parameter => reduce (body parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.neg (body parameter))) := by
  classical
  by_cases bodyValue : Expr.isValue bodyFamily.skeleton = true
  · cases bodySkeletonEq : bodyFamily.skeleton
    case real =>
      let resultFamily := realCoordinateResult bodyFamily
        (fun parameter => -((body parameter).realCoordinates.getD 0 0))
        (bodyFamily.coordinate_measurable 0).neg
      apply congr (nextFamily resultFamily)
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_neg_eq, actualBodyValue] <;>
        simp_all [resultFamily, Expr.realCoordinates, List.getD]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_neg_eq, actualBodyValue] <;>
        simp
  · apply congr (bodyReduce.wrapUnary .neg .neg
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualBodyValue : (body parameter).isValue = false := by
      rw [bodyFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true bodyValue
    rw [reduce_neg_eq, actualBodyValue]
    simp

theorem reduce_add_eq
    (left right : Expr) :
    reduce (.add left right) =
      if left.isValue then
        if right.isValue then
          match realValue? left, realValue? right with
          | some x, some y => .next (.real (x + y))
          | _, _ => .stuck
        else (reduce right).wrap (.add left)
      else (reduce left).wrap (fun next => .add next right) := by
  cases left <;> cases right <;> simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

noncomputable def reduceAdd {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.add (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let resultFamily := realCoordinateResult leftFamily
            (fun parameter => (left parameter).realCoordinates.getD 0 0 +
              (right parameter).realCoordinates.getD 0 0)
            ((leftFamily.coordinate_measurable 0).add (rightFamily.coordinate_measurable 0))
          apply congr (nextFamily resultFamily)
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_add_eq, actualLeftValue, actualRightValue] <;>
            simp_all [resultFamily, realValue?, Expr.realCoordinates, List.getD]
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_add_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_add_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          .add .add (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_add_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        .add .add (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_add_eq, actualLeftValue]
    simp

theorem reduce_mul_eq
    (left right : Expr) :
    reduce (.mul left right) =
      if left.isValue then
        if right.isValue then
          match realValue? left, realValue? right with
          | some x, some y => .next (.real (x * y))
          | _, _ => .stuck
        else (reduce right).wrap (.mul left)
      else (reduce left).wrap (fun next => .mul next right) := by
  cases left <;> cases right <;> simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

noncomputable def reduceMul {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.mul (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let resultFamily := realCoordinateResult leftFamily
            (fun parameter => (left parameter).realCoordinates.getD 0 0 *
              (right parameter).realCoordinates.getD 0 0)
            ((leftFamily.coordinate_measurable 0).mul (rightFamily.coordinate_measurable 0))
          apply congr (nextFamily resultFamily)
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_mul_eq, actualLeftValue, actualRightValue] <;>
            simp_all [resultFamily, realValue?, Expr.realCoordinates, List.getD]
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_mul_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_mul_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          .mul .mul (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_mul_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        .mul .mul (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_mul_eq, actualLeftValue]
    simp

theorem reduce_div_eq
    (left right : Expr) :
    reduce (.div left right) =
      if left.isValue then
        if right.isValue then
          match realValue? left, realValue? right with
          | some x, some y => .next (.real (x / y))
          | _, _ => .stuck
        else (reduce right).wrap (.div left)
      else (reduce left).wrap (fun next => .div next right) := by
  cases left <;> cases right <;> simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

noncomputable def reduceDiv {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.div (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let resultFamily := realCoordinateResult leftFamily
            (fun parameter => (left parameter).realCoordinates.getD 0 0 /
              (right parameter).realCoordinates.getD 0 0)
            ((leftFamily.coordinate_measurable 0).div (rightFamily.coordinate_measurable 0))
          apply congr (nextFamily resultFamily)
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_div_eq, actualLeftValue, actualRightValue] <;>
            simp_all [resultFamily, realValue?, Expr.realCoordinates, List.getD]
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_div_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_div_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          .div .div (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_div_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        .div .div (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_div_eq, actualLeftValue]
    simp

theorem reduce_lt_eq
    (left right : Expr) :
    reduce (.lt left right) =
      if left.isValue then
        if right.isValue then
          match realValue? left, realValue? right with
          | some x, some y => .next (.bool (x < y))
          | _, _ => .stuck
        else (reduce right).wrap (.lt left)
      else (reduce left).wrap (fun next => .lt next right) := by
  cases left <;> cases right <;> simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

noncomputable def reduceLt {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.lt (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let region : Set α := {parameter |
            (left parameter).realCoordinates.getD 0 0 <
              (right parameter).realCoordinates.getD 0 0}
          have regionMeasurable : MeasurableSet region :=
            measurableSet_lt (leftFamily.coordinate_measurable 0)
              (rightFamily.coordinate_measurable 0)
          let trueFamily : MeasurableActionFamily α (fun _ => .next (.bool true)) :=
            nextFamily (MeasurableFamily.constant (.bool true))
          let falseFamily : MeasurableActionFamily α (fun _ => .next (.bool false)) :=
            nextFamily (MeasurableFamily.constant (.bool false))
          apply congr (.piecewise regionMeasurable trueFamily falseFamily)
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_lt_eq, actualLeftValue, actualRightValue] <;>
            by_cases less : parameter ∈ region <;>
            simp_all [region, trueFamily, falseFamily, Set.piecewise, realValue?,
              Expr.realCoordinates, List.getD] <;>
            split <;> simp_all
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_lt_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_lt_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          Expr.lt Expr.lt (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_lt_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        Expr.lt Expr.lt (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_lt_eq, actualLeftValue]
    simp

theorem reduce_uniform_eq (mode : Mode) (kind : Kind) (lower upper : Expr) :
    reduce (.uniform mode kind lower upper) =
      if lower.isValue then
        if upper.isValue then match realValue? lower, realValue? upper with
          | some a, some b => .sample (mode, kind, .uniform) (uniformFiber kind a b) .real
          | _, _ => .stuck
        else (reduce upper).wrap (.uniform mode kind lower)
      else (reduce lower).wrap (fun next => .uniform mode kind next upper) := by
  simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

theorem reduce_gaussian_eq (mode : Mode) (kind : Kind) (mean variance : Expr) :
    reduce (.gaussian mode kind mean variance) =
      if mean.isValue then
        if variance.isValue then match realValue? mean, realValue? variance with
          | some m, some v => .sample (mode, kind, .gaussian) (gaussianFiber kind m v) .real
          | _, _ => .stuck
        else (reduce variance).wrap (.gaussian mode kind mean)
      else (reduce mean).wrap (fun next => .gaussian mode kind next variance) := by
  simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

theorem reduce_poisson_eq (mode : Mode) (kind : Kind) (rate : Expr) :
    reduce (.poisson mode kind rate) =
      if rate.isValue then match realValue? rate with
        | some r => .sample (mode, kind, .poisson) (poissonFiber kind r) .real
        | none => .stuck
      else (reduce rate).wrap (.poisson mode kind) := by
  simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

theorem reduce_bernoulli_eq (mode : Mode) (kind : Kind) (probability : Expr) :
    reduce (.bernoulli mode kind probability) =
      if probability.isValue then match realValue? probability with
        | some r => .sample (mode, kind, .bernoulli) (bernoulliFiber kind r) .real
        | none => .stuck
      else (reduce probability).wrap (.bernoulli mode kind) := by
  simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

theorem reduce_exponential_eq (mode : Mode) (kind : Kind) (rate : Expr) :
    reduce (.exponential mode kind rate) =
      if rate.isValue then match realValue? rate with
        | some r => .sample (mode, kind, .exponential) (exponentialFiber kind r) .real
        | none => .stuck
      else (reduce rate).wrap (.exponential mode kind) := by
  simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

theorem reduce_beta_eq (mode : Mode) (kind : Kind) (alpha beta : Expr) :
    reduce (.beta mode kind alpha beta) =
      if alpha.isValue then
        if beta.isValue then match realValue? alpha, realValue? beta with
          | some a, some b => .sample (mode, kind, .beta) (betaFiber kind a b) .real
          | _, _ => .stuck
        else (reduce beta).wrap (.beta mode kind alpha)
      else (reduce alpha).wrap (fun next => .beta mode kind next beta) := by
  simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

theorem reduce_gamma_eq (mode : Mode) (kind : Kind) (shape rate : Expr) :
    reduce (.gamma mode kind shape rate) =
      if shape.isValue then
        if rate.isValue then match realValue? shape, realValue? rate with
          | some k, some r => .sample (mode, kind, .gamma) (gammaFiber kind k r) .real
          | _, _ => .stuck
        else (reduce rate).wrap (.gamma mode kind shape)
      else (reduce shape).wrap (fun next => .gamma mode kind next rate) := by
  simp only [reduce, Determinize.Spec.Paper.reduce] <;> rfl

noncomputable def reduceUniform {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (mode : Mode) (kind : Kind)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.uniform mode kind (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let parameters := fun parameter =>
            ((left parameter).realCoordinates.getD 0 0, (right parameter).realCoordinates.getD 0 0)
          have parametersMeasurable : Measurable parameters :=
            (leftFamily.coordinate_measurable 0).prodMk (rightFamily.coordinate_measurable 0)
          apply congr (.sample (site := (mode, kind, .uniform)) (SFiniteKernel.pullback (uniformDraw laws kind) parameters
            parametersMeasurable) (measurable_realLiteral measurable_snd))
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_uniform_eq, actualLeftValue, actualRightValue] <;>
            simp_all [pullback_apply, uniformDraw_apply, parameters, realValue?, Expr.realCoordinates,
              List.getD]
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_uniform_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_uniform_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          (.uniform mode kind) (.uniform mode kind) (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_uniform_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        (.uniform mode kind) (.uniform mode kind) (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_uniform_eq, actualLeftValue]
    simp

noncomputable def reduceGaussian {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (mode : Mode) (kind : Kind)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.gaussian mode kind (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let parameters := fun parameter =>
            ((left parameter).realCoordinates.getD 0 0, (right parameter).realCoordinates.getD 0 0)
          have parametersMeasurable : Measurable parameters :=
            (leftFamily.coordinate_measurable 0).prodMk (rightFamily.coordinate_measurable 0)
          apply congr (.sample (site := (mode, kind, .gaussian)) (SFiniteKernel.pullback (gaussianDraw laws kind) parameters
            parametersMeasurable) (measurable_realLiteral measurable_snd))
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_gaussian_eq, actualLeftValue, actualRightValue] <;>
            simp_all [pullback_apply, gaussianDraw_apply, parameters, realValue?, Expr.realCoordinates,
              List.getD]
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_gaussian_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_gaussian_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          (.gaussian mode kind) (.gaussian mode kind) (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_gaussian_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        (.gaussian mode kind) (.gaussian mode kind) (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_gaussian_eq, actualLeftValue]
    simp

noncomputable def reduceBeta {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (mode : Mode) (kind : Kind)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.beta mode kind (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let parameters := fun parameter =>
            ((left parameter).realCoordinates.getD 0 0, (right parameter).realCoordinates.getD 0 0)
          have parametersMeasurable : Measurable parameters :=
            (leftFamily.coordinate_measurable 0).prodMk (rightFamily.coordinate_measurable 0)
          apply congr (.sample (site := (mode, kind, .beta)) (SFiniteKernel.pullback (betaDraw laws kind) parameters
            parametersMeasurable) (measurable_realLiteral measurable_snd))
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_beta_eq, actualLeftValue, actualRightValue] <;>
            simp_all [pullback_apply, betaDraw_apply, parameters, realValue?, Expr.realCoordinates,
              List.getD]
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_beta_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_beta_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          (.beta mode kind) (.beta mode kind) (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_beta_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        (.beta mode kind) (.beta mode kind) (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_beta_eq, actualLeftValue]
    simp

noncomputable def reduceGamma {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (mode : Mode) (kind : Kind)
    {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.gamma mode kind (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let parameters := fun parameter =>
            ((left parameter).realCoordinates.getD 0 0, (right parameter).realCoordinates.getD 0 0)
          have parametersMeasurable : Measurable parameters :=
            (leftFamily.coordinate_measurable 0).prodMk (rightFamily.coordinate_measurable 0)
          apply congr (.sample (site := (mode, kind, .gamma)) (SFiniteKernel.pullback (gammaDraw laws kind) parameters
            parametersMeasurable) (measurable_realLiteral measurable_snd))
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_gamma_eq, actualLeftValue, actualRightValue] <;>
            simp_all [pullback_apply, gammaDraw_apply, parameters, realValue?, Expr.realCoordinates,
              List.getD]
        all_goals
          apply congr stuck
          funext parameter
          have leftFixed := leftFamily.skeleton_eq parameter
          have rightFixed := rightFamily.skeleton_eq parameter
          have actualLeftValue : (left parameter).isValue = true :=
            (leftFamily.isValue_eq parameter).trans leftValue
          have actualRightValue : (right parameter).isValue = true :=
            (rightFamily.isValue_eq parameter).trans rightValue
          rw [leftSkeletonEq] at leftFixed
          rw [rightSkeletonEq] at rightFixed
          cases leftActualEq : left parameter <;>
            rw [leftActualEq] at actualLeftValue <;>
            simp [leftActualEq, Expr.skeleton] at leftFixed <;>
            cases rightActualEq : right parameter <;>
            rw [rightActualEq] at actualRightValue <;>
            simp [rightActualEq, Expr.skeleton] at rightFixed <;>
            rw [reduce_gamma_eq, actualLeftValue, actualRightValue] <;>
            simp [realValue?]
      all_goals
        apply congr stuck
        funext parameter
        have leftFixed := leftFamily.skeleton_eq parameter
        have actualLeftValue : (left parameter).isValue = true :=
          (leftFamily.isValue_eq parameter).trans leftValue
        have actualRightValue : (right parameter).isValue = true :=
          (rightFamily.isValue_eq parameter).trans rightValue
        rw [leftSkeletonEq] at leftFixed
        cases leftActualEq : left parameter <;>
          rw [leftActualEq] at actualLeftValue <;>
          simp [leftActualEq, Expr.skeleton] at leftFixed <;>
          rw [reduce_gamma_eq, actualLeftValue, actualRightValue] <;>
          simp [realValue?]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable
          (.gamma mode kind) (.gamma mode kind) (by intros; simp [Expr.skeleton])
          (by intros; simp [Expr.realCoordinates]))
      funext parameter
      have actualLeftValue : (left parameter).isValue = true :=
        (leftFamily.isValue_eq parameter).trans leftValue
      have actualRightValue : (right parameter).isValue = false := by
        rw [rightFamily.isValue_eq parameter]
        exact Bool.eq_false_of_not_eq_true rightValue
      rw [reduce_gamma_eq, actualLeftValue, actualRightValue]
      simp
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable
        (.gamma mode kind) (.gamma mode kind) (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_gamma_eq, actualLeftValue]
    simp

noncomputable def reducePoisson {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (mode : Mode) (kind : Kind)
    {body : α → Expr} (bodyFamily : MeasurableFamily α body)
    (bodyReduce : MeasurableActionFamily α (fun parameter => reduce (body parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.poisson mode kind (body parameter))) := by
  classical
  by_cases bodyValue : Expr.isValue bodyFamily.skeleton = true
  · cases bodySkeletonEq : bodyFamily.skeleton
    case real =>
      let parameters := fun parameter => (body parameter).realCoordinates.getD 0 0
      have parametersMeasurable : Measurable parameters := bodyFamily.coordinate_measurable 0
      apply congr (.sample (site := (mode, kind, .poisson)) (SFiniteKernel.pullback (poissonDraw laws kind) parameters
        parametersMeasurable) (measurable_realLiteral measurable_snd))
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_poisson_eq, actualBodyValue] <;>
        simp_all [pullback_apply, poissonDraw_apply, parameters, realValue?, Expr.realCoordinates,
          List.getD]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_poisson_eq, actualBodyValue] <;>
        simp [realValue?]
  · apply congr (bodyReduce.wrapUnary (.poisson mode kind) (.poisson mode kind)
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualBodyValue : (body parameter).isValue = false := by
      rw [bodyFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true bodyValue
    rw [reduce_poisson_eq, actualBodyValue]
    simp

noncomputable def reduceBernoulli {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (mode : Mode) (kind : Kind)
    {body : α → Expr} (bodyFamily : MeasurableFamily α body)
    (bodyReduce : MeasurableActionFamily α (fun parameter => reduce (body parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.bernoulli mode kind (body parameter))) := by
  classical
  by_cases bodyValue : Expr.isValue bodyFamily.skeleton = true
  · cases bodySkeletonEq : bodyFamily.skeleton
    case real =>
      let parameters := fun parameter => (body parameter).realCoordinates.getD 0 0
      have parametersMeasurable : Measurable parameters := bodyFamily.coordinate_measurable 0
      apply congr (.sample (site := (mode, kind, .bernoulli)) (SFiniteKernel.pullback (bernoulliDraw laws kind) parameters
        parametersMeasurable) (measurable_realLiteral measurable_snd))
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_bernoulli_eq, actualBodyValue] <;>
        simp_all [pullback_apply, bernoulliDraw_apply, parameters, realValue?, Expr.realCoordinates,
          List.getD]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_bernoulli_eq, actualBodyValue] <;>
        simp [realValue?]
  · apply congr (bodyReduce.wrapUnary (.bernoulli mode kind) (.bernoulli mode kind)
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualBodyValue : (body parameter).isValue = false := by
      rw [bodyFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true bodyValue
    rw [reduce_bernoulli_eq, actualBodyValue]
    simp

noncomputable def reduceExponential {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (mode : Mode) (kind : Kind)
    {body : α → Expr} (bodyFamily : MeasurableFamily α body)
    (bodyReduce : MeasurableActionFamily α (fun parameter => reduce (body parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.exponential mode kind (body parameter))) := by
  classical
  by_cases bodyValue : Expr.isValue bodyFamily.skeleton = true
  · cases bodySkeletonEq : bodyFamily.skeleton
    case real =>
      let parameters := fun parameter => (body parameter).realCoordinates.getD 0 0
      have parametersMeasurable : Measurable parameters := bodyFamily.coordinate_measurable 0
      apply congr (.sample (site := (mode, kind, .exponential)) (SFiniteKernel.pullback (exponentialDraw laws kind) parameters
        parametersMeasurable) (measurable_realLiteral measurable_snd))
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_exponential_eq, actualBodyValue] <;>
        simp_all [pullback_apply, exponentialDraw_apply, parameters, realValue?, Expr.realCoordinates,
          List.getD]
    all_goals
      apply congr stuck
      funext parameter
      have fixed := bodyFamily.skeleton_eq parameter
      have actualBodyValue : (body parameter).isValue = true :=
        (bodyFamily.isValue_eq parameter).trans bodyValue
      rw [bodySkeletonEq] at fixed
      cases actualEq : body parameter <;>
        rw [actualEq] at actualBodyValue <;>
        simp [actualEq, Expr.skeleton] at fixed <;>
        rw [reduce_exponential_eq, actualBodyValue] <;>
        simp [realValue?]
  · apply congr (bodyReduce.wrapUnary (.exponential mode kind) (.exponential mode kind)
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualBodyValue : (body parameter).isValue = false := by
      rw [bodyFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true bodyValue
    rw [reduce_exponential_eq, actualBodyValue]
    simp

@[simp] theorem MeasurableFamily.firstChild_skeleton_def {α : Type*}
    [MeasurableSpace α] {expression : α → Expr}
    (family : MeasurableFamily α expression) :
    family.firstChild.skeleton = Skeleton.firstChild family.skeleton := rfl

@[simp] theorem MeasurableFamily.secondChild_skeleton_def {α : Type*}
    [MeasurableSpace α] {expression : α → Expr}
    (family : MeasurableFamily α expression) :
    family.secondChild.skeleton = Skeleton.secondChild family.skeleton := rfl

noncomputable def measurable_reduceAux
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (size : Nat) :
    {α : Type u} → [MeasurableSpace α] → {expression : α → Expr} →
      (family : MeasurableFamily α expression) → sizeOf family.skeleton = size →
      MeasurableActionFamily α (fun parameter => reduce (expression parameter)) := by
  induction size using Nat.strongRecOn with
  | ind size ih =>
      intro α measurableSpace expression family sizeEq
      classical
      have childReduce {child : α → Expr} (childFamily : MeasurableFamily α child)
          (smaller : sizeOf childFamily.skeleton < size) :
          MeasurableActionFamily α (fun parameter => reduce (child parameter)) :=
        ih _ smaller childFamily rfl
      cases skeletonEq : family.skeleton with
      | bvar index =>
          apply congr stuck
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp [actualEq, Expr.skeleton, reduce] at fixed ⊢
      | discrete mode kind d =>
          apply congr (.sample (site := (mode, kind, .discrete d))
            (SFiniteKernel.pullback (discreteDraw laws kind d) (fun _ => ()) measurable_const)
            (measurable_realLiteral measurable_snd))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [Expr.skeleton, reduce, pullback_apply, discreteDraw_apply]
      | reject | unit | bool | real | lam | fix | nil =>
          apply congr (nextFamily family)
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp [actualEq, Expr.skeleton, reduce] at fixed ⊢
      | pair leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reducePair laws family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | cons headSkeleton tailSkeleton =>
          have headSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have tailSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceCons laws family.firstChild family.secondChild
            (childReduce family.firstChild headSmaller)
            (childReduce family.secondChild tailSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | inl valueSkeleton =>
          have valueSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceInl laws family.firstChild
            (childReduce family.firstChild valueSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
      | inr valueSkeleton =>
          have valueSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceInr laws family.firstChild
            (childReduce family.firstChild valueSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
      | app functionSkeleton argumentSkeleton =>
          have functionSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have argumentSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceApp laws family.firstChild family.secondChild
            (childReduce family.firstChild functionSmaller)
            (childReduce family.secondChild argumentSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | fst pairSkeleton =>
          have pairSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceFst laws family.firstChild
            (childReduce family.firstChild pairSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
      | snd pairSkeleton =>
          have pairSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceSnd laws family.firstChild
            (childReduce family.firstChild pairSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
      | matchSum scrutineeSkeleton leftSkeleton rightSkeleton =>
          have scrutineeSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceMatchSum laws family.firstChild family.secondChild
            family.thirdChild (childReduce family.firstChild scrutineeSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild,
              Expr.thirdChild]
      | matchList scrutineeSkeleton nilSkeleton consSkeleton =>
          have scrutineeSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceMatchList laws family.firstChild
            family.secondChild family.thirdChild
            (childReduce family.firstChild scrutineeSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild,
              Expr.thirdChild]
      | ite conditionSkeleton thenSkeleton elseSkeleton =>
          have conditionSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceIte laws family.firstChild family.secondChild
            family.thirdChild (childReduce family.firstChild conditionSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild,
              Expr.thirdChild]
      | letE valueSkeleton bodySkeleton =>
          have valueSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceLet laws family.firstChild family.secondChild
            (childReduce family.firstChild valueSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | neg bodySkeleton =>
          have bodySmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceNeg laws family.firstChild
            (childReduce family.firstChild bodySmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
      | add leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceAdd laws family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | mul leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceMul laws family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | div leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceDiv laws family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | lt leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceLt laws family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | uniform mode kind leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceUniform laws mode kind family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | gaussian mode kind leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceGaussian laws mode kind family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | beta mode kind leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceBeta laws mode kind family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | gamma mode kind leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceGamma laws mode kind family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | poisson mode kind bodySkeleton =>
          have bodySmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reducePoisson laws mode kind family.firstChild
            (childReduce family.firstChild bodySmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
      | bernoulli mode kind bodySkeleton =>
          have bodySmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceBernoulli laws mode kind family.firstChild
            (childReduce family.firstChild bodySmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
      | exponential mode kind bodySkeleton =>
          have bodySmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceExponential laws mode kind family.firstChild
            (childReduce family.firstChild bodySmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [-Determinize.Spec.Paper.reduce, actualEq, Expr.skeleton, Expr.firstChild]
noncomputable def measurable_reduce {α : Type u} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {expression : α → Expr} (family : MeasurableFamily α expression) :
    MeasurableActionFamily α (fun parameter => reduce (expression parameter)) :=
  measurable_reduceAux laws (sizeOf family.skeleton) family rfl

noncomputable def kernel {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) :
    SFiniteKernel α Expr := by
  induction family with
  | next successorMeasurable =>
      exact SFiniteKernel.deterministic _
        successorMeasurable
  | sample draw continuationMeasurable =>
      exact SFiniteKernel.mapWithInput draw _
        continuationMeasurable
  | stuck => exact SFiniteKernel.zero
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily
      trueKernel falseKernel =>
      exact SFiniteKernel.piecewise
        measurableRegion trueKernel falseKernel

theorem kernel_apply {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) (parameter : α) :
    family.kernel.kernel parameter =
      match action parameter with
      | .next successor => Measure.dirac successor
      | .sample site fiber continuation => fiber.map continuation
      | .stuck => 0 := by
  induction family with
  | @next successor successorMeasurable =>
      simp only [kernel, SFiniteKernel.deterministic,
        Kernel.deterministic_apply]
  | @sample site draw continuation hContinuation =>
      let _ := draw.sfinite
      ext set measurableSet
      unfold kernel SFiniteKernel.mapWithInput
      rw [Kernel.map_apply' _ measurable_snd _ measurableSet]
      rw [Kernel.compProd_apply (measurable_snd measurableSet)]
      have sectionMeasurable : Measurable fun value : ℝ =>
          continuation (parameter, value) :=
        hContinuation.comp (measurable_const.prodMk measurable_id)
      rw [Measure.map_apply sectionMeasurable measurableSet]
      simp only [Kernel.deterministic_apply]
      change (∫⁻ value, (Measure.dirac (continuation (parameter, value))) set
          ∂draw.kernel parameter) =
        draw.kernel parameter
          ((fun value => continuation (parameter, value)) ⁻¹' set)
      simp_rw [Measure.dirac_apply' _ measurableSet]
      exact lintegral_indicator_one (sectionMeasurable measurableSet)
  | stuck => rfl
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily
      trueApply falseApply =>
      classical
      by_cases member : parameter ∈ region
      · simp only [kernel, SFiniteKernel.piecewise,
          Kernel.piecewise_apply, if_pos member]
        simp only [Set.piecewise, member]
        change trueFamily.kernel.kernel parameter = _
        exact trueApply
      · simp only [kernel, SFiniteKernel.piecewise,
          Kernel.piecewise_apply, if_neg member]
        simp only [Set.piecewise, member]
        change falseFamily.kernel.kernel parameter = _
        exact falseApply

noncomputable def toSkeletonFiber (skeleton : Skeleton) (expression : Expr) :
    SkeletonFiber skeleton := by
  classical
  let base := (SkeletonFiber skeleton).piecewise id (fun _ => zeroFill skeleton)
  refine ⟨base expression, ?_⟩
  by_cases member : expression ∈ SkeletonFiber skeleton
  · simpa [base, Set.piecewise, member] using member
  · have different : expression.skeleton ≠ skeleton := by
      simpa [SkeletonFiber] using member
    simp [base, Set.piecewise, different, SkeletonFiber]

@[simp] theorem toSkeletonFiber_coe_of_mem (skeleton : Skeleton) (expression : Expr)
    (member : expression ∈ SkeletonFiber skeleton) :
    (toSkeletonFiber skeleton expression : Expr) = expression := by
  classical
  simp [toSkeletonFiber, Set.piecewise, member]

theorem measurable_toSkeletonFiber (skeleton : Skeleton) :
    Measurable (toSkeletonFiber skeleton) := by
  classical
  let base : Expr → Expr := (SkeletonFiber skeleton).piecewise id (fun _ => zeroFill skeleton)
  have baseMeasurable : Measurable base :=
    measurable_id.piecewise (skeletonFiber_measurable skeleton) measurable_const
  have baseFixed : ∀ expression, base expression ∈ SkeletonFiber skeleton := by
    intro expression
    by_cases member : expression ∈ SkeletonFiber skeleton
    · simpa [base, Set.piecewise, member] using member
    · have different : expression.skeleton ≠ skeleton := by
        simpa [SkeletonFiber] using member
      simp [base, Set.piecewise, different, SkeletonFiber]
  have lifted : Measurable fun expression =>
      (⟨base expression, baseFixed expression⟩ : SkeletonFiber skeleton) :=
    Measurable.subtype_mk baseMeasurable
  have equality : toSkeletonFiber skeleton = fun expression =>
      (⟨base expression, baseFixed expression⟩ : SkeletonFiber skeleton) := by
    funext expression
    apply Subtype.ext
    rfl
  rw [equality]
  exact lifted

noncomputable def skeletonKernel
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (skeleton : Skeleton) : Kernel Expr Expr := by
  classical
  let actionFamily := measurable_reduce laws (MeasurableFamily.skeletonFiber skeleton)
  let localKernel := actionFamily.kernel
  let pulled := localKernel.kernel.comap (toSkeletonFiber skeleton)
    (measurable_toSkeletonFiber skeleton)
  exact Kernel.piecewise (skeletonFiber_measurable skeleton) pulled 0

theorem skeletonKernel_apply_of_mem
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (skeleton : Skeleton) (expression : Expr)
    (member : expression ∈ SkeletonFiber skeleton) :
    skeletonKernel laws skeleton expression = stepMeasure expression := by
  classical
  simp only [skeletonKernel, Kernel.piecewise_apply, if_pos member,
    Kernel.comap_apply]
  let actionFamily := measurable_reduce laws (MeasurableFamily.skeletonFiber skeleton)
  rw [kernel_apply actionFamily (toSkeletonFiber skeleton expression)]
  rw [toSkeletonFiber_coe_of_mem skeleton expression member]
  unfold stepMeasure
  cases reduce expression <;> rfl

theorem skeletonKernel_apply_of_not_mem
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (skeleton : Skeleton) (expression : Expr)
    (notMember : expression ∉ SkeletonFiber skeleton) :
    skeletonKernel laws skeleton expression = 0 := by
  classical
  simp [skeletonKernel, Kernel.piecewise_apply, notMember]

theorem skeletonKernel_sfinite
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (skeleton : Skeleton) : IsSFiniteKernel (skeletonKernel laws skeleton) := by
  classical
  unfold skeletonKernel
  let actionFamily := measurable_reduce laws (MeasurableFamily.skeletonFiber skeleton)
  letI := actionFamily.kernel.sfinite
  infer_instance

noncomputable def globalKernel
    (laws : Determinize.Proof.Paper.PrimitiveLaws) : Kernel Expr Expr :=
  Kernel.sum (skeletonKernel laws)

theorem globalKernel_apply
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (expression : Expr) :
    globalKernel laws expression = stepMeasure expression := by
  classical
  rw [globalKernel, Kernel.sum_apply]
  apply Measure.ext
  intro set measurableSet
  rw [Measure.sum_apply _ measurableSet, tsum_eq_single expression.skeleton]
  · exact congrArg (fun measure : Measure Expr => measure set)
      (skeletonKernel_apply_of_mem laws expression.skeleton expression
        (by simp [SkeletonFiber]))
  · intro other different
    rw [skeletonKernel_apply_of_not_mem]
    · rfl
    · simpa [SkeletonFiber, eq_comm] using different

theorem globalKernel_sfinite
    (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    IsSFiniteKernel (globalKernel laws) := by
  classical
  letI (skeleton : Skeleton) : IsSFiniteKernel (skeletonKernel laws skeleton) :=
    skeletonKernel_sfinite laws skeleton
  unfold globalKernel
  infer_instance

theorem sample_continuation_measurable_of_family
    {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) (parameter : α)
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (equality : action parameter = .sample site fiber continuation) :
    Measurable continuation := by
  induction family with
  | next => simp at equality
  | @sample actualSite draw inner innerMeasurable =>
      simp only [Action.sample.injEq] at equality
      rw [← equality.2.2]
      exact innerMeasurable.comp (measurable_const.prodMk measurable_id)
  | stuck => simp at equality
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily
      trueResult falseResult =>
      classical
      by_cases member : parameter ∈ region
      · apply trueResult
        simpa [Set.piecewise, member] using equality
      · apply falseResult
        simpa [Set.piecewise, member] using equality

theorem reduce_sample_continuation_measurable
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (expression : Expr) {site : Mode × Kind × Op} (fiber : Measure ℝ) (continuation : ℝ → Expr)
    (equality : reduce expression = .sample site fiber continuation) :
    Measurable continuation := by
  let family := measurable_reduce laws
    (MeasurableFamily.constant (α := Unit) expression)
  exact sample_continuation_measurable_of_family family () equality

theorem meanKernel_mass_le_one
    (op : Determinize.Spec.Paper.Op)
    (params : Determinize.Spec.Paper.Params op) :
    (primitiveKernelPack laws .mean op).kernel params Set.univ ≤ 1 := by
  classical
  change (Kernel.piecewise
    (Determinize.Proof.Paper.measurableSet_domain op)
    (Kernel.deterministic (Determinize.Spec.Paper.meanValue op)
      (measurable_meanValue op)) 0 params) Set.univ ≤ 1
  rw [Kernel.piecewise_apply]
  split <;> simp [Kernel.deterministic_apply]

theorem primitiveFiber_mass_le_one
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind) (op : Op)
    (affine general : List ℝ)
    (affineArity : affine.length = Determinize.Spec.Paper.affineArity op)
    (generalArity : general.length = Determinize.Spec.Paper.generalArity op) :
    primitiveFiber kind op affine general Set.univ ≤ 1 := by
  rw [primitiveFiber_eq_atomic laws kind op affine general affineArity generalArity]
  cases kind with
  | stochastic => exact laws.mass_le_one op _
  | mean => exact meanKernel_mass_le_one op _

theorem uniformFiber_mass_le_one (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (lower upper : ℝ) : uniformFiber kind lower upper Set.univ ≤ 1 := by
  rw [uniformFiber_eq]
  exact primitiveFiber_mass_le_one laws kind .uniform [lower, upper] [] rfl rfl

theorem gaussianFiber_mass_le_one (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (mean variance : ℝ) : gaussianFiber kind mean variance Set.univ ≤ 1 := by
  rw [gaussianFiber_eq]
  exact primitiveFiber_mass_le_one laws kind .gaussian [mean] [variance] rfl rfl

theorem poissonFiber_mass_le_one (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (rate : ℝ) : poissonFiber kind rate Set.univ ≤ 1 := by
  rw [poissonFiber_eq]
  exact primitiveFiber_mass_le_one laws kind .poisson [rate] [] rfl rfl

theorem bernoulliFiber_mass_le_one (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (probability : ℝ) : bernoulliFiber kind probability Set.univ ≤ 1 := by
  rw [bernoulliFiber_eq]
  exact primitiveFiber_mass_le_one laws kind .bernoulli [probability] [] rfl rfl

theorem exponentialFiber_mass_le_one (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (rate : ℝ) : exponentialFiber kind rate Set.univ ≤ 1 := by
  rw [exponentialFiber_eq]
  exact primitiveFiber_mass_le_one laws kind .exponential [] [rate] rfl rfl

theorem betaFiber_mass_le_one (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (alpha beta : ℝ) : betaFiber kind alpha beta Set.univ ≤ 1 := by
  rw [betaFiber_eq]
  exact primitiveFiber_mass_le_one laws kind .beta [] [alpha, beta] rfl rfl

theorem gammaFiber_mass_le_one (laws : Determinize.Proof.Paper.PrimitiveLaws) (kind : Kind)
    (shape rate : ℝ) : gammaFiber kind shape rate Set.univ ≤ 1 := by
  rw [gammaFiber_eq]
  exact primitiveFiber_mass_le_one laws kind .gamma [shape] [rate] rfl rfl

def SampleMassLE (action : Action) : Prop :=
  ∀ site fiber continuation, action = .sample site fiber continuation → fiber Set.univ ≤ 1

theorem sampleMassLE_next (successor : Expr) :
    SampleMassLE (.next successor) := by
  intro site fiber continuation equality
  simp at equality

theorem sampleMassLE_stuck : SampleMassLE .stuck := by
  intro site fiber continuation equality
  simp at equality

theorem sampleMassLE_sample {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (mass : fiber Set.univ ≤ 1) :
    SampleMassLE (.sample site fiber continuation) := by
  intro actualSite actualFiber actualContinuation equality
  cases equality
  exact mass

theorem SampleMassLE.wrap {action : Action} (mass : SampleMassLE action)
    (context : Expr → Expr) : SampleMassLE (action.wrap context) := by
  intro site fiber continuation equality
  rcases Action.wrap_eq_sample equality with ⟨inner, innerEquality, _⟩
  exact mass site fiber inner innerEquality

theorem reduce_sample_mass_le_one
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (expression : Expr) (fiber : Measure ℝ) (continuation : ℝ → Expr)
    (equality : reduce expression = .sample site fiber continuation) :
    fiber Set.univ ≤ 1 := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression continuation with
  | h size ih =>
      have recurse (child : Expr) (smaller : sizeOf child < size)
          (inner : ℝ → Expr)
          (childEquality : reduce child = .sample site fiber inner) :
          fiber Set.univ ≤ 1 :=
        ih (sizeOf child) smaller child inner childEquality rfl
      have wrapped (child : Expr) (smaller : sizeOf child < size)
          (context : Expr → Expr)
          (wrappedEquality : (reduce child).wrap context =
            .sample site fiber continuation) :
          fiber Set.univ ≤ 1 := by
        rcases Action.wrap_eq_sample wrappedEquality with
          ⟨inner, childEquality, _⟩
        exact recurse child smaller inner childEquality
      cases expression with
      | bvar index => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | reject => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | discrete mode kind d =>
          simp only [reduce, Determinize.Spec.Paper.reduce] at equality
          cases equality
          rw [discreteFiber_eq]
          exact primitiveFiber_mass_le_one laws kind (.discrete d) [] [] rfl rfl
      | unit => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | bool value => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | real value => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | lam body => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | fix body => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | nil => simp [reduce, Determinize.Spec.Paper.reduce] at equality
      | pair left right =>
          simp only [reduce, Determinize.Spec.Paper.reduce] at equality
          split at equality
          · split at equality
            · simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | inl value =>
          simp only [reduce, Determinize.Spec.Paper.reduce] at equality
          split at equality
          · simp at equality
          · exact wrapped value (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | inr value =>
          simp only [reduce, Determinize.Spec.Paper.reduce] at equality
          split at equality
          · simp at equality
          · exact wrapped value (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | cons head tail =>
          simp only [reduce, Determinize.Spec.Paper.reduce] at equality
          split at equality
          · split at equality
            · simp at equality
            · exact wrapped tail (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped head (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | app function argument =>
          rw [reduce_app_eq] at equality
          split at equality
          · split at equality
            · split at equality <;> simp at equality
            · exact wrapped argument (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped function (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | fst pair =>
          rw [reduce_fst_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped pair (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | snd pair =>
          rw [reduce_snd_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped pair (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | matchSum scrutinee left right =>
          rw [reduce_matchSum_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped scrutinee (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | matchList scrutinee nilCase consCase =>
          rw [reduce_matchList_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped scrutinee (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | ite condition thenBranch elseBranch =>
          rw [reduce_ite_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped condition (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | letE value body =>
          rw [reduce_let_eq] at equality
          split at equality
          · simp at equality
          · exact wrapped value (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | neg body =>
          rw [reduce_neg_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped body (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | add left right =>
          rw [reduce_add_eq] at equality
          split at equality
          · split at equality
            · split at equality <;> simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | mul left right =>
          rw [reduce_mul_eq] at equality
          split at equality
          · split at equality
            · split at equality <;> simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | div left right =>
          rw [reduce_div_eq] at equality
          split at equality
          · split at equality
            · split at equality <;> simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | lt left right =>
          rw [reduce_lt_eq] at equality
          split at equality
          · split at equality
            · split at equality <;> simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | uniform mode kind left right =>
          rw [reduce_uniform_eq] at equality
          split at equality
          · split at equality
            · split at equality
              · simp only [Action.sample.injEq] at equality
                rw [← equality.2.1]
                exact uniformFiber_mass_le_one laws kind _ _
              · simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | gaussian mode kind left right =>
          rw [reduce_gaussian_eq] at equality
          split at equality
          · split at equality
            · split at equality
              · simp only [Action.sample.injEq] at equality
                rw [← equality.2.1]
                exact gaussianFiber_mass_le_one laws kind _ _
              · simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | beta mode kind left right =>
          rw [reduce_beta_eq] at equality
          split at equality
          · split at equality
            · split at equality
              · simp only [Action.sample.injEq] at equality
                rw [← equality.2.1]
                exact betaFiber_mass_le_one laws kind _ _
              · simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | gamma mode kind left right =>
          rw [reduce_gamma_eq] at equality
          split at equality
          · split at equality
            · split at equality
              · simp only [Action.sample.injEq] at equality
                rw [← equality.2.1]
                exact gammaFiber_mass_le_one laws kind _ _
              · simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | poisson mode kind body =>
          rw [reduce_poisson_eq] at equality
          split at equality
          · split at equality
            · simp only [Action.sample.injEq] at equality
              rw [← equality.2.1]
              exact poissonFiber_mass_le_one laws kind _
            · simp at equality
          · exact wrapped body (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | bernoulli mode kind body =>
          rw [reduce_bernoulli_eq] at equality
          split at equality
          · split at equality
            · simp only [Action.sample.injEq] at equality
              rw [← equality.2.1]
              exact bernoulliFiber_mass_le_one laws kind _
            · simp at equality
          · exact wrapped body (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | exponential mode kind body =>
          rw [reduce_exponential_eq] at equality
          split at equality
          · split at equality
            · simp only [Action.sample.injEq] at equality
              rw [← equality.2.1]
              exact exponentialFiber_mass_le_one laws kind _
            · simp at equality
          · exact wrapped body (by rw [← sizeEq]; simp_wf <;> omega) _ equality
theorem stepMeasure_mass_le_one
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (expression : Expr) : stepMeasure expression Set.univ ≤ 1 := by
  unfold stepMeasure
  cases equality : reduce expression with
  | next successor => simp [Determinize.Spec.Paper.Action.measure]
  | stuck => simp [Determinize.Spec.Paper.Action.measure]
  | sample site fiber continuation =>
      have continuationMeasurable := reduce_sample_continuation_measurable laws
        expression fiber continuation equality
      simp only [Determinize.Spec.Paper.Action.measure]
      rw [Measure.map_apply continuationMeasurable MeasurableSet.univ]
      simp only [Set.preimage_univ]
      exact reduce_sample_mass_le_one laws expression fiber continuation equality

theorem globalKernel_mass_le_one
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (expression : Expr) : globalKernel laws expression Set.univ ≤ 1 := by
  rw [globalKernel_apply]
  exact stepMeasure_mass_le_one laws expression

noncomputable def stepKernel
    (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    StepKernel where
  kernel := globalKernel laws
  kernel_eq_stepMeasure := globalKernel_apply laws
  kernel_sfinite := globalKernel_sfinite laws
  mass_le_one := globalKernel_mass_le_one laws
  sample_continuation_measurable := reduce_sample_continuation_measurable laws
  terminal_measurable := terminalFloatSet_measurable
  terminal_value_measurable := terminalFloatValue_measurable

def valueSet : Set Expr := {expression | expression.isValue = true}

theorem valueSet_measurable : MeasurableSet valueSet := by
  have equality : valueSet =
      Expr.skeleton ⁻¹' {skeleton | Expr.isValue skeleton = true} := by
    ext expression
    change expression.isValue = true ↔
      Expr.isValue expression.skeleton = true
    rw [isValue_eq_skeletonIsValue]
  rw [equality]
  exact measurable_skeleton
    ((measurable_of_countable Expr.isValue) (measurableSet_singleton true))

noncomputable def exactOutputKernelPack
    (stepKernel : StepKernel) : Nat →
      SFiniteKernel Expr ℝ
  | 0 => SFiniteKernel.piecewise
      terminalFloatSet_measurable
      (SFiniteKernel.deterministic
        terminalFloatValue terminalFloatValue_measurable)
      SFiniteKernel.zero
  | depth + 1 => by
      let previous := exactOutputKernelPack stepKernel depth
      letI := stepKernel.kernel_sfinite
      letI := previous.sfinite
      let composed : SFiniteKernel Expr ℝ :=
        ⟨previous.kernel ∘ₖ stepKernel.kernel, inferInstance⟩
      exact SFiniteKernel.piecewise
        valueSet_measurable SFiniteKernel.zero composed

noncomputable def exactOutputKernel
    (stepKernel : StepKernel) (depth : Nat) : Kernel Expr ℝ :=
  (exactOutputKernelPack stepKernel depth).kernel

theorem exactOutputKernel_sfinite
    (stepKernel : StepKernel) (depth : Nat) :
    IsSFiniteKernel (exactOutputKernel stepKernel depth) :=
  (exactOutputKernelPack stepKernel depth).sfinite

theorem exactOutputKernel_apply
    (stepKernel : StepKernel) (depth : Nat) (expression : Expr) :
    exactOutputKernel stepKernel depth expression =
      exactOutputMeasure stepKernel depth expression := by
  classical
  induction depth generalizing expression with
  | zero =>
      unfold exactOutputKernel exactOutputKernelPack
        SFiniteKernel.piecewise
        SFiniteKernel.deterministic
        SFiniteKernel.zero
      rw [Kernel.piecewise_apply]
      cases expression <;>
        try simp [exactOutputMeasure, terminalFloatSet,
          terminalFloatValue, Kernel.deterministic_apply]
  | succ depth ih =>
      unfold exactOutputKernel exactOutputKernelPack
        SFiniteKernel.piecewise
        SFiniteKernel.zero
      unfold exactOutputMeasure
      rw [Kernel.piecewise_apply]
      by_cases isValue : expression.isValue = true
      · simp [valueSet, isValue]
      · have notMember : expression ∉ valueSet := isValue
        rw [if_neg notMember]
        rw [if_neg isValue]
        simp only [Kernel.comp_apply]
        congr 1
        funext successor
        exact ih successor

noncomputable def nStepKernelPack
    (stepKernel : StepKernel) : Nat →
      SFiniteKernel Expr Expr
  | 0 => SFiniteKernel.deterministic id measurable_id
  | fuel + 1 => by
      let previous := nStepKernelPack stepKernel fuel
      letI := previous.sfinite
      letI := stepKernel.kernel_sfinite
      exact ⟨stepKernel.kernel ∘ₖ previous.kernel, inferInstance⟩

@[simp] theorem nStepKernelPack_zero_kernel
    (stepKernel : StepKernel) :
    (nStepKernelPack stepKernel 0).kernel = Kernel.id := by
  rfl

@[simp] theorem nStepKernelPack_succ_kernel
    (stepKernel : StepKernel) (fuel : Nat) :
    (nStepKernelPack stepKernel (fuel + 1)).kernel =
      stepKernel.kernel ∘ₖ (nStepKernelPack stepKernel fuel).kernel := by
  rfl

theorem nStepKernelPack_apply
    (stepKernel : StepKernel) (fuel : Nat) (expression : Expr) :
    (nStepKernelPack stepKernel fuel).kernel expression =
      nStepMeasure stepKernel fuel expression := by
  induction fuel generalizing expression with
  | zero =>
      simp [nStepKernelPack,
        SFiniteKernel.deterministic,
        Kernel.deterministic_apply, nStepMeasure]
  | succ fuel ih =>
      unfold nStepKernelPack nStepMeasure
      rw [Kernel.comp_apply]
      rw [ih expression]

theorem map_restrict_terminal_eq_bind_exactZero
    (stepKernel : StepKernel) (measure : Measure Expr) :
    (measure.restrict terminalFloatSet).map terminalFloatValue =
      measure.bind (exactOutputKernel stepKernel 0) := by
  ext set measurableSet
  rw [Measure.map_apply terminalFloatValue_measurable measurableSet,
    Measure.restrict_apply (terminalFloatValue_measurable measurableSet)]
  rw [Measure.bind_apply measurableSet
    (exactOutputKernel stepKernel 0).measurable.aemeasurable]
  have integrand : (fun expression => exactOutputKernel stepKernel 0 expression set) =
      terminalFloatSet.indicator
        (fun expression => (Measure.dirac (terminalFloatValue expression)) set) := by
    funext expression
    rw [exactOutputKernel_apply]
    cases expression <;> simp [exactOutputMeasure, terminalFloatSet,
      terminalFloatValue, Set.indicator]
  rw [integrand, lintegral_indicator terminalFloatSet_measurable]
  simp_rw [Measure.dirac_apply' _ measurableSet]
  have indicatorEquality :
      (fun expression => set.indicator (1 : ℝ → ENNReal)
        (terminalFloatValue expression)) =
        (terminalFloatValue ⁻¹' set).indicator (1 : Expr → ENNReal) := by
    funext expression
    rfl
  rw [indicatorEquality,
    lintegral_indicator_one (terminalFloatValue_measurable measurableSet),
    Measure.restrict_apply (terminalFloatValue_measurable measurableSet)]

theorem cumulativeOutputMeasure_eq_kernel
    (stepKernel : StepKernel) (fuel : Nat) (program : Expr) :
    cumulativeOutputMeasure stepKernel fuel program =
      ((exactOutputKernel stepKernel 0) ∘ₖ
        (nStepKernelPack stepKernel fuel).kernel) program := by
  unfold cumulativeOutputMeasure
  rw [← nStepKernelPack_apply stepKernel fuel program, Kernel.comp_apply,
    map_restrict_terminal_eq_bind_exactZero]

theorem stepKernel_eq_dirac_of_value
    (stepKernel : StepKernel) (expression : Expr)
    (isValue : expression.isValue = true) :
    stepKernel.kernel expression = Measure.dirac expression := by
  rw [stepKernel.kernel_eq_stepMeasure]
  unfold stepMeasure
  cases expression <;>
    simp_all [Expr.isValue, reduce, Determinize.Spec.Paper.Action.measure,
      Bool.and_eq_true]

theorem nStepKernel_commutes
    (stepKernel : StepKernel) (fuel : Nat) :
    stepKernel.kernel ∘ₖ (nStepKernelPack stepKernel fuel).kernel =
      (nStepKernelPack stepKernel fuel).kernel ∘ₖ stepKernel.kernel := by
  induction fuel with
  | zero =>
      rw [nStepKernelPack_zero_kernel]
      simp
  | succ fuel ih =>
      rw [nStepKernelPack_succ_kernel]
      calc
        stepKernel.kernel ∘ₖ
            (stepKernel.kernel ∘ₖ (nStepKernelPack stepKernel fuel).kernel) =
            stepKernel.kernel ∘ₖ
              ((nStepKernelPack stepKernel fuel).kernel ∘ₖ stepKernel.kernel) := by
          exact congrArg (fun kernel => stepKernel.kernel ∘ₖ kernel) ih
        _ = (stepKernel.kernel ∘ₖ (nStepKernelPack stepKernel fuel).kernel) ∘ₖ
              stepKernel.kernel := (Kernel.comp_assoc _ _ _).symm

theorem cumulativeKernel_succ
    (stepKernel : StepKernel) (fuel : Nat) :
    (exactOutputKernel stepKernel 0) ∘ₖ
        (nStepKernelPack stepKernel (fuel + 1)).kernel =
      ((exactOutputKernel stepKernel 0) ∘ₖ
        (nStepKernelPack stepKernel fuel).kernel) ∘ₖ stepKernel.kernel := by
  rw [nStepKernelPack_succ_kernel]
  rw [nStepKernel_commutes stepKernel fuel]
  exact (Kernel.comp_assoc _ _ _).symm

theorem exactOutputKernel_succ_apply_of_not_value
    (stepKernel : StepKernel) (depth : Nat) (expression : Expr)
    (notValue : expression.isValue ≠ true) :
    exactOutputKernel stepKernel (depth + 1) expression =
      ((exactOutputKernel stepKernel depth) ∘ₖ stepKernel.kernel) expression := by
  classical
  change
    (Kernel.piecewise valueSet_measurable 0
      ((exactOutputKernelPack stepKernel depth).kernel ∘ₖ stepKernel.kernel)) expression =
    (((exactOutputKernelPack stepKernel depth).kernel ∘ₖ stepKernel.kernel) expression)
  rw [Kernel.piecewise_apply]
  simp [valueSet, notValue]

theorem exactOutputKernel_succ_apply_of_value
    (stepKernel : StepKernel) (depth : Nat) (expression : Expr)
    (isValue : expression.isValue = true) :
    exactOutputKernel stepKernel (depth + 1) expression = 0 := by
  classical
  change
    (Kernel.piecewise valueSet_measurable 0
      ((exactOutputKernelPack stepKernel depth).kernel ∘ₖ stepKernel.kernel)) expression = 0
  rw [Kernel.piecewise_apply]
  simp [valueSet, isValue]

theorem exactOutputKernel_zero_apply_of_not_value
    (stepKernel : StepKernel) (expression : Expr)
    (notValue : expression.isValue ≠ true) :
    exactOutputKernel stepKernel 0 expression = 0 := by
  rw [exactOutputKernel_apply]
  cases expression <;> simp_all [Expr.isValue, exactOutputMeasure]

theorem exactOutputKernel_zero_comp_surface
    (stepKernel : StepKernel) :
    exactOutputKernel stepKernel 0 ∘ₖ stepKernel.kernel =
      exactOutputKernel stepKernel 0 + exactOutputKernel stepKernel 1 := by
  apply Kernel.ext
  intro expression
  by_cases isValue : expression.isValue = true
  · rw [Kernel.comp_apply, stepKernel_eq_dirac_of_value stepKernel expression isValue,
      Measure.dirac_bind (exactOutputKernel stepKernel 0).measurable]
    have successorZero : exactOutputKernel stepKernel 1 expression = 0 := by
      simpa using exactOutputKernel_succ_apply_of_value stepKernel 0 expression isValue
    change exactOutputKernel stepKernel 0 expression =
      exactOutputKernel stepKernel 0 expression + exactOutputKernel stepKernel 1 expression
    rw [successorZero]
    simp
  · have successorZero : exactOutputKernel stepKernel 1 expression =
        (exactOutputKernel stepKernel 0 ∘ₖ stepKernel.kernel) expression := by
      simpa using exactOutputKernel_succ_apply_of_not_value stepKernel 0 expression isValue
    change (exactOutputKernel stepKernel 0 ∘ₖ stepKernel.kernel) expression =
      exactOutputKernel stepKernel 0 expression + exactOutputKernel stepKernel 1 expression
    rw [successorZero, exactOutputKernel_zero_apply_of_not_value
      stepKernel expression isValue]
    simp

theorem exactOutputKernel_succ_comp_surface
    (stepKernel : StepKernel) (depth : Nat) :
    exactOutputKernel stepKernel (depth + 1) ∘ₖ stepKernel.kernel =
      exactOutputKernel stepKernel (depth + 2) := by
  apply Kernel.ext
  intro expression
  by_cases isValue : expression.isValue = true
  · rw [Kernel.comp_apply, stepKernel_eq_dirac_of_value stepKernel expression isValue,
      Measure.dirac_bind (exactOutputKernel stepKernel (depth + 1)).measurable,
      exactOutputKernel_succ_apply_of_value stepKernel depth expression isValue,
      exactOutputKernel_succ_apply_of_value stepKernel (depth + 1) expression isValue]
  · simpa [Nat.add_assoc] using
      (exactOutputKernel_succ_apply_of_not_value stepKernel (depth + 1)
        expression isValue).symm

theorem finsetSum_comp_kernel {α β γ ι : Type*}
    [MeasurableSpace α] [MeasurableSpace β] [MeasurableSpace γ]
    (indices : Finset ι) (kernels : ι → Kernel β γ) (kernel : Kernel α β) :
    (∑ index ∈ indices, kernels index) ∘ₖ kernel =
      ∑ index ∈ indices, kernels index ∘ₖ kernel := by
  classical
  induction indices using Finset.induction with
  | empty => simp
  | @insert index indices absent ih =>
      simp [Finset.sum_insert absent, Kernel.comp_add_left, ih]

theorem cumulativeKernel_eq_finsetSum
    (stepKernel : StepKernel) (fuel : Nat) :
    exactOutputKernel stepKernel 0 ∘ₖ (nStepKernelPack stepKernel fuel).kernel =
      ∑ depth ∈ Finset.range (fuel + 1), exactOutputKernel stepKernel depth := by
  induction fuel with
  | zero =>
      rw [nStepKernelPack_zero_kernel]
      simp
  | succ fuel ih =>
      rw [cumulativeKernel_succ, ih,
        finsetSum_comp_kernel, Finset.sum_range_succ']
      rw [exactOutputKernel_zero_comp_surface]
      simp_rw [exactOutputKernel_succ_comp_surface]
      rw [Finset.sum_range_succ']
      have shifted :
          (∑ depth ∈ Finset.range (fuel + 1),
              exactOutputKernel stepKernel (depth + 1)) =
            exactOutputKernel stepKernel 1 +
              ∑ depth ∈ Finset.range fuel,
                exactOutputKernel stepKernel (depth + 2) := by
        rw [Finset.sum_range_succ']
        simp only [Nat.zero_add, Nat.add_assoc]
        ac_rfl
      rw [shifted]
      ac_rfl

theorem cumulativeOutputMeasure_eq_finsetSum
    (stepKernel : StepKernel) (fuel : Nat) (program : Expr) :
    cumulativeOutputMeasure stepKernel fuel program =
      ∑ depth ∈ Finset.range (fuel + 1),
        exactOutputMeasure stepKernel depth program := by
  rw [cumulativeOutputMeasure_eq_kernel]
  have kernelEquality := cumulativeKernel_eq_finsetSum stepKernel fuel
  have fiberEquality := congrArg (fun kernel : Kernel Expr ℝ => kernel program)
    kernelEquality
  rw [fiberEquality]
  simp only [FunLike.coe_sum, Finset.sum_apply]
  apply Finset.sum_congr rfl
  intro depth _
  exact exactOutputKernel_apply stepKernel depth program

theorem measure_sum_eq_iSup_range_succ {α : Type*} [MeasurableSpace α]
    (measures : Nat → Measure α) :
    Measure.sum measures =
      ⨆ fuel, ∑ depth ∈ Finset.range (fuel + 1), measures depth := by
  apply le_antisymm
  · rw [Measure.le_iff]
    intro set measurableSet
    rw [Measure.sum_apply measures measurableSet, ENNReal.tsum_eq_iSup_nat]
    refine iSup_le fun fuel => ?_
    have finiteLe :
        (∑ depth ∈ Finset.range (fuel + 1), measures depth) ≤
          ⨆ fuel, ∑ depth ∈ Finset.range (fuel + 1), measures depth :=
      le_iSup (fun fuel => ∑ depth ∈ Finset.range (fuel + 1), measures depth) fuel
    have applyLe := Measure.le_iff.mp finiteLe set measurableSet
    have scalarLe :
        (∑ depth ∈ Finset.range fuel, measures depth set) ≤
          (∑ depth ∈ Finset.range (fuel + 1), measures depth set) := by
      rw [Finset.sum_range_succ]
      exact le_add_right le_rfl
    exact scalarLe.trans <| by
      simpa [Measure.finsetSum_apply] using applyLe
  · refine iSup_le fun fuel => ?_
    rw [Measure.le_iff]
    intro set measurableSet
    rw [Measure.finsetSum_apply, Measure.sum_apply measures measurableSet,
      ENNReal.tsum_eq_iSup_nat]
    exact le_iSup (fun fuel => ∑ depth ∈ Finset.range fuel, measures depth set)
      (fuel + 1)

theorem exactDepthConstruction (stepKernel : StepKernel) (program : Expr) :
    bigStepMeasure stepKernel program =
      Measure.sum (fun depth => exactOutputMeasure stepKernel depth program) := by
  unfold bigStepMeasure
  calc
    (⨆ fuel, cumulativeOutputMeasure stepKernel fuel program) =
        ⨆ fuel, ∑ depth ∈ Finset.range (fuel + 1),
          exactOutputMeasure stepKernel depth program := by
      apply iSup_congr
      intro fuel
      exact cumulativeOutputMeasure_eq_finsetSum stepKernel fuel program
    _ = Measure.sum (fun depth => exactOutputMeasure stepKernel depth program) :=
      (measure_sum_eq_iSup_range_succ
        (fun depth => exactOutputMeasure stepKernel depth program)).symm

end MeasurableActionFamily

end

end Determinize.Proof.Paper
