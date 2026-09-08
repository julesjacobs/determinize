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
open Determinize.Statement.Paper

noncomputable section

attribute [local simp] Determinize.Statement.Paper.reduce

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

/- A mutually recursive presentation removes the nested `List Skeleton`
recursion that Lean's declaration-site `Countable` deriver cannot see. -/
mutual
  inductive SkeletonCode where
    | bvar (index : Nat) | unit | bool (value : Bool) | real (mode : Mode)
    | lam (body : SkeletonCode)
    | fix (body : SkeletonCode)
    | app (function argument : SkeletonCode)
    | pair (left right : SkeletonCode) | fst (pair : SkeletonCode)
    | snd (pair : SkeletonCode) | inl (value : SkeletonCode)
    | inr (value : SkeletonCode)
    | matchSum (scrutinee left right : SkeletonCode)
    | nil | cons (head tail : SkeletonCode)
    | matchList (scrutinee nilCase consCase : SkeletonCode)
    | ite (condition thenBranch elseBranch : SkeletonCode)
    | letE (value body : SkeletonCode)
    | promote (body : SkeletonCode) | neg (mode : Mode) (body : SkeletonCode)
    | add (mode : Mode) (left right : SkeletonCode)
    | mul (mode : Mode) (left right : SkeletonCode)
    | div (mode : Mode) (left right : SkeletonCode) | lt (left right : SkeletonCode)
    | sample (mode : Mode) (op : Tag)
        (affineArgs generalArgs : SkeletonCodeList)
  deriving Countable

  inductive SkeletonCodeList where
    | nil
    | cons (head : SkeletonCode) (tail : SkeletonCodeList)
  deriving Countable
end

mutual
  def SkeletonCode.decode : SkeletonCode → Skeleton
    | .bvar index => .bvar index | .unit => .unit | .bool value => .bool value
    | .real mode => .real mode | .lam body => .lam body.decode
    | .fix body => .fix body.decode
    | .app function argument => .app function.decode argument.decode
    | .pair left right => .pair left.decode right.decode
    | .fst pair => .fst pair.decode | .snd pair => .snd pair.decode
    | .inl value => .inl value.decode
    | .inr value => .inr value.decode
    | .matchSum scrutinee left right =>
        .matchSum scrutinee.decode left.decode right.decode
    | .nil => .nil | .cons head tail => .cons head.decode tail.decode
    | .matchList scrutinee nilCase consCase =>
        .matchList scrutinee.decode nilCase.decode consCase.decode
    | .ite condition thenBranch elseBranch =>
        .ite condition.decode thenBranch.decode elseBranch.decode
    | .letE value body => .letE value.decode body.decode
    | .promote body => .promote body.decode | .neg mode body => .neg mode body.decode
    | .add mode left right => .add mode left.decode right.decode
    | .mul mode left right => .mul mode left.decode right.decode
    | .div mode left right => .div mode left.decode right.decode
    | .lt left right => .lt left.decode right.decode
    | .sample mode op affine general => .sample mode op affine.decode general.decode

  def SkeletonCodeList.decode : SkeletonCodeList → List Skeleton
    | .nil => []
    | .cons head tail => head.decode :: tail.decode
end

mutual
  def encodeSkeletonCode : Skeleton → SkeletonCode
    | .bvar index => .bvar index | .unit => .unit | .bool value => .bool value
    | .real mode => .real mode | .lam body => .lam (encodeSkeletonCode body)
    | .fix body => .fix (encodeSkeletonCode body)
    | .app function argument =>
        .app (encodeSkeletonCode function) (encodeSkeletonCode argument)
    | .pair left right => .pair (encodeSkeletonCode left) (encodeSkeletonCode right)
    | .fst pair => .fst (encodeSkeletonCode pair)
    | .snd pair => .snd (encodeSkeletonCode pair)
    | .inl value => .inl (encodeSkeletonCode value)
    | .inr value => .inr (encodeSkeletonCode value)
    | .matchSum scrutinee left right =>
        .matchSum (encodeSkeletonCode scrutinee) (encodeSkeletonCode left)
          (encodeSkeletonCode right)
    | .nil => .nil
    | .cons head tail => .cons (encodeSkeletonCode head) (encodeSkeletonCode tail)
    | .matchList scrutinee nilCase consCase =>
        .matchList (encodeSkeletonCode scrutinee) (encodeSkeletonCode nilCase)
          (encodeSkeletonCode consCase)
    | .ite condition thenBranch elseBranch =>
        .ite (encodeSkeletonCode condition) (encodeSkeletonCode thenBranch)
          (encodeSkeletonCode elseBranch)
    | .letE value body =>
        .letE (encodeSkeletonCode value) (encodeSkeletonCode body)
    | .promote body => .promote (encodeSkeletonCode body)
    | .neg mode body => .neg mode (encodeSkeletonCode body)
    | .add mode left right => .add mode (encodeSkeletonCode left) (encodeSkeletonCode right)
    | .mul mode left right => .mul mode (encodeSkeletonCode left) (encodeSkeletonCode right)
    | .div mode left right => .div mode (encodeSkeletonCode left) (encodeSkeletonCode right)
    | .lt left right => .lt (encodeSkeletonCode left) (encodeSkeletonCode right)
    | .sample mode op affine general =>
        .sample mode op (encodeSkeletonCodeList affine) (encodeSkeletonCodeList general)

  def encodeSkeletonCodeList : List Skeleton → SkeletonCodeList
    | [] => .nil
    | head :: tail => .cons (encodeSkeletonCode head) (encodeSkeletonCodeList tail)
end

mutual
  theorem SkeletonCode.decode_encode (skeleton : Skeleton) :
      (encodeSkeletonCode skeleton).decode = skeleton := by
    cases skeleton <;> simp [encodeSkeletonCode, SkeletonCode.decode,
      SkeletonCode.decode_encode, SkeletonCodeList.decode_encode]

  theorem SkeletonCodeList.decode_encode (skeletons : List Skeleton) :
      (encodeSkeletonCodeList skeletons).decode = skeletons := by
    cases skeletons <;> simp [encodeSkeletonCodeList, SkeletonCodeList.decode,
      SkeletonCode.decode_encode, SkeletonCodeList.decode_encode]
end

mutual
  def SkeletonCode.zeroFill : SkeletonCode → Expr
    | .bvar index => .bvar index
    | .unit => .unit
    | .bool value => .bool value
    | .real mode => .real mode 0
    | .lam body => .lam body.zeroFill
    | .fix body => .fix body.zeroFill
    | .app function argument => .app function.zeroFill argument.zeroFill
    | .pair left right => .pair left.zeroFill right.zeroFill
    | .fst pair => .fst pair.zeroFill
    | .snd pair => .snd pair.zeroFill
    | .inl value => .inl value.zeroFill
    | .inr value => .inr value.zeroFill
    | .matchSum scrutinee left right =>
        .matchSum scrutinee.zeroFill left.zeroFill right.zeroFill
    | .nil => .nil
    | .cons head tail => .cons head.zeroFill tail.zeroFill
    | .matchList scrutinee nilCase consCase =>
        .matchList scrutinee.zeroFill nilCase.zeroFill consCase.zeroFill
    | .ite condition thenBranch elseBranch =>
        .ite condition.zeroFill thenBranch.zeroFill elseBranch.zeroFill
    | .letE value body => .letE value.zeroFill body.zeroFill
    | .promote body => .promote body.zeroFill
    | .neg mode body => .neg mode body.zeroFill
    | .add mode left right => .add mode left.zeroFill right.zeroFill
    | .mul mode left right => .mul mode left.zeroFill right.zeroFill
    | .div mode left right => .div mode left.zeroFill right.zeroFill
    | .lt left right => .lt left.zeroFill right.zeroFill
    | .sample mode op affine general =>
        .sample mode op affine.zeroFill general.zeroFill

  def SkeletonCodeList.zeroFill : SkeletonCodeList → List Expr
    | .nil => []
    | .cons head tail => head.zeroFill :: tail.zeroFill
end

mutual
  theorem SkeletonCode.zeroFill_skeleton (code : SkeletonCode) :
      code.zeroFill.skeleton = code.decode := by
    cases code <;> simp [SkeletonCode.zeroFill, SkeletonCodeList.zeroFill,
      SkeletonCode.decode, SkeletonCodeList.decode, Expr.skeleton,
      SkeletonCode.zeroFill_skeleton, SkeletonCodeList.zeroFill_skeleton]

  theorem SkeletonCodeList.zeroFill_skeleton (codes : SkeletonCodeList) :
      codes.zeroFill.map Expr.skeleton = codes.decode := by
    cases codes <;> simp [SkeletonCode.zeroFill, SkeletonCodeList.zeroFill,
      SkeletonCode.decode, SkeletonCodeList.decode,
      SkeletonCode.zeroFill_skeleton, SkeletonCodeList.zeroFill_skeleton]
end

def zeroFill (skeleton : Skeleton) : Expr :=
  (encodeSkeletonCode skeleton).zeroFill

@[simp] theorem zeroFill_skeleton (skeleton : Skeleton) :
    (zeroFill skeleton).skeleton = skeleton := by
  rw [zeroFill, SkeletonCode.zeroFill_skeleton, SkeletonCode.decode_encode]

local instance : Countable Skeleton :=
  (show Function.Surjective SkeletonCode.decode from
    fun skeleton => ⟨encodeSkeletonCode skeleton, SkeletonCode.decode_encode skeleton⟩).countable

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
  | bvar | unit | bool | real | nil =>
      simp [Expr.realCoordinates, Expr.skeleton, Expr.realArity]
  | lam body | fix body | fst body | snd body | inl body
  | inr body | promote body | neg _ body =>
      simpa [Expr.realCoordinates, Expr.skeleton, Expr.realArity] using
        realCoordinates_length body
  | app left right | pair left right | cons left right | add _ left right
  | mul _ left right | div _ left right | lt left right | letE left right =>
      simpa [Expr.realCoordinates, Expr.skeleton, Expr.realArity] using congrArg₂ (· + ·)
          (realCoordinates_length left) (realCoordinates_length right)
  | matchSum first second third | matchList first second third
  | ite first second third =>
      simpa [Expr.realCoordinates, Expr.skeleton, Expr.realArity,
        Nat.add_assoc] using congrArg₂ (· + ·)
          (realCoordinates_length first)
          (congrArg₂ (· + ·) (realCoordinates_length second)
            (realCoordinates_length third))
  | sample _ _ affine general =>
      simp only [Expr.realCoordinates, Expr.skeleton, Expr.realArity,
        List.length_append, List.length_flatMap]
      have affineLengths :
          affine.map (fun item => item.realCoordinates.length) =
            affine.map (fun item => item.skeleton.realArity) := by
        apply List.map_congr_left
        intro item member
        exact realCoordinates_length item
      have generalLengths :
          general.map (fun item => item.realCoordinates.length) =
            general.map (fun item => item.skeleton.realArity) := by
        apply List.map_congr_left
        intro item member
        exact realCoordinates_length item
      rw [affineLengths, generalLengths]
      simp [Function.comp_def]
termination_by sizeOf expression
decreasing_by
  all_goals decreasing_trivial

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

def realLiteral (mode : Mode) :
    MeasurableFamily ℝ (fun value => Expr.real mode value) where
  skeleton := .real mode
  skeleton_eq _ := by simp [Expr.skeleton]
  coordinate_count _ := by simp [Expr.realCoordinates, Expr.skeleton, Expr.realArity]
  coordinate_measurable index := by
    cases index with
    | zero =>
        have functionEq :
            (fun value : ℝ => (Expr.real mode value).realCoordinates.getD 0 0) = id := by
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

def add {α : Type*} [MeasurableSpace α] (mode : Mode)
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .add mode (left parameter) (right parameter)) :=
  combine leftFamily rightFamily (.add mode) (.add mode)
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def mul {α : Type*} [MeasurableSpace α] (mode : Mode)
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .mul mode (left parameter) (right parameter)) :=
  combine leftFamily rightFamily (.mul mode) (.mul mode)
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def div {α : Type*} [MeasurableSpace α] (mode : Mode)
    {left right : α → Expr} (leftFamily : MeasurableFamily α left)
    (rightFamily : MeasurableFamily α right) :
    MeasurableFamily α (fun parameter => .div mode (left parameter) (right parameter)) :=
  combine leftFamily rightFamily (.div mode) (.div mode)
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

def promote {α : Type*} [MeasurableSpace α] {body : α → Expr}
    (family : MeasurableFamily α body) :
    MeasurableFamily α (fun parameter => .promote (body parameter)) :=
  wrap family .promote .promote
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])
    (by intros; simp [Expr.realArity])

def neg {α : Type*} [MeasurableSpace α] (mode : Mode) {body : α → Expr}
    (family : MeasurableFamily α body) :
    MeasurableFamily α (fun parameter => .neg mode (body parameter)) :=
  wrap family (.neg mode) (.neg mode)
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
  | .inl body | .inr body | .promote body | .neg _ body => body
  | .app left _ | .pair left _ | .cons left _ | .add _ left _
  | .mul _ left _ | .div _ left _ | .lt left _ => left
  | .matchSum first _ _ | .matchList first _ _ | .ite first _ _ => first
  | .letE first _ => first
  | expression => expression

def secondChild : Expr → Expr
  | .app _ right | .pair _ right | .cons _ right | .add _ _ right
  | .mul _ _ right | .div _ _ right | .lt _ right => right
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
  | .inl body | .inr body | .promote body | .neg _ body => body
  | .app left _ | .pair left _ | .cons left _ | .add _ left _
  | .mul _ left _ | .div _ left _ | .lt left _ => left
  | .matchSum first _ _ | .matchList first _ _ | .ite first _ _ => first
  | .letE first _ => first
  | skeleton => skeleton

def secondChild : Skeleton → Skeleton
  | .app _ right | .pair _ right | .cons _ right | .add _ _ right
  | .mul _ _ right | .div _ _ right | .lt _ right => right
  | .matchSum _ second _ | .matchList _ second _ | .ite _ second _ => second
  | .letE _ second => second
  | skeleton => skeleton

def thirdChild : Skeleton → Skeleton
  | .matchSum _ _ third | .matchList _ _ third | .ite _ _ third => third
  | skeleton => skeleton

def secondOffset : Skeleton → Nat
  | .app first _ | .pair first _ | .cons first _ | .add _ first _
  | .mul _ first _ | .div _ first _ | .lt first _ => first.realArity
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
      lt function argument =>
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

theorem flatRealCoordinates_length (expressions : List Expr) :
    (expressions.flatMap Expr.realCoordinates).length =
      (expressions.map (Expr.realArity ∘ Expr.skeleton)).sum := by
  rw [List.length_flatMap]
  apply congrArg List.sum
  apply List.map_congr_left
  intro expression member
  exact realCoordinates_length expression

theorem flatMap_decompose_getElem {α β : Type*} (function : α → List β)
    (values : List α) (index : Nat) (inBounds : index < values.length) :
    ∃ front suffix,
      values.flatMap function = front ++ function values[index] ++ suffix ∧
      front.length = ((values.take index).map (List.length ∘ function)).sum := by
  induction values generalizing index with
  | nil => simp at inBounds
  | cons head tail ih =>
      cases index with
      | zero => exact ⟨[], tail.flatMap function, by simp⟩
      | succ index =>
          have tailBounds : index < tail.length := by simpa using inBounds
          rcases ih index tailBounds with ⟨front, suffix, equality, frontLength⟩
          refine ⟨function head ++ front, suffix, ?_, ?_⟩
          · simp only [List.flatMap_cons, List.getElem_cons_succ]
            rw [equality]
            simp [List.append_assoc]
          · simp [frontLength]

def listElement (expressions : List Expr) (index : Nat) : Expr :=
  expressions.getD index .unit

theorem listElement_eq_getElem {expressions : List Expr} {index : Nat}
    (inBounds : index < expressions.length) :
    listElement expressions index = expressions[index] := by
  simp [listElement, List.getD, inBounds]

theorem map_skeleton_length {expressions : List Expr} {skeletons : List Skeleton}
    (equality : expressions.map Expr.skeleton = skeletons) :
    expressions.length = skeletons.length := by
  simpa using congrArg List.length equality

theorem listElement_skeleton {expressions : List Expr} {skeletons : List Skeleton}
    (equality : expressions.map Expr.skeleton = skeletons) {index : Nat}
    (inBounds : index < skeletons.length) :
    (listElement expressions index).skeleton = skeletons[index] := by
  have actualBounds : index < expressions.length := by
    rwa [map_skeleton_length equality]
  rw [listElement_eq_getElem actualBounds]
  have elementEquality := congrArg (fun values : List Skeleton => values[index]?) equality
  simpa [List.getElem?_map, actualBounds, inBounds] using elementEquality

theorem prefix_coordinate_length {expressions : List Expr} {skeletons : List Skeleton}
    (equality : expressions.map Expr.skeleton = skeletons) (index : Nat) :
    ((expressions.take index).map (List.length ∘ Expr.realCoordinates)).sum =
      ((skeletons.take index).map Expr.realArity).sum := by
  have mapped := congrArg (List.take index) equality
  rw [← List.map_take] at mapped
  rw [← mapped]
  apply congrArg List.sum
  simp only [List.map_map, Function.comp_apply]
  apply List.map_congr_left
  intro expression member
  exact realCoordinates_length expression

def replaceListElement (expressions : List Expr) (index : Nat)
    (replacement : Expr) : List Expr :=
  expressions.take index ++ replacement :: expressions.drop (index + 1)

theorem replaceListElement_skeletons (expressions : List Expr) (index : Nat)
    (replacement : Expr) :
    (replaceListElement expressions index replacement).map Expr.skeleton =
      (expressions.map Expr.skeleton).take index ++ replacement.skeleton ::
        (expressions.map Expr.skeleton).drop (index + 1) := by
  simp [replaceListElement]

theorem replaceListElement_coordinates (expressions : List Expr) (index : Nat)
    (replacement : Expr) :
    (replaceListElement expressions index replacement).flatMap Expr.realCoordinates =
      (expressions.take index).flatMap Expr.realCoordinates ++
        replacement.realCoordinates ++
        (expressions.drop (index + 1)).flatMap Expr.realCoordinates := by
  simp [replaceListElement, List.flatMap_append, List.append_assoc]

theorem list_coordinates_take_element_drop (expressions : List Expr) (index : Nat)
    (inBounds : index < expressions.length) :
    expressions.flatMap Expr.realCoordinates =
      (expressions.take index).flatMap Expr.realCoordinates ++
        expressions[index].realCoordinates ++
        (expressions.drop (index + 1)).flatMap Expr.realCoordinates := by
  induction expressions generalizing index with
  | nil => simp at inBounds
  | cons head tail ih =>
      cases index with
      | zero => simp [List.append_assoc]
      | succ index =>
          have tailBounds : index < tail.length := by simpa using inBounds
          simp only [List.flatMap_cons, List.take_succ_cons, List.getElem_cons_succ,
            List.drop_succ_cons]
          rw [ih index tailBounds]
          simp [List.append_assoc]

def MeasurableFamily.listElement {α : Type*} [MeasurableSpace α]
    {parent : α → Expr} (parentFamily : MeasurableFamily α parent)
    (expressions : α → List Expr) (skeletons : List Skeleton)
    (skeletonEquality : ∀ parameter,
      (expressions parameter).map Expr.skeleton = skeletons)
    (blockOffset : Nat)
    (blockDecompose : ∀ parameter, ∃ before after,
      (parent parameter).realCoordinates =
        before ++ (expressions parameter).flatMap Expr.realCoordinates ++ after ∧
      before.length = blockOffset)
    (index : Nat) (inBounds : index < skeletons.length) :
    MeasurableFamily α (fun parameter => listElement (expressions parameter) index) := by
  let childSkeleton := skeletons[index]
  let childOffset := blockOffset +
    ((skeletons.take index).map Expr.realArity).sum
  apply parentFamily.extractContiguous childSkeleton childOffset
  · intro parameter
    exact listElement_skeleton (skeletonEquality parameter) inBounds
  · intro parameter coordinate coordinateBounds
    have actualBounds : index < (expressions parameter).length := by
      rw [map_skeleton_length (skeletonEquality parameter)]
      exact inBounds
    rw [listElement_eq_getElem actualBounds]
    rcases blockDecompose parameter with ⟨before, after, parentEquality, beforeLength⟩
    rcases flatMap_decompose_getElem Expr.realCoordinates (expressions parameter)
      index actualBounds with ⟨front, suffix, flatEquality, frontLength⟩
    have frontLength' : front.length =
        ((skeletons.take index).map Expr.realArity).sum :=
      frontLength.trans (prefix_coordinate_length (skeletonEquality parameter) index)
    rw [parentEquality, flatEquality]
    have coordinateInBounds : coordinate <
        ((expressions parameter)[index]).realCoordinates.length := by
      rw [realCoordinates_length]
      have elementSkeleton := listElement_skeleton (skeletonEquality parameter) inBounds
      rw [listElement_eq_getElem actualBounds] at elementSkeleton
      rw [elementSkeleton]
      exact coordinateBounds
    have extraction := list_getD_append_middle
      (before ++ front) ((expressions parameter)[index].realCoordinates)
      (suffix ++ after) coordinate 0 coordinateInBounds
    rw [List.length_append, beforeLength, frontLength'] at extraction
    simpa [childOffset, List.append_assoc, Nat.add_assoc] using extraction.symm

/-- A parameter-varying list of expressions with fixed element skeletons and
measurable coordinates in the flattened traversal order. -/
structure MeasurableExprListFamily (α : Type*) [MeasurableSpace α]
    (expressions : α → List Expr) where
  skeletons : List Skeleton
  skeleton_eq : ∀ parameter, (expressions parameter).map Expr.skeleton = skeletons
  coordinate_measurable : ∀ index : Nat, Measurable fun parameter =>
    ((expressions parameter).flatMap Expr.realCoordinates).getD index 0

namespace MeasurableExprListFamily

theorem coordinate_count {α : Type*} [MeasurableSpace α]
    {expressions : α → List Expr} (family : MeasurableExprListFamily α expressions)
    (parameter : α) :
    ((expressions parameter).flatMap Expr.realCoordinates).length =
      (family.skeletons.map Expr.realArity).sum := by
  rw [flatRealCoordinates_length]
  have fixed := congrArg (List.map Expr.realArity) (family.skeleton_eq parameter)
  simpa only [List.map_map, Function.comp_apply] using congrArg List.sum fixed

theorem measurable_realCoordinates {α : Type*} [MeasurableSpace α]
    {expressions : α → List Expr} (family : MeasurableExprListFamily α expressions) :
    Measurable fun parameter =>
      (⟨(expressions parameter).flatMap Expr.realCoordinates⟩ : RealCoordinates) := by
  apply RealCoordinates.measurable_of_length_getD
  · have constantLength :
        (fun parameter => ((expressions parameter).flatMap Expr.realCoordinates).length) =
          fun _ => (family.skeletons.map Expr.realArity).sum :=
      funext family.coordinate_count
    change Measurable fun parameter =>
      ((expressions parameter).flatMap Expr.realCoordinates).length
    rw [constantLength]
    exact measurable_const
  · exact family.coordinate_measurable

def comp {α β : Type*} [MeasurableSpace α] [MeasurableSpace β]
    {expressions : α → List Expr} (family : MeasurableExprListFamily α expressions)
    (function : β → α) (measurableFunction : Measurable function) :
    MeasurableExprListFamily β (expressions ∘ function) where
  skeletons := family.skeletons
  skeleton_eq parameter := family.skeleton_eq (function parameter)
  coordinate_measurable index :=
    (family.coordinate_measurable index).comp measurableFunction

def take {α : Type*} [MeasurableSpace α] {expressions : α → List Expr}
    (family : MeasurableExprListFamily α expressions) (count : Nat) :
    MeasurableExprListFamily α (fun parameter => (expressions parameter).take count) where
  skeletons := family.skeletons.take count
  skeleton_eq parameter := by
    simpa only [List.map_take] using congrArg (List.take count) (family.skeleton_eq parameter)
  coordinate_measurable index := by
    let arity := ((family.skeletons.take count).map Expr.realArity).sum
    by_cases inBounds : index < arity
    · have functionEquality :
          (fun parameter =>
            (((expressions parameter).take count).flatMap Expr.realCoordinates).getD index 0) =
          fun parameter =>
            ((expressions parameter).flatMap Expr.realCoordinates).getD index 0 := by
        funext parameter
        have decomposition := congrArg (List.flatMap Expr.realCoordinates)
          (List.take_append_drop count (expressions parameter))
        simp only [List.flatMap_append] at decomposition
        have prefixLength :
            (((expressions parameter).take count).flatMap Expr.realCoordinates).length =
              arity := by
          rw [flatRealCoordinates_length]
          have fixed := congrArg (List.take count) (family.skeleton_eq parameter)
          have sums := congrArg (fun skeletons =>
            (skeletons.map Expr.realArity).sum) fixed
          rw [← List.map_take] at sums
          simpa only [List.map_map, Function.comp_apply] using sums
        rw [← decomposition, list_getD_append_left]
        rwa [prefixLength]
      rw [functionEquality]
      exact family.coordinate_measurable index
    · have functionEquality :
          (fun parameter =>
            (((expressions parameter).take count).flatMap Expr.realCoordinates).getD index 0) =
            fun _ => 0 := by
        funext parameter
        rw [List.getD_eq_getElem?_getD, List.getElem?_eq_none]
        · rfl
        · rw [flatRealCoordinates_length]
          have fixed := congrArg (List.take count) (family.skeleton_eq parameter)
          have sums := congrArg (fun skeletons =>
            (skeletons.map Expr.realArity).sum) fixed
          rw [← List.map_take] at sums
          rw [show (List.map (Expr.realArity ∘ Expr.skeleton)
              (List.take count (expressions parameter))).sum = arity by
            simpa only [List.map_map, Function.comp_apply] using sums]
          exact Nat.le_of_not_gt inBounds
      rw [functionEquality]
      exact measurable_const

def drop {α : Type*} [MeasurableSpace α] {expressions : α → List Expr}
    (family : MeasurableExprListFamily α expressions) (count : Nat) :
    MeasurableExprListFamily α (fun parameter => (expressions parameter).drop count) where
  skeletons := family.skeletons.drop count
  skeleton_eq parameter := by
    simpa only [List.map_drop] using congrArg (List.drop count) (family.skeleton_eq parameter)
  coordinate_measurable index := by
    let offset := ((family.skeletons.take count).map Expr.realArity).sum
    have functionEquality :
        (fun parameter =>
          (((expressions parameter).drop count).flatMap Expr.realCoordinates).getD index 0) =
        fun parameter =>
          ((expressions parameter).flatMap Expr.realCoordinates).getD (offset + index) 0 := by
      funext parameter
      have decomposition := congrArg (List.flatMap Expr.realCoordinates)
        (List.take_append_drop count (expressions parameter))
      simp only [List.flatMap_append] at decomposition
      have prefixLength :
          (((expressions parameter).take count).flatMap Expr.realCoordinates).length =
            offset := by
        rw [flatRealCoordinates_length]
        have fixed := congrArg (List.take count) (family.skeleton_eq parameter)
        have sums := congrArg (fun skeletons =>
          (skeletons.map Expr.realArity).sum) fixed
        rw [← List.map_take] at sums
        simpa only [List.map_map, Function.comp_apply] using sums
      rw [← decomposition, ← prefixLength, list_getD_append_right]
    rw [functionEquality]
    exact family.coordinate_measurable _

def element {α : Type*} [MeasurableSpace α] {expressions : α → List Expr}
    (family : MeasurableExprListFamily α expressions) (index : Nat)
    (inBounds : index < family.skeletons.length) :
    MeasurableFamily α (fun parameter => listElement (expressions parameter) index) where
  skeleton := family.skeletons[index]
  skeleton_eq parameter := listElement_skeleton (family.skeleton_eq parameter) inBounds
  coordinate_count parameter := by
    rw [realCoordinates_length, listElement_skeleton (family.skeleton_eq parameter) inBounds]
  coordinate_measurable coordinate := by
    by_cases coordinateBounds : coordinate < family.skeletons[index].realArity
    · have functionEquality :
          (fun parameter =>
            (listElement (expressions parameter) index).realCoordinates.getD coordinate 0) =
          fun parameter => ((expressions parameter).flatMap Expr.realCoordinates).getD
            (((family.skeletons.take index).map Expr.realArity).sum + coordinate) 0 := by
        funext parameter
        have parameterBounds : index < (expressions parameter).length := by
          rw [map_skeleton_length (family.skeleton_eq parameter)]
          exact inBounds
        rw [listElement_eq_getElem parameterBounds]
        rcases flatMap_decompose_getElem Expr.realCoordinates (expressions parameter)
          index parameterBounds with ⟨front, suffix, flatEquality, frontLength⟩
        have fixedFront : front.length =
            ((family.skeletons.take index).map Expr.realArity).sum :=
          frontLength.trans (prefix_coordinate_length (family.skeleton_eq parameter) index)
        rw [flatEquality, ← fixedFront, list_getD_append_middle]
        rw [realCoordinates_length]
        have itemSkeleton := listElement_skeleton (family.skeleton_eq parameter) inBounds
        rw [listElement_eq_getElem parameterBounds] at itemSkeleton
        rwa [itemSkeleton]
      rw [functionEquality]
      exact family.coordinate_measurable _
    · have functionEquality :
          (fun parameter =>
            (listElement (expressions parameter) index).realCoordinates.getD coordinate 0) =
            fun _ => 0 := by
        funext parameter
        have outOfBounds :
            (listElement (expressions parameter) index).realCoordinates.length ≤ coordinate := by
          rw [realCoordinates_length,
            listElement_skeleton (family.skeleton_eq parameter) inBounds]
          exact Nat.le_of_not_gt coordinateBounds
        rw [List.getD_eq_getElem?_getD, List.getElem?_eq_none outOfBounds]
        rfl
      rw [functionEquality]
      exact measurable_const

def ofBlock {α : Type*} [MeasurableSpace α] {parent : α → Expr}
    (parentFamily : MeasurableFamily α parent) (expressions : α → List Expr)
    (skeletons : List Skeleton)
    (skeletonEquality : ∀ parameter,
      (expressions parameter).map Expr.skeleton = skeletons)
    (blockOffset : Nat)
    (blockDecompose : ∀ parameter, ∃ before after,
      (parent parameter).realCoordinates =
        before ++ (expressions parameter).flatMap Expr.realCoordinates ++ after ∧
      before.length = blockOffset) :
    MeasurableExprListFamily α expressions where
  skeletons := skeletons
  skeleton_eq := skeletonEquality
  coordinate_measurable index := by
    by_cases inBounds : index < (skeletons.map Expr.realArity).sum
    · have functionEquality :
          (fun parameter =>
            ((expressions parameter).flatMap Expr.realCoordinates).getD index 0) =
          fun parameter => (parent parameter).realCoordinates.getD (blockOffset + index) 0 := by
        funext parameter
        rcases blockDecompose parameter with ⟨before, after, equality, beforeLength⟩
        rw [equality, ← beforeLength, list_getD_append_middle]
        rw [flatRealCoordinates_length]
        have fixed := congrArg (fun skeletons =>
          (skeletons.map Expr.realArity).sum) (skeletonEquality parameter)
        have normalized :
            ((expressions parameter).map
              (Expr.realArity ∘ Expr.skeleton)).sum =
              (skeletons.map Expr.realArity).sum := by
          simpa only [List.map_map, Function.comp_apply] using fixed
        rwa [normalized]
      rw [functionEquality]
      exact parentFamily.coordinate_measurable _
    · have functionEquality :
          (fun parameter =>
            ((expressions parameter).flatMap Expr.realCoordinates).getD index 0) =
            fun _ => 0 := by
        funext parameter
        have outOfBounds :
            ((expressions parameter).flatMap Expr.realCoordinates).length ≤ index := by
          rw [flatRealCoordinates_length]
          have fixed := congrArg (fun skeletons =>
            (skeletons.map Expr.realArity).sum) (skeletonEquality parameter)
          have normalized :
              ((expressions parameter).map
                (Expr.realArity ∘ Expr.skeleton)).sum =
                (skeletons.map Expr.realArity).sum := by
            simpa only [List.map_map, Function.comp_apply] using fixed
          rw [normalized]
          exact Nat.le_of_not_gt inBounds
        rw [List.getD_eq_getElem?_getD, List.getElem?_eq_none outOfBounds]
        rfl
      rw [functionEquality]
      exact measurable_const

def sample {α : Type*} [MeasurableSpace α] (mode : Mode) (op : Tag)
    {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) :
    MeasurableFamily α
      (fun parameter => Expr.sample mode op (affine parameter) (general parameter)) where
  skeleton := .sample mode op affineFamily.skeletons generalFamily.skeletons
  skeleton_eq parameter := by
    simp [Expr.skeleton, affineFamily.skeleton_eq parameter,
      generalFamily.skeleton_eq parameter]
  coordinate_count parameter := by
    simp only [Expr.realCoordinates, Expr.realArity, List.length_append]
    rw [affineFamily.coordinate_count, generalFamily.coordinate_count]
  coordinate_measurable index := by
    by_cases inAffine : index < (affineFamily.skeletons.map Expr.realArity).sum
    · have functionEquality :
          (fun parameter =>
            (Expr.sample mode op (affine parameter) (general parameter)).realCoordinates.getD
              index 0) =
          fun parameter => ((affine parameter).flatMap Expr.realCoordinates).getD index 0 := by
        funext parameter
        simp only [Expr.realCoordinates]
        rw [list_getD_append_left]
        rwa [affineFamily.coordinate_count]
      rw [functionEquality]
      exact affineFamily.coordinate_measurable index
    · have functionEquality :
          (fun parameter =>
            (Expr.sample mode op (affine parameter) (general parameter)).realCoordinates.getD
              index 0) =
          fun parameter => ((general parameter).flatMap Expr.realCoordinates).getD
            (index - (affineFamily.skeletons.map Expr.realArity).sum) 0 := by
        funext parameter
        simp only [Expr.realCoordinates]
        rw [List.getD_eq_getElem?_getD, List.getElem?_append,
          affineFamily.coordinate_count, if_neg inAffine, ← List.getD_eq_getElem?_getD]
      rw [functionEquality]
      exact generalFamily.coordinate_measurable _

def replaceAt {α : Type*} [MeasurableSpace α] {expressions : α → List Expr}
    (family : MeasurableExprListFamily α expressions) (index : Nat)
    (inBounds : index < family.skeletons.length)
    {replacement : α → Expr} (replacementFamily : MeasurableFamily α replacement) :
    MeasurableExprListFamily α
      (fun parameter => replaceListElement (expressions parameter) index
        (replacement parameter)) := by
  let resultSkeletons := family.skeletons.take index ++
    replacementFamily.skeleton :: family.skeletons.drop (index + 1)
  let prefixArity :=
    ((family.skeletons.take index).map Expr.realArity).sum
  let oldArity := family.skeletons[index].realArity
  let newArity := replacementFamily.skeleton.realArity
  refine {
    skeletons := resultSkeletons
    skeleton_eq := fun parameter => ?_
    coordinate_measurable := fun coordinate => ?_
  }
  · rw [replaceListElement_skeletons, family.skeleton_eq]
    rw [replacementFamily.skeleton_eq]
  · by_cases inPrefix : coordinate < prefixArity
    · have functionEquality :
          (fun parameter =>
            ((replaceListElement (expressions parameter) index
              (replacement parameter)).flatMap Expr.realCoordinates).getD coordinate 0) =
          fun parameter =>
            ((expressions parameter).flatMap Expr.realCoordinates).getD coordinate 0 := by
        funext parameter
        have actualBounds : index < (expressions parameter).length := by
          rw [map_skeleton_length (family.skeleton_eq parameter)]
          exact inBounds
        rw [replaceListElement_coordinates,
          list_coordinates_take_element_drop _ _ actualBounds]
        have prefixLength :
            ((expressions parameter).take index |>.flatMap Expr.realCoordinates).length =
              prefixArity := by
          rw [List.length_flatMap]
          exact prefix_coordinate_length (family.skeleton_eq parameter) index
        have actualInPrefix : coordinate <
            ((expressions parameter).take index |>.flatMap Expr.realCoordinates).length := by
          rwa [prefixLength]
        rw [List.append_assoc,
          list_getD_append_left _ _ coordinate 0 actualInPrefix,
          List.append_assoc,
          list_getD_append_left _ _ coordinate 0 actualInPrefix]
      rw [functionEquality]
      exact family.coordinate_measurable coordinate
    · by_cases inReplacement : coordinate - prefixArity < newArity
      · have functionEquality :
            (fun parameter =>
              ((replaceListElement (expressions parameter) index
                (replacement parameter)).flatMap Expr.realCoordinates).getD coordinate 0) =
            fun parameter => (replacement parameter).realCoordinates.getD
              (coordinate - prefixArity) 0 := by
          funext parameter
          rw [replaceListElement_coordinates]
          have prefixLength :
              ((expressions parameter).take index |>.flatMap Expr.realCoordinates).length =
                prefixArity := by
            rw [List.length_flatMap]
            exact prefix_coordinate_length (family.skeleton_eq parameter) index
          have replacementLength :
              (replacement parameter).realCoordinates.length = newArity := by
            rw [realCoordinates_length, replacementFamily.skeleton_eq]
          have coordinateForm :
              coordinate = prefixArity + (coordinate - prefixArity) := by omega
          have replacementBounds : coordinate - prefixArity <
              (replacement parameter).realCoordinates.length := by
            rwa [replacementLength]
          have extraction := list_getD_append_middle
            ((expressions parameter).take index |>.flatMap Expr.realCoordinates)
            (replacement parameter).realCoordinates
            ((expressions parameter).drop (index + 1) |>.flatMap Expr.realCoordinates)
            (coordinate - prefixArity) 0 replacementBounds
          rw [prefixLength] at extraction
          rw [coordinateForm]
          rw [Nat.add_sub_cancel_left]
          exact extraction
        rw [functionEquality]
        exact replacementFamily.coordinate_measurable _
      · have functionEquality :
            (fun parameter =>
              ((replaceListElement (expressions parameter) index
                (replacement parameter)).flatMap Expr.realCoordinates).getD coordinate 0) =
            fun parameter =>
              ((expressions parameter).flatMap Expr.realCoordinates).getD
                (prefixArity + oldArity + (coordinate - prefixArity - newArity)) 0 := by
          funext parameter
          have actualBounds : index < (expressions parameter).length := by
            rw [map_skeleton_length (family.skeleton_eq parameter)]
            exact inBounds
          rw [replaceListElement_coordinates,
            list_coordinates_take_element_drop _ _ actualBounds]
          have prefixLength :
              ((expressions parameter).take index |>.flatMap Expr.realCoordinates).length =
                prefixArity := by
            rw [List.length_flatMap]
            exact prefix_coordinate_length (family.skeleton_eq parameter) index
          have replacementLength :
              (replacement parameter).realCoordinates.length = newArity := by
            rw [realCoordinates_length, replacementFamily.skeleton_eq]
          have oldSkeleton :
              (expressions parameter)[index].skeleton = family.skeletons[index] := by
            have equality := listElement_skeleton
              (family.skeleton_eq parameter) inBounds
            rwa [listElement_eq_getElem actualBounds] at equality
          have oldLength :
              (expressions parameter)[index].realCoordinates.length = oldArity := by
            rw [realCoordinates_length, oldSkeleton]
          have coordinateForm :
              coordinate = prefixArity + newArity +
                (coordinate - prefixArity - newArity) := by omega
          let suffixCoordinate := coordinate - prefixArity - newArity
          calc
            (((expressions parameter).take index |>.flatMap Expr.realCoordinates) ++
                (replacement parameter).realCoordinates ++
                ((expressions parameter).drop (index + 1) |>.flatMap
                  Expr.realCoordinates)).getD coordinate 0 =
                ((expressions parameter).drop (index + 1) |>.flatMap
                  Expr.realCoordinates).getD suffixCoordinate 0 := by
              have extraction := list_getD_append_third
                ((expressions parameter).take index |>.flatMap Expr.realCoordinates)
                (replacement parameter).realCoordinates
                ((expressions parameter).drop (index + 1) |>.flatMap Expr.realCoordinates)
                suffixCoordinate 0
              rw [prefixLength, replacementLength] at extraction
              rw [coordinateForm]
              exact extraction
            _ = (((expressions parameter).take index |>.flatMap Expr.realCoordinates) ++
                (expressions parameter)[index].realCoordinates ++
                ((expressions parameter).drop (index + 1) |>.flatMap
                  Expr.realCoordinates)).getD
                    (prefixArity + oldArity + suffixCoordinate) 0 := by
              symm
              have extraction := list_getD_append_third
                ((expressions parameter).take index |>.flatMap Expr.realCoordinates)
                (expressions parameter)[index].realCoordinates
                ((expressions parameter).drop (index + 1) |>.flatMap Expr.realCoordinates)
                suffixCoordinate 0
              rw [prefixLength, oldLength] at extraction
              exact extraction
        rw [functionEquality]
        exact family.coordinate_measurable _

def sampleReplaceAffine {α : Type*} [MeasurableSpace α] (mode : Mode) (op : Tag)
    {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general)
    (index : Nat) (inBounds : index < affineFamily.skeletons.length)
    {replacement : α → Expr} (replacementFamily : MeasurableFamily α replacement) :
    MeasurableFamily α (fun parameter => .sample mode op
      (replaceListElement (affine parameter) index (replacement parameter))
      (general parameter)) :=
  sample mode op (affineFamily.replaceAt index inBounds replacementFamily) generalFamily

def sampleReplaceGeneral {α : Type*} [MeasurableSpace α] (mode : Mode) (op : Tag)
    {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general)
    (index : Nat) (inBounds : index < generalFamily.skeletons.length)
    {replacement : α → Expr} (replacementFamily : MeasurableFamily α replacement) :
    MeasurableFamily α (fun parameter => .sample mode op (affine parameter)
      (replaceListElement (general parameter) index (replacement parameter))) :=
  sample mode op affineFamily
    (generalFamily.replaceAt index inBounds replacementFamily)

end MeasurableExprListFamily

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

def sampleAffineArgument {α : Type*} [MeasurableSpace α] (mode : Mode) (op : Tag)
    (front suffix general : List Expr) {current : α → Expr}
    (family : MeasurableFamily α current) :
    MeasurableFamily α (fun parameter =>
      .sample mode op (front ++ current parameter :: suffix) general) := by
  let before := front.flatMap Expr.realCoordinates
  let after := suffix.flatMap Expr.realCoordinates ++ general.flatMap Expr.realCoordinates
  apply surround family
    (fun expression => .sample mode op (front ++ expression :: suffix) general)
    (.sample mode op
      (front.map Expr.skeleton ++ family.skeleton :: suffix.map Expr.skeleton)
      (general.map Expr.skeleton))
    (by intro parameter; simp [Expr.skeleton, family.skeleton_eq parameter]) before after
  · intro parameter
    simp [Expr.realCoordinates, before, after, List.flatMap_append, List.append_assoc]
  · simp only [Expr.realArity, before, after, List.map_append, List.sum_append,
      List.map_cons, List.sum_cons, List.length_append]
    rw [flatRealCoordinates_length front, flatRealCoordinates_length suffix,
      flatRealCoordinates_length general]
    simp only [List.map_map, Function.comp_apply]
    omega

def sampleGeneralArgument {α : Type*} [MeasurableSpace α] (mode : Mode) (op : Tag)
    (affine front suffix : List Expr) {current : α → Expr}
    (family : MeasurableFamily α current) :
    MeasurableFamily α (fun parameter =>
      .sample mode op affine (front ++ current parameter :: suffix)) := by
  let before := affine.flatMap Expr.realCoordinates ++ front.flatMap Expr.realCoordinates
  let after := suffix.flatMap Expr.realCoordinates
  apply surround family
    (fun expression => .sample mode op affine (front ++ expression :: suffix))
    (.sample mode op (affine.map Expr.skeleton)
      (front.map Expr.skeleton ++ family.skeleton :: suffix.map Expr.skeleton))
    (by intro parameter; simp [Expr.skeleton, family.skeleton_eq parameter]) before after
  · intro parameter
    simp [Expr.realCoordinates, before, after, List.flatMap_append, List.append_assoc]
  · simp only [Expr.realArity, before, after, List.map_append, List.sum_append,
      List.map_cons, List.sum_cons, List.length_append]
    rw [flatRealCoordinates_length affine, flatRealCoordinates_length front,
      flatRealCoordinates_length suffix]
    simp only [List.map_map, Function.comp_apply]
    omega

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

theorem measurable_promote {α : Type*} [MeasurableSpace α]
    {body : α → Expr} (bodyMeasurable : Measurable body) :
    Measurable fun parameter => Expr.promote (body parameter) :=
  measurable_unaryConstructor bodyMeasurable Expr.promote Expr.promote
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_neg {α : Type*} [MeasurableSpace α] (mode : Mode)
    {body : α → Expr} (bodyMeasurable : Measurable body) :
    Measurable fun parameter => Expr.neg mode (body parameter) :=
  measurable_unaryConstructor bodyMeasurable (.neg mode) (.neg mode)
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_realLiteral {α : Type*} [MeasurableSpace α] (mode : Mode)
    {value : α → ℝ} (valueMeasurable : Measurable value) :
    Measurable fun parameter => Expr.real mode (value parameter) := by
  apply measurable_expr_of_parts
  · simpa [Expr.skeleton] using
      (measurable_const : Measurable fun _ : α => Skeleton.real mode)
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

theorem measurable_add {α : Type*} [MeasurableSpace α] (mode : Mode)
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.add mode (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable (.add mode) (.add mode)
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_mul {α : Type*} [MeasurableSpace α] (mode : Mode)
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.mul mode (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable (.mul mode) (.mul mode)
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_div {α : Type*} [MeasurableSpace α] (mode : Mode)
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.div mode (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable (.div mode) (.div mode)
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_lt {α : Type*} [MeasurableSpace α]
    {left right : α → Expr} (leftMeasurable : Measurable left)
    (rightMeasurable : Measurable right) :
    Measurable fun parameter => Expr.lt (left parameter) (right parameter) :=
  measurable_binaryConstructor leftMeasurable rightMeasurable Expr.lt Expr.lt
    (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates])

theorem measurable_sampleReplaceAffine {α : Type*} [MeasurableSpace α]
    (mode : Mode) (op : Tag) {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) (index : Nat)
    {replacement : α → Expr} (replacementMeasurable : Measurable replacement) :
    Measurable fun parameter => Expr.sample mode op
      (replaceListElement (affine parameter) index (replacement parameter))
      (general parameter) := by
  apply measurable_expr_of_parts
  · let operation : Skeleton → Skeleton := fun replacementSkeleton =>
      .sample mode op
        (affineFamily.skeletons.take index ++ replacementSkeleton ::
          affineFamily.skeletons.drop (index + 1))
        generalFamily.skeletons
    have operationMeasurable : Measurable operation := measurable_of_countable _
    convert operationMeasurable.comp (measurable_skeleton.comp replacementMeasurable) using 1
    funext parameter
    simp [operation, Expr.skeleton, replaceListElement_skeletons,
      affineFamily.skeleton_eq parameter, generalFamily.skeleton_eq parameter]
  · let prefixFamily := affineFamily.take index
    let suffixFamily := affineFamily.drop (index + 1)
    have prefixMeasurable := prefixFamily.measurable_realCoordinates
    have replacementCoordinates : Measurable fun parameter =>
        (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates) :=
      measurable_realCoordinates.comp replacementMeasurable
    have suffixMeasurable := suffixFamily.measurable_realCoordinates
    have generalMeasurable := generalFamily.measurable_realCoordinates
    have prefixReplacementPair : Measurable fun parameter =>
        ((⟨((affine parameter).take index).flatMap Expr.realCoordinates⟩ :
            RealCoordinates),
          (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates)) :=
      Measurable.prod prefixMeasurable replacementCoordinates
    have prefixReplacement := realCoordinatesAppend_measurable.comp prefixReplacementPair
    have prefixReplacementSuffixPair : Measurable fun parameter =>
        (realCoordinatesAppend
            (⟨((affine parameter).take index).flatMap Expr.realCoordinates⟩ : RealCoordinates)
            (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates),
          (⟨((affine parameter).drop (index + 1)).flatMap Expr.realCoordinates⟩ :
            RealCoordinates)) :=
      Measurable.prod prefixReplacement suffixMeasurable
    have prefixReplacementSuffix :=
      realCoordinatesAppend_measurable.comp prefixReplacementSuffixPair
    have allCoordinatesPair : Measurable fun parameter =>
        (realCoordinatesAppend
            (realCoordinatesAppend
              (⟨((affine parameter).take index).flatMap Expr.realCoordinates⟩ :
                RealCoordinates)
              (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates))
            (⟨((affine parameter).drop (index + 1)).flatMap Expr.realCoordinates⟩ :
              RealCoordinates),
          (⟨(general parameter).flatMap Expr.realCoordinates⟩ : RealCoordinates)) :=
      Measurable.prod prefixReplacementSuffix generalMeasurable
    have allCoordinates := realCoordinatesAppend_measurable.comp allCoordinatesPair
    convert allCoordinates using 1
    funext parameter
    congr 1
    simp [prefixFamily, suffixFamily, realCoordinatesAppend, Expr.realCoordinates,
      replaceListElement_coordinates, List.append_assoc]

theorem measurable_sampleReplaceGeneral {α : Type*} [MeasurableSpace α]
    (mode : Mode) (op : Tag) {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) (index : Nat)
    {replacement : α → Expr} (replacementMeasurable : Measurable replacement) :
    Measurable fun parameter => Expr.sample mode op (affine parameter)
      (replaceListElement (general parameter) index (replacement parameter)) := by
  apply measurable_expr_of_parts
  · let operation : Skeleton → Skeleton := fun replacementSkeleton =>
      .sample mode op affineFamily.skeletons
        (generalFamily.skeletons.take index ++ replacementSkeleton ::
          generalFamily.skeletons.drop (index + 1))
    have operationMeasurable : Measurable operation := measurable_of_countable _
    convert operationMeasurable.comp (measurable_skeleton.comp replacementMeasurable) using 1
    funext parameter
    simp [operation, Expr.skeleton, replaceListElement_skeletons,
      affineFamily.skeleton_eq parameter, generalFamily.skeleton_eq parameter]
  · let prefixFamily := generalFamily.take index
    let suffixFamily := generalFamily.drop (index + 1)
    have affineMeasurable := affineFamily.measurable_realCoordinates
    have prefixMeasurable := prefixFamily.measurable_realCoordinates
    have replacementCoordinates : Measurable fun parameter =>
        (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates) :=
      measurable_realCoordinates.comp replacementMeasurable
    have suffixMeasurable := suffixFamily.measurable_realCoordinates
    have prefixReplacementPair : Measurable fun parameter =>
        ((⟨((general parameter).take index).flatMap Expr.realCoordinates⟩ :
            RealCoordinates),
          (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates)) :=
      Measurable.prod prefixMeasurable replacementCoordinates
    have prefixReplacement := realCoordinatesAppend_measurable.comp prefixReplacementPair
    have prefixReplacementSuffixPair : Measurable fun parameter =>
        (realCoordinatesAppend
            (⟨((general parameter).take index).flatMap Expr.realCoordinates⟩ :
              RealCoordinates)
            (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates),
          (⟨((general parameter).drop (index + 1)).flatMap Expr.realCoordinates⟩ :
            RealCoordinates)) :=
      Measurable.prod prefixReplacement suffixMeasurable
    have prefixReplacementSuffix :=
      realCoordinatesAppend_measurable.comp prefixReplacementSuffixPair
    have allCoordinatesPair : Measurable fun parameter =>
        ((⟨(affine parameter).flatMap Expr.realCoordinates⟩ : RealCoordinates),
          realCoordinatesAppend
            (realCoordinatesAppend
              (⟨((general parameter).take index).flatMap Expr.realCoordinates⟩ :
                RealCoordinates)
              (⟨(replacement parameter).realCoordinates⟩ : RealCoordinates))
            (⟨((general parameter).drop (index + 1)).flatMap Expr.realCoordinates⟩ :
              RealCoordinates)) :=
      Measurable.prod affineMeasurable prefixReplacementSuffix
    have allCoordinates := realCoordinatesAppend_measurable.comp allCoordinatesPair
    convert allCoordinates using 1
    funext parameter
    congr 1
    simp [prefixFamily, suffixFamily, realCoordinatesAppend, Expr.realCoordinates,
      replaceListElement_coordinates, List.append_assoc]

theorem terminalFloatSet_eq :
    terminalFloatSet = Expr.skeleton ⁻¹' {Skeleton.real .E} := by
  ext expression
  cases expression <;>
    simp [terminalFloatSet, Expr.skeleton]

theorem terminalFloatSet_measurable : MeasurableSet terminalFloatSet := by
  rw [terminalFloatSet_eq]
  have singletonMeasurable : MeasurableSet ({Skeleton.real .E} : Set Skeleton) := by
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
    case real mode value => cases mode <;>
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
  | .bool value => .bool value
  | .real mode => .real mode
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
  | .promote body => .promote (skeletonShift amount cutoff body)
  | .neg mode body => .neg mode (skeletonShift amount cutoff body)
  | .add mode left right =>
      .add mode (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .mul mode left right =>
      .mul mode (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .div mode left right =>
      .div mode (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .lt left right => .lt (skeletonShift amount cutoff left) (skeletonShift amount cutoff right)
  | .sample mode op affine general => .sample mode op
      (affine.map (skeletonShift amount cutoff)) (general.map (skeletonShift amount cutoff))

theorem shift_skeleton (amount cutoff : Nat) (expression : Expr) :
    (expression.shift amount cutoff).skeleton = skeletonShift amount cutoff expression.skeleton := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression cutoff with
  | h size ih =>
      cases expression with
      | sample mode op affine general =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift, List.map_map]
          apply congrArg₂ (Expr.sample mode op)
          · apply List.map_congr_left
            intro child member
            apply ih (sizeOf child)
            · rw [← sizeEq]
              have := List.sizeOf_lt_of_mem member
              simp_wf
              omega
            · rfl
          · apply List.map_congr_left
            intro child member
            apply ih (sizeOf child)
            · rw [← sizeEq]
              have := List.sizeOf_lt_of_mem member
              simp_wf
              omega
            · rfl
      | pair left right | app left right | cons left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | add mode left right | mul mode left right | div mode left right =>
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
      | lam body | fst body | snd body | promote body =>
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
      | neg mode body =>
          simp only [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | bvar index | unit | bool flag | real mode index | nil =>
          simp [Expr.shift, Expr.mapVars, Expr.skeleton, skeletonShift]

theorem shift_realCoordinates (amount cutoff : Nat) (expression : Expr) :
    (expression.shift amount cutoff).realCoordinates = expression.realCoordinates := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression cutoff with
  | h size ih =>
      cases expression with
      | sample mode op affine general =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates, List.flatMap_map]
          apply congrArg₂ List.append
          · apply List.flatMap_congr
            intro child member
            apply ih (sizeOf child)
            · rw [← sizeEq]
              have := List.sizeOf_lt_of_mem member
              simp_wf
              omega
            · rfl
          · apply List.flatMap_congr
            intro child member
            apply ih (sizeOf child)
            · rw [← sizeEq]
              have := List.sizeOf_lt_of_mem member
              simp_wf
              omega
            · rfl
      | pair left right | app left right | cons left right =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) cutoff left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) cutoff right rfl]
      | add mode left right | mul mode left right | div mode left right =>
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
      | lam body | fst body | snd body | promote body =>
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
      | neg mode body =>
          simp only [Expr.shift, Expr.mapVars, Expr.realCoordinates]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) cutoff body rfl]
      | bvar index | unit | bool flag | real mode index | nil =>
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
  | .bool value => .bool value
  | .real mode => .real mode
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
  | .promote body => .promote (skeletonSubstAt depth replacement body)
  | .neg mode body => .neg mode (skeletonSubstAt depth replacement body)
  | .add mode left right => .add mode
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .mul mode left right => .mul mode
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .div mode left right => .div mode
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .lt left right => .lt
      (skeletonSubstAt depth replacement left) (skeletonSubstAt depth replacement right)
  | .sample mode op affine general => .sample mode op
      (affine.map (skeletonSubstAt depth replacement))
      (general.map (skeletonSubstAt depth replacement))

theorem substAt_skeleton (depth : Nat) (replacement expression : Expr) :
    (Expr.substAt depth replacement expression).skeleton =
      skeletonSubstAt depth replacement.skeleton expression.skeleton := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression depth with
  | h size ih =>
      cases expression with
      | sample mode op affine general =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt, List.map_map]
          apply congrArg₂ (Expr.sample mode op)
          · apply List.map_congr_left
            intro child member
            apply ih (sizeOf child)
            · rw [← sizeEq]
              have := List.sizeOf_lt_of_mem member
              simp_wf
              omega
            · rfl
          · apply List.map_congr_left
            intro child member
            apply ih (sizeOf child)
            · rw [← sizeEq]
              have := List.sizeOf_lt_of_mem member
              simp_wf
              omega
            · rfl
      | pair left right | app left right | cons left right =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf <;> omega) depth left rfl,
            ih (sizeOf right) (by rw [← sizeEq]; simp_wf <;> omega) depth right rfl]
      | add mode left right | mul mode left right | div mode left right =>
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
      | lam body | fst body | snd body | promote body =>
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
      | neg mode body =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          rw [ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega) depth body rfl]
      | bvar index =>
          simp only [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]
          split
          · exact shift_skeleton depth 0 replacement
          · simp only [Expr.skeleton]
      | unit | bool flag | real mode index | nil =>
          simp [Expr.substAt, Expr.mapVars, Expr.skeleton, skeletonSubstAt]

inductive CoordinateSelector where
  | body (index : Nat)
  | replacement (index : Nat)

mutual

def coordinatePlan (depth : Nat) (replacement : Skeleton) (bodyOffset : Nat) :
    Skeleton → List CoordinateSelector
  | .bvar index =>
      if index = depth then
        (List.range replacement.realArity).map CoordinateSelector.replacement
      else []
  | .real _ => [.body bodyOffset]
  | .lam body => coordinatePlan (depth + 1) replacement bodyOffset body
  | .fix body => coordinatePlan (depth + 2) replacement bodyOffset body
  | .fst body | .snd body | .inl body | .inr body
  | .promote body | .neg _ body => coordinatePlan depth replacement bodyOffset body
  | .app left right | .pair left right | .cons left right
  | .add _ left right | .mul _ left right | .div _ left right | .lt left right =>
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
  | .sample _ _ affine general =>
      coordinatePlanList depth replacement bodyOffset affine ++
        coordinatePlanList depth replacement
          (bodyOffset + (affine.map Expr.realArity).sum) general
  | _ => []
termination_by skeleton => sizeOf skeleton

def coordinatePlanList (depth : Nat) (replacement : Skeleton) (bodyOffset : Nat) :
    List Skeleton → List CoordinateSelector
  | [] => []
  | head :: tail =>
      coordinatePlan depth replacement bodyOffset head ++
        coordinatePlanList depth replacement (bodyOffset + head.realArity) tail
termination_by skeletons => sizeOf skeletons

end

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
      have listExact : ∀ (expressions : List Expr) (before suffix : List ℝ),
          (∀ child ∈ expressions, sizeOf child < size) →
          applyCoordinatePlan
              (coordinatePlanList depth replacement.skeleton before.length
                (expressions.map Expr.skeleton))
              (before ++ expressions.flatMap Expr.realCoordinates ++ suffix)
              replacement.realCoordinates =
            expressions.flatMap
              (fun child => (Expr.substAt depth replacement child).realCoordinates) := by
        intro expressions
        induction expressions with
        | nil =>
            intro before suffix smaller
            simp [coordinatePlanList, applyCoordinatePlan]
        | cons head tail tailIH =>
            intro before suffix smaller
            have headSmaller : sizeOf head < size :=
              smaller head (List.mem_cons_self)
            have tailSmaller : ∀ child ∈ tail, sizeOf child < size := by
              intro child member
              exact smaller child (List.mem_cons_of_mem head member)
            have headResult := ih (sizeOf head) headSmaller depth head before
              (tail.flatMap Expr.realCoordinates ++ suffix) rfl
            have tailResult := tailIH (before ++ head.realCoordinates) suffix tailSmaller
            simp only [List.map_cons, coordinatePlanList, List.flatMap_cons,
              applyCoordinatePlan_append]
            simp only [List.append_assoc] at headResult tailResult ⊢
            rw [headResult]
            have offsetEquality :
                before.length + head.skeleton.realArity =
                  (before ++ head.realCoordinates).length := by
              rw [List.length_append, realCoordinates_length head]
            rw [offsetEquality, tailResult]
      cases expression with
      | bvar index =>
          simp only [coordinatePlan, Expr.substAt, Expr.mapVars, Expr.skeleton, Expr.realCoordinates]
          split
          · rw [← realCoordinates_length replacement,
              applyCoordinatePlan_replacementRange]
            exact shift_realCoordinates depth 0 replacement |>.symm
          · simp [applyCoordinatePlan, Expr.realCoordinates]
      | real mode value =>
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
      | add mode left right | mul mode left right | div mode left right =>
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
      | promote body | neg mode body =>
          simpa only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars] using
            ih (sizeOf body) (by rw [← sizeEq]; simp_wf <;> omega)
              depth body before suffix rfl
      | sample mode op affine general =>
          simp only [coordinatePlan, Expr.skeleton, Expr.realCoordinates, Expr.substAt, Expr.mapVars,
            applyCoordinatePlan_append]
          have affineSmaller : ∀ child ∈ affine, sizeOf child < size := by
            intro child member
            rw [← sizeEq]
            have := List.sizeOf_lt_of_mem member
            simp_wf
            omega
          have generalSmaller : ∀ child ∈ general, sizeOf child < size := by
            intro child member
            rw [← sizeEq]
            have := List.sizeOf_lt_of_mem member
            simp_wf
            omega
          have affineResult := listExact affine before
            (general.flatMap Expr.realCoordinates ++ suffix) affineSmaller
          have generalResult := listExact general
            (before ++ affine.flatMap Expr.realCoordinates) suffix generalSmaller
          simp only [List.append_assoc] at affineResult generalResult ⊢
          rw [affineResult]
          have offsetEquality : before.length +
                ((affine.map Expr.skeleton).map Expr.realArity).sum =
              (before ++ affine.flatMap Expr.realCoordinates).length := by
            rw [List.length_append, flatRealCoordinates_length affine]
            simp only [List.map_map]
          rw [offsetEquality, generalResult]
          simp only [List.flatMap_map, Function.comp_apply]
      | unit | bool flag | nil =>
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
  | .bvar _ | .unit | .bool _ | .real _ _ | .lam _ | .fix _
  | .app _ _ | .fst _ | .snd _ | .matchSum _ _ _ | .nil
  | .matchList _ _ _ | .ite _ _ _ | .letE _ _ | .promote _
  | .neg _ _ | .add _ _ _ | .mul _ _ _ | .div _ _ _ | .lt _ _ => by
      simp [Expr.isValue, Expr.skeleton, Expr.isValue]
  | .sample _ _ _ _ => by simp [Expr.isValue, Expr.skeleton, Expr.isValue]
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

def firstNonValueIndex : List Skeleton → Option Nat
  | [] => none
  | head :: tail =>
      if Expr.isValue head then (Nat.succ <$> firstNonValueIndex tail) else some 0

theorem firstNonValueIndex_lt_length {skeletons : List Skeleton} {index : Nat}
    (found : firstNonValueIndex skeletons = some index) : index < skeletons.length := by
  induction skeletons generalizing index with
  | nil => simp [firstNonValueIndex] at found
  | cons head tail ih =>
      simp only [firstNonValueIndex] at found
      by_cases headValue : Expr.isValue head
      · rw [if_pos headValue] at found
        cases tailFound : firstNonValueIndex tail with
        | none => simp [tailFound] at found
        | some tailIndex =>
            rw [tailFound] at found
            change some (Nat.succ tailIndex) = some index at found
            have indexEq : Nat.succ tailIndex = index := Option.some.inj found
            have smaller := ih tailFound
            subst index
            simpa using Nat.succ_lt_succ smaller
      · rw [if_neg headValue] at found
        simp only [Option.some.injEq] at found
        subst index
        simp

theorem firstNonValue_eq_index {expressions : List Expr} {skeletons : List Skeleton}
    (skeletonEquality : expressions.map Expr.skeleton = skeletons) :
    firstNonValue expressions =
      match firstNonValueIndex skeletons with
      | none => none
      | some index => some (expressions.take index, listElement expressions index,
          expressions.drop (index + 1)) := by
  induction expressions generalizing skeletons with
  | nil =>
      have empty : skeletons = [] := by simpa using skeletonEquality.symm
      rw [empty]
      rfl
  | cons head tail ih =>
      cases skeletons with
      | nil => simp at skeletonEquality
      | cons headSkeleton tailSkeleton =>
          simp only [List.map_cons, List.cons.injEq] at skeletonEquality
          rcases skeletonEquality with ⟨headEquality, tailEquality⟩
          rw [firstNonValue, isValue_eq_skeletonIsValue, headEquality]
          by_cases headValue : Expr.isValue headSkeleton
          · rw [if_pos headValue, firstNonValueIndex, if_pos headValue, ih tailEquality]
            cases indexEquality : firstNonValueIndex tailSkeleton <;>
              simp [indexEquality, listElement]
          · rw [if_neg headValue, firstNonValueIndex, if_neg headValue]
            simp [listElement]

def skeletonIsReal : Skeleton → Bool
  | .real _ => true
  | _ => false

def allRealSkeletons (skeletons : List Skeleton) : Bool :=
  skeletons.all skeletonIsReal

theorem sum_realArity_eq_length_of_allReal {skeletons : List Skeleton}
    (allReal : allRealSkeletons skeletons = true) :
    (skeletons.map Expr.realArity).sum = skeletons.length := by
  induction skeletons with
  | nil => rfl
  | cons head tail ih =>
      simp only [allRealSkeletons, List.all_cons, Bool.and_eq_true] at allReal
      rcases allReal with ⟨headReal, tailReal⟩
      cases head <;> simp [skeletonIsReal] at headReal
      have tailResult := ih tailReal
      simp [Expr.realArity]
      omega

theorem allRealValues_eq_skeletons (expressions : List Expr) :
    allRealValues? expressions =
      if allRealSkeletons (expressions.map Expr.skeleton) then
        some (expressions.flatMap Expr.realCoordinates)
      else none := by
  induction expressions with
  | nil => rfl
  | cons head tail ih =>
      cases head <;>
        simp [allRealValues?, allRealSkeletons, skeletonIsReal, Expr.skeleton,
          Expr.realCoordinates, ih, List.getD]

def paramsFromCoordinates (op : Determinize.Statement.Paper.Op)
    (affine general : List ℝ) : Determinize.Statement.Paper.Params op :=
  (fun index => affine.getD index.1 0, fun index => general.getD index.1 0)

def atomicParams (op : Tag) (affine general : List ℝ) :
    Determinize.Statement.Paper.Params op.base :=
  paramsFromCoordinates op.base affine general

theorem measurable_paramsFromCoordinates {α : Type*} [MeasurableSpace α]
    (op : Determinize.Statement.Paper.Op)
    {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) :
    Measurable fun parameter => paramsFromCoordinates op
      ((affine parameter).flatMap Expr.realCoordinates)
      ((general parameter).flatMap Expr.realCoordinates) := by
  apply Measurable.prod
  · rw [measurable_pi_iff]
    intro index
    exact affineFamily.coordinate_measurable index.1
  · rw [measurable_pi_iff]
    intro index
    exact generalFamily.coordinate_measurable index.1

theorem measurable_atomicParams {α : Type*} [MeasurableSpace α]
    (op : Tag)
    {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) :
    Measurable fun parameter => atomicParams op
      ((affine parameter).flatMap Expr.realCoordinates)
      ((general parameter).flatMap Expr.realCoordinates) := by
  exact measurable_paramsFromCoordinates op.base affineFamily generalFamily

theorem paramsFromCoordinates_eq_getElem
    (op : Determinize.Statement.Paper.Op) (affine general : List ℝ)
    (affineArity : affine.length =
      Determinize.Statement.Paper.affineArity op)
    (generalArity : general.length =
      Determinize.Statement.Paper.generalArity op) :
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

theorem measurable_meanValue (op : Determinize.Statement.Paper.Op) :
    Measurable (Determinize.Statement.Paper.meanValue op) := by
  cases op <;> unfold Determinize.Statement.Paper.meanValue <;> fun_prop

noncomputable def primitiveKernelPack
    (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    (op : Tag) → SFiniteKernel (Determinize.Statement.Paper.Params op.base) ℝ
  | .stochastic base => ⟨laws.kernel base, laws.kernel_sfinite base⟩
  | .mean base => SFiniteKernel.piecewise
      (Determinize.Proof.Paper.measurableSet_domain base)
      (SFiniteKernel.deterministic
        (Determinize.Statement.Paper.meanValue base) (measurable_meanValue base))
      SFiniteKernel.zero

theorem primitiveFiber_eq_atomic
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (op : Tag)
    (affine general : List ℝ)
    (affineArity : affine.length =
      Determinize.Statement.Paper.affineArity op.base)
    (generalArity : general.length =
      Determinize.Statement.Paper.generalArity op.base) :
    primitiveFiber op affine general =
      (primitiveKernelPack laws op).kernel (atomicParams op affine general) := by
  classical
  cases op with
  | stochastic base =>
      simp only [Tag.base] at affineArity generalArity ⊢
      simp only [atomicParams, primitiveKernelPack, Tag.base]
      unfold primitiveFiber
        Determinize.Statement.Paper.parseParams
      simp only
      rw [dif_pos affineArity, dif_pos generalArity]
      rw [laws.kernel_eq_paperMeasure]
      apply congrArg (Determinize.Statement.Paper.paperMeasure base)
      exact (paramsFromCoordinates_eq_getElem base affine general
        affineArity generalArity).symm
  | mean base =>
      simp only [Tag.base] at affineArity generalArity ⊢
      simp only [atomicParams, primitiveKernelPack, Tag.base]
      unfold primitiveFiber
        Determinize.Statement.Paper.parseParams
      simp only
      rw [dif_pos affineArity, dif_pos generalArity]
      let actualParams : Determinize.Statement.Paper.Params base :=
        (fun index => affine[index.1]'(by simpa [affineArity] using index.2),
          fun index => general[index.1]'(by simpa [generalArity] using index.2))
      have actualParamsEquality : actualParams =
          paramsFromCoordinates base affine general := by
        dsimp only [actualParams]
        exact (paramsFromCoordinates_eq_getElem base affine general
          affineArity generalArity).symm
      change (if Determinize.Statement.Paper.domain base actualParams then
          Measure.dirac (Determinize.Statement.Paper.meanValue base actualParams)
        else 0) =
          (SFiniteKernel.piecewise
            (Determinize.Proof.Paper.measurableSet_domain base)
            (SFiniteKernel.deterministic
              (Determinize.Statement.Paper.meanValue base)
              (measurable_meanValue base))
            SFiniteKernel.zero).kernel (paramsFromCoordinates base affine general)
      unfold SFiniteKernel.piecewise SFiniteKernel.deterministic SFiniteKernel.zero
      rw [Kernel.piecewise_apply]
      simp only [Set.mem_ofPred_eq]
      by_cases paramsDomain : Determinize.Statement.Paper.domain base
          (paramsFromCoordinates base affine general)
      · have actualDomain : Determinize.Statement.Paper.domain base actualParams :=
          actualParamsEquality.symm ▸ paramsDomain
        rw [if_pos paramsDomain, if_pos actualDomain, Kernel.deterministic_apply]
        exact congrArg Measure.dirac
          (congrArg (Determinize.Statement.Paper.meanValue base)
            actualParamsEquality)
      · have actualOutside : ¬ Determinize.Statement.Paper.domain base actualParams :=
          fun actualDomain => paramsDomain (actualParamsEquality ▸ actualDomain)
        rw [if_neg paramsDomain, if_neg actualOutside]
        simp

theorem primitiveFiber_eq_zero_of_affine_length_ne
    (op : Tag) (affine general : List ℝ)
    (wrongArity : affine.length ≠
      Determinize.Statement.Paper.affineArity op.base) :
    primitiveFiber op affine general = 0 := by
  classical
  cases op <;> simp_all [primitiveFiber, Determinize.Statement.Paper.primitiveFiber,
    Determinize.Statement.Paper.parseParams, Tag.base]

theorem primitiveFiber_eq_zero_of_general_length_ne
    (op : Tag) (affine general : List ℝ)
    (affineArity : affine.length =
      Determinize.Statement.Paper.affineArity op.base)
    (wrongArity : general.length ≠
      Determinize.Statement.Paper.generalArity op.base) :
    primitiveFiber op affine general = 0 := by
  classical
  cases op <;> simp_all [primitiveFiber, Determinize.Statement.Paper.primitiveFiber,
    Determinize.Statement.Paper.parseParams, Tag.base]

universe u

/-- A measurable finite-piece description of a parameter-dependent reduction
action.  Sample continuations are jointly measurable in the original parameter and
the freshly drawn real. -/
inductive MeasurableActionFamily (α : Type u) [MeasurableSpace α] :
    (α → Action) → Type (u + 1)
  | next {successor : α → Expr} (measurable : Measurable successor) :
      MeasurableActionFamily α (fun parameter => .next (successor parameter))
  | sample {site : Mode × Tag} (draw : SFiniteKernel α ℝ)
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

def wrapSampleAffine {α : Type*} [MeasurableSpace α]
    {action : α → Action} (family : MeasurableActionFamily α action)
    (mode : Mode) (op : Tag) {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) (index : Nat) :
    MeasurableActionFamily α (fun parameter => (action parameter).wrap
      (fun next => Expr.sample mode op
        (replaceListElement (affine parameter) index next) (general parameter))) := by
  apply family.map (fun parameter next => Expr.sample mode op
    (replaceListElement (affine parameter) index next) (general parameter))
  · intro body bodyMeasurable
    exact measurable_sampleReplaceAffine mode op affineFamily generalFamily index bodyMeasurable
  · intro body bodyMeasurable
    let affinePairFamily := affineFamily.comp
      (fun input : α × ℝ => input.1) measurable_fst
    let generalPairFamily := generalFamily.comp
      (fun input : α × ℝ => input.1) measurable_fst
    exact measurable_sampleReplaceAffine mode op affinePairFamily generalPairFamily index
      bodyMeasurable

def wrapSampleGeneral {α : Type*} [MeasurableSpace α]
    {action : α → Action} (family : MeasurableActionFamily α action)
    (mode : Mode) (op : Tag) {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) (index : Nat) :
    MeasurableActionFamily α (fun parameter => (action parameter).wrap
      (fun next => Expr.sample mode op (affine parameter)
        (replaceListElement (general parameter) index next))) := by
  apply family.map (fun parameter next => Expr.sample mode op (affine parameter)
    (replaceListElement (general parameter) index next))
  · intro body bodyMeasurable
    exact measurable_sampleReplaceGeneral mode op affineFamily generalFamily index bodyMeasurable
  · intro body bodyMeasurable
    let affinePairFamily := affineFamily.comp
      (fun input : α × ℝ => input.1) measurable_fst
    let generalPairFamily := generalFamily.comp
      (fun input : α × ℝ => input.1) measurable_fst
    exact measurable_sampleReplaceGeneral mode op affinePairFamily generalPairFamily index
      bodyMeasurable

noncomputable def sampleZero {α : Type*} [MeasurableSpace α] (mode : Mode) (op : Tag) :
    MeasurableActionFamily α (fun _ => Action.sample (mode, op) 0 (Expr.real mode)) := by
  let draw : SFiniteKernel α ℝ :=
    SFiniteKernel.zero
  apply congr (.sample draw (measurable_realLiteral mode measurable_snd))
  funext parameter
  rfl

noncomputable def sampleAtomic {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (op : Tag) {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general) :
    MeasurableActionFamily α (fun parameter => Action.sample (mode, op)
      ((primitiveKernelPack laws op).kernel
          (atomicParams op
            ((affine parameter).flatMap Expr.realCoordinates)
            ((general parameter).flatMap Expr.realCoordinates)))
      (Expr.real mode)) := by
  let atom := primitiveKernelPack laws op
  let parameters := fun parameter => atomicParams op
    ((affine parameter).flatMap Expr.realCoordinates)
    ((general parameter).flatMap Expr.realCoordinates)
  have parametersMeasurable : Measurable parameters :=
    measurable_atomicParams op affineFamily generalFamily
  let draw := SFiniteKernel.pullback atom parameters
    parametersMeasurable
  apply congr (.sample draw (measurable_realLiteral mode measurable_snd))
  funext parameter
  rw [pullback_apply]

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
      simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, leftFamily.skeleton_eq parameter,
        rightFamily.skeleton_eq parameter, leftValue, rightValue]
    · apply congr (rightReduce.wrapBinaryRight left leftFamily.measurable .pair .pair
          (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
      funext parameter
      simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, leftFamily.skeleton_eq parameter,
        rightFamily.skeleton_eq parameter, leftValue, rightValue]
  · apply congr (leftReduce.wrapBinaryLeft right rightFamily.measurable .pair .pair
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, leftFamily.skeleton_eq parameter, leftValue]

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
      simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, headFamily.skeleton_eq parameter,
        tailFamily.skeleton_eq parameter, headValue, tailValue]
    · apply congr (tailReduce.wrapBinaryRight head headFamily.measurable .cons .cons
          (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
      funext parameter
      simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, headFamily.skeleton_eq parameter,
        tailFamily.skeleton_eq parameter, headValue, tailValue]
  · apply congr (headReduce.wrapBinaryLeft tail tailFamily.measurable .cons .cons
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, headFamily.skeleton_eq parameter, headValue]

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
    simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]
  · apply congr (valueReduce.wrapUnary .inl .inl
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]

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
    simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]
  · apply congr (valueReduce.wrapUnary .inr .inr
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    simp [reduce, Determinize.Statement.Paper.reduce, isValue_eq_skeletonIsValue, valueFamily.skeleton_eq parameter, isValue]

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
  cases function <;> simp only [reduce, Determinize.Statement.Paper.reduce]

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
  cases pair <;> simp only [reduce, Determinize.Statement.Paper.reduce]

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
  cases pair <;> simp only [reduce, Determinize.Statement.Paper.reduce]

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
  cases scrutinee <;> simp only [reduce, Determinize.Statement.Paper.reduce]

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
  cases scrutinee <;> simp only [reduce, Determinize.Statement.Paper.reduce]

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
  cases condition <;> simp only [reduce, Determinize.Statement.Paper.reduce]
  case bool value => cases value <;> simp only [reduce, Determinize.Statement.Paper.reduce]

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
  cases value <;> simp only [reduce, Determinize.Statement.Paper.reduce]

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
    (mode : Mode) (value : α → ℝ) (valueMeasurable : Measurable value) :
    MeasurableFamily α (fun parameter => Expr.real mode (value parameter)) :=
  (MeasurableFamily.realLiteral mode).comp value valueMeasurable

theorem reduce_promote_eq
    (body : Expr) :
    reduce (.promote body) =
      if body.isValue then
        match body with
        | .real .G value => .next (.real .E value)
        | _ => .stuck
      else (reduce body).wrap .promote := by
  cases body <;> simp only [reduce, Determinize.Statement.Paper.reduce]
  case real mode value => cases mode <;> simp only [reduce, Determinize.Statement.Paper.reduce]

noncomputable def reducePromote {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    {body : α → Expr} (bodyFamily : MeasurableFamily α body)
    (bodyReduce : MeasurableActionFamily α (fun parameter => reduce (body parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.promote (body parameter))) := by
  classical
  by_cases bodyValue : Expr.isValue bodyFamily.skeleton = true
  · cases bodySkeletonEq : bodyFamily.skeleton
    case real mode unitValue =>
      cases mode with
      | E =>
          apply congr stuck
          funext parameter
          have fixed := bodyFamily.skeleton_eq parameter
          have actualBodyValue : (body parameter).isValue = true :=
            (bodyFamily.isValue_eq parameter).trans bodyValue
          rw [bodySkeletonEq] at fixed
          cases actualEq : body parameter <;>
            rw [actualEq] at actualBodyValue <;>
            simp [actualEq, Expr.skeleton] at fixed <;>
            rw [reduce_promote_eq, actualBodyValue] <;>
            simp_all
      | G =>
          let resultFamily := realCoordinateResult bodyFamily .E
            (fun parameter => (body parameter).realCoordinates.getD 0 0)
            (bodyFamily.coordinate_measurable 0)
          apply congr (nextFamily resultFamily)
          funext parameter
          have fixed := bodyFamily.skeleton_eq parameter
          have actualBodyValue : (body parameter).isValue = true :=
            (bodyFamily.isValue_eq parameter).trans bodyValue
          rw [bodySkeletonEq] at fixed
          cases actualEq : body parameter <;>
            rw [actualEq] at actualBodyValue <;>
            simp [actualEq, Expr.skeleton] at fixed <;>
            rw [reduce_promote_eq, actualBodyValue] <;>
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
        rw [reduce_promote_eq, actualBodyValue] <;>
        simp
  · apply congr (bodyReduce.wrapUnary .promote .promote
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualBodyValue : (body parameter).isValue = false := by
      rw [bodyFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true bodyValue
    rw [reduce_promote_eq, actualBodyValue]
    simp

theorem reduce_neg_eq
    (mode : Mode) (body : Expr) :
    reduce (.neg mode body) =
      if body.isValue then
        match body with
        | .real _ value => .next (.real mode (-value))
        | _ => .stuck
      else (reduce body).wrap (.neg mode) := by
  cases body <;> simp only [reduce, Determinize.Statement.Paper.reduce]

noncomputable def reduceNeg {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) {body : α → Expr} (bodyFamily : MeasurableFamily α body)
    (bodyReduce : MeasurableActionFamily α (fun parameter => reduce (body parameter))) :
    MeasurableActionFamily α (fun parameter => reduce (.neg mode (body parameter))) := by
  classical
  by_cases bodyValue : Expr.isValue bodyFamily.skeleton = true
  · cases bodySkeletonEq : bodyFamily.skeleton
    case real =>
      let resultFamily := realCoordinateResult bodyFamily mode
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
  · apply congr (bodyReduce.wrapUnary (.neg mode) (.neg mode)
        (by intros; simp [Expr.skeleton]) (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualBodyValue : (body parameter).isValue = false := by
      rw [bodyFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true bodyValue
    rw [reduce_neg_eq, actualBodyValue]
    simp

theorem reduce_add_eq
    (mode : Mode) (left right : Expr) :
    reduce (.add mode left right) =
      if left.isValue then
        if right.isValue then
          match realValue? left, realValue? right with
          | some x, some y => .next (.real mode (x + y))
          | _, _ => .stuck
        else (reduce right).wrap (.add mode left)
      else (reduce left).wrap (fun next => .add mode next right) := by
  cases left <;> cases right <;> simp only [reduce, Determinize.Statement.Paper.reduce] <;> rfl

noncomputable def reduceAdd {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.add mode (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let resultFamily := realCoordinateResult leftFamily mode
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
          (.add mode) (.add mode) (by intros; simp [Expr.skeleton])
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
        (.add mode) (.add mode) (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_add_eq, actualLeftValue]
    simp

theorem reduce_mul_eq
    (mode : Mode) (left right : Expr) :
    reduce (.mul mode left right) =
      if left.isValue then
        if right.isValue then
          match realValue? left, realValue? right with
          | some x, some y => .next (.real mode (x * y))
          | _, _ => .stuck
        else (reduce right).wrap (.mul mode left)
      else (reduce left).wrap (fun next => .mul mode next right) := by
  cases left <;> cases right <;> simp only [reduce, Determinize.Statement.Paper.reduce] <;> rfl

noncomputable def reduceMul {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.mul mode (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let resultFamily := realCoordinateResult leftFamily mode
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
          (.mul mode) (.mul mode) (by intros; simp [Expr.skeleton])
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
        (.mul mode) (.mul mode) (by intros; simp [Expr.skeleton])
        (by intros; simp [Expr.realCoordinates]))
    funext parameter
    have actualLeftValue : (left parameter).isValue = false := by
      rw [leftFamily.isValue_eq parameter]
      exact Bool.eq_false_of_not_eq_true leftValue
    rw [reduce_mul_eq, actualLeftValue]
    simp

theorem reduce_div_eq
    (mode : Mode) (left right : Expr) :
    reduce (.div mode left right) =
      if left.isValue then
        if right.isValue then
          match realValue? left, realValue? right with
          | some x, some y => .next (.real mode (x / y))
          | _, _ => .stuck
        else (reduce right).wrap (.div mode left)
      else (reduce left).wrap (fun next => .div mode next right) := by
  cases left <;> cases right <;> simp only [reduce, Determinize.Statement.Paper.reduce] <;> rfl

noncomputable def reduceDiv {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) {left right : α → Expr}
    (leftFamily : MeasurableFamily α left) (rightFamily : MeasurableFamily α right)
    (leftReduce : MeasurableActionFamily α (fun parameter => reduce (left parameter)))
    (rightReduce : MeasurableActionFamily α (fun parameter => reduce (right parameter))) :
    MeasurableActionFamily α
      (fun parameter => reduce (.div mode (left parameter) (right parameter))) := by
  classical
  by_cases leftValue : Expr.isValue leftFamily.skeleton = true
  · by_cases rightValue : Expr.isValue rightFamily.skeleton = true
    · cases leftSkeletonEq : leftFamily.skeleton
      case real =>
        cases rightSkeletonEq : rightFamily.skeleton
        case real =>
          let resultFamily := realCoordinateResult leftFamily mode
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
          (.div mode) (.div mode) (by intros; simp [Expr.skeleton])
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
        (.div mode) (.div mode) (by intros; simp [Expr.skeleton])
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
  cases left <;> cases right <;> simp only [reduce, Determinize.Statement.Paper.reduce] <;> rfl

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

theorem reduce_sample_eq
    (mode : Mode) (op : Tag) (affine general : List Expr) :
    reduce (.sample mode op affine general) =
      match firstNonValue affine with
      | some (front, current, suffix) =>
          (reduce current).wrap
            (fun next => .sample mode op (front ++ next :: suffix) general)
      | none => match firstNonValue general with
        | some (front, current, suffix) =>
            (reduce current).wrap
              (fun next => .sample mode op affine (front ++ next :: suffix))
        | none => match allRealValues? affine, allRealValues? general with
          | some affineValues, some generalValues =>
              .sample (mode, op) (primitiveFiber op affineValues generalValues) (.real mode)
          | _, _ => .stuck := by
  simp only [reduce, Determinize.Statement.Paper.reduce]
  cases affineFound : firstNonValue affine with
  | some result =>
      rcases result with ⟨front, current, suffix⟩
      rfl
  | none =>
      cases generalFound : firstNonValue general with
      | some result =>
          rcases result with ⟨front, current, suffix⟩
          rfl
      | none => rfl

noncomputable def reduceSample {α : Type*} [MeasurableSpace α]
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (mode : Mode) (op : Tag) {affine general : α → List Expr}
    (affineFamily : MeasurableExprListFamily α affine)
    (generalFamily : MeasurableExprListFamily α general)
    (recurse : ∀ {current : α → Expr} (currentFamily : MeasurableFamily α current),
      (sizeOf currentFamily.skeleton <
        sizeOf (Expr.sample mode op affineFamily.skeletons generalFamily.skeletons)) →
      MeasurableActionFamily α (fun parameter => reduce (current parameter))) :
    MeasurableActionFamily α (fun parameter =>
      reduce (Expr.sample mode op (affine parameter) (general parameter))) := by
  classical
  cases affineFound : firstNonValueIndex affineFamily.skeletons with
  | some index =>
      have indexBounds := firstNonValueIndex_lt_length affineFound
      let currentFamily := affineFamily.element index indexBounds
      have currentSmaller : sizeOf currentFamily.skeleton <
          sizeOf (Expr.sample mode op affineFamily.skeletons generalFamily.skeletons) := by
        have member : affineFamily.skeletons[index] ∈ affineFamily.skeletons :=
          List.getElem_mem indexBounds
        have childSmaller := List.sizeOf_lt_of_mem member
        exact childSmaller.trans (by simp_wf; omega)
      have childReduce := recurse currentFamily currentSmaller
      apply congr (childReduce.wrapSampleAffine mode op affineFamily generalFamily index)
      funext parameter
      rw [reduce_sample_eq,
        firstNonValue_eq_index (affineFamily.skeleton_eq parameter), affineFound]
      rfl
  | none =>
      cases generalFound : firstNonValueIndex generalFamily.skeletons with
      | some index =>
          have indexBounds := firstNonValueIndex_lt_length generalFound
          let currentFamily := generalFamily.element index indexBounds
          have currentSmaller : sizeOf currentFamily.skeleton <
              sizeOf (Expr.sample mode op affineFamily.skeletons
                generalFamily.skeletons) := by
            have member : generalFamily.skeletons[index] ∈ generalFamily.skeletons :=
              List.getElem_mem indexBounds
            have childSmaller := List.sizeOf_lt_of_mem member
            exact childSmaller.trans (by simp_wf)
          have childReduce := recurse currentFamily currentSmaller
          apply congr (childReduce.wrapSampleGeneral mode op affineFamily generalFamily index)
          funext parameter
          rw [reduce_sample_eq,
            firstNonValue_eq_index (affineFamily.skeleton_eq parameter), affineFound,
            firstNonValue_eq_index (generalFamily.skeleton_eq parameter), generalFound]
          rfl
      | none =>
          by_cases affineReal : allRealSkeletons affineFamily.skeletons = true
          · by_cases generalReal : allRealSkeletons generalFamily.skeletons = true
            · by_cases affineArity : affineFamily.skeletons.length =
                Determinize.Statement.Paper.affineArity op.base
              · by_cases generalArity : generalFamily.skeletons.length =
                  Determinize.Statement.Paper.generalArity op.base
                · apply congr (sampleAtomic laws mode op affineFamily generalFamily)
                  funext parameter
                  rw [reduce_sample_eq,
                    firstNonValue_eq_index (affineFamily.skeleton_eq parameter), affineFound,
                    firstNonValue_eq_index (generalFamily.skeleton_eq parameter), generalFound]
                  have affineValues := allRealValues_eq_skeletons (affine parameter)
                  have generalValues := allRealValues_eq_skeletons (general parameter)
                  rw [affineFamily.skeleton_eq parameter, affineReal] at affineValues
                  rw [generalFamily.skeleton_eq parameter, generalReal] at generalValues
                  rw [affineValues, generalValues]
                  apply congrArg (fun fiber => Action.sample (mode, op) fiber (Expr.real mode))
                  symm
                  apply primitiveFiber_eq_atomic
                  · rw [affineFamily.coordinate_count,
                      sum_realArity_eq_length_of_allReal affineReal, affineArity]
                  · rw [generalFamily.coordinate_count,
                      sum_realArity_eq_length_of_allReal generalReal, generalArity]
                · apply congr (sampleZero mode op)
                  funext parameter
                  rw [reduce_sample_eq,
                    firstNonValue_eq_index (affineFamily.skeleton_eq parameter), affineFound,
                    firstNonValue_eq_index (generalFamily.skeleton_eq parameter), generalFound]
                  have affineValues := allRealValues_eq_skeletons (affine parameter)
                  have generalValues := allRealValues_eq_skeletons (general parameter)
                  rw [affineFamily.skeleton_eq parameter, affineReal] at affineValues
                  rw [generalFamily.skeleton_eq parameter, generalReal] at generalValues
                  rw [affineValues, generalValues]
                  apply congrArg (fun fiber => Action.sample (mode, op) fiber (Expr.real mode))
                  symm
                  apply primitiveFiber_eq_zero_of_general_length_ne
                  · rw [affineFamily.coordinate_count,
                      sum_realArity_eq_length_of_allReal affineReal, affineArity]
                  · rw [generalFamily.coordinate_count,
                      sum_realArity_eq_length_of_allReal generalReal]
                    exact generalArity
              · apply congr (sampleZero mode op)
                funext parameter
                rw [reduce_sample_eq,
                  firstNonValue_eq_index (affineFamily.skeleton_eq parameter), affineFound,
                  firstNonValue_eq_index (generalFamily.skeleton_eq parameter), generalFound]
                have affineValues := allRealValues_eq_skeletons (affine parameter)
                have generalValues := allRealValues_eq_skeletons (general parameter)
                rw [affineFamily.skeleton_eq parameter, affineReal] at affineValues
                rw [generalFamily.skeleton_eq parameter, generalReal] at generalValues
                rw [affineValues, generalValues]
                apply congrArg (fun fiber => Action.sample (mode, op) fiber (Expr.real mode))
                symm
                apply primitiveFiber_eq_zero_of_affine_length_ne
                rw [affineFamily.coordinate_count,
                  sum_realArity_eq_length_of_allReal affineReal]
                exact affineArity
            · apply congr stuck
              funext parameter
              rw [reduce_sample_eq,
                firstNonValue_eq_index (affineFamily.skeleton_eq parameter), affineFound,
                firstNonValue_eq_index (generalFamily.skeleton_eq parameter), generalFound]
              have affineValues := allRealValues_eq_skeletons (affine parameter)
              rw [affineFamily.skeleton_eq parameter, affineReal] at affineValues
              have generalValues : allRealValues? (general parameter) = none := by
                rw [allRealValues_eq_skeletons, generalFamily.skeleton_eq parameter,
                  if_neg generalReal]
              simp [affineValues, generalValues]
          · apply congr stuck
            funext parameter
            rw [reduce_sample_eq,
              firstNonValue_eq_index (affineFamily.skeleton_eq parameter), affineFound,
              firstNonValue_eq_index (generalFamily.skeleton_eq parameter), generalFound]
            have affineValues : allRealValues? (affine parameter) = none := by
              rw [allRealValues_eq_skeletons, affineFamily.skeleton_eq parameter,
                if_neg affineReal]
            simp [affineValues]

namespace Expr

def sampleAffineArgs : Expr → List Expr
  | .sample _ _ affine _ => affine
  | _ => []

def sampleGeneralArgs : Expr → List Expr
  | .sample _ _ _ general => general
  | _ => []

end Expr

def sampleAffineArgsFamily {α : Type*} [MeasurableSpace α]
    {expression : α → Expr} (family : MeasurableFamily α expression)
    (mode : Mode) (op : Tag) (affineSkeletons generalSkeletons : List Skeleton)
    (fixed : family.skeleton = .sample mode op affineSkeletons generalSkeletons) :
    MeasurableExprListFamily α (fun parameter =>
      Expr.sampleAffineArgs (expression parameter)) := by
  apply MeasurableExprListFamily.ofBlock family _ affineSkeletons
    (blockOffset := 0)
  · intro parameter
    have actual := family.skeleton_eq parameter
    rw [fixed] at actual
    cases actualExpression : expression parameter <;> rw [actualExpression] at actual
    all_goals simp [Expr.skeleton] at actual
    exact actual.2.2.1
  · intro parameter
    have actual := family.skeleton_eq parameter
    rw [fixed] at actual
    cases actualExpression : expression parameter <;> rw [actualExpression] at actual
    all_goals simp [Expr.skeleton] at actual
    simp [Expr.sampleAffineArgs, Expr.realCoordinates]

def sampleGeneralArgsFamily {α : Type*} [MeasurableSpace α]
    {expression : α → Expr} (family : MeasurableFamily α expression)
    (mode : Mode) (op : Tag) (affineSkeletons generalSkeletons : List Skeleton)
    (fixed : family.skeleton = .sample mode op affineSkeletons generalSkeletons) :
    MeasurableExprListFamily α (fun parameter =>
      Expr.sampleGeneralArgs (expression parameter)) := by
  apply MeasurableExprListFamily.ofBlock family _ generalSkeletons
    (blockOffset := (affineSkeletons.map Expr.realArity).sum)
  · intro parameter
    have actual := family.skeleton_eq parameter
    rw [fixed] at actual
    cases actualExpression : expression parameter <;> rw [actualExpression] at actual
    all_goals simp [Expr.skeleton] at actual
    exact actual.2.2.2
  · intro parameter
    have actual := family.skeleton_eq parameter
    rw [fixed] at actual
    cases actualExpression : expression parameter <;> rw [actualExpression] at actual
    all_goals simp [Expr.skeleton] at actual
    rcases actual with ⟨rfl, rfl, affineFixed, generalFixed⟩
    refine ⟨(Expr.sampleAffineArgs (expression parameter)).flatMap
      Expr.realCoordinates, [], ?_, ?_⟩
    · rw [actualExpression]
      simp [Expr.sampleAffineArgs, Expr.sampleGeneralArgs, Expr.realCoordinates]
    · rw [actualExpression]
      simp only [Expr.sampleAffineArgs]
      rw [flatRealCoordinates_length]
      have fixedArity := congrArg (fun skeletons =>
        (skeletons.map Expr.realArity).sum) affineFixed
      simpa only [List.map_map, Function.comp_apply] using fixedArity

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
      | unit | bool | real | lam | fix | nil =>
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild,
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild,
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild,
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | promote bodySkeleton =>
          have bodySmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reducePromote laws family.firstChild
            (childReduce family.firstChild bodySmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [actualEq, Expr.skeleton, Expr.firstChild]
      | neg mode bodySkeleton =>
          have bodySmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceNeg laws mode family.firstChild
            (childReduce family.firstChild bodySmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [actualEq, Expr.skeleton, Expr.firstChild]
      | add mode leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceAdd laws mode family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | mul mode leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceMul laws mode family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | div mode leftSkeleton rightSkeleton =>
          have leftSmaller : sizeOf family.firstChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          have rightSmaller : sizeOf family.secondChild.skeleton < size := by
            rw [← sizeEq, skeletonEq]
            simp_all [Skeleton.firstChild, Skeleton.secondChild]
            omega
          apply congr (reduceDiv laws mode family.firstChild family.secondChild
            (childReduce family.firstChild leftSmaller)
            (childReduce family.secondChild rightSmaller))
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
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
            simp_all [actualEq, Expr.skeleton, Expr.firstChild, Expr.secondChild]
      | sample mode op affineSkeletons generalSkeletons =>
          let affineFamily := sampleAffineArgsFamily family mode op affineSkeletons
            generalSkeletons skeletonEq
          let generalFamily := sampleGeneralArgsFamily family mode op affineSkeletons
            generalSkeletons skeletonEq
          have sampled := reduceSample laws mode op affineFamily generalFamily
            (fun currentFamily smaller => childReduce currentFamily (by
              rw [← sizeEq, skeletonEq]
              exact smaller))
          apply congr sampled
          funext parameter
          have fixed := family.skeleton_eq parameter
          rw [skeletonEq] at fixed
          cases actualEq : expression parameter <;>
            simp_all [actualEq, Expr.skeleton, Expr.sampleAffineArgs, Expr.sampleGeneralArgs,
              affineFamily, generalFamily]

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
    (expression : Expr) {site : Mode × Tag} (fiber : Measure ℝ) (continuation : ℝ → Expr)
    (equality : reduce expression = .sample site fiber continuation) :
    Measurable continuation := by
  let family := measurable_reduce laws
    (MeasurableFamily.constant (α := Unit) expression)
  exact sample_continuation_measurable_of_family family () equality

theorem meanKernel_mass_le_one
    (op : Determinize.Statement.Paper.Op)
    (params : Determinize.Statement.Paper.Params op) :
    (primitiveKernelPack laws (.mean op)).kernel params Set.univ ≤ 1 := by
  classical
  change (Kernel.piecewise
    (Determinize.Proof.Paper.measurableSet_domain op)
    (Kernel.deterministic (Determinize.Statement.Paper.meanValue op)
      (measurable_meanValue op)) 0 params) Set.univ ≤ 1
  rw [Kernel.piecewise_apply]
  split <;> simp [Kernel.deterministic_apply]

theorem primitiveFiber_mass_le_one
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (op : Tag) (affine general : List ℝ) :
    primitiveFiber op affine general Set.univ ≤ 1 := by
  classical
  by_cases affineArity : affine.length =
      Determinize.Statement.Paper.affineArity op.base
  · by_cases generalArity : general.length =
        Determinize.Statement.Paper.generalArity op.base
    · rw [primitiveFiber_eq_atomic laws op affine general affineArity generalArity]
      cases op with
      | stochastic base => exact laws.mass_le_one base _
      | mean base =>
          exact meanKernel_mass_le_one base _
    · rw [primitiveFiber_eq_zero_of_general_length_ne op affine general
        affineArity generalArity]
      simp
  · rw [primitiveFiber_eq_zero_of_affine_length_ne op affine general affineArity]
    simp

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
      | bvar index => simp [reduce, Determinize.Statement.Paper.reduce] at equality
      | unit => simp [reduce, Determinize.Statement.Paper.reduce] at equality
      | bool value => simp [reduce, Determinize.Statement.Paper.reduce] at equality
      | real mode value => simp [reduce, Determinize.Statement.Paper.reduce] at equality
      | lam body => simp [reduce, Determinize.Statement.Paper.reduce] at equality
      | fix body => simp [reduce, Determinize.Statement.Paper.reduce] at equality
      | nil => simp [reduce, Determinize.Statement.Paper.reduce] at equality
      | pair left right =>
          simp only [reduce, Determinize.Statement.Paper.reduce] at equality
          split at equality
          · split at equality
            · simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | inl value =>
          simp only [reduce, Determinize.Statement.Paper.reduce] at equality
          split at equality
          · simp at equality
          · exact wrapped value (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | inr value =>
          simp only [reduce, Determinize.Statement.Paper.reduce] at equality
          split at equality
          · simp at equality
          · exact wrapped value (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | cons head tail =>
          simp only [reduce, Determinize.Statement.Paper.reduce] at equality
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
      | promote body =>
          rw [reduce_promote_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped body (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | neg mode body =>
          rw [reduce_neg_eq] at equality
          split at equality
          · split at equality <;> simp at equality
          · exact wrapped body (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | add mode left right =>
          rw [reduce_add_eq] at equality
          split at equality
          · split at equality
            · split at equality <;> simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | mul mode left right =>
          rw [reduce_mul_eq] at equality
          split at equality
          · split at equality
            · split at equality <;> simp at equality
            · exact wrapped right (by rw [← sizeEq]; simp_wf <;> omega) _ equality
          · exact wrapped left (by rw [← sizeEq]; simp_wf <;> omega) _ equality
      | div mode left right =>
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
      | sample mode op affine general =>
          rw [reduce_sample_eq] at equality
          split at equality
          · rename_i front current suffix found
            exact wrapped current (by
              rw [← sizeEq]
              exact Nat.lt_trans (List.sizeOf_lt_of_mem
                (firstNonValue_current_mem found)) (by simp_wf <;> omega)) _ equality
          · split at equality
            · rename_i front current suffix found
              exact wrapped current (by
                rw [← sizeEq]
                exact Nat.lt_trans (List.sizeOf_lt_of_mem
                  (firstNonValue_current_mem found)) (by simp_wf <;> omega)) _ equality
            · split at equality
              · simp only [Action.sample.injEq] at equality
                rw [← equality.2.1]
                exact primitiveFiber_mass_le_one laws op _ _
              · simp at equality

theorem stepMeasure_mass_le_one
    (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (expression : Expr) : stepMeasure expression Set.univ ≤ 1 := by
  unfold stepMeasure
  cases equality : reduce expression with
  | next successor => simp [Determinize.Statement.Paper.Action.measure]
  | stuck => simp [Determinize.Statement.Paper.Action.measure]
  | sample site fiber continuation =>
      have continuationMeasurable := reduce_sample_continuation_measurable laws
        expression fiber continuation equality
      simp only [Determinize.Statement.Paper.Action.measure]
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
      case real mode value =>
        cases mode <;> simp [exactOutputMeasure, terminalFloatSet,
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
    case real mode value => cases mode <;>
      simp [exactOutputMeasure, terminalFloatSet, terminalFloatValue, Set.indicator]
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
    simp_all [Expr.isValue, reduce, Determinize.Statement.Paper.Action.measure,
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
