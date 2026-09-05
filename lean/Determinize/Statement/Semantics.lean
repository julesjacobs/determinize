import Determinize.Statement.Syntax
import Mathlib.MeasureTheory.Measure.GiryMonad

/-!
# Paper operational semantics

The reducer uses the six canonical primitive measures directly. Arithmetic is
Lean real arithmetic; in particular, division is total and `x / 0 = 0`. The
output semantics integrates sampled reals directly, without measures on expressions.
-/

namespace Determinize.Statement.Paper

open MeasureTheory ProbabilityTheory
open scoped ENNReal ProbabilityTheory

/-- A pointwise reduction action. -/
inductive Action where
  | next (expression : Expr)
  | sample (modeTag : Mode × Tag) (fiber : Measure ℝ) (continuation : ℝ → Expr)
  | stuck

def Action.wrap (context : Expr → Expr) : Action → Action
  | .next expression => .next (context expression)
  | .sample modeTag fiber continuation => .sample modeTag fiber (context ∘ continuation)
  | .stuck => .stuck

def realValue? : Expr → Option ℝ
  | .real _ value => some value
  | _ => none

def allRealValues? : List Expr → Option (List ℝ)
  | [] => some []
  | .real _ value :: tail => (value :: ·) <$> allRealValues? tail
  | _ => none

def firstNonValue : List Expr → Option (List Expr × Expr × List Expr)
  | [] => none
  | head :: tail =>
      if head.isValue then
        match firstNonValue tail with
        | none => none
        | some (front, current, suffix) => some (head :: front, current, suffix)
      else some ([], head, tail)

theorem firstNonValue_current_mem {arguments : List Expr} {front current suffix}
    (found : firstNonValue arguments = some (front, current, suffix)) :
    current ∈ arguments := by
  induction arguments generalizing front current suffix with
  | nil => simp [firstNonValue] at found
  | cons head tail ih =>
      simp only [firstNonValue] at found
      split at found
      · split at found <;> simp_all
      · simp_all

/-- The canonical stochastic or atomic-mean fiber at evaluated arguments. -/
noncomputable def primitiveFiber (tag : Tag) (affine general : List ℝ) : Measure ℝ := by
  classical
  exact match tag with
    | .stochastic op =>
        match parseParams op affine general with
        | none => 0
        | some params => paperMeasure op params
    | .mean op =>
        match parseParams op affine general with
        | none => 0
        | some params =>
            if domain op params then Measure.dirac (meanValue op params) else 0

/-- Conventional left-to-right call-by-value reduction, with values absorbing. -/
noncomputable def reduce : Expr → Action
  | .bvar _ => .stuck
  | .unit => .next .unit
  | expression@(.bool _) => .next expression
  | expression@(.real _ _) => .next expression
  | expression@(.lam _) => .next expression
  | expression@(.fix _) => .next expression
  | expression@.nil => .next expression
  | expression@(.pair left right) =>
      if left.isValue then
        if right.isValue then .next expression
        else (reduce right).wrap (fun next => .pair left next)
      else (reduce left).wrap (fun next => .pair next right)
  | expression@(.inl value) =>
      if value.isValue then .next expression else (reduce value).wrap .inl
  | expression@(.inr value) =>
      if value.isValue then .next expression else (reduce value).wrap .inr
  | expression@(.cons head tail) =>
      if head.isValue then
        if tail.isValue then .next expression
        else (reduce tail).wrap (fun next => .cons head next)
      else (reduce head).wrap (fun next => .cons next tail)
  | .app function argument =>
      if function.isValue then
        if argument.isValue then
          match function with
          | .lam body => .next (body.substHead argument)
          | fix@(.fix body) => .next (body.substTwo argument fix)
          | _ => .stuck
        else (reduce argument).wrap (fun next => .app function next)
      else (reduce function).wrap (fun next => .app next argument)
  | .fst pair =>
      if pair.isValue then match pair with | .pair left _ => .next left | _ => .stuck
      else (reduce pair).wrap .fst
  | .snd pair =>
      if pair.isValue then match pair with | .pair _ right => .next right | _ => .stuck
      else (reduce pair).wrap .snd
  | .matchSum scrutinee left right =>
      if scrutinee.isValue then
        match scrutinee with
        | .inl value => .next (left.substHead value)
        | .inr value => .next (right.substHead value)
        | _ => .stuck
      else (reduce scrutinee).wrap (fun next => .matchSum next left right)
  | .matchList scrutinee nilCase consCase =>
      if scrutinee.isValue then
        match scrutinee with
        | .nil => .next nilCase
        | .cons head tail => .next (consCase.substTwo head tail)
        | _ => .stuck
      else (reduce scrutinee).wrap
        (fun next => .matchList next nilCase consCase)
  | .ite condition thenBranch elseBranch =>
      if condition.isValue then
        match condition with
        | .bool true => .next thenBranch
        | .bool false => .next elseBranch
        | _ => .stuck
      else (reduce condition).wrap (fun next => .ite next thenBranch elseBranch)
  | .letE value body =>
      if value.isValue then .next (body.substHead value)
      else (reduce value).wrap (fun next => .letE next body)
  | .promote body =>
      if body.isValue then match body with
        | .real .G value => .next (.real .E value) | _ => .stuck
      else (reduce body).wrap .promote
  | .neg mode body =>
      if body.isValue then match body with
        | .real _ value => .next (.real mode (-value)) | _ => .stuck
      else (reduce body).wrap (.neg mode)
  | .add mode left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.real mode (x + y)) | _, _ => .stuck
        else (reduce right).wrap (.add mode left)
      else (reduce left).wrap (fun next => .add mode next right)
  | .mul mode left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.real mode (x * y)) | _, _ => .stuck
        else (reduce right).wrap (.mul mode left)
      else (reduce left).wrap (fun next => .mul mode next right)
  | .div mode left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.real mode (x / y)) | _, _ => .stuck
        else (reduce right).wrap (.div mode left)
      else (reduce left).wrap (fun next => .div mode next right)
  | .lt left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.bool (x < y)) | _, _ => .stuck
        else (reduce right).wrap (.lt left)
      else (reduce left).wrap (fun next => .lt next right)
  | .sample mode op affine general =>
      match _hAffine : firstNonValue affine with
      | some (front, current, suffix) =>
          (reduce current).wrap (fun next => .sample mode op (front ++ next :: suffix) general)
      | none => match _hGeneral : firstNonValue general with
        | some (front, current, suffix) =>
            (reduce current).wrap (fun next => .sample mode op affine (front ++ next :: suffix))
        | none => match allRealValues? affine, allRealValues? general with
          | some affineValues, some generalValues =>
              .sample (mode, op) (primitiveFiber op affineValues generalValues) (.real mode)
          | _, _ => .stuck
termination_by expression => sizeOf expression
decreasing_by
  all_goals
    first
    | decreasing_trivial
    | apply Nat.lt_trans (List.sizeOf_lt_of_mem (firstNonValue_current_mem (by assumption)))
      try simp_wf
      try omega

/-- Real output accumulated through `fuel` reduction steps. Only terminal
expectation-mode reals contribute output; other types may occur during evaluation. -/
noncomputable def cumulativeOutputMeasure : Nat → Expr → Measure ℝ
  | 0, .real .E value => Measure.dirac value
  | 0, _ => 0
  | fuel + 1, expression => match reduce expression with
      | .next next => cumulativeOutputMeasure fuel next
      | .sample _ fiber continuation =>
          fiber.bind fun value => cumulativeOutputMeasure fuel (continuation value)
      | .stuck => 0

/-- The output law, without conditioning on termination. -/
noncomputable def bigStepMeasure (program : Expr) : Measure ℝ :=
  ⨆ fuel, cumulativeOutputMeasure fuel program

/-- Operational non-stuckness through every finite stochastic execution depth. -/
def DoesNotGetStuckAt : Nat → Expr → Prop
  | 0, _ => True
  | fuel + 1, expression =>
      if expression.isValue then True
      else match reduce expression with
      | .next next => DoesNotGetStuckAt fuel next
      | .sample _ fiber continuation =>
          fiber Set.univ = 1 ∧ ∀ᵐ value ∂fiber, DoesNotGetStuckAt fuel (continuation value)
      | .stuck => False

def DoesNotGetStuck (program : Expr) : Prop :=
  ∀ fuel, DoesNotGetStuckAt fuel program

end Determinize.Statement.Paper
