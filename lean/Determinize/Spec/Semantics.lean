import Determinize.Spec.Syntax
import Mathlib.MeasureTheory.Measure.GiryMonad

/-!
# Paper operational semantics

The reducer evaluates the operands of a primitive left to right and then draws from its fiber. Arithmetic is
Lean real arithmetic; in particular, division is total and `x / 0 = 0`. The
output semantics integrates sampled reals directly, without measures on expressions.
-/

namespace Determinize.Spec.Paper

open MeasureTheory ProbabilityTheory
open scoped ENNReal ProbabilityTheory

/-- A pointwise reduction action. -/
inductive Action where
  | next (expression : Expr)
  | sample (site : DistributionAction × Op) (fiber : Measure ℝ) (continuation : ℝ → Expr)
  | stuck

def Action.wrap (context : Expr → Expr) : Action → Action
  | .next expression => .next (context expression)
  | .sample site fiber continuation => .sample site fiber (context ∘ continuation)
  | .stuck => .stuck

def realValue? : Expr → Option ℝ
  | .real value => some value
  | _ => none

/-- Conventional left-to-right call-by-value reduction, with values absorbing. -/
noncomputable def reduce : Expr → Action
  | .bvar _ => .stuck
  | .reject => .next .reject
  | .unit => .next .unit
  | expression@(.bool _) => .next expression
  | expression@(.real _) => .next expression
  | expression@(.lam _) => .next expression
  | expression@(.fix _) => .next expression
  | expression@.nil => .next expression
  | expression@(.pair left right) =>
      if left.isValue then
        if right.isValue then .next expression
        else (reduce right).wrap (fun next => .pair left next)
      else (reduce left).wrap (fun next => .pair next right)
  | expression@(.inl operand) =>
      if operand.isValue then .next expression else (reduce operand).wrap .inl
  | expression@(.inr operand) =>
      if operand.isValue then .next expression else (reduce operand).wrap .inr
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
  | .neg body =>
      if body.isValue then match body with
        | .real value => .next (.real (-value)) | _ => .stuck
      else (reduce body).wrap .neg
  | .add left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.real (x + y)) | _, _ => .stuck
        else (reduce right).wrap (.add left)
      else (reduce left).wrap (fun next => .add next right)
  | .mul left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.real (x * y)) | _, _ => .stuck
        else (reduce right).wrap (.mul left)
      else (reduce left).wrap (fun next => .mul next right)
  | .div left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.real (x / y)) | _, _ => .stuck
        else (reduce right).wrap (.div left)
      else (reduce left).wrap (fun next => .div next right)
  | .lt left right =>
      if left.isValue then
        if right.isValue then match realValue? left, realValue? right with
          | some x, some y => .next (.bool (x < y)) | _, _ => .stuck
        else (reduce right).wrap (.lt left)
      else (reduce left).wrap (fun next => .lt next right)
  | .uniform action lower upper =>
      if lower.isValue then
        if upper.isValue then match realValue? lower, realValue? upper with
          | some a, some b => .sample (action, .uniform) (uniformFiber action a b) .real
          | _, _ => .stuck
        else (reduce upper).wrap (.uniform action lower)
      else (reduce lower).wrap (fun next => .uniform action next upper)
  | .gaussian action mean variance =>
      if mean.isValue then
        if variance.isValue then match realValue? mean, realValue? variance with
          | some m, some v => .sample (action, .gaussian) (gaussianFiber action m v) .real
          | _, _ => .stuck
        else (reduce variance).wrap (.gaussian action mean)
      else (reduce mean).wrap (fun next => .gaussian action next variance)
  | .poisson action rate =>
      if rate.isValue then match realValue? rate with
        | some r => .sample (action, .poisson) (poissonFiber action r) .real
        | none => .stuck
      else (reduce rate).wrap (.poisson action)
  | .discrete action d => .sample (action, .discrete d) (discreteFiber action d) .real
  | .bernoulli action probability =>
      if probability.isValue then match realValue? probability with
        | some r => .sample (action, .bernoulli) (bernoulliFiber action r) .real
        | none => .stuck
      else (reduce probability).wrap (.bernoulli action)
  | .exponential action rate =>
      if rate.isValue then match realValue? rate with
        | some r => .sample (action, .exponential) (exponentialFiber action r) .real
        | none => .stuck
      else (reduce rate).wrap (.exponential action)
  | .beta action alpha beta =>
      if alpha.isValue then
        if beta.isValue then match realValue? alpha, realValue? beta with
          | some a, some b => .sample (action, .beta) (betaFiber action a b) .real
          | _, _ => .stuck
        else (reduce beta).wrap (.beta action alpha)
      else (reduce alpha).wrap (fun next => .beta action next beta)
  | .gamma action shape rate =>
      if shape.isValue then
        if rate.isValue then match realValue? shape, realValue? rate with
          | some k, some r => .sample (action, .gamma) (gammaFiber action k r) .real
          | _, _ => .stuck
        else (reduce rate).wrap (.gamma action shape)
      else (reduce shape).wrap (fun next => .gamma action next rate)

/-- Real output accumulated through `fuel` reduction steps. Only terminal reals
contribute output; other types may occur during evaluation. -/
noncomputable def cumulativeOutputMeasure : Nat → Expr → Measure ℝ
  | 0, .real value => Measure.dirac value
  | 0, _ => 0
  | fuel + 1, expression => match reduce expression with
      | .next next => cumulativeOutputMeasure fuel next
      | .sample _ fiber continuation =>
          fiber.bind fun value => cumulativeOutputMeasure fuel (continuation value)
      | .stuck => 0

/-- The output law, without conditioning on termination. -/
noncomputable def bigStepMeasure (program : Expr) : Measure ℝ :=
  ⨆ fuel, cumulativeOutputMeasure fuel program

/--
Primitive-domain safety at a finite reduction depth. Canonical primitive fibers
have mass one exactly on their parameter domain and are zero off-domain.
Structural stuckness is outside this predicate; typing supplies structural progress.
-/
def PrimitiveDomainSafeAt : Nat → Expr → Prop
  | 0, _ => True
  | fuel + 1, expression =>
      if expression.isValue then True
      else match reduce expression with
      | .next next => PrimitiveDomainSafeAt fuel next
      | .sample _ fiber continuation =>
          fiber Set.univ = 1 ∧
            ∀ᵐ value ∂fiber, PrimitiveDomainSafeAt fuel (continuation value)
      | .stuck => True

/-- Every primitive call reached at a finite depth has valid parameters almost surely. -/
def PrimitiveDomainSafe (program : Expr) : Prop :=
  ∀ fuel, PrimitiveDomainSafeAt fuel program

end Determinize.Spec.Paper
