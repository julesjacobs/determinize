import Determinize.Checking.Typing
import Determinize.Checking.Input

namespace Determinize.Checking
open Spec.Paper

private def actionAffinities : DistributionAction → List Affinity
  | .sample affinity => [affinity]
  | .mean => []

def sampleAffinities : Core → List Affinity
  | .bvar _ => []
  | .reject => []
  | .unit => []
  | .bool _ => []
  | .real _ => []
  | .lam body => (sampleAffinities body)
  | .fix body => (sampleAffinities body)
  | .app fn arg => (sampleAffinities fn) ++ (sampleAffinities arg)
  | .pair left right => (sampleAffinities left) ++ (sampleAffinities right)
  | .fst pairValue => (sampleAffinities pairValue)
  | .snd pairValue => (sampleAffinities pairValue)
  | .inl value => (sampleAffinities value)
  | .inr value => (sampleAffinities value)
  | .matchSum scrutinee left right => (sampleAffinities scrutinee) ++ (sampleAffinities left) ++ (sampleAffinities right)
  | .nil => []
  | .cons head tail => (sampleAffinities head) ++ (sampleAffinities tail)
  | .matchList scrutinee nilCase consCase => (sampleAffinities scrutinee) ++ (sampleAffinities nilCase) ++ (sampleAffinities consCase)
  | .ite condition thenBranch elseBranch => (sampleAffinities condition) ++ (sampleAffinities thenBranch) ++ (sampleAffinities elseBranch)
  | .letE value body => (sampleAffinities value) ++ (sampleAffinities body)
  | .neg body => (sampleAffinities body)
  | .add left right => (sampleAffinities left) ++ (sampleAffinities right)
  | .mul left right => (sampleAffinities left) ++ (sampleAffinities right)
  | .div left right => (sampleAffinities left) ++ (sampleAffinities right)
  | .lt left right => (sampleAffinities left) ++ (sampleAffinities right)
  | .uniform action lower upper => actionAffinities action ++ ((sampleAffinities lower) ++ (sampleAffinities upper))
  | .gaussian action mean variance => actionAffinities action ++ ((sampleAffinities mean) ++ (sampleAffinities variance))
  | .poisson action rate => actionAffinities action ++ ((sampleAffinities rate))
  | .discrete action _ => actionAffinities action
  | .bernoulli action probability => actionAffinities action ++ ((sampleAffinities probability))
  | .exponential action rate => actionAffinities action ++ ((sampleAffinities rate))
  | .beta action alpha betaArg => actionAffinities action ++ ((sampleAffinities alpha) ++ (sampleAffinities betaArg))
  | .gamma action shape rate => actionAffinities action ++ ((sampleAffinities shape) ++ (sampleAffinities rate))

/-- The candidate preserves every constructor and payload, and fills only omitted affinities. -/
def Input.matches : Input → Core → Bool
  | .bvar index, .bvar index' => decide (index = index')
  | .reject, .reject => true
  | .unit, .unit => true
  | .bool value, .bool value' => decide (value = value')
  | .real value, .real value' => decide (value = value')
  | .nil, .nil => true
  | .lam body, .lam body' =>
      body.matches body'
  | .fix body, .fix body' =>
      body.matches body'
  | .app fn arg, .app fn' arg' =>
      fn.matches fn' && arg.matches arg'
  | .pair left right, .pair left' right' =>
      left.matches left' && right.matches right'
  | .fst body, .fst body' =>
      body.matches body'
  | .snd body, .snd body' =>
      body.matches body'
  | .inl body, .inl body' =>
      body.matches body'
  | .inr body, .inr body' =>
      body.matches body'
  | .matchSum scrutinee left right, .matchSum scrutinee' left' right' =>
      scrutinee.matches scrutinee' && left.matches left' && right.matches right'
  | .cons head tail, .cons head' tail' =>
      head.matches head' && tail.matches tail'
  | .matchList scrutinee nilCase consCase, .matchList scrutinee' nilCase' consCase' =>
      scrutinee.matches scrutinee' && nilCase.matches nilCase' && consCase.matches consCase'
  | .ite condition thenBranch elseBranch, .ite condition' thenBranch' elseBranch' =>
      condition.matches condition' && thenBranch.matches thenBranch' && elseBranch.matches elseBranch'
  | .letE value body, .letE value' body' =>
      value.matches value' && body.matches body'
  | .neg body, .neg body' =>
      body.matches body'
  | .add left right, .add left' right' =>
      left.matches left' && right.matches right'
  | .mul left right, .mul left' right' =>
      left.matches left' && right.matches right'
  | .div left right, .div left' right' =>
      left.matches left' && right.matches right'
  | .lt left right, .lt left' right' =>
      left.matches left' && right.matches right'
  | .uniform requested lower upper, .uniform (.sample actual) lower' upper' =>
      (requested.isNone || requested == some actual) && lower.matches lower' && upper.matches upper'
  | .gaussian requested mean variance, .gaussian (.sample actual) mean' variance' =>
      (requested.isNone || requested == some actual) && mean.matches mean' && variance.matches variance'
  | .poisson requested rate, .poisson (.sample actual) rate' =>
      (requested.isNone || requested == some actual) && rate.matches rate'
  | .bernoulli requested probability, .bernoulli (.sample actual) probability' =>
      (requested.isNone || requested == some actual) && probability.matches probability'
  | .exponential requested rate, .exponential (.sample actual) rate' =>
      (requested.isNone || requested == some actual) && rate.matches rate'
  | .beta requested alpha betaArg, .beta (.sample actual) alpha' betaArg' =>
      (requested.isNone || requested == some actual) && alpha.matches alpha' && betaArg.matches betaArg'
  | .gamma requested shape rate, .gamma (.sample actual) shape' rate' =>
      (requested.isNone || requested == some actual) && shape.matches shape' && rate.matches rate'
  | .discrete requested distribution, .discrete (.sample actual) distribution' =>
      (requested.isNone || requested == some actual) && decide (distribution = distribution')
  | _, _ => false

structure Certified (input : Input) where
  source : Core
  ty : Ty
  typed : Typed [] (interpret source) ty
  sourceOnly : source.sourceForm = true
  aligned : input.matches source = true

def certify (input : Input) (candidate : Core) (c : Certificate) :
    Option (Certified input) := do
  if hSource : candidate.sourceForm = true then
    if hAlign : input.matches candidate = true then
      let h ← check [] candidate c.ty c
      return ⟨candidate, c.ty, h.down, hSource, hAlign⟩
    else none
  else none

end Determinize.Checking
