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
  | .discrete action p => actionAffinities action ++ sampleAffinities p
  | .bernoulli action probability => actionAffinities action ++ ((sampleAffinities probability))
  | .exponential action rate => actionAffinities action ++ ((sampleAffinities rate))
  | .beta action alpha betaArg => actionAffinities action ++ ((sampleAffinities alpha) ++ (sampleAffinities betaArg))
  | .gamma action shape rate => actionAffinities action ++ ((sampleAffinities shape) ++ (sampleAffinities rate))


structure Certified (input : Input) where
  source : Core
  ty : Ty
  typed : Typed [] (interpret source) ty
  aligned : input.matches source = true

def certify (input : Input) (candidate : Core) (c : Certificate) :
    Option (Certified input) := do
  if hAlign : input.matches candidate = true then
    let h ← check [] candidate c.ty c
    return ⟨candidate, c.ty, h.down, hAlign⟩
  else none

end Determinize.Checking
