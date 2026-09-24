import Determinize.Frontend.Parser
import Determinize.Frontend.Elaborate
import Determinize.Theorems

namespace Determinize.Frontend
open Spec.Paper

/-- A compiled program: the resolved input, the annotated program and type that `infer` returns,
and the guarantee of `Theorems.inferenceCorrectness` for them. -/
structure Program where
  input : Input
  source : Core
  ty : Ty
  aligned : input.matches source = true
  typed : Typed [] (interpret source) ty

def compile (text : String) : Except String Program := do
  let input ← elaborate (← parse text)
  match inferred : infer input with
  | .ok (source, ty) =>
    have correct := inferred ▸ Theorems.inferenceCorrectness input
    return { input, source, ty, aligned := correct.1, typed := correct.2.1 }
  | .error message => throw message

private def actionAffinities : DistributionAction → List Affinity
  | .sample affinity => [affinity]
  | .mean => []

/-- The affinities of the sample sites, in syntax order. Mean sites have none. -/
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

end Determinize.Frontend
