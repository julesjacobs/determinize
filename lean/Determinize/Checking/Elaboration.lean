import Determinize.Checking.Typing

namespace Determinize.Checking
open Spec.Paper

private def actionAffinities : DistributionAction → List Affinity
  | .sample affinity => [affinity]
  | .mean => []

/-- Forget sample affinities, retaining all other syntax. -/
def eraseAnnotations : Core → Core
  | .bvar index => .bvar index
  | .unit => .unit
  | .reject => .reject
  | .discrete action d => .discrete (setAffinity action .G) d
  | .bool value => .bool value
  | .real value => .real value
  | .lam body => .lam ((eraseAnnotations body))
  | .fix body => .fix ((eraseAnnotations body))
  | .app fn arg => .app ((eraseAnnotations fn)) ((eraseAnnotations arg))
  | .pair left right => .pair ((eraseAnnotations left)) ((eraseAnnotations right))
  | .fst pairValue => .fst ((eraseAnnotations pairValue))
  | .snd pairValue => .snd ((eraseAnnotations pairValue))
  | .inl value => .inl ((eraseAnnotations value))
  | .inr value => .inr ((eraseAnnotations value))
  | .matchSum scrutinee left right => .matchSum ((eraseAnnotations scrutinee)) ((eraseAnnotations left)) ((eraseAnnotations right))
  | .nil => .nil
  | .cons head tail => .cons ((eraseAnnotations head)) ((eraseAnnotations tail))
  | .matchList scrutinee nilCase consCase => .matchList ((eraseAnnotations scrutinee)) ((eraseAnnotations nilCase)) ((eraseAnnotations consCase))
  | .ite condition thenBranch elseBranch => .ite ((eraseAnnotations condition)) ((eraseAnnotations thenBranch)) ((eraseAnnotations elseBranch))
  | .letE value body => .letE ((eraseAnnotations value)) ((eraseAnnotations body))
  | .neg body => .neg ((eraseAnnotations body))
  | .add left right => .add ((eraseAnnotations left)) ((eraseAnnotations right))
  | .mul left right => .mul ((eraseAnnotations left)) ((eraseAnnotations right))
  | .div left right => .div ((eraseAnnotations left)) ((eraseAnnotations right))
  | .lt left right => .lt ((eraseAnnotations left)) ((eraseAnnotations right))
  | .uniform action lower upper => .uniform (setAffinity action .G) ((eraseAnnotations lower)) ((eraseAnnotations upper))
  | .gaussian action mean variance => .gaussian (setAffinity action .G) ((eraseAnnotations mean)) ((eraseAnnotations variance))
  | .poisson action rate => .poisson (setAffinity action .G) ((eraseAnnotations rate))
  | .bernoulli action probability => .bernoulli (setAffinity action .G) ((eraseAnnotations probability))
  | .exponential action rate => .exponential (setAffinity action .G) ((eraseAnnotations rate))
  | .beta action alpha betaArg => .beta (setAffinity action .G) ((eraseAnnotations alpha)) ((eraseAnnotations betaArg))
  | .gamma action shape rate => .gamma (setAffinity action .G) ((eraseAnnotations shape)) ((eraseAnnotations rate))

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

def respectsAffinities : List (Option Affinity) → List Affinity → Bool
  | [], [] => true
  | requested :: rs, actual :: ms =>
      (requested.isNone || requested == some actual) && respectsAffinities rs ms
  | _, _ => false

structure Certified (original : Core) (requested : List (Option Affinity)) where
  source : Core
  ty : Ty
  typed : Typed [] (interpret source) ty
  sourceOnly : source.sourceForm = true
  aligned : eraseAnnotations source = eraseAnnotations original
  affinitiesRespected : respectsAffinities requested (sampleAffinities source) = true

def certify (original candidate : Core) (requested : List (Option Affinity)) (c : Certificate) :
    Option (Certified original requested) := do
  if hSource : candidate.sourceForm = true then
    if hAlign : eraseAnnotations candidate = eraseAnnotations original then
      if hAffinities : respectsAffinities requested (sampleAffinities candidate) = true then
        let h ← check [] candidate c.ty c
        return ⟨candidate, c.ty, h.down, hSource, hAlign, hAffinities⟩
      else none
    else none
  else none

end Determinize.Checking
