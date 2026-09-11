import Determinize.Checking.Typing

namespace Determinize.Checking
open Spec.Paper

/-- Forget sample modes, retaining all other syntax. -/
def eraseAnnotations : Core → Core
  | .bvar index => .bvar index
  | .unit => .unit
  | .reject => .reject
  | .discrete _ kind d => .discrete .G kind d
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
  | .uniform _mode kind lower upper => .uniform .G kind ((eraseAnnotations lower)) ((eraseAnnotations upper))
  | .gaussian _mode kind mean variance => .gaussian .G kind ((eraseAnnotations mean)) ((eraseAnnotations variance))
  | .poisson _mode kind rate => .poisson .G kind ((eraseAnnotations rate))
  | .bernoulli _mode kind probability => .bernoulli .G kind ((eraseAnnotations probability))
  | .exponential _mode kind rate => .exponential .G kind ((eraseAnnotations rate))
  | .beta _mode kind alpha betaArg => .beta .G kind ((eraseAnnotations alpha)) ((eraseAnnotations betaArg))
  | .gamma _mode kind shape rate => .gamma .G kind ((eraseAnnotations shape)) ((eraseAnnotations rate))

def sampleModes : Core → List Mode
  | .bvar _ => []
  | .reject => []
  | .unit => []
  | .bool _ => []
  | .real _ => []
  | .lam body => (sampleModes body)
  | .fix body => (sampleModes body)
  | .app fn arg => (sampleModes fn) ++ (sampleModes arg)
  | .pair left right => (sampleModes left) ++ (sampleModes right)
  | .fst pairValue => (sampleModes pairValue)
  | .snd pairValue => (sampleModes pairValue)
  | .inl value => (sampleModes value)
  | .inr value => (sampleModes value)
  | .matchSum scrutinee left right => (sampleModes scrutinee) ++ (sampleModes left) ++ (sampleModes right)
  | .nil => []
  | .cons head tail => (sampleModes head) ++ (sampleModes tail)
  | .matchList scrutinee nilCase consCase => (sampleModes scrutinee) ++ (sampleModes nilCase) ++ (sampleModes consCase)
  | .ite condition thenBranch elseBranch => (sampleModes condition) ++ (sampleModes thenBranch) ++ (sampleModes elseBranch)
  | .letE value body => (sampleModes value) ++ (sampleModes body)
  | .neg body => (sampleModes body)
  | .add left right => (sampleModes left) ++ (sampleModes right)
  | .mul left right => (sampleModes left) ++ (sampleModes right)
  | .div left right => (sampleModes left) ++ (sampleModes right)
  | .lt left right => (sampleModes left) ++ (sampleModes right)
  | .uniform mode _ lower upper => mode :: ((sampleModes lower) ++ (sampleModes upper))
  | .gaussian mode _ mean variance => mode :: ((sampleModes mean) ++ (sampleModes variance))
  | .poisson mode _ rate => mode :: ((sampleModes rate))
  | .discrete mode _ _ => [mode]
  | .bernoulli mode _ probability => mode :: ((sampleModes probability))
  | .exponential mode _ rate => mode :: ((sampleModes rate))
  | .beta mode _ alpha betaArg => mode :: ((sampleModes alpha) ++ (sampleModes betaArg))
  | .gamma mode _ shape rate => mode :: ((sampleModes shape) ++ (sampleModes rate))

def respectsModes : List (Option Mode) → List Mode → Bool
  | [], [] => true
  | requested :: rs, actual :: ms =>
      (requested.isNone || requested == some actual) && respectsModes rs ms
  | _, _ => false

structure Certified (original : Core) (requested : List (Option Mode)) where
  source : Core
  ty : Ty
  typed : Typed [] (interpret source) ty
  sourceOnly : source.sourceForm = true
  aligned : eraseAnnotations source = eraseAnnotations original
  modesRespected : respectsModes requested (sampleModes source) = true

def certify (original candidate : Core) (requested : List (Option Mode)) (c : Certificate) :
    Option (Certified original requested) := do
  if hSource : candidate.sourceForm = true then
    if hAlign : eraseAnnotations candidate = eraseAnnotations original then
      if hModes : respectsModes requested (sampleModes candidate) = true then
        let h ← check [] candidate c.ty c
        return ⟨candidate, c.ty, h.down, hSource, hAlign, hModes⟩
      else none
    else none
  else none

end Determinize.Checking
