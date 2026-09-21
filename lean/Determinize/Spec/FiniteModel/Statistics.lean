import Determinize.Spec.FiniteModel.Model

namespace Determinize.Spec.FiniteModel
open MeasureTheory

structure OutputStatistics where
  returnMass : Rat
  firstMoment : Rat
  secondMoment : Rat
  deriving Repr, BEq, DecidableEq

def OutputStatistics.conditionalMean (statistics : OutputStatistics) : Option Rat :=
  if statistics.returnMass = 0 then none else some (statistics.firstMoment / statistics.returnMass)

def OutputStatistics.conditionalVariance (statistics : OutputStatistics) : Option Rat :=
  if statistics.returnMass = 0 then none else
    some (statistics.secondMoment / statistics.returnMass -
      (statistics.firstMoment / statistics.returnMass) ^ 2)

/-- Finite mass and genuine first and second moments of the complete output law. -/
structure OutputStatistics.Matches (statistics : OutputStatistics) (law : Measure ℝ) : Prop where
  finite : IsFiniteMeasure law
  squareIntegrable : Integrable (fun x : ℝ => x ^ 2) law
  mass : law.real Set.univ = (statistics.returnMass : ℝ)
  first : (∫ x : ℝ, x ∂law) = (statistics.firstMoment : ℝ)
  second : (∫ x : ℝ, x ^ 2 ∂law) = (statistics.secondMoment : ℝ)

end Determinize.Spec.FiniteModel
