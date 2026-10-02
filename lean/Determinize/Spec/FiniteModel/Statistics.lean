import Determinize.Spec.FiniteModel.Model

/-! # Rational statistics of an output law -/

namespace Determinize.Spec.FiniteModel
open MeasureTheory

/-- Rational mass and first and second moments of an unnormalized output law. -/
structure OutputStatistics where
  /-- The total mass: the probability of returning. -/
  returnMass : Rat
  /-- The integral of the output. -/
  firstMoment : Rat
  /-- The integral of the squared output. -/
  secondMoment : Rat
  deriving Repr, BEq, DecidableEq

/-- The mean of the output conditioned on returning; `none` when the return mass is zero. -/
def OutputStatistics.conditionalMean (statistics : OutputStatistics) : Option Rat :=
  if statistics.returnMass = 0 then none else some (statistics.firstMoment / statistics.returnMass)

/-- The variance of the output conditioned on returning; `none` when the return mass is zero. -/
def OutputStatistics.conditionalVariance (statistics : OutputStatistics) : Option Rat :=
  if statistics.returnMass = 0 then none else
    some (statistics.secondMoment / statistics.returnMass -
      (statistics.firstMoment / statistics.returnMass) ^ 2)

/-- Finite mass and genuine first and second moments of the complete output law. -/
structure OutputStatistics.Matches (statistics : OutputStatistics) (law : Measure ℝ) : Prop where
  finite : IsFiniteMeasure law
  square_integrable : Integrable (fun x : ℝ ↦ x ^ 2) law
  mass : law.real Set.univ = (statistics.returnMass : ℝ)
  first : (∫ x : ℝ, x ∂law) = (statistics.firstMoment : ℝ)
  second : (∫ x : ℝ, x ^ 2 ∂law) = (statistics.secondMoment : ℝ)

end Determinize.Spec.FiniteModel
