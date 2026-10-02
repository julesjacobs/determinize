import Determinize.Spec.FiniteModel.Model

/-! # Return, rejection and divergence probabilities of a finite model -/

namespace Determinize.Spec.FiniteModel
open MeasureTheory

/-- Record rejection as the output 1 and discard successful returns. -/
abbrev Model.rejectionModel (model : Model) : Model :=
  {model with kind := fun state ↦ match model.kind state with
    | .transient => .transient | .returned _ => .rejected | .rejected => .returned 1}

/-- The probability that evaluation reaches a rejected state. -/
noncomputable def Model.rejectionProbability (model : Model) : ENNReal :=
  model.rejectionModel.outputMeasure Set.univ

/-- Probability of remaining transient forever; rejection is terminal here. -/
noncomputable def Model.divergenceProbability (model : Model) : ENNReal :=
  ⨅ n, ENNReal.ofReal (model.survivalWithin n model.initial : ℝ)

/-- Rational probabilities of the three ways evaluation can go. -/
structure TerminationStatistics where
  /-- The probability of reaching a returned state. -/
  returnProbability : Rat
  /-- The probability of reaching a rejected state. -/
  rejectionProbability : Rat
  /-- The probability of never reaching a terminal state. -/
  divergenceProbability : Rat
  deriving Repr, BEq, DecidableEq

/-- The statistics are the return, rejection and divergence probabilities of the model. -/
def TerminationStatistics.Matches (statistics : TerminationStatistics) (model : Model) : Prop :=
  (model.outputMeasure Set.univ).toReal = (statistics.returnProbability : ℝ) ∧
    model.rejectionProbability.toReal = (statistics.rejectionProbability : ℝ) ∧
    model.divergenceProbability.toReal = (statistics.divergenceProbability : ℝ)

end Determinize.Spec.FiniteModel
