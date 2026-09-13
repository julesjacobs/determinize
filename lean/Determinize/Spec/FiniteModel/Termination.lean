import Determinize.Spec.FiniteModel.Model

namespace Determinize.Spec.FiniteModel
open MeasureTheory

/-- Record rejection as the output 1 and discard successful returns. -/
abbrev Model.rejectionModel (model : Model) : Model :=
  {model with kind := fun state => match model.kind state with
    | .transient => .transient | .returned _ => .rejected | .rejected => .returned 1}

noncomputable def Model.rejectionProbability (model : Model) : ENNReal :=
  model.rejectionModel.outputMeasure Set.univ

/-- Probability of remaining transient forever; rejection is terminal here. -/
noncomputable def Model.divergenceProbability (model : Model) : ENNReal :=
  ⨅ n, ENNReal.ofReal (model.survivalWithin n model.initial : ℝ)

def massBalanceThm : Prop :=
  ∀ model : Model, model.outputMeasure Set.univ + model.rejectionProbability + model.divergenceProbability = 1

structure TerminationStatistics where
  returnProbability : Rat
  rejectionProbability : Rat
  divergenceProbability : Rat
  deriving Repr, BEq, DecidableEq

def TerminationStatistics.Matches (statistics : TerminationStatistics) (model : Model) : Prop :=
  (model.outputMeasure Set.univ).toReal = (statistics.returnProbability : ℝ) ∧
    model.rejectionProbability.toReal = (statistics.rejectionProbability : ℝ) ∧
    model.divergenceProbability.toReal = (statistics.divergenceProbability : ℝ)

end Determinize.Spec.FiniteModel
