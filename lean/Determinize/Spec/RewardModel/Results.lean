import Determinize.Spec.RewardModel.Model
import Determinize.Spec.FiniteModel.Statistics

namespace Determinize.Spec.RewardModel
open MeasureTheory

/-- An exported reward result includes integrability, not only integral equalities. -/
def ResultMatches (model : Model) (program : Paper.Expr)
    (statistics : FiniteModel.OutputStatistics) : Prop :=
  model.Matches program ∧
  Integrable (fun x : ℝ => x) model.outputMeasure ∧
  Integrable (fun x : ℝ => x^2) model.outputMeasure ∧
  statistics.Matches model.outputMeasure

end Determinize.Spec.RewardModel
