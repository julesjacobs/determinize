import Determinize.Spec.RewardModel.Model
import Determinize.Spec.FiniteModel.Statistics

namespace Determinize.Spec.RewardModel
open MeasureTheory

/-- Every state has finite absolute first and second output moments. -/
def Model.IntegrableMoments (model : Model) : Prop :=
  ∀ i, Integrable (fun x : ℝ => x) (model.outputAt i) ∧
    Integrable (fun x : ℝ => x^2) (model.outputAt i)

/-- An exported reward result includes integrability, not only integral equalities. -/
def ResultMatches (model : Model) (program : Paper.Expr)
    (statistics : FiniteModel.OutputStatistics) : Prop :=
  model.Matches program ∧
  Integrable (fun x : ℝ => x) model.outputMeasure ∧
  Integrable (fun x : ℝ => x^2) model.outputMeasure ∧
  statistics.Matches model.outputMeasure

end Determinize.Spec.RewardModel
