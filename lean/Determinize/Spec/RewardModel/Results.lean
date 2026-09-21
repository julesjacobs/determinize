import Determinize.Spec.RewardModel.Model
import Determinize.Spec.FiniteModel.Statistics

namespace Determinize.Spec.RewardModel
open MeasureTheory

/-- Every state has finite absolute first and second output moments. -/
def Model.IntegrableMoments (model : Model) : Prop :=
  ∀ i, Integrable (fun x : ℝ => x) (model.outputAt i) ∧
    Integrable (fun x : ℝ => x^2) (model.outputAt i)

/-- Every finite reward model has finite output moments, without extra premises. -/
def finiteIntegrabilityThm : Prop := ∀ model : Model, model.IntegrableMoments

/-- An exported reward result includes integrability, not only integral equalities. -/
def ResultMatches (model : Model) (program : Paper.Expr)
    (statistics : FiniteModel.OutputStatistics) : Prop :=
  model.Matches program ∧
  statistics.Matches model.outputMeasure

end Determinize.Spec.RewardModel
