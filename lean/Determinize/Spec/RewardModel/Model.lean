import Determinize.Spec.FiniteModel.Model

namespace Determinize.Spec.RewardModel
open MeasureTheory

structure Edge (size : Nat) where
  target : Fin size
  probability : Rat
  reward : Rat

/-- Edge rewards translate successful outputs. Rejection and divergence discard them. -/
structure Model where
  size : Nat
  initial : Fin size
  kind : Fin size → FiniteModel.StateKind
  edges : Fin size → List (Edge size)
  nonnegative : ∀ i e, e ∈ edges i → 0 ≤ e.probability
  normalized : ∀ i, ((edges i).map Edge.probability).sum = 1

def controlWeight (model : Model) (i j : Fin model.size) : Rat :=
  ((model.edges i).map fun e => if e.target = j then e.probability else 0).sum

noncomputable def shift (reward : Rat) (law : Measure ℝ) : Measure ℝ :=
  law.map (fun x => x + (reward : ℝ))

noncomputable def Model.outputWithin (model : Model) : Nat → Fin model.size → Measure ℝ
  | 0, state => match model.kind state with
      | .returned value => Measure.dirac (value : ℝ)
      | _ => 0
  | steps + 1, state => match model.kind state with
      | .returned value => Measure.dirac (value : ℝ)
      | .rejected => 0
      | .transient => ((model.edges state).map fun edge =>
          ENNReal.ofReal (edge.probability : ℝ) •
            shift edge.reward (model.outputWithin steps edge.target)).sum

noncomputable def Model.outputAt (model : Model) (state : Fin model.size) : Measure ℝ :=
  ⨆ steps, model.outputWithin steps state

noncomputable def Model.outputMeasure (model : Model) : Measure ℝ :=
  model.outputAt model.initial

/-- Exact source correspondence includes failure freedom and the entire output law. -/
def Model.Matches (model : Model) (program : Paper.Expr) : Prop :=
  Paper.DomainSafe program ∧ model.outputMeasure = Paper.bigStepMeasure program

end Determinize.Spec.RewardModel
