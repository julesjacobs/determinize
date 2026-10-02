import Determinize.Spec.FiniteModel.Model

/-!
# Reward models

Finite models whose transitions are lists of edges, each paying a reward that is added to the
eventual output.
-/

namespace Determinize.Spec.RewardModel
open MeasureTheory

/-- An outgoing edge of a state. -/
structure Edge (size : Nat) where
  /-- The state the edge leads to. -/
  target : Fin size
  /-- The probability of taking the edge. -/
  probability : Rat
  /-- The reward added to the output when the edge is taken. -/
  reward : Rat

/-- Edge rewards translate successful outputs. Rejection and divergence discard them. -/
structure Model where
  /-- The number of states. -/
  size : Nat
  /-- The state in which evaluation starts. -/
  initial : Fin size
  /-- Whether each state is transient, returns a value, or rejects. -/
  kind : Fin size → FiniteModel.StateKind
  /-- The outgoing edges of each state. -/
  edges : Fin size → List (Edge size)
  nonnegative : ∀ i e, e ∈ edges i → 0 ≤ e.probability
  normalized : ∀ i, ((edges i).map Edge.probability).sum = 1

/-- The probability of moving from state `i` to state `j` in one step: the total probability of
the edges from `i` to `j`. -/
def controlWeight (model : Model) (i j : Fin model.size) : Rat :=
  ((model.edges i).map fun e ↦ if e.target = j then e.probability else 0).sum

/-- The law of `x + reward` when `x` has law `law`. -/
noncomputable def shift (reward : Rat) (law : Measure ℝ) : Measure ℝ :=
  law.map (fun x ↦ x + (reward : ℝ))

/-- Unnormalized output accumulated within `steps` transitions from a state. Taking an edge adds
its reward to the eventual output. -/
noncomputable def Model.outputWithin (model : Model) : Nat → Fin model.size → Measure ℝ
  | 0, state => match model.kind state with
      | .returned value => Measure.dirac (value : ℝ)
      | _ => 0
  | steps + 1, state => match model.kind state with
      | .returned value => Measure.dirac (value : ℝ)
      | .rejected => 0
      | .transient => ((model.edges state).map fun edge ↦
          ENNReal.ofReal (edge.probability : ℝ) •
            shift edge.reward (model.outputWithin steps edge.target)).sum

/-- The output law from a state: the limit of `outputWithin` over the number of steps. -/
noncomputable def Model.outputAt (model : Model) (state : Fin model.size) : Measure ℝ :=
  ⨆ steps, model.outputWithin steps state

/-- The output law of the model: the output law from its initial state. -/
noncomputable def Model.outputMeasure (model : Model) : Measure ℝ :=
  model.outputAt model.initial

/-- Exact source correspondence includes failure freedom and the entire output law. -/
def Model.Matches (model : Model) (program : Paper.Expr) : Prop :=
  Paper.DomainSafe program ∧ model.outputMeasure = Paper.bigStepMeasure program

end Determinize.Spec.RewardModel
