import Determinize.Statement.Semantics
import Mathlib.Algebra.BigOperators.Ring.Finset

namespace Determinize.Statement.FiniteModel
open MeasureTheory

inductive StateKind where
  | transient
  | returned (reward : Rat)
  | rejected
deriving DecidableEq, Repr

/-- Terminal states are absorbing. Rewards are paid once, on return. -/
structure Model where
  size : Nat
  initial : Fin size
  kind : Fin size → StateKind
  transition : Fin size → Fin size → Rat
  nonnegative : ∀ i j, 0 ≤ transition i j
  normalized : ∀ i, ∑ j, transition i j = 1
  absorbing : ∀ i, kind i ≠ .transient → ∀ j, transition i j = if i = j then 1 else 0

/-- Probability that evaluation is still transient after at most `steps` transitions. -/
def Model.survivalWithin (model : Model) : Nat → Fin model.size → Rat
  | 0, state => if model.kind state = .transient then 1 else 0
  | steps + 1, state =>
      if model.kind state = .transient then
        ∑ next, model.transition state next * model.survivalWithin steps next
      else 0

/-- Expected reward returned within `steps` transitions; unfinished paths contribute zero. -/
def Model.rewardWithin (model : Model) : Nat → Fin model.size → Rat
  | 0, state => match model.kind state with
      | .returned reward => reward
      | _ => 0
  | steps + 1, state => match model.kind state with
      | .returned reward => reward
      | .rejected => 0
      | .transient => ∑ next, model.transition state next * model.rewardWithin steps next

/-- Unnormalized real output accumulated within `steps` transitions. -/
noncomputable def Model.outputWithin (model : Model) : Nat → Fin model.size → Measure ℝ
  | 0, state => match model.kind state with
      | .returned reward => Measure.dirac (reward : ℝ)
      | _ => 0
  | steps + 1, state => match model.kind state with
      | .returned reward => Measure.dirac (reward : ℝ)
      | .rejected => 0
      | .transient => ∑ next, ENNReal.ofReal (model.transition state next : ℝ) •
          model.outputWithin steps next

/-- Rejection and nontermination contribute no output mass. There is no conditioning. -/
noncomputable def Model.outputMeasure (model : Model) : Measure ℝ :=
  ⨆ steps, model.outputWithin steps model.initial

/-- Integrability is explicit so Lean's default integral of a nonintegrable function
cannot be mistaken for a certified answer. -/
def Model.HasExpectedReward (model : Model) (answer : Rat) : Prop :=
  Integrable id model.outputMeasure ∧
    (∫ value : ℝ, value ∂model.outputMeasure) = (answer : ℝ)

/-- The model represents the entire output law of this exact core program, and
accepted programs cannot get stuck. Rejection and divergence remain possible. -/
def Model.Matches (model : Model) (program : Paper.Expr) : Prop :=
  Paper.DoesNotGetStuck program ∧ model.outputMeasure = Paper.bigStepMeasure program

end Determinize.Statement.FiniteModel
