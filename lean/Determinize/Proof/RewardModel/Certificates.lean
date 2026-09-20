import Determinize.Proof.RewardModel.Boundary
import Determinize.Proof.FiniteModel.Statistics

namespace Determinize.Proof.RewardModel
open Spec.RewardModel FiniteModel

def translatedValue {n : Nat} (values : Moment → Fin n → Rat) (moment : Moment) (e : Edge n) : Rat :=
  match moment with
  | .mass => values .mass e.target
  | .first => values .first e.target + e.reward * values .mass e.target
  | .second => values .second e.target + 2*e.reward*values .first e.target + e.reward^2*values .mass e.target

def MomentEquations (model : Model) (values : Moment → Fin model.size → Rat) : Prop :=
  ∀ moment i, values moment i = match model.kind i with
    | .returned b => moment.rational b
    | .rejected => 0
    | .transient => ((model.edges i).map fun e => e.probability * translatedValue values moment e).sum

instance (model : Model) (values : Moment → Fin model.size → Rat) : Decidable (MomentEquations model values) :=
  inferInstanceAs (Decidable (∀ _ _, _))

end Determinize.Proof.RewardModel
