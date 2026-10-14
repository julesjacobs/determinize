import Determinize.Proof.FiniteModel.Boundary
import Determinize.Proof.FiniteModel.Paths

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel MeasureTheory

/-- The value equations of a finite model: a returned state's value is its reward, a rejected
state's is zero, and a transient state's is the expected value of its successors. They need not
determine the expected reward. -/
def ValueEquations (model : Model) (values : Fin model.size → Rat) : Prop :=
  ∀ state, values state = match model.kind state with
    | .returned reward => reward
    | .rejected => 0
    | .transient => ∑ next, model.transition state next * values next

instance (model : Model) (values : Fin model.size → Rat) :
    Decidable (ValueEquations model values) := inferInstanceAs (Decidable (∀ _, _))

/-- Change only the observable assigned to a successful return. -/
abbrev rewards (model : Model) (f : Rat → Rat) : Model :=
  {model with kind := fun state ↦ match model.kind state with
    | .returned r => .returned (f r)
    | other => other}

theorem query_sound (model : Model) (f : Rat → Rat) (g : ℝ → ℝ)
    (agree : ∀ r : Rat, g (r : ℝ) = (f r : ℝ))
    (values : Fin model.size → Rat)
    (equations : ValueEquations (rewards model f) values)
    (paths : Paths (rewards model f)) (valid : paths.Valid (rewards model f)) :
    (∫ x, g x ∂model.outputMeasure) = (values model.initial : ℝ) := by
  have unique := paths_unique (rewards model f) paths valid
    (fun state ↦ (∫ x, g x ∂outputAt model state) - (values state : ℝ))
  have zero := unique (by
    intro state
    rw [outputAt_equations model state g]
    have eqs := equations state
    cases h : model.kind state with
    | returned reward => simp [h] at eqs; simp [h, agree, eqs]
    | rejected => simp [h] at eqs; simp [h, eqs]
    | transient =>
      simp only [rewards, h] at eqs ⊢
      rw [eqs]
      simp [Rat.cast_sum, Rat.cast_mul, Finset.sum_sub_distrib, mul_sub]) model.initial
  exact sub_eq_zero.mp zero

end Determinize.Proof.FiniteModel
