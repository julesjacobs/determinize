import Determinize.Proof.FiniteModel.Boundary
import Determinize.Proof.FiniteModel.Paths

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel MeasureTheory

/-- Change only the observable assigned to a successful return. -/
abbrev rewards (model : Model) (f : Rat → Rat) : Model :=
  {model with kind := fun state => match model.kind state with
    | .returned r => .returned (f r)
    | other => other}

theorem query_sound (model : Model) (f : Rat → Rat) (g : ℝ → ℝ)
    (agree : ∀ r : Rat, g (r : ℝ) = (f r : ℝ))
    (certificate : ResultCertificate (rewards model f))
    (equations : certificate.Equations (rewards model f))
    (paths : Paths (rewards model f)) (valid : paths.Valid (rewards model f)) :
    (∫ x, g x ∂model.outputMeasure) = (certificate.values model.initial : ℝ) := by
  have unique := paths_unique (rewards model f) paths valid
    (fun state => (∫ x, g x ∂outputAt model state) - (certificate.values state : ℝ))
  have zero := unique (by
    intro state
    rw [outputAt_equations model state g]
    have eqs := equations state
    cases h : model.kind state with
    | returned reward => simp [rewards, h] at eqs; simp [rewards, h, agree, eqs]
    | rejected => simp [rewards, h] at eqs; simp [rewards, h, eqs]
    | transient =>
      simp only [rewards, h] at eqs ⊢
      rw [eqs]
      simp [Rat.cast_sum, Rat.cast_mul, Finset.sum_sub_distrib, mul_sub]) model.initial
  exact sub_eq_zero.mp zero

end Determinize.Proof.FiniteModel
