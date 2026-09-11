import Determinize.Spec.FiniteModel.Model

namespace Determinize.Spec.FiniteModel

/-- Expected reward returned within `steps` transitions; unfinished paths contribute zero. -/
def Model.rewardWithin (model : Model) : Nat → Fin model.size → Rat
  | 0, state => match model.kind state with
      | .returned reward => reward
      | _ => 0
  | steps + 1, state => match model.kind state with
      | .returned reward => reward
      | .rejected => 0
      | .transient => ∑ next, model.transition state next * model.rewardWithin steps next

end Determinize.Spec.FiniteModel

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel MeasureTheory

/-- Increasing the exploration horizon only adds returned output mass. -/
theorem outputWithin_mono (model : Model) (state : Fin model.size) :
    Monotone (fun steps => model.outputWithin steps state) := by
  apply monotone_nat_of_le_succ
  intro steps
  induction steps generalizing state with
  | zero =>
      cases h : model.kind state <;> simp [Model.outputWithin, h]
      exact bot_le
  | succ steps ih =>
      cases h : model.kind state with
      | returned reward => simp [Model.outputWithin, h]
      | rejected => simp [Model.outputWithin, h]
      | transient =>
          simp only [Model.outputWithin, h]
          apply Finset.sum_le_sum
          intro next _
          exact smul_le_smul_left _ (ih next)

theorem returned_outputWithin (model : Model) (state : Fin model.size) (reward : Rat)
    (returned : model.kind state = .returned reward) (steps : Nat) :
    model.outputWithin steps state = Measure.dirac (reward : ℝ) := by
  cases steps <;> simp [Model.outputWithin, returned]

theorem rejected_outputWithin (model : Model) (state : Fin model.size)
    (rejected : model.kind state = .rejected) (steps : Nat) :
    model.outputWithin steps state = 0 := by
  cases steps <;> simp [Model.outputWithin, rejected]

theorem returned_output (model : Model) (reward : Rat)
    (returned : model.kind model.initial = .returned reward) :
    model.outputMeasure = Measure.dirac (reward : ℝ) := by
  simp [Model.outputMeasure, returned_outputWithin model model.initial reward returned]

theorem rejected_output (model : Model)
    (rejected : model.kind model.initial = .rejected) : model.outputMeasure = 0 := by
  simp [Model.outputMeasure, rejected_outputWithin model model.initial rejected]

/-- Every finite-horizon output law has finite support. -/
theorem outputWithin_integrable (model : Model) (steps : Nat) (state : Fin model.size)
    (f : ℝ → ℝ) : Integrable f (model.outputWithin steps state) := by
  induction steps generalizing state with
  | zero =>
      cases h : model.kind state <;> simp [Model.outputWithin, h]
      exact integrable_dirac (by simp)
  | succ steps ih =>
      cases h : model.kind state with
      | returned reward =>
          simpa [Model.outputWithin, h] using
            (integrable_dirac (by simp) : Integrable f (Measure.dirac (reward : ℝ)))
      | rejected => simp [Model.outputWithin, h]
      | transient =>
          simp only [Model.outputWithin, h]
          apply integrable_finsetSum_measure.mpr
          intro next _
          exact (ih next).smul_measure (by simp)

/-- Exact rational reward iteration agrees with the real-measure interpretation. -/
theorem rewardWithin_integral (model : Model) (steps : Nat) (state : Fin model.size) :
    (∫ value : ℝ, value ∂model.outputWithin steps state) =
      (model.rewardWithin steps state : ℝ) := by
  induction steps generalizing state with
  | zero =>
      cases h : model.kind state <;> simp [Model.outputWithin, Model.rewardWithin, h]
  | succ steps ih =>
      cases h : model.kind state with
      | returned reward => simp [Model.outputWithin, Model.rewardWithin, h]
      | rejected => simp [Model.outputWithin, Model.rewardWithin, h]
      | transient =>
          simp only [Model.outputWithin, Model.rewardWithin, h]
          rw [integral_finsetSum_measure (s := Finset.univ)
            (μ := fun next => ENNReal.ofReal (model.transition state next : ℝ) •
              model.outputWithin steps next)
            (fun next _ => (outputWithin_integrable model steps next (fun x => x)).smul_measure
              (by simp))]
          simp only [integral_smul_measure, ih, Rat.cast_sum, Rat.cast_mul, smul_eq_mul]
          apply Finset.sum_congr rfl
          intro next _
          rw [ENNReal.toReal_ofReal (by exact_mod_cast model.nonnegative state next)]

end Determinize.Proof.FiniteModel
