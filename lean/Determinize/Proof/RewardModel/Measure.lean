import Determinize.Spec.RewardModel.Model
import Determinize.Proof.FiniteModel.Result

namespace Determinize.Proof.RewardModel
open Spec.RewardModel MeasureTheory
open scoped ENNReal

private theorem measurable_translation (r : Rat) : Measurable (fun x : ℝ => x + (r : ℝ)) :=
  measurable_id.add_const _

@[simp] theorem shift_zero_measure (r : Rat) : shift r 0 = 0 := by
  simp [shift]

@[simp] theorem shift_zero (μ : Measure ℝ) : shift 0 μ = μ := by
  simp [shift]

theorem shift_add (r s : Rat) (μ : Measure ℝ) :
    shift r (shift s μ) = shift (r + s) μ := by
  unfold shift
  rw [Measure.map_map (measurable_translation r) (measurable_translation s)]
  congr 1
  funext x
  simp only [Function.comp_apply, Rat.cast_add]
  ring

theorem shift_mono (r : Rat) {μ ν : Measure ℝ} (h : μ ≤ ν) : shift r μ ≤ shift r ν :=
  Measure.map_mono h (measurable_translation r)

@[simp] theorem shift_dirac (r x : Rat) :
    shift r (Measure.dirac (x : ℝ)) = Measure.dirac ((x + r : Rat) : ℝ) := by
  simp [shift, Measure.map_dirac, Rat.cast_add]

theorem shift_add_measure (r : Rat) (μ ν : Measure ℝ) :
    shift r (μ + ν) = shift r μ + shift r ν := by
  exact Measure.map_add _ _ (measurable_translation r)

theorem shift_smul (r : Rat) (p : ℝ≥0∞) (μ : Measure ℝ) :
    shift r (p • μ) = p • shift r μ := by
  exact Measure.map_smul p μ _

theorem outputWithin_mono (model : Model) (state : Fin model.size) :
    Monotone (fun n => model.outputWithin n state) := by
  apply monotone_nat_of_le_succ
  intro n
  induction n generalizing state with
  | zero =>
      cases h : model.kind state <;> simp [Model.outputWithin, h]
      exact bot_le
  | succ n ih =>
      cases h : model.kind state with
      | returned value => simp [Model.outputWithin, h]
      | rejected => simp [Model.outputWithin, h]
      | transient =>
        simp only [Model.outputWithin, h]
        apply List.sum_le_sum
        intro a ha
        exact smul_le_smul_left _ (shift_mono _ (ih a.target))

end Determinize.Proof.RewardModel
