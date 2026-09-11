import Determinize.Proof.FiniteModel.Sampling
import Determinize.Proof.OrdinarySemantics

namespace Determinize.Proof.FiniteModel
open Spec.Paper MeasureTheory

noncomputable def weightedOutput {α : Type} (outcomes : List (Rat × α))
    (continuation : α → Measure ℝ) : Measure ℝ :=
  (outcomes.map fun entry => ENNReal.ofReal (entry.1 : ℝ) • continuation entry.2).sum

theorem outcomeMeasure_bind (outcomes : List (Rat × Rat)) (continuation : ℝ → Measure ℝ)
    (measurable : Measurable continuation) :
    (outcomeMeasure outcomes).bind continuation =
      weightedOutput outcomes (fun q => continuation (q : ℝ)) := by
  induction outcomes with
  | nil => simp [outcomeMeasure, weightedOutput]
  | cons entry rest ih =>
      ext set hs
      rw [Measure.bind_apply hs measurable.aemeasurable]
      simp only [outcomeMeasure, List.map_cons, List.sum_cons, lintegral_add_measure,
        lintegral_smul_measure, lintegral_dirac]
      rw [← Measure.bind_apply hs measurable.aemeasurable]
      change _ + (outcomeMeasure rest).bind continuation set = _
      rw [ih]
      simp [weightedOutput, Measure.add_apply, Measure.smul_apply, smul_eq_mul]

theorem outcomeMeasure_ae (outcomes : List (Rat × Rat)) (predicate : ℝ → Prop) :
    (∀ᵐ x ∂outcomeMeasure outcomes, predicate x) ↔
      ∀ entry ∈ outcomes, 0 < entry.1 → predicate (entry.2 : ℝ) := by
  induction outcomes with
  | nil => simp [outcomeMeasure]
  | cons entry rest ih =>
      simp only [outcomeMeasure, List.map_cons, List.sum_cons, ae_add_measure_iff]
      change _ ∧ (∀ᵐ x ∂outcomeMeasure rest, predicate x) ↔ _
      rw [ih]
      by_cases positive : 0 < entry.1
      · have hp : ENNReal.ofReal (entry.1 : ℝ) ≠ 0 := by
          exact ne_of_gt (ENNReal.ofReal_pos.mpr (by exact_mod_cast positive))
        have scale : (∀ᵐ x ∂ENNReal.ofReal (entry.1 : ℝ) • Measure.dirac (entry.2 : ℝ), predicate x) ↔
            ∀ᵐ x ∂Measure.dirac (entry.2 : ℝ), predicate x := by
          simp [hp]
        rw [scale, ae_dirac_eq]
        simp [positive]
      · have hp : ENNReal.ofReal (entry.1 : ℝ) = 0 := by
          apply ENNReal.ofReal_eq_zero.mpr
          exact_mod_cast le_of_not_gt positive
        simp [hp, positive]

theorem weightedOutput_mono {α : Type} (outcomes : List (Rat × α))
    (left right : α → Measure ℝ)
    (bound : ∀ entry ∈ outcomes, 0 < entry.1 → left entry.2 ≤ right entry.2) :
    weightedOutput outcomes left ≤ weightedOutput outcomes right := by
  induction outcomes with
  | nil => rfl
  | cons entry rest ih =>
      simp only [weightedOutput, List.map_cons, List.sum_cons]
      apply add_le_add
      · by_cases hp : 0 < entry.1
        · exact smul_le_smul_left _ (bound entry (by simp) hp)
        · have zero : ENNReal.ofReal (entry.1 : ℝ) = 0 :=
            ENNReal.ofReal_eq_zero.mpr (by exact_mod_cast le_of_not_gt hp)
          simp [zero]
      · exact ih (fun e he => bound e (by simp [he]))

theorem weightedOutput_uniform_bound {α : Type} (outcomes : List (Rat × α))
    (left : α → Measure ℝ) (right : Nat → α → Measure ℝ)
    (mono : ∀ state, Monotone (fun fuel => right fuel state))
    (bound : ∀ entry ∈ outcomes, 0 < entry.1 → ∃ fuel, left entry.2 ≤ right fuel entry.2) :
    ∃ fuel, weightedOutput outcomes left ≤ weightedOutput outcomes (right fuel) := by
  induction outcomes with
  | nil => exact ⟨0, le_rfl⟩
  | cons entry rest ih =>
      obtain ⟨n, hn⟩ := ih (fun e he => bound e (by simp [he]))
      by_cases positive : 0 < entry.1
      · obtain ⟨m, hm⟩ := bound entry (by simp) positive
        refine ⟨max m n, ?_⟩
        simp only [weightedOutput, List.map_cons, List.sum_cons]
        apply add_le_add
        · exact smul_le_smul_left _ (hm.trans (mono entry.2 (le_max_left _ _)))
        · exact hn.trans (weightedOutput_mono _ _ _ (fun e _ _ => mono e.2 (le_max_right _ _)))
      · refine ⟨n, ?_⟩
        have zero : ENNReal.ofReal (entry.1 : ℝ) = 0 :=
          ENNReal.ofReal_eq_zero.mpr (by exact_mod_cast le_of_not_gt positive)
        simpa [weightedOutput, zero] using hn

end Determinize.Proof.FiniteModel
