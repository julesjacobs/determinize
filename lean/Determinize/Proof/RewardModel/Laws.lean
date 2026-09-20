import Determinize.Proof.RewardModel.Integrability

namespace Determinize.Proof.RewardModel
open MeasureTheory Spec.RewardModel
open scoped ENNReal

private theorem list_measure_apply {α : Type} (xs : List α) (μ : α → Measure ℝ) (s : Set ℝ) :
    (xs.map μ).sum s = (xs.map fun x => μ x s).sum := by
  induction xs with
  | nil => simp
  | cons x xs ih => simp [Measure.add_apply, ih]

private theorem list_sum_iSup {α : Type} (xs : List α) (f : Nat → α → ℝ≥0∞)
    (mono : ∀ a, Monotone (fun n => f n a)) :
    (⨆ n, (xs.map (f n)).sum) = (xs.map fun a => ⨆ n, f n a).sum := by
  induction xs with
  | nil => simp
  | cons a xs ih =>
    simp only [List.map_cons, List.sum_cons]
    rw [← ENNReal.iSup_add_iSup_of_monotone (mono a)
      (fun n m h => List.sum_le_sum (fun x _ => mono x h)), ih]

theorem shift_iSup (r : Rat) (μ : Nat → Measure ℝ) (mono : Monotone μ) :
    shift r (⨆ n, μ n) = ⨆ n, shift r (μ n) := by
  ext s hs
  have meas : Measurable (fun x : ℝ => x+(r:ℝ)) := measurable_id.add_const _
  rw [FiniteModel.monotone_measure_iSup_apply _ (fun _ _ h => shift_mono r (mono h)) s hs]
  simp only [shift, Measure.map_apply meas hs,
    FiniteModel.monotone_measure_iSup_apply μ mono _ (meas hs)]

theorem outputAt_equation (model : Model) (i : Fin model.size) :
    model.outputAt i = match model.kind i with
    | .returned b => Measure.dirac (b : ℝ)
    | .rejected => 0
    | .transient => ((model.edges i).map fun e =>
        ENNReal.ofReal (e.probability : ℝ) • shift e.reward (model.outputAt e.target)).sum := by
  cases kind : model.kind i with
  | returned b =>
    have terminal (n : Nat) : model.outputWithin n i = Measure.dirac (b : ℝ) := by
      cases n <;> simp [Model.outputWithin, kind]
    simp [Model.outputAt, terminal]
  | rejected =>
    have terminal (n : Nat) : model.outputWithin n i = 0 := by
      cases n <;> simp [Model.outputWithin, kind]
    simp [Model.outputAt, terminal]
  | transient =>
    have horizon : model.outputAt i = ⨆ n, model.outputWithin (n+1) i := by
      apply le_antisymm
      · exact iSup_le fun n => (outputWithin_mono model i (Nat.le_succ n)).trans
          (le_iSup (fun n => model.outputWithin (n+1) i) n)
      · exact iSup_le fun n => le_iSup (fun n => model.outputWithin n i) (n+1)
    rw [horizon]
    ext s hs
    rw [FiniteModel.monotone_measure_iSup_apply _
      (fun n m h => outputWithin_mono model i (Nat.add_le_add_right h 1)) s hs]
    simp only [Model.outputWithin, kind, list_measure_apply, Measure.smul_apply, smul_eq_mul]
    simp_rw [Model.outputAt, shift_iSup _ _ (outputWithin_mono model _),
      FiniteModel.monotone_measure_iSup_apply _ (fun _ _ h => shift_mono _ (outputWithin_mono model _ h)) s hs,
      ENNReal.mul_iSup]
    exact list_sum_iSup _ _ (fun e _ _ h => mul_le_mul' le_rfl (shift_mono _ (outputWithin_mono model _ h) s))

theorem outputAt_mass_le_one (model : Model) (i : Fin model.size) : model.outputAt i Set.univ ≤ 1 := by
  rw [Model.outputAt, FiniteModel.monotone_measure_iSup_apply _ (outputWithin_mono model i) _ MeasurableSet.univ]
  exact iSup_le (fun n => outputWithin_mass_le_one model n i)

instance (model : Model) (i : Fin model.size) : IsFiniteMeasure (model.outputAt i) :=
  ⟨(outputAt_mass_le_one model i).trans_lt (by simp)⟩

private theorem integrable_list_sum {α : Type} (xs : List α) (μ : α → Measure ℝ) (f : ℝ → ℝ)
    (integrable : ∀ x ∈ xs, Integrable f (μ x)) : Integrable f (xs.map μ).sum := by
  induction xs with
  | nil => simp
  | cons x xs ih =>
    simp only [List.map_cons, List.sum_cons]
    exact (integrable x (by simp)).add_measure (ih (fun a ha => integrable a (by simp [ha])))

theorem integral_list_sum {α : Type} (xs : List α) (μ : α → Measure ℝ) (f : ℝ → ℝ)
    (integrable : ∀ x ∈ xs, Integrable f (μ x)) :
    (∫ y, f y ∂(xs.map μ).sum) = (xs.map fun x => ∫ y, f y ∂μ x).sum := by
  induction xs with
  | nil => simp
  | cons x xs ih =>
    simp only [List.map_cons, List.sum_cons]
    rw [integral_add_measure (integrable x (by simp))
      (integrable_list_sum xs μ f (fun a ha => integrable a (by simp [ha]))),
      ih (fun a ha => integrable a (by simp [ha]))]

end Determinize.Proof.RewardModel
