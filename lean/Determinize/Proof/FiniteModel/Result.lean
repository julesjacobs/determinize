import Determinize.Proof.FiniteModel.Model
import Determinize.Statement.FiniteModel.Certificates
import Mathlib.Data.ENNReal.BigOperators

namespace Determinize.Proof.FiniteModel
open Statement.FiniteModel MeasureTheory

private theorem monotone_measure_iSup_apply (μ : Nat → Measure ℝ) (mono : Monotone μ)
    (s : Set ℝ) (hs : MeasurableSet s) : (⨆ n, μ n) s = ⨆ n, μ n s := by
  let ν : Measure ℝ := Measure.ofMeasurable (fun s _ => ⨆ n, μ n s) (by simp) (by
    intro f hf hd
    simp_rw [measure_iUnion hd hf, ENNReal.tsum_eq_iSup_sum]
    rw [iSup_comm]
    congr 1
    funext t
    exact (ENNReal.finsetSum_iSup_of_monotone (fun i a b hab => mono hab (f i))).symm)
  have ν_apply (t : Set ℝ) (ht : MeasurableSet t) : ν t = ⨆ n, μ n t :=
    Measure.ofMeasurable_apply t ht
  have eq : (⨆ n, μ n) = ν := by
    apply le_antisymm
    · apply iSup_le
      intro n
      apply Measure.le_iff.mpr
      intro t ht
      rw [ν_apply t ht]
      exact le_iSup (fun n => μ n t) n
    · apply Measure.le_iff.mpr
      intro t ht
      rw [ν_apply t ht]
      exact iSup_le (fun n => (le_iSup μ n) t)
  rw [eq, ν_apply s hs]

private noncomputable def terminalBound (model : Model) : Measure ℝ :=
  ∑ state, match model.kind state with
    | .returned reward => Measure.dirac (reward : ℝ)
    | _ => 0

private theorem terminalBound_integrable (model : Model) (f : ℝ → ℝ) :
    Integrable f (terminalBound model) := by
  apply integrable_finsetSum_measure.mpr
  intro state _
  cases model.kind state <;> simp
  exact integrable_dirac (by simp)

private theorem transition_mass (model : Model) (state : Fin model.size) :
    ∑ next, ENNReal.ofReal (model.transition state next : ℝ) = 1 := by
  rw [← ENNReal.ofReal_sum_of_nonneg (fun i _ => by exact_mod_cast model.nonnegative state i)]
  have h : ∑ i, (model.transition state i : ℝ) = 1 := by
    exact_mod_cast model.normalized state
  simp [h]

private theorem outputWithin_bound (model : Model) (n : Nat) (state : Fin model.size) :
    model.outputWithin n state ≤ terminalBound model := by
  have terminal (state : Fin model.size) (q : Rat) (h : model.kind state = .returned q) :
      Measure.dirac (q : ℝ) ≤ terminalBound model := by
    have := Finset.single_le_sum (fun i (_ : i ∈ Finset.univ) =>
      Measure.zero_le (match model.kind i with | .returned r => Measure.dirac (r : ℝ) | _ => 0))
      (Finset.mem_univ state)
    simpa [terminalBound, h] using this
  induction n generalizing state with
  | zero =>
      cases h : model.kind state <;> simp only [Model.outputWithin, h]
      · exact Measure.zero_le _
      · exact terminal state _ h
      · exact Measure.zero_le _
  | succ n ih =>
      cases h : model.kind state with
      | returned q => simpa [Model.outputWithin, h] using terminal state q h
      | rejected => simpa [Model.outputWithin, h] using Measure.zero_le (terminalBound model)
      | transient =>
          simp only [Model.outputWithin, h]
          calc
            _ ≤ ∑ next, ENNReal.ofReal (model.transition state next : ℝ) • terminalBound model :=
              Finset.sum_le_sum (fun next _ => smul_le_smul_left _ (ih next))
            _ = terminalBound model := by rw [← Finset.sum_smul, transition_mass, one_smul]

private noncomputable def outputAt (model : Model) (state : Fin model.size) : Measure ℝ :=
  ⨆ n, model.outputWithin n state

private theorem outputAt_integrable (model : Model) (state : Fin model.size) :
    Integrable id (outputAt model state) :=
  (terminalBound_integrable model id).mono_measure (iSup_le (fun n => outputWithin_bound model n state))

/-- Finite terminal rewards bound the output law, including for nonabsorbing models. -/
theorem outputMeasure_integrable (model : Model) : Integrable id model.outputMeasure :=
  outputAt_integrable model model.initial

private theorem outputAt_transient (model : Model) (state : Fin model.size)
    (h : model.kind state = .transient) :
    outputAt model state = ∑ next, ENNReal.ofReal (model.transition state next : ℝ) •
      outputAt model next := by
  have shift : outputAt model state = ⨆ n, model.outputWithin (n + 1) state := by
    apply le_antisymm
    · exact iSup_le (fun n => (outputWithin_mono model state (Nat.le_succ n)).trans
        (le_iSup (fun n => model.outputWithin (n + 1) state) n))
    · exact iSup_le (fun n => le_iSup (fun n => model.outputWithin n state) (n + 1))
  ext s hs
  rw [shift, monotone_measure_iSup_apply _ (by intro a b hab; exact outputWithin_mono model state (Nat.add_le_add_right hab 1)) s hs]
  simp only [Model.outputWithin, h, Measure.finsetSum_apply, Measure.smul_apply, smul_eq_mul]
  simp_rw [outputAt, monotone_measure_iSup_apply _ (outputWithin_mono model _) s hs,
    ENNReal.mul_iSup]
  symm
  apply ENNReal.finsetSum_iSup_of_monotone
  intro i a b hab
  exact mul_le_mul_right (outputWithin_mono model i hab s) _

private theorem homogeneous_bound (model : Model) (d : Fin model.size → ℝ)
    (eqs : ∀ state, d state = if model.kind state = .transient then
      ∑ next, (model.transition state next : ℝ) * d next else 0)
    (M : ℝ) (bound : ∀ state, |d state| ≤ M) (n : Nat) (state : Fin model.size) :
    |d state| ≤ M * (model.survivalWithin n state : ℝ) := by
  induction n generalizing state with
  | zero =>
      by_cases h : model.kind state = .transient
      · simpa [Model.survivalWithin, h] using bound state
      · rw [eqs state]
        simp [Model.survivalWithin, h]
  | succ n ih =>
      by_cases h : model.kind state = .transient
      · rw [eqs state, if_pos h]
        calc
          _ ≤ ∑ next, |(model.transition state next : ℝ) * d next| :=
            Finset.abs_sum_le_sum_abs _ _
          _ = ∑ next, (model.transition state next : ℝ) * |d next| := by
            apply Finset.sum_congr rfl
            intro next _
            rw [abs_mul, abs_of_nonneg (by exact_mod_cast model.nonnegative state next)]
          _ ≤ ∑ next, (model.transition state next : ℝ) *
              (M * (model.survivalWithin n next : ℝ)) :=
            Finset.sum_le_sum (fun next _ => mul_le_mul_of_nonneg_left (ih next)
              (by exact_mod_cast model.nonnegative state next))
          _ = _ := by simp [Model.survivalWithin, h, Rat.cast_sum, Rat.cast_mul,
            Finset.mul_sum, mul_left_comm]
      · rw [eqs state]
        simp [Model.survivalWithin, h]

private theorem homogeneous_unique (model : Model) (certificate : ResultCertificate model)
    (absorption : certificate.Absorption model) (d : Fin model.size → ℝ)
    (eqs : ∀ state, d state = if model.kind state = .transient then
      ∑ next, (model.transition state next : ℝ) * d next else 0) : ∀ state, d state = 0 := by
  obtain ⟨largest, _, largest_bound⟩ := Finset.exists_max_image Finset.univ
    (fun state => |d state|) ⟨model.initial, Finset.mem_univ _⟩
  have bound := homogeneous_bound model d eqs |d largest|
    (fun state => largest_bound state (Finset.mem_univ _)) certificate.horizon largest
  have survival : (model.survivalWithin certificate.horizon largest : ℝ) < 1 := by
    exact_mod_cast absorption largest
  have zero : |d largest| = 0 := by
    nlinarith [abs_nonneg (d largest)]
  intro state
  apply abs_eq_zero.mp
  exact le_antisymm (zero ▸ largest_bound state (Finset.mem_univ _)) (abs_nonneg _)

private theorem outputAt_equations (model : Model) (state : Fin model.size) :
    (∫ value : ℝ, value ∂outputAt model state) = match model.kind state with
      | .returned reward => (reward : ℝ)
      | .rejected => 0
      | .transient => ∑ next, (model.transition state next : ℝ) *
          (∫ value : ℝ, value ∂outputAt model next) := by
  cases h : model.kind state with
  | returned reward => simp [outputAt, returned_outputWithin model state reward h]
  | rejected => simp [outputAt, rejected_outputWithin model state h]
  | transient =>
      rw [outputAt_transient model state h]
      rw [integral_finsetSum_measure (f := fun x : ℝ => x) (s := Finset.univ)
        (μ := fun next => ENNReal.ofReal (model.transition state next : ℝ) • outputAt model next)
        (fun next _ =>
        (outputAt_integrable model next).smul_measure (by simp))]
      simp only [integral_smul_measure, smul_eq_mul]
      apply Finset.sum_congr rfl
      intro next _
      rw [ENNReal.toReal_ofReal (by exact_mod_cast model.nonnegative state next)]

/-- Absorption rules out spurious solutions of the value equations. -/
theorem resultCertificate_sound (model : Model) (certificate : ResultCertificate model)
    (valid : certificate.Valid model) :
    model.expectedReward = (certificate.values model.initial : ℝ) := by
  have unique := homogeneous_unique model certificate valid.2
    (fun state => (∫ value : ℝ, value ∂outputAt model state) - (certificate.values state : ℝ))
  have zero := unique (by
    intro state
    rw [outputAt_equations]
    have eqs := valid.1 state
    cases h : model.kind state with
    | returned reward => simp only [h] at eqs ⊢; simp [eqs]
    | rejected => simp only [h] at eqs ⊢; simp [eqs]
    | transient =>
        simp only [h] at eqs ⊢
        rw [eqs]
        simp [Rat.cast_sum, Rat.cast_mul, Finset.sum_sub_distrib, mul_sub]) model.initial
  exact sub_eq_zero.mp zero

end Determinize.Proof.FiniteModel
