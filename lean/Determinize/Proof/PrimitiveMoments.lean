import Determinize.Proof.PrimitiveKernels
import Mathlib.Probability.Kernel.Composition.IntegralCompProd

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper

noncomputable section

/-- First absolute moments grow at most linearly in the affine parameters. -/
def PrimitiveMomentBounds (laws : PrimitiveLaws) : Prop :=
  ∀ op general, ∃ bound : ℝ, 0 ≤ bound ∧
    ∀ affine, domain op (affine, general) →
      (∫ value : ℝ, |value| ∂laws.kernel op (affine, general)) ≤
        bound * (1 + ∑ i, |affine i|)

theorem gamma_nonnegative (shape rate : ℝ) :
    ∀ᵐ value ∂gammaMeasure shape rate, 0 ≤ value := by
  rw [ae_iff]
  simp only [not_le]
  change gammaMeasure shape rate (Set.Iio 0) = 0
  rw [gammaMeasure, withDensity_apply _ measurableSet_Iio]
  exact lintegral_gammaPDF_of_nonpos le_rfl

theorem poisson_nonnegative (rate : NNReal) :
    ∀ᵐ value ∂(poissonMeasure rate).map (fun n : Nat => (n : ℝ)), 0 ≤ value := by
  exact ae_map_iff (by fun_prop) measurableSet_Ici |>.2 (ae_of_all _ fun _ => Nat.cast_nonneg _)

theorem uniform_abs_bound (lower upper : ℝ) (ordered : lower ≤ upper) :
    ∀ᵐ value ∂uniformMeasure lower upper, |value| ≤ |lower| + |upper| := by
  unfold uniformMeasure
  rw [dif_pos ordered]
  split
  · rename_i point
    simp only [ae_dirac_eq, Filter.eventually_pure]
    exact le_add_of_nonneg_right (abs_nonneg _)
  · apply Measure.ae_smul_measure
    filter_upwards [ae_restrict_mem measurableSet_Icc] with value member
    exact abs_le.mpr ⟨by linarith [neg_abs_le lower, abs_nonneg upper, member.1],
      by linarith [le_abs_self upper, abs_nonneg lower, member.2]⟩

theorem gaussian_abs_bound (mean : ℝ) (variance : NNReal) :
    (∫ value : ℝ, |value| ∂gaussianReal mean variance) ≤
      (∫ value : ℝ, |value| ∂gaussianReal 0 variance) + |mean| := by
  have shift : (gaussianReal 0 variance).map (fun x => x + mean) =
      gaussianReal mean variance := by simpa using gaussianReal_map_add_const (μ := 0) (v := variance) mean
  rw [← shift, integral_map (by fun_prop) (by fun_prop)]
  have integrable : Integrable (fun x : ℝ => x) (gaussianReal 0 variance) := IsGaussian.integrable_id
  calc
    _ ≤ ∫ value : ℝ, |value| + |mean| ∂gaussianReal 0 variance := by
      apply integral_mono
      · exact (integrable.add (integrable_const mean)).abs
      · exact integrable.abs.add (integrable_const _)
      · exact fun value => abs_add_le value mean
    _ = _ := by rw [integral_add integrable.abs (integrable_const _), integral_const]; simp

theorem primitiveMomentBounds : PrimitiveMomentBounds primitiveLaws := by
  intro op general
  have nonnegMoment (affine) : 0 ≤ ∫ x : ℝ, |x| ∂primitiveLaws.kernel op (affine, general) :=
    integral_nonneg fun _ => abs_nonneg _
  cases op with
  | exponential =>
      let affine : Fin (affineArity .exponential) → ℝ := Fin.elim0
      refine ⟨(∫ x : ℝ, |x| ∂primitiveLaws.kernel .exponential (affine, general)), nonnegMoment affine, ?_⟩
      intro actual _
      have eq : actual = affine := by funext i; exact Fin.elim0 i
      rw [eq]
      simp
  | beta =>
      let affine : Fin (affineArity .beta) → ℝ := Fin.elim0
      refine ⟨(∫ x : ℝ, |x| ∂primitiveLaws.kernel .beta (affine, general)), nonnegMoment affine, ?_⟩
      intro actual _
      have eq : actual = affine := by funext i; exact Fin.elim0 i
      rw [eq]
      simp
  | uniform =>
      refine ⟨1, zero_le_one, ?_⟩
      intro affine valid
      have mass := primitiveLaws.mass_one .uniform (affine, general) valid
      let : IsProbabilityMeasure (primitiveLaws.kernel .uniform (affine, general)) := ⟨mass⟩
      have bound : ∀ᵐ value ∂primitiveLaws.kernel .uniform (affine, general),
          |value| ≤ |affine 0| + |affine 1| := by
        rw [primitiveLaws.kernel_eq_paperMeasure]
        exact uniform_abs_bound _ _ valid
      have inequality := integral_mono_ae
        (primitiveLaws.integrable_id .uniform (affine, general) valid).abs
        (integrable_const _) bound
      simp only [integral_const, probReal_univ, one_smul] at inequality
      have sumEq : (∑ i, |affine i|) = |affine 0| + |affine 1| := by
        change (∑ i : Fin 2, |affine i|) = _
        exact Fin.sum_univ_two _
      rw [sumEq, one_mul]
      linarith
  | gaussian =>
      let variance : NNReal := ⟨max (general 0) 0, le_max_right _ _⟩
      let c := ∫ value : ℝ, |value| ∂gaussianReal 0 variance
      have hc : 0 ≤ c := integral_nonneg fun _ => abs_nonneg _
      refine ⟨c + 1, by positivity, ?_⟩
      intro affine valid
      have hv : 0 ≤ general 0 := valid
      have eq : variance = ⟨general 0, hv⟩ := by
        apply Subtype.ext
        exact max_eq_left hv
      rw [primitiveLaws.kernel_eq_paperMeasure, paperMeasure, dif_pos hv, ← eq]
      have bound := gaussian_abs_bound (affine 0) variance
      change _ ≤ (c + 1) * (1 + ∑ i : Fin 1, |affine i|)
      rw [Fin.sum_univ_one]
      dsimp only [c] at hc ⊢
      change _ ≤ (∫ value : ℝ, |value| ∂gaussianReal 0 variance + 1) * (1 + |affine 0|)
      nlinarith [abs_nonneg (affine 0)]
  | poisson =>
      refine ⟨1, zero_le_one, ?_⟩
      intro affine valid
      have nonnegative : ∀ᵐ value ∂primitiveLaws.kernel .poisson (affine, general), 0 ≤ value := by
        rw [primitiveLaws.kernel_eq_paperMeasure]
        change ∀ᵐ value ∂(if h : 0 ≤ affine 0 then
          (poissonMeasure ⟨affine 0, h⟩).map (fun n : Nat => (n : ℝ)) else 0), 0 ≤ value
        rw [dif_pos (show 0 ≤ affine 0 from valid)]
        exact poisson_nonnegative _
      have meanEq : meanValue .poisson (affine, general) = affine 0 := rfl
      have sumEq : (∑ i, |affine i|) = |affine 0| := Fin.sum_univ_one _
      rw [integral_congr_ae (nonnegative.mono fun value h => abs_of_nonneg h),
        primitiveLaws.mean_law .poisson (affine, general) valid, meanEq, sumEq, one_mul]
      linarith [le_abs_self (affine 0)]
  | gamma =>
      refine ⟨|1 / general 0| + 1, by positivity, ?_⟩
      intro affine valid
      have nonnegative : ∀ᵐ value ∂primitiveLaws.kernel .gamma (affine, general), 0 ≤ value := by
        rw [primitiveLaws.kernel_eq_paperMeasure]
        change ∀ᵐ value ∂(if 0 < affine 0 ∧ 0 < general 0 then
          gammaMeasure (affine 0) (general 0) else 0), 0 ≤ value
        rw [if_pos (show 0 < affine 0 ∧ 0 < general 0 from valid)]
        exact gamma_nonnegative _ _
      rw [integral_congr_ae (nonnegative.mono fun value h => abs_of_nonneg h),
        primitiveLaws.mean_law .gamma (affine, general) valid]
      rw [meanValue_eq_affine]
      change (0 + ∑ i : Fin 1, 1 / general 0 * affine i) ≤
        (|1 / general 0| + 1) * (1 + ∑ i : Fin 1, |affine i|)
      simp only [Fin.sum_univ_one, zero_add]
      have bound : 1 / general 0 * affine 0 ≤
          |1 / general 0| * |affine 0| := by
        rw [← abs_mul]
        exact le_abs_self _
      change 1 / general 0 * affine 0 ≤
        (|1 / general 0| + 1) * (1 + |affine 0|)
      nlinarith [abs_nonneg (1 / general 0), abs_nonneg (affine 0)]

end

end Determinize.Proof.Paper
