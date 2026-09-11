import Determinize.Proof.Internal.PrimitiveLaws
import Determinize.Proof.DiscreteLaws
import Mathlib.Analysis.SpecialFunctions.Gamma.Deriv
import Mathlib.Analysis.SpecialFunctions.Pow.Continuity
import Mathlib.Probability.Distributions.Gaussian.Fernique
import Mathlib.Probability.Kernel.WithDensity

namespace Determinize.Proof.Paper

set_option linter.style.haveILetI false

open MeasureTheory

private theorem measurable_paperMeasure_gaussian :
    Measurable (Determinize.Statement.Paper.paperMeasure .gaussian) := by
  let fiber := fun params : Determinize.Statement.Paper.Params .gaussian =>
    if h : 0 ≤ params.2 0 then
      ProbabilityTheory.gaussianReal (params.1 0)
        (params.2 0).toNNReal else 0
  have hFiber : Determinize.Statement.Paper.paperMeasure .gaussian = fiber := by
    funext params
    simp only [Determinize.Statement.Paper.paperMeasure, fiber]
    split <;> rename_i h
    · have hNNReal : (⟨params.2 0, h⟩ : NNReal) =
          (params.2 0).toNNReal := by
        ext
        exact (Real.coe_toNNReal _ h).symm
      rw [hNNReal]
    · rfl
  rw [hFiber]
  apply Measurable.ite (measurableSet_le measurable_const (by fun_prop))
  · have hPair : Measurable (fun x : Determinize.Statement.Paper.Params .gaussian =>
        (x.1 0, (x.2 0).toNNReal)) :=
      Measurable.prod (by fun_prop) (by fun_prop)
    convert ProbabilityTheory.measurable_gaussianReal.comp hPair using 1
    funext x
    rfl
  · exact measurable_const

private theorem measurable_rpow_uncurry :
    Measurable (fun pair : ℝ × ℝ => pair.1 ^ pair.2) := by
  fun_prop

private noncomputable def safePos (value : ℝ) : ℝ := if 0 < value then value else 1

private theorem safePos_of_pos {value : ℝ} (hValue : 0 < value) :
    safePos value = value := by simp [safePos, hValue]

private theorem safePos_pos (value : ℝ) : 0 < safePos value := by
  unfold safePos
  split_ifs <;> positivity

private theorem measurable_safePos : Measurable safePos := by
  unfold safePos
  exact Measurable.ite (measurableSet_lt measurable_const measurable_id)
    measurable_id measurable_const

private theorem measurable_gamma_safe :
    Measurable (fun value : ℝ => Real.Gamma (safePos value)) := by
  have hGamma : Measurable ((Set.Ioi (0 : ℝ)).piecewise Real.Gamma
      (fun _ => Real.Gamma 1)) :=
    Real.differentiableOn_Gamma_Ioi.continuousOn.measurable_piecewise
      continuousOn_const measurableSet_Ioi
  have hFunctions : (fun value : ℝ => Real.Gamma (safePos value)) =
      (Set.Ioi (0 : ℝ)).piecewise Real.Gamma (fun _ => Real.Gamma 1) := by
    funext value
    by_cases hValue : 0 < value <;> simp [Set.piecewise, safePos, hValue]
  rw [hFunctions]
  exact hGamma

private noncomputable def gammaJointDensity (params : Determinize.Statement.Paper.Params .gamma) (value : ℝ) : ENNReal :=
  ENNReal.ofReal (ProbabilityTheory.gammaPDFReal
    (safePos (params.1 0))
    (safePos (params.2 0)) value)

private theorem measurable_gammaJointDensity :
    Measurable (Function.uncurry gammaJointDensity) := by
  let shape := fun pair : Determinize.Statement.Paper.Params .gamma × ℝ =>
    safePos (pair.1.1 0)
  let rate := fun pair : Determinize.Statement.Paper.Params .gamma × ℝ =>
    safePos (pair.1.2 0)
  have hShape : Measurable shape := measurable_safePos.comp (by fun_prop)
  have hRate : Measurable rate := measurable_safePos.comp (by fun_prop)
  have hGamma : Measurable (fun pair : Determinize.Statement.Paper.Params .gamma × ℝ =>
      Real.Gamma (shape pair)) := measurable_gamma_safe.comp (by fun_prop)
  have hRatePow : Measurable (fun pair : Determinize.Statement.Paper.Params .gamma × ℝ =>
      rate pair ^ shape pair) := by
    change Measurable ((fun pair : ℝ × ℝ => pair.1 ^ pair.2) ∘
      fun pair => (rate pair, shape pair))
    exact measurable_rpow_uncurry.comp (hRate.prodMk hShape)
  have hValuePow : Measurable (fun pair : Determinize.Statement.Paper.Params .gamma × ℝ =>
      pair.2 ^ (shape pair - 1)) :=
    by
      have hOne : Measurable (fun _ : Determinize.Statement.Paper.Params .gamma × ℝ => (1 : ℝ)) :=
        measurable_const
      change Measurable ((fun pair : ℝ × ℝ => pair.1 ^ pair.2) ∘
        fun pair => (pair.2, shape pair - 1))
      exact measurable_rpow_uncurry.comp (measurable_snd.prodMk (hShape.sub hOne))
  apply Measurable.ennreal_ofReal
  change Measurable (fun pair : Determinize.Statement.Paper.Params .gamma × ℝ =>
    ProbabilityTheory.gammaPDFReal (shape pair) (rate pair) pair.2)
  unfold ProbabilityTheory.gammaPDFReal
  apply Measurable.ite (measurableSet_le measurable_const measurable_snd)
  exact (((hRatePow.div hGamma).mul hValuePow).mul (by fun_prop))
  exact measurable_const

private noncomputable def gammaKernel :
    ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params .gamma) ℝ := by
  classical
  exact ProbabilityTheory.Kernel.piecewise (Determinize.Proof.Paper.measurableSet_domain .gamma)
    (ProbabilityTheory.Kernel.withDensity
      (ProbabilityTheory.Kernel.const _ volume) gammaJointDensity) 0

private theorem gammaKernel_apply (params : Determinize.Statement.Paper.Params .gamma) :
    gammaKernel params = Determinize.Statement.Paper.paperMeasure .gamma params := by
  classical
  by_cases hDomain : Determinize.Statement.Paper.domain .gamma params
  · rw [gammaKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_pos hDomain,
      ProbabilityTheory.Kernel.withDensity_apply _ measurable_gammaJointDensity,
      ProbabilityTheory.Kernel.const_apply]
    rcases hDomain with ⟨hShape, hRate⟩
    have hPaper : Determinize.Statement.Paper.paperMeasure .gamma params =
        ProbabilityTheory.gammaMeasure (params.1 0)
          (params.2 0) := by
      change (if h : 0 < params.1 0 ∧ 0 < params.2 0
        then ProbabilityTheory.gammaMeasure (params.1 0)
          (params.2 0) else 0) = _
      rw [dif_pos ⟨hShape, hRate⟩]
    rw [hPaper, ProbabilityTheory.gammaMeasure]
    apply withDensity_congr_ae
    filter_upwards [volume.ae_ne 0] with value hValueNe
    unfold gammaJointDensity
    rw [safePos_of_pos hShape, safePos_of_pos hRate]
    rfl
  · rw [gammaKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_neg hDomain]
    change 0 = if 0 < params.1 0 ∧ 0 < params.2 0
      then ProbabilityTheory.gammaMeasure (params.1 0)
        (params.2 0) else 0
    rw [if_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]

private noncomputable def exponentialJointDensity
    (params : Determinize.Statement.Paper.Params .exponential) (value : ℝ) : ENNReal :=
  ENNReal.ofReal (ProbabilityTheory.exponentialPDFReal
    (safePos (params.2 0)) value)

private theorem measurable_exponentialJointDensity :
    Measurable (Function.uncurry exponentialJointDensity) := by
  let rate := fun pair : Determinize.Statement.Paper.Params .exponential × ℝ =>
    safePos (pair.1.2 0)
  have hRate : Measurable rate := measurable_safePos.comp (by fun_prop)
  apply Measurable.ennreal_ofReal
  change Measurable (fun pair : Determinize.Statement.Paper.Params .exponential × ℝ =>
    ProbabilityTheory.exponentialPDFReal (rate pair) pair.2)
  unfold ProbabilityTheory.exponentialPDFReal ProbabilityTheory.gammaPDFReal
  simp only [Real.rpow_one, Real.Gamma_one, div_one, sub_self, Real.rpow_zero, mul_one]
  apply Measurable.ite (measurableSet_le measurable_const measurable_snd)
  · exact hRate.mul ((hRate.mul measurable_snd).neg.exp)
  · exact measurable_const

private noncomputable def exponentialKernel :
    ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params .exponential) ℝ := by
  classical
  exact ProbabilityTheory.Kernel.piecewise (Determinize.Proof.Paper.measurableSet_domain .exponential)
    (ProbabilityTheory.Kernel.withDensity
      (ProbabilityTheory.Kernel.const _ volume) exponentialJointDensity) 0

private theorem exponentialKernel_apply (params : Determinize.Statement.Paper.Params .exponential) :
    exponentialKernel params = Determinize.Statement.Paper.paperMeasure .exponential params := by
  classical
  by_cases hDomain : Determinize.Statement.Paper.domain .exponential params
  · rw [exponentialKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_pos hDomain,
      ProbabilityTheory.Kernel.withDensity_apply _ measurable_exponentialJointDensity,
      ProbabilityTheory.Kernel.const_apply]
    have hRate : 0 < params.2 0 := by
      simpa only [Determinize.Statement.Paper.domain] using hDomain
    have hPaper : Determinize.Statement.Paper.paperMeasure .exponential params =
        ProbabilityTheory.expMeasure (params.2 0) := by
      change (if 0 < params.2 0 then
        ProbabilityTheory.expMeasure (params.2 0) else 0) = _
      rw [if_pos hRate]
    rw [hPaper, ProbabilityTheory.expMeasure, ProbabilityTheory.gammaMeasure]
    apply withDensity_congr_ae
    exact ae_of_all _ fun value => by
      unfold exponentialJointDensity ProbabilityTheory.exponentialPDFReal
        ProbabilityTheory.gammaPDF
      rw [safePos_of_pos hRate]
  · rw [exponentialKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_neg hDomain]
    change 0 = if 0 < params.2 0 then
      ProbabilityTheory.expMeasure (params.2 0) else 0
    rw [if_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]

private noncomputable def betaJointDensity (params : Determinize.Statement.Paper.Params .beta)
    (value : ℝ) : ENNReal :=
  ENNReal.ofReal (ProbabilityTheory.betaPDFReal
    (safePos (params.2 0))
    (safePos (params.2 1)) value)

private theorem measurable_betaJointDensity :
    Measurable (Function.uncurry betaJointDensity) := by
  let alpha := fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
    safePos (pair.1.2 0)
  let beta := fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
    safePos (pair.1.2 1)
  have hAlpha : Measurable alpha := measurable_safePos.comp (by fun_prop)
  have hBeta : Measurable beta := measurable_safePos.comp (by fun_prop)
  have hGammaAlpha : Measurable (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
      Real.Gamma (alpha pair)) := measurable_gamma_safe.comp (by fun_prop)
  have hGammaBeta : Measurable (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
      Real.Gamma (beta pair)) := measurable_gamma_safe.comp (by fun_prop)
  have hGammaSum : Measurable (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
      Real.Gamma (alpha pair + beta pair)) := by
    have hSafe := measurable_gamma_safe.comp (hAlpha.add hBeta)
    have hFunctions : (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
        Real.Gamma (alpha pair + beta pair)) =
        (fun pair => Real.Gamma (safePos (alpha pair + beta pair))) := by
      funext pair
      rw [safePos_of_pos (add_pos (safePos_pos _) (safePos_pos _))]
    rw [hFunctions]
    exact hSafe
  have hNorm : Measurable (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
      ProbabilityTheory.beta (alpha pair) (beta pair)) := by
    unfold ProbabilityTheory.beta
    exact (hGammaAlpha.mul hGammaBeta).div hGammaSum
  have hValuePow : Measurable (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
      pair.2 ^ (alpha pair - 1)) := by
    have hOne : Measurable (fun _ : Determinize.Statement.Paper.Params .beta × ℝ => (1 : ℝ)) := measurable_const
    change Measurable ((fun pair : ℝ × ℝ => pair.1 ^ pair.2) ∘
      fun pair => (pair.2, alpha pair - 1))
    exact measurable_rpow_uncurry.comp (measurable_snd.prodMk (hAlpha.sub hOne))
  have hOneSubPow : Measurable (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
      (1 - pair.2) ^ (beta pair - 1)) := by
    have hOne : Measurable (fun _ : Determinize.Statement.Paper.Params .beta × ℝ => (1 : ℝ)) := measurable_const
    change Measurable ((fun pair : ℝ × ℝ => pair.1 ^ pair.2) ∘
      fun pair => (1 - pair.2, beta pair - 1))
    exact measurable_rpow_uncurry.comp
      ((hOne.sub measurable_snd).prodMk (hBeta.sub hOne))
  apply Measurable.ennreal_ofReal
  change Measurable (fun pair : Determinize.Statement.Paper.Params .beta × ℝ =>
    ProbabilityTheory.betaPDFReal (alpha pair) (beta pair) pair.2)
  unfold ProbabilityTheory.betaPDFReal
  apply Measurable.ite
    ((measurableSet_lt measurable_const measurable_snd).inter
      (measurableSet_lt measurable_snd measurable_const))
  · exact (((measurable_const.div hNorm).mul hValuePow).mul hOneSubPow)
  · exact measurable_const

private noncomputable def betaKernel :
    ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params .beta) ℝ := by
  classical
  exact ProbabilityTheory.Kernel.piecewise (Determinize.Proof.Paper.measurableSet_domain .beta)
    (ProbabilityTheory.Kernel.withDensity
      (ProbabilityTheory.Kernel.const _ volume) betaJointDensity) 0

private theorem betaKernel_apply (params : Determinize.Statement.Paper.Params .beta) :
    betaKernel params = Determinize.Statement.Paper.paperMeasure .beta params := by
  classical
  by_cases hDomain : Determinize.Statement.Paper.domain .beta params
  · rw [betaKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_pos hDomain,
      ProbabilityTheory.Kernel.withDensity_apply _ measurable_betaJointDensity,
      ProbabilityTheory.Kernel.const_apply]
    rcases hDomain with ⟨hAlpha, hBeta⟩
    have hPaper : Determinize.Statement.Paper.paperMeasure .beta params =
        ProbabilityTheory.betaMeasure (params.2 0)
          (params.2 1) := by
      change (if 0 < params.2 0 ∧ 0 < params.2 1
        then ProbabilityTheory.betaMeasure (params.2 0)
          (params.2 1) else 0) = _
      rw [if_pos ⟨hAlpha, hBeta⟩]
    rw [hPaper, ProbabilityTheory.betaMeasure]
    apply withDensity_congr_ae
    exact ae_of_all _ fun value => by
      unfold betaJointDensity ProbabilityTheory.betaPDF
      rw [safePos_of_pos hAlpha, safePos_of_pos hBeta]
  · rw [betaKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_neg hDomain]
    change 0 = if 0 < params.2 0 ∧ 0 < params.2 1
      then ProbabilityTheory.betaMeasure (params.2 0)
        (params.2 1) else 0
    rw [if_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]

private noncomputable def uniformJointDensity (params : Determinize.Statement.Paper.Params .uniform)
    (value : ℝ) : ENNReal :=
  (Set.Icc (params.1 0) (params.1 1)).indicator
    (fun _ => (ENNReal.ofReal
      (params.1 1 - params.1 0))⁻¹) value

private theorem measurable_uniformJointDensity :
    Measurable (Function.uncurry uniformJointDensity) := by
  unfold uniformJointDensity Set.indicator
  apply Measurable.ite
  · exact (measurableSet_le (by fun_prop) measurable_snd).inter
      (measurableSet_le measurable_snd (by fun_prop))
  · fun_prop
  · exact measurable_const

private noncomputable def uniformKernel :
    ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params .uniform) ℝ := by
  classical
  let intervalKernel := ProbabilityTheory.Kernel.withDensity
    (ProbabilityTheory.Kernel.const (Determinize.Statement.Paper.Params .uniform) volume) uniformJointDensity
  let pointKernel := ProbabilityTheory.Kernel.deterministic
    (fun params : Determinize.Statement.Paper.Params .uniform => params.1 0) (by fun_prop)
  let hPointSet : MeasurableSet {params : Determinize.Statement.Paper.Params .uniform |
      params.1 0 = params.1 1} :=
    measurableSet_eq_fun (by fun_prop) (by fun_prop)
  let domainKernel := ProbabilityTheory.Kernel.piecewise
    hPointSet pointKernel intervalKernel
  exact ProbabilityTheory.Kernel.piecewise (Determinize.Proof.Paper.measurableSet_domain .uniform) domainKernel 0

private theorem uniformKernel_apply (params : Determinize.Statement.Paper.Params .uniform) :
    uniformKernel params = Determinize.Statement.Paper.paperMeasure .uniform params := by
  classical
  let lower := params.1 0
  let upper := params.1 1
  by_cases hDomain : Determinize.Statement.Paper.domain .uniform params
  · have hle : lower ≤ upper := by simpa [lower, upper, Determinize.Statement.Paper.domain] using hDomain
    rw [uniformKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_pos hDomain, ProbabilityTheory.Kernel.piecewise_apply]
    by_cases hPoint : lower = upper
    · rw [if_pos (by simpa [lower, upper] using hPoint),
        ProbabilityTheory.Kernel.deterministic_apply]
      change Measure.dirac lower = Determinize.Statement.Paper.uniformMeasure lower upper
      unfold Determinize.Statement.Paper.uniformMeasure
      rw [dif_pos hle, dif_pos hPoint]
    · rw [if_neg (by simpa [lower, upper] using hPoint),
        ProbabilityTheory.Kernel.withDensity_apply _ measurable_uniformJointDensity,
        ProbabilityTheory.Kernel.const_apply]
      change volume.withDensity
        ((Set.Icc lower upper).indicator
          (fun _ => (ENNReal.ofReal (upper - lower))⁻¹)) =
        Determinize.Statement.Paper.uniformMeasure lower upper
      unfold Determinize.Statement.Paper.uniformMeasure
      rw [dif_pos hle, dif_neg hPoint, withDensity_indicator measurableSet_Icc,
        withDensity_const]
  · rw [uniformKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_neg hDomain]
    change 0 = Determinize.Statement.Paper.uniformMeasure (params.1 0)
      (params.1 1)
    unfold Determinize.Statement.Paper.uniformMeasure
    rw [dif_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]

private noncomputable def poissonWeight (params : Determinize.Statement.Paper.Params .poisson)
    (index : Nat) : ENNReal :=
  let rate : ℝ := params.1 0 |>.toNNReal
  ENNReal.ofReal (Real.exp (-rate) * rate ^ index / index.factorial)

private theorem measurable_poissonWeight (index : Nat) :
    Measurable (fun params : Determinize.Statement.Paper.Params .poisson => poissonWeight params index) := by
  unfold poissonWeight
  fun_prop

private noncomputable def poissonAtomKernel (index : Nat) :
    ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params .poisson) ℝ :=
  ProbabilityTheory.Kernel.withDensity
    (ProbabilityTheory.Kernel.deterministic
      (fun _ : Determinize.Statement.Paper.Params .poisson => (index : ℝ)) measurable_const)
    (fun params _ => poissonWeight params index)

private theorem measurable_poissonAtomDensity (index : Nat) :
    Measurable (Function.uncurry
      (fun params : Determinize.Statement.Paper.Params .poisson => fun _ : ℝ => poissonWeight params index)) := by
  exact (measurable_poissonWeight index).comp measurable_fst

private noncomputable def poissonKernel :
    ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params .poisson) ℝ := by
  classical
  exact ProbabilityTheory.Kernel.piecewise (Determinize.Proof.Paper.measurableSet_domain .poisson)
    (ProbabilityTheory.Kernel.sum poissonAtomKernel) 0

private theorem poissonKernel_apply (params : Determinize.Statement.Paper.Params .poisson) :
    poissonKernel params = Determinize.Statement.Paper.paperMeasure .poisson params := by
  classical
  by_cases hDomain : Determinize.Statement.Paper.domain .poisson params
  · rw [poissonKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_pos hDomain, ProbabilityTheory.Kernel.sum_apply]
    have hRate : 0 ≤ params.1 0 := by
      simpa only [Determinize.Statement.Paper.domain] using hDomain
    let rate : NNReal := (params.1 0).toNNReal
    have hPaper : Determinize.Statement.Paper.paperMeasure .poisson params =
        (ProbabilityTheory.poissonMeasure rate).map (fun value : Nat => (value : ℝ)) := by
      change (if h : 0 ≤ params.1 0 then
        (ProbabilityTheory.poissonMeasure ⟨params.1 0, h⟩).map
          (fun value : Nat => (value : ℝ)) else 0) = _
      rw [dif_pos hRate]
      congr 2
      exact Subtype.ext (by simp [rate, Real.toNNReal_of_nonneg hRate])
    rw [hPaper, ProbabilityTheory.poissonMeasure,
      Measure.map_sum
        (measurable_of_countable (fun value : Nat => (value : ℝ))).aemeasurable]
    congr 1
    funext index
    rw [poissonAtomKernel,
      ProbabilityTheory.Kernel.withDensity_apply _ (measurable_poissonAtomDensity index),
      ProbabilityTheory.Kernel.deterministic_apply, withDensity_const,
      Measure.map_smul,
      Measure.map_dirac' (measurable_of_countable (fun value : Nat => (value : ℝ)))]
    unfold poissonWeight
    rw [show ((params.1 0).toNNReal : ℝ) = (rate : ℝ) by rfl]
  · rw [poissonKernel, ProbabilityTheory.Kernel.piecewise_apply]
    simp only [Set.mem_ofPred_eq]
    rw [if_neg hDomain]
    change 0 = if h : 0 ≤ params.1 0 then
      (ProbabilityTheory.poissonMeasure ⟨params.1 0, h⟩).map
        (fun value : Nat => (value : ℝ)) else 0
    rw [dif_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]

private theorem paperMeasure_mass_one (op : Determinize.Statement.Paper.Op) (params : Determinize.Statement.Paper.Params op)
    (hDomain : Determinize.Statement.Paper.domain op params) : Determinize.Statement.Paper.paperMeasure op params Set.univ = 1 := by
  cases op with
  | uniform =>
      change Determinize.Statement.Paper.uniformMeasure (params.1 0)
        (params.1 1) Set.univ = 1
      unfold Determinize.Statement.Paper.uniformMeasure
      have hd : params.1 0 ≤ params.1 1 := by
        simpa only [Determinize.Statement.Paper.domain] using hDomain
      rw [dif_pos hd]
      split <;> rename_i hPoint
      · simp
      · rw [Measure.smul_apply, Measure.restrict_apply_univ,
          Real.volume_Icc]
        have hPositive : 0 < params.1 1 - params.1 0 :=
          sub_pos.mpr (lt_of_le_of_ne hd hPoint)
        apply ENNReal.inv_mul_cancel
        · exact (ENNReal.ofReal_pos.mpr hPositive).ne'
        · exact ENNReal.ofReal_ne_top
  | gaussian =>
      change (if h : 0 ≤ params.2 0 then
        ProbabilityTheory.gaussianReal (params.1 0)
          ⟨params.2 0, h⟩ else 0) Set.univ = 1
      have hd : 0 ≤ params.2 0 := by
        simpa only [Determinize.Statement.Paper.domain] using hDomain
      rw [dif_pos hd]
      letI := ProbabilityTheory.instIsProbabilityMeasureGaussianReal
        (params.1 0) ⟨params.2 0, hd⟩
      exact measure_univ
  | poisson =>
      change (if h : 0 ≤ params.1 0 then
        (ProbabilityTheory.poissonMeasure ⟨params.1 0, h⟩).map
          (fun value : Nat => (value : ℝ)) else 0) Set.univ = 1
      have hd : 0 ≤ params.1 0 := by
        simpa only [Determinize.Statement.Paper.domain] using hDomain
      rw [dif_pos hd]
      have hRate : (⟨params.1 0, hd⟩ : NNReal) =
          (params.1 0).toNNReal := by
        ext
        exact (Real.coe_toNNReal _ hd).symm
      rw [hRate]
      letI : IsProbabilityMeasure
          ((ProbabilityTheory.poissonMeasure
              (params.1 0).toNNReal).map
            (fun value : Nat => (value : ℝ))) :=
        Measure.isProbabilityMeasure_map .of_discrete
      exact measure_univ
  | exponential =>
      change (if 0 < params.2 0 then
        ProbabilityTheory.expMeasure (params.2 0) else 0) Set.univ = 1
      have hd : 0 < params.2 0 := by
        simpa only [Determinize.Statement.Paper.domain] using hDomain
      rw [if_pos hd]
      letI := ProbabilityTheory.isProbabilityMeasure_expMeasure hd
      exact measure_univ
  | beta =>
      change (if 0 < params.2 0 ∧ 0 < params.2 1 then
        ProbabilityTheory.betaMeasure (params.2 0)
          (params.2 1) else 0) Set.univ = 1
      have hd : 0 < params.2 0 ∧
          0 < params.2 1 := by
        simpa only [Determinize.Statement.Paper.domain] using hDomain
      rw [if_pos hd]
      letI := ProbabilityTheory.isProbabilityMeasureBeta hd.1 hd.2
      exact measure_univ
  | gamma =>
      change (if 0 < params.1 0 ∧ 0 < params.2 0 then
        ProbabilityTheory.gammaMeasure (params.1 0)
          (params.2 0) else 0) Set.univ = 1
      have hd : 0 < params.1 0 ∧
          0 < params.2 0 := by
        simpa only [Determinize.Statement.Paper.domain] using hDomain
      rw [if_pos hd]
      letI := ProbabilityTheory.isProbabilityMeasure_gammaMeasure hd.1 hd.2
      exact measure_univ

  | bernoulli =>
      letI := DiscreteLaws.bernoulli_probability .stochastic (params.1 0) hDomain
      exact measure_univ (μ := Determinize.Statement.Paper.bernoulliFiber .stochastic (params.1 0))
  | discrete d =>
      exact measure_univ (μ := Determinize.Statement.Paper.discreteFiber .stochastic d)

private theorem paperMeasure_zero_off_domain (op : Determinize.Statement.Paper.Op) (params : Determinize.Statement.Paper.Params op)
    (hDomain : ¬ Determinize.Statement.Paper.domain op params) : Determinize.Statement.Paper.paperMeasure op params = 0 := by
  cases op with
  | uniform =>
      change Determinize.Statement.Paper.uniformMeasure (params.1 0)
        (params.1 1) = 0
      unfold Determinize.Statement.Paper.uniformMeasure
      rw [dif_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]
  | gaussian =>
      change (if h : 0 ≤ params.2 0 then
        ProbabilityTheory.gaussianReal (params.1 0)
          ⟨params.2 0, h⟩ else 0) = 0
      rw [dif_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]
  | poisson =>
      change (if h : 0 ≤ params.1 0 then
        (ProbabilityTheory.poissonMeasure ⟨params.1 0, h⟩).map
          (fun value : Nat => (value : ℝ)) else 0) = 0
      rw [dif_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]
  | exponential =>
      change (if 0 < params.2 0 then
        ProbabilityTheory.expMeasure (params.2 0) else 0) = 0
      rw [if_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]
  | beta =>
      change (if 0 < params.2 0 ∧ 0 < params.2 1 then
        ProbabilityTheory.betaMeasure (params.2 0)
          (params.2 1) else 0) = 0
      rw [if_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]
  | gamma =>
      change (if 0 < params.1 0 ∧ 0 < params.2 0 then
        ProbabilityTheory.gammaMeasure (params.1 0)
          (params.2 0) else 0) = 0
      rw [if_neg (by simpa only [Determinize.Statement.Paper.domain] using hDomain)]

  | bernoulli => exact DiscreteLaws.bernoulli_off_domain .stochastic (params.1 0) hDomain
  | discrete _ => exact False.elim (hDomain trivial)

private theorem paperMeasure_mass_le_one (op : Determinize.Statement.Paper.Op) (params : Determinize.Statement.Paper.Params op) :
    Determinize.Statement.Paper.paperMeasure op params Set.univ ≤ 1 := by
  by_cases hDomain : Determinize.Statement.Paper.domain op params
  · rw [paperMeasure_mass_one op params hDomain]
  · rw [paperMeasure_zero_off_domain op params hDomain]
    simp

private theorem gaussian_integrable_id (params : Determinize.Statement.Paper.Params .gaussian)
    (hDomain : Determinize.Statement.Paper.domain .gaussian params) :
    Integrable id (Determinize.Statement.Paper.paperMeasure .gaussian params) := by
  have hd : 0 ≤ params.2 0 := by
    simpa only [Determinize.Statement.Paper.domain] using hDomain
  change Integrable id (if h : 0 ≤ params.2 0 then
    ProbabilityTheory.gaussianReal (params.1 0)
      ⟨params.2 0, h⟩ else 0)
  rw [dif_pos hd]
  letI : ProbabilityTheory.IsGaussian
      (ProbabilityTheory.gaussianReal (params.1 0)
        ⟨params.2 0, hd⟩) :=
    ProbabilityTheory.isGaussian_gaussianReal _ _
  exact ProbabilityTheory.IsGaussian.integrable_id

private theorem gaussian_mean (params : Determinize.Statement.Paper.Params .gaussian)
    (hDomain : Determinize.Statement.Paper.domain .gaussian params) :
    (∫ value : ℝ, value ∂Determinize.Statement.Paper.paperMeasure .gaussian params) =
      Determinize.Statement.Paper.meanValue .gaussian params := by
  have hd : 0 ≤ params.2 0 := by
    simpa only [Determinize.Statement.Paper.domain] using hDomain
  change (∫ value : ℝ, value ∂if h : 0 ≤ params.2 0 then
    ProbabilityTheory.gaussianReal (params.1 0)
      ⟨params.2 0, h⟩ else 0) = _
  rw [dif_pos hd]
  calc
    _ = params.1 0 := ProbabilityTheory.integral_id_gaussianReal
    _ = _ := by
      simp only [Determinize.Proof.Paper.meanValue_eq_affine, Determinize.Proof.Paper.meanConstant, Determinize.Proof.Paper.meanCoeff,
        Determinize.Statement.Paper.affineArity, zero_add, one_mul]
      symm
      exact Fin.sum_univ_one _

private theorem uniform_integrable_id (params : Determinize.Statement.Paper.Params .uniform)
    (hDomain : Determinize.Statement.Paper.domain .uniform params) :
    Integrable id (Determinize.Statement.Paper.paperMeasure .uniform params) := by
  let lower := params.1 0
  let upper := params.1 1
  have hle : lower ≤ upper := by simpa [lower, upper, Determinize.Statement.Paper.domain] using hDomain
  change Integrable id (Determinize.Statement.Paper.uniformMeasure lower upper)
  unfold Determinize.Statement.Paper.uniformMeasure
  rw [dif_pos hle]
  by_cases hPoint : lower = upper
  · rw [dif_pos hPoint]
    exact integrable_dirac (by simp)
  · rw [dif_neg hPoint]
    have hPositive : 0 < upper - lower := sub_pos.mpr (lt_of_le_of_ne hle hPoint)
    have hScaleZero : (ENNReal.ofReal (upper - lower))⁻¹ ≠ 0 :=
      ENNReal.inv_ne_zero.mpr ENNReal.ofReal_ne_top
    have hScaleTop : (ENNReal.ofReal (upper - lower))⁻¹ ≠ ⊤ :=
      ENNReal.inv_ne_top.mpr (ENNReal.ofReal_pos.mpr hPositive).ne'
    exact (integrable_smul_measure (f := id) hScaleZero hScaleTop).mpr
      continuous_id.continuousOn.integrableOn_Icc

private theorem uniform_mean (params : Determinize.Statement.Paper.Params .uniform)
    (hDomain : Determinize.Statement.Paper.domain .uniform params) :
    (∫ value : ℝ, value ∂Determinize.Statement.Paper.paperMeasure .uniform params) =
      Determinize.Statement.Paper.meanValue .uniform params := by
  let lower := params.1 0
  let upper := params.1 1
  have hle : lower ≤ upper := by simpa [lower, upper, Determinize.Statement.Paper.domain] using hDomain
  change (∫ value : ℝ, value ∂Determinize.Statement.Paper.uniformMeasure lower upper) = _
  unfold Determinize.Statement.Paper.uniformMeasure
  rw [dif_pos hle]
  by_cases hPoint : lower = upper
  · rw [dif_pos hPoint]
    simp only [integral_dirac]
    subst upper
    simp only [Determinize.Proof.Paper.meanValue_eq_affine, Determinize.Proof.Paper.meanConstant, Determinize.Proof.Paper.meanCoeff,
      Determinize.Statement.Paper.affineArity, zero_add]
    have hSum : (∑ i, 1 / 2 * params.1 i) =
        1 / 2 * params.1 0 + 1 / 2 * params.1 1 := by
      exact Fin.sum_univ_two _
    calc
      lower = 1 / 2 * params.1 0 +
          1 / 2 * params.1 1 := by
            rw [← hPoint]
            dsimp [lower]
            ring
      _ = _ := hSum.symm

  · rw [dif_neg hPoint, integral_smul_measure]
    have hPositive : 0 < upper - lower := sub_pos.mpr (lt_of_le_of_ne hle hPoint)
    have hSetIntegral : (∫ value : ℝ in Set.Icc lower upper, value) =
        (upper ^ 2 - lower ^ 2) / 2 := by
      rw [integral_Icc_eq_integral_Ioc, ← intervalIntegral.integral_of_le hle,
        integral_id]
    rw [hSetIntegral]
    have hScale : (ENNReal.ofReal (upper - lower))⁻¹.toReal =
        1 / (upper - lower) := by
      rw [ENNReal.toReal_inv]
      simp [ENNReal.toReal_ofReal hPositive.le]
    rw [hScale]
    simp only [smul_eq_mul, Determinize.Proof.Paper.meanValue_eq_affine, Determinize.Proof.Paper.meanConstant, Determinize.Proof.Paper.meanCoeff,
      Determinize.Statement.Paper.affineArity, zero_add]
    have hSum : (∑ i, 1 / 2 * params.1 i) =
        1 / 2 * params.1 0 + 1 / 2 * params.1 1 := by
      exact Fin.sum_univ_two _
    calc
      1 / (upper - lower) * ((upper ^ 2 - lower ^ 2) / 2) =
          1 / 2 * params.1 0 +
            1 / 2 * params.1 1 := by
              rw [show upper ^ 2 - lower ^ 2 =
                (upper - lower) * (upper + lower) by ring]
              field_simp [hPositive.ne']
              dsimp only [lower, upper]
              ring
      _ = _ := hSum.symm

private theorem hasSum_poisson_first_moment (rate : NNReal) :
    HasSum (fun n : Nat => Real.exp (-(rate : ℝ)) * (rate : ℝ) ^ n /
      n.factorial * (n : ℝ)) (rate : ℝ) := by
  let term := fun n : Nat => Real.exp (-(rate : ℝ)) * (rate : ℝ) ^ n /
    n.factorial * (n : ℝ)
  have hSeries := (NormedSpace.expSeries_div_hasSum_exp (rate : ℝ)).mul_left
    (Real.exp (-(rate : ℝ)) * (rate : ℝ))
  have hTail : HasSum (fun n : Nat => term (n + 1)) (rate : ℝ) := by
    have hTerms : (fun n : Nat => term (n + 1)) =
        fun n : Nat => Real.exp (-(rate : ℝ)) * (rate : ℝ) *
          ((rate : ℝ) ^ n / n.factorial) := by
      funext n
      dsimp only [term]
      rw [pow_succ, Nat.factorial_succ, Nat.cast_mul, Nat.cast_succ]
      field_simp
    rw [hTerms]
    have hResult : Real.exp (-(rate : ℝ)) * (rate : ℝ) *
        NormedSpace.exp (rate : ℝ) = (rate : ℝ) := by
      rw [← Real.exp_eq_exp_ℝ]
      calc
        Real.exp (-(rate : ℝ)) * (rate : ℝ) * Real.exp (rate : ℝ) =
            (rate : ℝ) * (Real.exp (-(rate : ℝ)) * Real.exp (rate : ℝ)) := by ring
        _ = _ := by rw [← Real.exp_add]; simp
    rw [hResult] at hSeries
    exact hSeries
  apply (hasSum_nat_add_iff' 1).mp
  simpa [term] using hTail

private theorem poisson_integrable_id (params : Determinize.Statement.Paper.Params .poisson)
    (hDomain : Determinize.Statement.Paper.domain .poisson params) :
    Integrable id (Determinize.Statement.Paper.paperMeasure .poisson params) := by
  have hd : 0 ≤ params.1 0 := by
    simpa only [Determinize.Statement.Paper.domain] using hDomain
  let rate : NNReal := (params.1 0).toNNReal
  have hRate : (⟨params.1 0, hd⟩ : NNReal) = rate := by
    apply Subtype.ext
    simp [rate, Real.toNNReal_of_nonneg hd]
  change Integrable id (if h : 0 ≤ params.1 0 then
    (ProbabilityTheory.poissonMeasure ⟨params.1 0, h⟩).map
      (fun value : Nat => (value : ℝ)) else 0)
  rw [dif_pos hd, hRate,
    integrable_map_measure continuous_id.aestronglyMeasurable
      (measurable_of_countable (fun value : Nat => (value : ℝ))).aemeasurable,
    ProbabilityTheory.integrable_poissonMeasure_iff]
  simpa [Function.comp_apply, Real.norm_eq_abs] using
    (hasSum_poisson_first_moment rate).summable

private theorem poisson_mean (params : Determinize.Statement.Paper.Params .poisson)
    (hDomain : Determinize.Statement.Paper.domain .poisson params) :
    (∫ value : ℝ, value ∂Determinize.Statement.Paper.paperMeasure .poisson params) =
      Determinize.Statement.Paper.meanValue .poisson params := by
  have hd : 0 ≤ params.1 0 := by
    simpa only [Determinize.Statement.Paper.domain] using hDomain
  let rate : NNReal := (params.1 0).toNNReal
  have hRate : (⟨params.1 0, hd⟩ : NNReal) = rate := by
    apply Subtype.ext
    simp [rate, Real.toNNReal_of_nonneg hd]
  change (∫ value : ℝ, value ∂if h : 0 ≤ params.1 0 then
    (ProbabilityTheory.poissonMeasure ⟨params.1 0, h⟩).map
      (fun value : Nat => (value : ℝ)) else 0) = _
  rw [dif_pos hd, hRate]
  have hMap : (∫ value : ℝ, value ∂
      (ProbabilityTheory.poissonMeasure rate).map (fun value : Nat => (value : ℝ))) =
      ∫ value : Nat, (value : ℝ) ∂ProbabilityTheory.poissonMeasure rate := by
    exact integral_map_of_stronglyMeasurable
      (measurable_of_countable (fun value : Nat => (value : ℝ)))
      continuous_id.stronglyMeasurable
  rw [hMap, ProbabilityTheory.integral_poissonMeasure rate,
    show (∑' n : Nat, (Real.exp (-(rate : ℝ)) * (rate : ℝ) ^ n /
      n.factorial) • (n : ℝ)) =
        ∑' n : Nat, Real.exp (-(rate : ℝ)) * (rate : ℝ) ^ n /
          n.factorial * (n : ℝ) by simp only [smul_eq_mul],
    (hasSum_poisson_first_moment rate).tsum_eq]
  simp only [Determinize.Proof.Paper.meanValue_eq_affine, Determinize.Proof.Paper.meanConstant, Determinize.Proof.Paper.meanCoeff,
    Determinize.Statement.Paper.affineArity, zero_add, one_mul]
  rw [show (rate : ℝ) = params.1 0 by
    simp [rate, Real.toNNReal_of_nonneg hd]]
  symm
  exact Fin.sum_univ_one _

private theorem gamma_mul_pdf_ae {shape rate : ℝ} (hShape : 0 < shape)
    (hRate : 0 < rate) :
    ∀ᵐ value : ℝ ∂volume,
      value * ProbabilityTheory.gammaPDFReal shape rate value =
        (shape / rate) * ProbabilityTheory.gammaPDFReal (shape + 1) rate value := by
  filter_upwards [volume.ae_ne 0] with value hValueNe
  by_cases hValueNeg : value < 0
  · simp [ProbabilityTheory.gammaPDFReal, not_le.mpr hValueNeg]
  · have hValue : 0 < value := lt_of_le_of_ne (le_of_not_gt hValueNeg) (Ne.symm hValueNe)
    simp only [ProbabilityTheory.gammaPDFReal, if_pos hValue.le]
    rw [Real.Gamma_add_one hShape.ne', Real.rpow_add_one hRate.ne',
      show shape + 1 - 1 = shape by ring]
    rw [show value ^ shape = value * value ^ (shape - 1) by
      calc
        value ^ shape = value ^ (1 + (shape - 1)) := by congr 1; ring
        _ = value ^ (1 : ℝ) * value ^ (shape - 1) := Real.rpow_add hValue _ _
        _ = _ := by rw [Real.rpow_one]]
    field_simp [hShape.ne', hRate.ne', (Real.Gamma_pos_of_pos hShape).ne']

private theorem gamma_pdf_integrable {shape rate : ℝ} (hShape : 0 < shape)
    (hRate : 0 < rate) :
    Integrable (ProbabilityTheory.gammaPDFReal shape rate) volume := by
  letI : IsProbabilityMeasure (ProbabilityTheory.gammaMeasure shape rate) :=
    ProbabilityTheory.isProbabilityMeasure_gammaMeasure hShape hRate
  have hOne : Integrable (fun _ : ℝ => (1 : ℝ))
      (ProbabilityTheory.gammaMeasure shape rate) := integrable_const _
  rw [ProbabilityTheory.gammaMeasure] at hOne
  have hWeighted := (integrable_withDensity_iff (μ := volume)
    (ProbabilityTheory.measurable_gammaPDFReal shape rate).ennreal_ofReal
    (by simp)).mp hOne
  apply hWeighted.congr
  exact ae_of_all _ fun value => by
    simp only [one_mul]
    rw [ENNReal.toReal_ofReal
      (ProbabilityTheory.gammaPDFReal_nonneg hShape hRate value)]

private theorem integral_gamma_pdf_eq_one {shape rate : ℝ} (hShape : 0 < shape)
    (hRate : 0 < rate) :
    (∫ value : ℝ, ProbabilityTheory.gammaPDFReal shape rate value) = 1 := by
  letI : IsProbabilityMeasure (ProbabilityTheory.gammaMeasure shape rate) :=
    ProbabilityTheory.isProbabilityMeasure_gammaMeasure hShape hRate
  have hOne : (∫ _value : ℝ, (1 : ℝ) ∂
      ProbabilityTheory.gammaMeasure shape rate) = 1 := by simp
  calc
    (∫ value : ℝ, ProbabilityTheory.gammaPDFReal shape rate value) =
        ∫ value : ℝ, (ProbabilityTheory.gammaPDF shape rate value).toReal • (1 : ℝ) := by
          apply integral_congr_ae
          exact ae_of_all _ fun value => by
            simp only [smul_eq_mul, mul_one, ProbabilityTheory.gammaPDF]
            rw [ENNReal.toReal_ofReal
              (ProbabilityTheory.gammaPDFReal_nonneg hShape hRate value)]
    _ = ∫ _value : ℝ, (1 : ℝ) ∂ProbabilityTheory.gammaMeasure shape rate := by
      rw [ProbabilityTheory.gammaMeasure]
      exact (integral_withDensity_eq_integral_toReal_smul
        (ProbabilityTheory.measurable_gammaPDFReal shape rate).ennreal_ofReal
        (by simp) _).symm
    _ = 1 := hOne

private theorem gamma_integrable_id_at {shape rate : ℝ} (hShape : 0 < shape)
    (hRate : 0 < rate) :
    Integrable id (ProbabilityTheory.gammaMeasure shape rate) := by
  rw [ProbabilityTheory.gammaMeasure]
  apply (integrable_withDensity_iff (μ := volume)
    (ProbabilityTheory.measurable_gammaPDFReal shape rate).ennreal_ofReal
    (by simp)).mpr
  have hShift := (gamma_pdf_integrable (add_pos hShape zero_lt_one) hRate).const_mul
    (shape / rate)
  apply hShift.congr
  filter_upwards [gamma_mul_pdf_ae hShape hRate] with value hValue
  simpa only [id_eq, ProbabilityTheory.gammaPDF,
    ENNReal.toReal_ofReal
      (ProbabilityTheory.gammaPDFReal_nonneg hShape hRate value)] using hValue.symm

private theorem gamma_mean_at {shape rate : ℝ} (hShape : 0 < shape)
    (hRate : 0 < rate) :
    (∫ value : ℝ, value ∂ProbabilityTheory.gammaMeasure shape rate) = shape / rate := by
  calc
    (∫ value : ℝ, value ∂ProbabilityTheory.gammaMeasure shape rate) =
        ∫ value : ℝ, (ProbabilityTheory.gammaPDF shape rate value).toReal • value := by
          rw [ProbabilityTheory.gammaMeasure]
          exact integral_withDensity_eq_integral_toReal_smul
            (ProbabilityTheory.measurable_gammaPDFReal shape rate).ennreal_ofReal
            (by simp) _
    _ = ∫ value : ℝ, (shape / rate) *
        ProbabilityTheory.gammaPDFReal (shape + 1) rate value := by
          apply integral_congr_ae
          filter_upwards [gamma_mul_pdf_ae hShape hRate] with value hValue
          simpa only [smul_eq_mul, ProbabilityTheory.gammaPDF, mul_comm,
            ENNReal.toReal_ofReal
              (ProbabilityTheory.gammaPDFReal_nonneg hShape hRate value)] using hValue
    _ = shape / rate := by
      rw [integral_const_mul,
        integral_gamma_pdf_eq_one (add_pos hShape zero_lt_one) hRate, mul_one]

private theorem gamma_integrable_id (params : Determinize.Statement.Paper.Params .gamma)
    (hDomain : Determinize.Statement.Paper.domain .gamma params) :
    Integrable id (Determinize.Statement.Paper.paperMeasure .gamma params) := by
  rcases hDomain with ⟨hShape, hRate⟩
  change Integrable id (if h : 0 < params.1 0 ∧
    0 < params.2 0 then
      ProbabilityTheory.gammaMeasure (params.1 0)
        (params.2 0) else 0)
  rw [dif_pos ⟨hShape, hRate⟩]
  exact gamma_integrable_id_at hShape hRate

private theorem gamma_mean (params : Determinize.Statement.Paper.Params .gamma)
    (hDomain : Determinize.Statement.Paper.domain .gamma params) :
    (∫ value : ℝ, value ∂Determinize.Statement.Paper.paperMeasure .gamma params) =
      Determinize.Statement.Paper.meanValue .gamma params := by
  rcases hDomain with ⟨hShape, hRate⟩
  change (∫ value : ℝ, value ∂if h : 0 < params.1 0 ∧
    0 < params.2 0 then
      ProbabilityTheory.gammaMeasure (params.1 0)
        (params.2 0) else 0) = _
  rw [dif_pos ⟨hShape, hRate⟩, gamma_mean_at hShape hRate]
  simp only [Determinize.Proof.Paper.meanValue_eq_affine, Determinize.Proof.Paper.meanConstant, Determinize.Proof.Paper.meanCoeff,
    Determinize.Statement.Paper.affineArity, zero_add]
  change params.1 (0 : Fin 1) / params.2 (0 : Fin 1) =
    ∑ i : Fin 1, 1 / params.2 (0 : Fin 1) * params.1 i
  rw [Fin.sum_univ_one]
  ring

private theorem exponential_integrable_id (params : Determinize.Statement.Paper.Params .exponential)
    (hDomain : Determinize.Statement.Paper.domain .exponential params) :
    Integrable id (Determinize.Statement.Paper.paperMeasure .exponential params) := by
  have hRate : 0 < params.2 0 := by
    simpa only [Determinize.Statement.Paper.domain] using hDomain
  change Integrable id (if h : 0 < params.2 0 then
    ProbabilityTheory.expMeasure (params.2 0) else 0)
  rw [dif_pos hRate, ProbabilityTheory.expMeasure]
  exact gamma_integrable_id_at zero_lt_one hRate

private theorem exponential_mean (params : Determinize.Statement.Paper.Params .exponential)
    (hDomain : Determinize.Statement.Paper.domain .exponential params) :
    (∫ value : ℝ, value ∂Determinize.Statement.Paper.paperMeasure .exponential params) =
      Determinize.Statement.Paper.meanValue .exponential params := by
  have hRate : 0 < params.2 0 := by
    simpa only [Determinize.Statement.Paper.domain] using hDomain
  change (∫ value : ℝ, value ∂if h : 0 < params.2 0 then
    ProbabilityTheory.expMeasure (params.2 0) else 0) = _
  rw [dif_pos hRate, ProbabilityTheory.expMeasure,
    gamma_mean_at zero_lt_one hRate]
  rw [Determinize.Proof.Paper.meanValue_eq_affine]
  unfold Determinize.Proof.Paper.meanConstant
  have hSum : (∑ i : Fin (Determinize.Statement.Paper.affineArity .exponential),
      Determinize.Proof.Paper.meanCoeff .exponential params.2 i * params.1 i) = 0 := by
    apply Finset.sum_eq_zero
    intro index _
    exact Fin.elim0 index
  rw [hSum, add_zero]

private theorem beta_add_one (alpha beta : ℝ) (hAlpha : 0 < alpha)
    (hBeta : 0 < beta) :
    ProbabilityTheory.beta (alpha + 1) beta =
      alpha / (alpha + beta) * ProbabilityTheory.beta alpha beta := by
  unfold ProbabilityTheory.beta
  rw [Real.Gamma_add_one hAlpha.ne',
    show alpha + 1 + beta = (alpha + beta) + 1 by ring,
    Real.Gamma_add_one (add_pos hAlpha hBeta).ne']
  field_simp [(Real.Gamma_pos_of_pos hAlpha).ne',
    (Real.Gamma_pos_of_pos hBeta).ne',
    (Real.Gamma_pos_of_pos (add_pos hAlpha hBeta)).ne',
    (add_pos hAlpha hBeta).ne']

private theorem beta_pdf_nonneg (alpha beta : ℝ) (hAlpha : 0 < alpha)
    (hBeta : 0 < beta) (value : ℝ) :
    0 ≤ ProbabilityTheory.betaPDFReal alpha beta value := by
  unfold ProbabilityTheory.betaPDFReal
  split_ifs with hValue
  · exact mul_nonneg
      (mul_nonneg (one_div_nonneg.mpr (ProbabilityTheory.beta_pos hAlpha hBeta).le)
        (Real.rpow_nonneg hValue.1.le _))
      (Real.rpow_nonneg (sub_nonneg.mpr hValue.2.le) _)
  · exact le_rfl

private theorem beta_mul_pdf (alpha beta : ℝ) (hAlpha : 0 < alpha)
    (hBeta : 0 < beta) (value : ℝ) :
    value * ProbabilityTheory.betaPDFReal alpha beta value =
      (alpha / (alpha + beta)) *
        ProbabilityTheory.betaPDFReal (alpha + 1) beta value := by
  unfold ProbabilityTheory.betaPDFReal
  by_cases hValue : 0 < value ∧ value < 1
  · rw [if_pos hValue, if_pos hValue, beta_add_one alpha beta hAlpha hBeta]
    rw [show alpha + 1 - 1 = alpha by ring]
    rw [show value ^ alpha = value * value ^ (alpha - 1) by
      calc
        value ^ alpha = value ^ (1 + (alpha - 1)) := by congr 1; ring
        _ = value ^ (1 : ℝ) * value ^ (alpha - 1) :=
          Real.rpow_add hValue.1 _ _
        _ = _ := by rw [Real.rpow_one]]
    field_simp [(ProbabilityTheory.beta_pos hAlpha hBeta).ne',
      (add_pos hAlpha hBeta).ne']
  · rw [if_neg hValue, if_neg hValue, mul_zero, mul_zero]

private theorem beta_pdf_integrable {alpha beta : ℝ} (hAlpha : 0 < alpha)
    (hBeta : 0 < beta) :
    Integrable (ProbabilityTheory.betaPDFReal alpha beta) volume := by
  letI : IsProbabilityMeasure (ProbabilityTheory.betaMeasure alpha beta) :=
    ProbabilityTheory.isProbabilityMeasureBeta hAlpha hBeta
  have hOne : Integrable (fun _ : ℝ => (1 : ℝ))
      (ProbabilityTheory.betaMeasure alpha beta) := integrable_const _
  rw [ProbabilityTheory.betaMeasure] at hOne
  have hWeighted := (integrable_withDensity_iff (μ := volume)
    (ProbabilityTheory.measurable_betaPDFReal alpha beta).ennreal_ofReal
    (by simp)).mp hOne
  apply hWeighted.congr
  exact ae_of_all _ fun value => by
    simp only [one_mul]
    rw [ENNReal.toReal_ofReal (beta_pdf_nonneg alpha beta hAlpha hBeta value)]

private theorem integral_beta_pdf_eq_one {alpha beta : ℝ} (hAlpha : 0 < alpha)
    (hBeta : 0 < beta) :
    (∫ value : ℝ, ProbabilityTheory.betaPDFReal alpha beta value) = 1 := by
  letI : IsProbabilityMeasure (ProbabilityTheory.betaMeasure alpha beta) :=
    ProbabilityTheory.isProbabilityMeasureBeta hAlpha hBeta
  have hOne : (∫ _value : ℝ, (1 : ℝ) ∂
      ProbabilityTheory.betaMeasure alpha beta) = 1 := by simp
  calc
    (∫ value : ℝ, ProbabilityTheory.betaPDFReal alpha beta value) =
        ∫ value : ℝ, (ProbabilityTheory.betaPDF alpha beta value).toReal • (1 : ℝ) := by
          apply integral_congr_ae
          exact ae_of_all _ fun value => by
            simp only [smul_eq_mul, mul_one, ProbabilityTheory.betaPDF]
            rw [ENNReal.toReal_ofReal (beta_pdf_nonneg alpha beta hAlpha hBeta value)]
    _ = ∫ _value : ℝ, (1 : ℝ) ∂ProbabilityTheory.betaMeasure alpha beta := by
      rw [ProbabilityTheory.betaMeasure]
      exact (integral_withDensity_eq_integral_toReal_smul
        (ProbabilityTheory.measurable_betaPDFReal alpha beta).ennreal_ofReal
        (by simp) _).symm
    _ = 1 := hOne

private theorem beta_integrable_id_at {alpha beta : ℝ} (hAlpha : 0 < alpha)
    (hBeta : 0 < beta) :
    Integrable id (ProbabilityTheory.betaMeasure alpha beta) := by
  rw [ProbabilityTheory.betaMeasure]
  apply (integrable_withDensity_iff (μ := volume)
    (ProbabilityTheory.measurable_betaPDFReal alpha beta).ennreal_ofReal
    (by simp)).mpr
  have hShift := (beta_pdf_integrable (add_pos hAlpha zero_lt_one) hBeta).const_mul
    (alpha / (alpha + beta))
  apply hShift.congr
  exact ae_of_all _ fun value => by
    simpa only [id_eq, ProbabilityTheory.betaPDF,
      ENNReal.toReal_ofReal (beta_pdf_nonneg alpha beta hAlpha hBeta value)] using
      (beta_mul_pdf alpha beta hAlpha hBeta value).symm

private theorem beta_mean_at {alpha beta : ℝ} (hAlpha : 0 < alpha)
    (hBeta : 0 < beta) :
    (∫ value : ℝ, value ∂ProbabilityTheory.betaMeasure alpha beta) =
      alpha / (alpha + beta) := by
  calc
    (∫ value : ℝ, value ∂ProbabilityTheory.betaMeasure alpha beta) =
        ∫ value : ℝ, (ProbabilityTheory.betaPDF alpha beta value).toReal • value := by
          rw [ProbabilityTheory.betaMeasure]
          exact integral_withDensity_eq_integral_toReal_smul
            (ProbabilityTheory.measurable_betaPDFReal alpha beta).ennreal_ofReal
            (by simp) _
    _ = ∫ value : ℝ, (alpha / (alpha + beta)) *
        ProbabilityTheory.betaPDFReal (alpha + 1) beta value := by
          apply integral_congr_ae
          exact ae_of_all _ fun value => by
            simpa only [smul_eq_mul, ProbabilityTheory.betaPDF, mul_comm,
              ENNReal.toReal_ofReal (beta_pdf_nonneg alpha beta hAlpha hBeta value)] using
              beta_mul_pdf alpha beta hAlpha hBeta value
    _ = alpha / (alpha + beta) := by
      rw [integral_const_mul,
        integral_beta_pdf_eq_one (add_pos hAlpha zero_lt_one) hBeta, mul_one]

private theorem beta_integrable_id (params : Determinize.Statement.Paper.Params .beta)
    (hDomain : Determinize.Statement.Paper.domain .beta params) :
    Integrable id (Determinize.Statement.Paper.paperMeasure .beta params) := by
  rcases hDomain with ⟨hAlpha, hBeta⟩
  change Integrable id (if h : 0 < params.2 0 ∧
    0 < params.2 1 then
      ProbabilityTheory.betaMeasure (params.2 0)
        (params.2 1) else 0)
  rw [dif_pos ⟨hAlpha, hBeta⟩]
  exact beta_integrable_id_at hAlpha hBeta

private theorem beta_mean (params : Determinize.Statement.Paper.Params .beta)
    (hDomain : Determinize.Statement.Paper.domain .beta params) :
    (∫ value : ℝ, value ∂Determinize.Statement.Paper.paperMeasure .beta params) =
      Determinize.Statement.Paper.meanValue .beta params := by
  rcases hDomain with ⟨hAlpha, hBeta⟩
  change (∫ value : ℝ, value ∂if h : 0 < params.2 0 ∧
    0 < params.2 1 then
      ProbabilityTheory.betaMeasure (params.2 0)
        (params.2 1) else 0) = _
  rw [dif_pos ⟨hAlpha, hBeta⟩, beta_mean_at hAlpha hBeta]
  rw [Determinize.Proof.Paper.meanValue_eq_affine]
  unfold Determinize.Proof.Paper.meanConstant
  have hSum : (∑ i : Fin (Determinize.Statement.Paper.affineArity .beta),
      Determinize.Proof.Paper.meanCoeff .beta params.2 i * params.1 i) = 0 := by
    apply Finset.sum_eq_zero
    intro index _
    exact Fin.elim0 index
  rw [hSum, add_zero]

private noncomputable def gaussianKernel :
    ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params .gaussian) ℝ :=
  ⟨Determinize.Statement.Paper.paperMeasure .gaussian, measurable_paperMeasure_gaussian⟩

noncomputable def primitiveKernel :
    (op : Determinize.Statement.Paper.Op) → ProbabilityTheory.Kernel (Determinize.Statement.Paper.Params op) ℝ
  | .uniform => uniformKernel
  | .gaussian => gaussianKernel
  | .poisson => poissonKernel
  | .exponential => exponentialKernel
  | .beta => betaKernel
  | .gamma => gammaKernel
  | .bernoulli => ⟨Determinize.Statement.Paper.paperMeasure .bernoulli,
      DiscreteLaws.bernoulli_measurable .stochastic |>.comp
        (show Measurable (fun params : Determinize.Statement.Paper.Params .bernoulli =>
          params.1 0) by fun_prop)⟩
  | .discrete d => ⟨Determinize.Statement.Paper.paperMeasure (.discrete d), measurable_const⟩

theorem primitiveKernel_apply (op : Determinize.Statement.Paper.Op) (params : Determinize.Statement.Paper.Params op) :
    primitiveKernel op params = Determinize.Statement.Paper.paperMeasure op params := by
  cases op with
  | uniform => exact uniformKernel_apply params
  | gaussian => rfl
  | poisson => exact poissonKernel_apply params
  | exponential => exact exponentialKernel_apply params
  | beta => exact betaKernel_apply params
  | gamma => exact gammaKernel_apply params
  | bernoulli => rfl
  | discrete _ => rfl

theorem primitiveKernel_finite (op : Determinize.Statement.Paper.Op) :
    ProbabilityTheory.IsFiniteKernel (primitiveKernel op) := by
  constructor
  exact ⟨1, ENNReal.one_lt_top, fun params => by
    rw [primitiveKernel_apply]
    exact paperMeasure_mass_le_one op params⟩

noncomputable def primitiveLaws : Determinize.Proof.Paper.PrimitiveLaws where
  kernel := primitiveKernel
  kernel_eq_paperMeasure := primitiveKernel_apply
  kernel_sfinite := by
    intro op
    letI : ProbabilityTheory.IsFiniteKernel (primitiveKernel op) :=
      primitiveKernel_finite op
    infer_instance
  mass_le_one := by
    intro op params
    rw [primitiveKernel_apply]
    exact paperMeasure_mass_le_one op params
  kernel_zero_off_domain := by
    intro op params hDomain
    rw [primitiveKernel_apply]
    exact paperMeasure_zero_off_domain op params hDomain
  mass_one := by
    intro op params hDomain
    rw [primitiveKernel_apply]
    exact paperMeasure_mass_one op params hDomain
  integrable_id := by
    intro op params hDomain
    rw [primitiveKernel_apply]
    cases op with
    | uniform => exact uniform_integrable_id params hDomain
    | gaussian => exact gaussian_integrable_id params hDomain
    | poisson => exact poisson_integrable_id params hDomain
    | exponential => exact exponential_integrable_id params hDomain
    | beta => exact beta_integrable_id params hDomain
    | gamma => exact gamma_integrable_id params hDomain
    | bernoulli => exact DiscreteLaws.bernoulli_integrable .stochastic (params.1 0) _
    | discrete d => exact DiscreteLaws.discrete_integrable .stochastic d _
  mean_law := by
    intro op params hDomain
    rw [primitiveKernel_apply]
    cases op with
    | uniform => exact uniform_mean params hDomain
    | gaussian => exact gaussian_mean params hDomain
    | poisson => exact poisson_mean params hDomain
    | exponential => exact exponential_mean params hDomain
    | beta => exact beta_mean params hDomain
    | gamma => exact gamma_mean params hDomain
    | bernoulli => exact DiscreteLaws.bernoulli_mean .stochastic (params.1 0) hDomain
    | discrete d => exact DiscreteLaws.discrete_mean .stochastic d

end Determinize.Proof.Paper
