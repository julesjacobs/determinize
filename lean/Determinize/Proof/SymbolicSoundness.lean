import Determinize.Proof.SymbolicMean
import Mathlib.Probability.Kernel.Composition.IntegralCompProd
import Mathlib.Probability.Kernel.Composition.MeasureComp

/-!
# Soundness of symbolic reduction

The symbolic semantics preserves typing and primitive-domain safety while
replacing E samples by their means. Operational trace soundness builds on these
invariants in `CompactFiberSoundness`.
-/

namespace Determinize.Proof.Paper.SymbolicSoundness

open MeasureTheory ProbabilityTheory
open Determinize.Spec.Paper

attribute [local simp] Determinize.Spec.Paper.reduce

namespace SampleEnv

theorem convex_integral_mem_real {α : Type*} [MeasurableSpace α]
    {μ : Measure α} [IsProbabilityMeasure μ] {set : Set ℝ} {value : α → ℝ}
    (hConvex : Convex ℝ set) (hMem : ∀ᵐ x ∂μ, value x ∈ set)
    (hIntegrable : Integrable value μ) : (∫ x, value x ∂μ) ∈ set := by
  let mean := ∫ x, value x ∂μ
  change mean ∈ set
  by_contra hMean
  have hNonempty : set.Nonempty := by
    rcases hMem.exists with ⟨x, hx⟩
    exact ⟨value x, hx⟩
  rcases hNonempty with ⟨point, hPoint⟩
  have hPointNe : point ≠ mean := fun equality => hMean (equality ▸ hPoint)
  have hConstant : Integrable (fun _ : α => mean) μ := integrable_const mean
  have hConstantIntegral : (∫ _ : α, mean ∂μ) = mean := by simp
  rcases lt_or_gt_of_ne hPointNe with hBelow | hAbove
  · have hAllBelow : ∀ y ∈ set, y < mean := by
      intro y hy
      by_contra hNotBelow
      exact hMean (hConvex.ordConnected.out hPoint hy
        ⟨hBelow.le, le_of_not_gt hNotBelow⟩)
    have hStrict : ∀ᵐ x ∂μ, value x < mean :=
      hMem.mono fun x hx => hAllBelow (value x) hx
    have hEq : value =ᵐ[μ] fun _ => mean :=
      (integral_eq_iff_of_ae_le hIntegrable hConstant
        (hStrict.mono fun _ hx => hx.le)).mp (by rw [hConstantIntegral])
    exact (hEq.and hStrict).exists.elim fun _ hx => (hx.2.ne hx.1).elim
  · have hAllAbove : ∀ y ∈ set, mean < y := by
      intro y hy
      by_contra hNotAbove
      exact hMean (hConvex.ordConnected.out hy hPoint
        ⟨le_of_not_gt hNotAbove, hAbove.le⟩)
    have hStrict : ∀ᵐ x ∂μ, mean < value x :=
      hMem.mono fun x hx => hAllAbove (value x) hx
    have hEq : (fun _ : α => mean) =ᵐ[μ] value :=
      (integral_eq_iff_of_ae_le hConstant hIntegrable
        (hStrict.mono fun _ hx => hx.le)).mp (by rw [hConstantIntegral])
    exact (hEq.and hStrict).exists.elim fun _ hx => (hx.2.ne hx.1).elim

namespace Affine

open Symbolic.Affine (add smul tail primitiveMean eval_primitiveMean)

noncomputable def substituteHeadMean (expression : Symbolic.Affine (n + 1)) (op : Determinize.Spec.Paper.Op)
    (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ) : Symbolic.Affine n :=
  add (tail expression) (smul (expression.2 0) (primitiveMean op affineArgs generalArgs))

theorem eval_cons (expression : Symbolic.Affine (n + 1)) (value : ℝ)
    (environment : Env n) :
    Symbolic.Affine.eval expression (Env.cons value environment) =
      Symbolic.Affine.eval (tail expression) environment + expression.2 0 * value := by
  simp only [Symbolic.Affine.eval, tail, Fin.tail, Fin.sum_univ_succ, Env.cons_zero,
    Env.cons_succ]
  ring

theorem eval_cons_convexCombination (expression : Symbolic.Affine (n + 1))
    (environment : Env n) (left right a b : ℝ) (hSum : a + b = 1) :
    Symbolic.Affine.eval (tail expression) environment +
        expression.2 0 * (a * left + b * right) =
      a * (Symbolic.Affine.eval (tail expression) environment + expression.2 0 * left) +
        b * (Symbolic.Affine.eval (tail expression) environment + expression.2 0 * right) := by
  linear_combination -(Symbolic.Affine.eval (tail expression) environment) * hSum

theorem eval_substituteHeadMean (expression : Symbolic.Affine (n + 1)) (op : Determinize.Spec.Paper.Op)
    (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ)
    (environment : Env n) :
    Symbolic.Affine.eval (substituteHeadMean expression op affineArgs generalArgs) environment =
      Symbolic.Affine.eval (tail expression) environment + expression.2 0 *
        Determinize.Spec.Paper.meanValue op
          (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs) := by
  rw [substituteHeadMean, Symbolic.Affine.eval_add, Symbolic.Affine.eval_smul, eval_primitiveMean]

end Affine

theorem domain_valueSet_convex (queryOp : Determinize.Spec.Paper.Op)
    (queryAffineArgs : Fin (Determinize.Spec.Paper.affineArity queryOp) → Symbolic.Affine (n + 1))
    (queryGeneralArgs : Fin (Determinize.Spec.Paper.generalArity queryOp) → ℝ)
    (environment : Env n) :
    Convex ℝ {value : ℝ | Determinize.Spec.Paper.domain queryOp
      (fun i => Symbolic.Affine.eval (queryAffineArgs i) (Env.cons value environment),
        queryGeneralArgs)} := by
  rw [convex_iff_add_mem]
  intro left leftDomain right rightDomain a b ha hb hab
  cases queryOp with
  | uniform =>
      change Fin 2 → Symbolic.Affine (n + 1) at queryAffineArgs
      change Fin 0 → ℝ at queryGeneralArgs
      simp only [Determinize.Spec.Paper.domain, Set.mem_ofPred_eq] at leftDomain rightDomain ⊢
      simp only [Affine.eval_cons, smul_eq_mul] at leftDomain rightDomain ⊢
      simp_rw [Affine.eval_cons_convexCombination _ environment left right a b hab]
      exact add_le_add (mul_le_mul_of_nonneg_left leftDomain ha)
        (mul_le_mul_of_nonneg_left rightDomain hb)
  | gaussian => exact leftDomain
  | poisson =>
      change Fin 1 → Symbolic.Affine (n + 1) at queryAffineArgs
      change Fin 0 → ℝ at queryGeneralArgs
      simp only [Determinize.Spec.Paper.domain, Set.mem_ofPred_eq] at leftDomain rightDomain ⊢
      simp only [Affine.eval_cons, smul_eq_mul] at leftDomain rightDomain ⊢
      rw [Affine.eval_cons_convexCombination _ environment left right a b hab]
      exact add_nonneg (mul_nonneg ha leftDomain) (mul_nonneg hb rightDomain)
  | exponential => exact leftDomain
  | beta => exact leftDomain
  | gamma =>
      change Fin 1 → Symbolic.Affine (n + 1) at queryAffineArgs
      change Fin 1 → ℝ at queryGeneralArgs
      simp only [Determinize.Spec.Paper.domain, Set.mem_ofPred_eq] at leftDomain rightDomain ⊢
      constructor
      · simp only [Affine.eval_cons, smul_eq_mul] at leftDomain rightDomain ⊢
        rw [Affine.eval_cons_convexCombination _ environment left right a b hab]
        rcases ha.eq_or_lt with rfl | haPos
        · have hbOne : b = 1 := by linarith
          simpa [hbOne] using rightDomain.1
        · exact add_pos_of_pos_of_nonneg (mul_pos haPos leftDomain.1)
            (mul_nonneg hb rightDomain.1.le)
      · exact leftDomain.2

  | bernoulli =>
      change Fin 1 → Symbolic.Affine (n + 1) at queryAffineArgs
      change Fin 0 → ℝ at queryGeneralArgs
      simp only [Determinize.Spec.Paper.domain, Set.mem_ofPred_eq] at leftDomain rightDomain ⊢
      simp only [Affine.eval_cons, smul_eq_mul] at leftDomain rightDomain ⊢
      rw [Affine.eval_cons_convexCombination _ environment left right a b hab]
      constructor
      · exact add_nonneg (mul_nonneg ha leftDomain.1) (mul_nonneg hb rightDomain.1)
      · have bound := add_le_add (mul_le_mul_of_nonneg_left leftDomain.2 ha)
          (mul_le_mul_of_nonneg_left rightDomain.2 hb)
        simpa [hab] using bound
  | discrete _ => trivial

noncomputable def transitionPack (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (op : Determinize.Spec.Paper.Op)
    (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ) :
    SFiniteKernel (Env n) (Env (n + 1)) :=
  SFiniteKernel.mapWithInput
    (SFiniteKernel.pullback (primitiveKernelPack laws (.sample .E) op)
      (fun environment : Env n =>
        (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs))
      ((measurable_pi_lambda _ fun i =>
        Determinize.Proof.Paper.Symbolic.AffineExpr.affine_eval_measurable (affineArgs i)).prodMk
        measurable_const))
    (fun input => Env.cons input.2 input.1)
    (measurable_envCons.comp (measurable_snd.prodMk measurable_fst))

theorem transitionPack_apply (laws : Determinize.Proof.Paper.PrimitiveLaws) (op : Determinize.Spec.Paper.Op)
    (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ)
    (environment : Env n) :
    (transitionPack laws op affineArgs generalArgs).kernel environment =
      (laws.kernel op
        (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)).map
          (fun value => Env.cons value environment) := by
  rw [transitionPack, SFiniteKernel.mapWithInput_apply, MeasurableActionFamily.pullback_apply]
  rfl

theorem actualMeasure_univ_eq_one (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (history : Symbolic.SampleEnv laws n)
    (safe : Symbolic.SampleEnv.DomainSafe laws history) :
    Symbolic.SampleEnv.actualMeasure laws history Set.univ = 1 := by
  induction history with
  | nil => simp [Symbolic.SampleEnv.actualMeasure]
  | snoc history op affineArgs generalArgs ih =>
      rw [Determinize.Proof.Paper.Symbolic.AffineExpr.actualMeasure_snoc]
      have transitionEq :
          (fun environment =>
            (laws.kernel op
              (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)).map
                (fun value => Env.cons value environment)) =
            (transitionPack laws op affineArgs generalArgs).kernel := by
        funext environment
        exact (transitionPack_apply laws op affineArgs generalArgs environment).symm
      rw [transitionEq]
      rw [Measure.bind_apply MeasurableSet.univ
        (transitionPack laws op affineArgs generalArgs).kernel.aemeasurable]
      have fiberOne : ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
          (transitionPack laws op affineArgs generalArgs).kernel environment Set.univ = 1 := by
        filter_upwards [safe.2] with environment domain
        rw [transitionPack_apply]
        have sectionMeasurable : Measurable (fun value : ℝ =>
            Env.cons value environment) :=
          measurable_envCons.comp (measurable_id.prodMk measurable_const)
        rw [Measure.map_apply sectionMeasurable MeasurableSet.univ]
        simpa only [Set.preimage_univ] using laws.mass_one op _ domain
      calc
        (∫⁻ environment,
            (transitionPack laws op affineArgs generalArgs).kernel environment Set.univ
            ∂Symbolic.SampleEnv.actualMeasure laws history) =
            ∫⁻ _environment, 1 ∂Symbolic.SampleEnv.actualMeasure laws history :=
          lintegral_congr_ae fiberOne
        _ = Symbolic.SampleEnv.actualMeasure laws history Set.univ := lintegral_one
        _ = 1 := ih safe.1

theorem actualMeasure_snoc_eq_comp (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (history : Symbolic.SampleEnv laws n) (op : Determinize.Spec.Paper.Op)
    (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ) :
    Symbolic.SampleEnv.actualMeasure laws
        (.snoc history op affineArgs generalArgs) =
      (transitionPack laws op affineArgs generalArgs).kernel ∘ₘ
        Symbolic.SampleEnv.actualMeasure laws history := by
  rw [Determinize.Proof.Paper.Symbolic.AffineExpr.actualMeasure_snoc]
  apply Measure.bind_congr_right
  filter_upwards [] with environment
  exact (transitionPack_apply laws op affineArgs generalArgs environment).symm

theorem integrable_eval_transition (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (op : Determinize.Spec.Paper.Op)
    (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ)
    (expression : Symbolic.Affine (n + 1)) (environment : Env n)
    (domain : Determinize.Spec.Paper.domain op
      (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)) :
    Integrable (fun nextEnvironment => Symbolic.Affine.eval expression nextEnvironment)
      ((transitionPack laws op affineArgs generalArgs).kernel environment) := by
  rw [transitionPack_apply]
  have sectionMeasurable : Measurable (fun value : ℝ => Env.cons value environment) :=
    measurable_envCons.comp (measurable_id.prodMk measurable_const)
  rw [integrable_map_measure
    (Determinize.Proof.Paper.Symbolic.AffineExpr.affine_eval_measurable
      expression).aestronglyMeasurable sectionMeasurable.aemeasurable]
  let params : Determinize.Spec.Paper.Params op :=
    (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)
  have mass : laws.kernel op params Set.univ = 1 := laws.mass_one op params domain
  let _ : IsFiniteMeasure (laws.kernel op params) := ⟨by rw [mass]; simp⟩
  change Integrable (Symbolic.Affine.eval expression ∘
    fun value => Env.cons value environment) (laws.kernel op params)
  have affineIntegrable := (integrable_const
    (Symbolic.Affine.eval (Symbolic.Affine.tail expression) environment)).add
      ((laws.integrable_id op params domain).const_mul (expression.2 0))
  have functionEq : (Symbolic.Affine.eval expression ∘
      fun value => Env.cons value environment) =
      (fun value => Symbolic.Affine.eval (Symbolic.Affine.tail expression) environment) +
        fun value => expression.2 0 * value := by
    funext value
    exact Affine.eval_cons expression value environment
  rw [functionEq]
  exact affineIntegrable

theorem integral_eval_transition (laws : Determinize.Proof.Paper.PrimitiveLaws) (op : Determinize.Spec.Paper.Op)
    (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ)
    (expression : Symbolic.Affine (n + 1)) (environment : Env n)
    (domain : Determinize.Spec.Paper.domain op
      (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)) :
    (∫ nextEnvironment, Symbolic.Affine.eval expression nextEnvironment
        ∂(transitionPack laws op affineArgs generalArgs).kernel environment) =
      Symbolic.Affine.eval
        (Affine.substituteHeadMean expression op affineArgs generalArgs) environment := by
  rw [transitionPack_apply]
  have sectionMeasurable : Measurable (fun value : ℝ => Env.cons value environment) :=
    measurable_envCons.comp (measurable_id.prodMk measurable_const)
  rw [integral_map sectionMeasurable.aemeasurable
    (Determinize.Proof.Paper.Symbolic.AffineExpr.affine_eval_measurable
      expression).aestronglyMeasurable]
  simp_rw [Affine.eval_cons]
  let params : Determinize.Spec.Paper.Params op :=
    (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)
  have mass : laws.kernel op params Set.univ = 1 := laws.mass_one op params domain
  let _ : IsFiniteMeasure (laws.kernel op params) := ⟨by rw [mass]; simp⟩
  have identityIntegrable : Integrable (fun value : ℝ => value) (laws.kernel op params) :=
    laws.integrable_id op params domain
  have constantIntegrable : Integrable
      (fun _value : ℝ => Symbolic.Affine.eval (Symbolic.Affine.tail expression) environment)
      (laws.kernel op params) :=
    integrable_const_iff.mpr (Or.inr inferInstance)
  have scaledIntegrable : Integrable
      (fun value : ℝ => expression.2 0 * value) (laws.kernel op params) :=
    identityIntegrable.const_mul _
  rw [integral_add constantIntegrable scaledIntegrable]
  rw [integral_const, integral_const_mul]
  rw [laws.mean_law op params domain]
  rw [Affine.eval_substituteHeadMean]
  simp only [Measure.real, mass, ENNReal.toReal_one, one_smul]
  rfl

theorem domain_at_meanEnvironment (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (history : Symbolic.SampleEnv laws n)
    (safe : Symbolic.SampleEnv.DomainSafe laws history)
    (op : Determinize.Spec.Paper.Op) (affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ)
    (domainAE : ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
      Determinize.Spec.Paper.domain op
        (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs)) :
    Determinize.Spec.Paper.domain op
      (fun i => Symbolic.Affine.eval (affineArgs i)
        (Symbolic.SampleEnv.meanEnvironment laws history), generalArgs) := by
  induction history generalizing op with
  | nil =>
      simpa [Symbolic.SampleEnv.actualMeasure, Symbolic.SampleEnv.meanEnvironment,
        ] using domainAE
  | @snoc n history sampledOp sampledAffineArgs sampledGeneralArgs ih =>
      let prior := Symbolic.SampleEnv.actualMeasure laws history
      let transition :=
        (transitionPack laws sampledOp sampledAffineArgs sampledGeneralArgs).kernel
      have actualEq : Symbolic.SampleEnv.actualMeasure laws
          (.snoc history sampledOp sampledAffineArgs sampledGeneralArgs) = transition ∘ₘ prior :=
        actualMeasure_snoc_eq_comp laws history sampledOp sampledAffineArgs sampledGeneralArgs
      have fiberDomain : ∀ᵐ environment ∂prior,
          ∀ᵐ nextEnvironment ∂transition environment,
            Determinize.Spec.Paper.domain op
              (fun i => Symbolic.Affine.eval (affineArgs i) nextEnvironment, generalArgs) := by
        rw [actualEq] at domainAE
        exact Measure.ae_ae_of_ae_bind transition.aemeasurable domainAE
      let reducedArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n := fun i =>
        Affine.substituteHeadMean (affineArgs i) sampledOp
          sampledAffineArgs sampledGeneralArgs
      have reducedDomainAE : ∀ᵐ environment ∂prior,
          Determinize.Spec.Paper.domain op
            (fun i => Symbolic.Affine.eval (reducedArgs i) environment, generalArgs) := by
        filter_upwards [safe.2, fiberDomain] with environment sampledDomain queryDomain
        let sampledParams : Determinize.Spec.Paper.Params sampledOp :=
          (fun i => Symbolic.Affine.eval (sampledAffineArgs i) environment,
            sampledGeneralArgs)
        have transitionApply : transition environment =
            (laws.kernel sampledOp sampledParams).map
              (fun value => Env.cons value environment) := by
          exact transitionPack_apply laws sampledOp sampledAffineArgs sampledGeneralArgs environment
        rw [transitionApply] at queryDomain
        have sectionMeasurable : Measurable (fun value : ℝ => Env.cons value environment) :=
          measurable_envCons.comp (measurable_id.prodMk measurable_const)
        have drawDomain : ∀ᵐ value ∂laws.kernel sampledOp sampledParams,
            Determinize.Spec.Paper.domain op
              (fun i => Symbolic.Affine.eval (affineArgs i)
                (Env.cons value environment), generalArgs) :=
          MeasureTheory.ae_of_ae_map sectionMeasurable.aemeasurable queryDomain
        let _ : IsProbabilityMeasure (laws.kernel sampledOp sampledParams) :=
          ⟨laws.mass_one sampledOp sampledParams sampledDomain⟩
        have meanDomain := convex_integral_mem_real
          (domain_valueSet_convex op affineArgs generalArgs environment) drawDomain
          (laws.integrable_id sampledOp sampledParams sampledDomain)
        rw [laws.mean_law sampledOp sampledParams sampledDomain] at meanDomain
        change Determinize.Spec.Paper.domain op
          (fun i => Symbolic.Affine.eval (affineArgs i)
            (Env.cons (Determinize.Spec.Paper.meanValue sampledOp sampledParams) environment), generalArgs)
          at meanDomain
        simpa only [reducedArgs, Affine.eval_substituteHeadMean, ← Affine.eval_cons]
          using meanDomain
      have reducedAtMean := ih safe.1 op reducedArgs generalArgs reducedDomainAE
      rw [Symbolic.SampleEnv.meanEnvironment]
      simpa only [reducedArgs, Affine.eval_substituteHeadMean, ← Affine.eval_cons]
        using reducedAtMean

end SampleEnv

namespace TargetSafety

/-- The source-side invariant used while replacing the recorded E draws by
their sequential conditional means. -/
def SafeConfigAt (laws : Determinize.Proof.Paper.PrimitiveLaws) (fuel : Nat)
    (history : Symbolic.SampleEnv laws n) (expression : Symbolic.AffineExpr n) : Prop :=
  Symbolic.SampleEnv.DomainSafe laws history ∧
    Symbolic.AffineExpr.WellTyped [] expression (.float .E) ∧
    ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
      PrimitiveDomainSafeAt fuel (expression.realize environment)

theorem stochastic_mass_one_imp_domain (laws : Determinize.Proof.Paper.PrimitiveLaws)
    (op : Determinize.Spec.Paper.Op) (params : Determinize.Spec.Paper.Params op)
    (mass : laws.kernel op params Set.univ = 1) : Determinize.Spec.Paper.domain op params := by
  by_contra outside
  rw [laws.kernel_zero_off_domain op params outside] at mass
  simp at mass

theorem nStepMeasure_mass_le_one (stepKernel : StepKernel)
    (fuel : Nat) (expression : Expr) :
    nStepMeasure stepKernel fuel expression Set.univ ≤ 1 := by
  induction fuel generalizing expression with
  | zero => simp [nStepMeasure]
  | succ fuel ih =>
      rw [nStepMeasure, Measure.bind_apply MeasurableSet.univ
        stepKernel.kernel.aemeasurable]
      calc
        (∫⁻ current, stepKernel.kernel current Set.univ
            ∂nStepMeasure stepKernel fuel expression) ≤
            ∫⁻ _current, 1 ∂nStepMeasure stepKernel fuel expression :=
          lintegral_mono fun current => stepKernel.mass_le_one current
        _ = nStepMeasure stepKernel fuel expression Set.univ := lintegral_one
        _ ≤ 1 := ih expression

theorem ae_eq_one_of_lintegral_eq_one {α : Type*} [MeasurableSpace α]
    (μ : Measure α) (mass : μ Set.univ = 1)
    (f : α → ENNReal) (measurableF : Measurable f)
    (bounded : ∀ x, f x ≤ 1) (integral : (∫⁻ x, f x ∂μ) = 1) :
    ∀ᵐ x ∂μ, f x = 1 := by
  have finite : (∫⁻ x, f x ∂μ) ≠ ⊤ := by simp [integral]
  have subIntegral : (∫⁻ x, 1 - f x ∂μ) = 0 := by
    rw [lintegral_sub measurableF finite (Filter.Eventually.of_forall bounded)]
    simp [mass, integral]
  have subZero := (lintegral_eq_zero_iff'
    (measurable_const.sub measurableF).aemeasurable).mp subIntegral
  filter_upwards [subZero] with x hx
  exact le_antisymm (bounded x) (tsub_eq_zero_iff_le.mp hx)

theorem nStepMeasure_univ_eq_one_of_value (stepKernel : StepKernel)
    (fuel : Nat) (expression : Expr) (value : expression.isValue = true) :
    nStepMeasure stepKernel fuel expression Set.univ = 1 := by
  have measureEq : ∀ fuel,
      nStepMeasure stepKernel fuel expression = Measure.dirac expression := by
    intro depth
    induction depth with
    | zero => rfl
    | succ depth ih =>
        rw [nStepMeasure, ih, Measure.dirac_bind stepKernel.kernel.measurable,
          Determinize.Proof.Paper.MeasurableActionFamily.stepKernel_eq_dirac_of_value
            stepKernel expression value]
  rw [measureEq fuel]
  simp

theorem nStepMeasure_succ_eq_firstStep (stepKernel : StepKernel)
    (fuel : Nat) (expression : Expr) :
    nStepMeasure stepKernel (fuel + 1) expression =
      (stepKernel.kernel expression).bind (nStepMeasure stepKernel fuel) := by
  rw [← Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_apply
    stepKernel (fuel + 1) expression]
  rw [Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_succ_kernel]
  rw [Determinize.Proof.Paper.MeasurableActionFamily.nStepKernel_commutes]
  rw [Kernel.comp_apply]
  congr 1
  funext successor
  exact Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_apply
    stepKernel fuel successor

theorem nStepMeasure_succ_sample_univ
    (stepKernel : StepKernel) (fuel : Nat) (expression : Expr)
    (fiber : Measure ℝ) (continuation : ℝ → Expr)
    (reduction : reduce expression = .sample site fiber continuation) :
    nStepMeasure stepKernel (fuel + 1) expression Set.univ =
      ∫⁻ value, nStepMeasure stepKernel fuel (continuation value) Set.univ ∂fiber := by
  rw [nStepMeasure_succ_eq_firstStep stepKernel fuel expression,
    show nStepMeasure stepKernel fuel =
        (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
          stepKernel fuel).kernel from by
      funext successor
      exact (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_apply
        stepKernel fuel successor).symm,
    Measure.bind_apply MeasurableSet.univ
      (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
        stepKernel fuel).kernel.aemeasurable]
  rw [stepKernel.kernel_eq_stepMeasure]
  unfold stepMeasure
  rw [reduction]
  simp only [Determinize.Spec.Paper.Action.measure]
  have continuationMeasurable := stepKernel.sample_continuation_measurable
    expression fiber continuation reduction
  rw [MeasureTheory.lintegral_map'
    ((Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
      stepKernel fuel).kernel.measurable_coe MeasurableSet.univ).aemeasurable
    continuationMeasurable.aemeasurable]

theorem nStepMeasure_succ_next_univ
    (stepKernel : StepKernel) (fuel : Nat) (expression next : Expr)
    (reduction : reduce expression = .next next) :
    nStepMeasure stepKernel (fuel + 1) expression Set.univ =
      nStepMeasure stepKernel fuel next Set.univ := by
  rw [nStepMeasure_succ_eq_firstStep stepKernel fuel expression, stepKernel.kernel_eq_stepMeasure]
  unfold stepMeasure
  rw [reduction]
  simp only [Determinize.Spec.Paper.Action.measure]
  rw [show nStepMeasure stepKernel fuel =
      (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
        stepKernel fuel).kernel from by
    funext successor
    exact (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_apply
      stepKernel fuel successor).symm,
    Measure.dirac_bind
      (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
        stepKernel fuel).kernel.measurable]

theorem doesNotGetStuckAt_iff_nStepMeasure_univ_eq_one
    (stepKernel : StepKernel) (fuel : Nat) (expression : Expr)
    (typed : Determinize.Spec.Paper.Typed [] expression ty) :
    Determinize.Spec.Paper.DoesNotGetStuckAt fuel expression ↔
      nStepMeasure stepKernel fuel expression Set.univ = 1 := by
  induction fuel generalizing expression ty with
  | zero => simp [DoesNotGetStuckAt, nStepMeasure]
  | succ fuel ih =>
      by_cases value : expression.isValue = true
      · simp only [DoesNotGetStuckAt, value, ↓reduceIte]
        exact (iff_true_intro
          (nStepMeasure_univ_eq_one_of_value stepKernel (fuel + 1) expression value)).symm
      · have valueFalse : expression.isValue = false := Bool.eq_false_of_not_eq_true value
        simp only [DoesNotGetStuckAt, valueFalse, Bool.false_eq_true, ↓reduceIte]
        have actionTyped := Typing.reduce_typed_closed typed
        cases reduction : reduce expression with
        | next next =>
          simp only
          rw [reduction] at actionTyped
          cases actionTyped with
            | next nextTyped =>
                rw [nStepMeasure_succ_next_univ stepKernel fuel expression next reduction]
                exact ih next nextTyped
        | sample site fiber continuation =>
          simp only
          rw [reduction] at actionTyped
          cases actionTyped with
            | sample continuationTyped =>
                rw [nStepMeasure_succ_sample_univ stepKernel fuel expression fiber continuation
                  reduction]
                constructor
                · rintro ⟨fiberMass, continuationSafe⟩
                  calc
                    (∫⁻ value, nStepMeasure stepKernel fuel (continuation value) Set.univ
                        ∂fiber) = ∫⁻ _value, 1 ∂fiber :=
                      lintegral_congr_ae (continuationSafe.mono fun value safe =>
                        (ih (continuation value) (continuationTyped value)).mp safe)
                    _ = fiber Set.univ := lintegral_one
                    _ = 1 := fiberMass
                · intro totalMass
                  have integralLeFiber :
                      (∫⁻ value, nStepMeasure stepKernel fuel (continuation value) Set.univ
                          ∂fiber) ≤ fiber Set.univ := by
                    calc
                      (∫⁻ value, nStepMeasure stepKernel fuel (continuation value) Set.univ
                          ∂fiber) ≤ ∫⁻ _value, 1 ∂fiber :=
                        lintegral_mono fun value =>
                          nStepMeasure_mass_le_one stepKernel fuel (continuation value)
                      _ = fiber Set.univ := lintegral_one
                  have fiberLeOne : fiber Set.univ ≤ 1 := by
                    have kernelLeOne := stepKernel.mass_le_one expression
                    rw [stepKernel.kernel_eq_stepMeasure] at kernelLeOne
                    unfold stepMeasure at kernelLeOne
                    rw [reduction] at kernelLeOne
                    simp only [Determinize.Spec.Paper.Action.measure] at kernelLeOne
                    rw [Measure.map_apply
                      (stepKernel.sample_continuation_measurable expression fiber continuation
                        reduction) MeasurableSet.univ] at kernelLeOne
                    simpa using kernelLeOne
                  have fiberMass : fiber Set.univ = 1 :=
                    le_antisymm fiberLeOne (totalMass ▸ integralLeFiber)
                  have continuationMeasurable :=
                    stepKernel.sample_continuation_measurable expression fiber continuation reduction
                  have massMeasurable : Measurable fun value =>
                      nStepMeasure stepKernel fuel (continuation value) Set.univ := by
                    have kernelMeasurable :=
                      (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
                        stepKernel fuel).kernel.measurable_coe MeasurableSet.univ
                    have composed := kernelMeasurable.comp continuationMeasurable
                    change Measurable ((fun successor =>
                      nStepMeasure stepKernel fuel successor Set.univ) ∘ continuation)
                    simpa only [Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_apply]
                      using composed
                  refine ⟨fiberMass, ?_⟩
                  have continuationMass := ae_eq_one_of_lintegral_eq_one fiber fiberMass
                    (fun value => nStepMeasure stepKernel fuel (continuation value) Set.univ)
                    massMeasurable
                    (fun value => nStepMeasure_mass_le_one stepKernel fuel (continuation value))
                    totalMass
                  filter_upwards [continuationMass] with value mass
                  exact (ih (continuation value) (continuationTyped value)).mpr mass
        | stuck =>
            rw [reduction] at actionTyped
            cases actionTyped

theorem primitiveDomainSafeAt_iff_nStepMeasure_univ_eq_one
    (stepKernel : StepKernel) (fuel : Nat) (expression : Expr)
    (typed : Determinize.Spec.Paper.Typed [] expression ty) :
    PrimitiveDomainSafeAt fuel expression ↔
      nStepMeasure stepKernel fuel expression Set.univ = 1 := by
  rw [← doesNotGetStuckAt_iff_nStepMeasure_univ_eq_one stepKernel fuel expression typed]
  constructor
  · exact Typing.primitiveDomainSafeAt_imp_doesNotGetStuckAt typed
  · exact Typing.doesNotGetStuckAt_imp_primitiveDomainSafeAt

theorem measurable_nStepMass_realize (stepKernel : StepKernel)
    (fuel : Nat) (expression : Symbolic.AffineExpr n) :
    Measurable fun environment =>
      nStepMeasure stepKernel fuel (expression.realize environment) Set.univ := by
  have massMeasurable :=
    (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
      stepKernel fuel).kernel.measurable_coe MeasurableSet.univ
  have composed := massMeasurable.comp expression.realize_measurable
  change Measurable ((fun successor =>
    nStepMeasure stepKernel fuel successor Set.univ) ∘ expression.realize)
  simpa only [Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_apply]
    using composed

theorem measurableSet_primitiveDomainSafeAt_realize
    (stepKernel : StepKernel) (fuel : Nat)
    (expression : Symbolic.AffineExpr n)
    (typed : Symbolic.AffineExpr.WellTyped [] expression ty) :
    MeasurableSet {environment |
      PrimitiveDomainSafeAt fuel (expression.realize environment)} := by
  have setEq : {environment |
      PrimitiveDomainSafeAt fuel (expression.realize environment)} =
      (fun environment => nStepMeasure stepKernel fuel
        (expression.realize environment) Set.univ) ⁻¹' {1} := by
    ext environment
    exact primitiveDomainSafeAt_iff_nStepMeasure_univ_eq_one stepKernel fuel
      (expression.realize environment) (typed.realize_typed environment)
  rw [setEq]
  exact measurable_nStepMass_realize stepKernel fuel expression (measurableSet_singleton 1)

theorem exists_jointContinuation {α : Type*} [MeasurableSpace α]
    {action : α → Action}
    (family : MeasurableActionFamily α action) :
    ∃ joint : α × ℝ → Expr, Measurable joint ∧
      ∀ parameter fiber continuation,
        action parameter = .sample site fiber continuation →
          ∀ value, joint (parameter, value) = continuation value := by
  induction family with
  | @next successor successorMeasurable =>
      exact ⟨fun _ => .unit, measurable_const, by
        intro parameter fiber continuation equality
        cases equality⟩
  | @sample actualSite draw continuation continuationMeasurable =>
      exact ⟨continuation, continuationMeasurable, by
        intro parameter fiber next equality value
        simp only [Action.sample.injEq] at equality
        exact congrFun equality.2.2 value⟩
  | stuck =>
      exact ⟨fun _ => .unit, measurable_const, by
        intro parameter fiber continuation equality
        cases equality⟩
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily
      trueResult falseResult =>
      rcases trueResult with ⟨trueContinuation, trueMeasurable, trueRule⟩
      rcases falseResult with ⟨falseContinuation, falseMeasurable, falseRule⟩
      classical
      let pairRegion : Set (α × ℝ) := {pair | pair.1 ∈ region}
      refine ⟨pairRegion.piecewise trueContinuation falseContinuation, ?_, ?_⟩
      · exact Measurable.piecewise (measurableRegion.preimage measurable_fst)
          trueMeasurable falseMeasurable
      · intro parameter fiber continuation equality value
        by_cases member : parameter ∈ region
        · simp only [Set.piecewise, member, ↓reduceIte] at equality
          change (if parameter ∈ region then trueContinuation (parameter, value)
            else falseContinuation (parameter, value)) = continuation value
          rw [if_pos member]
          exact trueRule parameter fiber continuation equality value
        · simp only [Set.piecewise, member, ↓reduceIte] at equality
          change (if parameter ∈ region then trueContinuation (parameter, value)
            else falseContinuation (parameter, value)) = continuation value
          rw [if_neg member]
          exact falseRule parameter fiber continuation equality value

theorem sampleG_joint_nStepMass_measurable (laws : PrimitiveLaws)
    (stepKernel : StepKernel) (fuel : Nat)
    (expression : Symbolic.AffineExpr n)
    (typed : Symbolic.AffineExpr.WellTyped [] expression ty)
    (fiber : Measure ℝ)
    (continuation : ℝ → Symbolic.AffineExpr n)
    (reduction : Symbolic.AffineExpr.symbolicReduce expression =
      Symbolic.AffineExpr.SymbolicAction.sampleG site fiber continuation) :
    Measurable fun pair : Env n × ℝ =>
      nStepMeasure stepKernel fuel ((continuation pair.2).realize pair.1) Set.univ := by
  let family := Determinize.Proof.Paper.MeasurableActionFamily.reduceFamily
    laws expression.realizeFamily
  rcases exists_jointContinuation family with ⟨joint, jointMeasurable, jointRule⟩
  have jointEq : joint = fun pair : Env n × ℝ =>
      (continuation pair.2).realize pair.1 := by
    funext pair
    exact jointRule pair.1 fiber
      (fun value => (continuation value).realize pair.1) (by
        rw [← Symbolic.AffineExpr.symbolicReduce_realize typed pair.1, reduction]
        rfl) pair.2
  change Measurable ((fun successor =>
    nStepMeasure stepKernel fuel successor Set.univ) ∘
      fun pair : Env n × ℝ => (continuation pair.2).realize pair.1)
  rw [← jointEq]
  have massMeasurable :=
    (Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack
      stepKernel fuel).kernel.measurable_coe MeasurableSet.univ
  have composed := massMeasurable.comp jointMeasurable
  simpa only [Determinize.Proof.Paper.MeasurableActionFamily.nStepKernelPack_apply]
    using composed

set_option maxHeartbeats 800000 in
theorem not_value_of_reduce_sample
    (expression : Expr) (fiber : Measure ℝ) (continuation : ℝ → Expr)
    (reduction : reduce expression = .sample site fiber continuation) :
    expression.isValue ≠ true := by
  intro value
  cases expression <;> simp_all [Expr.isValue, reduce, Bool.and_eq_true]

theorem source_sampleG_safe_swap
    (stepKernel : StepKernel) (fuel : Nat)
    (history : Symbolic.SampleEnv laws n)
    (historySafe : Symbolic.SampleEnv.DomainSafe laws history)
    (expression : Symbolic.AffineExpr n)
    (typed : Symbolic.AffineExpr.WellTyped [] expression ty)
    (sourceSafe : ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
      PrimitiveDomainSafeAt (fuel + 1) (expression.realize environment))
    (fiber : Measure ℝ) (continuation : ℝ → Symbolic.AffineExpr n)
    (reduction : Symbolic.AffineExpr.symbolicReduce expression =
      Symbolic.AffineExpr.SymbolicAction.sampleG site fiber continuation) :
    fiber Set.univ = 1 ∧
      ∀ᵐ value ∂fiber,
        ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
          PrimitiveDomainSafeAt fuel ((continuation value).realize environment) := by
  have actionTyped := Symbolic.AffineExpr.symbolicReduce_wellTyped typed
  rw [reduction] at actionTyped
  have continuationTyped : ∀ value,
      Symbolic.AffineExpr.WellTyped [] (continuation value) ty := by
    simpa only [Symbolic.AffineExpr.SymbolicAction.wellTyped_sampleG_iff]
      using actionTyped
  have concreteReduction (environment : Env n) :
      reduce (expression.realize environment) =
        .sample site fiber (fun value => (continuation value).realize environment) := by
    rw [← Symbolic.AffineExpr.symbolicReduce_realize typed environment,
      reduction]
    rfl
  have sourceActionSafe :
      ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
        fiber Set.univ = 1 ∧ ∀ᵐ value ∂fiber,
          PrimitiveDomainSafeAt fuel ((continuation value).realize environment) := by
    filter_upwards [sourceSafe] with environment safe
    have notValue := not_value_of_reduce_sample (expression.realize environment)
      fiber (fun value => (continuation value).realize environment) (concreteReduction environment)
    simp only [PrimitiveDomainSafeAt, Bool.eq_false_of_not_eq_true notValue,
      Bool.false_eq_true, ↓reduceIte, concreteReduction] at safe
    exact safe
  have actualMass := SampleEnv.actualMeasure_univ_eq_one laws history historySafe
  let : NeZero (Symbolic.SampleEnv.actualMeasure laws history) := ⟨by
    intro zero
    rw [zero] at actualMass
    simp at actualMass⟩
  have fiberMass : fiber Set.univ = 1 := by
    have massAE := sourceActionSafe.mono fun _ safe => safe.1
    rcases massAE.exists with ⟨_, mass⟩
    exact mass
  refine ⟨fiberMass, ?_⟩
  have nestedMass :
      ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
        ∀ᵐ value ∂fiber,
          nStepMeasure stepKernel fuel ((continuation value).realize environment) Set.univ = 1 := by
    filter_upwards [sourceActionSafe] with environment safe
    filter_upwards [safe.2] with value continuationSafe
    exact (primitiveDomainSafeAt_iff_nStepMeasure_univ_eq_one stepKernel fuel
      ((continuation value).realize environment)
      ((continuationTyped value).realize_typed environment)).mp continuationSafe
  have jointMeasurable := sampleG_joint_nStepMass_measurable laws stepKernel fuel expression
    typed fiber continuation reduction
  have massSetMeasurable : MeasurableSet
      {pair : Env n × ℝ |
        nStepMeasure stepKernel fuel ((continuation pair.2).realize pair.1) Set.univ = 1} :=
    jointMeasurable (measurableSet_singleton 1)
  let : IsFiniteMeasure fiber := ⟨by rw [fiberMass]; simp⟩
  let : IsFiniteMeasure (Symbolic.SampleEnv.actualMeasure laws history) :=
    ⟨by rw [actualMass]; simp⟩
  have swappedMass : ∀ᵐ value ∂fiber,
      ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
        nStepMeasure stepKernel fuel ((continuation value).realize environment) Set.univ = 1 :=
    (Measure.ae_ae_comm massSetMeasurable).mp nestedMass
  filter_upwards [swappedMass] with value safeAtValue
  filter_upwards [safeAtValue] with environment mass
  exact (primitiveDomainSafeAt_iff_nStepMeasure_univ_eq_one stepKernel fuel
    ((continuation value).realize environment)
    ((continuationTyped value).realize_typed environment)).mpr mass

theorem source_sampleE_safe_extension
    (stepKernel : StepKernel) (fuel : Nat)
    (history : Symbolic.SampleEnv laws n)
    (historySafe : Symbolic.SampleEnv.DomainSafe laws history)
    (expression : Symbolic.AffineExpr n)
    (typed : Symbolic.AffineExpr.WellTyped [] expression ty)
    (sourceSafe : ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
      PrimitiveDomainSafeAt (fuel + 1) (expression.realize environment))
    (op : Determinize.Spec.Paper.Op) (affine : List (Symbolic.Affine n)) (general : List ℝ)
    (continuation : Symbolic.AffineExpr (n + 1))
    (reduction : Symbolic.AffineExpr.symbolicReduce expression =
      Symbolic.AffineExpr.SymbolicAction.sampleE op affine general continuation) :
    let affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n :=
      fun index => affine.getD index.1 (0, fun _ => 0)
    let generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ :=
      fun index => general.getD index.1 0
    let extended := Symbolic.SampleEnv.snoc history op affineArgs generalArgs
    Symbolic.SampleEnv.DomainSafe laws extended ∧
      Symbolic.AffineExpr.WellTyped [] continuation ty ∧
      ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws extended,
        PrimitiveDomainSafeAt fuel (continuation.realize environment) := by
  have actionTyped := Symbolic.AffineExpr.symbolicReduce_wellTyped typed
  rw [reduction] at actionTyped
  rcases (Symbolic.AffineExpr.SymbolicAction.wellTyped_sampleE_iff.mp actionTyped) with
    ⟨affineLength, generalLength, continuationTyped⟩
  let affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n :=
    fun index => affine.getD index.1 (0, fun _ => 0)
  let generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ :=
    fun index => general.getD index.1 0
  let extended := Symbolic.SampleEnv.snoc history op affineArgs generalArgs
  have concreteReduction (environment : Env n) :
      reduce (expression.realize environment) =
        .sample (.sample .E, op) (laws.kernel op
          (fun index => Symbolic.Affine.eval (affineArgs index) environment, generalArgs))
          (fun value => continuation.realize (Env.cons value environment)) := by
    rw [← Symbolic.AffineExpr.symbolicReduce_realize typed environment,
      reduction]
    simp only [Symbolic.AffineExpr.SymbolicAction.realize]
    congr 1
    classical
    unfold primitiveFiber
      Determinize.Spec.Paper.parseParams
    simp only
    rw [dif_pos (by simpa using affineLength), dif_pos generalLength]
    simp only
    rw [laws.kernel_eq_paperMeasure]
    congr 1
    apply Prod.ext
    · funext index
      simp [affineArgs, List.getD_eq_getElem?_getD, affineLength]
    · funext index
      simp [generalArgs, List.getD_eq_getElem?_getD, generalLength]
  have sourceActionSafe :
      ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
        (laws.kernel op
          (fun index => Symbolic.Affine.eval (affineArgs index) environment, generalArgs))
            Set.univ = 1 ∧
          ∀ᵐ value ∂laws.kernel op
            (fun index => Symbolic.Affine.eval (affineArgs index) environment, generalArgs),
            PrimitiveDomainSafeAt fuel
              (continuation.realize (Env.cons value environment)) := by
    filter_upwards [sourceSafe] with environment safe
    have notValue := not_value_of_reduce_sample (expression.realize environment)
      _ _ (concreteReduction environment)
    simp only [PrimitiveDomainSafeAt, Bool.eq_false_of_not_eq_true notValue,
      Bool.false_eq_true, ↓reduceIte, concreteReduction] at safe
    exact safe
  have domainAE : ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
      Determinize.Spec.Paper.domain op
        (fun index => Symbolic.Affine.eval (affineArgs index) environment, generalArgs) := by
    filter_upwards [sourceActionSafe] with environment safe
    exact stochastic_mass_one_imp_domain laws op _ safe.1
  have extendedSafe : Symbolic.SampleEnv.DomainSafe laws extended :=
    ⟨historySafe, domainAE⟩
  refine ⟨extendedSafe, continuationTyped, ?_⟩
  let transition := (SampleEnv.transitionPack laws op affineArgs generalArgs).kernel
  have nested : ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws history,
      ∀ᵐ nextEnvironment ∂transition environment,
        PrimitiveDomainSafeAt fuel (continuation.realize nextEnvironment) := by
    filter_upwards [sourceActionSafe] with environment safe
    rw [SampleEnv.transitionPack_apply]
    exact (MeasureTheory.ae_map_iff
      (μ := laws.kernel op
        (fun i => Symbolic.Affine.eval (affineArgs i) environment, generalArgs))
      (f := fun value => Env.cons value environment)
      (p := fun nextEnvironment => PrimitiveDomainSafeAt fuel
        (continuation.realize nextEnvironment))
      (measurable_envCons.comp
        (measurable_id.prodMk measurable_const)).aemeasurable
      (measurableSet_primitiveDomainSafeAt_realize stepKernel fuel continuation
        continuationTyped)).mpr safe.2
  have measureEq : Symbolic.SampleEnv.actualMeasure laws extended =
      transition ∘ₘ Symbolic.SampleEnv.actualMeasure laws history := by
    exact SampleEnv.actualMeasure_snoc_eq_comp laws history op affineArgs generalArgs
  rw [measureEq]
  exact Measure.ae_comp_of_ae_ae
    (measurableSet_primitiveDomainSafeAt_realize stepKernel fuel continuation continuationTyped)
    nested

theorem determinize_isValue (expression : Expr) :
    expression.determinize.isValue = expression.isValue := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on generalizing expression with
  | h size ih =>
      cases expression <;> simp only [Expr.determinize, Expr.isValue]
      case pair left right =>
        rw [ih (sizeOf left) (by rw [← sizeEq]; simp_wf; omega) left rfl,
          ih (sizeOf right) (by rw [← sizeEq]; simp_wf) right rfl]
      case inl value =>
        exact ih (sizeOf value) (by rw [← sizeEq]; simp_wf) value rfl
      case inr value =>
        exact ih (sizeOf value) (by rw [← sizeEq]; simp_wf) value rfl
      case cons head tail =>
        rw [ih (sizeOf head) (by rw [← sizeEq]; simp_wf; omega) head rfl,
          ih (sizeOf tail) (by rw [← sizeEq]; simp_wf) tail rfl]

noncomputable def targetRealize (environment : Env n) :
    Symbolic.AffineExpr.SymbolicAction n → Action
  | .next expression => .next (expression.realize environment).determinize
  | .sampleE op affine general continuation =>
      .sample (.mean, op) (primitiveFiber .mean op
        (affine.map (Symbolic.Affine.eval · environment)) general)
        (fun value => (continuation.realize (Env.cons value environment)).determinize)
  | .mean op affine general continuation =>
      .sample (.mean, op) (primitiveFiber .mean op
        (affine.map (Symbolic.Affine.eval · environment)) general)
        (fun value => ((continuation (value, 0)).realize environment).determinize)
  | .sampleG site fiber continuation =>
      .sample site fiber (fun value => ((continuation value).realize environment).determinize)
  | .stuck => .stuck

theorem determinize_shift (amount cutoff : Nat) (expression : Expr) :
    (expression.shift amount cutoff).determinize =
      expression.determinize.shift amount cutoff := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression cutoff with
  | h size ih =>
      have recurse (child : Expr) (childCutoff : Nat)
          (smaller : sizeOf child < sizeOf expression) :
          (child.shift amount childCutoff).determinize =
            child.determinize.shift amount childCutoff :=
        ih (sizeOf child) (sizeEq ▸ smaller) childCutoff child rfl
      cases expression
      all_goals
        simp (disch := simp_wf <;> omega) only [Expr.shift, Expr.mapVars, Expr.determinize, recurse]

set_option maxHeartbeats 800000 in
theorem determinize_substAt (depth : Nat) (replacement expression : Expr) :
    (Expr.substAt depth replacement expression).determinize =
      Expr.substAt depth replacement.determinize expression.determinize := by
  induction sizeEq : sizeOf expression using Nat.strong_induction_on
      generalizing expression depth with
  | h size ih =>
      have recurse (child : Expr) (childDepth : Nat)
          (smaller : sizeOf child < sizeOf expression) :
          (Expr.substAt childDepth replacement child).determinize =
            Expr.substAt childDepth replacement.determinize child.determinize :=
        ih (sizeOf child) (by rwa [← sizeEq]) childDepth child rfl
      cases expression with
      | bvar index =>
          simp only [Expr.substAt, Expr.mapVars, Expr.determinize]
          split <;> simp_all only [Expr.determinize, determinize_shift]
      | _ =>
          simp (disch := simp_wf <;> omega) only
            [Expr.substAt, Expr.mapVars, Expr.determinize, recurse]

theorem determinize_substHead (body replacement : Expr) :
    (body.substHead replacement).determinize =
      body.determinize.substHead replacement.determinize :=
  determinize_substAt 0 replacement body

theorem determinize_substTwo (body argument function : Expr) :
    (body.substTwo argument function).determinize =
      body.determinize.substTwo argument.determinize function.determinize := by
  simp only [Expr.substTwo, determinize_substAt]

theorem targetRealize_wrap
    (action : Symbolic.AffineExpr.SymbolicAction n)
    (environment : Env n)
    (context : Symbolic.AffineExpr n → Symbolic.AffineExpr n)
    (liftedContext : Symbolic.AffineExpr (n + 1) → Symbolic.AffineExpr (n + 1))
    (context_realize : ∀ expression,
      ((context expression).realize environment).determinize =
        ExprContext ((expression.realize environment).determinize))
    (lifted_realize : ∀ expression value,
      ((liftedContext expression).realize (Env.cons value environment)).determinize =
        ExprContext ((expression.realize (Env.cons value environment)).determinize)) :
    targetRealize environment (action.wrap context liftedContext) =
      (targetRealize environment action).wrap ExprContext := by
  cases action with
  | next expression => simp [Symbolic.AffineExpr.SymbolicAction.wrap,
      targetRealize, Action.wrap, context_realize]
  | stuck => rfl
  | sampleE op affine general continuation =>
      simp only [Symbolic.AffineExpr.SymbolicAction.wrap, targetRealize,
        Action.wrap, Action.sample.injEq, true_and]
      funext value
      exact lifted_realize continuation value
  | mean op affine general continuation =>
      simp only [Symbolic.AffineExpr.SymbolicAction.wrap, targetRealize,
        Action.wrap, Action.sample.injEq, true_and, Function.comp_apply]
      funext value
      exact context_realize (continuation (value, 0))
  | sampleG site fiber continuation =>
      simp only [Symbolic.AffineExpr.SymbolicAction.wrap, targetRealize,
        Action.wrap, Action.sample.injEq, true_and, Function.comp_apply]
      funext value
      exact context_realize (continuation value)

open Symbolic Symbolic.AffineExpr

set_option maxHeartbeats 1600000 in
set_option maxRecDepth 4000 in
theorem symbolicReduce_targetRealize
    {expression : AffineExpr n} (typed : WellTyped context expression ty)
    (environment : Env n) :
    targetRealize environment (symbolicReduce expression) =
      reduce ((expression.realize environment).determinize) := by
  induction typed generalizing environment with
  | bvar hvar => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | reject | «unit» => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | discrete =>
      rename_i context' affinity d
      cases affinity <;> simp [symbolicReduce, targetRealize, realize, Expr.determinize,
        DistributionAction.determinize, reduce, discreteFiber_eq, Affine.eval_fresh]
  | discreteMean =>
      simp [symbolicReduce, targetRealize, realize, Expr.determinize, DistributionAction.determinize, discreteFiber_eq]
  | bool => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | realE => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | realG => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | lam => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | fix => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | nil => simp [symbolicReduce, targetRealize, realize, Expr.determinize, Expr.determinize, reduce]
  | pair leftTyped rightTyped ihl ihr =>
      rename_i context' left leftTy right rightTy
      rw [symbolicReduce, realize, Expr.determinize, reduce,
        determinize_isValue, realize_isValue]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp [rightValue, targetRealize, realize, Expr.determinize,
            determinize_isValue, realize_isValue]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .pair
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue,
            if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .pair
            next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | inl valueTyped ih =>
      rename_i context' value leftTy rightTy
      rw [symbolicReduce, realize, Expr.determinize, reduce,
        determinize_isValue, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [valueIsValue, targetRealize, realize, Expr.determinize, Expr.determinize]
      · simp only [valueIsValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .inl next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | inr valueTyped ih =>
      rename_i context' value rightTy leftTy
      rw [symbolicReduce, realize, Expr.determinize, reduce,
        determinize_isValue, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [valueIsValue, targetRealize, realize, Expr.determinize, Expr.determinize]
      · simp only [valueIsValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .inr next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | cons headTyped tailTyped ihh iht =>
      rename_i context' head element tail
      rw [symbolicReduce, realize, Expr.determinize, reduce,
        determinize_isValue, realize_isValue]
      by_cases headValue : head.isValue = true
      · simp only [headValue, ↓reduceIte]
        by_cases tailValue : tail.isValue = true
        · simp [tailValue, targetRealize, realize, Expr.determinize,
            determinize_isValue, realize_isValue]
        · simp only [tailValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .cons ((head.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            iht environment, determinize_isValue, realize_isValue,
            if_neg tailValue]
      · simp only [headValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .cons next ((tail.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihh environment]
  | app functionTyped operandTyped ihf iho =>
      rename_i context' function argumentTy result operand
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_app_eq, determinize_isValue, realize_isValue]
      by_cases functionValue : function.isValue = true
      · simp only [functionValue, ↓reduceIte]
        by_cases operandValue : operand.isValue = true
        · rcases wellTyped_arr_value functionTyped functionValue with ⟨body, rfl⟩ | ⟨body, rfl⟩
          · simp [symbolicReduce, functionValue, operandValue, targetRealize,
              realize, Expr.determinize, determinize_isValue, realize_isValue,
              realize_substHead, determinize_substHead]
          · simp [symbolicReduce, functionValue, operandValue, targetRealize,
              realize, Expr.determinize, determinize_isValue, realize_isValue,
              realize_substTwo, determinize_substTwo]
        · rw [symbolicReduce_app_eq]
          simp only [functionValue, operandValue,
            Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .app ((function.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            iho environment, determinize_isValue, realize_isValue, if_neg operandValue]
      · rw [symbolicReduce_app_eq]
        simp only [functionValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .app next ((operand.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihf environment]
  | fst pairTyped ih =>
      rename_i context' pairValue leftTy rightTy
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_fst_eq, determinize_isValue, realize_isValue]
      by_cases pairIsValue : pairValue.isValue = true
      · simp only [pairIsValue, ↓reduceIte]
        obtain ⟨left, right, rfl⟩ := wellTyped_prod_value pairTyped pairIsValue
        simp [symbolicReduce, pairIsValue, targetRealize, realize, Expr.determinize, Expr.determinize]
      · rw [symbolicReduce_fst_eq]
        simp only [pairIsValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .fst next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | snd pairTyped ih =>
      rename_i context' pairValue leftTy rightTy
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_snd_eq, determinize_isValue, realize_isValue]
      by_cases pairIsValue : pairValue.isValue = true
      · simp only [pairIsValue, ↓reduceIte]
        obtain ⟨left, right, rfl⟩ := wellTyped_prod_value pairTyped pairIsValue
        simp [symbolicReduce, pairIsValue, targetRealize, realize, Expr.determinize, Expr.determinize]
      · rw [symbolicReduce_snd_eq]
        simp only [pairIsValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .snd next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | matchSum scrutineeTyped leftTyped rightTyped ihs ihl ihr =>
      rename_i context' scrutinee leftTy rightTy left result right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_matchSum_eq, determinize_isValue, realize_isValue]
      by_cases scrutineeValue : scrutinee.isValue = true
      · simp only [scrutineeValue, ↓reduceIte]
        rcases wellTyped_sum_value scrutineeTyped scrutineeValue with ⟨child, rfl⟩ | ⟨child, rfl⟩
        · simp [symbolicReduce, scrutineeValue, targetRealize, realize, Expr.determinize,
            realize_substHead, determinize_substHead]
        · simp [symbolicReduce, scrutineeValue, targetRealize, realize, Expr.determinize,
            realize_substHead, determinize_substHead]
      · rw [symbolicReduce_matchSum_eq]
        simp only [scrutineeValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .matchSum next
            ((left.realize environment).determinize) ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihs environment]
  | matchList scrutineeTyped nilTyped consTyped ihs ihn ihc =>
      rename_i context' scrutinee element nilCase result consCase
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_matchList_eq, determinize_isValue, realize_isValue]
      by_cases scrutineeValue : scrutinee.isValue = true
      · simp only [scrutineeValue, ↓reduceIte]
        rcases wellTyped_list_value scrutineeTyped scrutineeValue with equality | ⟨head, tail, equality⟩
        · subst scrutinee
          simp [symbolicReduce, scrutineeValue, targetRealize, realize, Expr.determinize, Expr.determinize]
        · subst scrutinee
          simp [symbolicReduce, scrutineeValue, targetRealize, realize, Expr.determinize,
            realize_substTwo, determinize_substTwo]
      · rw [symbolicReduce_matchList_eq]
        simp only [scrutineeValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .matchList next
            ((nilCase.realize environment).determinize) ((consCase.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihs environment]
  | ite conditionTyped thenTyped elseTyped ihc iht ihe =>
      rename_i context' condition thenBranch result elseBranch
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_ite_eq, determinize_isValue, realize_isValue]
      by_cases conditionValue : condition.isValue = true
      · simp only [conditionValue, ↓reduceIte]
        obtain ⟨answer, rfl⟩ := wellTyped_bool_value conditionTyped conditionValue
        cases answer <;> simp [symbolicReduce, conditionValue, targetRealize, realize, Expr.determinize, Expr.determinize]
      · rw [symbolicReduce_ite_eq]
        simp only [conditionValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .ite next
            ((thenBranch.realize environment).determinize) ((elseBranch.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihc environment]
  | letE valueTyped bodyTyped ihv ihb =>
      rename_i context' value valueTy body result
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_let_eq, determinize_isValue, realize_isValue]
      by_cases valueIsValue : value.isValue = true
      · simp [symbolicReduce, valueIsValue, targetRealize, realize_substHead, determinize_substHead]
      · rw [symbolicReduce_let_eq]
        simp only [valueIsValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .letE next ((body.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihv environment]
  | sub valueTyped h ih => exact ih environment
  | negE valueTyped ih =>
      rename_i context' value
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_neg_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [targetRealize, realize, Expr.determinize, Affine.eval_neg]
      · simp only [valueIsValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .neg next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | negG valueTyped ih =>
      rename_i context' value
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_neg_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨coordinate, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [targetRealize, realize, Expr.determinize, Affine.eval_neg]
      · simp only [valueIsValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .neg next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | addE leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_add_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          simp [affineValue?, targetRealize, realize, Expr.determinize, Expr.isValue,
            realValue?, Affine.eval_add]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .add ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .add next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | addG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_add_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          simp [affineValue?, targetRealize, realize, Expr.determinize, Expr.isValue,
            realValue?, Affine.eval_add]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .add ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .add next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | mulGE leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_mul_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          have leftZero : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain ⟨result, product⟩ :=
            Affine.mul?_eq_some_of_left (left := (x0, xc)) (right := y) leftZero
          simp [affineValue?, product, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, Affine.eval_mul_of_eq_some product]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .mul ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .mul next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | mulGG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_mul_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          rcases y with ⟨y0, yc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, Affine.mul?, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero]
          ring
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .mul ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .mul next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | divEG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_div_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases y with ⟨y0, yc⟩
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, Affine.div?, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero,
            div_eq_mul_inv]
          have sumRule : (∑ i, y0⁻¹ * x.2 i * environment i) =
              y0⁻¹ * ∑ i, x.2 i * environment i := by
            rw [Finset.mul_sum]
            apply Finset.sum_congr rfl
            intro i _
            ring
          rw [sumRule]
          ring
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .div ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .div next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | divGG leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_div_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          rcases y with ⟨y0, yc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, Affine.div?, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, Symbolic.Affine.eval, Finset.sum_const_zero,
            div_eq_mul_inv]
          ring
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .div ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .div next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | lt leftTyped rightTyped ihl ihr =>
      rename_i context' left right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_lt_eq, determinize_isValue, realize_isValue,
        symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases x with ⟨x0, xc⟩
          rcases y with ⟨y0, yc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [constantValue?, targetRealize, realize, Expr.determinize, Expr.isValue,
            realValue?, Symbolic.Affine.eval, Finset.sum_const_zero]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .lt ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .lt next ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | uniform leftTyped rightTyped ihl ihr =>
      rename_i context' left affinity right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_uniform_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases affinity with
          | E =>
              simp [affineValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?, uniformFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .uniform (DistributionAction.determinize (.sample affinity))
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .uniform (DistributionAction.determinize (.sample affinity)) next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | uniformMean leftTyped rightTyped ihl ihr =>
      rename_i context' left affinity right
      rw [realize, Expr.determinize]
      simp only [DistributionAction.determinize]
      rw [MeasurableActionFamily.reduce_uniform_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          simp [affineValue?, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, uniformFiber_eq]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .uniform .mean
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .uniform .mean next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
          ihl environment]
  | gaussian leftTyped rightTyped ihl ihr =>
      rename_i context' left affinity right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_gaussian_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases affinity with
          | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [affineValue?, constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?, gaussianFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .gaussian (DistributionAction.determinize (.sample affinity))
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .gaussian (DistributionAction.determinize (.sample affinity)) next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | gaussianMean leftTyped rightTyped ihl ihr =>
      rename_i context' left affinity right
      rw [realize, Expr.determinize]
      simp only [DistributionAction.determinize]
      rw [MeasurableActionFamily.reduce_gaussian_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases y with ⟨y0, yc⟩
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, constantValue?, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, gaussianFiber_eq]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .gaussian .mean
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .gaussian .mean next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
          ihl environment]
  | poisson valueTyped ih =>
      rename_i context' value affinity
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_poisson_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        cases affinity with
        | E =>
            simp [affineValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
              realValue?, poissonFiber_eq, Affine.eval_fresh]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
              realValue?]
      · simp only [valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .poisson (DistributionAction.determinize (.sample affinity)) next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | poissonMean valueTyped ih =>
      rename_i context' value affinity
      rw [realize, Expr.determinize]
      simp only [DistributionAction.determinize]
      rw [MeasurableActionFamily.reduce_poisson_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [affineValue?, targetRealize, realize, Expr.determinize,
          realValue?, poissonFiber_eq]
      · simp only [valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .poisson .mean next)
          (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize]), ih environment]
  | bernoulli valueTyped ih =>
      rename_i context' value affinity
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_bernoulli_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        cases affinity with
        | E =>
            simp [affineValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
              realValue?, bernoulliFiber_eq, Affine.eval_fresh]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
              realValue?]
      · simp only [valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .bernoulli (DistributionAction.determinize (.sample affinity)) next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | bernoulliMean valueTyped ih =>
      rename_i context' value affinity
      rw [realize, Expr.determinize]
      simp only [DistributionAction.determinize]
      rw [MeasurableActionFamily.reduce_bernoulli_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        simp [affineValue?, targetRealize, realize, Expr.determinize,
          realValue?, bernoulliFiber_eq]
      · simp only [valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .bernoulli .mean next)
          (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize]), ih environment]
  | exponential valueTyped ih =>
      rename_i context' value affinity
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_exponential_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        cases affinity with
        | E =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
              realValue?, exponentialFiber_eq, Affine.eval_fresh]
        | G =>
            rcases x with ⟨x0, xc⟩
            obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
            simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
              realValue?]
      · simp only [valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .exponential (DistributionAction.determinize (.sample affinity)) next)
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize]), ih environment]
  | exponentialMean valueTyped ih =>
      rename_i context' value affinity
      rw [realize, Expr.determinize]
      simp only [DistributionAction.determinize]
      rw [MeasurableActionFamily.reduce_exponential_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases valueIsValue : value.isValue = true
      · simp only [valueIsValue, ↓reduceIte]
        obtain ⟨x, rfl⟩ := wellTyped_real_value valueTyped valueIsValue
        rcases x with ⟨x0, xc⟩
        obtain rfl : xc = 0 := wellTyped_realG_coefficients valueTyped
        simp [constantValue?, targetRealize, realize, Expr.determinize,
          realValue?, exponentialFiber_eq]
      · simp only [valueIsValue,
          Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .exponential .mean next)
          (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize]), ih environment]
  | beta leftTyped rightTyped ihl ihr =>
      rename_i context' left right affinity
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_beta_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases affinity with
          | E =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?, betaFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .beta (DistributionAction.determinize (.sample affinity))
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .beta (DistributionAction.determinize (.sample affinity)) next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]
  | betaMean leftTyped rightTyped ihl ihr =>
      rename_i context' left right affinity
      rw [realize, Expr.determinize]
      simp only [DistributionAction.determinize]
      rw [MeasurableActionFamily.reduce_beta_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases y with ⟨y0, yc⟩
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          rcases x with ⟨x0, xc⟩
          obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
          simp [constantValue?, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, betaFiber_eq]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .beta .mean
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .beta .mean next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
          ihl environment]
  | gamma leftTyped rightTyped ihl ihr =>
      rename_i context' left affinity right
      rw [realize, Expr.determinize, MeasurableActionFamily.reduce_gamma_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          cases affinity with
          | E =>
              rcases y with ⟨y0, yc⟩
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [affineValue?, constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?, gammaFiber_eq, Affine.eval_fresh]
          | G =>
              rcases x with ⟨x0, xc⟩
              rcases y with ⟨y0, yc⟩
              obtain rfl : xc = 0 := wellTyped_realG_coefficients leftTyped
              obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
              simp [constantValue?, targetRealize, realize, Expr.determinize, DistributionAction.determinize,
                Expr.isValue, realValue?]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .gamma (DistributionAction.determinize (.sample affinity))
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .gamma (DistributionAction.determinize (.sample affinity)) next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, realize_weakenSamples]),
          ihl environment]

  | gammaMean leftTyped rightTyped ihl ihr =>
      rename_i context' left affinity right
      rw [realize, Expr.determinize]
      simp only [DistributionAction.determinize]
      rw [MeasurableActionFamily.reduce_gamma_eq, determinize_isValue,
        realize_isValue, symbolicReduce.eq_def]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          obtain ⟨x, rfl⟩ := wellTyped_real_value leftTyped leftValue
          obtain ⟨y, rfl⟩ := wellTyped_real_value rightTyped rightValue
          rcases y with ⟨y0, yc⟩
          obtain rfl : yc = 0 := wellTyped_realG_coefficients rightTyped
          simp [affineValue?, constantValue?, targetRealize, realize, Expr.determinize,
            Expr.isValue, realValue?, gammaFiber_eq]
        · simp only [rightValue, Bool.false_eq_true, ↓reduceIte]
          rw [targetRealize_wrap
            (ExprContext := fun next => .gamma .mean
              ((left.realize environment).determinize) next)
            (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
            (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
            ihr environment, determinize_isValue, realize_isValue, if_neg rightValue]
      · simp only [leftValue, Bool.false_eq_true, ↓reduceIte]
        rw [targetRealize_wrap
          (ExprContext := fun next => .gamma .mean next
            ((right.realize environment).determinize))
          (context_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize])
          (lifted_realize := by intros; simp only [realize, Expr.determinize, DistributionAction.determinize, realize_weakenSamples]),
          ihl environment]

/-- A valid mean call takes one deterministic step, using no additional sample coordinate. -/
theorem source_mean_safe_step
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce expression = .mean op affine general continuation)
    (environment : Env n)
    (safe : PrimitiveDomainSafeAt (fuel + 1) (expression.realize environment)) :
    domain op (meanParams op affine general environment) ∧
      PrimitiveDomainSafeAt fuel ((continuation (meanAffine op affine general)).realize environment) := by
  have actionTyped := symbolicReduce_wellTyped typed
  rw [actionEq] at actionTyped
  obtain ⟨ha, hg, nextTyped, natural⟩ := SymbolicAction.wellTyped_mean_iff.mp actionTyped
  have reduction : reduce (expression.realize environment) =
      .sample (.mean, op)
        (primitiveFiber .mean op (affine.map (Symbolic.Affine.eval · environment)) general)
        (fun value => (continuation (value, 0)).realize environment) := by
    rw [← symbolicReduce_realize typed environment, actionEq]
    rfl
  have nv := not_value_of_reduce_sample _ _ _ reduction
  rw [PrimitiveDomainSafeAt, if_neg nv, reduction] at safe
  dsimp only at safe
  have valid := (primitiveFiber_mean_mass op affine general ha hg environment).mp safe.1
  refine ⟨valid, ?_⟩
  rw [primitiveFiber_mean_formula op affine general ha hg environment, if_pos valid,
    ae_dirac_eq] at safe
  rw [natural]
  exact safe.2

set_option maxHeartbeats 1600000 in
theorem safeConfigAt_target
    (stepKernel : StepKernel) (fuel : Nat)
    (history : Symbolic.SampleEnv laws n) (expression : AffineExpr n)
    (safe : SafeConfigAt laws fuel history expression) :
    PrimitiveDomainSafeAt fuel
      ((expression.realize (Symbolic.SampleEnv.meanEnvironment laws history)).determinize) := by
  induction fuel generalizing n history expression with
  | zero => trivial
  | succ fuel ih =>
      rcases safe with ⟨historySafe, typed, sourceSafe⟩
      let mean := Symbolic.SampleEnv.meanEnvironment laws history
      by_cases value : expression.isValue = true
      · simp only [PrimitiveDomainSafeAt, determinize_isValue,
          Symbolic.AffineExpr.realize_isValue, value, ↓reduceIte]
      · have targetNotValue :
            (expression.realize mean).determinize.isValue ≠ true := by
          simpa only [determinize_isValue, Symbolic.AffineExpr.realize_isValue] using value
        rw [PrimitiveDomainSafeAt, if_neg targetNotValue]
        have targetStep := symbolicReduce_targetRealize typed mean
        rw [← targetStep]
        have actionTyped := Symbolic.AffineExpr.symbolicReduce_wellTyped typed
        generalize actionEq : symbolicReduce expression = action at actionTyped ⊢
        cases action with
        | next next =>
            have nextTyped : WellTyped [] next (.float .E) := by
              simpa only [Symbolic.AffineExpr.SymbolicAction.wellTyped_next_iff]
                using actionTyped
            have nextSafe : ∀ᵐ environment
                ∂Symbolic.SampleEnv.actualMeasure laws history,
                PrimitiveDomainSafeAt fuel (next.realize environment) := by
              filter_upwards [sourceSafe] with environment environmentSafe
              have concreteStep : reduce (expression.realize environment) =
                  .next (next.realize environment) := by
                rw [← Symbolic.AffineExpr.symbolicReduce_realize typed
                  environment, actionEq]
                rfl
              have sourceNotValue : (expression.realize environment).isValue ≠ true := by
                simpa only [Symbolic.AffineExpr.realize_isValue] using value
              simpa only [PrimitiveDomainSafeAt,
                Bool.eq_false_of_not_eq_true sourceNotValue, Bool.false_eq_true,
                ↓reduceIte, concreteStep] using environmentSafe
            exact ih history next ⟨historySafe, nextTyped, nextSafe⟩
        | mean op affine general continuation =>
            obtain ⟨ha, hg, nextTyped, natural⟩ := SymbolicAction.wellTyped_mean_iff.mp actionTyped
            have stepSafe := sourceSafe.mono (fun env safe =>
              source_mean_safe_step expression typed actionEq env safe)
            have valid := SampleEnv.domain_at_meanEnvironment laws history historySafe op
              (fun i => affine.getD i.1 0) (fun i => general.getD i.1 0)
              (stepSafe.mono fun _ h => h.1)
            change domain op (meanParams op affine general mean) at valid
            have nextSafe := ih history (continuation (meanAffine op affine general))
              ⟨historySafe, nextTyped, stepSafe.mono fun _ h => h.2⟩
            simp only [targetRealize]
            rw [primitiveFiber_mean_formula op affine general ha hg mean, if_pos valid]
            refine ⟨by simp, ?_⟩
            rw [ae_dirac_eq, Filter.eventually_pure, ← natural]
            exact nextSafe
        | sampleE op affine general continuation =>
            let affineArgs : Fin (Determinize.Spec.Paper.affineArity op) → Symbolic.Affine n :=
              fun index => affine.getD index.1 (0, fun _ => 0)
            let generalArgs : Fin (Determinize.Spec.Paper.generalArity op) → ℝ :=
              fun index => general.getD index.1 0
            let extended := Symbolic.SampleEnv.snoc history op affineArgs generalArgs
            have extendedSafe := source_sampleE_safe_extension stepKernel fuel history
              historySafe expression typed sourceSafe op affine general continuation actionEq
            change Symbolic.SampleEnv.DomainSafe laws extended ∧
                WellTyped [] continuation (.float .E) ∧
                ∀ᵐ environment ∂Symbolic.SampleEnv.actualMeasure laws extended,
                  PrimitiveDomainSafeAt fuel (continuation.realize environment) at extendedSafe
            rcases extendedSafe with ⟨extendedDomainSafe, continuationTyped,
              continuationSourceSafe⟩
            have actionShape :=
              Symbolic.AffineExpr.SymbolicAction.wellTyped_sampleE_iff.mp actionTyped
            rcases actionShape with ⟨affineLength, generalLength, _⟩
            have meanDomain : Determinize.Spec.Paper.domain op
                (fun index => Symbolic.Affine.eval (affineArgs index) mean, generalArgs) :=
              SampleEnv.domain_at_meanEnvironment laws history historySafe op affineArgs
                generalArgs extendedDomainSafe.2
            have continuationTargetSafe := ih extended continuation
              ⟨extendedDomainSafe, continuationTyped, continuationSourceSafe⟩
            classical
            simp only [targetRealize]
            let params : Determinize.Spec.Paper.Params op :=
              (fun index => (affine.map (Symbolic.Affine.eval · mean))[index.1]'(by
                  simp [affineLength]),
                fun index => general[index.1]'(by simp [generalLength]))
            have paramsEq : params =
                (fun index => Symbolic.Affine.eval (affineArgs index) mean, generalArgs) := by
              apply Prod.ext <;> funext index
              · simp [params, affineArgs, List.getD_eq_getElem?_getD, affineLength]
              · simp [params, generalArgs, List.getD_eq_getElem?_getD, generalLength]
            have paramsDomain : Determinize.Spec.Paper.domain op params := by
              simpa only [paramsEq] using meanDomain
            have fiberEq : primitiveFiber .mean op
                (affine.map (Symbolic.Affine.eval · mean)) general =
                Measure.dirac (Determinize.Spec.Paper.meanValue op params) := by
              have affineMappedLength :
                  (affine.map (Symbolic.Affine.eval · mean)).length =
                    Determinize.Spec.Paper.affineArity op := by simpa using affineLength
              unfold primitiveFiber
                Determinize.Spec.Paper.parseParams
              simp only
              rw [dif_pos affineMappedLength, dif_pos generalLength]
              simp only
              change (if Determinize.Spec.Paper.domain op params then
                Measure.dirac (Determinize.Spec.Paper.meanValue op params) else 0) = _
              rw [if_pos paramsDomain]
            rw [fiberEq]
            constructor
            · simp
            · rw [ae_dirac_eq]
              change PrimitiveDomainSafeAt fuel
                ((continuation.realize
                  (Env.cons (Determinize.Spec.Paper.meanValue op params) mean)).determinize)
              have meanExtended : Symbolic.SampleEnv.meanEnvironment laws extended =
                  Env.cons (Determinize.Spec.Paper.meanValue op
                    (fun index => Symbolic.Affine.eval (affineArgs index) mean,
                      generalArgs)) mean := by
                rfl
              rw [meanExtended] at continuationTargetSafe
              simpa only [paramsEq] using continuationTargetSafe
        | sampleG site fiber continuation =>
            have continuationTyped : ∀ value,
                WellTyped [] (continuation value) (.float .E) := by
              simpa only [Symbolic.AffineExpr.SymbolicAction.wellTyped_sampleG_iff]
                using actionTyped
            rcases source_sampleG_safe_swap stepKernel fuel history historySafe expression
              typed sourceSafe fiber continuation actionEq with ⟨fiberMass, continuationSafe⟩
            unfold targetRealize
            refine ⟨fiberMass, ?_⟩
            filter_upwards [continuationSafe] with sampledValue sampledSafe
            exact ih history (continuation sampledValue)
              ⟨historySafe, continuationTyped sampledValue, sampledSafe⟩
        | stuck => exact (Symbolic.AffineExpr.SymbolicAction.not_wellTyped_stuck actionTyped).elim

theorem determinize_primitiveDomainSafe_of_typed_source
    (laws : Determinize.Proof.Paper.PrimitiveLaws) (stepKernel : StepKernel)
    (program : Expr) (typed : Determinize.Spec.Paper.Typed [] program (.float .E))
    (sourceSafe : PrimitiveDomainSafe program) :
    PrimitiveDomainSafe program.determinize := by
  let symbolic := AffineExpr.ofExpr program
  have symbolicTyped : WellTyped [] symbolic (.float .E) :=
    AffineExpr.wellTyped_ofExpr_of_typed typed
  intro fuel
  have symbolicSafe : SafeConfigAt laws fuel (.nil : Symbolic.SampleEnv laws 0) symbolic := by
    refine ⟨trivial, symbolicTyped, ?_⟩
    rw [Symbolic.SampleEnv.actualMeasure, ae_dirac_eq]
    simpa only [Filter.eventually_pure, symbolic, AffineExpr.realize_ofExpr]
      using sourceSafe fuel
  have targetSafe := safeConfigAt_target stepKernel fuel
    (.nil : Symbolic.SampleEnv laws 0) symbolic symbolicSafe
  simpa only [Symbolic.SampleEnv.meanEnvironment, symbolic, AffineExpr.realize_ofExpr]
    using targetSafe

theorem bind_bind_const_swap {alpha beta gamma : Type*}
    [MeasurableSpace alpha] [MeasurableSpace beta] [MeasurableSpace gamma]
    (left : Measure alpha) (right : Measure beta) [SFinite left] [SFinite right]
    (kernel : Kernel (alpha × beta) gamma) [IsSFiniteKernel kernel] :
    left.bind (fun a => right.bind fun b => kernel (a, b)) =
      right.bind (fun b => left.bind fun a => kernel (a, b)) := by
  have leftFamilyMeasurable : Measurable
      (fun a => right.bind fun b => kernel (a, b)) := by
    apply Measure.measurable_of_measurable_coe
    intro set measurableSet
    have jointMeasurable := kernel.measurable_coe measurableSet
    change Measurable (Function.uncurry (fun a b => kernel (a, b) set)) at jointMeasurable
    convert jointMeasurable.lintegral_prod_right (ν := right) using 1
    funext a
    exact Measure.bind_apply measurableSet
      ((kernel.measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable)
  have rightFamilyMeasurable : Measurable
      (fun b => left.bind fun a => kernel (a, b)) := by
    apply Measure.measurable_of_measurable_coe
    intro set measurableSet
    have jointMeasurable := kernel.measurable_coe measurableSet
    change Measurable (Function.uncurry (fun a b => kernel (a, b) set)) at jointMeasurable
    convert jointMeasurable.lintegral_prod_left (μ := left) using 1
    funext b
    exact Measure.bind_apply measurableSet
      ((kernel.measurable.comp (measurable_id.prodMk measurable_const)).aemeasurable)
  ext set measurableSet
  rw [Measure.bind_apply measurableSet leftFamilyMeasurable.aemeasurable,
    Measure.bind_apply measurableSet rightFamilyMeasurable.aemeasurable]
  have leftApply (a : alpha) :
      (right.bind fun b => kernel (a, b)) set =
        ∫⁻ b, kernel (a, b) set ∂right :=
    Measure.bind_apply measurableSet
      ((kernel.measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable)
  have rightApply (b : beta) :
      (left.bind fun a => kernel (a, b)) set =
        ∫⁻ a, kernel (a, b) set ∂left :=
    Measure.bind_apply measurableSet
      ((kernel.measurable.comp (measurable_id.prodMk measurable_const)).aemeasurable)
  simp_rw [leftApply, rightApply]
  exact MeasureTheory.lintegral_lintegral_swap
    (kernel.measurable_coe measurableSet).aemeasurable

end TargetSafety

end Determinize.Proof.Paper.SymbolicSoundness
