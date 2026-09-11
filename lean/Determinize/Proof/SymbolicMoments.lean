import Determinize.Proof.PrimitiveMoments
import Determinize.Proof.SymbolicSoundness

namespace Determinize.Proof.Paper.SymbolicSoundness.SampleEnv

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper
open scoped ProbabilityTheory

noncomputable section

theorem transitionMoment_le (laws : PrimitiveLaws)
    (op : Op) (affineArgs : Fin (affineArity op) → Symbolic.Affine n)
    (generalArgs : Fin (generalArity op) → ℝ)
    (expression : Symbolic.Affine (n + 1)) (environment : Env n)
    (valid : domain op (fun i => (affineArgs i).eval environment, generalArgs)) :
    (∫ next, ‖expression.eval next‖ ∂(transitionPack laws op affineArgs generalArgs).kernel environment) ≤
      |(Affine.tail expression).eval environment| + |expression.2 0| *
        (∫ value : ℝ, |value| ∂laws.kernel op (fun i => (affineArgs i).eval environment, generalArgs)) := by
  rw [transitionPack_apply, integral_map
    (show AEMeasurable (fun value : ℝ => Env.cons value environment)
      (laws.kernel op (fun i => (affineArgs i).eval environment, generalArgs)) from
      (measurable_envCons.comp (measurable_id.prodMk measurable_const)).aemeasurable)
    (Symbolic.AffineExpr.affine_eval_measurable expression).norm.aestronglyMeasurable]
  simp only [Affine.eval_cons, Real.norm_eq_abs]
  let params := (fun i => (affineArgs i).eval environment, generalArgs)
  have mass := laws.mass_one op params valid
  let : IsProbabilityMeasure (laws.kernel op params) := ⟨mass⟩
  have integrable : Integrable (fun x : ℝ => x) (laws.kernel op params) :=
    laws.integrable_id op params valid
  have leftIntegrable : Integrable
      (fun x => |(Affine.tail expression).eval environment + expression.2 0 * x|)
      (laws.kernel op params) :=
    ((integrable_const _).add (integrable.const_mul _)).abs
  calc
    _ ≤ ∫ x : ℝ, |(Affine.tail expression).eval environment| + |expression.2 0| * |x|
        ∂laws.kernel op params := by
      apply integral_mono leftIntegrable
        ((integrable_const _).add (integrable.abs.const_mul _))
      intro x
      change |(Affine.tail expression).eval environment + expression.2 0 * x| ≤
        |(Affine.tail expression).eval environment| + |expression.2 0| * |x|
      simpa only [abs_mul] using abs_add_le ((Affine.tail expression).eval environment) (expression.2 0 * x)
    _ = _ := by
      rw [integral_add (integrable_const _) (integrable.abs.const_mul _),
        integral_const, integral_const_mul]
      simp [params]

theorem integrable_affine (laws : PrimitiveLaws) (bounds : PrimitiveMomentBounds laws)
    (history : Symbolic.SampleEnv laws n) (safe : history.DomainSafe laws)
    (expression : Symbolic.Affine n) :
    Integrable expression.eval (history.actualMeasure laws) := by
  induction history with
  | nil => exact integrable_dirac (by simp)
  | @snoc n history op affineArgs generalArgs ih =>
      let transition := (transitionPack laws op affineArgs generalArgs).kernel
      let prior := history.actualMeasure laws
      have priorMass : prior Set.univ = 1 := actualMeasure_univ_eq_one laws history safe.1
      let : IsProbabilityMeasure prior := ⟨priorMass⟩
      let := (transitionPack laws op affineArgs generalArgs).sfinite
      rw [actualMeasure_snoc_eq_comp]
      apply (Measure.integrable_comp_iff
        (Symbolic.AffineExpr.affine_eval_measurable expression).aestronglyMeasurable).2
      constructor
      · filter_upwards [safe.2] with environment valid
        exact integrable_eval_transition laws op affineArgs generalArgs expression environment valid
      · obtain ⟨bound, boundNonneg, boundRule⟩ := bounds op generalArgs
        let upper : Env n → ℝ := fun environment =>
          |(Affine.tail expression).eval environment| + |expression.2 0| *
            (bound * (1 + ∑ i, |(affineArgs i).eval environment|))
        have sumIntegrable : Integrable (fun environment => ∑ i, |(affineArgs i).eval environment|) prior :=
          integrable_finsetSum _ fun i _ => (ih safe.1 (affineArgs i)).abs
        have upperIntegrable : Integrable upper prior :=
          (ih safe.1 (Affine.tail expression)).abs.add
            (((integrable_const (1 : ℝ)).add sumIntegrable).const_mul bound |>.const_mul _)
        have normMeasurable : AEStronglyMeasurable
            (fun environment => ∫ next, ‖expression.eval next‖ ∂transition environment) prior :=
          ((Symbolic.AffineExpr.affine_eval_measurable expression).norm.stronglyMeasurable.integral_kernel
            (κ := transition)).aestronglyMeasurable
        apply upperIntegrable.mono' normMeasurable
        filter_upwards [safe.2] with environment valid
        rw [Real.norm_eq_abs, abs_of_nonneg (integral_nonneg fun _ => norm_nonneg _)]
        exact (transitionMoment_le laws op affineArgs generalArgs expression environment valid).trans
          (add_le_add le_rfl (mul_le_mul_of_nonneg_left (boundRule _ valid)
            (abs_nonneg _)))

theorem integral_affine (laws : PrimitiveLaws) (bounds : PrimitiveMomentBounds laws)
    (history : Symbolic.SampleEnv laws n) (safe : history.DomainSafe laws)
    (expression : Symbolic.Affine n) :
    (∫ environment, expression.eval environment ∂history.actualMeasure laws) =
      expression.eval (history.meanEnvironment laws) := by
  induction history with
  | nil => simp [Symbolic.SampleEnv.actualMeasure, Symbolic.SampleEnv.meanEnvironment]
  | @snoc n history op affineArgs generalArgs ih =>
      let transition := (transitionPack laws op affineArgs generalArgs).kernel
      have mass := actualMeasure_univ_eq_one laws history safe.1
      let : IsProbabilityMeasure (history.actualMeasure laws) := ⟨mass⟩
      let := (transitionPack laws op affineArgs generalArgs).sfinite
      have integrable := integrable_affine laws bounds
        (.snoc history op affineArgs generalArgs) safe expression
      rw [actualMeasure_snoc_eq_comp] at integrable ⊢
      rw [Measure.comp_eq_comp_const_apply] at integrable ⊢
      rw [Kernel.integral_comp integrable]
      simp only [Kernel.const_apply]
      calc
        _ = ∫ env, (Affine.substituteHeadMean expression op affineArgs generalArgs).eval env
            ∂history.actualMeasure laws := by
          apply integral_congr_ae
          filter_upwards [safe.2] with env domain
          exact integral_eval_transition laws op affineArgs generalArgs expression env domain
        _ = (Affine.substituteHeadMean expression op affineArgs generalArgs).eval
            (history.meanEnvironment laws) := ih safe.1 _
        _ = _ := by
          simp only [Symbolic.SampleEnv.meanEnvironment, Affine.eval_cons,
            Affine.eval_substituteHeadMean]

end

end Determinize.Proof.Paper.SymbolicSoundness.SampleEnv
