import Determinize.Spec.Main
import Determinize.Proof.PrimitiveKernels
import Determinize.Proof.SymbolicSoundness

/-!
# Ordinary semantics and source typing

This file proves measurability of the direct output evaluator using expression
kernels and transports source typing to the symbolic language.
-/

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory
open scoped ProbabilityTheory
open Determinize.Spec.Paper

private noncomputable abbrev paperStepKernel := MeasurableActionFamily.stepKernel primitiveLaws

private noncomputable abbrev cumulativeKernel (fuel : Nat) :=
  (MeasurableActionFamily.exactOutputKernel paperStepKernel 0) ∘ₖ
    (MeasurableActionFamily.nStepKernelPack paperStepKernel fuel).kernel

/-- The direct evaluator is represented by a measurable kernel in its expression input. -/
theorem cumulativeKernel_apply (fuel : Nat) (expression : Expr) :
    cumulativeKernel fuel expression =
      Determinize.Spec.Paper.cumulativeOutputMeasure fuel expression := by
  symm
  induction fuel generalizing expression with
  | zero =>
      simp only [cumulativeKernel, MeasurableActionFamily.nStepKernelPack_zero_kernel,
        Kernel.comp_id, MeasurableActionFamily.exactOutputKernel_apply]
      cases expression <;> try rfl
  | succ fuel ih =>
      rw [cumulativeKernel, MeasurableActionFamily.cumulativeKernel_succ,
        Kernel.comp_apply, paperStepKernel.kernel_eq_stepMeasure]
      change Determinize.Spec.Paper.cumulativeOutputMeasure (fuel + 1) expression =
        (reduce expression).measure.bind (cumulativeKernel fuel)
      cases reduction : reduce expression with
      | next next =>
          simp only [Determinize.Spec.Paper.cumulativeOutputMeasure, reduction,
            Action.measure, Measure.dirac_bind (cumulativeKernel fuel).measurable]
          exact ih next
      | sample modeTag fiber continuation =>
          simp only [Determinize.Spec.Paper.cumulativeOutputMeasure, reduction, Action.measure]
          simp_rw [ih]
          have measurable := paperStepKernel.sample_continuation_measurable
            expression fiber continuation reduction
          ext set hs
          change (fiber.bind ((cumulativeKernel fuel) ∘ continuation)) set = _
          rw [Measure.bind_apply hs ((cumulativeKernel fuel).measurable.comp measurable).aemeasurable,
            Measure.bind_apply hs (cumulativeKernel fuel).aemeasurable,
            lintegral_map ((cumulativeKernel fuel).measurable_coe hs) measurable]
          rfl
      | stuck => simp [Determinize.Spec.Paper.cumulativeOutputMeasure, reduction, Action.measure]

theorem cumulativeOutputMeasure_eq (fuel : Nat) (expression : Expr) :
    Determinize.Proof.Paper.cumulativeOutputMeasure paperStepKernel fuel expression =
      Determinize.Spec.Paper.cumulativeOutputMeasure fuel expression := by
  rw [MeasurableActionFamily.cumulativeOutputMeasure_eq_kernel]
  exact cumulativeKernel_apply fuel expression

theorem bigStepMeasure_eq (expression : Expr) :
    Determinize.Proof.Paper.bigStepMeasure paperStepKernel expression =
      Determinize.Spec.Paper.bigStepMeasure expression := by
  unfold Determinize.Proof.Paper.bigStepMeasure Determinize.Spec.Paper.bigStepMeasure
  congr 1
  funext fuel
  exact cumulativeOutputMeasure_eq fuel expression

theorem measurable_direct_cumulative (fuel : Nat) :
    Measurable (Determinize.Spec.Paper.cumulativeOutputMeasure fuel) := by
  have equality : (fun expression => cumulativeKernel fuel expression) =
      Determinize.Spec.Paper.cumulativeOutputMeasure fuel := funext (cumulativeKernel_apply fuel)
  rw [← equality]
  exact (cumulativeKernel fuel).measurable

theorem measurable_sample_cumulative (fuel : Nat) (expression : Expr)
    (site : DistributionAction × Op) (fiber : Measure ℝ) (continuation : ℝ → Expr)
    (action : reduce expression = .sample site fiber continuation) :
    Measurable (fun x => Determinize.Spec.Paper.cumulativeOutputMeasure fuel (continuation x)) :=
  (measurable_direct_cumulative fuel).comp
    (paperStepKernel.sample_continuation_measurable expression fiber continuation action)

theorem direct_cumulative_mono (expression : Expr) :
    Monotone (fun fuel => Determinize.Spec.Paper.cumulativeOutputMeasure fuel expression) := by
  apply monotone_nat_of_le_succ
  intro fuel
  rw [← cumulativeOutputMeasure_eq, ← cumulativeOutputMeasure_eq,
    MeasurableActionFamily.cumulativeOutputMeasure_eq_finsetSum,
    MeasurableActionFamily.cumulativeOutputMeasure_eq_finsetSum]
  conv_rhs => rw [Finset.sum_range_succ]
  exact Measure.le_add_right le_rfl

theorem typed_determinize
    (typed : Determinize.Spec.Paper.Typed context expression ty) :
    Determinize.Spec.Paper.Typed context expression.determinize ty := by
  induction typed with
  | sub _ h ih => exact ih.sub h
  | _ =>
      simp only [Expr.determinize, DistributionAction.determinize]
      try cases ‹Affinity›
      all_goals aesop (add unsafe constructors Determinize.Spec.Paper.Typed)

set_option maxHeartbeats 800000 in
theorem sourceTags_of_sourceForm {expression : Expr}
    (source : expression.sourceForm = true) :
    (Symbolic.AffineExpr.ofExpr expression).SourceTags := by
  fun_induction Expr.sourceForm expression <;>
    simp_all [Symbolic.AffineExpr.ofExpr, Symbolic.AffineExpr.SourceTags]

/-- Typing preservation's companion for validity: the determinization of a well-typed source that
does not get stuck does not get stuck either. -/
theorem doesNotGetStuck_determinize (typed : Typed [] source (.float .E))
    (sourceForm : source.sourceForm = true)
    (safe : DoesNotGetStuck source) : DoesNotGetStuck source.determinize :=
  (Typing.primitiveDomainSafe_iff_doesNotGetStuck (typed_determinize typed)).1
    (SymbolicSoundness.TargetSafety.determinize_primitiveDomainSafe_of_typed_source primitiveLaws
      (MeasurableActionFamily.stepKernel primitiveLaws) source typed
      (sourceTags_of_sourceForm sourceForm)
      ((Typing.primitiveDomainSafe_iff_doesNotGetStuck typed).2 safe))

end Determinize.Proof.Paper
