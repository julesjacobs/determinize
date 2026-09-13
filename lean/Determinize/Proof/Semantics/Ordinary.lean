import Determinize.Spec.Main
import Determinize.Proof.Semantics.Cumulative
import Determinize.Proof.Primitives.Kernels
import Determinize.Proof.Symbolic.Soundness

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
      Determinize.Proof.Cumulative.outputMeasure fuel expression := by
  symm
  induction fuel generalizing expression with
  | zero =>
      simp only [cumulativeKernel, MeasurableActionFamily.nStepKernelPack_zero_kernel,
        Kernel.comp_id, MeasurableActionFamily.exactOutputKernel_apply]
      cases expression <;> try rfl
  | succ fuel ih =>
      rw [cumulativeKernel, MeasurableActionFamily.cumulativeKernel_succ,
        Kernel.comp_apply, paperStepKernel.kernel_eq_stepMeasure]
      change Determinize.Proof.Cumulative.outputMeasure (fuel + 1) expression =
        (reduce expression).measure.bind (cumulativeKernel fuel)
      cases reduction : reduce expression with
      | next next =>
          simp only [Determinize.Proof.Cumulative.outputMeasure, reduction,
            Action.measure, Measure.dirac_bind (cumulativeKernel fuel).measurable]
          exact ih next
      | sample modeTag fiber continuation =>
          simp only [Determinize.Proof.Cumulative.outputMeasure, reduction, Action.measure]
          simp_rw [ih]
          have measurable := paperStepKernel.sample_continuation_measurable
            expression fiber continuation reduction
          ext set hs
          change (fiber.bind ((cumulativeKernel fuel) ∘ continuation)) set = _
          rw [Measure.bind_apply hs ((cumulativeKernel fuel).measurable.comp measurable).aemeasurable,
            Measure.bind_apply hs (cumulativeKernel fuel).aemeasurable,
            lintegral_map ((cumulativeKernel fuel).measurable_coe hs) measurable]
          rfl
      | stuck => simp [Determinize.Proof.Cumulative.outputMeasure, reduction, Action.measure]

theorem cumulativeOutputMeasure_eq (fuel : Nat) (expression : Expr) :
    Determinize.Proof.Paper.cumulativeOutputMeasure paperStepKernel fuel expression =
      Determinize.Proof.Cumulative.outputMeasure fuel expression := by
  rw [MeasurableActionFamily.cumulativeOutputMeasure_eq_kernel]
  exact cumulativeKernel_apply fuel expression

theorem exactOutputMeasure_eq (depth : Nat) (expression : Expr) :
    Determinize.Proof.Paper.exactOutputMeasure paperStepKernel depth expression =
      Determinize.Spec.Paper.outputMeasureAt depth expression := by
  rw [← MeasurableActionFamily.exactOutputKernel_apply]
  symm
  induction depth generalizing expression with
  | zero =>
      rw [MeasurableActionFamily.exactOutputKernel_apply]
      cases expression <;> rfl
  | succ depth ih =>
      by_cases value : expression.isValue = true
      · simp [Determinize.Spec.Paper.outputMeasureAt, value,
          MeasurableActionFamily.exactOutputKernel_succ_apply_of_value]
      · rw [Determinize.Spec.Paper.outputMeasureAt, if_neg value,
          MeasurableActionFamily.exactOutputKernel_succ_apply_of_not_value
            paperStepKernel depth expression value,
          Kernel.comp_apply, paperStepKernel.kernel_eq_stepMeasure, stepMeasure]
        cases reduction : reduce expression with
        | next next =>
            simp only [Action.measure,
              Measure.dirac_bind (MeasurableActionFamily.exactOutputKernel paperStepKernel depth).measurable]
            exact ih next
        | sample site fiber continuation =>
            simp only [Action.measure]
            simp_rw [ih]
            have measurable := paperStepKernel.sample_continuation_measurable
              expression fiber continuation reduction
            ext set hs
            change (fiber.bind ((MeasurableActionFamily.exactOutputKernel paperStepKernel depth) ∘ continuation)) set = _
            rw [Measure.bind_apply hs
                ((MeasurableActionFamily.exactOutputKernel paperStepKernel depth).measurable.comp measurable).aemeasurable,
              Measure.bind_apply hs
                (MeasurableActionFamily.exactOutputKernel paperStepKernel depth).measurable.aemeasurable,
              lintegral_map
                ((MeasurableActionFamily.exactOutputKernel paperStepKernel depth).measurable_coe hs) measurable]
            rfl
        | stuck => simp [Action.measure]

theorem cumulativeOutputMeasure_eq_sum (fuel : Nat) (expression : Expr) :
    Cumulative.outputMeasure fuel expression =
      ∑ depth ∈ Finset.range (fuel + 1), Determinize.Spec.Paper.outputMeasureAt depth expression := by
  rw [← cumulativeOutputMeasure_eq,
    MeasurableActionFamily.cumulativeOutputMeasure_eq_finsetSum]
  simp_rw [exactOutputMeasure_eq]

theorem bigStepMeasure_eq (expression : Expr) :
    Determinize.Proof.Paper.bigStepMeasure paperStepKernel expression =
      Determinize.Spec.Paper.bigStepMeasure expression := by
  rw [MeasurableActionFamily.bigStepMeasure_eq_sum_exactOutputMeasure]
  simp_rw [exactOutputMeasure_eq]
  rfl

theorem bigStepMeasure_eq_iSup_cumulative (expression : Expr) :
    Determinize.Spec.Paper.bigStepMeasure expression =
      ⨆ fuel, Cumulative.outputMeasure fuel expression := by
  rw [← bigStepMeasure_eq]
  unfold Determinize.Proof.Paper.bigStepMeasure
  exact iSup_congr fun fuel => cumulativeOutputMeasure_eq fuel expression

theorem measurable_direct_cumulative (fuel : Nat) :
    Measurable (Determinize.Proof.Cumulative.outputMeasure fuel) := by
  have equality : (fun expression => cumulativeKernel fuel expression) =
      Determinize.Proof.Cumulative.outputMeasure fuel := funext (cumulativeKernel_apply fuel)
  rw [← equality]
  exact (cumulativeKernel fuel).measurable

theorem measurable_sample_cumulative (fuel : Nat) (expression : Expr)
    (site : DistributionAction × Op) (fiber : Measure ℝ) (continuation : ℝ → Expr)
    (action : reduce expression = .sample site fiber continuation) :
    Measurable (fun x => Determinize.Proof.Cumulative.outputMeasure fuel (continuation x)) :=
  (measurable_direct_cumulative fuel).comp
    (paperStepKernel.sample_continuation_measurable expression fiber continuation action)

theorem direct_cumulative_mono (expression : Expr) :
    Monotone (fun fuel => Determinize.Proof.Cumulative.outputMeasure fuel expression) := by
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

/-- Typing preservation's companion for validity: the determinization of a well-typed source that
does not get stuck does not get stuck either. -/
theorem domainSafe_determinize (typed : Typed [] source (.float .E))
    (safe : DomainSafe source) : DomainSafe source.determinize :=
  SymbolicSoundness.TargetSafety.determinize_domainSafe_of_typed_source primitiveLaws
    (MeasurableActionFamily.stepKernel primitiveLaws) source typed safe

end Determinize.Proof.Paper
