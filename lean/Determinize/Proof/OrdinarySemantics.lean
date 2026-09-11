import Determinize.Statement.Main
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
open Determinize.Statement.Paper

private noncomputable abbrev paperStepKernel := MeasurableActionFamily.stepKernel primitiveLaws

private noncomputable abbrev cumulativeKernel (fuel : Nat) :=
  (MeasurableActionFamily.exactOutputKernel paperStepKernel 0) ∘ₖ
    (MeasurableActionFamily.nStepKernelPack paperStepKernel fuel).kernel

/-- The value `unit` never produces a real output; it is the sink of a rejected execution. -/
theorem cumulativeOutputMeasure_unit (fuel : Nat) :
    Determinize.Statement.Paper.cumulativeOutputMeasure fuel .unit = 0 := by
  induction fuel with
  | zero => rfl
  | succ fuel ih => simpa [Determinize.Statement.Paper.cumulativeOutputMeasure, reduce] using ih

/-- The direct evaluator is represented by a measurable kernel in its expression input. -/
theorem cumulativeKernel_apply (fuel : Nat) (expression : Expr) :
    cumulativeKernel fuel expression =
      Determinize.Statement.Paper.cumulativeOutputMeasure fuel expression := by
  symm
  induction fuel generalizing expression with
  | zero =>
      simp only [cumulativeKernel, MeasurableActionFamily.nStepKernelPack_zero_kernel,
        Kernel.comp_id, MeasurableActionFamily.exactOutputKernel_apply]
      cases expression <;> try rfl
  | succ fuel ih =>
      rw [cumulativeKernel, MeasurableActionFamily.cumulativeKernel_succ,
        Kernel.comp_apply, paperStepKernel.kernel_eq_stepMeasure]
      change Determinize.Statement.Paper.cumulativeOutputMeasure (fuel + 1) expression =
        (reduce expression).measure.bind (cumulativeKernel fuel)
      cases reduction : reduce expression with
      | next next =>
          simp only [Determinize.Statement.Paper.cumulativeOutputMeasure, reduction,
            Action.measure, Measure.dirac_bind (cumulativeKernel fuel).measurable]
          exact ih next
      | sample modeTag fiber continuation =>
          simp only [Determinize.Statement.Paper.cumulativeOutputMeasure, reduction, Action.measure]
          simp_rw [ih]
          have measurable := paperStepKernel.sample_continuation_measurable
            expression fiber continuation reduction
          ext set hs
          change (fiber.bind ((cumulativeKernel fuel) ∘ continuation)) set = _
          rw [Measure.bind_apply hs ((cumulativeKernel fuel).measurable.comp measurable).aemeasurable,
            Measure.bind_apply hs (cumulativeKernel fuel).aemeasurable,
            lintegral_map ((cumulativeKernel fuel).measurable_coe hs) measurable]
          rfl
      | stuck => simp [Determinize.Statement.Paper.cumulativeOutputMeasure, reduction, Action.measure]
      | reject =>
          simp only [Determinize.Statement.Paper.cumulativeOutputMeasure, reduction,
            Action.measure, Measure.dirac_bind (cumulativeKernel fuel).measurable]
          rw [← ih .unit, cumulativeOutputMeasure_unit]

theorem cumulativeOutputMeasure_eq (fuel : Nat) (expression : Expr) :
    Determinize.Proof.Paper.cumulativeOutputMeasure paperStepKernel fuel expression =
      Determinize.Statement.Paper.cumulativeOutputMeasure fuel expression := by
  rw [MeasurableActionFamily.cumulativeOutputMeasure_eq_kernel]
  exact cumulativeKernel_apply fuel expression

theorem bigStepMeasure_eq (expression : Expr) :
    Determinize.Proof.Paper.bigStepMeasure paperStepKernel expression =
      Determinize.Statement.Paper.bigStepMeasure expression := by
  unfold Determinize.Proof.Paper.bigStepMeasure Determinize.Statement.Paper.bigStepMeasure
  congr 1
  funext fuel
  exact cumulativeOutputMeasure_eq fuel expression

theorem typed_determinize
    (typed : Determinize.Statement.Paper.Typed context expression ty) :
    Determinize.Statement.Paper.Typed context expression.determinize ty := by
  induction typed with
  | _ =>
      simp only [Expr.determinize]
      aesop (add safe constructors Determinize.Statement.Paper.Typed)

set_option maxHeartbeats 800000 in
theorem sourceTags_of_sourceForm {expression : Expr}
    (source : expression.sourceForm = true) :
    (Symbolic.AffineExpr.ofExpr expression).SourceTags := by
  fun_induction Expr.sourceForm expression <;>
    simp_all [Symbolic.AffineExpr.ofExpr, Symbolic.AffineExpr.SourceTags]

end Determinize.Proof.Paper
