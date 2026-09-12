import Determinize.Proof.TraceSemantics

/-!
# Sampling continuations

`sampleContinuation` feeds a recorded value to the sampling step of an expression; the compact
replay (`Proof/CompactReplay.lean`) and the symbolic trace generation use it to read a
G-affinity draw from a trace, and `sampleContinuation_measurable` makes that step a
measurable function of the expression and the value.
-/

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper
open scoped ProbabilityTheory

noncomputable section

/-- Supply a retained draw directly; other reduction actions have no sampling continuation. -/
def sampleContinuation (expression : Expr) (value : ℝ) : Expr :=
  match reduce expression with
  | .sample _ _ continuation => continuation value
  | _ => .unit

theorem actionContinuation_measurable {α : Type*} [MeasurableSpace α]
    {action : α → Action} (family : MeasurableActionFamily α action) :
    Measurable (fun pair : α × ℝ => match action pair.1 with
      | .sample _ _ continuation => continuation pair.2
      | _ => .unit) := by
  induction family with
  | next => exact measurable_const
  | sample draw measurable => exact measurable
  | stuck => exact measurable_const
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily ihTrue ihFalse =>
      classical
      convert ihTrue.piecewise (measurableRegion.preimage measurable_fst) ihFalse using 1
      funext pair
      by_cases h : pair.1 ∈ region <;> simp [Set.piecewise, h]
      all_goals infer_instance

theorem sampleContinuation_measurable :
    Measurable (fun pair : Expr × ℝ => sampleContinuation pair.1 pair.2) := by
  have localMeasurable (skeleton : Skeleton) :
      Measurable (fun pair : SkeletonFiber skeleton × ℝ =>
        sampleContinuation pair.1.val pair.2) :=
    actionContinuation_measurable
      (MeasurableActionFamily.reduceFamily primitiveLaws (MeasurableFamily.skeletonFiber skeleton))
  let selected : Expr × ℝ → Expr := fun pair =>
    sampleContinuation (MeasurableActionFamily.toSkeletonFiber pair.1.skeleton pair.1).val pair.2
  have auxiliary : Measurable (fun pair : Skeleton × (Expr × ℝ) =>
      sampleContinuation (MeasurableActionFamily.toSkeletonFiber pair.1 pair.2.1).val pair.2.2) := by
    apply measurable_from_prod_countable_right
    intro skeleton
    exact (localMeasurable skeleton).comp
      (((MeasurableActionFamily.measurable_toSkeletonFiber skeleton).comp measurable_fst).prodMk
        measurable_snd)
  have selectedMeasurable : Measurable selected :=
    auxiliary.comp ((measurable_skeleton.comp measurable_fst).prodMk measurable_id)
  convert selectedMeasurable using 1
  funext pair
  change sampleContinuation pair.1 pair.2 =
    sampleContinuation (MeasurableActionFamily.toSkeletonFiber pair.1.skeleton pair.1).val pair.2
  rw [MeasurableActionFamily.toSkeletonFiber_coe_of_mem _ _ rfl]

end

end Determinize.Proof.StepTraces
