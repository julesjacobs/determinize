import Determinize.Proof.TraceSemantics

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
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
  | reject => exact measurable_const
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
      (MeasurableActionFamily.measurable_reduce primitiveLaws (MeasurableFamily.skeletonFiber skeleton))
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

/-- Replay G draws from a tape, while still sampling all E draws. -/
def replayMeasure : (depth : Nat) → Expr → Trace → Measure ℝ
  | 0, expression, _ => exactOutputMeasure (MeasurableActionFamily.stepKernel primitiveLaws) 0 expression
  | depth + 1, expression, tape =>
      if expression.isValue then 0
      else match generationOp expression.skeleton with
        | none => (Determinize.Statement.Paper.stepMeasure expression).bind fun next => replayMeasure depth next (List.tail tape)
        | some _ => replayMeasure depth (sampleContinuation expression (eventValue (tape.headD none))) (List.tail tape)


def replayStep : SFiniteKernel (Trace × Expr) (Trace × Expr) := by
  classical
  let step := MeasurableActionFamily.stepKernel primitiveLaws
  let sampled := SFiniteKernel.mapWithInput
    (SFiniteKernel.pullback ⟨step.kernel, step.kernel_sfinite⟩ Prod.snd measurable_snd)
    (fun pair : (Trace × Expr) × Expr => (List.tail pair.1.1, pair.2))
    ((trace_tail_measurable.comp (measurable_fst.comp measurable_fst)).prodMk measurable_snd)
  let forced := SFiniteKernel.deterministic
    (fun pair : Trace × Expr =>
      (List.tail pair.1, sampleContinuation pair.2 (eventValue (pair.1.headD none))))
    ((trace_tail_measurable.comp measurable_fst).prodMk
      (sampleContinuation_measurable.comp (measurable_snd.prodMk
        (eventValue_measurable.comp (trace_head_measurable.comp measurable_fst)))))
  let region : Set (Trace × Expr) :=
    {pair | generationOp pair.2.skeleton = none}
  have measurableRegion : MeasurableSet region :=
    (measurable_skeleton.comp measurable_snd) (show MeasurableSet {s | generationOp s = none} from trivial)
  exact SFiniteKernel.piecewise measurableRegion sampled forced

theorem replayStep_apply (tape : Trace) (expression : Expr) :
    (replayStep).kernel (tape, expression) =
      if generationOp expression.skeleton = none then
        (Determinize.Statement.Paper.stepMeasure expression).map (fun next => (List.tail tape, next))
      else Measure.dirac (List.tail tape, sampleContinuation expression (eventValue (tape.headD none))) := by
  classical
  unfold replayStep SFiniteKernel.piecewise
  simp only [Kernel.piecewise, Kernel.coe_mk, Set.mem_ofPred_eq]
  by_cases h : generationOp expression.skeleton = none
  · rw [if_pos h, if_pos h,
      SymbolicSoundness.TargetSafety.sfiniteKernel_mapWithInput_apply,
      MeasurableActionFamily.pullback_apply, StepKernel.kernel_eq_stepMeasure]
  · rw [if_neg h, if_neg h]
    rfl

def replayKernel : (depth : Nat) → SFiniteKernel (Trace × Expr) ℝ
  | 0 => SFiniteKernel.pullback
      (MeasurableActionFamily.exactOutputKernelPack
        (MeasurableActionFamily.stepKernel primitiveLaws) 0) Prod.snd measurable_snd
  | depth + 1 => by
      let previous := replayKernel depth
      let step := replayStep
      letI := previous.sfinite
      letI := step.sfinite
      have measurableValues : MeasurableSet {pair : Trace × Expr | pair.2.isValue = true} :=
        MeasurableActionFamily.valueSet_measurable.preimage measurable_snd
      exact SFiniteKernel.piecewise measurableValues SFiniteKernel.zero
        ⟨previous.kernel ∘ₖ step.kernel, inferInstance⟩

theorem replayKernel_apply (depth : Nat) (tape : Trace) (expression : Expr) :
    (replayKernel depth).kernel (tape, expression) = replayMeasure depth expression tape := by
  classical
  induction depth generalizing expression tape with
  | zero =>
      rw [replayKernel, MeasurableActionFamily.pullback_apply]
      exact MeasurableActionFamily.exactOutputKernel_apply _ _ _
  | succ depth ih =>
      unfold replayKernel SFiniteKernel.piecewise
      dsimp only
      simp only [Kernel.piecewise, Kernel.coe_mk, Set.mem_ofPred_eq]
      by_cases value : expression.isValue = true
      · simp [value, SFiniteKernel.zero, replayMeasure]
      · rw [if_neg value, replayMeasure, if_neg value, Kernel.comp_apply, replayStep_apply]
        cases siteEq : generationOp expression.skeleton with
        | none =>
            rw [if_pos rfl]
            have pairing : Measurable (fun next : Expr => (List.tail tape, next)) :=
              measurable_const.prodMk measurable_id
            rw [← Measure.bind_dirac_eq_map _ pairing,
              Measure.bind_bind (show AEMeasurable
                (fun next : Expr => Measure.dirac (List.tail tape, next)) (Determinize.Statement.Paper.stepMeasure expression) from
                  (Measure.measurable_dirac.comp pairing).aemeasurable)
                (replayKernel depth).kernel.aemeasurable]
            simp_rw [Measure.dirac_bind (replayKernel depth).kernel.measurable, ih]
        | some site =>
            rw [if_neg (by simp), Measure.dirac_bind (replayKernel depth).kernel.measurable]
            exact ih _ _


end

end Determinize.Proof.StepTraces
