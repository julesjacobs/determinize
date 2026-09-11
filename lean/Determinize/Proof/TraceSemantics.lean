import Determinize.Proof.TraceLabels
import Determinize.Proof.Internal.StepTraces
import Determinize.Proof.OrdinarySemantics

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper
open scoped ProbabilityTheory
open Classical

noncomputable section

theorem trace_length_measurable : Measurable (List.length : Trace → Nat) :=
  measurable_fst.comp (comap_measurable _)

theorem trace_event_measurable (index : Nat) :
    Measurable (fun trace : Trace => trace.getD index none) := by
  have coordinates : Measurable (fun trace : Trace =>
      (trace.length, fun i : Nat => trace.getD i none)) := comap_measurable _
  exact (measurable_pi_apply index).comp (measurable_snd.comp coordinates)

theorem trace_cons_measurable :
    Measurable (fun pair : Event × Trace => pair.1 :: pair.2) := by
  apply measurable_comap_iff.mpr
  apply Measurable.prodMk
  · exact (trace_length_measurable.comp measurable_snd).add_const 1
  · apply measurable_pi_lambda
    intro index
    cases index with
    | zero => exact measurable_fst
    | succ index => exact (trace_event_measurable index).comp measurable_snd

theorem trace_tail_measurable : Measurable (List.tail : Trace → Trace) := by
  apply measurable_comap_iff.mpr
  apply Measurable.prodMk
  · simpa using trace_length_measurable.sub_const 1
  · apply measurable_pi_lambda
    intro index
    simpa using trace_event_measurable (index + 1)

theorem trace_head_measurable : Measurable (fun trace : Trace => trace.headD none) := by
  have same : (fun trace : Trace => trace.headD none) =
      fun trace => trace.getD 0 none := by funext trace; cases trace <;> rfl
  rw [same]
  exact trace_event_measurable 0

theorem prepend_measurable :
    Measurable (fun pair : Event × Output => prepend pair.1 pair.2) :=
  (trace_cons_measurable.comp (measurable_fst.prodMk
    (measurable_fst.comp measurable_snd))).prodMk (measurable_snd.comp measurable_snd)

theorem map_bind {α β γ : Type*} [MeasurableSpace α] [MeasurableSpace β]
    [MeasurableSpace γ] (μ : Measure α) (kernel : Kernel α β)
    (f : β → γ) (hf : Measurable f) :
    (μ.bind kernel).map f = μ.bind (fun a => (kernel a).map f) := by
  rw [← Measure.bind_dirac_eq_map _ hf]
  rw [Measure.bind_bind kernel.aemeasurable
    (show AEMeasurable (fun x => Measure.dirac (f x)) (μ.bind kernel) from
      (Measure.measurable_dirac.comp hf).aemeasurable)]
  simp_rw [Measure.bind_dirac_eq_map _ hf]

theorem bind_map {α β γ : Type*} [MeasurableSpace α] [MeasurableSpace β]
    [MeasurableSpace γ] (μ : Measure α) (f : α → β) (hf : Measurable f)
    (kernel : Kernel β γ) :
    (μ.map f).bind kernel = μ.bind (fun a => kernel (f a)) := by
  rw [← Measure.bind_dirac_eq_map _ hf,
    Measure.bind_bind (show AEMeasurable (fun x => Measure.dirac (f x)) μ from
      (Measure.measurable_dirac.comp hf).aemeasurable) kernel.aemeasurable]
  simp_rw [Measure.dirac_bind kernel.measurable]

/-- A one-step expression kernel used to prove measurability of the direct trace evaluator. -/
def record : Action → Measure (Event × Expr)
  | .next expression => Measure.dirac (none, expression)
  | .sample modeTag fiber continuation =>
      fiber.map fun value => (generationEvent modeTag value, continuation value)
  | .stuck => 0

def recordKernel {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) : SFiniteKernel α (Event × Expr) := by
  induction family with
  | @next successor measurable =>
      exact SFiniteKernel.deterministic (fun a => (none, successor a))
        (measurable_const.prodMk measurable)
  | @sample site draw continuation measurable =>
      exact SFiniteKernel.mapWithInput draw
        (fun pair => (generationEvent site pair.2, continuation pair))
        (((generationEvent_measurable site).comp measurable_snd).prodMk measurable)
  | stuck => exact SFiniteKernel.zero
  | piecewise measurableRegion _ _ ihTrue ihFalse =>
      exact SFiniteKernel.piecewise measurableRegion ihTrue ihFalse

theorem recordKernel_apply {α : Type*} [MeasurableSpace α] {action : α → Action}
    (family : MeasurableActionFamily α action) (parameter : α) :
    (recordKernel family).kernel parameter = record (action parameter) := by
  classical
  induction family with
  | next measurable => rfl
  | sample draw measurable =>
      exact SymbolicSoundness.TargetSafety.sfiniteKernel_mapWithInput_apply _ _ _ _
  | stuck => rfl
  | @piecewise region _ measurableRegion whenTrue whenFalse trueFamily falseFamily ihTrue ihFalse =>
      calc
        _ = @ite _ (parameter ∈ region) (Classical.propDecidable _)
              ((recordKernel trueFamily).kernel parameter)
              ((recordKernel falseFamily).kernel parameter) := rfl
        _ = _ := by
          by_cases member : parameter ∈ region
          · simpa only [Set.piecewise, if_pos member] using ihTrue
          · simpa only [Set.piecewise, if_neg member] using ihFalse

def recordSkeletonKernel (skeleton : Skeleton) : Kernel Expr (Event × Expr) := by
  classical
  let family := MeasurableActionFamily.measurable_reduce primitiveLaws
    (MeasurableFamily.skeletonFiber skeleton)
  let localKernel := recordKernel family
  exact Kernel.piecewise (skeletonFiber_measurable skeleton)
    (localKernel.kernel.comap (MeasurableActionFamily.toSkeletonFiber skeleton)
      (MeasurableActionFamily.measurable_toSkeletonFiber skeleton)) 0

theorem recordSkeletonKernel_apply (skeleton : Skeleton) (expression : Expr) :
    recordSkeletonKernel skeleton expression =
      if expression.skeleton = skeleton then record (reduce expression) else 0 := by
  classical
  unfold recordSkeletonKernel
  rw [Kernel.piecewise_apply]
  by_cases member : expression.skeleton = skeleton
  · rw [if_pos (show expression ∈ SkeletonFiber skeleton from member), if_pos member, Kernel.comap_apply, recordKernel_apply,
      MeasurableActionFamily.toSkeletonFiber_coe_of_mem _ _ member]
  · rw [if_neg (show expression ∉ SkeletonFiber skeleton from member), if_neg member]
    rfl

theorem recordSkeletonKernel_sfinite (skeleton : Skeleton) :
    IsSFiniteKernel (recordSkeletonKernel skeleton) := by
  classical
  unfold recordSkeletonKernel
  let family := MeasurableActionFamily.measurable_reduce primitiveLaws
    (MeasurableFamily.skeletonFiber skeleton)
  let := (recordKernel family).sfinite
  infer_instance

def tracedStepKernel : SFiniteKernel Expr (Event × Expr) := by
  let _ (skeleton : Skeleton) := recordSkeletonKernel_sfinite skeleton
  exact ⟨Kernel.sum recordSkeletonKernel, inferInstance⟩

theorem tracedStepKernel_apply (expression : Expr) :
    tracedStepKernel.kernel expression = record (reduce expression) := by
  classical
  change (Kernel.sum recordSkeletonKernel) expression = _
  rw [Kernel.sum_apply]
  ext set measurableSet
  rw [Measure.sum_apply _ measurableSet, tsum_eq_single expression.skeleton]
  · rw [recordSkeletonKernel_apply, if_pos rfl]
  · intro skeleton different
    rw [recordSkeletonKernel_apply, if_neg (Ne.symm different)]
    rfl

theorem tracedStep_erasure (expression : Expr) :
    (record (reduce expression)).map Prod.snd =
      Determinize.Spec.Paper.stepMeasure expression := by
  unfold Determinize.Spec.Paper.stepMeasure
  generalize reduction : reduce expression = action
  cases action with
  | next next => simp [record, Action.measure, Measure.map_dirac' measurable_snd]
  | stuck => simp [record, Action.measure]
  | sample site fiber continuation =>
      have measurable := (MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable
        expression fiber continuation reduction
      rw [record, Measure.map_map measurable_snd
        (show Measurable (fun value => (generationEvent site value, continuation value)) from
          (generationEvent_measurable site).prodMk measurable)]
      rfl

def successorKernel (previous : SFiniteKernel Expr (Output)) :
    SFiniteKernel (Event × Expr) (Output) :=
  SFiniteKernel.mapWithInput
    (SFiniteKernel.pullback previous Prod.snd measurable_snd)
    (fun pair => prepend pair.1.1 pair.2)
    (prepend_measurable.comp ((measurable_fst.comp measurable_fst).prodMk measurable_snd))

theorem successorKernel_apply (previous : SFiniteKernel Expr (Output))
    (entry : Event) (expression : Expr) :
    (successorKernel previous).kernel (entry, expression) =
      (previous.kernel expression).map (prepend entry) := by
  rw [successorKernel, SymbolicSoundness.TargetSafety.sfiniteKernel_mapWithInput_apply,
    MeasurableActionFamily.pullback_apply]

def exactKernel : (depth : Nat) → SFiniteKernel Expr (Output)
  | 0 => SFiniteKernel.piecewise terminalFloatSet_measurable
      (SFiniteKernel.deterministic (fun e => ([], terminalFloatValue e))
        (measurable_const.prodMk terminalFloatValue_measurable)) SFiniteKernel.zero
  | depth + 1 => by
      let previous := exactKernel depth
      let next := successorKernel previous
      let := next.sfinite
      let := tracedStepKernel.sfinite
      exact SFiniteKernel.piecewise MeasurableActionFamily.valueSet_measurable
        SFiniteKernel.zero ⟨next.kernel ∘ₖ tracedStepKernel.kernel, inferInstance⟩

theorem exactKernel_apply (depth : Nat) (expression : Expr) :
    (exactKernel depth).kernel expression = exactMeasure depth expression := by
  classical
  induction depth generalizing expression with
  | zero =>
      unfold exactKernel SFiniteKernel.piecewise SFiniteKernel.deterministic SFiniteKernel.zero
      rw [Kernel.piecewise_apply]
      cases expression <;> try simp [terminalFloatSet, exactMeasure,
        terminalFloatValue, Kernel.deterministic_apply]
  | succ depth ih =>
      unfold exactKernel SFiniteKernel.piecewise SFiniteKernel.zero
      rw [Kernel.piecewise_apply]
      by_cases value : expression.isValue = true
      · simp [MeasurableActionFamily.valueSet, value, exactMeasure]
      · have notMember : expression ∉ MeasurableActionFamily.valueSet := value
        rw [if_neg notMember, exactMeasure, if_neg value, Kernel.comp_apply, tracedStepKernel_apply]
        cases reduction : reduce expression with
        | next next =>
            rw [record, Measure.dirac_bind (successorKernel (exactKernel depth)).kernel.measurable,
              successorKernel_apply, ih]
        | sample modeTag fiber continuation =>
            have measurable := (MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable
              expression fiber continuation reduction
            rw [record, bind_map _ _ ((generationEvent_measurable modeTag).prodMk measurable)]
            apply Measure.bind_congr_right
            filter_upwards [] with value
            rw [successorKernel_apply, ih]
        | stuck => simp [record]

theorem exact_succ_kernel (depth : Nat) (expression : Expr)
    (notValue : expression.isValue ≠ true) :
    exactMeasure (depth + 1) expression =
      (record (reduce expression)).bind (successorKernel (exactKernel depth)).kernel := by
  rw [← exactKernel_apply, exactKernel, SFiniteKernel.piecewise, Kernel.piecewise_apply,
    if_neg (show expression ∉ MeasurableActionFamily.valueSet from notValue),
    Kernel.comp_apply, tracedStepKernel_apply]

theorem exact_length (depth : Nat) (expression : Expr) :
    ∀ᵐ point ∂exactMeasure depth expression, point.1.length = depth := by
  induction depth generalizing expression with
  | zero =>
      cases expression <;> try (solve | simp [exactMeasure])
      case real value =>
        change ∀ᵐ point : Output ∂Measure.dirac ([], value), point.1.length = 0
        have measurableLength : MeasurableSet {point : Output | point.1.length = 0} :=
          measurableSet_eq_fun (trace_length_measurable.comp measurable_fst) measurable_const
        exact (ae_dirac_iff measurableLength).2 rfl
  | succ depth ih =>
      by_cases value : expression.isValue = true
      · simp [exactMeasure, value]
      · rw [exact_succ_kernel depth expression value]
        have measurableLength : MeasurableSet {point : Output | point.1.length = depth + 1} :=
          measurableSet_eq_fun (trace_length_measurable.comp measurable_fst) measurable_const
        rw [Measure.ae_comp_iff measurableLength]
        filter_upwards [] with next
        rw [successorKernel_apply, exactKernel_apply,
          ae_map_iff (show Measurable (prepend next.1) from
            prepend_measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable
            measurableLength]
        simpa only [prepend, List.length_cons, Nat.add_left_inj] using ih next.2

theorem exact_erasure (step : StepKernel) (depth : Nat) (expression : Expr) :
    (exactMeasure depth expression).map Prod.snd = exactOutputMeasure step depth expression := by
  induction depth generalizing expression with
  | zero =>
      cases expression <;> try simp [exactMeasure, exactOutputMeasure]
      case real value => simp [Measure.map_dirac' measurable_snd]
  | succ depth ih =>
      by_cases value : expression.isValue = true
      · simp [exactMeasure, exactOutputMeasure, value]
      · rw [exact_succ_kernel depth expression value, map_bind _ _ _ measurable_snd,
          exactOutputMeasure, if_neg value, step.kernel_eq_stepMeasure,
          ← tracedStep_erasure expression]
        have ordinaryEq : exactOutputMeasure step depth =
            MeasurableActionFamily.exactOutputKernel step depth := by
          funext next
          exact (MeasurableActionFamily.exactOutputKernel_apply step depth next).symm
        rw [ordinaryEq, bind_map _ _ measurable_snd (MeasurableActionFamily.exactOutputKernel step depth)]
        apply Measure.bind_congr_right
        filter_upwards [] with next
        rw [successorKernel_apply, exactKernel_apply,
          Measure.map_map measurable_snd
            (show Measurable (prepend next.1) from
              prepend_measurable.comp (measurable_const.prodMk measurable_id))]
        exact (ih next.2).trans (MeasurableActionFamily.exactOutputKernel_apply step depth next.2).symm

theorem correspondence : Determinize.Proof.StepTraces.correspondenceThm := by
  intro program
  let step := MeasurableActionFamily.stepKernel primitiveLaws
  rw [jointMeasure, Measure.map_sum measurable_snd.aemeasurable]
  change Measure.sum (fun depth => (exactMeasure depth program).map Prod.snd) = _
  simp_rw [exact_erasure step]
  rw [← MeasurableActionFamily.exactDepthConstruction step,
    Determinize.Proof.Paper.bigStepMeasure_eq]

end

end Determinize.Proof.StepTraces
