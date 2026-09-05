import Determinize.Proof.SymbolicTraceSamples

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety
noncomputable section

theorem sampleG_joint_measurable (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation) :
    Measurable (fun pair : Env n × ℝ => (continuation pair.2).realize pair.1) := by
  have eq : (fun pair : Env n × ℝ => (continuation pair.2).realize pair.1) =
      fun pair => sampleContinuation (expression.realize pair.1) pair.2 := by
    funext pair
    unfold sampleContinuation
    rw [← symbolicReduce_realize primitiveLaws typed typed.gconstant pair.1, actionEq]
    rfl
  rw [eq]
  exact sampleContinuation_measurable.comp
    ((expression.realize_measurable.comp measurable_fst).prodMk measurable_snd)

theorem sampleG_target_measurable (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation) (env : Env n) :
    Measurable (fun v => ((continuation v).realize env).determinize) := by
  apply (MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable (expression.realize env).determinize fiber
  rw [← symbolicReduce_targetRealize primitiveLaws typed typed.gconstant, actionEq]
  rfl

def generatedSourceKernel (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) (op : Op) :
    SFiniteKernel ℝ (Output) := by
  let : IsProbabilityMeasure (history.actualMeasure primitiveLaws) :=
    ⟨SymbolicSoundness.SampleEnv.actualMeasure_univ_eq_one _ history safe⟩
  let output := SFiniteKernel.pullback (exactKernel depth)
    (fun pair : ℝ × Env n => sampleContinuation (expression.realize pair.2) pair.1)
    (sampleContinuation_measurable.comp ((expression.realize_measurable.comp measurable_snd).prodMk measurable_fst))
  exact SFiniteKernel.mapWithInput (averageKernel (history.actualMeasure primitiveLaws) output)
    (fun pair : ℝ × Output => prepend (entry (some op) pair.1) pair.2)
    (prepend_measurable.comp
      ((entry_measurable.comp ((measurable_const (a := some op)).prodMk measurable_fst)).prodMk measurable_snd))

def generatedTargetKernel (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (op : Op) : SFiniteKernel ℝ (Output) :=
  SFiniteKernel.mapWithInput
    (SFiniteKernel.pullback (exactKernel depth)
      (fun value => sampleContinuation (expression.realize (history.meanEnvironment primitiveLaws)).determinize value)
      (sampleContinuation_measurable.comp (measurable_const.prodMk measurable_id)))
    (fun pair : ℝ × Output => prepend (entry (some op) pair.1) pair.2)
    (prepend_measurable.comp
      ((entry_measurable.comp ((measurable_const (a := some op)).prodMk measurable_fst)).prodMk measurable_snd))

theorem generatedSourceKernel_apply (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation) (op : Op) (value : ℝ) :
    (generatedSourceKernel depth history safe expression op).kernel value =
      (actualTraceLaw depth history (continuation value)).map (prepend (entry (some op) value)) := by
  rw [generatedSourceKernel, sfiniteKernel_mapWithInput_apply, averageKernel_apply]
  congr 1
  apply Measure.bind_congr_right
  filter_upwards [] with env
  rw [MeasurableActionFamily.pullback_apply, exactKernel_apply]
  unfold sampleContinuation
  rw [← symbolicReduce_realize primitiveLaws typed typed.gconstant env, actionEq]
  rfl

theorem generatedTargetKernel_apply (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation) (op : Op) (value : ℝ) :
    (generatedTargetKernel depth history expression op).kernel value =
      (targetTraceLaw depth history (continuation value)).map (prepend (entry (some op) value)) := by
  rw [generatedTargetKernel, sfiniteKernel_mapWithInput_apply, MeasurableActionFamily.pullback_apply, exactKernel_apply]
  unfold sampleContinuation
  rw [← symbolicReduce_targetRealize primitiveLaws typed typed.gconstant, actionEq]
  rfl

theorem actualTraceLaw_sampleG (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true) (fiber : Measure ℝ) (continuation : ℝ → AffineExpr n)
    (fiberMass : fiber Set.univ = 1)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation)
    (op : Op) (opEq : generationOp expression.skeleton = some op) :
    actualTraceLaw (depth+1) history expression =
      fiber.bind (generatedSourceKernel depth history safe expression op).kernel := by
  let : IsProbabilityMeasure (history.actualMeasure primitiveLaws) :=
    ⟨SymbolicSoundness.SampleEnv.actualMeasure_univ_eq_one _ history safe⟩
  let : IsProbabilityMeasure fiber := ⟨fiberMass⟩
  let next := SFiniteKernel.pullback (exactKernel depth)
    (fun pair : Env n × ℝ => (continuation pair.2).realize pair.1)
    (sampleG_joint_measurable expression typed actionEq)
  let joint := SFiniteKernel.mapWithInput next
    (fun pair : (Env n × ℝ) × Output => prepend (entry (some op) pair.1.2) pair.2)
    (prepend_measurable.comp ((entry_measurable.comp
      ((measurable_const (a := some op)).prodMk (measurable_snd.comp measurable_fst))).prodMk measurable_snd))
  have jointEq (env : Env n) (v : ℝ) : joint.kernel (env,v) =
      (exactMeasure depth ((continuation v).realize env)).map (prepend (entry (some op) v)) := by
    rw [sfiniteKernel_mapWithInput_apply, MeasurableActionFamily.pullback_apply, exactKernel_apply]
  have sourceEq : actualTraceLaw (depth+1) history expression =
      (history.actualMeasure primitiveLaws).bind (fun env => fiber.bind (fun v => joint.kernel (env,v))) := by
    apply Measure.bind_congr_right
    filter_upwards [] with env
    have reduction : reduce (expression.realize env) = .sample site fiber (fun v => (continuation v).realize env) := by
      rw [← symbolicReduce_realize primitiveLaws typed typed.gconstant env, actionEq]
      rfl
    rw [exact_succ_sample _ _ _ _ (by simpa only [AffineExpr.realize_isValue] using notValue) reduction,
      generationOp_realize, opEq]
    simp_rw [jointEq]
  let := joint.sfinite
  rw [sourceEq, bind_bind_const_swap _ _ joint.kernel]
  apply Measure.bind_congr_right
  filter_upwards [] with v
  rw [generatedSourceKernel_apply _ _ _ _ typed actionEq, actualTraceLaw,
    map_bind_fun _ (fun env => exactMeasure depth ((continuation v).realize env)) ((exact_measurable depth).comp (continuation v).realize_measurable) _
      (show Measurable (prepend (entry (some op) v) : Output → Output) from
        prepend_measurable.comp (measurable_const.prodMk measurable_id))]
  simp_rw [jointEq]

theorem targetTraceLaw_sampleG (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true) (fiber : Measure ℝ) (continuation : ℝ → AffineExpr n)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation)
    (op : Op) (opEq : generationOp expression.skeleton = some op) :
    targetTraceLaw (depth+1) history expression =
      fiber.bind (generatedTargetKernel depth history expression op).kernel := by
  have reduction : reduce (expression.realize (history.meanEnvironment primitiveLaws)).determinize =
      .sample site fiber (fun v => ((continuation v).realize (history.meanEnvironment primitiveLaws)).determinize) := by
    rw [← symbolicReduce_targetRealize primitiveLaws typed typed.gconstant, actionEq]
    rfl
  rw [targetTraceLaw, exact_succ_sample _ _ _ _
    (by simpa only [determinize_isValue, AffineExpr.realize_isValue] using notValue) reduction,
    generationOp_determinize, generationOp_realize, opEq]
  apply Measure.bind_congr_right
  filter_upwards [] with v
  exact (generatedTargetKernel_apply depth history expression typed actionEq op v).symm

theorem historyReplay_sampleG (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true) (fiber : Measure ℝ) (continuation : ℝ → AffineExpr n)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation)
    (op : Op) (opEq : generationOp expression.skeleton = some op) (tape : Trace) :
    (historyReplay (depth+1) history safe expression).kernel tape =
      (historyReplay depth history safe (continuation (eventValue (tape.headD none)))).kernel (List.tail tape) := by
  rw [historyReplay_apply, historyReplay_apply]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  apply replay_succ_sampleG depth (expression.realize env) fiber (fun v => (continuation v).realize env) tape op
  · simpa only [AffineExpr.realize_isValue] using notValue
  · rw [generationOp_realize, opEq]
  · rw [← symbolicReduce_realize primitiveLaws typed typed.gconstant env, actionEq]
    rfl

end
end Determinize.Proof.StepTraces
