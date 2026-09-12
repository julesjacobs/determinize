import Determinize.Proof.CompactReplay

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety
open Determinize.Proof.Traces (outputGivenTraceAt)

noncomputable section

/-- A concrete mean step records an empty event and consumes one reduction step. -/
theorem exact_succ_mean (depth : Nat) (expression : Expr) (op : Op) (value : ℝ)
    (continuation : ℝ → Expr)
    (reduction : reduce expression = .sample (.mean, op) (Measure.dirac value) continuation) :
    exactMeasure (depth + 1) expression =
      (exactMeasure depth (continuation value)).map (prepend none) := by
  rw [exact_succ_sample _ _ _ _ (not_value_of_reduce_sample _ _ _ reduction) reduction,
    reduce_site reduction]
  change (Measure.dirac value).bind (fun v =>
    (exactMeasure depth (continuation v)).map (prepend none)) = _
  apply Measure.dirac_bind
  exact (Measure.measurable_map (prepend none)
    (prepend_measurable.comp (measurable_const.prodMk measurable_id))).comp
      ((exact_measurable depth).comp
        ((MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable
          _ _ _ reduction))

theorem ogtAt_succ_mean (depth : Nat) (expression : Expr) (op : Op) (value : ℝ)
    (continuation : ℝ → Expr)
    (reduction : reduce expression = .sample (.mean, op) (Measure.dirac value) continuation)
    (tape : DrawTrace) :
    outputGivenTraceAt (depth + 1) expression tape =
      outputGivenTraceAt depth (continuation value) tape := by
  rw [ogtAt_succ_sampleE depth (not_value_of_reduce_sample _ _ _ reduction) reduction rfl,
    Measure.dirac_bind (ogtAt_continuation_measurable depth reduction tape)]

theorem concrete_mean (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce expression = .mean op affine general continuation)
    (environment : Env n) (valid : domain op (meanParams op affine general environment)) :
    reduce (expression.realize environment) =
      .sample (.mean, op) (Measure.dirac ((meanAffine op affine general).eval environment))
        (fun value => (continuation (value, 0)).realize environment) := by
  have actionTyped := symbolicReduce_wellTyped typed
  rw [actionEq] at actionTyped
  obtain ⟨ha, hg, _, _⟩ := SymbolicAction.wellTyped_mean_iff.mp actionTyped
  rw [← symbolicReduce_realize typed environment, actionEq]
  simp only [SymbolicAction.realize]
  rw [primitiveFiber_mean_formula op affine general ha hg environment, if_pos valid]

theorem concrete_target_mean (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce expression = .mean op affine general continuation)
    (environment : Env n) (valid : domain op (meanParams op affine general environment)) :
    reduce (expression.realize environment).determinize =
      .sample (.mean, op) (Measure.dirac ((meanAffine op affine general).eval environment))
        (fun value => ((continuation (value, 0)).realize environment).determinize) := by
  have actionTyped := symbolicReduce_wellTyped typed
  rw [actionEq] at actionTyped
  obtain ⟨ha, hg, _, _⟩ := SymbolicAction.wellTyped_mean_iff.mp actionTyped
  rw [← symbolicReduce_targetRealize typed environment, actionEq]
  simp only [targetRealize]
  rw [primitiveFiber_mean_formula op affine general ha hg environment, if_pos valid]

theorem actualTraceLaw_mean (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce expression = .mean op affine general continuation)
    (valid : ∀ᵐ env ∂history.actualMeasure primitiveLaws,
      domain op (meanParams op affine general env)) :
    actualTraceLaw (depth + 1) history expression =
      (actualTraceLaw depth history (continuation (meanAffine op affine general))).map
        (prepend none) := by
  have actionTyped := symbolicReduce_wellTyped typed
  rw [actionEq] at actionTyped
  rw [actualTraceLaw, actualTraceLaw, map_bind_fun (history.actualMeasure primitiveLaws)
    (fun env => exactMeasure depth ((continuation (meanAffine op affine general)).realize env))
    ((exact_measurable depth).comp (continuation (meanAffine op affine general)).realize_measurable)
    (prepend none) (show Measurable (prepend none : Output → Output) from
      prepend_measurable.comp (measurable_const.prodMk measurable_id))]
  apply Measure.bind_congr_right
  filter_upwards [valid] with env valid
  rw [exact_succ_mean _ _ _ _ _ (concrete_mean expression typed actionEq env valid),
    ← mean_continuation_realize actionTyped env]

theorem targetTraceLaw_mean (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce expression = .mean op affine general continuation)
    (valid : domain op (meanParams op affine general (history.meanEnvironment primitiveLaws))) :
    targetTraceLaw (depth + 1) history expression =
      (targetTraceLaw depth history (continuation (meanAffine op affine general))).map
        (prepend none) := by
  have actionTyped := symbolicReduce_wellTyped typed
  rw [actionEq] at actionTyped
  rw [targetTraceLaw, exact_succ_mean _ _ _ _ _
    (concrete_target_mean expression typed actionEq _ valid),
    ← mean_continuation_realize actionTyped]
  rfl

theorem targetReplay_mean (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (actionEq : symbolicReduce expression = .mean op affine general continuation)
    (valid : domain op (meanParams op affine general (history.meanEnvironment primitiveLaws)))
    (tape : DrawTrace) :
    outputGivenTraceAt (depth + 1)
      (expression.realize (history.meanEnvironment primitiveLaws)).determinize tape =
    outputGivenTraceAt depth
      ((continuation (meanAffine op affine general)).realize
        (history.meanEnvironment primitiveLaws)).determinize tape := by
  have actionTyped := symbolicReduce_wellTyped typed
  rw [actionEq] at actionTyped
  rw [ogtAt_succ_mean _ _ _ _ _ (concrete_target_mean expression typed actionEq _ valid),
    ← mean_continuation_realize actionTyped]

end
end Determinize.Proof.StepTraces
