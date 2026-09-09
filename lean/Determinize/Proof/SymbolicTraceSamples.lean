import Determinize.Proof.SymbolicTraceLaws

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety
noncomputable section

theorem concrete_sampleE (laws : PrimitiveLaws)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (op : Op) (affine : List (Symbolic.Affine n)) (general : List ℝ)
    (continuation : AffineExpr (n+1))
    (actionEq : symbolicReduce laws expression = .sampleE op affine general continuation)
    (environment : Env n) :
    reduce (expression.realize environment) =
      .sample (.E, .stochastic op) (laws.kernel op
        (fun i => Symbolic.Affine.eval (affine.getD i.1 (0, fun _ => 0)) environment,
         fun i => general.getD i.1 0))
        (fun value => continuation.realize (Env.cons value environment)) := by
  have actionTyped := symbolicReduce_wellTyped laws typed
  rw [actionEq] at actionTyped
  rcases SymbolicAction.wellTyped_sampleE_iff.mp actionTyped with ⟨affineLength, generalLength, _⟩

  rw [← symbolicReduce_realize laws typed environment, actionEq]
  simp only [Symbolic.AffineExpr.SymbolicAction.realize]
  congr 1
  classical
  unfold primitiveFiber
    Determinize.Statement.Paper.parseParams
  simp only
  rw [dif_pos (by simpa [Tag.base] using affineLength),
    dif_pos (by simpa [Tag.base] using generalLength)]
  simp only
  rw [laws.kernel_eq_paperMeasure]
  congr 1
  apply Prod.ext <;> funext index
  · simp [List.getD_eq_getElem?_getD, index.isLt, affineLength]
  · simp [List.getD_eq_getElem?_getD, index.isLt, generalLength]

theorem concrete_target_sampleE (laws : PrimitiveLaws)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (op : Op) (affine : List (Symbolic.Affine n)) (general : List ℝ)
    (continuation : AffineExpr (n+1))
    (actionEq : symbolicReduce laws expression = .sampleE op affine general continuation)
    (mean : Env n)
    (paramsDomain : domain op
        (fun i => Symbolic.Affine.eval (affine.getD i.1 (0, fun _ => 0)) mean,
         fun i => general.getD i.1 0)) :
    reduce (expression.realize mean).determinize =
      .sample (.E, .mean op) (Measure.dirac (meanValue op
        (fun i => Symbolic.Affine.eval (affine.getD i.1 (0, fun _ => 0)) mean,
         fun i => general.getD i.1 0)))
        (fun value => (continuation.realize (Env.cons value mean)).determinize) := by
  let affineArgs : Fin (affineArity op) → Symbolic.Affine n := fun i => affine.getD i.1 (0, fun _ => 0)
  let generalArgs : Fin (generalArity op) → ℝ := fun i => general.getD i.1 0
  let params : Params op := (fun i => Symbolic.Affine.eval (affineArgs i) mean, generalArgs)
  have actionTyped := symbolicReduce_wellTyped laws typed
  rw [actionEq] at actionTyped
  rcases SymbolicAction.wellTyped_sampleE_iff.mp actionTyped with ⟨affineLength, generalLength, _⟩

  rw [← symbolicReduce_targetRealize laws typed mean, actionEq]
  simp only [targetRealize]
  congr 1
  classical
  unfold primitiveFiber
    Determinize.Statement.Paper.parseParams
  simp only
  rw [dif_pos (by simpa [Tag.base] using affineLength),
    dif_pos (by simpa [Tag.base] using generalLength)]
  simp only
  let evaluatedParams : Determinize.Statement.Paper.Params op :=
    (fun index => (affine.map (Symbolic.Affine.eval · mean))[index.1]'(by
        simp [affineLength]),
      fun index => general[index.1]'(by simp [generalLength]))
  have evaluatedParamsEq : evaluatedParams = params := by
    apply Prod.ext <;> funext index
    · simp [evaluatedParams, params, affineArgs, List.getD_eq_getElem?_getD,
        affineLength]
    · simp [evaluatedParams, params, generalArgs, List.getD_eq_getElem?_getD,
        generalLength]
  change (if Determinize.Statement.Paper.domain op evaluatedParams then
    Measure.dirac (Determinize.Statement.Paper.meanValue op evaluatedParams) else 0) = _
  rw [evaluatedParamsEq, if_pos paramsDomain]

variable {β : Type*} [MeasurableSpace β]

theorem history_bind_snoc (laws : PrimitiveLaws) (history : Symbolic.SampleEnv laws n)
    (op : Op) (affine : Fin (affineArity op) → Symbolic.Affine n)
    (general : Fin (generalArity op) → ℝ) (family : Env (n+1) → Measure β)
    (measurable : Measurable family) :
    ((Symbolic.SampleEnv.snoc history op affine general).actualMeasure laws).bind family =
      (history.actualMeasure laws).bind (fun env =>
        (laws.kernel op (fun i => (affine i).eval env, general)).bind (fun value => family (Env.cons value env))) := by
  change ((history.actualMeasure laws).bind
    (fun env => (laws.kernel op (fun i => (affine i).eval env, general)).map (fun v => Env.cons v env))).bind family = _
  have transitionEq :
      (fun env => (laws.kernel op (fun i => (affine i).eval env, general)).map (fun v => Env.cons v env)) =
      (SymbolicSoundness.SampleEnv.transitionPack laws op affine general).kernel := by
    funext env
    exact (SymbolicSoundness.SampleEnv.transitionPack_apply laws op affine general env).symm
  rw [transitionEq, Measure.bind_bind (SymbolicSoundness.SampleEnv.transitionPack laws op affine general).kernel.aemeasurable measurable.aemeasurable]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  rw [SymbolicSoundness.SampleEnv.transitionPack_apply]
  exact bind_map _ _ (measurable_envCons.comp (measurable_id.prodMk measurable_const)) ⟨family, measurable⟩

theorem actualTraceLaw_sampleE (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty) (notValue : expression.isValue ≠ true)
    (op : Op) (affine : List (Symbolic.Affine n)) (general : List ℝ) (continuation : AffineExpr (n+1))
    (actionEq : symbolicReduce primitiveLaws expression = .sampleE op affine general continuation) :
    actualTraceLaw (depth+1) history expression =
      (actualTraceLaw depth (Symbolic.SampleEnv.snoc history op
        (fun i => affine.getD i.1 (0,fun _ => 0)) (fun i => general.getD i.1 0)) continuation).map (prepend none) := by
  have prefixMeasurable : Measurable (prepend none : Output → Output) :=
    prepend_measurable.comp (measurable_const.prodMk measurable_id)
  rw [actualTraceLaw, actualTraceLaw,
    map_bind_fun _ (fun env => exactMeasure depth (continuation.realize env)) ((exact_measurable depth).comp continuation.realize_measurable) _ prefixMeasurable,
    history_bind_snoc _ _ _ _ _ (fun env => (exactMeasure depth (continuation.realize env)).map (prepend none))
      ((Measure.measurable_map _ prefixMeasurable).comp ((exact_measurable depth).comp continuation.realize_measurable))]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  rw [exact_succ_sample _ _ _ _ (by simpa only [AffineExpr.realize_isValue] using notValue)
    (concrete_sampleE primitiveLaws expression typed op affine general continuation actionEq env),
    generationOp_realize, sampleE_opNone typed actionEq]
  rfl

theorem historyReplay_sampleE (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty) (notValue : expression.isValue ≠ true)
    (op : Op) (affine : List (Symbolic.Affine n)) (general : List ℝ) (continuation : AffineExpr (n+1))
    (actionEq : symbolicReduce primitiveLaws expression = .sampleE op affine general continuation)
    (extendedSafe : (Symbolic.SampleEnv.snoc history op
        (fun i => affine.getD i.1 (0,fun _ => 0)) (fun i => general.getD i.1 0)).DomainSafe primitiveLaws)
    (tape : Trace) :
    (historyReplay (depth+1) history safe expression).kernel tape =
      (historyReplay depth (Symbolic.SampleEnv.snoc history op
        (fun i => affine.getD i.1 (0,fun _ => 0)) (fun i => general.getD i.1 0)) extendedSafe continuation).kernel (List.tail tape) := by
  rw [historyReplay_apply, historyReplay_apply,
    history_bind_snoc _ _ _ _ _ (fun env => replayMeasure depth (continuation.realize env) (List.tail tape)) ((replay_measurable depth (List.tail tape)).comp continuation.realize_measurable)]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  apply replay_succ_sampleE
  · simpa only [AffineExpr.realize_isValue] using notValue
  · rw [generationOp_realize, sampleE_opNone typed actionEq]
  · exact concrete_sampleE primitiveLaws expression typed op affine general continuation actionEq env

theorem targetTraceLaw_sampleE (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty) (notValue : expression.isValue ≠ true)
    (op : Op) (affine : List (Symbolic.Affine n)) (general : List ℝ) (continuation : AffineExpr (n+1))
    (actionEq : symbolicReduce primitiveLaws expression = .sampleE op affine general continuation)
    (extendedSafe : (Symbolic.SampleEnv.snoc history op
        (fun i => affine.getD i.1 (0,fun _ => 0)) (fun i => general.getD i.1 0)).DomainSafe primitiveLaws) :
    targetTraceLaw (depth+1) history expression =
      (targetTraceLaw depth (Symbolic.SampleEnv.snoc history op
        (fun i => affine.getD i.1 (0,fun _ => 0)) (fun i => general.getD i.1 0)) continuation).map (prepend none) := by
  let affineArgs : Fin (affineArity op) → Symbolic.Affine n := fun i => affine.getD i.1 (0,fun _ => 0)
  let generalArgs : Fin (generalArity op) → ℝ := fun i => general.getD i.1 0
  let mean := history.meanEnvironment primitiveLaws
  have domainAtMean := SymbolicSoundness.SampleEnv.domain_at_meanEnvironment primitiveLaws history safe op affineArgs generalArgs extendedSafe.2
  have reduction := concrete_target_sampleE primitiveLaws expression typed op affine general continuation actionEq mean domainAtMean
  rw [targetTraceLaw, exact_succ_sample _ _ _ _
    (by simpa only [determinize_isValue, AffineExpr.realize_isValue] using notValue) reduction,
    generationOp_determinize, generationOp_realize, sampleE_opNone typed actionEq]
  have contMeasurable := (MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable _ _ _ reduction
  have measurable : Measurable (fun value =>
      (exactMeasure depth (continuation.realize (Env.cons value mean)).determinize).map (prepend none)) :=
    (Measure.measurable_map (prepend none) (prepend_measurable.comp (measurable_const.prodMk measurable_id))).comp
      ((exact_measurable depth).comp contMeasurable)
  change (Measure.dirac _).bind (fun value =>
      (exactMeasure depth (continuation.realize (Env.cons value mean)).determinize).map (prepend none)) = _
  rw [Measure.dirac_bind measurable]
  rfl

end
end Determinize.Proof.StepTraces
