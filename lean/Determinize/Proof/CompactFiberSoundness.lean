import Determinize.Proof.CompactReplay

/-!
# Fiber soundness of the compact replay

The lockstep argument of `SymbolicTraceSoundness` repeated for `Traces.outputGivenTraceAt`:
along the symbolic action of a well-typed source, the compact replay of its realizations,
pulled back along `retain` to detailed traces, is a sound fiber of the detailed joint laws,
and the compact replay of the target reproduces the target output on almost every trace.
-/

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety
open Determinize.Traces (outputGivenTraceAt outputGivenTrace)
open scoped ProbabilityTheory
noncomputable section

/-! ### The compact replay of a symbolic source -/

/-- The compact replay of the realized source, averaged over the history of E draws. -/
def compactHistoryReplay (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) :
    SFiniteKernel DrawTrace ℝ := by
  let : IsProbabilityMeasure (history.actualMeasure primitiveLaws) :=
    ⟨SymbolicSoundness.SampleEnv.actualMeasure_univ_eq_one _ history safe⟩
  exact averageKernel (history.actualMeasure primitiveLaws)
    (SFiniteKernel.pullback (compactReplayKernel depth)
      (fun pair : DrawTrace × Env n => (pair.1, expression.realize pair.2))
      (measurable_fst.prodMk (expression.realize_measurable.comp measurable_snd)))

theorem compactHistoryReplay_apply (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) (tape : DrawTrace) :
    (compactHistoryReplay depth history safe expression).kernel tape =
      (history.actualMeasure primitiveLaws).bind
        (fun env => outputGivenTraceAt depth (expression.realize env) tape) := by
  rw [compactHistoryReplay, averageKernel_apply]
  simp_rw [MeasurableActionFamily.pullback_apply, compactReplayKernel_apply]

theorem compactHistoryReplay_next (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression next : AffineExpr n)
    (typed : WellTyped [] expression ty) (notValue : expression.isValue ≠ true)
    (actionEq : symbolicReduce primitiveLaws expression = .next next) (tape : DrawTrace) :
    (compactHistoryReplay (depth + 1) history safe expression).kernel tape =
      (compactHistoryReplay depth history safe next).kernel tape := by
  rw [compactHistoryReplay_apply, compactHistoryReplay_apply]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  apply ogtAt_succ_next
  · simpa only [AffineExpr.realize_isValue] using notValue
  · rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
    rfl

theorem compactHistoryReplay_sampleE (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws)
    (expression : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true)
    (op : Op) (affine : List (Symbolic.Affine n)) (general : List ℝ)
    (continuation : AffineExpr (n + 1))
    (actionEq : symbolicReduce primitiveLaws expression = .sampleE op affine general continuation)
    (extendedSafe : (Symbolic.SampleEnv.snoc history op
        (fun i => affine.getD i.1 (0, fun _ => 0))
        (fun i => general.getD i.1 0)).DomainSafe primitiveLaws)
    (tape : DrawTrace) :
    (compactHistoryReplay (depth + 1) history safe expression).kernel tape =
      (compactHistoryReplay depth (Symbolic.SampleEnv.snoc history op
        (fun i => affine.getD i.1 (0, fun _ => 0)) (fun i => general.getD i.1 0))
        extendedSafe continuation).kernel tape := by
  rw [compactHistoryReplay_apply, compactHistoryReplay_apply,
    history_bind_snoc _ _ _ _ _ (fun env => outputGivenTraceAt depth (continuation.realize env) tape)
      ((ogtAt_expression_measurable depth tape).comp continuation.realize_measurable)]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  apply ogtAt_succ_sampleE
  · simpa only [AffineExpr.realize_isValue] using notValue
  · exact concrete_sampleE primitiveLaws expression typed op affine general continuation actionEq env
  · rfl

theorem compactHistoryReplay_sampleG (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n)
    (typed : WellTyped [] expression ty) (notValue : expression.isValue ≠ true)
    (fiber : Measure ℝ) (continuation : ℝ → AffineExpr n)
    (actionEq : symbolicReduce primitiveLaws expression = .sampleG site fiber continuation)
    (op : Op) (opEq : generationOp expression.skeleton = some op) (value : ℝ) (tape : DrawTrace) :
    (compactHistoryReplay (depth + 1) history safe expression).kernel ((op, value) :: tape) =
      (compactHistoryReplay depth history safe (continuation value)).kernel tape := by
  rw [compactHistoryReplay_apply, compactHistoryReplay_apply]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  have reduction : reduce (expression.realize env) =
      .sample site fiber (fun v => (continuation v).realize env) := by
    rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
    rfl
  have siteEq : site = (.G, .stochastic, op) := by
    have h := reduce_site reduction
    rw [generationOp_realize, opEq] at h
    rcases site with ⟨mode, kind, op'⟩
    cases mode <;> cases kind <;> simp_all [siteOp]
  subst siteEq
  exact ogtAt_succ_sampleG depth (by simpa only [AffineExpr.realize_isValue] using notValue)
    reduction value tape

/-- The compact replay read through the retained draws of a detailed trace. -/
def compactFiber (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) :
    SFiniteKernel Trace ℝ :=
  SFiniteKernel.pullback (compactHistoryReplay depth history safe expression) retain
    retain_measurable

theorem compactFiber_apply (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) (trace : Trace) :
    (compactFiber depth history safe expression).kernel trace =
      (compactHistoryReplay depth history safe expression).kernel (retain trace) :=
  MeasurableActionFamily.pullback_apply _ _ _ _

theorem retain_cons_none (trace : Trace) : retain (none :: trace) = retain trace := rfl

theorem retain_cons_some (draw : Op × ℝ) (trace : Trace) :
    retain (some draw :: trace) = draw :: retain trace := rfl

/-! ### Fiber soundness along the symbolic action -/

set_option maxHeartbeats 1200000 in
theorem compact_exactZero_fiberSound (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (safe : SafeConfigAt primitiveLaws 0 history expression) :
    FiberSound (compactFiber 0 history safe.1 expression)
      (actualTraceLaw 0 history expression) (targetTraceLaw 0 history expression) := by
  by_cases value : expression.isValue = true
  · obtain ⟨affine, rfl⟩ := wellTyped_real_value safe.2.1 value
    let law := history.actualMeasure primitiveLaws
    let μ := law.map affine.eval
    have mass : law Set.univ = 1 :=
      SymbolicSoundness.SampleEnv.actualMeasure_univ_eq_one _ history safe.1
    let : IsProbabilityMeasure law := ⟨mass⟩
    have replayEq : (compactFiber 0 history safe.1 (.real affine)).kernel [] = μ := by
      rw [compactFiber_apply, compactHistoryReplay_apply]
      simp only [AffineExpr.realize]
      change law.bind (fun env => Measure.dirac (affine.eval env)) = μ
      exact Measure.bind_dirac_eq_map _ (affine_eval_measurable affine)
    have sourceEq : actualTraceLaw 0 history (.real affine) =
        μ.map (fun x => (([] : Trace), x)) := by
      rw [actualTraceLaw]
      simp only [AffineExpr.realize, exactMeasure]
      change law.bind (fun env => Measure.dirac (([] : Trace), affine.eval env)) = _
      rw [Measure.bind_dirac_eq_map _ (measurable_const.prodMk (affine_eval_measurable affine))]
      dsimp only [μ]
      rw [Measure.map_map (show Measurable (fun x : ℝ => (([] : Trace), x)) from
        measurable_const.prodMk measurable_id) (affine_eval_measurable affine)]
      rfl
    have integrable := SymbolicSoundness.SampleEnv.integrable_affine primitiveLaws
      primitiveMomentBounds history safe.1 affine
    have mean := SymbolicSoundness.SampleEnv.integral_affine primitiveLaws primitiveMomentBounds
      history safe.1 affine
    have targetEq : targetTraceLaw 0 history (.real affine) =
        Measure.dirac (([] : Trace), affine.eval (history.meanEnvironment primitiveLaws)) := by
      simp only [targetTraceLaw, AffineExpr.realize, Expr.determinize, exactMeasure]
    rw [sourceEq, ← replayEq, targetEq]
    apply FiberSound.terminal
    rw [FiberGood, replayEq]
    refine ⟨?_, ?_, ?_⟩
    · exact Measure.map_apply_of_aemeasurable (affine_eval_measurable affine).aemeasurable
        MeasurableSet.univ |>.trans (by simp [mass])
    · exact (integrable_map_measure measurable_id.aestronglyMeasurable
        (affine_eval_measurable affine).aemeasurable).2 integrable
    · exact (integral_map (affine_eval_measurable affine).aemeasurable
        aestronglyMeasurable_id).trans mean
  · have actualZero : actualTraceLaw 0 history expression = 0 := by
      unfold actualTraceLaw
      have h : ∀ env, exactMeasure 0 (expression.realize env) = 0 := by
        intro env
        cases expression <;> simp_all [AffineExpr.realize, exactMeasure, AffineExpr.isValue]
      simp_rw [h]
      simp
    have targetZero : targetTraceLaw 0 history expression = 0 := by
      have nv : (expression.realize (history.meanEnvironment primitiveLaws)).determinize.isValue
          ≠ true := by
        simpa only [determinize_isValue, AffineExpr.realize_isValue] using value
      unfold targetTraceLaw
      generalize eq : (expression.realize (history.meanEnvironment primitiveLaws)).determinize = e
        at nv ⊢
      cases e <;> try rfl
      simp_all [Expr.isValue]
    rw [actualZero, targetZero]
    exact FiberSound.zero _

set_option maxHeartbeats 1600000 in
theorem compact_exactDepth_fiberSound (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (safe : SafeConfigAt primitiveLaws depth history expression) :
    FiberSound (compactFiber depth history safe.1 expression)
      (actualTraceLaw depth history expression) (targetTraceLaw depth history expression) := by
  induction depth generalizing n history expression with
  | zero => exact compact_exactZero_fiberSound history expression safe
  | succ depth ih =>
      rcases safe with ⟨historySafe, typed, sourceSafe⟩
      by_cases value : expression.isValue = true
      · have actualZero : actualTraceLaw (depth + 1) history expression = 0 := by
          unfold actualTraceLaw
          have h : ∀ env, exactMeasure (depth + 1) (expression.realize env) = 0 := by
            intro env
            rw [exactMeasure, if_pos (by simpa only [AffineExpr.realize_isValue] using value)]
          simp_rw [h]
          simp
        have targetZero : targetTraceLaw (depth + 1) history expression = 0 := by
          rw [targetTraceLaw, exactMeasure,
            if_pos (by simpa only [determinize_isValue, AffineExpr.realize_isValue] using value)]
        rw [actualZero, targetZero]
        exact FiberSound.zero _
      · have actionTyped := symbolicReduce_wellTyped primitiveLaws typed
        generalize actionEq : symbolicReduce primitiveLaws expression = action at actionTyped
        cases action with
        | next next =>
            have nextTyped : WellTyped [] next (.float .E) :=
              SymbolicAction.wellTyped_next_iff.mp actionTyped
            have nextSafe : ∀ᵐ env ∂history.actualMeasure primitiveLaws,
                PrimitiveDomainSafeAt depth (next.realize env) := by
              filter_upwards [sourceSafe] with env valid
              have reduction : reduce (expression.realize env) = .next (next.realize env) := by
                rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
                rfl
              have nv : (expression.realize env).isValue ≠ true := by
                simpa only [AffineExpr.realize_isValue] using value
              simpa only [PrimitiveDomainSafeAt, Bool.eq_false_of_not_eq_true nv,
                Bool.false_eq_true, ↓reduceIte, reduction] using valid
            have sound := ih history next ⟨historySafe, nextTyped, nextSafe⟩
            rw [actualTraceLaw_next _ _ _ _ typed value actionEq,
              targetTraceLaw_next _ _ _ _ typed value actionEq]
            exact FiberSound.mapTrace _ _ _ _ sound (fun tape : Trace => (List.cons none tape : Trace))
              (trace_cons_measurable.comp (measurable_const.prodMk measurable_id))
              (fun tape => by
                rw [compactFiber_apply, compactFiber_apply, retain_cons_none]
                exact compactHistoryReplay_next depth history historySafe expression next typed
                  value actionEq _)
        | sampleE op affine general continuation =>
            let extended := Symbolic.SampleEnv.snoc history op
              (fun i => affine.getD i.1 (0, fun _ => 0)) (fun i => general.getD i.1 0)
            have extension := source_sampleE_safe_extension
              (MeasurableActionFamily.stepKernel primitiveLaws) depth history historySafe
              expression typed sourceSafe op affine general continuation actionEq
            have extendedSafe : extended.DomainSafe primitiveLaws := extension.1
            have sound := ih extended continuation extension
            rw [actualTraceLaw_sampleE _ _ _ typed value _ _ _ _ actionEq,
              targetTraceLaw_sampleE _ _ historySafe _ typed value _ _ _ _ actionEq extendedSafe]
            exact FiberSound.mapTrace _ _ _ _ sound (fun tape : Trace => (List.cons none tape : Trace))
              (trace_cons_measurable.comp (measurable_const.prodMk measurable_id))
              (fun tape => by
                rw [compactFiber_apply, compactFiber_apply, retain_cons_none]
                exact compactHistoryReplay_sampleE depth history historySafe expression typed
                  value op affine general continuation actionEq extendedSafe _)
        | sampleG site fiber continuation =>
            rcases source_sampleG_safe_swap (MeasurableActionFamily.stepKernel primitiveLaws) depth
              history historySafe expression typed sourceSafe fiber continuation actionEq with
              ⟨fiberMass, continuationSafe⟩
            obtain ⟨op, opEq⟩ := sampleG_opSome typed actionEq
            rw [actualTraceLaw_sampleG _ _ historySafe _ typed value _ _ fiberMass actionEq op opEq,
              targetTraceLaw_sampleG _ _ _ typed value _ _ actionEq op opEq]
            apply FiberSound.mix
            filter_upwards [continuationSafe] with v valid
            have continuationTyped := SymbolicAction.wellTyped_sampleG_iff.mp actionTyped v
            have sound := ih history (continuation v) ⟨historySafe, continuationTyped, valid⟩
            rw [generatedSourceKernel_apply _ _ _ _ typed actionEq,
              generatedTargetKernel_apply _ _ _ typed actionEq]
            exact FiberSound.mapTrace _ _ _ _ sound
              (fun tape : Trace => (List.cons (entry (some op) v) tape : Trace))
              (trace_cons_measurable.comp (measurable_const.prodMk measurable_id))
              (fun tape => by
                rw [compactFiber_apply, compactFiber_apply, entry, Option.map_some,
                  retain_cons_some]
                exact compactHistoryReplay_sampleG depth history historySafe expression typed
                  value fiber continuation actionEq op opEq v _)
        | stuck => exact (SymbolicAction.not_wellTyped_stuck actionTyped).elim
/-! ### The target replays itself -/

theorem selfReplay_measurable (depth : Nat) (target : Expr) :
    MeasurableSet {p : Output |
      outputGivenTraceAt depth target (retain p.1) = Measure.dirac p.2} := by
  let κ := SFiniteKernel.pullback (compactReplayKernel depth)
    (fun p : Output => (retain p.1, target))
    ((retain_measurable.comp measurable_fst).prodMk measurable_const)
  have := κ.sfinite
  have eq : {p : Output | outputGivenTraceAt depth target (retain p.1) = Measure.dirac p.2} =
      {p | κ.kernel p = Measure.dirac p.2} := by
    ext p
    simp only [Set.mem_ofPred_eq, κ, MeasurableActionFamily.pullback_apply,
      compactReplayKernel_apply]
  rw [eq]
  exact measurableSet_kernel_eq_dirac κ.kernel measurable_snd

set_option maxHeartbeats 1600000 in
/-- Replaying the determinized program along its own compact trace returns its output. -/
theorem target_selfReplay (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (safe : SafeConfigAt primitiveLaws depth history expression) :
    ∀ᵐ p ∂targetTraceLaw depth history expression,
      outputGivenTraceAt depth
        (expression.realize (history.meanEnvironment primitiveLaws)).determinize (retain p.1) =
        Measure.dirac p.2 := by
  induction depth generalizing n history expression with
  | zero =>
      by_cases value : expression.isValue = true
      · obtain ⟨a, rfl⟩ := wellTyped_real_value safe.2.1 value
        have eq : targetTraceLaw 0 history (.real a) =
            Measure.dirac (([] : Trace), a.eval (history.meanEnvironment primitiveLaws)) := by
          simp only [targetTraceLaw, AffineExpr.realize, Expr.determinize, exactMeasure]
        rw [eq, ae_dirac_iff (selfReplay_measurable _ _)]
        rfl
      · have nv : (expression.realize (history.meanEnvironment primitiveLaws)).determinize.isValue
            ≠ true := by
          simpa only [determinize_isValue, AffineExpr.realize_isValue] using value
        unfold targetTraceLaw
        generalize he : (expression.realize (history.meanEnvironment primitiveLaws)).determinize = e
          at nv ⊢
        cases e <;> try simp [exactMeasure]
        case real r => simp_all [Expr.isValue]
  | succ depth ih =>
      rcases safe with ⟨historySafe, typed, sourceSafe⟩
      by_cases value : expression.isValue = true
      · simp [targetTraceLaw, exactMeasure, determinize_isValue, AffineExpr.realize_isValue, value]
      · have nv : (expression.realize (history.meanEnvironment primitiveLaws)).determinize.isValue
            ≠ true := by
          simpa only [determinize_isValue, AffineExpr.realize_isValue] using value
        have actionTyped := symbolicReduce_wellTyped primitiveLaws typed
        generalize actionEq : symbolicReduce primitiveLaws expression = action at actionTyped
        cases action with
        | next next =>
            have nextTyped := SymbolicAction.wellTyped_next_iff.mp actionTyped
            have nextSafe : ∀ᵐ env ∂history.actualMeasure primitiveLaws,
                PrimitiveDomainSafeAt depth (next.realize env) := by
              filter_upwards [sourceSafe] with env valid
              have reduction : reduce (expression.realize env) = .next (next.realize env) := by
                rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
                rfl
              have nv' : (expression.realize env).isValue ≠ true := by
                simpa only [AffineExpr.realize_isValue] using value
              simpa only [PrimitiveDomainSafeAt, Bool.eq_false_of_not_eq_true nv',
                Bool.false_eq_true, ↓reduceIte, reduction] using valid
            have reduction :
                reduce (expression.realize (history.meanEnvironment primitiveLaws)).determinize =
                  .next (next.realize (history.meanEnvironment primitiveLaws)).determinize := by
              rw [← symbolicReduce_targetRealize primitiveLaws typed, actionEq]
              rfl
            rw [targetTraceLaw_next _ _ _ _ typed value actionEq,
              ae_map_iff (show Measurable (prepend none) from
                prepend_measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable
                (selfReplay_measurable _ _)]
            filter_upwards [ih history next ⟨historySafe, nextTyped, nextSafe⟩] with p hp
            simpa only [prepend, retain_cons_none, ogtAt_succ_next depth nv reduction] using hp
        | sampleE op affine general continuation =>
            let extended := Symbolic.SampleEnv.snoc history op
              (fun i => affine.getD i.1 (0, fun _ => 0)) (fun i => general.getD i.1 0)
            have extension := source_sampleE_safe_extension
              (MeasurableActionFamily.stepKernel primitiveLaws) depth history historySafe
              expression typed sourceSafe op affine general continuation actionEq
            have extendedSafe : extended.DomainSafe primitiveLaws := extension.1
            have domainMean := SymbolicSoundness.SampleEnv.domain_at_meanEnvironment primitiveLaws
              history historySafe op (fun i => affine.getD i.1 (0, fun _ => 0))
                (fun i => general.getD i.1 0) extendedSafe.2
            have reduction := concrete_target_sampleE primitiveLaws expression typed op affine
              general continuation actionEq (history.meanEnvironment primitiveLaws) domainMean
            rw [targetTraceLaw_sampleE _ _ historySafe _ typed value _ _ _ _ actionEq extendedSafe,
              ae_map_iff (show Measurable (prepend none) from
                prepend_measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable
                (selfReplay_measurable _ _)]
            filter_upwards [ih extended continuation extension] with p hp
            rw [prepend, retain_cons_none, ogtAt_succ_sampleE depth nv reduction rfl,
              Measure.dirac_bind (ogtAt_continuation_measurable depth reduction _)]
            simpa only [extended, Symbolic.SampleEnv.meanEnvironment] using hp
        | sampleG site fiber continuation =>
            rcases source_sampleG_safe_swap (MeasurableActionFamily.stepKernel primitiveLaws) depth
              history historySafe expression typed sourceSafe fiber continuation actionEq with
              ⟨_, continuationSafe⟩
            obtain ⟨op, opEq⟩ := sampleG_opSome typed actionEq
            have reduction :
                reduce (expression.realize (history.meanEnvironment primitiveLaws)).determinize =
                  .sample site fiber (fun r =>
                    ((continuation r).realize (history.meanEnvironment primitiveLaws)).determinize) := by
              rw [← symbolicReduce_targetRealize primitiveLaws typed, actionEq]
              rfl
            have siteEq : site = (.G, .stochastic, op) := by
              have h := reduce_site reduction
              rw [generationOp_determinize, generationOp_realize, opEq] at h
              rcases site with ⟨mode, kind, op'⟩
              cases mode <;> cases kind <;> simp_all [siteOp]
            subst siteEq
            rw [targetTraceLaw_sampleG _ _ _ typed value _ _ actionEq op opEq,
              Measure.ae_comp_iff (selfReplay_measurable _ _)]
            filter_upwards [continuationSafe] with r valid
            rw [generatedTargetKernel_apply _ _ _ typed actionEq,
              ae_map_iff (show Measurable (prepend (entry (some op) r)) from
                prepend_measurable.comp (measurable_const.prodMk measurable_id)).aemeasurable
                (selfReplay_measurable _ _)]
            filter_upwards [ih history (continuation r)
              ⟨historySafe, SymbolicAction.wellTyped_sampleG_iff.mp actionTyped r, valid⟩] with p hp
            simpa only [prepend, entry, Option.map_some, retain_cons_some,
              ogtAt_succ_sampleG depth nv reduction] using hp
        | stuck => exact (SymbolicAction.not_wellTyped_stuck actionTyped).elim
/-! ### Specialization to a concrete source -/

theorem compactHistoryReplay_nil (depth : Nat) (source : Expr) (tape : DrawTrace) :
    (compactHistoryReplay depth .nil nilDomainSafe (AffineExpr.ofExpr source)).kernel tape =
      outputGivenTraceAt depth source tape := by
  rw [compactHistoryReplay_apply]
  change (Measure.dirac Env.empty).bind
    (fun env => outputGivenTraceAt depth ((AffineExpr.ofExpr source).realize env) tape) = _
  rw [Measure.dirac_bind (show Measurable
    (fun env => outputGivenTraceAt depth ((AffineExpr.ofExpr source).realize env) tape) from
    (ogtAt_expression_measurable depth tape).comp (AffineExpr.ofExpr source).realize_measurable),
    AffineExpr.realize_ofExpr]

theorem safeConfig_of_source (source : Expr) (typed : Typed [] source (.float .E))
    (sourceTags : (AffineExpr.ofExpr source).SourceTags) (sourceSafe : PrimitiveDomainSafe source)
    (depth : Nat) : SafeConfigAt primitiveLaws depth .nil (AffineExpr.ofExpr source) := by
  refine ⟨trivial, AffineExpr.wellTyped_ofExpr_of_typed typed sourceTags, ?_⟩
  rw [Symbolic.SampleEnv.actualMeasure, ae_dirac_eq]
  simpa only [Filter.eventually_pure, AffineExpr.realize_ofExpr] using sourceSafe depth

theorem compact_exactDepth_source_fiberSound (source : Expr) (typed : Typed [] source (.float .E))
    (sourceTags : (AffineExpr.ofExpr source).SourceTags) (sourceSafe : PrimitiveDomainSafe source)
    (depth : Nat) :
    FiberSound (compactFiber depth .nil nilDomainSafe (AffineExpr.ofExpr source))
      (exactMeasure depth source) (exactMeasure depth source.determinize) := by
  have sound := compact_exactDepth_fiberSound depth .nil (AffineExpr.ofExpr source)
    (safeConfig_of_source source typed sourceTags sourceSafe depth)
  have actualEq : actualTraceLaw depth .nil (AffineExpr.ofExpr source) = exactMeasure depth source := by
    unfold actualTraceLaw
    change (Measure.dirac Env.empty).bind
      (fun env => exactMeasure depth ((AffineExpr.ofExpr source).realize env)) = _
    rw [Measure.dirac_bind (show Measurable
      (fun env => exactMeasure depth ((AffineExpr.ofExpr source).realize env)) from
      (exact_measurable depth).comp (AffineExpr.ofExpr source).realize_measurable),
      AffineExpr.realize_ofExpr]
  have targetEq : targetTraceLaw depth .nil (AffineExpr.ofExpr source) =
      exactMeasure depth source.determinize := by
    simp only [targetTraceLaw, Symbolic.SampleEnv.meanEnvironment, AffineExpr.realize_ofExpr]
  rwa [actualEq, targetEq] at sound

theorem target_selfReplay_source (source : Expr) (typed : Typed [] source (.float .E))
    (sourceTags : (AffineExpr.ofExpr source).SourceTags) (sourceSafe : PrimitiveDomainSafe source)
    (depth : Nat) :
    ∀ᵐ p ∂exactMeasure depth source.determinize,
      outputGivenTraceAt depth source.determinize (retain p.1) = Measure.dirac p.2 := by
  have h := target_selfReplay depth .nil (AffineExpr.ofExpr source)
    (safeConfig_of_source source typed sourceTags sourceSafe depth)
  simpa only [targetTraceLaw, Symbolic.SampleEnv.meanEnvironment, AffineExpr.realize_ofExpr] using h

end
end Determinize.Proof.StepTraces
