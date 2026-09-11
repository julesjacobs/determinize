import Determinize.Proof.SymbolicTraceGeneration

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety
noncomputable section


set_option maxHeartbeats 1200000 in
theorem exactZero_fiberSound (history : Symbolic.SampleEnv primitiveLaws n) (expression : AffineExpr n)
    (safe : SafeConfigAt primitiveLaws 0 history expression) :
    FiberSound (historyReplay 0 history safe.1 expression)
      (actualTraceLaw 0 history expression) (targetTraceLaw 0 history expression) := by
  by_cases value : expression.isValue = true
  · obtain ⟨affine, rfl⟩ := wellTyped_real_value safe.2.1 value
    let law := history.actualMeasure primitiveLaws
    let μ := law.map affine.eval
    have mass : law Set.univ = 1 := SymbolicSoundness.SampleEnv.actualMeasure_univ_eq_one _ history safe.1
    let : IsProbabilityMeasure law := ⟨mass⟩
    have replayEq (tape : Trace) :
        (historyReplay 0 history safe.1 (.real affine)).kernel tape = μ := by
      rw [historyReplay_apply]
      simp only [AffineExpr.realize, replayMeasure, exactOutputMeasure]
      change law.bind (fun env => Measure.dirac (affine.eval env)) = μ
      exact Measure.bind_dirac_eq_map _ (affine_eval_measurable affine)
    have sourceEq : actualTraceLaw 0 history (.real affine) =
        μ.map (fun x => (([] : Trace), x)) := by
      rw [actualTraceLaw]
      simp only [AffineExpr.realize, exactMeasure]
      change law.bind (fun env => Measure.dirac (([] : Trace), affine.eval env)) = _
      rw [Measure.bind_dirac_eq_map _ (measurable_const.prodMk (affine_eval_measurable affine))]
      dsimp only [μ]
      rw [Measure.map_map (show Measurable (fun x : ℝ => (([] : Trace),x)) from measurable_const.prodMk measurable_id) (affine_eval_measurable affine)]
      rfl
    have integrable := SymbolicSoundness.SampleEnv.integrable_affine primitiveLaws primitiveMomentBounds history safe.1 affine
    have mean := SymbolicSoundness.SampleEnv.integral_affine primitiveLaws primitiveMomentBounds history safe.1 affine
    have targetEq : targetTraceLaw 0 history (.real affine) = Measure.dirac (([] : Trace), affine.eval (history.meanEnvironment primitiveLaws)) := by
      simp only [targetTraceLaw, AffineExpr.realize, Expr.determinize, exactMeasure]
    rw [sourceEq, ← replayEq [], targetEq]
    apply FiberSound.terminal
    rw [FiberGood, replayEq]
    refine ⟨?_, ?_, ?_⟩
    · exact Measure.map_apply_of_aemeasurable (affine_eval_measurable affine).aemeasurable MeasurableSet.univ |>.trans (by simp [mass])
    · exact (integrable_map_measure measurable_id.aestronglyMeasurable (affine_eval_measurable affine).aemeasurable).2 integrable
    · exact (integral_map (affine_eval_measurable affine).aemeasurable aestronglyMeasurable_id).trans mean
  · have actualZero : actualTraceLaw 0 history expression = 0 := by
      unfold actualTraceLaw
      have h : ∀ env, exactMeasure 0 (expression.realize env) = 0 := by
        intro env
        cases expression <;> simp_all [AffineExpr.realize, exactMeasure, AffineExpr.isValue]
      simp_rw [h]
      simp
    have targetZero : targetTraceLaw 0 history expression = 0 := by
      have nv : (expression.realize (history.meanEnvironment primitiveLaws)).determinize.isValue ≠ true := by
        simpa only [determinize_isValue, AffineExpr.realize_isValue] using value
      unfold targetTraceLaw
      generalize eq : (expression.realize (history.meanEnvironment primitiveLaws)).determinize = e at nv ⊢
      cases e <;> try rfl
      simp_all [Expr.isValue]
    rw [actualZero, targetZero]
    exact FiberSound.zero _

set_option maxHeartbeats 1600000 in
theorem exactDepth_fiberSound (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (safe : SafeConfigAt primitiveLaws depth history expression) :
    FiberSound (historyReplay depth history safe.1 expression)
      (actualTraceLaw depth history expression) (targetTraceLaw depth history expression) := by
  induction depth generalizing n history expression with
  | zero => exact exactZero_fiberSound history expression safe
  | succ depth ih =>
      rcases safe with ⟨historySafe, typed, sourceSafe⟩
      by_cases value : expression.isValue = true
      · have actualZero : actualTraceLaw (depth+1) history expression = 0 := by
          unfold actualTraceLaw
          have h : ∀ env, exactMeasure (depth+1) (expression.realize env) = 0 := by
            intro env
            rw [exactMeasure, if_pos (by simpa only [AffineExpr.realize_isValue] using value)]
          simp_rw [h]
          simp
        have targetZero : targetTraceLaw (depth+1) history expression = 0 := by
          rw [targetTraceLaw, exactMeasure,
            if_pos (by simpa only [determinize_isValue, AffineExpr.realize_isValue] using value)]
        rw [actualZero, targetZero]
        exact FiberSound.zero _
      · have actionTyped := symbolicReduce_wellTyped primitiveLaws typed
        generalize actionEq : symbolicReduce primitiveLaws expression = action at actionTyped
        cases action with
        | next next =>
            have nextTyped : WellTyped [] next (.float .E) := SymbolicAction.wellTyped_next_iff.mp actionTyped
            have nextSafe : ∀ᵐ env ∂history.actualMeasure primitiveLaws,
                PrimitiveDomainSafeAt depth (next.realize env) := by
              filter_upwards [sourceSafe] with env valid
              have reduction : reduce (expression.realize env) = .next (next.realize env) := by
                rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
                rfl
              have nv : (expression.realize env).isValue ≠ true := by
                simpa only [AffineExpr.realize_isValue] using value
              simpa only [PrimitiveDomainSafeAt, Bool.eq_false_of_not_eq_true nv, Bool.false_eq_true,
                ↓reduceIte, reduction] using valid
            have sound := ih history next ⟨historySafe,nextTyped,nextSafe⟩
            rw [actualTraceLaw_next _ _ _ _ typed value actionEq,
              targetTraceLaw_next _ _ _ _ typed value actionEq]
            exact FiberSound.mapTrace _ _ _ _ sound (fun tape : Trace => (List.cons none tape : Trace)) (trace_cons_measurable.comp (measurable_const.prodMk measurable_id))
              (fun tape => by simpa using historyReplay_next depth history historySafe expression next typed value actionEq (List.cons none tape))
        | sampleE op affine general continuation =>
            let extended := Symbolic.SampleEnv.snoc history op
              (fun i => affine.getD i.1 (0,fun _ => 0)) (fun i => general.getD i.1 0)
            have extension := source_sampleE_safe_extension (MeasurableActionFamily.stepKernel primitiveLaws)
              depth history historySafe expression typed sourceSafe op affine general continuation actionEq
            have extendedSafe : extended.DomainSafe primitiveLaws := extension.1
            have sound := ih extended continuation extension
            rw [actualTraceLaw_sampleE _ _ _ typed value _ _ _ _ actionEq,
              targetTraceLaw_sampleE _ _ historySafe _ typed value _ _ _ _ actionEq extendedSafe]
            exact FiberSound.mapTrace _ _ _ _ sound (fun tape : Trace => (List.cons none tape : Trace)) (trace_cons_measurable.comp (measurable_const.prodMk measurable_id))
              (fun tape => by simpa [extended] using historyReplay_sampleE depth history historySafe expression typed value op affine general continuation actionEq extendedSafe (List.cons none tape))
        | sampleG site fiber continuation =>
            rcases source_sampleG_safe_swap (MeasurableActionFamily.stepKernel primitiveLaws) depth history historySafe
              expression typed sourceSafe fiber continuation actionEq with ⟨fiberMass, continuationSafe⟩
            obtain ⟨op, opEq⟩ := sampleG_opSome typed actionEq
            rw [actualTraceLaw_sampleG _ _ historySafe _ typed value _ _ fiberMass actionEq op opEq,
              targetTraceLaw_sampleG _ _ _ typed value _ _ actionEq op opEq]
            apply FiberSound.mix
            filter_upwards [continuationSafe] with v valid
            have continuationTyped := SymbolicAction.wellTyped_sampleG_iff.mp actionTyped v
            have sound := ih history (continuation v) ⟨historySafe,continuationTyped,valid⟩
            rw [generatedSourceKernel_apply _ _ _ _ typed actionEq, generatedTargetKernel_apply _ _ _ typed actionEq]
            exact FiberSound.mapTrace _ _ _ _ sound (fun tape : Trace => (List.cons (entry (some op) v) tape : Trace)) (trace_cons_measurable.comp (measurable_const.prodMk measurable_id))
              (fun tape => by simpa [entry, eventValue] using historyReplay_sampleG depth history historySafe expression typed value fiber continuation actionEq op opEq (List.cons (entry (some op) v) tape))
        | stuck => exact (SymbolicAction.not_wellTyped_stuck actionTyped).elim

end
end Determinize.Proof.StepTraces
