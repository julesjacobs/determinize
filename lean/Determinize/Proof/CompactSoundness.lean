import Determinize.Proof.CompactTrace

namespace Determinize.Proof.Traces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Traces
open Determinize.Proof.Paper
open StepTraces (retain retain_measurable decode decode_measurable FiberSound mapTraceOutput
  mapTrace_measurable)
open scoped ProbabilityTheory
noncomputable section

abbrev eraseOutput : StepTraces.Output → Output := mapTraceOutput retain

theorem eraseOutput_measurable : Measurable eraseOutput :=
  mapTrace_measurable retain retain_measurable

theorem record_measurable (site : Mode × Tag) (value : ℝ) : Measurable (record site value) := by
  rcases site with ⟨mode, tag⟩
  cases mode <;> cases tag <;> try exact measurable_id
  exact (StepTraces.draw_cons_measurable.comp
    (measurable_const.prodMk measurable_fst)).prodMk measurable_snd

theorem exact_eq_detailed (depth : Nat) (e : Expr) :
    exactMeasure depth e = (StepTraces.exactMeasure depth e).map eraseOutput := by
  induction depth generalizing e with
  | zero =>
      cases e <;> try simp [exactMeasure, StepTraces.exactMeasure]
      case real r =>
        simp [Measure.map_dirac' eraseOutput_measurable, eraseOutput, mapTraceOutput, retain]
  | succ depth ih =>
      by_cases value : e.isValue = true
      · simp [exactMeasure, StepTraces.exactMeasure, value]
      · rw [exactMeasure, StepTraces.exactMeasure, if_neg value, if_neg value]
        cases reduction : reduce e with
        | stuck => simp
        | next next =>
            simp only [ih]
            rw [Measure.map_map eraseOutput_measurable
              (show Measurable (StepTraces.prepend none) from
                StepTraces.prepend_measurable.comp (measurable_const.prodMk measurable_id))]
            congr 1
        | sample site fiber cont =>
            have hc := (MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable
              e fiber cont reduction
            have hm := (StepTraces.successorKernel (StepTraces.exactKernel depth)).kernel.measurable.comp
              ((StepTraces.generationEvent_measurable site).prodMk hc)
            simp only [Function.comp_def] at hm
            simp_rw [StepTraces.successorKernel_apply, StepTraces.exactKernel_apply] at hm
            rw [StepTraces.map_bind_fun _ _ hm _ eraseOutput_measurable]
            apply Measure.bind_congr_right
            filter_upwards [] with r
            rw [ih, Measure.map_map (record_measurable site r) eraseOutput_measurable,
              Measure.map_map eraseOutput_measurable
                (show Measurable (StepTraces.prepend (StepTraces.generationEvent site r)) from
                  StepTraces.prepend_measurable.comp (measurable_const.prodMk measurable_id))]
            congr 1
            funext p
            rcases site with ⟨mode, tag⟩
            cases mode <;> cases tag <;> rfl

theorem joint_eq_detailed (e : Expr) :
    jointMeasure e = (StepTraces.jointMeasure e).map eraseOutput := by
  rw [jointMeasure, StepTraces.jointMeasure, Measure.map_sum eraseOutput_measurable.aemeasurable]
  simp_rw [exact_eq_detailed]

theorem correspondence : Determinize.Traces.correspondenceThm := by
  intro e
  rw [joint_eq_detailed, Measure.map_map measurable_snd eraseOutput_measurable]
  exact StepTraces.correspondence e

theorem joint_mass_le_one (e : Expr) : jointMeasure e Set.univ ≤ 1 := by
  rw [joint_eq_detailed, Measure.map_apply eraseOutput_measurable MeasurableSet.univ]
  exact StepTraces.joint_mass_le_one e

theorem traceLaw_mass_le_one (e : Expr) : traceLaw e Set.univ ≤ 1 := by
  rw [traceLaw, Measure.map_apply measurable_fst MeasurableSet.univ]
  exact joint_mass_le_one e

instance (e : Expr) : IsFiniteMeasure (traceLaw e) :=
  ⟨(traceLaw_mass_le_one e).trans_lt (by simp)⟩

theorem exact_succ_next (depth : Nat) (e next : Expr) (nv : e.isValue ≠ true)
    (h : reduce e = .next next) : exactMeasure (depth+1) e = exactMeasure depth next := by
  simp [exactMeasure, nv, h]

/-- The promotion step is invisible to compact traces: it records no draw and produces no
output of its own. -/
theorem exact_succ_promote (depth : Nat) (e : Expr) :
    exactMeasure (depth + 1) (.promote e) = exactMeasure depth e := by
  induction depth generalizing e with
  | zero =>
      by_cases value : e.isValue = true
      · conv_lhs => rw [exactMeasure]
        rw [if_neg (by simp [Expr.isValue]), MeasurableActionFamily.reduce_promote_eq,
          if_pos value]
        cases e <;> simp [exactMeasure, Expr.isValue] at value ⊢
      · have rhs : exactMeasure 0 e = 0 := by
          cases e <;> simp_all [exactMeasure, Expr.isValue]
        have valueFalse := Bool.eq_false_of_not_eq_true value
        rw [rhs]
        conv_lhs => rw [exactMeasure]
        rw [if_neg (by simp [Expr.isValue]), MeasurableActionFamily.reduce_promote_eq,
          valueFalse]
        simp only [Bool.false_eq_true, ↓reduceIte]
        cases reduce e <;>
          simp [exactMeasure, Action.wrap, Measure.map_zero]
  | succ depth ih =>
      by_cases value : e.isValue = true
      · have rhs : exactMeasure (depth + 1) e = 0 := by rw [exactMeasure, if_pos value]
        rw [rhs]
        conv_lhs => rw [exactMeasure]
        rw [if_neg (by simp [Expr.isValue]), MeasurableActionFamily.reduce_promote_eq,
          if_pos value]
        cases e <;> simp [exactMeasure, Expr.isValue] at value ⊢
      · have valueFalse := Bool.eq_false_of_not_eq_true value
        conv_lhs => rw [exactMeasure]
        conv_rhs => rw [exactMeasure]
        rw [if_neg (by simp [Expr.isValue]), MeasurableActionFamily.reduce_promote_eq,
          valueFalse]
        simp only [Bool.false_eq_true, ↓reduceIte]
        cases reduce e <;> simp [Action.wrap, ih]

theorem jointMeasure_promote (e : Expr) : jointMeasure (.promote e) = jointMeasure e := by
  ext s hs
  simp only [jointMeasure, Measure.sum_apply _ hs]
  rw [show (∑' depth, exactMeasure depth (Expr.promote e) s) =
      exactMeasure 0 (Expr.promote e) s + ∑' depth, exactMeasure (depth + 1) (Expr.promote e) s
    from tsum_eq_zero_add' ENNReal.summable]
  simp only [exact_succ_promote]
  have zero : exactMeasure 0 (.promote e) s = 0 := by simp [exactMeasure]
  rw [zero, zero_add]

def traceFiber (source : Expr) : SFiniteKernel Trace ℝ :=
  SFiniteKernel.pullback (StepTraces.traceFiber source) (decode source.determinize)
    (decode_measurable source.determinize)

theorem traceFiber_apply (source : Expr) (t : Trace) :
    (traceFiber source).kernel t = (StepTraces.traceFiber source).kernel (decode source.determinize t) :=
  MeasurableActionFamily.pullback_apply _ _ _ _

instance traceFiber_markov (source : Expr) : IsMarkovKernel (traceFiber source).kernel := by
  constructor
  intro t
  rw [traceFiber_apply]
  let := StepTraces.normalizedReplay_markov source
  change IsProbabilityMeasure (StepTraces.normalizedReplay source _)
  infer_instance

theorem joint_fiberSound (source : Expr) (typed : Typed [] source (.float .E))
    (tags : (Symbolic.AffineExpr.ofExpr source).SourceTags) (safe : PrimitiveDomainSafe source) :
    FiberSound (traceFiber source) (jointMeasure source) (jointMeasure source.determinize) := by
  rw [joint_eq_detailed, joint_eq_detailed]
  apply StepTraces.FiberSound.mapTrace_ae _ _ _ _
    (StepTraces.joint_fiberSound source typed tags safe) retain retain_measurable
  filter_upwards [StepTraces.target_decode source typed tags safe] with p hp
  rw [traceFiber_apply, hp]

theorem soundness : Determinize.Traces.soundnessThm := by
  intro mode program typed sourceForm sourceSafe
  -- A general-mode program is observed through an explicit promotion, which compact traces
  -- do not see, so both modes reduce to the expectation-mode argument.
  obtain ⟨source, sourceTyped, sourceHasSourceForm, sourceIsSafe, jointEq, jointTargetEq,
      targetSafe⟩ : ∃ source : Expr, Typed [] source (.float .E) ∧ source.sourceForm = true ∧
        DoesNotGetStuck source ∧ jointMeasure source = jointMeasure program ∧
        jointMeasure source.determinize = jointMeasure program.determinize ∧
        (DoesNotGetStuck source.determinize → DoesNotGetStuck program.determinize) := by
    cases mode with
    | E => exact ⟨program, typed, sourceForm, sourceSafe, rfl, rfl, id⟩
    | G =>
        have targetEq : (Expr.promote program).determinize = .promote program.determinize := by
          simp only [Expr.determinize]
        refine ⟨.promote program, .promote typed, by simpa [Expr.sourceForm] using sourceForm,
          (Typing.doesNotGetStuck_promote_iff typed).2 sourceSafe, jointMeasure_promote program,
          by rw [targetEq]; exact jointMeasure_promote program.determinize, fun safe fuel => ?_⟩
        rw [targetEq] at safe
        exact Typing.doesNotGetStuckAt_of_promote (safe fuel)
  have tags := sourceTags_of_sourceForm sourceHasSourceForm
  have domainSafe := (Typing.primitiveDomainSafe_iff_doesNotGetStuck sourceTyped).2 sourceIsSafe
  refine ⟨targetSafe (StepTraces.soundness source sourceTyped sourceHasSourceForm sourceIsSafe).1, ?_⟩
  rcases (joint_fiberSound source sourceTyped tags domainSafe).factorization
    (joint_mass_le_one source.determinize) with ⟨ν, f, hm, hf, hs, ht, hmean⟩
  rw [jointEq] at hs
  rw [jointTargetEq] at ht
  have hν : traceLaw program = ν := by
    let : IsFiniteMeasure ν := ⟨hm.trans_lt (by simp)⟩
    rw [traceLaw, hs]
    exact Measure.fst_compProd ν (traceFiber source).kernel
  subst hν
  exact ⟨(traceFiber source).kernel, f, inferInstance, hf, hs, ht, hmean⟩

end
end Determinize.Proof.Traces
