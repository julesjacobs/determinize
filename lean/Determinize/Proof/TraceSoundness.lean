import Determinize.Proof.SymbolicTraceSoundness

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety
open scoped ProbabilityTheory
noncomputable section
open Classical

def rawReplay (source : Expr) : Kernel Trace ℝ := by
  have measurableAt (depth : Nat) : Measurable (fun tape : Trace => replayMeasure depth source tape) := by
    have eq : (fun tape => replayMeasure depth source tape) =
        fun tape => (replayKernel depth).kernel (tape, source) :=
      funext fun tape => (replayKernel_apply _ _ _).symm
    rw [eq]
    exact (replayKernel depth).kernel.measurable.comp (measurable_id.prodMk measurable_const)
  have measurableReplay : Measurable (fun pair : Nat × Trace => replayMeasure pair.1 source pair.2) :=
    measurable_from_prod_countable_right measurableAt
  exact ⟨fun trace => replayMeasure trace.length source trace,
    measurableReplay.comp (trace_length_measurable.prodMk measurable_id)⟩

def normalizedReplay (source : Expr) : Kernel Trace ℝ :=
  Kernel.piecewise
    (measurableSet_eq_fun ((rawReplay source).measurable_coe MeasurableSet.univ) (measurable_const (a := (1 : ENNReal))))
    (rawReplay source) (Kernel.deterministic (fun _ => (0 : ℝ)) measurable_const)

theorem normalizedReplay_eq (source : Expr) (trace : Trace)
    (mass : rawReplay source trace Set.univ = 1) :
    normalizedReplay source trace = rawReplay source trace := by
  simp [normalizedReplay, Kernel.piecewise, mass]

instance normalizedReplay_markov (source : Expr) : IsMarkovKernel (normalizedReplay source) := by
  constructor
  intro trace
  by_cases mass : rawReplay source trace Set.univ = 1
  · rw [normalizedReplay_eq source trace mass]
    exact ⟨mass⟩
  · simp only [normalizedReplay, Kernel.piecewise, Kernel.coe_mk, Set.mem_ofPred_eq, if_neg mass, Kernel.deterministic_apply]
    infer_instance

def traceFiber (source : Expr) : SFiniteKernel Trace ℝ := ⟨normalizedReplay source, inferInstance⟩

theorem nilDomainSafe : (Symbolic.SampleEnv.nil : Symbolic.SampleEnv primitiveLaws 0).DomainSafe primitiveLaws := trivial

theorem historyReplay_nil (depth : Nat) (source : Expr) (tape : Trace)
    (length : tape.length = depth) :
    (historyReplay depth .nil nilDomainSafe (AffineExpr.ofExpr source)).kernel tape =
      rawReplay source tape := by
  rw [historyReplay_apply]
  change (Measure.dirac Env.empty).bind (fun env => replayMeasure depth ((AffineExpr.ofExpr source).realize env) tape) = _
  rw [Measure.dirac_bind (show Measurable (fun env => replayMeasure depth ((AffineExpr.ofExpr source).realize env) tape) from
    (replay_measurable depth tape).comp (AffineExpr.ofExpr source).realize_measurable), AffineExpr.realize_ofExpr]
  simp only [rawReplay, Kernel.coe_mk, length]

theorem exactDepth_source_fiberSound (source : Expr) (typed : Typed [] source (.float .E))
    (sourceTags : (AffineExpr.ofExpr source).SourceTags) (sourceSafe : PrimitiveDomainSafe source) (depth : Nat) :
    FiberSound (historyReplay depth .nil nilDomainSafe (AffineExpr.ofExpr source))
      (exactMeasure depth source) (exactMeasure depth source.determinize) := by
  have symbolicTyped := AffineExpr.wellTyped_ofExpr_of_typed typed sourceTags
  have safe : SafeConfigAt primitiveLaws depth .nil (AffineExpr.ofExpr source) := by
    refine ⟨trivial, symbolicTyped, ?_⟩
    rw [Symbolic.SampleEnv.actualMeasure, ae_dirac_eq]
    simpa only [Filter.eventually_pure, AffineExpr.realize_ofExpr] using sourceSafe depth
  have sound := exactDepth_fiberSound depth .nil (AffineExpr.ofExpr source) safe
  have actualEq : actualTraceLaw depth .nil (AffineExpr.ofExpr source) = exactMeasure depth source := by
    unfold actualTraceLaw
    change (Measure.dirac Env.empty).bind (fun env => exactMeasure depth ((AffineExpr.ofExpr source).realize env)) = _
    rw [Measure.dirac_bind (show Measurable (fun env => exactMeasure depth ((AffineExpr.ofExpr source).realize env)) from
      (exact_measurable depth).comp (AffineExpr.ofExpr source).realize_measurable), AffineExpr.realize_ofExpr]
  have targetEq : targetTraceLaw depth .nil (AffineExpr.ofExpr source) = exactMeasure depth source.determinize := by
    simp only [targetTraceLaw, Symbolic.SampleEnv.meanEnvironment, AffineExpr.realize_ofExpr]
  rwa [actualEq, targetEq] at sound

theorem joint_fiberSound (source : Expr) (typed : Typed [] source (.float .E))
    (sourceTags : (AffineExpr.ofExpr source).SourceTags) (sourceSafe : PrimitiveDomainSafe source) :
    FiberSound (traceFiber source) (jointMeasure source) (jointMeasure source.determinize) := by
  apply FiberSound.sum
  intro depth
  have sound := exactDepth_source_fiberSound source typed sourceTags sourceSafe depth
  have transported := FiberSound.mapTrace_ae _ (traceFiber source) _ _ sound id measurable_id (by
    filter_upwards [sound.2, exact_length depth source.determinize] with point good length
    rw [historyReplay_nil depth source point.1 length]
    exact normalizedReplay_eq source point.1
      (by simpa only [historyReplay_nil depth source point.1 length] using good.1))
  simpa only [show mapTraceOutput (id : Trace → Trace) = id from rfl,
    Measure.map_id] using transported

theorem output_mass_le_one (source : Expr) :
    Determinize.Statement.Paper.bigStepMeasure source Set.univ ≤ 1 := by
  let step := MeasurableActionFamily.stepKernel primitiveLaws
  rw [← Determinize.Proof.Paper.bigStepMeasure_eq,
    MeasurableActionFamily.exactDepthConstruction step source,
    Measure.sum_apply _ MeasurableSet.univ, ENNReal.tsum_eq_iSup_nat]
  apply iSup_le
  intro fuel
  cases fuel with
  | zero => simp
  | succ fuel =>
      rw [← Measure.finsetSum_apply _ _ Set.univ,
        ← MeasurableActionFamily.cumulativeOutputMeasure_eq_finsetSum]
      unfold Determinize.Proof.Paper.cumulativeOutputMeasure
      rw [Measure.map_apply terminalFloatValue_measurable MeasurableSet.univ]
      simp only [Set.preimage_univ, Measure.restrict_apply_univ]
      exact (measure_mono (Set.subset_univ _)).trans (nStepMeasure_mass_le_one step fuel source)

theorem joint_mass_le_one (source : Expr) : jointMeasure source Set.univ ≤ 1 := by
  have eq := correspondence source
  have mass := congrArg (fun μ : Measure ℝ => μ Set.univ) eq
  rw [Measure.map_apply measurable_snd MeasurableSet.univ] at mass
  exact mass.le.trans (output_mass_le_one source)

theorem meanOnTraces_of_fiberSound (source target : Expr) (fiber : SFiniteKernel Trace ℝ)
    [IsMarkovKernel fiber.kernel] (sound : FiberSound fiber (jointMeasure source) (jointMeasure target)) :
    MeanOnTraces source target  := by
  rcases sound.factorization (joint_mass_le_one target) with ⟨ν,f,hm,hf,hs,ht,hmean⟩
  exact ⟨ν,fiber.kernel,f,hm,inferInstance,hf,hs,ht,hmean⟩

theorem soundness : Determinize.Proof.StepTraces.soundnessThm := by
  intro source typed sourceForm sourceSafe
  have tags := sourceTags_of_sourceForm sourceForm
  have domainSafe := (Typing.primitiveDomainSafe_iff_doesNotGetStuck typed).2 sourceSafe
  have targetSafe := determinize_primitiveDomainSafe_of_typed_source primitiveLaws
    (MeasurableActionFamily.stepKernel primitiveLaws) source typed tags domainSafe
  refine ⟨(Typing.primitiveDomainSafe_iff_doesNotGetStuck (typed_determinize typed)).1 targetSafe, ?_⟩
  let : IsMarkovKernel (traceFiber source).kernel := normalizedReplay_markov source
  exact meanOnTraces_of_fiberSound source source.determinize (traceFiber source)
    (joint_fiberSound source typed tags domainSafe)

end
end Determinize.Proof.StepTraces
