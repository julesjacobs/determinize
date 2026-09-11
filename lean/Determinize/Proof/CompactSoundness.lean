import Determinize.Proof.CompactFiberSoundness
import Determinize.Proof.TraceFactorization

/-!
# Compact trace soundness

The detailed step-trace results are transported to compact traces of general-affinity draws, with
`Spec.Traces.outputGivenTrace` as the fiber: the Markov version `normalizedOutputGivenTrace` factors
the joint laws, and almost surely it agrees with `outputGivenTrace` on the source, while on the
target `outputGivenTrace` is the Dirac mass at the target output.
-/

namespace Determinize.Proof.Traces
open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Spec.Traces
open Determinize.Proof.Paper
open StepTraces (retain retain_measurable FiberSound mapTraceOutput mapTrace_measurable
  normalizedOutputGivenTrace normalizedOutputGivenTrace_eq outputGivenTraceKernel
  outputGivenTraceKernel_apply outputGivenTrace_eq_ogtAt
  compactFiber_apply compactHistoryReplay_nil measurableSet_kernel_eq_dirac)
open scoped ProbabilityTheory
noncomputable section

abbrev eraseOutput : StepTraces.Output → Output := mapTraceOutput retain

theorem eraseOutput_measurable : Measurable eraseOutput :=
  mapTrace_measurable retain retain_measurable

theorem record_measurable (site : DistributionAction × Op) (value : ℝ) : Measurable (record site value) := by
  rcases site with ⟨kind, op⟩
  cases kind with
  | sample affinity =>
      cases affinity with
      | E => exact measurable_id
      | G => exact (StepTraces.draw_cons_measurable.comp
          (measurable_const.prodMk measurable_fst)).prodMk measurable_snd
  | mean => exact measurable_id

theorem exact_eq_detailed (depth : Nat) (e : Expr) :
    traceAndOutputLawAt depth e = (StepTraces.exactMeasure depth e).map eraseOutput := by
  induction depth generalizing e with
  | zero =>
      cases e <;> try simp [traceAndOutputLawAt, StepTraces.exactMeasure]
      case real r =>
        simp [Measure.map_dirac' eraseOutput_measurable, eraseOutput, mapTraceOutput, retain]
  | succ depth ih =>
      by_cases value : e.isValue = true
      · simp [traceAndOutputLawAt, StepTraces.exactMeasure, value]
      · rw [traceAndOutputLawAt, StepTraces.exactMeasure, if_neg value, if_neg value]
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
            rcases site with ⟨kind, op⟩
            cases kind with
            | sample affinity => cases affinity <;> rfl
            | mean => rfl

theorem joint_eq_detailed (e : Expr) :
    traceAndOutputLaw e = (StepTraces.jointMeasure e).map eraseOutput := by
  rw [traceAndOutputLaw, StepTraces.jointMeasure, Measure.map_sum eraseOutput_measurable.aemeasurable]
  simp_rw [exact_eq_detailed]

theorem correspondence : Determinize.Spec.Traces.correspondenceThm := by
  intro e
  rw [joint_eq_detailed, Measure.map_map measurable_snd eraseOutput_measurable]
  exact StepTraces.correspondence e

theorem joint_mass_le_one (e : Expr) : traceAndOutputLaw e Set.univ ≤ 1 := by
  rw [joint_eq_detailed, Measure.map_apply eraseOutput_measurable MeasurableSet.univ]
  exact StepTraces.joint_mass_le_one e

theorem traceLaw_mass_le_one (e : Expr) : traceLaw e Set.univ ≤ 1 := by
  rw [traceLaw, Measure.map_apply measurable_fst MeasurableSet.univ]
  exact joint_mass_le_one e

instance (e : Expr) : IsFiniteMeasure (traceLaw e) :=
  ⟨(traceLaw_mass_le_one e).trans_lt (by simp)⟩

theorem exact_succ_next (depth : Nat) (e next : Expr) (nv : e.isValue ≠ true)
    (h : reduce e = .next next) : traceAndOutputLawAt (depth+1) e = traceAndOutputLawAt depth next := by
  simp [traceAndOutputLawAt, nv, h]



/-! ### The compact replay as the fiber -/

/-- `Spec.Traces.outputGivenTrace` normalized to a Markov kernel, as an s-finite kernel. -/
def normalizedKernel (source : Expr) : SFiniteKernel Trace ℝ :=
  ⟨normalizedOutputGivenTrace source, inferInstance⟩

instance normalizedKernel_markov (source : Expr) : IsMarkovKernel (normalizedKernel source).kernel :=
  inferInstanceAs (IsMarkovKernel (normalizedOutputGivenTrace source))

/-- The normalized compact replay read through the retained draws of a detailed trace. -/
def normalizedFiber (source : Expr) : SFiniteKernel StepTraces.Trace ℝ :=
  SFiniteKernel.pullback (normalizedKernel source) retain retain_measurable

theorem selfReplay_compact_measurable (target : Expr) :
    MeasurableSet {q : Output | outputGivenTrace target q.1 = Measure.dirac q.2} := by
  let κ := SFiniteKernel.pullback ⟨outputGivenTraceKernel target, inferInstance⟩
    (Prod.fst : Output → Trace) measurable_fst
  have := κ.sfinite
  have eq : {q : Output | outputGivenTrace target q.1 = Measure.dirac q.2} =
      {q | κ.kernel q = Measure.dirac q.2} := by
    ext q
    simp only [Set.mem_ofPred_eq, κ, MeasurableActionFamily.pullback_apply,
      outputGivenTraceKernel_apply]
  rw [eq]
  exact measurableSet_kernel_eq_dirac κ.kernel measurable_snd

theorem massOne_compact_measurable (source : Expr) :
    MeasurableSet {q : Output | outputGivenTrace source q.1 Set.univ = 1} := by
  have eq : {q : Output | outputGivenTrace source q.1 Set.univ = 1} =
      Prod.fst ⁻¹' {trace | outputGivenTraceKernel source trace Set.univ = 1} := by
    ext q
    simp only [Set.mem_ofPred_eq, Set.mem_preimage, outputGivenTraceKernel_apply]
  rw [eq]
  exact (measurableSet_eq_fun ((outputGivenTraceKernel source).measurable_coe MeasurableSet.univ)
    measurable_const).preimage measurable_fst

/-- On the detailed joint laws, the normalized compact replay is a sound fiber. -/
theorem joint_normalized_fiberSound (source : Expr) (typed : Typed [] source (.float .E))
    (tags : (Symbolic.AffineExpr.ofExpr source).SourceTags) (safe : PrimitiveDomainSafe source) :
    FiberSound (normalizedFiber source) (StepTraces.jointMeasure source)
      (StepTraces.jointMeasure source.determinize) := by
  apply StepTraces.FiberSound.sum
  intro depth
  have sound := StepTraces.compact_exactDepth_source_fiberSound source typed tags safe depth
  have transported := StepTraces.FiberSound.mapTrace_ae _ (normalizedFiber source) _ _ sound id
    measurable_id (by
      filter_upwards [sound.2] with point good
      have massOne : outputGivenTraceAt depth source (retain point.1) Set.univ = 1 := by
        have h := good.1
        rwa [compactFiber_apply, compactHistoryReplay_nil] at h
      show (normalizedFiber source).kernel point.1 = _
      rw [normalizedFiber, MeasurableActionFamily.pullback_apply, compactFiber_apply,
        compactHistoryReplay_nil]
      change normalizedOutputGivenTrace source (retain point.1) = _
      rw [normalizedOutputGivenTrace_eq _ _ (by
        rw [outputGivenTrace_eq_ogtAt depth source _ massOne]
        exact massOne), outputGivenTrace_eq_ogtAt depth source _ massOne])
  simpa only [show mapTraceOutput (id : StepTraces.Trace → StepTraces.Trace) = id from rfl,
    Measure.map_id] using transported

/-- On the compact joint laws, the normalized compact replay is a sound fiber. -/
theorem compact_normalized_fiberSound (source : Expr) (typed : Typed [] source (.float .E))
    (tags : (Symbolic.AffineExpr.ofExpr source).SourceTags) (safe : PrimitiveDomainSafe source) :
    FiberSound (normalizedKernel source) (traceAndOutputLaw source) (traceAndOutputLaw source.determinize) := by
  rw [joint_eq_detailed, joint_eq_detailed]
  exact StepTraces.FiberSound.mapTrace _ _ _ _ (joint_normalized_fiberSound source typed tags safe)
    retain retain_measurable
    (fun trace => by rw [normalizedFiber, MeasurableActionFamily.pullback_apply])

/-- Almost every terminating target trace gives the source replay mass one. -/
theorem compact_source_massOne (source : Expr) (typed : Typed [] source (.float .E))
    (tags : (Symbolic.AffineExpr.ofExpr source).SourceTags) (safe : PrimitiveDomainSafe source) :
    ∀ᵐ q ∂traceAndOutputLaw source.determinize, outputGivenTrace source q.1 Set.univ = 1 := by
  rw [joint_eq_detailed, ae_map_iff eraseOutput_measurable.aemeasurable
    (massOne_compact_measurable source), StepTraces.jointMeasure, Measure.ae_sum_iff]
  intro depth
  filter_upwards [(StepTraces.compact_exactDepth_source_fiberSound source typed tags safe depth).2]
    with point good
  have massOne : outputGivenTraceAt depth source (retain point.1) Set.univ = 1 := by
    have h := good.1
    rwa [compactFiber_apply, compactHistoryReplay_nil] at h
  show outputGivenTrace source (retain point.1) Set.univ = 1
  rw [outputGivenTrace_eq_ogtAt depth source _ massOne]
  exact massOne

/-- Replaying the target along its own compact trace returns its output. -/
theorem compact_target_selfReplay (source : Expr) (typed : Typed [] source (.float .E))
    (tags : (Symbolic.AffineExpr.ofExpr source).SourceTags) (safe : PrimitiveDomainSafe source) :
    ∀ᵐ q ∂traceAndOutputLaw source.determinize,
      outputGivenTrace source.determinize q.1 = Measure.dirac q.2 := by
  rw [joint_eq_detailed, ae_map_iff eraseOutput_measurable.aemeasurable
    (selfReplay_compact_measurable source.determinize), StepTraces.jointMeasure,
    Measure.ae_sum_iff]
  intro depth
  filter_upwards [StepTraces.target_selfReplay_source source typed tags safe depth] with point hp
  show outputGivenTrace source.determinize (retain point.1) = Measure.dirac point.2
  rw [outputGivenTrace_eq_ogtAt depth _ _ (by rw [hp]; simp), hp]

/-! ### The factorization with its almost-sure identifications -/

/-- Trace soundness for an expectation-affinity source, as a factorization by the normalized
compact replay together with the identifications that give the public statement: almost
surely the source fiber is `outputGivenTrace` itself, and the target replay is the Dirac mass
at the target output. -/
theorem soundnessDataE (source : Expr) (typed : Typed [] source (.float .E))
    (sourceForm : source.sourceForm = true) (safe : DoesNotGetStuck source) :
    DoesNotGetStuck source.determinize ∧
      TraceFactorization source source.determinize (normalizedOutputGivenTrace source)
        (kernelMean (normalizedOutputGivenTrace source)) ∧
      (∀ᵐ trace ∂traceLaw source,
        outputGivenTrace source trace = normalizedOutputGivenTrace source trace) ∧
      ∀ᵐ trace ∂traceLaw source,
        outputGivenTrace source.determinize trace = Measure.dirac (kernelMean (normalizedOutputGivenTrace source) trace) := by
  have tags := sourceTags_of_sourceForm sourceForm
  have domainSafe := (Typing.primitiveDomainSafe_iff_doesNotGetStuck typed).2 safe
  refine ⟨(StepTraces.soundness source typed sourceForm safe).1, ?_⟩
  let ν := (traceAndOutputLaw source.determinize).map Prod.fst
  let f := kernelMean (normalizedOutputGivenTrace source)
  rcases (compact_normalized_fiberSound source typed tags domainSafe).factorization
    (joint_mass_le_one source.determinize) with ⟨hm, hf, hs, ht, hmean⟩
  have hν : traceLaw source = ν := by
    let : IsFiniteMeasure ν := ⟨hm.trans_lt (by simp)⟩
    rw [traceLaw, hs]
    exact Measure.fst_compProd ν (normalizedKernel source).kernel
  change _ = ν ⊗ₘ _ at hs
  change _ = ν.map (fun trace => (trace, f trace)) at ht
  change ∀ᵐ trace ∂ν, _ at hmean
  rw [← hν] at hs ht hmean
  have pairMeasurable : Measurable (fun trace : Trace => (trace, f trace)) :=
    measurable_id.prodMk hf
  refine ⟨⟨inferInstance, hf, hs, ht, hmean⟩, ?_, ?_⟩
  · have massOne := compact_source_massOne source typed tags domainSafe
    rw [ht] at massOne
    filter_upwards [ae_of_ae_map pairMeasurable.aemeasurable massOne] with trace mass
    exact (normalizedOutputGivenTrace_eq source trace mass).symm
  · have dirac := compact_target_selfReplay source typed tags domainSafe
    rw [ht] at dirac
    exact ae_of_ae_map pairMeasurable.aemeasurable dirac

/-- Apply expectation-affinity soundness using silent subtyping for general-affinity programs. -/
theorem soundnessData (affinity : Affinity) (program : Expr) (typed : Typed [] program (.float affinity))
    (sourceForm : program.sourceForm = true) (safe : DoesNotGetStuck program) :
    DoesNotGetStuck program.determinize ∧
      TraceFactorization program program.determinize (normalizedOutputGivenTrace program)
        (kernelMean (normalizedOutputGivenTrace program)) ∧
      (∀ᵐ trace ∂traceLaw program,
        outputGivenTrace program trace = normalizedOutputGivenTrace program trace) ∧
      ∀ᵐ trace ∂traceLaw program,
        outputGivenTrace program.determinize trace = Measure.dirac (kernelMean (normalizedOutputGivenTrace program) trace) := by
  cases affinity with
  | E => exact soundnessDataE program typed sourceForm safe
  | G => exact soundnessDataE program (.sub typed .general) sourceForm safe

/-- Some trace factorization exists: the input of the corollaries. -/
theorem meanOnTraces (affinity : Affinity) (program : Expr) (typed : Typed [] program (.float affinity))
    (sourceForm : program.sourceForm = true) (safe : DoesNotGetStuck program) :
    DoesNotGetStuck program.determinize ∧ MeanOnTraces program program.determinize :=
  let ⟨targetSafe, factor, _, _⟩ := soundnessData affinity program typed sourceForm safe
  ⟨targetSafe, _, factor⟩

/-- A measure composed with a kernel is the bind that pairs each point with its draw. -/
theorem compProd_eq_traceThenOutput (traces : Measure Trace) [SFinite traces]
    (fiber : Kernel Trace ℝ) [IsSFiniteKernel fiber] :
    traces ⊗ₘ fiber = traceThenOutput traces fiber := by
  let paired := SFiniteKernel.mapWithInput ⟨fiber, inferInstance⟩ id measurable_id
  have pairedEq (trace : Trace) :
      paired.kernel trace = (fiber trace).map (fun value => (trace, value)) := by
    rw [SymbolicSoundness.TargetSafety.sfiniteKernel_mapWithInput_apply]
    rfl
  have eq : traceThenOutput traces fiber = traces.bind paired.kernel := by
    rw [traceThenOutput]
    congr 1
    funext trace
    rw [pairedEq]
  rw [eq]
  ext set hs
  rw [Measure.bind_apply hs paired.kernel.aemeasurable, Measure.compProd_apply hs]
  apply lintegral_congr
  intro trace
  rw [pairedEq, Measure.map_apply (show Measurable (fun value : ℝ => (trace, value)) from
    measurable_const.prodMk measurable_id) hs]

/-- Determinization returns the canonical replay mean on each terminating trace. -/
theorem targetLaw (program : Expr) (typed : Typed [] program (.float .E))
    (sourceForm : program.sourceForm = true) (safe : DoesNotGetStuck program) :
    traceAndOutputLaw program.determinize =
      (traceLaw program).map (fun trace => (trace, replayMean program trace)) := by
  obtain ⟨_, factor, sameFiber, _⟩ := soundnessDataE program typed sourceForm safe
  refine factor.2.2.2.1.trans (Measure.map_congr ?_)
  filter_upwards [sameFiber] with trace same
  simp only [kernelMean, replayMean, same]

/-- The public trace soundness theorem. -/
theorem soundness : Determinize.Spec.Traces.soundnessThm := by
  intro program typed sourceForm safe
  let f := kernelMean (normalizedOutputGivenTrace program)
  obtain ⟨targetSafe, factor, massAe, diracAe⟩ :=
    soundnessData .E program typed sourceForm safe
  obtain ⟨markov, hf, hs, ht, hmean⟩ := factor
  have := markov
  refine ⟨targetSafe, ?_, ?_, ?_⟩
  · rw [hs, compProd_eq_traceThenOutput, traceThenOutput, traceThenOutput]
    exact Measure.bind_congr_right (massAe.mono fun trace h => by simp only [h])
  · rw [ht, traceThenOutput, ← Measure.bind_dirac_eq_map _
      (show Measurable (fun trace : Trace => (trace, f trace)) from measurable_id.prodMk hf)]
    apply Measure.bind_congr_right
    filter_upwards [diracAe] with trace h
    rw [h, Measure.map_dirac' (show Measurable (fun value : ℝ => (trace, value)) from
      measurable_const.prodMk measurable_id)]
  · filter_upwards [hmean, massAe, diracAe] with trace mean mass dirac
    simp only [replayMean, mass]
    exact ⟨mean.1, by rw [dirac, mean.2]⟩

end
end Determinize.Proof.Traces
