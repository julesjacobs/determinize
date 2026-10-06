import Determinize.Proof.Traces.CompactFiberSoundness
import Determinize.Proof.Traces.Factorization
import Determinize.Proof.Traces.Mass

/-!
# Compact trace soundness

The detailed step-trace results are transported to compact traces of general-affinity draws, with
`Proof.Traces.outputGivenTrace` as the fiber: the Markov version `normalizedOutputGivenTrace`
factors the joint laws, and almost surely it agrees with `outputGivenTrace` on the source, while
on the target `outputGivenTrace` is the Dirac mass at the target output.
-/

namespace Determinize.Proof.Traces
open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Spec.Traces
open Determinize.Proof.Paper
open StepTraces (retain measurable_retain FiberSound mapTraceOutput measurable_mapTraceOutput
  normalizedOutputGivenTrace normalizedOutputGivenTrace_eq outputGivenTraceKernel
  outputGivenTraceKernel_apply outputGivenTrace_eq_outputGivenTraceAt
  compactFiber_apply compactHistoryReplay_nil measurableSet_kernel_eq_dirac)
open scoped ProbabilityTheory
noncomputable section

theorem joint_eq_detailed (e : Expr) :
    traceAndOutputLaw e = (StepTraces.jointMeasure e).map eraseOutput := by
  rw [traceAndOutputLaw, StepTraces.jointMeasure,
    Measure.map_sum measurable_eraseOutput.aemeasurable]
  simp_rw [exact_eq_detailed]

theorem correspondence (e : Expr) :
    (traceAndOutputLaw e).map Prod.snd = Determinize.Spec.Paper.bigStepMeasure e := by
  rw [joint_eq_detailed, Measure.map_map measurable_snd measurable_eraseOutput]
  exact StepTraces.correspondence e

theorem exact_succ_next (depth : Nat) (e next : Expr) (nv : e.isValue ≠ true)
    (h : reduce e = .next next) :
    traceAndOutputLawAt (depth + 1) e = traceAndOutputLawAt depth next := by
  simp [traceAndOutputLawAt, nv, h]

/-! ### The compact replay as the fiber -/

/-- `Proof.Traces.outputGivenTrace` normalized to a Markov kernel, as an s-finite kernel. -/
def normalizedKernel (source : Expr) : SFiniteKernel Trace ℝ :=
  ⟨normalizedOutputGivenTrace source, inferInstance⟩

instance normalizedKernel_markov (source : Expr) :
    IsMarkovKernel (normalizedKernel source).kernel :=
  inferInstanceAs (IsMarkovKernel (normalizedOutputGivenTrace source))

/-- The normalized compact replay read through the retained draws of a detailed trace. -/
def normalizedFiber (source : Expr) : SFiniteKernel StepTraces.Trace ℝ :=
  SFiniteKernel.pullback (normalizedKernel source) retain measurable_retain

theorem measurableSet_selfReplay_compact (target : Expr) :
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

theorem measurableSet_massOne_compact (source : Expr) :
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
    (safe : DomainSafe source) :
    FiberSound (normalizedFiber source) (StepTraces.jointMeasure source)
      (StepTraces.jointMeasure source.determinize) := by
  apply StepTraces.FiberSound.sum
  intro depth
  have sound := StepTraces.compact_exactDepth_source_fiberSound source typed safe depth
  have transported := StepTraces.FiberSound.mapTrace_ae _ (normalizedFiber source) _ _ sound id
    measurable_id (by
      filter_upwards [sound.2] with point good
      have massOne : outputGivenTraceAt depth source (retain point.1) Set.univ = 1 := by
        have h := good.1
        rwa [compactFiber_apply, compactHistoryReplay_nil] at h
      change (normalizedFiber source).kernel point.1 = _
      rw [normalizedFiber, MeasurableActionFamily.pullback_apply, compactFiber_apply,
        compactHistoryReplay_nil]
      change normalizedOutputGivenTrace source (retain point.1) = _
      rw [normalizedOutputGivenTrace_eq _ _ (by
        rw [outputGivenTrace_eq_outputGivenTraceAt depth source _ massOne]
        exact massOne), outputGivenTrace_eq_outputGivenTraceAt depth source _ massOne])
  simpa only [show mapTraceOutput (id : StepTraces.Trace → StepTraces.Trace) = id from rfl,
    Measure.map_id] using transported

/-- On the compact joint laws, the normalized compact replay is a sound fiber. -/
theorem compact_normalized_fiberSound (source : Expr) (typed : Typed [] source (.float .E))
    (safe : DomainSafe source) :
    FiberSound (normalizedKernel source) (traceAndOutputLaw source)
        (traceAndOutputLaw source.determinize) := by
  rw [joint_eq_detailed, joint_eq_detailed]
  exact StepTraces.FiberSound.mapTrace _ _ _ _ (joint_normalized_fiberSound source typed safe)
    retain measurable_retain
    (fun trace ↦ by rw [normalizedFiber, MeasurableActionFamily.pullback_apply])

/-- Almost every terminating target trace gives the source replay mass one. -/
theorem compact_source_massOne (source : Expr) (typed : Typed [] source (.float .E))
    (safe : DomainSafe source) :
    ∀ᵐ q ∂traceAndOutputLaw source.determinize, outputGivenTrace source q.1 Set.univ = 1 := by
  rw [joint_eq_detailed, ae_map_iff measurable_eraseOutput.aemeasurable
    (measurableSet_massOne_compact source), StepTraces.jointMeasure, Measure.ae_sum_iff]
  intro depth
  filter_upwards [(StepTraces.compact_exactDepth_source_fiberSound source typed safe depth).2]
    with point good
  have massOne : outputGivenTraceAt depth source (retain point.1) Set.univ = 1 := by
    have h := good.1
    rwa [compactFiber_apply, compactHistoryReplay_nil] at h
  change outputGivenTrace source (retain point.1) Set.univ = 1
  rw [outputGivenTrace_eq_outputGivenTraceAt depth source _ massOne]
  exact massOne

/-- Replaying the target along its own compact trace returns its output. -/
theorem compact_target_selfReplay (source : Expr) (typed : Typed [] source (.float .E))
    (safe : DomainSafe source) :
    ∀ᵐ q ∂traceAndOutputLaw source.determinize,
      outputGivenTrace source.determinize q.1 = Measure.dirac q.2 := by
  rw [joint_eq_detailed, ae_map_iff measurable_eraseOutput.aemeasurable
    (measurableSet_selfReplay_compact source.determinize), StepTraces.jointMeasure,
    Measure.ae_sum_iff]
  intro depth
  filter_upwards [StepTraces.target_selfReplay_source source typed safe depth] with point hp
  change outputGivenTrace source.determinize (retain point.1) = Measure.dirac point.2
  rw [outputGivenTrace_eq_outputGivenTraceAt depth _ _ (by rw [hp]; simp), hp]

/-! ### The factorization with its almost-sure identifications -/

/-- Trace soundness for an expectation-affinity source, as a factorization by the normalized
compact replay together with the identifications that give the public statement: almost
surely the source fiber is `outputGivenTrace` itself, and the target replay is the Dirac mass
at the target output. -/
theorem soundness_data_of_E (source : Expr) (typed : Typed [] source (.float .E))
    (safe : DomainSafe source) :
    DomainSafe source.determinize ∧
      TraceFactorization source source.determinize (normalizedOutputGivenTrace source)
        (kernelMean (normalizedOutputGivenTrace source)) ∧
      (∀ᵐ trace ∂traceLaw source,
        outputGivenTrace source trace = normalizedOutputGivenTrace source trace) ∧
      ∀ᵐ trace ∂traceLaw source,
        outputGivenTrace source.determinize trace =
            Measure.dirac (kernelMean (normalizedOutputGivenTrace source) trace) := by
  refine ⟨domainSafe_determinize typed safe, ?_⟩
  let ν := (traceAndOutputLaw source.determinize).map Prod.fst
  let f := kernelMean (normalizedOutputGivenTrace source)
  rcases (compact_normalized_fiberSound source typed safe).factorization
    (traceAndOutputLaw_mass_le_one source.determinize) with ⟨hm, hf, hs, ht, hmean⟩
  have hν : traceLaw source = ν := by
    let : IsFiniteMeasure ν := ⟨hm.trans_lt (by simp)⟩
    rw [traceLaw, hs]
    exact Measure.fst_compProd ν (normalizedKernel source).kernel
  change _ = ν ⊗ₘ _ at hs
  change _ = ν.map (fun trace ↦ (trace, f trace)) at ht
  change ∀ᵐ trace ∂ν, _ at hmean
  rw [← hν] at hs ht hmean
  have pairMeasurable : Measurable (fun trace : Trace ↦ (trace, f trace)) :=
    measurable_id.prodMk hf
  refine ⟨⟨inferInstance, hf, hs, ht, hmean⟩, ?_, ?_⟩
  · have massOne := compact_source_massOne source typed safe
    rw [ht] at massOne
    filter_upwards [ae_of_ae_map pairMeasurable.aemeasurable massOne] with trace mass
    exact (normalizedOutputGivenTrace_eq source trace mass).symm
  · have dirac := compact_target_selfReplay source typed safe
    rw [ht] at dirac
    exact ae_of_ae_map pairMeasurable.aemeasurable dirac

/-- Apply expectation-affinity soundness using silent subtyping for general-affinity programs. -/
theorem soundness_data (affinity : Affinity) (program : Expr)
    (typed : Typed [] program (.float affinity))
    (safe : DomainSafe program) :
    DomainSafe program.determinize ∧
      TraceFactorization program program.determinize (normalizedOutputGivenTrace program)
        (kernelMean (normalizedOutputGivenTrace program)) ∧
      (∀ᵐ trace ∂traceLaw program,
        outputGivenTrace program trace = normalizedOutputGivenTrace program trace) ∧
      ∀ᵐ trace ∂traceLaw program,
        outputGivenTrace program.determinize trace =
            Measure.dirac (kernelMean (normalizedOutputGivenTrace program) trace) := by
  cases affinity with
  | E => exact soundness_data_of_E program typed safe
  | G => exact soundness_data_of_E program (.sub typed .general) safe

/-- Some trace factorization exists: the input of the corollaries. -/
theorem meanOnTraces_determinize (affinity : Affinity) (program : Expr)
    (typed : Typed [] program (.float affinity))
    (safe : DomainSafe program) :
    DomainSafe program.determinize ∧ MeanOnTraces program program.determinize :=
  let ⟨targetSafe, factor, _, _⟩ := soundness_data affinity program typed safe
  ⟨targetSafe, _, factor⟩

/-- A measure composed with a kernel is the bind that pairs each point with its draw. -/
theorem compProd_eq_traceThenOutput (traces : Measure Trace) [SFinite traces]
    (fiber : Kernel Trace ℝ) [IsSFiniteKernel fiber] :
    traces ⊗ₘ fiber = traceThenOutput traces fiber :=
  StepTraces.compProd_eq_bind_pair traces fiber

/-- Determinization returns the canonical replay mean on each terminating trace. -/
theorem traceAndOutputLaw_determinize (program : Expr) (typed : Typed [] program (.float .E))
    (safe : DomainSafe program) :
    traceAndOutputLaw program.determinize =
      (traceLaw program).map (fun trace ↦ (trace, replayMean program trace)) := by
  obtain ⟨_, factor, sameFiber, _⟩ := soundness_data_of_E program typed safe
  refine factor.2.2.2.1.trans (Measure.map_congr ?_)
  filter_upwards [sameFiber] with trace same
  simp only [kernelMean, replayMean, same]

/-- Replay factorization and its conditional-mean property, used to identify the conditional
laws. -/
theorem replay_soundness :
  ∀ (program : Expr),
    Typed [] program (.float .E) → DomainSafe program →
      DomainSafe program.determinize ∧
      traceAndOutputLaw program = traceThenOutput (traceLaw program) (outputGivenTrace program) ∧
      traceAndOutputLaw program.determinize =
        traceThenOutput (traceLaw program) (outputGivenTrace program.determinize) ∧
      ∀ᵐ trace ∂traceLaw program,
        Integrable id (outputGivenTrace program trace) ∧
        outputGivenTrace program.determinize trace =
          Measure.dirac (replayMean program trace) := by
  intro program typed safe
  let f := kernelMean (normalizedOutputGivenTrace program)
  obtain ⟨targetSafe, factor, massAe, diracAe⟩ :=
    soundness_data .E program typed safe
  obtain ⟨markov, hf, hs, ht, hmean⟩ := factor
  have := markov
  refine ⟨targetSafe, ?_, ?_, ?_⟩
  · rw [hs, compProd_eq_traceThenOutput, traceThenOutput, traceThenOutput]
    exact Measure.bind_congr_right (massAe.mono fun trace h ↦ by simp only [h])
  · rw [ht, traceThenOutput, ← Measure.bind_dirac_eq_map _
      (show Measurable (fun trace : Trace ↦ (trace, f trace)) from measurable_id.prodMk hf)]
    apply Measure.bind_congr_right
    filter_upwards [diracAe] with trace h
    rw [h, Measure.map_dirac' (show Measurable (fun value : ℝ ↦ (trace, value)) from
      measurable_const.prodMk measurable_id)]
  · filter_upwards [hmean, massAe, diracAe] with trace mean mass dirac
    simp only [replayMean, mass]
    exact ⟨mean.1, by rw [dirac, mean.2]⟩

end
end Determinize.Proof.Traces
