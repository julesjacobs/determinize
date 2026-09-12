import Determinize.Proof.TraceSteps
import Determinize.Proof.Internal.StepTraces
import Mathlib.Probability.Kernel.MeasurableIntegral

/-!
# Fiber soundness

`FiberSound fiber source target` is the invariant of the lockstep argument: the source joint
law is the target joint law pushed through the fibers (`fiberLift`), and almost every target
point carries a good fiber (`FiberHasMean`: a probability measure with a finite mean equal to the
point's output). The invariant survives pushing traces forward, mixing over a parameter and
countable sums, and it yields the trace factorization (`FiberSound.factorization`).
-/

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper
open scoped ProbabilityTheory

noncomputable section

variable {α β γ : Type*} [MeasurableSpace α] [MeasurableSpace β] [MeasurableSpace γ]

/-- Replace the output of a point by a draw from `fiber` at its trace. -/
def fiberLift (fiber : SFiniteKernel α ℝ) : SFiniteKernel (α × ℝ) (α × ℝ) :=
  SFiniteKernel.mapWithInput (SFiniteKernel.pullback fiber Prod.fst measurable_fst)
    (fun pair => (pair.1.1, pair.2)) ((measurable_fst.comp measurable_fst).prodMk measurable_snd)

theorem fiberLift_apply (fiber : SFiniteKernel α ℝ) (point : α × ℝ) :
    (fiberLift fiber).kernel point = (fiber.kernel point.1).map (fun x => (point.1, x)) := by
  rw [fiberLift, SFiniteKernel.mapWithInput_apply,
    MeasurableActionFamily.pullback_apply]

/-- The fiber at a point is a probability measure with a finite mean, and that mean is the
point's output. -/
def FiberHasMean (fiber : Kernel α ℝ) (point : α × ℝ) : Prop :=
  fiber point.1 Set.univ = 1 ∧ Integrable id (fiber point.1) ∧
    (∫ value : ℝ, value ∂fiber point.1) = point.2

theorem fiberHasMean_measurable (fiber : Kernel α ℝ) [IsSFiniteKernel fiber] :
    MeasurableSet {point | FiberHasMean fiber point} := by
  apply MeasurableSet.inter
  · exact measurableSet_eq_fun ((fiber.measurable_coe MeasurableSet.univ).comp measurable_fst) measurable_const
  · apply MeasurableSet.inter
    · exact (ProbabilityTheory.measurableSet_integrable (κ := fiber) stronglyMeasurable_id).preimage measurable_fst
    · exact measurableSet_eq_fun
        ((stronglyMeasurable_id.integral_kernel (κ := fiber)).measurable.comp measurable_fst) measurable_snd

/-- `source` is `target` pushed through the fibers, and almost every target point carries a
good fiber. -/
def FiberSound (fiber : SFiniteKernel α ℝ) (source target : Measure (α × ℝ)) : Prop :=
  source = target.bind (fiberLift fiber).kernel ∧ ∀ᵐ point ∂target, FiberHasMean fiber.kernel point

theorem FiberSound.zero (fiber : SFiniteKernel α ℝ) : FiberSound fiber 0 0 := by
  simp [FiberSound]

theorem FiberSound.terminal (fiber : SFiniteKernel α ℝ) (trace : α) (output : ℝ)
    (good : FiberHasMean fiber.kernel (trace, output)) :
    FiberSound fiber ((fiber.kernel trace).map (fun value => (trace, value)))
      (Measure.dirac (trace, output)) := by
  let := fiber.sfinite
  constructor
  · rw [Measure.dirac_bind (fiberLift fiber).kernel.measurable, fiberLift_apply]
  · exact (ae_dirac_iff (fiberHasMean_measurable fiber.kernel)).2 good

/-- Apply `f` to the trace component of a point. -/
def mapTraceOutput (f : α → β) (point : α × ℝ) : β × ℝ := (f point.1, point.2)

theorem mapTrace_measurable (f : α → β) (hf : Measurable f) : Measurable (mapTraceOutput f) :=
  (hf.comp measurable_fst).prodMk measurable_snd

theorem FiberSound.mapTrace_ae (fiber : SFiniteKernel α ℝ) (nextFiber : SFiniteKernel β ℝ)
    (source target : Measure (α × ℝ)) (sound : FiberSound fiber source target)
    (f : α → β) (hf : Measurable f) (sameFiber : ∀ᵐ point ∂target, nextFiber.kernel (f point.1) = fiber.kernel point.1) :
    FiberSound nextFiber (source.map (mapTraceOutput f)) (target.map (mapTraceOutput f)) := by
  constructor
  · rw [sound.1, map_bind _ _ _ (mapTrace_measurable f hf),
      bind_map _ _ (mapTrace_measurable f hf)]
    apply Measure.bind_congr_right
    filter_upwards [sameFiber] with point same
    rw [fiberLift_apply, fiberLift_apply, Measure.map_map (mapTrace_measurable f hf)
      (show Measurable (fun value : ℝ => (point.1, value)) from measurable_const.prodMk measurable_id)]
    simp only [mapTraceOutput, same]
    rfl
  · let := nextFiber.sfinite
    rw [ae_map_iff (mapTrace_measurable f hf).aemeasurable (fiberHasMean_measurable nextFiber.kernel)]
    filter_upwards [sound.2, sameFiber] with point good same
    simpa only [FiberHasMean, mapTraceOutput, same] using good

/-- `mapTrace_ae` with the fibers agreeing everywhere. -/
theorem FiberSound.mapTrace (fiber : SFiniteKernel α ℝ) (nextFiber : SFiniteKernel β ℝ)
    (source target : Measure (α × ℝ)) (sound : FiberSound fiber source target)
    (f : α → β) (hf : Measurable f)
    (sameFiber : ∀ trace, nextFiber.kernel (f trace) = fiber.kernel trace) :
    FiberSound nextFiber (source.map (mapTraceOutput f)) (target.map (mapTraceOutput f)) :=
  sound.mapTrace_ae fiber nextFiber source target f hf
    (Filter.Eventually.of_forall fun point => sameFiber point.1)

theorem FiberSound.mix (fiber : SFiniteKernel α ℝ) (outer : Measure β)
    (source target : Kernel β (α × ℝ))
    (sound : ∀ᵐ parameter ∂outer, FiberSound fiber (source parameter) (target parameter)) :
    FiberSound fiber (outer.bind source) (outer.bind target) := by
  constructor
  · rw [Measure.bind_bind target.aemeasurable (fiberLift fiber).kernel.aemeasurable]
    exact Measure.bind_congr_right (sound.mono fun _ valid => valid.1)
  · let := fiber.sfinite
    rw [Measure.ae_comp_iff (fiberHasMean_measurable fiber.kernel)]
    exact sound.mono fun _ valid => valid.2

theorem FiberSound.sum {ι : Type*} [Countable ι] (fiber : SFiniteKernel α ℝ)
    (source target : ι → Measure (α × ℝ)) (sound : ∀ i, FiberSound fiber (source i) (target i)) :
    FiberSound fiber (Measure.sum source) (Measure.sum target) := by
  constructor
  · rw [Measure.bind_sum _ _ (fiberLift fiber).kernel.aemeasurable]
    congr 1
    funext i
    exact (sound i).1
  · rw [Measure.ae_sum_iff]
    exact fun i => (sound i).2

/-- Composing a measure with a kernel pairs each point with its draw: Mathlib's
`μ ⊗ₘ κ = (Kernel.id ×ₖ κ) ∘ₘ μ`, pointwise. -/
theorem compProd_eq_bind_pair (ν : Measure α) [SFinite ν] (κ : Kernel α β)
    [IsSFiniteKernel κ] :
    ν ⊗ₘ κ = ν.bind fun point => (κ point).map fun value => (point, value) := by
  rw [Measure.compProd_eq_comp_prod]
  congr 1
  funext point
  ext set hs
  rw [Kernel.id_prod_apply' _ _ hs, Measure.map_apply measurable_prodMk_left hs]

theorem FiberSound.factorization {α : Type*} [MeasurableSpace α]
    {source target : Measure (α × ℝ)} {fiber : SFiniteKernel α ℝ}
    [IsMarkovKernel fiber.kernel] (sound : FiberSound fiber source target)
    (massTarget : target Set.univ ≤ 1) :
    let ν := target.map Prod.fst
    let output := fun trace => ∫ value : ℝ, value ∂fiber.kernel trace
    ν Set.univ ≤ 1 ∧ Measurable output ∧
      source = ν ⊗ₘ fiber.kernel ∧ target = ν.map (fun t => (t, output t)) ∧
      ∀ᵐ t ∂ν, Integrable id (fiber.kernel t) ∧ output t = ∫ r : ℝ, r ∂fiber.kernel t := by
  let ν := (target).map Prod.fst
  let output : α → ℝ := fun trace => ∫ value : ℝ, value ∂fiber.kernel trace
  have mass : ν Set.univ ≤ 1 := by
    rw [Measure.map_apply measurable_fst MeasurableSet.univ]
    exact massTarget
  let : IsFiniteMeasure ν := ⟨mass.trans_lt (by simp)⟩
  have measurableOutput : Measurable output := (stronglyMeasurable_id.integral_kernel (κ := fiber.kernel)).measurable
  refine ⟨mass, measurableOutput, ?_, ?_, ?_⟩
  · let paired := SFiniteKernel.mapWithInput fiber id measurable_id
    have pairedEq (trace : α) : paired.kernel trace =
        (fiber.kernel trace).map (fun value => (trace,value)) := by
      rw [SFiniteKernel.mapWithInput_apply]
      rfl
    have eq : (target).bind (fiberLift fiber).kernel = ν.bind paired.kernel := by
      rw [bind_map _ _ measurable_fst paired.kernel]
      apply Measure.bind_congr_right
      filter_upwards [] with point
      rw [fiberLift_apply, pairedEq]
    rw [sound.1, eq, compProd_eq_bind_pair ν fiber.kernel]
    congr 1
    funext trace
    exact pairedEq trace
  · have eq : (fun point : α × ℝ => (point.1, output point.1)) =ᵐ[target] id := by
      filter_upwards [sound.2] with point good
      exact Prod.ext rfl good.2.2
    have mapped : ν.map (fun trace => (trace,output trace)) = target := by
      dsimp only [ν]
      rw [Measure.map_map (show Measurable (fun trace : α => (trace,output trace)) from measurable_id.prodMk measurableOutput) measurable_fst]
      exact (Measure.map_congr eq).trans Measure.map_id
    exact mapped.symm
  · rw [ae_map_iff measurable_fst.aemeasurable
      (show MeasurableSet {trace : α | Integrable id (fiber.kernel trace) ∧ output trace = ∫value, value ∂fiber.kernel trace} from by
        simpa only [output, and_true] using ProbabilityTheory.measurableSet_integrable (κ := fiber.kernel) stronglyMeasurable_id)]
    filter_upwards [sound.2] with point good
    exact ⟨good.2.1, rfl⟩

end

end Determinize.Proof.StepTraces
