import Determinize.Spec.Traces.Semantics
import Determinize.Proof.Primitives.Mass
import Determinize.Proof.Symbolic.TraceLaws
import Determinize.Proof.Traces.CompactTrace

namespace Determinize.Proof.Traces

open MeasureTheory Determinize.Spec.Paper Determinize.Spec.Traces Determinize.Proof.Paper
open StepTraces (retain measurable_retain mapTraceOutput measurable_mapTraceOutput)
open scoped ENNReal

/-! ### The joint law as an erased detailed law

At each depth, the joint law is the detailed law `StepTraces.exactMeasure` with each trace
reduced to its general-affinity draws (`exact_eq_detailed`). -/

abbrev eraseOutput : StepTraces.Output → Output := mapTraceOutput retain

theorem measurable_eraseOutput : Measurable eraseOutput :=
  measurable_mapTraceOutput retain measurable_retain

theorem measurable_record (site : DistributionAction × Op) (value : ℝ) :
    Measurable (record site value) := by
  rcases site with ⟨kind, op⟩
  cases kind with
  | sample affinity =>
    cases affinity with
    | E => exact measurable_id
    | G => exact (StepTraces.measurable_draw_cons.comp
        (measurable_const.prodMk measurable_fst)).prodMk measurable_snd
  | mean => exact measurable_id

theorem exact_eq_detailed (depth : Nat) (e : Expr) :
    traceAndOutputLawAt depth e = (StepTraces.exactMeasure depth e).map eraseOutput := by
  induction depth generalizing e with
  | zero =>
    cases e <;> try simp only [traceAndOutputLawAt, StepTraces.exactMeasure, Measure.map_zero]
    case real r =>
      simp [Measure.map_dirac' measurable_eraseOutput, eraseOutput, mapTraceOutput, retain]
  | succ depth ih =>
    by_cases value : e.isValue = true
    · simp [traceAndOutputLawAt, StepTraces.exactMeasure, value]
    · rw [traceAndOutputLawAt, StepTraces.exactMeasure, ite_eq_right value, ite_eq_right value]
      cases reduction : reduce e with
      | stuck => simp
      | next next =>
        simp only [ih]
        rw [Measure.map_map measurable_eraseOutput
          (show Measurable (StepTraces.prepend none) from
            StepTraces.measurable_prepend.comp (measurable_const.prodMk measurable_id))]
        congr 1
      | sample site fiber cont =>
        have hc := (MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable
          e fiber cont reduction
        have hm :=
          (StepTraces.successorKernel (StepTraces.exactKernel depth)).kernel.measurable.comp
          ((StepTraces.measurable_generationEvent site).prodMk hc)
        simp only [Function.comp_def] at hm
        simp_rw [StepTraces.successorKernel_apply, StepTraces.exactKernel_apply] at hm
        rw [StepTraces.map_bind_fun _ _ hm _ measurable_eraseOutput]
        apply Measure.bind_congr_right
        filter_upwards [] with r
        rw [ih, Measure.map_map (measurable_record site r) measurable_eraseOutput,
          Measure.map_map measurable_eraseOutput
            (show Measurable (StepTraces.prepend (StepTraces.generationEvent site r)) from
              StepTraces.measurable_prepend.comp (measurable_const.prodMk measurable_id))]
        congr 1
        funext p
        rcases site with ⟨kind, op⟩
        cases kind with
        | sample affinity => cases affinity <;> rfl
        | mean => rfl

/-- The kernel to which a sample site binds its fiber is measurable: it is the erased law of
the measurable detailed kernel `StepTraces.exactKernel`, extended by the recorded draw. -/
theorem measurable_traceAndOutputLawAt_continuation (depth : Nat) {expression : Expr}
    {site : DistributionAction × Op} {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (reduction : reduce expression = .sample site fiber continuation) :
    Measurable fun value ↦
      (traceAndOutputLawAt depth (continuation value)).map (record site value) := by
  have hc := (MeasurableActionFamily.stepKernel primitiveLaws).sample_continuation_measurable
    expression fiber continuation reduction
  have hm := (Measure.measurable_map _ measurable_eraseOutput).comp
    ((StepTraces.successorKernel (StepTraces.exactKernel depth)).kernel.measurable.comp
      ((StepTraces.measurable_generationEvent site).prodMk hc))
  convert hm using 1
  funext value
  simp only [Function.comp_apply]
  rw [StepTraces.successorKernel_apply, StepTraces.exactKernel_apply, exact_eq_detailed,
    Measure.map_map (measurable_record site value) measurable_eraseOutput,
    Measure.map_map measurable_eraseOutput
      (show Measurable (StepTraces.prepend (StepTraces.generationEvent site value)) from
        StepTraces.measurable_prepend.comp (measurable_const.prodMk measurable_id))]
  congr 1
  funext p
  rcases site with ⟨kind, op⟩
  cases kind with
  | sample affinity => cases affinity <;> rfl
  | mean => rfl

/-! ### The joint law is a finite measure

Every fiber is a subprobability measure (`reduce_sample_mass_le_one`), so by induction on the
number of reduction depths the joint law has mass at most one. The induction needs the kernel
of each sample site to be measurable (`measurable_traceAndOutputLawAt_continuation`): along a
map that is not almost everywhere measurable, Mathlib's pushforward, and so its bind, is
unspecified. -/

theorem map_univ_le {α β : Type*} [MeasurableSpace α] [MeasurableSpace β] (f : α → β)
    (μ : Measure α) (hf : AEMeasurable f μ) : μ.map f Set.univ ≤ μ Set.univ := by
  rw [Measure.map_apply_of_aemeasurable hf MeasurableSet.univ, Set.preimage_univ]

theorem finset_sum_lintegral_le {α ι : Type*} [MeasurableSpace α] (μ : Measure α) (s : Finset ι)
    (f : ι → α → ℝ≥0∞) : ∑ i ∈ s, ∫⁻ a, f i a ∂μ ≤ ∫⁻ a, ∑ i ∈ s, f i a ∂μ := by
  classical
  induction s using Finset.induction_on with
  | empty => simp
  | insert i s hi ih =>
    simp only [Finset.sum_insert hi]
    exact (add_le_add le_rfl ih).trans (le_lintegral_add _ _)

/-- Through any number of reduction depths, the joint law has mass at most one: a real value
contributes a Dirac mass at depth zero and nothing later, another value contributes nothing,
and a reducible expression passes the bound of its successors through the fiber. -/
theorem traceAndOutputLawAt_partial_mass_le_one (bound : Nat) (expression : Expr) :
    ∑ depth ∈ Finset.range bound, traceAndOutputLawAt depth expression Set.univ ≤ 1 := by
  induction bound generalizing expression with
  | zero => simp
  | succ bound ih =>
    rw [Finset.sum_range_succ']
    by_cases value : expression.isValue = true
    · simp only [traceAndOutputLawAt, value]
      cases expression <;> simp [traceAndOutputLawAt]
    · have zero : traceAndOutputLawAt 0 expression Set.univ = 0 := by
        cases expression <;> first | exact absurd rfl value | simp [traceAndOutputLawAt]
      rw [zero, add_zero]
      simp only [traceAndOutputLawAt, value]
      cases reduction : reduce expression with
      | next next => exact ih next
      | stuck => simp
      | sample site fiber continuation =>
        calc ∑ depth ∈ Finset.range bound, (fiber.bind fun value ↦
                (traceAndOutputLawAt depth (continuation value)).map (record site value))
                Set.univ
            ≤ ∑ depth ∈ Finset.range bound,
                ∫⁻ value, traceAndOutputLawAt depth (continuation value) Set.univ ∂fiber :=
              Finset.sum_le_sum fun depth _ ↦
                (Measure.bind_apply_le
                  (measurable_traceAndOutputLawAt_continuation depth reduction).aemeasurable
                  MeasurableSet.univ).trans
                  (lintegral_mono fun value ↦
                    map_univ_le _ _ (measurable_record site value).aemeasurable)
          _ ≤ ∫⁻ value, ∑ depth ∈ Finset.range bound,
                traceAndOutputLawAt depth (continuation value) Set.univ ∂fiber :=
              finset_sum_lintegral_le _ _ _
          _ ≤ ∫⁻ _, 1 ∂fiber := lintegral_mono fun value ↦ ih (continuation value)
          _ = fiber Set.univ := by simp
          _ ≤ 1 := reduce_sample_mass_le_one reduction

theorem traceAndOutputLaw_mass_le_one (program : Expr) :
    traceAndOutputLaw program Set.univ ≤ 1 := by
  rw [traceAndOutputLaw, Measure.sum_apply _ MeasurableSet.univ]
  exact ENNReal.tsum_le_of_sum_range_le fun bound ↦
    traceAndOutputLawAt_partial_mass_le_one bound program

/-- The joint law of trace and output is a finite measure, as Mathlib's disintegration
`Measure.condKernel` requires of it (`Theorems.lean`). -/
instance isFiniteMeasure_traceAndOutputLaw (program : Expr) :
    IsFiniteMeasure (traceAndOutputLaw program) :=
  ⟨(traceAndOutputLaw_mass_le_one program).trans_lt ENNReal.one_lt_top⟩

instance isFiniteMeasure_traceLaw (program : Expr) : IsFiniteMeasure (traceLaw program) :=
  inferInstanceAs (IsFiniteMeasure ((traceAndOutputLaw program).map Prod.fst))

end Determinize.Proof.Traces
