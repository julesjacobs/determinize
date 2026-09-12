import Determinize.Spec.Traces.Semantics
import Determinize.Proof.PrimitiveMass

namespace Determinize.Proof.Traces

open MeasureTheory Determinize.Spec.Paper Determinize.Spec.Traces Determinize.Proof.Paper
open scoped ENNReal

/-! ### The joint law is a finite measure

Every fiber is a subprobability measure (`reduce_sample_mass_le_one`), so by induction on the
number of reduction depths the joint law has mass at most one. The argument needs no
measurability: the pushforward or bind of a non-measurable map is the zero measure. -/

theorem map_univ_le {α β : Type*} [MeasurableSpace α] [MeasurableSpace β] (f : α → β)
    (μ : Measure α) : μ.map f Set.univ ≤ μ Set.univ := by
  by_cases hf : AEMeasurable f μ
  · rw [Measure.map_apply_of_aemeasurable hf MeasurableSet.univ, Set.preimage_univ]
  · rw [Measure.map_of_not_aemeasurable hf]
    exact zero_le

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
            calc ∑ depth ∈ Finset.range bound, (fiber.bind fun value =>
                    (traceAndOutputLawAt depth (continuation value)).map (record site value))
                    Set.univ
                ≤ ∑ depth ∈ Finset.range bound,
                    ∫⁻ value, traceAndOutputLawAt depth (continuation value) Set.univ ∂fiber :=
                  Finset.sum_le_sum fun depth _ =>
                    (Measure.bind_apply_le _ MeasurableSet.univ).trans
                      (lintegral_mono fun value => map_univ_le _ _)
              _ ≤ ∫⁻ value, ∑ depth ∈ Finset.range bound,
                    traceAndOutputLawAt depth (continuation value) Set.univ ∂fiber :=
                  finset_sum_lintegral_le _ _ _
              _ ≤ ∫⁻ _, 1 ∂fiber := lintegral_mono fun value => ih (continuation value)
              _ = fiber Set.univ := by simp
              _ ≤ 1 := reduce_sample_mass_le_one reduction

theorem traceAndOutputLaw_mass_le_one (program : Expr) :
    traceAndOutputLaw program Set.univ ≤ 1 := by
  rw [traceAndOutputLaw, Measure.sum_apply _ MeasurableSet.univ]
  exact ENNReal.tsum_le_of_sum_range_le fun bound =>
    traceAndOutputLawAt_partial_mass_le_one bound program

/-- The joint law of trace and output is a finite measure, as Mathlib's disintegration
`Measure.condKernel` requires of it (`Traces/Main.lean`). -/
instance isFiniteMeasure_traceAndOutputLaw (program : Expr) :
    IsFiniteMeasure (traceAndOutputLaw program) :=
  ⟨(traceAndOutputLaw_mass_le_one program).trans_lt ENNReal.one_lt_top⟩

end Determinize.Proof.Traces
