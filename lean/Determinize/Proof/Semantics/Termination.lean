import Determinize.Proof.Semantics.Ordinary

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper
open SymbolicSoundness.TargetSafety
open scoped ENNReal

private noncomputable abbrev paperStepKernel := MeasurableActionFamily.stepKernel primitiveLaws

private theorem returned_add_running_eq_step_mass (depth : Nat) (typed : Typed [] expression (.float affinity)) :
    Cumulative.outputMeasure depth expression Set.univ + runningProbabilityAt depth expression =
      nStepMeasure paperStepKernel depth expression Set.univ := by
  induction depth generalizing expression with
  | zero =>
      by_cases value : expression.isValue = true
      · obtain ⟨r, rfl⟩ := Typing.typed_real_value typed value
        simp [Cumulative.outputMeasure, runningProbabilityAt, nStepMeasure, Expr.isValue]
      · cases expression <;> simp_all [Cumulative.outputMeasure, runningProbabilityAt, nStepMeasure, Expr.isValue]
  | succ depth ih =>
      by_cases value : expression.isValue = true
      · rw [nStepMeasure_univ_eq_one_of_value paperStepKernel _ _ value]
        obtain ⟨r, rfl⟩ := Typing.typed_real_value typed value
        have output : ∀ n, Cumulative.outputMeasure n (.real r) = Measure.dirac r := by
          intro n
          induction n with
          | zero => rfl
          | succ n ih => simpa [Cumulative.outputMeasure, reduce] using ih
        simp [output, runningProbabilityAt, Expr.isValue]
      · simp only [runningProbabilityAt, value, Bool.false_eq_true, ↓reduceIte]
        have actionTyped := Typing.reduce_typed_closed typed
        cases reduction : reduce expression with
        | stuck =>
            rw [nStepMeasure_succ_eq_firstStep, paperStepKernel.kernel_eq_stepMeasure]
            simp [stepMeasure, reduction, Action.measure, Cumulative.outputMeasure]
        | next next =>
            rw [reduction] at actionTyped
            cases actionTyped with
            | next nextTyped =>
                rw [nStepMeasure_succ_next_univ paperStepKernel _ _ _ reduction]
                simpa [Cumulative.outputMeasure, reduction] using ih nextTyped
        | sample site fiber continuation =>
            rw [reduction] at actionTyped
            cases actionTyped with
            | sample continuationTyped =>
                have hm : Measurable (fun r => Cumulative.outputMeasure depth (continuation r) Set.univ) :=
                  (Measure.measurable_coe MeasurableSet.univ).comp
                    (measurable_sample_cumulative depth expression site fiber continuation reduction)
                change Cumulative.outputMeasure (depth + 1) expression Set.univ +
                  (∫⁻ r, runningProbabilityAt depth (continuation r) ∂fiber) = _
                rw [Cumulative.outputMeasure, reduction,
                  Measure.bind_apply MeasurableSet.univ
                    (measurable_sample_cumulative depth expression site fiber continuation reduction).aemeasurable,
                  ← lintegral_add_left hm,
                  nStepMeasure_succ_sample_univ paperStepKernel _ _ _ _ reduction]
                exact lintegral_congr fun r => ih (continuationTyped r)

private theorem total_antitone (expression : Expr) :
    Antitone (fun n => nStepMeasure paperStepKernel n expression Set.univ) := by
  apply antitone_nat_of_succ_le
  intro n
  rw [nStepMeasure, Measure.bind_apply MeasurableSet.univ paperStepKernel.kernel.aemeasurable]
  calc
    (∫⁻ current, paperStepKernel.kernel current Set.univ ∂nStepMeasure paperStepKernel n expression) ≤
        ∫⁻ _, 1 ∂nStepMeasure paperStepKernel n expression :=
      lintegral_mono fun current => paperStepKernel.mass_le_one current
    _ = _ := lintegral_one

/-- At finite depth, returned and still-running probability exhaust all mass. -/
theorem returned_add_running (depth : Nat) (typed : Typed [] expression (.float affinity))
    (safe : DomainSafeAt depth expression) :
    Cumulative.outputMeasure depth expression Set.univ +
      runningProbabilityAt depth expression = 1 :=
  (returned_add_running_eq_step_mass depth typed).trans
    ((domainSafeAt_iff_nStepMeasure_univ_eq_one paperStepKernel depth expression typed).mp safe)

private theorem output_mass_eq_iSup_cumulative (expression : Expr) :
    Determinize.Spec.Paper.bigStepMeasure expression Set.univ =
      ⨆ depth, Cumulative.outputMeasure depth expression Set.univ := by
  rw [Determinize.Spec.Paper.bigStepMeasure, Measure.sum_apply _ MeasurableSet.univ]
  simp_rw [cumulativeOutputMeasure_eq_sum, Measure.finsetSum_apply]
  exact ENNReal.tsum_eq_iSup_nat' (Filter.tendsto_add_atTop_nat 1)

private theorem running_eq_sub (depth : Nat) (typed : Typed [] expression (.float affinity))
    (safe : DomainSafe expression) :
    runningProbabilityAt depth expression =
      1 - Cumulative.outputMeasure depth expression Set.univ :=
  ENNReal.eq_sub_of_add_eq' (by simp)
    (by simpa only [add_comm] using returned_add_running depth typed (safe depth))

theorem runningProbabilityAt_antitone (typed : Typed [] expression (.float affinity))
    (safe : DomainSafe expression) : Antitone (fun depth => runningProbabilityAt depth expression) := by
  intro n m h
  change runningProbabilityAt m expression ≤ runningProbabilityAt n expression
  rw [running_eq_sub m typed safe, running_eq_sub n typed safe]
  exact tsub_le_tsub_left ((direct_cumulative_mono expression h) Set.univ) 1

theorem returnOrDiverge : Determinize.Spec.returnOrDivergeThm := by
  intro affinity expression typed safe
  have bounded : Determinize.Spec.Paper.bigStepMeasure expression Set.univ ≤ 1 := by
    rw [output_mass_eq_iSup_cumulative]
    apply iSup_le
    intro depth
    calc
      Cumulative.outputMeasure depth expression Set.univ ≤
          Cumulative.outputMeasure depth expression Set.univ + runningProbabilityAt depth expression :=
        le_self_add
      _ = 1 := returned_add_running depth typed (safe depth)
  rw [divergenceProbability]
  simp_rw [running_eq_sub _ typed safe]
  rw [← ENNReal.sub_iSup (by simp), ← output_mass_eq_iSup_cumulative]
  exact add_tsub_cancel_of_le bounded

private theorem return_diverge_le_total (typed : Typed [] expression (.float affinity)) (n : Nat) :
    Determinize.Spec.Paper.bigStepMeasure expression Set.univ + divergenceProbability expression ≤
      nStepMeasure paperStepKernel n expression Set.univ := by
  rw [output_mass_eq_iSup_cumulative, ENNReal.iSup_add]
  apply iSup_le
  intro k
  calc
    Cumulative.outputMeasure k expression Set.univ + divergenceProbability expression ≤
        Cumulative.outputMeasure (max k n) expression Set.univ + runningProbabilityAt (max k n) expression :=
      add_le_add ((direct_cumulative_mono expression (Nat.le_max_left k n)) Set.univ) (iInf_le _ _)
    _ = _ := returned_add_running_eq_step_mass _ typed
    _ ≤ _ := total_antitone expression (Nat.le_max_right k n)

theorem domainSafe_iff_return_or_diverge (typed : Typed [] expression (.float affinity)) :
    DomainSafe expression ↔
      Determinize.Spec.Paper.bigStepMeasure expression Set.univ + divergenceProbability expression = 1 := by
  constructor
  · exact returnOrDiverge affinity expression typed
  · intro conserved n
    apply (domainSafeAt_iff_nStepMeasure_univ_eq_one paperStepKernel n expression typed).mpr
    apply le_antisymm (nStepMeasure_mass_le_one paperStepKernel n expression)
    rw [← conserved]
    exact return_diverge_le_total typed n

end Determinize.Proof.Paper
