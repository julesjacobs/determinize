import Determinize.Proof.CompactTrace

/-!
# The compact replay as a measurable kernel

`Spec.Traces.outputGivenTraceAt` replays a program along a compact trace of general-mode draws.
This file packages it as an s-finite kernel in the trace and the expression, mirroring
`replayKernel` for detailed traces, proves the one-step unfolding lemmas the lockstep
arguments use, bounds the total mass of `Spec.Traces.outputGivenTrace` by one, and builds the
Markov kernel that agrees with it wherever it has mass one.
-/

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper
open Determinize.Spec.Traces (outputGivenTraceAt outputGivenTrace)
open scoped ProbabilityTheory ENNReal
noncomputable section
open Classical

/-! ### Unfolding lemmas -/

theorem ogtAt_zero_real (value : ℝ) :
    outputGivenTraceAt 0 (.real value) [] = Measure.dirac value := rfl

theorem ogtAt_zero_of_notReal {expression : Expr} (notReal : ∀ value, expression ≠ .real value)
    (tape : DrawTrace) : outputGivenTraceAt 0 expression tape = 0 := by
  cases expression <;> cases tape <;> first | rfl | exact absurd rfl (notReal _)

theorem ogtAt_zero_cons (expression : Expr) (head : Op × ℝ) (tape : DrawTrace) :
    outputGivenTraceAt 0 expression (head :: tape) = 0 := by
  cases expression <;> rfl

theorem ogtAt_succ_value (depth : Nat) {expression : Expr} (value : expression.isValue = true)
    (tape : DrawTrace) : outputGivenTraceAt (depth + 1) expression tape = 0 := by
  rw [outputGivenTraceAt, if_pos value]

theorem ogtAt_succ_next (depth : Nat) {expression next : Expr}
    (notValue : expression.isValue ≠ true) (reduction : reduce expression = .next next)
    (tape : DrawTrace) :
    outputGivenTraceAt (depth + 1) expression tape = outputGivenTraceAt depth next tape := by
  rw [outputGivenTraceAt, if_neg notValue, reduction]

theorem ogtAt_succ_stuck (depth : Nat) {expression : Expr}
    (notValue : expression.isValue ≠ true) (reduction : reduce expression = .stuck)
    (tape : DrawTrace) : outputGivenTraceAt (depth + 1) expression tape = 0 := by
  rw [outputGivenTraceAt, if_neg notValue, reduction]


/-- An expectation-mode draw, or a mean site, is integrated and leaves the tape alone. -/
theorem ogtAt_succ_sampleE (depth : Nat) {expression : Expr} {site : Mode × Kind × Op}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (notValue : expression.isValue ≠ true)
    (reduction : reduce expression = .sample site fiber continuation)
    (notGeneration : siteOp site = none) (tape : DrawTrace) :
    outputGivenTraceAt (depth + 1) expression tape =
      fiber.bind fun value => outputGivenTraceAt depth (continuation value) tape := by
  rw [outputGivenTraceAt, if_neg notValue, reduction]
  rcases site with ⟨mode, kind, op⟩
  cases mode <;> cases kind <;> simp_all [siteOp]

/-- A general-mode draw is read from the tape when the primitive fits. -/
theorem ogtAt_succ_sampleG (depth : Nat) {expression : Expr} {op : Op}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (notValue : expression.isValue ≠ true)
    (reduction : reduce expression = .sample (.G, .stochastic, op) fiber continuation)
    (value : ℝ) (tape : DrawTrace) :
    outputGivenTraceAt (depth + 1) expression ((op, value) :: tape) =
      outputGivenTraceAt depth (continuation value) tape := by
  rw [outputGivenTraceAt, if_neg notValue, reduction]
  simp

theorem ogtAt_succ_sampleG_nil (depth : Nat) {expression : Expr} {op : Op}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (notValue : expression.isValue ≠ true)
    (reduction : reduce expression = .sample (.G, .stochastic, op) fiber continuation) :
    outputGivenTraceAt (depth + 1) expression [] = 0 := by
  rw [outputGivenTraceAt, if_neg notValue, reduction]

theorem ogtAt_succ_sampleG_mismatch (depth : Nat) {expression : Expr} {op op' : Op}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (notValue : expression.isValue ≠ true)
    (reduction : reduce expression = .sample (.G, .stochastic, op) fiber continuation)
    (mismatch : op ≠ op') (value : ℝ) (tape : DrawTrace) :
    outputGivenTraceAt (depth + 1) expression ((op', value) :: tape) = 0 := by
  rw [outputGivenTraceAt, if_neg notValue, reduction]
  simp [mismatch]

/-- The value `unit`, the sink of a rejected execution, has no output at any depth. -/
theorem ogtAt_unit (depth : Nat) (tape : DrawTrace) :
    outputGivenTraceAt depth (.unit : Expr) tape = 0 := by
  cases depth with
  | zero => exact ogtAt_zero_of_notReal (fun _ h => by cases h) tape
  | succ depth => exact ogtAt_succ_value depth rfl tape

/-! ### The active generation site determines the reduction -/

theorem Action.wrap_sample_or_stuck {context : Expr → Expr} {action : Action} {op : Op}
    (h : (∃ fiber continuation, action = .sample (.G, .stochastic, op) fiber continuation) ∨
      action = .stuck) :
    (∃ fiber continuation, action.wrap context = .sample (.G, .stochastic, op) fiber continuation) ∨
      action.wrap context = .stuck := by
  rcases h with ⟨fiber, continuation, rfl⟩ | rfl
  · exact Or.inl ⟨fiber, context ∘ continuation, rfl⟩
  · exact Or.inr rfl

theorem generationOp_of_isValue {expression : Expr} (value : expression.isValue = true) :
    generationOp expression.skeleton = none :=
  generationOp_value (by rwa [← isValue_eq_skeletonIsValue])

theorem not_isValue_of_generationOp_some {expression : Expr} {op : Op}
    (active : generationOp expression.skeleton = some op) : expression.isValue ≠ true :=
  fun value => by simp [generationOp_of_isValue value] at active

set_option maxHeartbeats 1600000 in
/-- When the skeleton names an active general-mode site, reduction samples at it with that
primitive, or is stuck on operands that are values but not reals. -/
theorem generationOp_some_reduce {expression : Expr} {op : Op}
    (active : generationOp expression.skeleton = some op) :
    (∃ fiber continuation, reduce expression = .sample (.G, .stochastic, op) fiber continuation) ∨
      reduce expression = .stuck := by
  cases expression <;> rw [reduce.eq_def]
  all_goals simp only [Expr.skeleton, generationOp, ← isValue_eq_skeletonIsValue] at active
  all_goals try simp only [reduceCtorEq] at active
  all_goals dsimp only
  all_goals repeat' split
  all_goals simp_all only [↓reduceIte, Bool.not_eq_true, Bool.false_eq_true, siteOp,
    reduceCtorEq]
  all_goals first
    | exact Or.inl ⟨_, _, rfl⟩
    | exact Or.inr rfl
    | exact Action.wrap_sample_or_stuck (generationOp_some_reduce active)
    | exact absurd ‹_› (not_isValue_of_generationOp_some active)
    | (cases ‹Mode› <;> cases ‹Kind› <;> simp_all)
termination_by sizeOf expression
decreasing_by
  all_goals subst_vars
  all_goals simp_wf
  all_goals omega

/-! ### The compact replay kernel -/

theorem continuation_measurable {expression : Expr} {site : Mode × Kind × Op}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (reduction : reduce expression = .sample site fiber continuation) :
    Measurable continuation :=
  StepKernel.sample_continuation_measurable (MeasurableActionFamily.stepKernel primitiveLaws)
    expression fiber continuation reduction

/-- The active general-mode site of the expression fits the head of the tape. -/
def matchesHead (pair : DrawTrace × Expr) : Prop :=
  pair.1 ≠ [] ∧ generationOp pair.2.skeleton = some (pair.1.getD 0 (.uniform, 0)).1

theorem nilRegion_measurable : MeasurableSet {pair : DrawTrace × Expr | pair.1 = []} := by
  have eq : {pair : DrawTrace × Expr | pair.1 = []} =
      (fun pair : DrawTrace × Expr => pair.1.length) ⁻¹' {0} := by
    ext pair
    simp
  rw [eq]
  exact (draw_length_measurable.comp measurable_fst) (measurableSet_singleton 0)

theorem matchesHead_measurable : MeasurableSet {pair : DrawTrace × Expr | matchesHead pair} := by
  have eq : {pair : DrawTrace × Expr | matchesHead pair} =
      {pair : DrawTrace × Expr | pair.1 = []}ᶜ ∩
        {pair : DrawTrace × Expr |
          generationOp pair.2.skeleton = some (pair.1.getD 0 (.uniform, 0)).1} := by
    ext pair
    simp [matchesHead]
  rw [eq]
  refine nilRegion_measurable.compl.inter (measurableSet_eq_fun ?_ ?_)
  · exact (measurable_of_countable generationOp).comp (measurable_skeleton.comp measurable_snd)
  · exact (measurable_of_countable some).comp
      (measurable_fst.comp ((draw_event_measurable 0).comp measurable_fst))

/-- One replay step: a deterministic step or an expectation-mode draw leaves the tape alone;
a general-mode draw consumes the head of the tape when its primitive fits, and stops
otherwise. -/
def compactReplayStep : SFiniteKernel (DrawTrace × Expr) (DrawTrace × Expr) := by
  let step := MeasurableActionFamily.stepKernel primitiveLaws
  let sampled := SFiniteKernel.mapWithInput
    (SFiniteKernel.pullback ⟨step.kernel, step.kernel_sfinite⟩ Prod.snd measurable_snd)
    (fun pair : (DrawTrace × Expr) × Expr => (pair.1.1, pair.2))
    ((measurable_fst.comp measurable_fst).prodMk measurable_snd)
  let forced := SFiniteKernel.deterministic
    (fun pair : DrawTrace × Expr =>
      (List.tail pair.1, sampleContinuation pair.2 (pair.1.getD 0 (.uniform, 0)).2))
    ((draw_tail_measurable.comp measurable_fst).prodMk
      (sampleContinuation_measurable.comp (measurable_snd.prodMk
        (measurable_snd.comp ((draw_event_measurable 0).comp measurable_fst)))))
  let region : Set (DrawTrace × Expr) := {pair | generationOp pair.2.skeleton = none}
  have measurableRegion : MeasurableSet region :=
    (measurable_skeleton.comp measurable_snd)
      (show MeasurableSet {s | generationOp s = none} from trivial)
  exact SFiniteKernel.piecewise measurableRegion sampled
    (SFiniteKernel.piecewise matchesHead_measurable forced SFiniteKernel.zero)

theorem compactReplayStep_apply (tape : DrawTrace) (expression : Expr) :
    compactReplayStep.kernel (tape, expression) =
      if generationOp expression.skeleton = none then
        (Determinize.Spec.Paper.stepMeasure expression).map (fun next => (tape, next))
      else if matchesHead (tape, expression) then
        Measure.dirac (tape.tail, sampleContinuation expression (tape.getD 0 (.uniform, 0)).2)
      else 0 := by
  unfold compactReplayStep SFiniteKernel.piecewise
  simp only [Kernel.piecewise, Kernel.coe_mk, Set.mem_ofPred_eq]
  by_cases none : generationOp expression.skeleton = none
  · rw [if_pos none, if_pos none,
      SymbolicSoundness.TargetSafety.sfiniteKernel_mapWithInput_apply,
      MeasurableActionFamily.pullback_apply, StepKernel.kernel_eq_stepMeasure]
  · rw [if_neg none, if_neg none]
    by_cases fits : matchesHead (tape, expression)
    · rw [if_pos fits, if_pos fits]
      rfl
    · rw [if_neg fits, if_neg fits]
      rfl

/-- The compact replay at a fixed depth, jointly measurable in the tape and the expression. -/
def compactReplayKernel : (depth : Nat) → SFiniteKernel (DrawTrace × Expr) ℝ
  | 0 => SFiniteKernel.piecewise nilRegion_measurable
      (SFiniteKernel.pullback
        (MeasurableActionFamily.exactOutputKernelPack
          (MeasurableActionFamily.stepKernel primitiveLaws) 0) Prod.snd measurable_snd)
      SFiniteKernel.zero
  | depth + 1 => by
      let previous := compactReplayKernel depth
      let step := compactReplayStep
      letI := previous.sfinite
      letI := step.sfinite
      have measurableValues : MeasurableSet {pair : DrawTrace × Expr | pair.2.isValue = true} :=
        MeasurableActionFamily.valueSet_measurable.preimage measurable_snd
      exact SFiniteKernel.piecewise measurableValues SFiniteKernel.zero
        ⟨previous.kernel ∘ₖ step.kernel, inferInstance⟩

theorem compactReplayKernel_apply (depth : Nat) (tape : DrawTrace) (expression : Expr) :
    (compactReplayKernel depth).kernel (tape, expression) =
      outputGivenTraceAt depth expression tape := by
  induction depth generalizing expression tape with
  | zero =>
      unfold compactReplayKernel SFiniteKernel.piecewise
      simp only [Kernel.piecewise, Kernel.coe_mk, Set.mem_ofPred_eq]
      by_cases nil : tape = []
      · rw [if_pos nil, MeasurableActionFamily.pullback_apply]
        subst nil
        exact (MeasurableActionFamily.exactOutputKernel_apply _ _ _).trans
          (by cases expression <;> rfl)
      · rw [if_neg nil]
        obtain ⟨head, rest, rfl⟩ := List.exists_cons_of_ne_nil nil
        exact (ogtAt_zero_cons _ _ _).symm
  | succ depth ih =>
      unfold compactReplayKernel SFiniteKernel.piecewise
      dsimp only
      simp only [Kernel.piecewise, Kernel.coe_mk, Set.mem_ofPred_eq]
      by_cases value : expression.isValue = true
      · rw [if_pos value, ogtAt_succ_value depth value]
        rfl
      · rw [if_neg value, Kernel.comp_apply, compactReplayStep_apply]
        let previous := SFiniteKernel.pullback (compactReplayKernel depth)
          (fun next : Expr => (tape, next)) (measurable_const.prodMk measurable_id)
        have previousEq : ∀ next, previous.kernel next = outputGivenTraceAt depth next tape := by
          intro next
          rw [MeasurableActionFamily.pullback_apply, ih]
        have previousFun : (fun next : Expr => (compactReplayKernel depth).kernel (tape, next)) =
            previous.kernel := by
          funext next
          rw [MeasurableActionFamily.pullback_apply]
        by_cases none : generationOp expression.skeleton = none
        · rw [if_pos none, bind_map _ (fun next : Expr => (tape, next))
            (measurable_const.prodMk measurable_id) (compactReplayKernel depth).kernel, previousFun]
          unfold Determinize.Spec.Paper.stepMeasure
          cases reduction : reduce expression with
          | next next =>
              rw [Action.measure, Measure.dirac_bind previous.kernel.measurable, previousEq,
                ogtAt_succ_next depth value reduction]
          | sample site fiber continuation =>
              rw [Action.measure, ogtAt_succ_sampleE depth value reduction
                (by rw [← reduce_site reduction, none]),
                bind_map _ _ (continuation_measurable reduction)]
              simp_rw [previousEq]
          | stuck =>
              rw [Action.measure, Measure.bind_zero_left, ogtAt_succ_stuck depth value reduction]
        · rw [if_neg none]
          obtain ⟨op, active⟩ := Option.ne_none_iff_exists'.mp none
          rcases generationOp_some_reduce active with ⟨fiber, continuation, reduction⟩ | reduction
          · by_cases fits : matchesHead (tape, expression)
            · rw [if_pos fits, Measure.dirac_bind (compactReplayKernel depth).kernel.measurable,
                ih]
              obtain ⟨⟨op', v⟩, rest, rfl⟩ := List.exists_cons_of_ne_nil fits.1
              have opEq : op' = op := by
                have h := fits.2
                simp only [active, List.getD_cons_zero, Option.some.injEq] at h
                exact h.symm
              subst opEq
              simp only [List.tail_cons, List.getD_cons_zero]
              rw [ogtAt_succ_sampleG depth value reduction, sampleContinuation, reduction]
            · rw [if_neg fits, Measure.bind_zero_left]
              cases tape with
              | nil => exact (ogtAt_succ_sampleG_nil depth value reduction).symm
              | cons head rest =>
                  obtain ⟨op', v⟩ := head
                  have ne : op ≠ op' := fun h => fits ⟨List.cons_ne_nil _ _, by simp [active, h]⟩
                  exact (ogtAt_succ_sampleG_mismatch depth value reduction ne v rest).symm
          · rw [ogtAt_succ_stuck depth value reduction]
            split_ifs with fits
            · rw [Measure.dirac_bind (compactReplayKernel depth).kernel.measurable, ih,
                sampleContinuation, reduction, ogtAt_unit]
            · exact Measure.bind_zero_left _

theorem ogtAt_measurable (depth : Nat) (expression : Expr) :
    Measurable (fun tape : DrawTrace => outputGivenTraceAt depth expression tape) := by
  have eq : (fun tape : DrawTrace => outputGivenTraceAt depth expression tape) =
      fun tape => (compactReplayKernel depth).kernel (tape, expression) := by
    funext tape
    exact (compactReplayKernel_apply _ _ _).symm
  rw [eq]
  exact (compactReplayKernel depth).kernel.measurable.comp (measurable_id.prodMk measurable_const)

theorem ogtAt_expression_measurable (depth : Nat) (tape : DrawTrace) :
    Measurable (fun expression : Expr => outputGivenTraceAt depth expression tape) := by
  have eq : (fun expression : Expr => outputGivenTraceAt depth expression tape) =
      fun expression => (compactReplayKernel depth).kernel (tape, expression) := by
    funext expression
    exact (compactReplayKernel_apply _ _ _).symm
  rw [eq]
  exact (compactReplayKernel depth).kernel.measurable.comp (measurable_const.prodMk measurable_id)

theorem ogtAt_continuation_measurable (depth : Nat) {expression : Expr}
    {site : Mode × Kind × Op} {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (reduction : reduce expression = .sample site fiber continuation) (tape : DrawTrace) :
    Measurable (fun value : ℝ => outputGivenTraceAt depth (continuation value) tape) := by
  have eq : (fun value : ℝ => outputGivenTraceAt depth (continuation value) tape) =
      fun value => (compactReplayKernel depth).kernel (tape, continuation value) := by
    funext value
    exact (compactReplayKernel_apply _ _ _).symm
  rw [eq]
  exact (compactReplayKernel depth).kernel.measurable.comp
    (measurable_const.prodMk (continuation_measurable reduction))

/-- The compact replay at every depth, summed: `Spec.Traces.outputGivenTrace` as a kernel. -/
def outputGivenTraceKernel (program : Expr) : Kernel DrawTrace ℝ :=
  Kernel.sum fun depth =>
    (SFiniteKernel.pullback (compactReplayKernel depth) (fun tape : DrawTrace => (tape, program))
      (measurable_id.prodMk measurable_const)).kernel

theorem outputGivenTraceKernel_apply (program : Expr) (tape : DrawTrace) :
    outputGivenTraceKernel program tape = outputGivenTrace program tape := by
  rw [outputGivenTraceKernel, Kernel.sum_apply, outputGivenTrace]
  congr 1
  funext depth
  rw [MeasurableActionFamily.pullback_apply, compactReplayKernel_apply]

instance outputGivenTraceKernel_sfinite (program : Expr) :
    IsSFiniteKernel (outputGivenTraceKernel program) := by
  unfold outputGivenTraceKernel
  have := fun depth => (SFiniteKernel.pullback (compactReplayKernel depth)
    (fun tape : DrawTrace => (tape, program)) (measurable_id.prodMk measurable_const)).sfinite
  infer_instance

/-! ### Total mass -/

theorem sample_fiber_mass_le_one {expression : Expr} {site : Mode × Kind × Op}
    {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (reduction : reduce expression = .sample site fiber continuation) : fiber Set.univ ≤ 1 := by
  have mass := (MeasurableActionFamily.stepKernel primitiveLaws).mass_le_one expression
  rw [StepKernel.kernel_eq_stepMeasure, Determinize.Spec.Paper.stepMeasure, reduction,
    Action.measure] at mass
  rwa [Measure.map_apply (continuation_measurable reduction) MeasurableSet.univ,
    Set.preimage_univ] at mass

theorem ogtAt_partial_mass_le_one (n : Nat) (expression : Expr) (tape : DrawTrace) :
    ∑ depth ∈ Finset.range n, outputGivenTraceAt depth expression tape Set.univ ≤ 1 := by
  induction n generalizing expression tape with
  | zero => simp
  | succ n ih =>
      rw [Finset.sum_range_succ']
      by_cases value : expression.isValue = true
      · simp only [ogtAt_succ_value _ value, Measure.coe_zero, Pi.zero_apply,
          Finset.sum_const_zero, zero_add]
        cases expression <;> cases tape <;> simp [outputGivenTraceAt]
      · have zero : outputGivenTraceAt 0 expression tape Set.univ = 0 := by
          rw [ogtAt_zero_of_notReal (fun v h => value (by subst h; rfl)) tape]
          rfl
        rw [zero, add_zero]
        cases reduction : reduce expression with
        | next next =>
            simp only [ogtAt_succ_next _ value reduction]
            exact ih next tape
        | stuck => simp [ogtAt_succ_stuck _ value reduction]
        | sample site fiber continuation =>
            rcases site with ⟨mode, kind, op⟩
            by_cases generation : siteOp (mode, kind, op) = none
            · simp only [ogtAt_succ_sampleE _ value reduction generation]
              simp_rw [Measure.bind_apply MeasurableSet.univ
                (ogtAt_continuation_measurable _ reduction tape).aemeasurable]
              have coeMeasurable (depth : Nat) : Measurable
                  (fun v : ℝ => outputGivenTraceAt depth (continuation v) tape Set.univ) :=
                Measurable.comp (MeasureTheory.Measure.measurable_coe MeasurableSet.univ)
                  (ogtAt_continuation_measurable depth reduction tape)
              rw [← lintegral_finsetSum _ (fun depth _ => coeMeasurable depth)]
              calc ∫⁻ v, ∑ depth ∈ Finset.range n,
                    outputGivenTraceAt depth (continuation v) tape Set.univ ∂fiber
                  ≤ ∫⁻ _, 1 ∂fiber := lintegral_mono fun v => ih (continuation v) tape
                _ = fiber Set.univ := by simp
                _ ≤ 1 := sample_fiber_mass_le_one reduction
            · have general : mode = .G ∧ kind = .stochastic := by
                cases mode <;> cases kind <;> simp_all [siteOp]
              obtain ⟨rfl, rfl⟩ := general
              cases tape with
              | nil => simp [ogtAt_succ_sampleG_nil _ value reduction]
              | cons head rest =>
                  obtain ⟨op', v⟩ := head
                  by_cases eq : op = op'
                  · subst eq
                    simp only [ogtAt_succ_sampleG _ value reduction]
                    exact ih _ _
                  · simp [ogtAt_succ_sampleG_mismatch _ value reduction eq]

theorem outputGivenTrace_mass_le_one (program : Expr) (tape : DrawTrace) :
    outputGivenTrace program tape Set.univ ≤ 1 := by
  rw [outputGivenTrace, Measure.sum_apply _ MeasurableSet.univ]
  exact ENNReal.tsum_le_of_sum_range_le fun n => ogtAt_partial_mass_le_one n program tape

theorem ogtAt_le_outputGivenTrace (depth : Nat) (program : Expr) (tape : DrawTrace) :
    outputGivenTraceAt depth program tape ≤ outputGivenTrace program tape :=
  Measure.le_sum _ depth

/-- A measure below a subprobability measure and of mass one is that measure. -/
theorem Measure.eq_of_le_of_mass {α : Type*} [MeasurableSpace α] {μ ν : Measure α} (le : μ ≤ ν)
    (massNu : ν Set.univ ≤ 1) (massMu : μ Set.univ = 1) : μ = ν := by
  ext s hs
  refine le_antisymm (Measure.le_iff'.1 le s) ?_
  have finite : ν sᶜ ≠ ⊤ :=
    ne_top_of_le_ne_top ENNReal.one_ne_top ((measure_mono (Set.subset_univ _)).trans massNu)
  have h : ν s + ν sᶜ ≤ μ s + ν sᶜ := by
    calc ν s + ν sᶜ = ν Set.univ := measure_add_measure_compl hs
      _ ≤ 1 := massNu
      _ = μ s + μ sᶜ := massMu.symm.trans (measure_add_measure_compl hs).symm
      _ ≤ μ s + ν sᶜ := add_le_add le_rfl (Measure.le_iff'.1 le sᶜ)
  exact (ENNReal.add_le_add_iff_right finite).mp h

/-- Where a fixed-depth replay already has mass one, it is the whole replay law. -/
theorem outputGivenTrace_eq_ogtAt (depth : Nat) (program : Expr) (tape : DrawTrace)
    (mass : outputGivenTraceAt depth program tape Set.univ = 1) :
    outputGivenTrace program tape = outputGivenTraceAt depth program tape :=
  (Measure.eq_of_le_of_mass (ogtAt_le_outputGivenTrace depth program tape)
    (outputGivenTrace_mass_le_one program tape) mass).symm


/-! ### A Markov version and a measurability lemma -/

/-- `Spec.Traces.outputGivenTrace` where it has mass one, the Dirac mass at `0` elsewhere. -/
def normalizedOutputGivenTrace (program : Expr) : Kernel DrawTrace ℝ :=
  Kernel.piecewise
    (measurableSet_eq_fun ((outputGivenTraceKernel program).measurable_coe MeasurableSet.univ)
      (measurable_const (a := (1 : ℝ≥0∞))))
    (outputGivenTraceKernel program) (Kernel.deterministic (fun _ => (0 : ℝ)) measurable_const)

theorem normalizedOutputGivenTrace_eq (program : Expr) (tape : DrawTrace)
    (mass : outputGivenTrace program tape Set.univ = 1) :
    normalizedOutputGivenTrace program tape = outputGivenTrace program tape := by
  simp [normalizedOutputGivenTrace, Kernel.piecewise, outputGivenTraceKernel_apply, mass]

instance normalizedOutputGivenTrace_markov (program : Expr) :
    IsMarkovKernel (normalizedOutputGivenTrace program) := by
  constructor
  intro tape
  by_cases mass : outputGivenTrace program tape Set.univ = 1
  · rw [normalizedOutputGivenTrace_eq program tape mass]
    exact ⟨mass⟩
  · simp only [normalizedOutputGivenTrace, Kernel.piecewise, Kernel.coe_mk, Set.mem_ofPred_eq,
      outputGivenTraceKernel_apply, if_neg mass, Kernel.deterministic_apply]
    infer_instance

/-- The traces on which a kernel is the Dirac mass at a measurable function form a
measurable set. -/
theorem measurableSet_kernel_eq_dirac {α : Type*} [MeasurableSpace α] (κ : Kernel α ℝ)
    [IsSFiniteKernel κ] {g : α → ℝ} (hg : Measurable g) :
    MeasurableSet {a | κ a = Measure.dirac (g a)} := by
  have offDiagonal : MeasurableSet {p : α × ℝ | p.2 ≠ g p.1} :=
    (measurableSet_eq_fun measurable_snd (hg.comp measurable_fst)).compl
  have preimage (a : α) : Prod.mk a ⁻¹' {p : α × ℝ | p.2 ≠ g p.1} = {g a}ᶜ := by
    ext v
    simp
  have eq : {a | κ a = Measure.dirac (g a)} = {a | κ a Set.univ = 1} ∩
      {a | κ a (Prod.mk a ⁻¹' {p : α × ℝ | p.2 ≠ g p.1}) = 0} := by
    ext a
    simp only [Set.mem_ofPred_eq, Set.mem_inter_iff, preimage]
    constructor
    · intro h
      rw [h]
      exact ⟨by simp, by simp⟩
    · rintro ⟨mass, null⟩
      ext s hs
      have diffNull : κ a (s \ {g a}) = 0 := measure_mono_null (fun v hv => hv.2) null
      rw [← measure_inter_add_sdiff s (measurableSet_singleton (g a)), diffNull, add_zero,
        Measure.dirac_apply' _ hs]
      have singleton : κ a {g a} = 1 := by
        have h := measure_add_measure_compl (μ := κ a) (measurableSet_singleton (g a))
        rwa [mass, null, add_zero] at h
      by_cases mem : g a ∈ s
      · rw [Set.inter_eq_right.mpr (Set.singleton_subset_iff.mpr mem), singleton,
          Set.indicator_of_mem mem]
        rfl
      · rw [Set.inter_singleton_eq_empty.mpr mem, measure_empty, Set.indicator_of_notMem mem]
  rw [eq]
  exact (measurableSet_eq_fun (κ.measurable_coe MeasurableSet.univ) measurable_const).inter
    (measurableSet_eq_fun (Kernel.measurable_kernel_prodMk_left offDiagonal) measurable_const)

end

end Determinize.Proof.StepTraces
