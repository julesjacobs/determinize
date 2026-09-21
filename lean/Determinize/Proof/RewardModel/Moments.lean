import Determinize.Proof.RewardModel.Laws
import Determinize.Proof.RewardModel.FiniteIntegrability
import Determinize.Finite.Reward.Solve
import Determinize.Spec.RewardModel.Results

namespace Determinize.Proof.RewardModel
open MeasureTheory Spec.RewardModel FiniteModel

noncomputable def momentAt (model : Model) (moment : Moment) (i : Fin model.size) : ℝ :=
  ∫ x, moment.real x ∂model.outputAt i

private theorem moment_measurable (moment : Moment) : Measurable moment.real := by
  cases moment <;> simp only [Moment.real] <;> fun_prop

private theorem translated_integrable (μ : Measure ℝ) [IsFiniteMeasure μ]
    (first : Integrable (fun x : ℝ => x) μ) (second : Integrable (fun x : ℝ => x^2) μ)
    (r : Rat) (moment : Moment) : Integrable moment.real (shift r μ) := by
  apply (integrable_map_measure (moment_measurable moment).aestronglyMeasurable
    (show AEMeasurable (fun x : ℝ => x+(r:ℝ)) μ from (measurable_id.add_const _).aemeasurable)).mpr
  cases moment with
  | mass => exact integrable_const 1
  | first => exact first.add (integrable_const (r : ℝ))
  | second =>
    have poly : (fun x : ℝ => (x+(r:ℝ))^2) = fun x => (x^2 + (2*(r:ℝ))*x) + (r:ℝ)^2 := by
      funext x
      ring
    change Integrable (fun x : ℝ => (x+(r:ℝ))^2) μ
    rw [poly]
    exact (second.add (first.const_mul _)).add (integrable_const _)

private theorem integral_shift_moment (μ : Measure ℝ) [IsFiniteMeasure μ]
    (first : Integrable (fun x : ℝ => x) μ) (second : Integrable (fun x : ℝ => x^2) μ)
    (r : Rat) (moment : Moment) :
    (∫ x, moment.real x ∂shift r μ) = match moment with
    | .mass => ∫ x, Moment.mass.real x ∂μ
    | .first => (∫ x, Moment.first.real x ∂μ) + (r:ℝ) * (∫ x, Moment.mass.real x ∂μ)
    | .second => (∫ x, Moment.second.real x ∂μ) + 2*(r:ℝ)*(∫ x, Moment.first.real x ∂μ) +
        (r:ℝ)^2*(∫ x, Moment.mass.real x ∂μ) := by
  rw [shift, integral_map
    (show AEMeasurable (fun x : ℝ => x+(r:ℝ)) μ from (measurable_id.add_const _).aemeasurable)
    (moment_measurable moment).aestronglyMeasurable]
  cases moment with
  | mass => rfl
  | first => simp [Moment.real, integral_add first (integrable_const (r:ℝ)), integral_const, mul_comm]
  | second =>
    have poly : (fun x : ℝ => (x+(r:ℝ))^2) = fun x => (x^2 + (2*(r:ℝ))*x) + (r:ℝ)^2 := by
      funext x
      ring
    simp only [Moment.real]
    have linear : Integrable (fun x : ℝ => 2*(r:ℝ)*x) μ := first.const_mul _
    have quadratic : Integrable (fun x : ℝ => x^2 + 2*(r:ℝ)*x) μ := second.add linear
    rw [poly, integral_add quadratic (integrable_const ((r:ℝ)^2)),
      integral_add second linear, integral_const_mul]
    simp [integral_const, mul_comm]

set_option maxHeartbeats 1200000 in
theorem momentAt_equation (model : Model) (moment : Moment)
    (i : Fin model.size) :
    momentAt model moment i = match model.kind i with
    | .returned b => moment.real (b : ℝ)
    | .rejected => 0
    | .transient => ((model.edges i).map fun e => (e.probability : ℝ) * match moment with
        | .mass => momentAt model .mass e.target
        | .first => momentAt model .first e.target + (e.reward:ℝ)*momentAt model .mass e.target
        | .second => momentAt model .second e.target + 2*(e.reward:ℝ)*momentAt model .first e.target +
            (e.reward:ℝ)^2*momentAt model .mass e.target).sum := by
  unfold momentAt
  rw [outputAt_equation]
  cases kind : model.kind i with
  | returned b => simp
  | rejected => simp
  | transient =>
    have integrable (e : Edge model.size) : Integrable moment.real
        (ENNReal.ofReal (e.probability : ℝ) • shift e.reward (model.outputAt e.target)) :=
      (translated_integrable (model.outputAt e.target)
        (finite_integrable model e.target).1 (finite_integrable model e.target).2
        e.reward moment).smul_measure (by simp)
    rw [integral_list_sum (model.edges i)
      (fun e => ENNReal.ofReal (e.probability : ℝ) • shift e.reward (model.outputAt e.target))
      moment.real (fun e _ => integrable e)]
    apply congrArg List.sum
    apply List.map_congr_left
    intro e he
    rw [integral_smul_measure, ENNReal.toReal_ofReal (by exact_mod_cast model.nonnegative i e he),
      integral_shift_moment _ (finite_integrable model e.target).1
        (finite_integrable model e.target).2]
    rfl

theorem linear_unique (model : Spec.FiniteModel.Model) (paths : Paths model) (valid : paths.Valid model)
    (rhs v w : Fin model.size → ℝ)
    (hv : ∀ i, v i = rhs i + if model.kind i = .transient then ∑ j, (model.transition i j : ℝ)*v j else 0)
    (hw : ∀ i, w i = rhs i + if model.kind i = .transient then ∑ j, (model.transition i j : ℝ)*w j else 0) :
    ∀ i, v i = w i := by
  have zero := paths_unique model paths valid (fun i => v i - w i) (by
    intro i
    rw [hv i, hw i]
    split_ifs
    · simp only [mul_sub, Finset.sum_sub_distrib]
      ring
    · simp)
  intro i
  exact sub_eq_zero.mp (zero i)

private theorem edge_unique (model : Model) (paths : Paths model.control) (valid : paths.Valid model.control)
    (terminal : Rat → ℝ) (extra : Edge model.size → ℝ) (v w : Fin model.size → ℝ)
    (hv : ∀ i, v i = match model.kind i with
      | .returned b => terminal b | .rejected => 0
      | .transient => ((model.edges i).map fun e => (e.probability:ℝ)*(v e.target + extra e)).sum)
    (hw : ∀ i, w i = match model.kind i with
      | .returned b => terminal b | .rejected => 0
      | .transient => ((model.edges i).map fun e => (e.probability:ℝ)*(w e.target + extra e)).sum) :
    ∀ i, v i = w i := by
  let rhs := fun (i : Fin model.size) => match model.kind i with
    | .returned b => terminal b | .rejected => 0
    | .transient => ((model.edges i).map fun (e : Edge model.size) => (e.probability:ℝ)*extra e).sum
  have convert (u : Fin model.size → ℝ) (eqs : ∀ i, u i = match model.kind i with
      | .returned b => terminal b | .rejected => 0
      | .transient => ((model.edges i).map fun e => (e.probability:ℝ)*(u e.target + extra e)).sum) :
      ∀ i, u i = rhs i + if model.control.kind i = .transient then
        ∑ j, (model.control.transition i j : ℝ)*u j else 0 := by
    intro i
    rw [eqs i]
    cases kind : model.kind i <;>
      simp [rhs, Model.control, kind, mul_add, List.sum_map_add, control_sum_real, add_comm]
  exact linear_unique model.control paths valid rhs v w (convert v hv) (convert w hw)

private theorem equations_real (model : Model) (values : Moment → Fin model.size → Rat)
    (valid : MomentEquations model values) (moment : Moment) (i : Fin model.size) :
    (values moment i : ℝ) = match model.kind i with
    | .returned b => moment.real (b:ℝ)
    | .rejected => 0
    | .transient => ((model.edges i).map fun e => (e.probability:ℝ) * match moment with
      | .mass => (values .mass e.target:ℝ)
      | .first => (values .first e.target:ℝ) + (e.reward:ℝ)*(values .mass e.target:ℝ)
      | .second => (values .second e.target:ℝ) + 2*(e.reward:ℝ)*(values .first e.target:ℝ) +
          (e.reward:ℝ)^2*(values .mass e.target:ℝ)).sum := by
  have h := congrArg (fun q : Rat => (q : ℝ)) (valid moment i)
  cases kind : model.kind i <;> cases moment <;>
    simpa [kind, translatedValue, Moment.rational, Moment.real, Rat.cast_list_sum,
      List.map_map, Function.comp_def] using h

theorem moment_values_sound (model : Model)
    (paths : Paths model.control) (pathsValid : paths.Valid model.control)
    (values : Moment → Fin model.size → Rat) (valid : MomentEquations model values) :
    ∀ moment i, momentAt model moment i = (values moment i : ℝ) := by
  have mass : ∀ i, momentAt model .mass i = (values .mass i:ℝ) := by
    apply edge_unique model paths pathsValid (fun _ => 1) (fun _ => 0)
    · intro i
      have h := momentAt_equation model .mass i
      cases kind : model.kind i <;> simpa [kind, Moment.real] using h
    · intro i
      have h := equations_real model values valid .mass i
      cases kind : model.kind i <;> simpa [kind, Moment.real] using h
  have first : ∀ i, momentAt model .first i = (values .first i:ℝ) := by
    apply edge_unique model paths pathsValid (fun b => (b:ℝ)) (fun e => (e.reward:ℝ)*(values .mass e.target:ℝ))
    · intro i
      have h := momentAt_equation model .first i
      cases kind : model.kind i <;> simpa [kind, Moment.real, mass] using h
    · intro i
      have h := equations_real model values valid .first i
      cases kind : model.kind i <;> simpa [kind, Moment.real] using h
  have second : ∀ i, momentAt model .second i = (values .second i:ℝ) := by
    apply edge_unique model paths pathsValid (fun b => (b:ℝ)^2)
      (fun e => 2*(e.reward:ℝ)*(values .first e.target:ℝ) + (e.reward:ℝ)^2*(values .mass e.target:ℝ))
    · intro i
      have h := momentAt_equation model .second i
      cases kind : model.kind i <;> simpa [kind, Moment.real, mass, first, add_assoc] using h
    · intro i
      have h := equations_real model values valid .second i
      cases kind : model.kind i <;> simpa [kind, Moment.real, add_assoc] using h
  intro moment
  cases moment
  · exact mass
  · exact first
  · exact second

theorem outputMeasure_integrable (model : Model) :
    Integrable (fun x : ℝ => x) model.outputMeasure ∧ Integrable (fun x : ℝ => x^2) model.outputMeasure :=
  finite_integrable model model.initial

theorem solution_statistics (model : Model) (solution : Determinize.Finite.Reward.Solution model) :
    solution.statistics.Matches model.outputMeasure := by
  let values := fun (moment : Moment) => match moment with
    | .mass => solution.mass | .first => solution.first | .second => solution.second
  have correct := moment_values_sound (cut model solution.boundary.dead)
    solution.paths solution.pathsValid values solution.momentsValid
  have moments (moment : Moment) : (∫ x, moment.real x ∂model.outputMeasure) = (values moment model.initial : ℝ) := by
    have h := correct moment model.initial
    unfold momentAt at h
    rwa [cut_outputAt model solution.boundary.dead solution.boundary.closed] at h
  refine ⟨inferInstanceAs (IsFiniteMeasure (model.outputAt model.initial)),
    (outputMeasure_integrable model).2, ?_, ?_, ?_⟩
  · simpa [Moment.real, Determinize.Finite.Reward.Solution.statistics, values, integral_const] using moments .mass
  · exact moments .first
  · exact moments .second

theorem solution_result (model : Model) (program : Spec.Paper.Expr) (matching : model.Matches program)
    (solution : Determinize.Finite.Reward.Solution model) : ResultMatches model program solution.statistics :=
  ⟨matching, solution_statistics model solution⟩

theorem solution_conditional_variance (model : Model) (solution : Determinize.Finite.Reward.Solution model)
    (positive : 0 < solution.statistics.returnMass) :
    ProbabilityTheory.variance id ((model.outputMeasure Set.univ)⁻¹ • model.outputMeasure) =
      ((solution.statistics.secondMoment / solution.statistics.returnMass -
        (solution.statistics.firstMoment / solution.statistics.returnMass)^2 : Rat) : ℝ) :=
  statistics_conditional_variance model.outputMeasure solution.statistics
    (solution_statistics model solution) positive

theorem solution_termination (model : Model) (solution : Determinize.Finite.Reward.Solution model) :
    (⟨solution.mass model.initial, solution.rejection model.initial,
      1-solution.mass model.initial-solution.rejection model.initial⟩ : Spec.FiniteModel.TerminationStatistics).Matches
      model.control := by
  have output : model.control.outputMeasure.real Set.univ = (solution.mass model.initial : ℝ) := by
    have query := query_sound (FiniteModel.cut model.control solution.boundary.dead) (fun _ => 1)
      (fun _ => 1) (fun _ => by simp) ⟨solution.mass, 0⟩ solution.massValid
      ⟨solution.paths.rank, solution.paths.next⟩ (by
        intro state transient
        apply solution.pathsValid state
        cases h : model.kind state <;> cases d : solution.boundary.dead state <;> simp_all)
    rw [FiniteModel.cut_outputMeasure _ _ solution.boundary.closed] at query
    simpa [integral_const] using query
  have rejection : model.control.rejectionProbability.toReal = (solution.rejection model.initial : ℝ) := by
    have query := query_sound (FiniteModel.cut model.control.rejectionModel solution.boundary.dead) (fun _ => 1)
      (fun _ => 1) (fun _ => by simp) ⟨solution.rejection, 0⟩ solution.rejectionValid
      ⟨solution.paths.rank, solution.paths.next⟩ (by
        intro state transient
        apply solution.pathsValid state
        cases h : model.kind state <;> cases d : solution.boundary.dead state <;> simp_all)
    rw [FiniteModel.cut_outputMeasure _ _ (rejection_closed model.control _ solution.boundary.closed)] at query
    simpa [integral_const, Spec.FiniteModel.Model.rejectionProbability, measureReal_def] using query
  refine ⟨output, rejection, ?_⟩
  have balance := massBalance model.control
  have finiteReturn : model.control.outputMeasure Set.univ ≠ ⊤ := by
    exact ne_top_of_le_ne_top (by simp) ((le_add_right le_rfl).trans ((le_add_right le_rfl).trans_eq balance))
  have finiteReject : model.control.rejectionProbability ≠ ⊤ := by
    exact ne_top_of_le_ne_top (by simp) ((le_add_left le_rfl).trans ((le_add_right le_rfl).trans_eq balance))
  have finiteDiverge : model.control.divergenceProbability ≠ ⊤ := by
    exact ne_top_of_le_ne_top (by simp) ((le_add_left le_rfl).trans_eq balance)
  have realBalance := congrArg ENNReal.toReal balance
  rw [ENNReal.toReal_add (ENNReal.add_ne_top.mpr ⟨finiteReturn, finiteReject⟩) finiteDiverge,
    ENNReal.toReal_add finiteReturn finiteReject, ENNReal.toReal_one] at realBalance
  change (model.control.outputMeasure Set.univ).toReal = _ at output
  rw [output, rejection] at realBalance
  simp only [Rat.cast_sub, Rat.cast_one]
  linarith

end Determinize.Proof.RewardModel
