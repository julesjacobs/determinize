import Determinize.Proof.RewardModel.Execution
import Determinize.Proof.RewardModel.Safety
import Determinize.Proof.RewardModel.Replay
import Determinize.Proof.FiniteModel.Initial

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Proof.FiniteModel MeasureTheory Spec.RewardModel

private theorem row_next (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) (i : Fin c.states.size) (tag : Evidence)
    (xs : List (Rat × State)) (action : step c.states[i] = .ok (.next tag xs)) :
    (c.row i).kind = .transient ∧
    ((c.row i).edges.map fun e => (e.probability, c.states[e.target]?.getD .rejected, e.reward)) =
      ((xs.filter fun x => 0 < x.1).map fun x =>
        (x.1, (Reward.normalize x.2).2, (Reward.normalize x.2).1)) := by
  have h := (valid.2.2.2.2.2 i).2.2
  simp only [Reward.step, action, bind, Except.bind, pure, Except.pure] at h
  refine ⟨h.1, ?_⟩
  simpa [List.filter_map, List.map_map, Function.comp_def] using h.2.2

private theorem row_no_failure (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) (i : Fin c.states.size) (e : Failure) :
    step c.states[i] ≠ .error e := by
  intro action
  have h := (valid.2.2.2.2.2 i).2.2
  simp only [Reward.step, action, bind, Except.bind] at h

private theorem row_covered (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) (i : Fin c.states.size) (tag : Evidence)
    (xs : List (Rat × State)) (action : step c.states[i] = .ok (.next tag xs))
    (x : Rat × State) (member : x ∈ xs) (positive : 0 < x.1) :
    ∃ j : Fin c.states.size, c.states[j] = (Reward.normalize x.2).2 := by
  have encoded := (row_next c valid i tag xs action).2
  have hm : (x.1, (Reward.normalize x.2).2, (Reward.normalize x.2).1) ∈
      (c.row i).edges.map (fun e => (e.probability, c.states[e.target]?.getD .rejected, e.reward)) := by
    rw [encoded]
    exact List.mem_map.mpr ⟨x, by simp [member, positive], rfl⟩
  obtain ⟨edge, he, eq⟩ := List.mem_map.mp hm
  have bound := ((valid.2.2.2.2.2 i).1 edge he).1
  refine ⟨⟨edge.target, bound⟩, ?_⟩
  have same := congrArg (fun t : Rat × State × Rat => t.2.1) eq
  simpa [bound] using same

private theorem replay_no_rewardFailure (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) (n : Nat) (i : Fin c.states.size) :
    ¬ rewardFailureWithin n c.states[i] := by
  induction n generalizing i with
  | zero => exact fun ⟨e, h⟩ => row_no_failure c valid i e h
  | succ n ih =>
    cases action : step c.states[i] with
    | error e => exact (row_no_failure c valid i e action).elim
    | ok result =>
      cases result with
      | returned b => simp only [rewardFailureWithin, action, not_false_eq_true]
      | rejected => simp only [rewardFailureWithin, action, not_false_eq_true]
      | next tag xs =>
        simp only [rewardFailureWithin, action]
        rintro ⟨x, member, positive, failed⟩
        obtain ⟨j, same⟩ := row_covered c valid i tag xs action x member positive
        exact ih j (same ▸ failed)

private theorem reachable_failure {root state : State} (reachable : MachineReachable root state)
    (n : Nat) (failed : failureWithin n state) : ∃ k, failureWithin k root := by
  induction reachable generalizing n with
  | initial => exact ⟨n, failed⟩
  | next prev action member positive ih =>
      exact ih (n+1) (by simp only [failureWithin, action]; exact ⟨_, member, positive, failed⟩)

theorem replay_reachable_no_failure (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) {state : State}
    (reachable : MachineReachable (initialState source subject) state) (e : Failure) :
    step state ≠ .error e := by
  intro failed
  obtain ⟨n, hn⟩ := reachable_failure reachable 0 ⟨e, failed⟩
  have initial : c.states[c.initial]'valid.2.1 = initialState source subject := by
    simpa [valid.2.1] using valid.2.2.1
  apply replay_no_rewardFailure c valid n ⟨c.initial, valid.2.1⟩
  change rewardFailureWithin n (c.states[c.initial]'valid.2.1)
  rw [initial]
  exact failure_implies_rewardFailure n _ hn

private theorem weighted_filter (xs : List (Rat × State)) (f : State → Measure ℝ) :
    weightedOutput (xs.filter fun x => 0 < x.1) f = weightedOutput xs f := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
    by_cases positive : 0 < x.1
    · simpa [weightedOutput, positive] using congrArg (fun μ => ENNReal.ofReal (x.1 : ℝ) • f x.2 + μ) ih
    · have zero : ENNReal.ofReal (x.1 : ℝ) = 0 :=
        ENNReal.ofReal_eq_zero.mpr (by exact_mod_cast le_of_not_gt positive)
      simpa [List.filter_cons, positive, weightedOutput, zero] using ih

private theorem replay_row_sum (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) (i : Fin c.states.size) (tag : Evidence)
    (xs : List (Rat × State)) (action : step c.states[i] = .ok (.next tag xs))
    (f : State → Measure ℝ) :
    (((c.toModel valid).edges i).map fun e => ENNReal.ofReal (e.probability : ℝ) •
      shift e.reward (f c.states[e.target])).sum =
      weightedOutput xs (fun s => shift (Reward.normalize s).1 (f (Reward.normalize s).2)) := by
  have encoded := (row_next c valid i tag xs action).2
  have mapped := congrArg (fun ys : List (Rat × State × Rat) =>
    (ys.map fun x => ENNReal.ofReal (x.1 : ℝ) • shift x.2.2 (f x.2.1)).sum) encoded
  simp only [List.map_map, Function.comp_def] at mapped
  have left : (((c.toModel valid).edges i).map fun e => ENNReal.ofReal (e.probability : ℝ) •
      shift e.reward (f c.states[e.target])).sum =
      ((c.row i).edges.map fun e => ENNReal.ofReal (e.probability : ℝ) •
        shift e.reward (f (c.states[e.target]?.getD .rejected))).sum := by
    change ((c.modelEdges valid i).map _).sum = _
    unfold Reward.Candidate.modelEdges
    simp only [List.map_map, Function.comp_def]
    congr 1
    conv_rhs => rw [← List.attach_map_val (l := (c.row i).edges)]
    apply List.map_congr_left
    intro e _
    simp [((valid.2.2.2.2.2 i).1 e.val e.property).1]
  rw [left, mapped]
  simpa only [weightedOutput] using weighted_filter xs
    (fun s => shift (Reward.normalize s).1 (f (Reward.normalize s).2))

theorem replay_outputWithin (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) (n : Nat) (i : Fin c.states.size) :
    (c.toModel valid).outputWithin n i = rewardOutput n c.states[i] := by
  induction n generalizing i with
  | zero =>
    have row := (valid.2.2.2.2.2 i).2.2
    cases action : step c.states[i] with
    | error e => exact (row_no_failure c valid i e action).elim
    | ok result =>
      cases result <;> simp only [Reward.step, action, bind, Except.bind, pure, Except.pure] at row
      all_goals simp only [rewardOutput, action]
      all_goals simp [Model.outputWithin, row]
  | succ n ih =>
    have row := (valid.2.2.2.2.2 i).2.2
    cases action : step c.states[i] with
    | error e => exact (row_no_failure c valid i e action).elim
    | ok result =>
      cases result with
      | returned b =>
        simp only [Reward.step, action, bind, Except.bind, pure, Except.pure] at row
        simp only [rewardOutput, action]
        simp [Model.outputWithin, row]
      | rejected =>
        simp only [Reward.step, action, bind, Except.bind, pure, Except.pure] at row
        simp only [rewardOutput, action]
        simp [Model.outputWithin, row]
      | next tag xs =>
        have kind := (row_next c valid i tag xs action).1
        simp only [Model.outputWithin, rewardOutput, action]
        change (match (c.row i).kind with
          | .returned b => Measure.dirac (b : ℝ)
          | .rejected => 0
          | .transient => _) = _
        rw [kind]
        simp_rw [ih]
        exact replay_row_sum c valid i tag xs action (rewardOutput n)

theorem replay_matches (c : Reward.Candidate) {source : Checking.Core} {subject : Spec.FiniteModel.Subject}
    (valid : c.ReplayValid source subject) :
    (c.toModel valid).Matches (subject.program source) := by
  have meaning : ∀ state, MachineReachable (initialState source subject) state → ∀ result,
      step state = .ok result → StepMeaning state result :=
    fun state reachable result action => stepMeaning state
      (program_reachable_shape source subject state reachable) result action
  have noFailure := fun state reachable failure => replay_reachable_no_failure c valid
    (state := state) reachable failure
  have initial : c.states[c.initial]'valid.2.1 = initialState source subject := by
    simpa [valid.2.1] using valid.2.2.1
  have initialEqual : stateExpr (initialState source subject) = subject.program source :=
    initial_reification source subject
      ((Binding.scoped_mapLiteral source (fun q : Rat => (q : ℝ)) 0).mpr valid.2.2.2.1)
  constructor
  · intro fuel
    rw [← initialEqual]
    exact execution_safe _ meaning noFailure fuel _ .initial
  · change (⨆ n, (c.toModel valid).outputWithin n _) = _
    simp_rw [replay_outputWithin]
    change (⨆ n, rewardOutput n (c.states[c.initial]'valid.2.1)) = _
    rw [initial, reward_output_eq_machine, execution_output_eq _ meaning noFailure _ .initial, initialEqual]

end Determinize.Proof.RewardModel
