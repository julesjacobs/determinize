import Determinize.Proof.RewardModel.Stack
import Determinize.Proof.RewardModel.Normalization

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Proof.FiniteModel MeasureTheory Spec.RewardModel

def additionStack (offsets : List Rat) : List Frame :=
  offsets.map fun c => .right .add (.number c)

theorem numeric_addition_output (offsets : List Rat) (b : Rat) (n : Nat) :
    machineOutput (n + offsets.length) (.deliver (.number b) (additionStack offsets)) =
      Measure.dirac ((b + offsets.sum : Rat) : ℝ) := by
  induction offsets generalizing b with
  | nil => cases n <;> simp [additionStack, machineOutput, step]
  | cons c cs ih =>
      rw [List.length_cons, Nat.add_succ]
      simp only [additionStack, List.map_cons, machineOutput, step, binary,
        pure, bind, Except.bind, Except.pure]
      simp only [weightedOutput, List.map_cons, List.map_nil, List.sum_cons, List.sum_nil,
        Rat.cast_one, ENNReal.ofReal_one, one_smul, add_zero]
      rw [← additionStack, ih]
      congr 1
      simp only [Rat.cast_add]
      ring

private theorem draw_not_terminal (site : Spec.Paper.DistributionAction × Spec.Paper.Op)
    (args : List Rat) (stack : List Frame) (result : Finite.Step)
    (action : draw site args stack = .ok result) :
    ∃ tag outcomes, result = .next tag outcomes := by
  unfold draw at action
  cases law : finiteLaw site.2 site.1 args with
  | error e => simp [law, bind, Except.bind] at action
  | ok xs =>
      simp only [law, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at action
      exact ⟨_, _, action.symm⟩

set_option maxHeartbeats 1600000 in
theorem step_terminal (state : State) (result : Finite.Step)
    (action : step state = .ok result) :
    (∃ tag outcomes, result = .next tag outcomes) ∨
    (∃ b, state = .deliver (.number b) [] ∧ result = .returned b) ∨
    (state = .rejected ∧ result = .rejected) := by
  cases state with
  | rejected => simp only [step, Except.ok.injEq] at action; exact Or.inr (Or.inr ⟨rfl, action.symm⟩)
  | eval expression env stack =>
      apply Or.inl
      cases expression <;>
        simp only [step, pure, bind, Except.bind, Except.pure] at action
      all_goals repeat' (split at action)
      all_goals first | contradiction | exact ⟨_, _, (Except.ok.inj action).symm⟩
  | deliver value stack =>
    cases stack with
    | nil =>
      cases value <;> simp only [step, Except.ok.injEq] at action
      all_goals first | contradiction | exact Or.inr (Or.inl ⟨_, rfl, action.symm⟩)
    | cons frame stack =>
      apply Or.inl
      cases frame with
      | discrete kind =>
        cases pv : value.probabilities? with
        | none => simp only [step, pv] at action; contradiction
        | some ps =>
          simp only [step, pv] at action
          exact draw_not_terminal _ _ _ _ action
      | draw site pending env args =>
        cases value <;> simp only [step, pure, bind, Except.bind, Except.pure] at action
        all_goals try contradiction
        rename_i x
        cases pending with
        | nil => exact draw_not_terminal _ _ _ _ action
        | cons next rest => exact ⟨_, _, (Except.ok.inj action).symm⟩
      | unary op =>
        cases op <;> cases value <;>
          simp only [step, unary, pure, bind, Except.bind, Except.pure] at action
        all_goals first | contradiction | exact ⟨_, _, (Except.ok.inj action).symm⟩
      | right op left =>
        cases op <;> cases left <;> cases value <;>
          simp only [step, binary, pure, bind, Except.bind, Except.pure] at action
        all_goals repeat' (split at action)
        all_goals first | contradiction | exact ⟨_, _, (Except.ok.inj action).symm⟩
      | left op rhs env => exact ⟨_, _, (Except.ok.inj action).symm⟩
      | letBody body env => exact ⟨_, _, (Except.ok.inj action).symm⟩
      | choose yes no env =>
        cases value <;> simp only [step, pure, bind, Except.bind, Except.pure] at action
        all_goals first | contradiction | exact ⟨_, _, (Except.ok.inj action).symm⟩
      | matchSum left right env =>
        cases value <;> simp only [step, pure, bind, Except.bind, Except.pure] at action
        all_goals first | contradiction | exact ⟨_, _, (Except.ok.inj action).symm⟩
      | matchList nilCase consCase env =>
        cases value <;> simp only [step, pure, bind, Except.bind, Except.pure] at action
        all_goals first | contradiction | exact ⟨_, _, (Except.ok.inj action).symm⟩

private theorem shift_weighted (r : Rat) (xs : List (Rat × State)) (f : State → Measure ℝ) :
    shift r (weightedOutput xs f) = weightedOutput xs (fun s => shift r (f s)) := by
  induction xs with
  | nil => simp [weightedOutput]
  | cons x xs ih =>
      simp only [weightedOutput, List.map_cons, List.sum_cons] at ih ⊢
      rw [shift_add_measure, shift_smul, ih]

private theorem weighted_extend (outer : List Frame) (xs : List (Rat × State))
    (f : State → Measure ℝ) :
    weightedOutput (extend outer xs) f = weightedOutput xs (fun s => f (pushStack s outer)) := by
  simp [weightedOutput, extend, List.map_map, Function.comp_def]

theorem addition_output_lower (offsets : List Rat) (n : Nat) (state : State) :
    shift offsets.sum (machineOutput n state) ≤
      machineOutput (n + offsets.length) (pushStack state (additionStack offsets)) := by
  induction n generalizing state with
  | zero =>
    cases action : step state with
    | error e => simp [machineOutput, action, Measure.zero_le]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp [machineOutput, action, Measure.zero_le]
      · simpa [machineOutput, step, pushStack, shift_dirac, add_comm] using
          (numeric_addition_output offsets b 0).ge
      · simp [machineOutput, step, Measure.zero_le]
  | succ n ih =>
    cases action : step state with
    | error e => simp [machineOutput, action, Measure.zero_le]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · have lifted := step_push state (additionStack offsets) tag xs action
        rw [Nat.succ_add]
        simp only [machineOutput, action, lifted]
        rw [shift_weighted, weighted_extend]
        exact weightedOutput_mono _ _ _ (fun e _ _ => ih e.2)
      · simpa [machineOutput, step, pushStack, shift_dirac, add_comm] using
          (numeric_addition_output offsets b (n+1)).ge
      · simp [machineOutput, step, Measure.zero_le]

private theorem draw_error_push (site : Spec.Paper.DistributionAction × Spec.Paper.Op)
    (args : List Rat) (inner outer : List Frame) (failure : Failure)
    (action : draw site args inner = .error failure) :
    draw site args (inner ++ outer) = .error failure := by
  unfold draw at action ⊢
  cases law : finiteLaw site.2 site.1 args <;>
    simp [law, bind, Except.bind, pure, Except.pure] at action ⊢
  exact action

set_option maxHeartbeats 1600000 in
theorem addition_failure (state : State) (offsets : List Rat) (failure : Failure)
    (action : step state = .error failure) :
    ∃ error, step (pushStack state (additionStack offsets)) = .error error := by
  cases state with
  | rejected => simp [step] at action
  | eval expression env stack =>
    refine ⟨failure, ?_⟩
    cases expression <;>
      simp only [step, pushStack, pure, bind, Except.bind, Except.pure] at action ⊢
    all_goals repeat' (split at action)
    all_goals simp_all
  | deliver value stack =>
    cases stack with
    | nil =>
      cases value <;> simp only [step] at action
      all_goals try contradiction
      all_goals cases offsets with
      | nil => exact ⟨_, action⟩
      | cons c cs => exact ⟨_, rfl⟩
    | cons frame stack =>
      refine ⟨failure, ?_⟩
      cases frame with
      | discrete kind =>
        cases pv : value.probabilities? with
        | none =>
          simp only [step, pv] at action
          simpa only [step, pushStack, List.cons_append, pv] using action
        | some ps =>
          simp only [step, pv] at action
          simpa [pushStack, step, pv, bind, Except.bind, Except.pure] using
            draw_error_push (kind, .discrete ps.length) ps stack (additionStack offsets) failure action
      | draw site pending env args =>
        cases value <;> simp only [step, pure, bind, Except.bind, Except.pure] at action
        all_goals try (simpa [pushStack, step, pure, bind, Except.bind, Except.pure] using action)
        rename_i x
        cases pending with
        | nil =>
          simpa [pushStack, step, bind, Except.bind, Except.pure] using
            draw_error_push site (args ++ [x]) stack (additionStack offsets) failure action
        | cons next rest => contradiction
      | unary op =>
        cases op <;> cases value <;>
          simp_all [pushStack, step, unary, pure, bind, Except.bind, Except.pure]
      | right op left =>
        cases op <;> cases left <;> cases value <;>
          simp only [step, binary, pure, bind, Except.bind, Except.pure] at action
        all_goals try split_ifs at action
        all_goals repeat' (split at action)
        all_goals simp_all [step, binary, pushStack, bind, Except.bind]
      | left op rhs env => contradiction
      | letBody body env => contradiction
      | choose yes no env =>
        cases value <;> simp_all [pushStack, step, pure, bind, Except.bind, Except.pure]
      | matchSum left right env =>
        cases value <;> simp_all [pushStack, step, pure, bind, Except.bind, Except.pure]
      | matchList nilCase consCase env =>
        cases value <;> simp_all [pushStack, step, pure, bind, Except.bind, Except.pure]

theorem numeric_addition_upper (offsets : List Rat) (b : Rat) (n : Nat) :
    machineOutput n (.deliver (.number b) (additionStack offsets)) ≤
      Measure.dirac ((b + offsets.sum : Rat) : ℝ) := by
  induction offsets generalizing b n with
  | nil => cases n <;> simp [additionStack, machineOutput, step]
  | cons c cs ih =>
    cases n with
    | zero => simp [additionStack, machineOutput, step, binary, pure, bind, Except.bind, Except.pure,
        Measure.zero_le]
    | succ n =>
      simp only [additionStack, List.map_cons, machineOutput, step, binary,
        pure, bind, Except.bind, Except.pure]
      simp only [weightedOutput, List.map_cons, List.map_nil, List.sum_cons, List.sum_nil,
        Rat.cast_one, ENNReal.ofReal_one, one_smul, add_zero]
      convert ih (c+b) n using 1 <;> congr 1
      simp only [Rat.cast_add]
      ring

theorem addition_output_upper (offsets : List Rat) (n : Nat) (state : State) :
    machineOutput n (pushStack state (additionStack offsets)) ≤
      shift offsets.sum (machineOutput n state) := by
  induction n generalizing state with
  | zero =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [machineOutput, action, herr]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp [machineOutput, action, step_push state (additionStack offsets) tag xs action]
      · simpa [pushStack, machineOutput, step, shift_dirac] using numeric_addition_upper offsets b 0
      · simp [pushStack, machineOutput, step]
  | succ n ih =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [machineOutput, action, herr]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp only [machineOutput, action, step_push state (additionStack offsets) tag xs action]
        rw [shift_weighted, weighted_extend]
        exact weightedOutput_mono _ _ _ (fun e _ _ => ih e.2)
      · simpa [pushStack, machineOutput, step, shift_dirac] using numeric_addition_upper offsets b (n+1)
      · simp [pushStack, machineOutput, step]

theorem numeric_addition_exact (offsets : List Rat) (b : Rat) (n : Nat) :
    machineOutput n (.deliver (.number b) (additionStack offsets)) =
      if offsets.length ≤ n then Measure.dirac ((b + offsets.sum : Rat) : ℝ) else 0 := by
  induction offsets generalizing b n with
  | nil => cases n <;> simp [additionStack, machineOutput, step]
  | cons c cs ih =>
    cases n with
    | zero => simp [additionStack, machineOutput, step, binary, pure, bind, Except.bind, Except.pure]
    | succ n =>
      simp only [additionStack, List.map_cons, machineOutput, step, binary,
        pure, bind, Except.bind, Except.pure]
      simp only [weightedOutput, List.map_cons, List.map_nil, List.sum_cons, List.sum_nil,
        Rat.cast_one, ENNReal.ofReal_one, one_smul, add_zero]
      rw [← additionStack, ih]
      simp only [List.length_cons, Nat.add_le_add_iff_right]
      split_ifs <;> try rfl
      congr 1
      simp only [Rat.cast_add]
      ring

theorem addition_guard_upper (offsets : List Rat) (nonempty : offsets ≠ [])
    (n : Nat) (state : State) :
    machineOutput n (pushStack state (additionStack offsets)) ≤
      shift offsets.sum (machineOutput n (pushStack state (additionStack [0]))) := by
  have numeric (b : Rat) (n : Nat) :
      machineOutput n (pushStack (.deliver (.number b) []) (additionStack offsets)) ≤
        shift offsets.sum (machineOutput n (pushStack (.deliver (.number b) []) (additionStack [0]))) := by
    simp only [pushStack, List.nil_append, numeric_addition_exact]
    by_cases enough : offsets.length ≤ n
    · have one : 1 ≤ n := by
        have := List.length_pos_iff.mpr nonempty
        omega
      simp only [if_pos enough, List.length_singleton, if_pos one, List.sum_cons, List.sum_nil,
        add_zero, shift_dirac]
      rfl
    · simp [enough, Measure.zero_le]
  induction n generalizing state with
  | zero =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [machineOutput, herr, Measure.zero_le]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp [machineOutput, step_push state (additionStack offsets) tag xs action, Measure.zero_le]
      · exact numeric b 0
      · simp [pushStack, machineOutput, step]
  | succ n ih =>
    cases action : step state with
    | error e =>
      obtain ⟨err, herr⟩ := addition_failure state offsets e action
      simp [machineOutput, herr, Measure.zero_le]
    | ok result =>
      rcases step_terminal state result action with ⟨tag, xs, rfl⟩ | ⟨b, rfl, rfl⟩ | ⟨rfl, rfl⟩
      · simp only [machineOutput, step_push state (additionStack offsets) tag xs action,
          step_push state (additionStack [0]) tag xs action]
        rw [shift_weighted, weighted_extend, weighted_extend]
        exact weightedOutput_mono _ _ _ (fun e _ _ => ih e.2)
      · exact numeric b (n+1)
      · simp [pushStack, machineOutput, step]

end Determinize.Proof.RewardModel
