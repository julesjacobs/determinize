import Determinize.Proof.RewardModel.Moments

namespace Determinize.Proof.RewardModel
open Spec.RewardModel FiniteModel
open Spec.FiniteModel (OutputStatistics)

structure Certificate (n : Nat) where
  dead : Fin n → Bool
  rank : Fin n → Nat
  next : Fin n → Fin n
  mass : Fin n → Rat
  first : Fin n → Rat
  second : Fin n → Rat
  rejection : Fin n → Rat

def Certificate.ofVectors (dead : Vector Bool n) (rank : Vector Nat n)
    (next : Vector (Fin n) n) (mass first second rejection : Vector Rat n) : Certificate n :=
  ⟨fun i ↦ dead[i], fun i ↦ rank[i], fun i ↦ next[i], fun i ↦ mass[i],
    fun i ↦ first[i], fun i ↦ second[i], fun i ↦ rejection[i]⟩

def Certificate.values (c : Certificate n) (moment : Moment) : Fin n → Rat :=
  fun i ↦ (match moment with
    | .mass => c.mass | .first => c.first | .second => c.second) i

def Certificate.StateValid (model : Model) (c : Certificate model.size)
    (i : Fin model.size) : Prop :=
  (c.dead i = true → model.kind i = .transient ∧
    ∀ e ∈ model.edges i, c.dead e.target = true) ∧
  ((cut model c.dead).kind i = .transient →
    (∃ e ∈ model.edges i, e.target = c.next i ∧ 0 < e.probability) ∧
      c.rank (c.next i) < c.rank i) ∧
  (∀ moment, c.values moment i = match (cut model c.dead).kind i with
    | .returned b => moment.rational b
    | .rejected => 0
    | .transient => ((model.edges i).map fun e ↦
        e.probability * translatedValue c.values moment e).sum) ∧
  (c.rejection i = if c.dead i then 0 else match model.kind i with
    | .returned _ => 0 | .rejected => 1
    | .transient => ((model.edges i).map fun e ↦ e.probability * c.rejection e.target).sum)

instance (model : Model) (c : Certificate model.size) (i : Fin model.size) :
    Decidable (c.StateValid model i) := inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ _))

theorem Certificate.closed (model : Model) (c : Certificate model.size)
    (valid : ∀ i, c.StateValid model i) :
    ClosedDivergence model.control c.dead := by
  intro i dead
  refine ⟨(valid i).1 dead |>.1, ?_⟩
  intro j positive
  by_contra h
  have zero : model.control.transition i j = 0 := by
    apply List.sum_eq_zero
    intro q member
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp member
    have different : e.target ≠ j := fun eq ↦ h (eq ▸ ((valid i).1 dead).2 e he)
    simp [different]
  rw [zero] at positive
  exact (lt_irrefl 0 positive)

def Certificate.solution (model : Model) (c : Certificate model.size)
    (valid : ∀ i, c.StateValid model i) : Determinize.Finite.Reward.Solution model where
  boundary := ⟨c.dead, c.closed model valid, c.rank⟩
  paths := ⟨c.rank, c.next⟩
  paths_valid := by
    intro i transient
    have h : (cut model c.dead).kind i = .transient := transient
    obtain ⟨⟨e, member, target, positive⟩, descending⟩ := (valid i).2.1 h
    refine ⟨?_, descending⟩
    change 0 < controlWeight model i (c.next i)
    rw [← target]
    exact positive.trans_le (edge_le_controlWeight model i e member)
  mass := c.mass
  mass_valid := by
    intro i
    have h := (valid i).2.2.1 Moment.mass
    cases dead : c.dead i <;> cases kind : model.kind i <;>
      simpa [ValueEquations, rewards, FiniteModel.cut, Model.control, cut, dead, kind,
        Certificate.values, Moment.rational, translatedValue, sum_controlWeight_mul] using h
  first := c.first
  first_valid := by
    intro i
    have h := (valid i).2.2.1 Moment.first
    cases dead : c.dead i <;> cases kind : model.kind i <;>
      simpa [Determinize.Finite.Reward.Equations, FiniteModel.cut, Model.control, cut,
        Determinize.Finite.Reward.firstRhs, dead, kind, Certificate.values,
        Moment.rational, translatedValue, sum_controlWeight_mul, mul_add,
        List.sum_map_add, add_comm, mul_assoc] using h
  second := c.second
  second_valid := by
    intro i
    have h := (valid i).2.2.1 Moment.second
    cases dead : c.dead i <;> cases kind : model.kind i <;>
      simpa [Determinize.Finite.Reward.Equations, FiniteModel.cut, Model.control, cut,
        Determinize.Finite.Reward.secondRhs, dead, kind, Certificate.values,
        Moment.rational, translatedValue, sum_controlWeight_mul, mul_add,
        List.sum_map_add, pow_two, add_comm, add_assoc] using h
  rejection := c.rejection
  rejection_valid := by
    intro i
    have h := (valid i).2.2.2
    cases dead : c.dead i <;> cases kind : model.kind i <;>
      simpa [ValueEquations, rejectionQuery, FiniteModel.cut, Model.control,
        Spec.FiniteModel.Model.rejectionModel, dead, kind, sum_controlWeight_mul] using h

theorem Certificate.statistics (model : Model) (c : Certificate model.size)
    (valid : ∀ i, c.StateValid model i) :
    (⟨c.mass model.initial, c.first model.initial, c.second model.initial⟩ :
      OutputStatistics).Matches model.outputMeasure :=
  solution_statistics model (c.solution model valid)

theorem solution_outputStatistics (model : Model) (program : Spec.Paper.Expr)
    (matching : model.Matches program) (solution : Determinize.Finite.Reward.Solution model) :
    solution.statistics.Matches (Spec.Paper.bigStepMeasure program) := by
  rw [← matching.2]
  exact solution_statistics model solution

end Determinize.Proof.RewardModel
