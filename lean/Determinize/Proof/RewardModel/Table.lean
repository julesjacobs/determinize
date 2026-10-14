import Determinize.Proof.RewardModel.Sparse
import Determinize.Proof.RewardModel.Soundness

set_option backward.isDefEq.respectTransparency false

namespace Determinize.Finite.Reward
open Spec.FiniteModel Spec.Paper

structure Table where
  size : Nat
  initial : Fin size
  state : Nat → State
  row : Nat → Row

def Table.RowClaim (t : Table) (i : Fin t.size) (action : Except Failure Step) : Prop :=
  (∀ e ∈ (t.row i).edges, e.target < t.size ∧ 0 < e.probability) ∧
  ((t.row i).edges.map Edge.probability).sum = 1 ∧
  match action with
  | .error _ => False
  | .ok (.returned b) => (t.row i).kind = .returned b
  | .ok .rejected => (t.row i).kind = .rejected
  | .ok (.next _ outcomes) =>
    (t.row i).kind = .transient ∧ (∀ o ∈ outcomes, 0 ≤ o.probability) ∧
    ((t.row i).edges.map fun e ↦ (e.probability, t.state e.target, e.reward)) =
      ((outcomes.filter fun o ↦ 0 < o.probability).map fun o ↦
        (o.probability, o.state, o.reward))

instance (t : Table) (i : Fin t.size) (action : Except Failure Step) :
    Decidable (t.RowClaim i action) := by
  unfold Table.RowClaim
  split <;> infer_instance

def Table.StateValid (t : Table) (i : Fin t.size) : Prop :=
  normalize (t.state i) = (0, t.state i) ∧ t.RowClaim i (step (t.state i))

instance (t : Table) (i : Fin t.size) : Decidable (t.StateValid i) :=
  inferInstanceAs (Decidable (_ ∧ _))

theorem Table.stateValid_of_step (t : Table) (i : Fin t.size)
    (action : Except Failure Step) (eq : step (t.state i) = action)
    (normalized : normalize (t.state i) = (0, t.state i))
    (row : t.RowClaim i action) : t.StateValid i := by
  refine ⟨normalized, ?_⟩
  rw [eq]
  exact row

def Table.candidate (t : Table) : Candidate :=
  ⟨t.initial, Array.ofFn (fun i : Fin t.size ↦ t.state i),
    Array.ofFn (fun i : Fin t.size ↦ t.row i)⟩

@[simp] theorem Table.state_at (t : Table) (i : Fin t.candidate.states.size) :
    t.candidate.states[i] = t.state i.val := by
  change (Array.ofFn (fun i : Fin t.size ↦ t.state i.val))[i.val] = _
  rw [Array.getElem_ofFn]

@[simp] theorem Table.row_at (t : Table) (i : Fin t.candidate.states.size) :
    t.candidate.row i = t.row i.val := by
  have bound : i.val < t.size := by simpa [Table.candidate] using i.isLt
  change (Array.ofFn (fun i : Fin t.size ↦ t.row i.val))[i.val]?.getD _ = _
  rw [Array.getElem?_ofFn]
  simp [bound]

theorem Table.replay (t : Table) (source : Core) (subject : Subject)
    (initial : t.state t.initial = initialState source subject)
    (sourceScoped : Proof.FiniteModel.Binding.Scoped 0 source)
    (valid : ∀ i, t.StateValid i) : t.candidate.ReplayValid source subject := by
  refine ⟨by simp [Table.candidate], by simp [Table.candidate],
    ?_, sourceScoped, ?_, ?_⟩
  · simpa [Table.candidate, t.initial.isLt] using congrArg some initial
  · intro i
    simpa only [Table.state_at] using (valid ⟨i.val, by simpa [Table.candidate] using i.isLt⟩).1
  · intro i
    have bound : i.val < t.size := by simpa [Table.candidate] using i.isLt
    have h := (valid ⟨i.val, bound⟩).2
    simp only [Table.RowClaim] at h
    unfold Candidate.RowValid Candidate.RowClaim
    rw [t.state_at i, t.row_at i]
    simp only [Table.candidate, Array.size_ofFn, Array.getElem?_ofFn]
    refine ⟨h.1, h.2.1, ?_⟩
    cases action : step (t.state i.val) with
    | error e => simpa [action] using h.2.2
    | ok result => cases result with
      | returned b => simpa [action] using h.2.2
      | rejected => simpa [action] using h.2.2
      | next tag outcomes =>
        simp only [action] at h ⊢
        refine ⟨h.2.2.1, h.2.2.2.1, ?_⟩
        rw [← h.2.2.2.2]
        apply List.map_congr_left
        intro e member
        simp [(h.1 e member).1]

abbrev Table.modelEdges (t : Table) (valid : ∀ i, t.StateValid i) (i : Fin t.size) :
    List (Spec.RewardModel.Edge t.size) :=
  (t.row i).edges.attach.map fun ⟨e, he⟩ ↦
    ⟨⟨e.target, ((valid i).2.1 e he).1⟩, e.probability, e.reward⟩

def Table.model (t : Table) (valid : ∀ i, t.StateValid i) : Spec.RewardModel.Model where
  size := t.size
  initial := t.initial
  kind := fun i ↦ (t.row i).kind
  edges := t.modelEdges valid
  nonnegative := by
    intro i e he
    obtain ⟨⟨original, member⟩, _, rfl⟩ := List.mem_map.mp he
    exact le_of_lt ((valid i).2.1 original member).2
  normalized := by
    intro i
    simpa [Table.modelEdges, List.map_map] using (valid i).2.2.1

private theorem lookupFunction_heq {n m : Nat} {β : Nat → Type}
    (size : n = m) (f : Fin n → β n) (g : Fin m → β m)
    (same : ∀ i, f i ≍ g ⟨i.val, size ▸ i.isLt⟩) : f ≍ g := by
  subst m
  apply heq_of_eq
  funext i
  exact eq_of_heq (same i)

private theorem edgeLists_heq {n m : Nat} (size : n = m) (xs ys : List Edge) (same : xs = ys)
    (left : ∀ e ∈ xs, e.target < n) (right : ∀ e ∈ ys, e.target < m) :
    (xs.attach.map fun e ↦ (⟨⟨e.val.target, left e.val e.property⟩,
      e.val.probability, e.val.reward⟩ : Spec.RewardModel.Edge n)) ≍
    (ys.attach.map fun e ↦ (⟨⟨e.val.target, right e.val e.property⟩,
      e.val.probability, e.val.reward⟩ : Spec.RewardModel.Edge m)) := by
  subst m
  subst ys
  rfl

theorem Table.model_eq (t : Table) (source : Core) (subject : Subject)
    (initial : t.state t.initial = initialState source subject)
    (sourceScoped : Proof.FiniteModel.Binding.Scoped 0 source)
    (valid : ∀ i, t.StateValid i) :
    t.model valid = t.candidate.toModel (t.replay source subject initial sourceScoped valid) := by
  have size : t.size = t.candidate.states.size := by simp [Table.candidate]
  have first : t.initial ≍ (t.candidate.toModel
      (t.replay source subject initial sourceScoped valid)).initial := by
    apply (Fin.heq_ext_iff size).mpr
    rfl
  have kinds : (t.model valid).kind ≍
      (t.candidate.toModel (t.replay source subject initial sourceScoped valid)).kind := by
    apply (Fin.heq_fun_iff size).mpr
    intro i
    change (t.row i.val).kind = (t.candidate.row _).kind
    rw [t.row_at]
  have edges : (t.model valid).edges ≍
      (t.candidate.toModel (t.replay source subject initial sourceScoped valid)).edges := by
    apply lookupFunction_heq (β := fun n ↦ List (Spec.RewardModel.Edge n)) size
    intro i
    simp only [Table.model, Table.modelEdges, Candidate.modelEdges]
    apply edgeLists_heq size
    rw [t.row_at]
  simp only [Table.model, Candidate.toModel, Spec.RewardModel.Model.mk.injEq]
  exact ⟨size, first, kinds, edges⟩

theorem Table.matches (t : Table) (source : Core) (subject : Subject)
    (initial : t.state t.initial = initialState source subject)
    (sourceScoped : Proof.FiniteModel.Binding.Scoped 0 source)
    (valid : ∀ i, t.StateValid i) : (t.model valid).Matches (subject.program source) := by
  rw [t.model_eq source subject initial sourceScoped valid]
  exact Proof.RewardModel.replay_matches _ _

end Determinize.Finite.Reward
