import Determinize.Spec.FiniteModel.Model

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel

structure Paths (model : Model) where
  rank : Fin model.size → Nat
  next : Fin model.size → Fin model.size

def Paths.Valid (model : Model) (paths : Paths model) : Prop :=
  ∀ state, model.kind state = .transient →
    0 < model.transition state (paths.next state) ∧ paths.rank (paths.next state) < paths.rank state

instance (model : Model) (paths : Paths model) : Decidable (paths.Valid model) :=
  inferInstanceAs (Decidable (∀ _, _))

theorem paths_unique (model : Model) (paths : Paths model) (valid : paths.Valid model)
    (d : Fin model.size → ℝ)
    (eqs : ∀ state, d state = if model.kind state = .transient then
      ∑ next, (model.transition state next : ℝ) * d next else 0) : ∀ state, d state = 0 := by
  obtain ⟨largest, _, bound⟩ := Finset.exists_max_image Finset.univ
    (fun state => |d state|) ⟨model.initial, Finset.mem_univ _⟩
  let M := |d largest|
  have bounded : ∀ state, |d state| ≤ M := fun state => bound state (Finset.mem_univ _)
  have maximum_next (state : Fin model.size) (transient : model.kind state = .transient)
      (maximum : |d state| = M) : |d (paths.next state)| = M := by
    have triangle : |d state| ≤ ∑ next, (model.transition state next : ℝ) * |d next| := by
      rw [eqs state, if_pos transient]
      calc
        _ ≤ ∑ next, |(model.transition state next : ℝ) * d next| := Finset.abs_sum_le_sum_abs _ _
        _ = _ := by
          apply Finset.sum_congr rfl
          intro next _
          rw [abs_mul, abs_of_nonneg (by exact_mod_cast model.nonnegative state next)]
    have total : ∑ next, (model.transition state next : ℝ) = 1 := by exact_mod_cast model.normalized state
    have sumDiff : (∑ next, (model.transition state next : ℝ) * (M - |d next|)) =
        M - ∑ next, (model.transition state next : ℝ) * |d next| := by
      simp only [mul_sub]
      rw [Finset.sum_sub_distrib, ← Finset.sum_mul, total, one_mul]
    have term := Finset.single_le_sum
      (fun next (_ : next ∈ Finset.univ) => mul_nonneg
        (show 0 ≤ (model.transition state next : ℝ) by exact_mod_cast model.nonnegative state next)
        (sub_nonneg.mpr (bounded next))) (Finset.mem_univ (paths.next state))
    have positive : 0 < (model.transition state (paths.next state) : ℝ) := by
      exact_mod_cast (valid state transient).1
    rw [sumDiff] at term
    rw [maximum] at triangle
    nlinarith [bounded (paths.next state)]
  have descend : ∀ n, ∀ state, paths.rank state = n → |d state| = M → M = 0 := by
    intro n
    induction n using Nat.strong_induction_on with
    | h n ih =>
      intro state rank maximum
      by_cases transient : model.kind state = .transient
      · exact ih (paths.rank (paths.next state)) (rank ▸ (valid state transient).2)
          (paths.next state) rfl (maximum_next state transient maximum)
      · rw [eqs state, if_neg transient, abs_zero] at maximum
        exact maximum.symm
  have zero := descend (paths.rank largest) largest rfl rfl
  intro state
  apply abs_eq_zero.mp
  exact le_antisymm (zero ▸ bounded state) (abs_nonneg _)

private def chooseAll : (n : Nat) → {α : Fin n → Type} →
    ((i : Fin n) → Option (α i)) → Option ((i : Fin n) → α i)
  | 0, _, _ => some (fun i => Fin.elim0 i)
  | n+1, _, choices => do
    let head ← choices 0
    let tail ← chooseAll n (fun i => choices i.succ)
    return fun i => match i with
      | ⟨0, _⟩ => head
      | ⟨k+1, h⟩ => tail ⟨k, Nat.lt_of_succ_lt_succ h⟩

def findPaths (model : Model) (rank : Fin model.size → Nat) :
    Except String {paths : Paths model // paths.Valid model} := do
  let choices := fun state : Fin model.size =>
    if transient : model.kind state = .transient then
      (List.finRange model.size).findSome? fun next =>
        if h : 0 < model.transition state next ∧ rank next < rank state then
          some (⟨next, fun _ => h⟩ : {next : Fin model.size // model.kind state = .transient →
            0 < model.transition state next ∧ rank next < rank state})
        else none
    else some (⟨state, fun h => (transient h).elim⟩ : {next : Fin model.size // model.kind state = .transient →
      0 < model.transition state next ∧ rank next < rank state})
  let some selected := chooseAll model.size choices
    | throw "no descending path to a terminal boundary"
  let next := Vector.ofFn fun state => (selected state).val
  return ⟨⟨rank, fun state => next[state]⟩, by
    intro state transient
    simpa [next] using (selected state).property transient⟩

end Determinize.Proof.FiniteModel
