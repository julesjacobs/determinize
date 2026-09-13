import Mathlib.Algebra.BigOperators.Fin
import Mathlib.Data.Rat.Cast.Order
import Mathlib.Tactic.FieldSimp
import Mathlib.Tactic.LinearCombination
import Mathlib.Tactic.Ring

namespace Determinize.Proof.LinearAlgebra

private theorem lift_solution {n : Nat}
    (A : Fin (n+1) → Fin (n+1) → Rat) (b : Fin (n+1) → Rat)
    (pivot : A 0 0 ≠ 0) (x : Fin n → Rat)
    (solved : ∀ i : Fin n, ∑ j, (A i.succ j.succ - A i.succ 0 / A 0 0 * A 0 j.succ) * x j =
      b i.succ - A i.succ 0 / A 0 0 * b 0) :
    ∀ i, ∑ j, A i j * Fin.cons (α := fun _ => Rat) ((b 0 - ∑ k, A 0 k.succ * x k) / A 0 0) x j = b i := by
  intro i
  refine Fin.cases ?_ (fun i => ?_) i
  · simp only [Fin.sum_univ_succ, Fin.cons_zero, Fin.cons_succ]
    field_simp [pivot]
    ring
  · have h := solved i
    simp only [sub_mul, Finset.sum_sub_distrib, mul_assoc, ← Finset.mul_sum] at h
    simp only [Fin.sum_univ_succ, Fin.cons_zero, Fin.cons_succ]
    field_simp [pivot] at h ⊢
    linear_combination h

/-- Gaussian elimination with a proof of the original equations on success. -/
def solve : (n : Nat) → (A : Fin n → Fin n → Rat) → (b : Fin n → Rat) →
    Option {x : Fin n → Rat // ∀ i, ∑ j, A i j * x j = b i}
  | 0, _, _ => some ⟨Fin.elim0, fun i => Fin.elim0 i⟩
  | n+1, A, b => do
    let pivot ← (List.finRange (n+1)).findSome? fun i =>
      if h : A i 0 ≠ 0 then some (⟨i, h⟩ : {i // A i 0 ≠ 0}) else none
    let swap := Equiv.swap pivot.val 0
    let B := fun i j => A (swap i) j
    let c := fun i => b (swap i)
    have hp : B 0 0 ≠ 0 := by simpa [B, swap] using pivot.property
    let reduced := Vector.ofFn fun i : Fin n => Vector.ofFn fun j : Fin n =>
      B i.succ j.succ - B i.succ 0 / B 0 0 * B 0 j.succ
    let rhs := Vector.ofFn fun i : Fin n => c i.succ - B i.succ 0 / B 0 0 * c 0
    let tail ← solve n (fun i j => reduced[i][j]) (fun i => rhs[i])
    let x := Vector.ofFn (Fin.cons (α := fun _ => Rat) ((c 0 - ∑ j, B 0 j.succ * tail.val j) / B 0 0) tail.val)
    have htail : ∀ i : Fin n, ∑ j, (B i.succ j.succ - B i.succ 0 / B 0 0 * B 0 j.succ) * tail.val j =
        c i.succ - B i.succ 0 / B 0 0 * c 0 := by
      simpa [reduced, rhs] using tail.property
    have hx := lift_solution B c hp tail.val htail
    return ⟨fun i => x[i], fun i => by
      simpa [x, B, c, swap] using hx (swap i)⟩

end Determinize.Proof.LinearAlgebra
