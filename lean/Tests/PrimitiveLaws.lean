import Determinize.Proof.Primitives.Moments

namespace Determinize.Tests
open Spec.Paper Proof.Paper MeasureTheory

example : Countable Op := inferInstance

example (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    (∫ x, x ∂primitiveLaws.kernel .bernoulli (fun _ => p, Fin.elim0)) = p :=
  primitiveLaws.mean_law .bernoulli _ h

example (p : ℝ) (h : p < 0 ∨ 1 < p) :
    primitiveLaws.kernel .bernoulli (fun _ => p, Fin.elim0) = 0 := by
  apply primitiveLaws.kernel_zero_off_domain
  change ¬ (0 ≤ p ∧ p ≤ 1)
  rcases h with h | h <;> intro valid <;> linarith [valid.1, valid.2]

example (n : Nat) (p : Fin n → ℝ) (valid : (∀ i, 0 ≤ p i) ∧ ∑ i, p i ≤ 1) :
    (∫ x, x ∂primitiveLaws.kernel (.discrete n) (p, Fin.elim0)) =
      (n : ℝ) + ∑ i : Fin n, (((i : ℕ) : ℝ) - n) * p i :=
  primitiveLaws.mean_law (.discrete n) _ valid

example (kind : DistributionAction) : primitiveFiber kind .bernoulli [] [] = 0 := by
  cases kind <;> simp [primitiveFiber, parseParams]

example (kind : DistributionAction) :
    primitiveFiber kind (.discrete 0) [1] [] = 0 := by
  cases kind <;> simp [primitiveFiber, parseParams]

example (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    primitiveFiber .mean .bernoulli [p] [] = Measure.dirac p := by
  rw [← bernoulliFiber_eq]
  simp [bernoulliFiber, h]

example : primitiveFiber .mean (.discrete 2) [1/4, 1/4] [] = Measure.dirac (5/4) := by
  norm_num [primitiveFiber, parseParams, domain, meanValue, Fin.sum_univ_two]

example : discreteFiber .mean [] = Measure.dirac 0 := by simp [discreteFiber]

#print axioms primitiveLaws
#print axioms primitiveMomentBounds

end Determinize.Tests
