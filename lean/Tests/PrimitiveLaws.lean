import Determinize.Proof.PrimitiveMoments

namespace Determinize.Tests
open Statement.Paper Proof.Paper MeasureTheory

example : Countable Op := inferInstance

example (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    (∫ x, x ∂primitiveLaws.kernel .bernoulli (fun _ => p, Fin.elim0)) = p :=
  primitiveLaws.mean_law .bernoulli _ h

example (p : ℝ) (h : p < 0 ∨ 1 < p) :
    primitiveLaws.kernel .bernoulli (fun _ => p, Fin.elim0) = 0 := by
  apply primitiveLaws.kernel_zero_off_domain
  change ¬ (0 ≤ p ∧ p ≤ 1)
  rcases h with h | h <;> intro valid <;> linarith [valid.1, valid.2]

example (d : FiniteDistribution) :
    (∫ x, x ∂primitiveLaws.kernel (.discrete d) (Fin.elim0, Fin.elim0)) =
      (d.mean : ℝ) :=
  primitiveLaws.mean_law (.discrete d) _ trivial

example (kind : Kind) : primitiveFiber kind .bernoulli [] [] = 0 := by
  cases kind <;> simp [primitiveFiber, parseParams]

example (kind : Kind) (d : FiniteDistribution) :
    primitiveFiber kind (.discrete d) [1] [] = 0 := by
  cases kind <;> simp [primitiveFiber, parseParams]

example (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    primitiveFiber .mean .bernoulli [p] [] = Measure.dirac p := by
  rw [← bernoulliFiber_eq]
  simp [bernoulliFiber, h]

example (d : FiniteDistribution) :
    primitiveFiber .mean (.discrete d) [] [] = Measure.dirac (d.mean : ℝ) := by
  rw [← discreteFiber_eq]
  rfl

#print axioms primitiveLaws
#print axioms primitiveMomentBounds

end Determinize.Tests
