import Mathlib.Algebra.BigOperators.Ring.List
import Mathlib.Data.Rat.Encodable
import Mathlib.Logic.Equiv.List
import Mathlib.Tactic.DeriveCountable

namespace Determinize.Spec.Paper

/-- Probabilities for the outcomes `0, ..., probabilities.length - 1`.
Zero-probability outcomes retain their indices. -/
structure FiniteDistribution where
  probabilities : List Rat
  nonnegative : ∀ p ∈ probabilities, 0 ≤ p
  total : probabilities.sum = 1
deriving DecidableEq, Repr, Countable

/-- Rational expectation of a numeric function of the outcome index. -/
def FiniteDistribution.expectation (d : FiniteDistribution) (value : Nat → Rat) : Rat :=
  (d.probabilities.zipIdx.map fun (p, i) => p * value i).sum

def FiniteDistribution.mean (d : FiniteDistribution) : Rat :=
  d.expectation (fun i => (i : Rat))

end Determinize.Spec.Paper
