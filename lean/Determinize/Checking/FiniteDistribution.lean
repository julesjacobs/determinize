import Mathlib.Algebra.BigOperators.Ring.List
import Mathlib.Data.Rat.Encodable
import Mathlib.Logic.Equiv.List
import Mathlib.Tactic.DeriveCountable

/-!
# Finite distributions

Distributions on finitely many outcomes with rational probabilities, their rational
expectations, and the checks that build them from literal probabilities.
-/

namespace Determinize.Checking

/-- Probabilities for the outcomes `0, ..., probabilities.length - 1`.
Zero-probability outcomes retain their indices. -/
structure FiniteDistribution where
  /-- The probability of each outcome, by index. -/
  probabilities : List Rat
  nonnegative : ∀ p ∈ probabilities, 0 ≤ p
  total : probabilities.sum = 1
deriving DecidableEq, Repr, Countable

/-- Rational expectation of a numeric function of the outcome index. -/
def FiniteDistribution.expectation (d : FiniteDistribution) (value : Nat → Rat) : Rat :=
  (d.probabilities.zipIdx.map fun (p, i) ↦ p * value i).sum

/-- The mean outcome index. -/
def FiniteDistribution.mean (d : FiniteDistribution) : Rat :=
  d.expectation (fun i ↦ (i : Rat))


/-- Check literal probabilities once, preserving their values and outcome indices. -/
def finiteDistribution (weights : List Rat) : Except String FiniteDistribution := do
  if hn : ∀ w ∈ weights, 0 ≤ w then
    if total : weights.sum = 1 then
      return ⟨weights, hn, total⟩
    else throw "discrete weights must sum to one"
  else throw "negative discrete weight"

/-- Complete the final probability and check the exact parameter domain. -/
def remainderDistribution (probabilities : List Rat) : Except String FiniteDistribution :=
  finiteDistribution (probabilities ++ [1 - probabilities.sum])

/-- Rational Bernoulli law on outcomes zero and one, including endpoints. -/
def bernoulliDistribution (p : Rat) : Except String FiniteDistribution := do
  if 0 ≤ p ∧ p ≤ 1 then finiteDistribution [1 - p, p]
  else throw "bernoulli requires probability in [0,1]"

end Determinize.Checking
