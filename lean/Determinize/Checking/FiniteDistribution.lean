import Determinize.Statement.FiniteDistribution

namespace Determinize.Checking
open Statement.Paper

/-- Check literal probabilities once, preserving their values and outcome indices. -/
def finiteDistribution (weights : List Rat) : Except String FiniteDistribution := do
  if hn : ∀ w ∈ weights, 0 ≤ w then
    if total : weights.sum = 1 then
      return ⟨weights, hn, total⟩
    else throw "discrete weights must sum to one"
  else throw "negative discrete weight"

/-- Rational Bernoulli law on outcomes zero and one, including endpoints. -/
def bernoulliDistribution (p : Rat) : Except String FiniteDistribution := do
  if 0 ≤ p ∧ p ≤ 1 then finiteDistribution [1 - p, p]
  else throw "bernoulli requires probability in [0,1]"

end Determinize.Checking
