import Determinize.Proof.FiniteDistribution

namespace Determinize.Checking
open Statement.Paper

/-- Normalize literal weights using exact arithmetic and carry normalization proofs. -/
def finiteDistribution (weights : List Rat) : Except String FiniteDistribution := do
  if hn : ∀ w ∈ weights, 0 ≤ w then
    if hp : 0 < weights.sum then
      return ⟨weights.map (· / weights.sum),
        Proof.FiniteDistribution.normalized_nonnegative weights hn hp,
        Proof.FiniteDistribution.normalized_total weights hp⟩
    else throw "discrete weights must have positive total"
  else throw "negative discrete weight"

/-- Rational Bernoulli law on outcomes zero and one, including endpoints. -/
def bernoulliDistribution (p : Rat) : Except String FiniteDistribution := do
  if 0 ≤ p ∧ p ≤ 1 then finiteDistribution [1 - p, p]
  else throw "bernoulli requires probability in [0,1]"

end Determinize.Checking
