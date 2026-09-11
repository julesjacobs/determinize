import Tests.Parsing
import Determinize.Checking.FiniteDistribution
import Determinize.Proof.DiscreteLaws

namespace Determinize.Tests
open Checking

example : (finiteDistribution [1/6, 1/3, 1/2]).map (·.probabilities) = .ok [1/6, 1/3, 1/2] := by
  decide +kernel

example : (finiteDistribution [0, 1, 0]).map (·.mean) = .ok 1 := by
  decide +kernel

example : (bernoulliDistribution (1/4)).map (·.mean) = .ok (1/4) := by
  decide +kernel

example (p : ℝ) (h : 0 ≤ p ∧ p ≤ 1) :
    (∫ x, x ∂Statement.Paper.bernoulliFiber .stochastic p) = p :=
  Proof.DiscreteLaws.bernoulli_mean .stochastic p h

#print axioms Proof.DiscreteLaws.discrete_mean
#print axioms Proof.DiscreteLaws.bernoulli_mean

def finiteDistributions : IO Unit := do
  for weights in [[], [0], [0, 0], [-1, 2], [2, -1], [1, 2, 3], [1/4, 1/4]] do
    assert (!(finiteDistribution weights).isOk) s!"invalid weights accepted: {weights}"
  for p in [(-1 : Rat), 2] do
    assert (!(bernoulliDistribution p).isOk) "invalid Bernoulli probability accepted"
  for p in [(0 : Rat), 1/4, 1] do
    let d ← IO.ofExcept (bernoulliDistribution p)
    assert (d.mean == p) s!"wrong Bernoulli mean at {p}"
    assert (d.probabilities == [1-p, p]) "Bernoulli outcomes reordered"
  let d ← IO.ofExcept (finiteDistribution [0, 1, 0])
  assert (d.probabilities == [0, 1, 0]) "zero weights changed outcome indices"
  let a ← IO.ofExcept (finiteDistribution [1/6, 1/3, 1/2])
  assert (a.probabilities == [1/6, 1/3, 1/2]) "probabilities changed"
  assert (a.mean == 4/3) "wrong categorical mean"

end Determinize.Tests
