import Determinize.Spec.Types
import Determinize.Spec.FiniteDistributionMeasure
import Mathlib.Probability.Distributions.Beta
import Mathlib.Probability.Distributions.Exponential
import Mathlib.Probability.Distributions.Gaussian.Real
import Mathlib.Probability.Distributions.Poisson.Basic
import Mathlib.Tactic.DeriveCountable

/-!
# Primitive distributions

Each primitive has one fiber: at evaluated parameters it is the primitive's law at a
stochastic site and the Dirac mass at the primitive's mean at a mean site, the form
determinization leaves behind. Both are the zero measure outside the parameter domain,
which makes an off-domain call stuck.
-/

namespace Determinize.Spec.Paper

open MeasureTheory ProbabilityTheory

noncomputable section

/-- The primitive names; a trace records which primitive a G draw came from. -/
inductive Op where
  | uniform
  | gaussian
  | poisson
  | exponential
  | beta
  | gamma
  | bernoulli
  | discrete (distribution : FiniteDistribution)
deriving DecidableEq, Repr, Countable

/-- Uniform probability measure on a closed interval, including a point interval. -/
def uniformMeasure (lower upper : ℝ) : Measure ℝ :=
  if _hDomain : lower ≤ upper then
    if _hPoint : lower = upper then Measure.dirac lower
    else (ENNReal.ofReal (upper - lower))⁻¹ • volume.restrict (Set.Icc lower upper)
  else 0

/-- `uniform(lower, upper)`: the uniform law on `[lower, upper]` or its mean. -/
def uniformFiber (action : DistributionAction) (lower upper : ℝ) : Measure ℝ :=
  if lower ≤ upper then
    match action with
    | .sample _ => uniformMeasure lower upper
    | .mean => Measure.dirac ((lower + upper) / 2)
  else 0

/-- `gaussian(mean, variance)`: the normal law or its mean. -/
def gaussianFiber (action : DistributionAction) (mean variance : ℝ) : Measure ℝ :=
  if h : 0 ≤ variance then
    match action with
    | .sample _ => gaussianReal mean ⟨variance, h⟩
    | .mean => Measure.dirac mean
  else 0

/-- `poisson(rate)`: the Poisson law on the naturals, read as reals, or its mean. -/
def poissonFiber (action : DistributionAction) (rate : ℝ) : Measure ℝ :=
  if h : 0 ≤ rate then
    match action with
    | .sample _ => (poissonMeasure ⟨rate, h⟩).map (fun value : Nat => (value : ℝ))
    | .mean => Measure.dirac rate
  else 0

/-- `exponential(rate)`: the exponential law or its mean `1 / rate`. -/
def exponentialFiber (action : DistributionAction) (rate : ℝ) : Measure ℝ :=
  if 0 < rate then
    match action with
    | .sample _ => expMeasure rate
    | .mean => Measure.dirac (1 / rate)
  else 0

/-- `beta(alpha, beta)`: the beta law or its mean `alpha / (alpha + beta)`. -/
def betaFiber (action : DistributionAction) (alpha beta : ℝ) : Measure ℝ :=
  if 0 < alpha ∧ 0 < beta then
    match action with
    | .sample _ => betaMeasure alpha beta
    | .mean => Measure.dirac (alpha / (alpha + beta))
  else 0

/-- `gamma(shape, rate)`: the gamma law or its mean `shape / rate`. -/
def gammaFiber (action : DistributionAction) (shape rate : ℝ) : Measure ℝ :=
  if 0 < shape ∧ 0 < rate then
    match action with
    | .sample _ => gammaMeasure shape rate
    | .mean => Measure.dirac (shape / rate)
  else 0

/-- Bernoulli on numeric outcomes zero and one. Invalid probabilities have zero mass. -/
noncomputable def bernoulliFiber (action : DistributionAction) (probability : ℝ) : Measure ℝ :=
  if 0 ≤ probability ∧ probability ≤ 1 then
    match action with
    | .sample _ => ENNReal.ofReal (1 - probability) • Measure.dirac 0 +
        ENNReal.ofReal probability • Measure.dirac 1
    | .mean => Measure.dirac probability
  else 0

/-- Finite numeric law or its mean; weights are already checked and normalized. -/
noncomputable def discreteFiber (action : DistributionAction) (distribution : FiniteDistribution) : Measure ℝ :=
  match action with
  | .sample _ => distribution.measure (fun i => (i : ℝ))
  | .mean => Measure.dirac (distribution.mean : ℝ)


end

end Determinize.Spec.Paper
