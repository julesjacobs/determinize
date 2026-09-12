import Mathlib.Probability.Distributions.Beta
import Mathlib.Probability.Distributions.Exponential
import Mathlib.Probability.Distributions.Gaussian.Real
import Mathlib.Probability.Distributions.Poisson.Basic
import Mathlib.Tactic.DeriveCountable

/-!
# The eight primitive distributions

Each primitive has one fiber: at evaluated parameters it is the primitive's law at a
stochastic site and the Dirac mass at the primitive's mean at a mean site, the form
determinization leaves behind. Both are the zero measure outside the parameter domain,
which makes an off-domain call stuck. Six primitives are the paper's continuous and
count-valued laws; `bernoulli(p)` is the two-point law on `{0, 1}` with mean `p`, the
paper's `bernoulli` and the draw behind `flip` (`Statement/Syntax.lean`), which compares it
with `0`; `discrete(w₀, …, wₙ₋₁)` draws the index `i` with probability `wᵢ` and has the mean
`∑ i, wᵢ · i`. Its name `Op.discrete n` records the number of weights, so that every primitive
has a fixed number of parameters.
-/

namespace Determinize.Statement.Paper

open MeasureTheory ProbabilityTheory

noncomputable section

/-- The primitive names; a trace records which primitive a general-mode draw came from.
`discrete n` carries its number of weights: no static arity check exists, but `reduce` stamps
`weights.length` into the site name, and replay compares site names (`Traces.outputGivenTraceAt`),
so a trace entry recorded by a `discrete` of another arity never fits. -/
inductive Op where
  | uniform
  | gaussian
  | poisson
  | exponential
  | beta
  | gamma
  | bernoulli
  | discrete (arity : Nat)
deriving DecidableEq, Repr, Countable

/-- A sampling site draws from its primitive or, after determinization, returns its mean. -/
inductive Kind where
  | stochastic
  | mean
deriving DecidableEq, Repr, Countable

def Kind.isStochastic : Kind → Bool
  | .stochastic => true
  | .mean => false

/-- Uniform probability measure on a closed interval, including a point interval. -/
def uniformMeasure (lower upper : ℝ) : Measure ℝ :=
  if _hDomain : lower ≤ upper then
    if _hPoint : lower = upper then Measure.dirac lower
    else (ENNReal.ofReal (upper - lower))⁻¹ • volume.restrict (Set.Icc lower upper)
  else 0

/-- `uniform(lower, upper)`: the uniform law on `[lower, upper]` or its mean. -/
def uniformFiber (kind : Kind) (lower upper : ℝ) : Measure ℝ :=
  if lower ≤ upper then
    match kind with
    | .stochastic => uniformMeasure lower upper
    | .mean => Measure.dirac ((lower + upper) / 2)
  else 0

/-- `gaussian(mean, variance)`: the normal law or its mean. -/
def gaussianFiber (kind : Kind) (mean variance : ℝ) : Measure ℝ :=
  if h : 0 ≤ variance then
    match kind with
    | .stochastic => gaussianReal mean ⟨variance, h⟩
    | .mean => Measure.dirac mean
  else 0

/-- `poisson(rate)`: the Poisson law on the naturals, read as reals, or its mean. -/
def poissonFiber (kind : Kind) (rate : ℝ) : Measure ℝ :=
  if h : 0 ≤ rate then
    match kind with
    | .stochastic => (poissonMeasure ⟨rate, h⟩).map (fun value : Nat => (value : ℝ))
    | .mean => Measure.dirac rate
  else 0

/-- `exponential(rate)`: the exponential law or its mean `1 / rate`. -/
def exponentialFiber (kind : Kind) (rate : ℝ) : Measure ℝ :=
  if 0 < rate then
    match kind with
    | .stochastic => expMeasure rate
    | .mean => Measure.dirac (1 / rate)
  else 0

/-- `beta(alpha, beta)`: the beta law or its mean `alpha / (alpha + beta)`. -/
def betaFiber (kind : Kind) (alpha beta : ℝ) : Measure ℝ :=
  if 0 < alpha ∧ 0 < beta then
    match kind with
    | .stochastic => betaMeasure alpha beta
    | .mean => Measure.dirac (alpha / (alpha + beta))
  else 0

/-- `gamma(shape, rate)`: the gamma law or its mean `shape / rate`. -/
def gammaFiber (kind : Kind) (shape rate : ℝ) : Measure ℝ :=
  if 0 < shape ∧ 0 < rate then
    match kind with
    | .stochastic => gammaMeasure shape rate
    | .mean => Measure.dirac (shape / rate)
  else 0

/-- `bernoulli(p)`: the two-point law with mass `p` at `1` and `1 - p` at `0`, or its mean `p`;
the domain is `0 ≤ p ≤ 1`. -/
def bernoulliFiber (kind : Kind) (probability : ℝ) : Measure ℝ :=
  if 0 ≤ probability ∧ probability ≤ 1 then
    match kind with
    | .stochastic =>
        ENNReal.ofReal (1 - probability) • Measure.dirac 0 +
          ENNReal.ofReal probability • Measure.dirac 1
    | .mean => Measure.dirac probability
  else 0

/-- `discrete(w₀, …, wₙ₋₁)`: the index `i ∈ {0, …, n - 1}` with probability `wᵢ`, or the mean
`∑ i, wᵢ · i`; the domain requires nonnegative weights that sum to one. -/
def discreteFiber (kind : Kind) (weights : List ℝ) : Measure ℝ :=
  if (∀ i : Fin weights.length, 0 ≤ weights[i]) ∧ ∑ i : Fin weights.length, weights[i] = 1 then
    match kind with
    | .stochastic =>
        ∑ i : Fin weights.length, ENNReal.ofReal weights[i] • Measure.dirac ((i : ℕ) : ℝ)
    | .mean => Measure.dirac (∑ i : Fin weights.length, weights[i] * ((i : ℕ) : ℝ))
  else 0

end

end Determinize.Statement.Paper
