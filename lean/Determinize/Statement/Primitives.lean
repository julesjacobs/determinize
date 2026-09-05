import Mathlib.Probability.Distributions.Beta
import Mathlib.Probability.Distributions.Exponential
import Mathlib.Probability.Distributions.Gaussian.Real
import Mathlib.Probability.Distributions.Poisson.Basic
import Mathlib.Tactic.DeriveCountable

/-!
# The six primitive distributions

`Params op` is the single arity-indexed representation of evaluated primitive
parameters used by both the operational semantics and its analytic proof.
-/

namespace Determinize.Statement.Paper

open MeasureTheory ProbabilityTheory

noncomputable section

inductive Op where
  | uniform
  | gaussian
  | poisson
  | exponential
  | beta
  | gamma
deriving DecidableEq, Repr, Countable

abbrev affineArity : Op → Nat
  | .uniform => 2
  | .gaussian => 1
  | .poisson => 1
  | .exponential => 0
  | .beta => 0
  | .gamma => 1

abbrev generalArity : Op → Nat
  | .uniform => 0
  | .gaussian => 1
  | .poisson => 0
  | .exponential => 1
  | .beta => 2
  | .gamma => 1

/-- Evaluated parameters, indexed by the primitive's two arities. -/
abbrev Params (op : Op) :=
  (Fin (affineArity op) → ℝ) × (Fin (generalArity op) → ℝ)

/-- Parse evaluated operand lists into the primitive's arity-indexed parameters. -/
def parseParams (op : Op) (affine general : List ℝ) : Option (Params op) :=
  if ha : affine.length = affineArity op then
    if hg : general.length = generalArity op then
      some
        (fun i => affine[i.1]'(ha.symm ▸ i.2),
         fun i => general[i.1]'(hg.symm ▸ i.2))
    else none
  else none

def domain : (op : Op) → Params op → Prop
  | .uniform, (a, _) => a 0 ≤ a 1
  | .gaussian, (_, g) => 0 ≤ g 0
  | .poisson, (a, _) => 0 ≤ a 0
  | .exponential, (_, g) => 0 < g 0
  | .beta, (_, g) => 0 < g 0 ∧ 0 < g 1
  | .gamma, (a, g) => 0 < a 0 ∧ 0 < g 0

/-- The six primitive means on evaluated parameters. -/
def meanValue : (op : Op) → Params op → ℝ
  | .uniform, (a, _) => (a 0 + a 1) / 2
  | .gaussian, (a, _) => a 0
  | .poisson, (a, _) => a 0
  | .exponential, (_, g) => 1 / g 0
  | .beta, (_, g) => g 0 / (g 0 + g 1)
  | .gamma, (a, g) => a 0 / g 0

/-- Uniform probability measure on a closed interval, including a point interval. -/
def uniformMeasure (lower upper : ℝ) : Measure ℝ :=
  if _hDomain : lower ≤ upper then
    if _hPoint : lower = upper then Measure.dirac lower
    else (ENNReal.ofReal (upper - lower))⁻¹ • volume.restrict (Set.Icc lower upper)
  else 0

/-- Canonical primitive measure; an off-domain call has zero measure. -/
def paperMeasure : (op : Op) → Params op → Measure ℝ
  | .uniform, params =>
      uniformMeasure (params.1 0) (params.1 1)
  | .gaussian, params =>
      let mean := params.1 0
      let variance := params.2 0
      if h : 0 ≤ variance then gaussianReal mean ⟨variance, h⟩ else 0
  | .poisson, params =>
      let rate := params.1 0
      if h : 0 ≤ rate then
        (poissonMeasure ⟨rate, h⟩).map (fun value : Nat => (value : ℝ))
      else 0
  | .exponential, params =>
      let rate := params.2 0
      if 0 < rate then expMeasure rate else 0
  | .beta, params =>
      let alpha := params.2 0
      let betaParam := params.2 1
      if 0 < alpha ∧ 0 < betaParam then betaMeasure alpha betaParam else 0
  | .gamma, params =>
      let shape := params.1 0
      let rate := params.2 0
      if 0 < shape ∧ 0 < rate then gammaMeasure shape rate else 0

end

end Determinize.Statement.Paper
