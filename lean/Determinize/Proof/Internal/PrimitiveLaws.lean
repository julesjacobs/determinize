import Determinize.Statement.Semantics
import Mathlib.Probability.Kernel.Basic

/-!
# Analytic laws of the six paper primitives

The proof indexes the primitives uniformly by `Op` and parameter tables; the
reviewer-facing per-primitive fibers of `Statement/Primitives.lean` are recovered from
these tables. The second part records the analytic facts required of the canonical
primitive measures.
-/

namespace Determinize.Statement.Paper

open MeasureTheory ProbabilityTheory

noncomputable section

/-! The proof indexes the six primitives uniformly by `Op`: an affine parameter position
is one in which the primitive's mean is affine (and which may therefore be typed at mode E),
a general position must be typed at mode G. The reviewer-facing fibers in
`Statement/Primitives.lean` are recovered from these tables below. -/

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

/-- The stochastic or mean fiber of a primitive at evaluated operand lists. -/
def primitiveFiber (kind : Kind) (op : Op) (affine general : List ℝ) : Measure ℝ := by
  classical
  exact match parseParams op affine general with
    | none => 0
    | some params =>
        match kind with
        | .stochastic => paperMeasure op params
        | .mean => if domain op params then Measure.dirac (meanValue op params) else 0

/-! The reviewer-facing fibers are the generic one at the primitive's operand lists. -/

theorem uniformFiber_eq (kind : Kind) (lower upper : ℝ) :
    uniformFiber kind lower upper = primitiveFiber kind .uniform [lower, upper] [] := by
  cases kind <;> by_cases h : lower ≤ upper <;>
    simp [uniformFiber, primitiveFiber, parseParams, paperMeasure, domain, meanValue,
      uniformMeasure, h]

theorem gaussianFiber_eq (kind : Kind) (mean variance : ℝ) :
    gaussianFiber kind mean variance = primitiveFiber kind .gaussian [mean] [variance] := by
  cases kind <;> by_cases h : 0 ≤ variance <;>
    simp [gaussianFiber, primitiveFiber, parseParams, paperMeasure, domain, meanValue, h]

theorem poissonFiber_eq (kind : Kind) (rate : ℝ) :
    poissonFiber kind rate = primitiveFiber kind .poisson [rate] [] := by
  cases kind <;> by_cases h : 0 ≤ rate <;>
    simp [poissonFiber, primitiveFiber, parseParams, paperMeasure, domain, meanValue, h]

theorem exponentialFiber_eq (kind : Kind) (rate : ℝ) :
    exponentialFiber kind rate = primitiveFiber kind .exponential [] [rate] := by
  cases kind <;> by_cases h : 0 < rate <;>
    simp [exponentialFiber, primitiveFiber, parseParams, paperMeasure, domain, meanValue, h]

theorem betaFiber_eq (kind : Kind) (alpha beta : ℝ) :
    betaFiber kind alpha beta = primitiveFiber kind .beta [] [alpha, beta] := by
  cases kind <;> by_cases h : 0 < alpha ∧ 0 < beta <;>
    simp [betaFiber, primitiveFiber, parseParams, paperMeasure, domain, meanValue, h]

theorem gammaFiber_eq (kind : Kind) (shape rate : ℝ) :
    gammaFiber kind shape rate = primitiveFiber kind .gamma [shape] [rate] := by
  cases kind <;> by_cases h : 0 < shape ∧ 0 < rate <;>
    simp [gammaFiber, primitiveFiber, parseParams, paperMeasure, domain, meanValue, h]

end

end Determinize.Statement.Paper

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory
open Determinize.Statement.Paper

noncomputable section

/-- The part of a primitive mean independent of affine-position parameters. -/
def meanConstant : (op : Op) → (Fin (generalArity op) → ℝ) → ℝ
  | .uniform, _ => 0
  | .gaussian, _ => 0
  | .poisson, _ => 0
  | .exponential, general => 1 / general
      0
  | .beta, general =>
      general 0 /
        (general 0 +
          general 1)
  | .gamma, _ => 0

/-- Coefficient of an affine-position parameter in a primitive mean. -/
def meanCoeff : (op : Op) →
    (Fin (generalArity op) → ℝ) → Fin (affineArity op) → ℝ
  | .uniform, _, _ => 1 / 2
  | .gaussian, _, _ => 1
  | .poisson, _, _ => 1
  | .exponential, _, index => Fin.elim0 index
  | .beta, _, index => Fin.elim0 index
  | .gamma, general, _ => 1 / general
      0

theorem meanValue_eq_affine (op : Op) (params : Params op) :
    meanValue op params = meanConstant op params.2 +
      ∑ i, meanCoeff op params.2 i * params.1 i := by
  cases op <;> simp [meanValue, meanConstant, meanCoeff, affineArity,
    Fin.sum_univ_two] <;> ring

theorem measurableSet_domain (op : Op) :
    MeasurableSet {params | domain op params} := by
  cases op
  · change MeasurableSet {params : Params .uniform | params.1 0 ≤ params.1 1}
    apply measurableSet_le <;> fun_prop
  · change MeasurableSet {params : Params .gaussian | 0 ≤ params.2 0}
    apply measurableSet_le <;> fun_prop
  · change MeasurableSet {params : Params .poisson | 0 ≤ params.1 0}
    apply measurableSet_le <;> fun_prop
  · change MeasurableSet {params : Params .exponential | 0 < params.2 0}
    apply measurableSet_lt <;> fun_prop
  · exact MeasurableSet.inter
      ((show Measurable (fun params : Params .beta => params.2 0) by fun_prop)
        measurableSet_Ioi)
      ((show Measurable (fun params : Params .beta => params.2 1) by fun_prop)
        measurableSet_Ioi)
  · exact MeasurableSet.inter
      ((show Measurable (fun params : Params .gamma => params.1 0) by fun_prop)
        measurableSet_Ioi)
      ((show Measurable (fun params : Params .gamma => params.2 0) by fun_prop)
        measurableSet_Ioi)

theorem measurable_meanConstant (op : Op) : Measurable (meanConstant op) := by
  cases op <;> unfold meanConstant <;> fun_prop

theorem measurable_meanCoeff (op : Op) (i : Fin (affineArity op)) :
    Measurable (fun general => meanCoeff op general i) := by
  cases op <;> unfold meanCoeff <;> fun_prop

/-- Analytic facts about the canonical primitive measures used by the soundness proof. -/
structure PrimitiveLaws where
  kernel : (op : Op) → Kernel (Params op) ℝ
  kernel_eq_paperMeasure : ∀ op params, kernel op params = paperMeasure op params
  kernel_sfinite : ∀ op, IsSFiniteKernel (kernel op)
  mass_le_one : ∀ op params, kernel op params Set.univ ≤ 1
  kernel_zero_off_domain : ∀ op params, ¬ domain op params → kernel op params = 0
  mass_one : ∀ op params, domain op params → kernel op params Set.univ = 1
  integrable_id : ∀ op params, domain op params →
    Integrable (fun value : ℝ => value) (kernel op params)
  mean_law : ∀ op params, domain op params →
    (∫ value : ℝ, value ∂kernel op params) = meanValue op params

end

end Determinize.Proof.Paper
