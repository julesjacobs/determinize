import Determinize.Statement.Primitives
import Mathlib.Probability.Kernel.Basic

/-!
# Analytic laws of the six paper primitives

This proof-only interface records the analytic facts required of the canonical
primitive measures. Its parameters and measures come directly from the public
semantics.
-/

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
