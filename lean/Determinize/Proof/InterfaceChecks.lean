import Determinize.Spec.Main
import Determinize.Spec.Traces.Main

/-! These checks use only the public definitions, before expression measurability is available. -/

namespace Determinize.Proof.InterfaceChecks
open MeasureTheory Determinize.Spec.Paper

example : True := by
  fail_if_success have := (inferInstance : MeasurableSpace Expr)
  trivial

example (value : ℝ) : outputMeasureAt 0 (.real value) = Measure.dirac value := rfl

example (depth : Nat) (value : ℝ) : outputMeasureAt (depth + 1) (.real value) = 0 := rfl

example (ty : Ty) : Typed [] (.lam (.bvar 0)) (.arr ty ty) :=
  .lam (.bvar .head)

example : ¬ DomainSafe (.app (.real 0) (.real 1)) := by
  intro safe
  have h := safe 1
  simp [DomainSafeAt, reduce, Expr.isValue] at h

example : ¬ DomainSafe (.uniform (.sample .E) (.real 1) (.real 0)) := by
  intro h
  have h := h 1
  norm_num [DomainSafeAt, reduce, Expr.isValue, realValue?, uniformFiber] at h

example (x : ℝ) : reduce (.div (.real x) (.real 0)) = .stuck := by
  simp [reduce, Expr.isValue, realValue?]

example (x : ℝ) : ¬ DomainSafe (.div (.real x) (.real 0)) := by
  intro safe
  have h := safe 1
  simp [DomainSafeAt, reduce, Expr.isValue, realValue?] at h

example (x : ℝ) : bigStepMeasure (.div (.real x) (.real 0)) = 0 := by
  have zeroAt : ∀ depth, outputMeasureAt depth (.div (.real x) (.real 0)) = 0 := by
    intro depth
    cases depth <;> simp [outputMeasureAt, reduce, Expr.isValue, realValue?]
  simp [bigStepMeasure, zeroAt]

example : ¬ DomainSafe (.div (.real 1) (.bernoulli (.sample .G) (.real (1/2)))) := by
  intro safe
  have h := safe 2
  norm_num [DomainSafeAt, reduce, Expr.isValue, realValue?, Action.wrap,
    Function.comp_def, bernoulliFiber] at h

def capturedSample : Expr :=
  .letE
    (.uniform (.sample .E) (.real 0) (.real 1))
    (.app (.lam (.add (.bvar 1) (.bvar 0))) (.real 2))

example : Typed [] capturedSample (.float .E) := by
  apply Typed.letE
  · exact .uniform .real .real
  · exact .app (.lam (.add (.bvar (.tail .head)) (.bvar .head))) .real

example : outputMeasureAt 4 capturedSample =
    (uniformFiber (.sample .G) 0 1).map (fun value => value + 2) := by
  simp [capturedSample, outputMeasureAt, reduce, Expr.isValue,
    realValue?, Action.wrap, Function.comp_def,
    Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars]
  exact Measure.bind_dirac_eq_map _ (measurable_id.add_const 2)

example (trace : Spec.Traces.Trace) (result value : ℝ) :
    Spec.Traces.record (.sample .E, .uniform) value (trace, result) = (trace, result) := rfl

example (trace : Spec.Traces.Trace) (result value : ℝ) :
    Spec.Traces.record (.sample .G, .uniform) value (trace, result) =
      ((.uniform, value) :: trace, result) := rfl

example (trace : Spec.Traces.Trace) (result value : ℝ) :
    Spec.Traces.record (.mean, .uniform) value (trace, result) = (trace, result) := rfl

example : Spec.Traces.traceAndOutputLawAt 4 capturedSample =
    (uniformFiber (.sample .G) 0 1).map
      (fun value => ([], value + 2)) := by
  simp [capturedSample, Spec.Traces.traceAndOutputLawAt, reduce, Expr.isValue,
    realValue?, Action.wrap, Function.comp_def,
    Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, realValue?]
  change ((uniformFiber (.sample .G) 0 1).bind
    fun value => (Measure.dirac ([], value + 2)).map id) = _
  simp only [Measure.map_id]
  exact Measure.bind_dirac_eq_map _ (measurable_const.prodMk (measurable_id.add_const 2))

end Determinize.Proof.InterfaceChecks
