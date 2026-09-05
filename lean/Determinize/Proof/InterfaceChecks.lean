import Determinize.Statement.Main
import Determinize.Traces.Main

/-! These checks use only the public definitions, before expression measurability is available. -/

namespace Determinize.Proof.InterfaceChecks
open MeasureTheory Determinize.Statement.Paper

example : True := by
  fail_if_success have := (inferInstance : MeasurableSpace Expr)
  trivial

example (fuel : Nat) (value : ℝ) :
    cumulativeOutputMeasure fuel (.real .E value) = Measure.dirac value := by
  induction fuel with
  | zero => rfl
  | succ fuel ih => simpa [cumulativeOutputMeasure, reduce] using ih

example (ty : Ty) : Typed [] (.lam (.bvar 0)) (.arr ty ty) :=
  .lam (.bvar .head)

def capturedSample : Expr :=
  .letE
    (.sample .E (.stochastic .uniform) [.real .E 0, .real .E 1] [])
    (.app (.lam (.add .E (.bvar 1) (.bvar 0))) (.real .E 2))

example : Typed [] capturedSample (.float .E) := by
  apply Typed.letE
  · apply Typed.sample _ rfl rfl
    · intro expression member
      simp only [List.mem_cons, List.not_mem_nil, or_false] at member
      rcases member with rfl | rfl <;> exact .real
    · simp
  · exact .app (.lam (.add (.bvar (.tail .head)) (.bvar .head))) .real

example : cumulativeOutputMeasure 4 capturedSample =
    (primitiveFiber (.stochastic .uniform) [0, 1] []).map (fun value => value + 2) := by
  simp [capturedSample, cumulativeOutputMeasure, reduce, Expr.isValue,
    firstNonValue, allRealValues?, Action.wrap, Function.comp_def,
    Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, realValue?]
  exact Measure.bind_dirac_eq_map _ (measurable_id.add_const 2)

example (trace : Traces.Trace) (result value : ℝ) :
    Traces.record (.E, .stochastic .uniform) value (trace, result) = (trace, result) := rfl

example (trace : Traces.Trace) (result value : ℝ) :
    Traces.record (.G, .stochastic .uniform) value (trace, result) =
      ((.uniform, value) :: trace, result) := rfl

example (trace : Traces.Trace) (result value : ℝ) :
    Traces.record (.G, .mean .uniform) value (trace, result) = (trace, result) := rfl

example : Traces.exactMeasure 4 capturedSample =
    (primitiveFiber (.stochastic .uniform) [0, 1] []).map
      (fun value => ([], value + 2)) := by
  simp [capturedSample, Traces.exactMeasure, reduce, Expr.isValue,
    firstNonValue, allRealValues?, Action.wrap, Function.comp_def,
    Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, realValue?]
  change ((primitiveFiber (.stochastic .uniform) [0, 1] []).bind
    fun value => (Measure.dirac ([], value + 2)).map id) = _
  simp only [Measure.map_id]
  exact Measure.bind_dirac_eq_map _ (measurable_const.prodMk (measurable_id.add_const 2))

end Determinize.Proof.InterfaceChecks
