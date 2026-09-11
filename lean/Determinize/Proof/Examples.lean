import Determinize.Theorems

namespace Determinize.Proof.Examples
open MeasureTheory Determinize.Spec.Paper Determinize.Spec.Traces

def uniform (affinity : Affinity) : Expr := .uniform (.sample affinity) (.real 0) (.real 1)

theorem uniform_typed (affinity : Affinity) : Typed context (uniform affinity) (.float affinity) :=
  .uniform .real .real

def nestedAffine : Expr := .uniform (.sample .E) (uniform .E) (.add (.real 2) (.real 3))

def nestedGeneral : Expr := .gaussian (.sample .E) (.real 0) (uniform .G)

def reciprocal : Expr :=
  .letE (uniform .E)
    (.letE (uniform .G)
      (.add (.bvar 1)
        (.div (.real 1) (.bvar 0))))

theorem nestedAffine_source : nestedAffine.sourceForm = true := by simp [nestedAffine, uniform, Expr.sourceForm, DistributionAction.isSample]
example : nestedGeneral.sourceForm = true := by simp [nestedGeneral, uniform, Expr.sourceForm, DistributionAction.isSample]
theorem reciprocal_source : reciprocal.sourceForm = true := by simp [reciprocal, uniform, Expr.sourceForm, DistributionAction.isSample]

example : Typed [] nestedAffine (.float .E) := .uniform (uniform_typed .E) (.add .real .real)

example : Typed [] nestedGeneral (.float .E) := .gaussian .real (uniform_typed .G)

theorem reciprocal_typed : Typed [] reciprocal (.float .E) :=
  .letE (uniform_typed .E) (.letE (uniform_typed .G)
    (.add (.bvar (.tail .head)) (.div .real (.bvar .head))))

example : reduce nestedGeneral =
    .sample (.sample .G, .uniform) (uniformFiber (.sample .G) 0 1)
      (fun value => .gaussian (.sample .E) (.real 0) (.real value)) := by
  simp [nestedGeneral, uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

example : reduce nestedGeneral.determinize =
    .sample (.sample .G, .uniform) (uniformFiber (.sample .G) 0 1)
      (fun value => .gaussian .mean (.real 0) (.real value)) := by
  simp [nestedGeneral, uniform, Expr.determinize, DistributionAction.determinize, reduce, Expr.isValue,
    realValue?, Action.wrap, Function.comp_def]

example : reduce nestedAffine =
    .sample (.sample .E, .uniform) (uniformFiber (.sample .E) 0 1)
      (fun value => .uniform (.sample .E) (.real value) (.add (.real 2) (.real 3))) := by
  simp [nestedAffine, uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

example : (Expr.uniform .mean (.real 0) (.real 1)).sourceForm = false := by
  simp [Expr.sourceForm, DistributionAction.isSample]

-- A literal takes either affinity, so a product of literals types at both affinities.
example : Typed [] (.mul (.real 2) (.real 1)) (.float .E) := .mul .real .real
example : Typed [] (.mul (.real 2) (.real 1)) (.float .G) := .mul .real .real

-- The general-affinity factor of an expectation-affinity product stands on the left: an
-- expectation-affinity draw may be scaled from the left, not from the right, and not squared.
example : Typed [] (.mul (.real 2) (uniform .E)) (.float .E) := .mul .real (uniform_typed .E)
private theorem uniformE_not_G : ¬ Typed context (uniform .E) (.float .G) := by
  intro typed
  generalize he : uniform .E = expression at typed
  generalize ht : Ty.float .G = ty at typed
  induction typed
  case sub h sub ih =>
    cases sub <;> cases ht
    exact ih he rfl
  all_goals cases ht <;> simp [uniform] at he

private theorem mul_uniformE_not_typed (right : Expr) :
    ¬ Typed context (.mul (uniform .E) right) ty := by
  intro typed
  generalize he : Expr.mul (uniform .E) right = expression at typed
  induction typed
  case sub h sub ih => exact ih he
  case mul left right ihl ihr =>
    cases he
    exact uniformE_not_G left
  all_goals cases he

example : ¬ Typed [] (.mul (uniform .E) (.real 2)) (.float .E) := mul_uniformE_not_typed _
example : ¬ Typed [] (.mul (uniform .E) (uniform .E)) (.float .E) := mul_uniformE_not_typed _

private theorem safe_next {expression next : Expr}
    (reduction : reduce expression = .next next) (safe : DoesNotGetStuck next) :
    DoesNotGetStuck expression := by
  intro fuel
  cases fuel with
  | zero => trivial
  | succ fuel =>
      rw [DoesNotGetStuckAt]
      split
      · trivial
      · rw [reduction]
        exact safe fuel

private theorem safe_sample {expression : Expr} {fiber : Measure ℝ} {continuation : ℝ → Expr}
    (reduction : reduce expression = .sample site fiber continuation) (mass : fiber Set.univ = 1)
    (safe : ∀ value, DoesNotGetStuck (continuation value)) : DoesNotGetStuck expression := by
  intro fuel
  cases fuel with
  | zero => trivial
  | succ fuel =>
      rw [DoesNotGetStuckAt]
      split
      · trivial
      · rw [reduction]
        exact ⟨mass, Filter.Eventually.of_forall (fun value => safe value fuel)⟩

private theorem safe_real (value : ℝ) : DoesNotGetStuck (.real value) := by
  intro fuel
  cases fuel <;> simp [DoesNotGetStuckAt, Expr.isValue]

private theorem safe_let_uniform (affinity : Affinity) (body : Expr)
    (safe : ∀ value, DoesNotGetStuck (body.substHead (.real value))) :
    DoesNotGetStuck (.letE (uniform affinity) body) := by
  let μ := uniformFiber (.sample affinity) 0 1
  have reduction : reduce (.letE (uniform affinity) body) =
      .sample (.sample affinity, .uniform) μ
        (fun value => .letE (.real value) body) := by
    simp [uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def, μ]
  apply safe_sample reduction
  · simp [μ, uniformFiber, uniformMeasure, Real.volume_Icc]
  · intro value
    exact safe_next (by simp [reduce, Expr.isValue]) (safe value)

theorem reciprocal_safe : DoesNotGetStuck reciprocal := by
  apply safe_let_uniform .E
  intro x
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, uniform]
  change DoesNotGetStuck (.letE (uniform .G)
    (.add (.real x) (.div (.real 1) (.bvar 0))))
  apply safe_let_uniform .G
  intro y
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars]
  apply safe_next (next := .add (.real x) (.real (1 / y)))
  · simp [reduce, Expr.isValue, realValue?, Action.wrap]
  apply safe_next (next := .real (x + 1 / y))
  · simp [reduce, Expr.isValue, realValue?]
  exact safe_real _

/-- This concrete trace result requires no global integrability premise. -/
example : Determinize.Proof.Traces.MeanOnTraces reciprocal reciprocal.determinize :=
  (Traces.meanOnTraces .E reciprocal reciprocal_typed reciprocal_source reciprocal_safe).2

/-- A general-affinity draw scales an expectation-affinity draw from the left. -/
def scaledSample : Expr := .letE (uniform .G) (.mul (.bvar 0) (uniform .E))

theorem scaledSample_source : scaledSample.sourceForm = true := by
  simp [scaledSample, uniform, Expr.sourceForm, DistributionAction.isSample]

theorem scaledSample_typed : Typed [] scaledSample (.float .E) :=
  .letE (uniform_typed .G) (.mul (.bvar .head) (uniform_typed .E))

theorem scaledSample_safe : DoesNotGetStuck scaledSample := by
  apply safe_let_uniform .G
  intro y
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, uniform]
  change DoesNotGetStuck (.mul (.real y) (uniform .E))
  let μ := uniformFiber (.sample .E) 0 1
  refine safe_sample (site := (.sample .E, .uniform)) (fiber := μ)
    (continuation := fun value => .mul (.real y) (.real value)) ?_ ?_ ?_
  · simp [uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def, μ]
  · simp [μ, uniformFiber, uniformMeasure, Real.volume_Icc]
  · intro value
    apply safe_next (next := .real (y * value))
    · simp [reduce, Expr.isValue, realValue?]
    exact safe_real _

example : Determinize.Proof.Traces.MeanOnTraces scaledSample scaledSample.determinize :=
  (Traces.meanOnTraces .E scaledSample scaledSample_typed scaledSample_source scaledSample_safe).2

def loopFunction : Expr :=
  .fix
    (.app (.bvar 1) (.bvar 0))

def loop : Expr := .app loopFunction .unit

example : Typed [] loop (.float .E) :=
  .app (.fix (.app (.bvar (.tail .head)) (.bvar .head))) .unit

example : loop.sourceForm = true := by simp [loop, loopFunction, Expr.sourceForm]

theorem loop_reduction : reduce loop = .next loop := by
  simp [loop, loopFunction, reduce, Expr.isValue, Expr.substTwo, Expr.substAt, Expr.shift, Expr.mapVars]

example : DoesNotGetStuck loop := by
  intro fuel
  induction fuel with
  | zero => trivial
  | succ fuel ih =>
      rw [DoesNotGetStuckAt, if_neg (by simp [loop, Expr.isValue]), loop_reduction]
      exact ih

example : traceAndOutputLaw loop = 0 := by
  have h (depth : Nat) : traceAndOutputLawAt depth loop = 0 := by
    induction depth with
    | zero => simp [loop, traceAndOutputLawAt]
    | succ depth ih =>
        rw [Traces.exact_succ_next depth loop loop (by simp [loop, Expr.isValue]) loop_reduction, ih]
  simp [traceAndOutputLaw, h]

end Determinize.Proof.Examples
