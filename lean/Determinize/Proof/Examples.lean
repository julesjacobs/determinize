import Determinize.Theorems

namespace Determinize.Proof.Examples
open MeasureTheory Determinize.Statement.Paper Determinize.Traces

def uniform (mode : Mode) : Expr :=
  .sample mode (.stochastic .uniform) [.real 0, .real 1] []

theorem uniform_typed (mode : Mode) : Typed context (uniform mode) (.float mode) := by
  cases mode <;> unfold uniform <;> constructor <;>
    simp_all [affineArity, generalArity] <;> aesop (add safe constructors Typed)

def nestedAffine : Expr :=
  .sample .E (.stochastic .uniform) [uniform .E, .add (.real 2) (.real 3)] []

def nestedGeneral : Expr :=
  .sample .E (.stochastic .gaussian) [.real 0] [uniform .G]

def reciprocal : Expr :=
  .letE (uniform .E)
    (.letE (uniform .G)
      (.add (.bvar 1)
        (.div (.real 1) (.bvar 0))))

theorem nestedAffine_source : nestedAffine.sourceForm = true := by simp [nestedAffine, uniform, Expr.sourceForm]
example : nestedGeneral.sourceForm = true := by simp [nestedGeneral, uniform, Expr.sourceForm]
theorem reciprocal_source : reciprocal.sourceForm = true := by simp [reciprocal, uniform, Expr.sourceForm]

example : Typed [] nestedAffine (.float .E) := by
  unfold nestedAffine
  apply Typed.sample _ rfl rfl
  · intro e member
    simp only [List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl
    · exact uniform_typed .E
    · exact .add .real .real
  · simp

example : Typed [] nestedGeneral (.float .E) := by
  unfold nestedGeneral
  apply Typed.sample _ rfl rfl
  · intro e member
    simp only [List.mem_singleton] at member
    subst e
    exact .real
  · intro e member
    simp only [List.mem_singleton] at member
    subst e
    exact uniform_typed .G

theorem reciprocal_typed : Typed [] reciprocal (.float .E) :=
  .letE (uniform_typed .E) (.letE (uniform_typed .G)
    (.add (.bvar (.tail .head)) (.div .real (.bvar .head))))

example : reduce nestedGeneral =
    .sample (.G, .stochastic .uniform) (primitiveFiber (.stochastic .uniform) [0, 1] [])
      (fun value => .sample .E (.stochastic .gaussian) [.real 0] [.real value]) := by
  simp [nestedGeneral, uniform, reduce, Expr.isValue, firstNonValue, allRealValues?,
    Action.wrap, Function.comp_def]

example : reduce nestedGeneral.determinize =
    .sample (.G, .stochastic .uniform) (primitiveFiber (.stochastic .uniform) [0, 1] [])
      (fun value => .sample .E (.mean .gaussian) [.real 0] [.real value]) := by
  simp [nestedGeneral, uniform, Expr.determinize, reduce, Expr.isValue, firstNonValue,
    allRealValues?, Action.wrap, Function.comp_def]

example : reduce nestedAffine =
    .sample (.E, .stochastic .uniform) (primitiveFiber (.stochastic .uniform) [0, 1] [])
      (fun value => .sample .E (.stochastic .uniform)
        [.real value, .add (.real 2) (.real 3)] []) := by
  simp [nestedAffine, uniform, reduce, Expr.isValue, firstNonValue, allRealValues?,
    Action.wrap, Function.comp_def]

example : (Expr.sample .E (.mean .uniform) [.real 0, .real 1] []).sourceForm = false := by simp [Expr.sourceForm]

-- A literal takes either mode, so a product of literals types at both modes.
example : Typed [] (.mul (.real 2) (.real 1)) (.float .E) := .mul .real .real
example : Typed [] (.mul (.real 2) (.real 1)) (.float .G) := .mul .real .real

-- The general-mode factor of an expectation-mode product stands on the left: an
-- expectation-mode draw may be scaled from the left, not from the right, and not squared.
example : Typed [] (.mul (.real 2) (uniform .E)) (.float .E) := .mul .real (uniform_typed .E)
example : ¬ Typed [] (.mul (uniform .E) (.real 2)) (.float .E) := by
  intro typed
  cases typed with
  | mul left _ =>
      unfold uniform at left
      cases left
example : ¬ Typed [] (.mul (uniform .E) (uniform .E)) (.float .E) := by
  intro typed
  cases typed with
  | mul left _ =>
      unfold uniform at left
      cases left

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

private theorem safe_let_uniform (mode : Mode) (body : Expr)
    (safe : ∀ value, DoesNotGetStuck (body.substHead (.real value))) :
    DoesNotGetStuck (.letE (uniform mode) body) := by
  let μ := primitiveFiber (.stochastic .uniform) [0,1] []
  have reduction : reduce (.letE (uniform mode) body) =
      .sample (mode, .stochastic .uniform) μ
        (fun value => .letE (.real value) body) := by
    simp [uniform, reduce, Expr.isValue, firstNonValue, allRealValues?, Action.wrap, Function.comp_def, μ]
  apply safe_sample reduction
  · simp [μ, primitiveFiber, parseParams, affineArity, generalArity, paperMeasure,
      uniformMeasure, Real.volume_Icc]
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
example : MeanOnTraces reciprocal reciprocal.determinize :=
  (Traces.soundness .E reciprocal reciprocal_typed reciprocal_source reciprocal_safe).2

/-- A general-mode draw scales an expectation-mode draw from the left. -/
def scaledSample : Expr := .letE (uniform .G) (.mul (.bvar 0) (uniform .E))

theorem scaledSample_source : scaledSample.sourceForm = true := by
  simp [scaledSample, uniform, Expr.sourceForm]

theorem scaledSample_typed : Typed [] scaledSample (.float .E) :=
  .letE (uniform_typed .G) (.mul (.bvar .head) (uniform_typed .E))

theorem scaledSample_safe : DoesNotGetStuck scaledSample := by
  apply safe_let_uniform .G
  intro y
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, uniform]
  change DoesNotGetStuck (.mul (.real y) (uniform .E))
  let μ := primitiveFiber (.stochastic .uniform) [0, 1] []
  refine safe_sample (site := (.E, .stochastic .uniform)) (fiber := μ)
    (continuation := fun value => .mul (.real y) (.real value)) ?_ ?_ ?_
  · simp [uniform, reduce, Expr.isValue, firstNonValue, allRealValues?, Action.wrap,
      Function.comp_def, μ]
  · simp [μ, primitiveFiber, parseParams, affineArity, generalArity, paperMeasure,
      uniformMeasure, Real.volume_Icc]
  · intro value
    apply safe_next (next := .real (y * value))
    · simp [reduce, Expr.isValue, realValue?]
    exact safe_real _

example : MeanOnTraces scaledSample scaledSample.determinize :=
  (Traces.soundness .E scaledSample scaledSample_typed scaledSample_source scaledSample_safe).2

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

example : jointMeasure loop = 0 := by
  have h (depth : Nat) : exactMeasure depth loop = 0 := by
    induction depth with
    | zero => simp [loop, exactMeasure]
    | succ depth ih =>
        rw [Traces.exact_succ_next depth loop loop (by simp [loop, Expr.isValue]) loop_reduction, ih]
  simp [jointMeasure, h]

end Determinize.Proof.Examples
