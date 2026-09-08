import Determinize.Theorems

namespace Determinize.Proof.Examples
open MeasureTheory Determinize.Statement.Paper Determinize.Traces

def uniform (mode : Mode) : Expr :=
  .sample mode (.stochastic .uniform) [.real mode 0, .real mode 1] []

theorem uniform_typed (mode : Mode) : Typed context (uniform mode) (.float mode) := by
  cases mode <;> unfold uniform <;> constructor <;>
    simp_all [affineArity, generalArity] <;> aesop (add safe constructors Typed)

def nestedAffine : Expr :=
  .sample .E (.stochastic .uniform) [uniform .E, .add .E (.real .E 2) (.real .E 3)] []

def nestedGeneral : Expr :=
  .sample .E (.stochastic .gaussian) [.real .E 0] [uniform .G]

def reciprocal : Expr :=
  .letE (uniform .E)
    (.letE (uniform .G)
      (.add .E (.bvar 1)
        (.div .E (.real .E 1) (.bvar 0))))

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
      (fun value => .sample .E (.stochastic .gaussian) [.real .E 0] [.real .G value]) := by
  simp [nestedGeneral, uniform, reduce, Expr.isValue, firstNonValue, allRealValues?,
    Action.wrap, Function.comp_def]

example : reduce nestedGeneral.determinize =
    .sample (.G, .stochastic .uniform) (primitiveFiber (.stochastic .uniform) [0, 1] [])
      (fun value => .sample .E (.mean .gaussian) [.real .E 0] [.real .G value]) := by
  simp [nestedGeneral, uniform, Expr.determinize, reduce, Expr.isValue, firstNonValue,
    allRealValues?, Action.wrap, Function.comp_def]

example : reduce nestedAffine =
    .sample (.E, .stochastic .uniform) (primitiveFiber (.stochastic .uniform) [0, 1] [])
      (fun value => .sample .E (.stochastic .uniform)
        [.real .E value, .add .E (.real .E 2) (.real .E 3)] []) := by
  simp [nestedAffine, uniform, reduce, Expr.isValue, firstNonValue, allRealValues?,
    Action.wrap, Function.comp_def]

example : (Expr.sample .E (.mean .uniform) [.real .E 0, .real .E 1] []).sourceForm = false := by simp [Expr.sourceForm]

example : ¬ Typed [] (.mul .E (.real .E 1) (.real .E 1)) (.float .E) := by
  intro typed
  cases typed with
  | mul _ right => cases right
  | mulLeftG left _ => cases left

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

private theorem safe_real (mode : Mode) (value : ℝ) : DoesNotGetStuck (.real mode value) := by
  intro fuel
  cases fuel <;> simp [DoesNotGetStuckAt, Expr.isValue]

private theorem safe_let_uniform (mode : Mode) (body : Expr)
    (safe : ∀ value, DoesNotGetStuck (body.substHead (.real mode value))) :
    DoesNotGetStuck (.letE (uniform mode) body) := by
  let μ := primitiveFiber (.stochastic .uniform) [0,1] []
  have reduction : reduce (.letE (uniform mode) body) =
      .sample (mode, .stochastic .uniform) μ
        (fun value => .letE (.real mode value) body) := by
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
    (.add .E (.real .E x) (.div .E (.real .E 1) (.bvar 0))))
  apply safe_let_uniform .G
  intro y
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars]
  apply safe_next (next := .add .E (.real .E x) (.real .E (1 / y)))
  · simp [reduce, Expr.isValue, realValue?, Action.wrap]
  apply safe_next (next := .real .E (x + 1 / y))
  · simp [reduce, Expr.isValue, realValue?]
  exact safe_real .E _

/-- This concrete trace result requires no global integrability premise. -/
example : MeanOnTraces reciprocal reciprocal.determinize :=
  (Traces.soundness .E reciprocal reciprocal_typed reciprocal_source reciprocal_safe).2

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
