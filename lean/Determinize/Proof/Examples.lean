import Determinize.Theorems

namespace Determinize.Proof.Examples
open MeasureTheory Determinize.Statement.Paper Determinize.Traces

def uniform (mode : Mode) : Expr := .uniform mode .stochastic (.real 0) (.real 1)

theorem uniform_typed (mode : Mode) : Typed context (uniform mode) (.float mode) :=
  .uniform .real .real

def nestedAffine : Expr := .uniform .E .stochastic (uniform .E) (.add (.real 2) (.real 3))

def nestedGeneral : Expr := .gaussian .E .stochastic (.real 0) (uniform .G)

def reciprocal : Expr :=
  .letE (uniform .E)
    (.letE (uniform .G)
      (.add (.bvar 1)
        (.div (.real 1) (.bvar 0))))

theorem nestedAffine_source : nestedAffine.sourceForm = true := by simp [nestedAffine, uniform, Expr.sourceForm]
example : nestedGeneral.sourceForm = true := by simp [nestedGeneral, uniform, Expr.sourceForm]
theorem reciprocal_source : reciprocal.sourceForm = true := by simp [reciprocal, uniform, Expr.sourceForm]

example : Typed [] nestedAffine (.float .E) := .uniform (uniform_typed .E) (.add .real .real)

example : Typed [] nestedGeneral (.float .E) := .gaussian .real (uniform_typed .G)

theorem reciprocal_typed : Typed [] reciprocal (.float .E) :=
  .letE (uniform_typed .E) (.letE (uniform_typed .G)
    (.add (.bvar (.tail .head)) (.div .real (.bvar .head))))

example : reduce nestedGeneral =
    .sample (.G, .stochastic, .uniform) (uniformFiber .stochastic 0 1)
      (fun value => .gaussian .E .stochastic (.real 0) (.real value)) := by
  simp [nestedGeneral, uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

example : reduce nestedGeneral.determinize =
    .sample (.G, .stochastic, .uniform) (uniformFiber .stochastic 0 1)
      (fun value => .gaussian .E .mean (.real 0) (.real value)) := by
  simp [nestedGeneral, uniform, Expr.determinize, Expr.determinizeKind, reduce, Expr.isValue,
    realValue?, Action.wrap, Function.comp_def]

example : reduce nestedAffine =
    .sample (.E, .stochastic, .uniform) (uniformFiber .stochastic 0 1)
      (fun value => .uniform .E .stochastic (.real value) (.add (.real 2) (.real 3))) := by
  simp [nestedAffine, uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

example : (Expr.uniform .E .mean (.real 0) (.real 1)).sourceForm = false := by
  simp [Expr.sourceForm, Kind.isStochastic]

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
  let μ := uniformFiber .stochastic 0 1
  have reduction : reduce (.letE (uniform mode) body) =
      .sample (mode, .stochastic, .uniform) μ
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
example : MeanOnTraces reciprocal reciprocal.determinize :=
  (Traces.soundness .E reciprocal reciprocal_typed reciprocal_source reciprocal_safe).2

/-- The law of total variance along the traces of the same program. -/
example : VarianceOnTraces reciprocal reciprocal.determinize :=
  Traces.varianceSoundness .E reciprocal reciprocal_typed reciprocal_source reciprocal_safe

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
  let μ := uniformFiber .stochastic 0 1
  refine safe_sample (site := (.E, .stochastic, .uniform)) (fiber := μ)
    (continuation := fun value => .mul (.real y) (.real value)) ?_ ?_ ?_
  · simp [uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def, μ]
  · simp [μ, uniformFiber, uniformMeasure, Real.volume_Icc]
  · intro value
    apply safe_next (next := .real (y * value))
    · simp [reduce, Expr.isValue, realValue?]
    exact safe_real _

example : MeanOnTraces scaledSample scaledSample.determinize :=
  (Traces.soundness .E scaledSample scaledSample_typed scaledSample_source scaledSample_safe).2

/-- `bernoulli_E(1/3)`: an expectation-mode Bernoulli draw with a literal probability. -/
noncomputable def bernoulliLiteral : Expr := .bernoulli .E .stochastic (.real (1 / 3))

theorem bernoulliLiteral_typed : Typed [] bernoulliLiteral (.float .E) := .bernoulli .real

theorem bernoulliLiteral_source : bernoulliLiteral.sourceForm = true := by
  simp [bernoulliLiteral, Expr.sourceForm]

/-- Determinization switches the Bernoulli site to its mean site. -/
example : bernoulliLiteral.determinize = .bernoulli .E .mean (.real (1 / 3)) := rfl

/-- A mean site of `bernoulli(p)` returns the probability `p` itself. -/
example (probability : ℝ) (bounds : 0 ≤ probability ∧ probability ≤ 1) :
    bernoulliFiber .mean probability = Measure.dirac probability := by
  simp [bernoulliFiber, bounds]

private theorem bernoulliFiber_stochastic_mass (probability : ℝ)
    (bounds : 0 ≤ probability ∧ probability ≤ 1) :
    bernoulliFiber .stochastic probability Set.univ = 1 := by
  rw [bernoulliFiber, if_pos bounds]
  simp only [Measure.add_apply, Measure.smul_apply, smul_eq_mul, measure_univ, mul_one]
  rw [← ENNReal.ofReal_add (by linarith [bounds.2]) bounds.1, sub_add_cancel, ENNReal.ofReal_one]

theorem bernoulliLiteral_safe : DoesNotGetStuck bernoulliLiteral := by
  refine safe_sample (site := (.E, .stochastic, .bernoulli))
    (fiber := bernoulliFiber .stochastic (1 / 3)) (continuation := .real) ?_ ?_ ?_
  · simp [bernoulliLiteral, reduce, Expr.isValue, realValue?]
  · exact bernoulliFiber_stochastic_mass _ (by norm_num)
  · exact safe_real

/-- The mean of `bernoulli_E(1/3)` is preserved along traces by its determinization `1/3`. -/
example : MeanOnTraces bernoulliLiteral bernoulliLiteral.determinize :=
  (Traces.soundness .E bernoulliLiteral bernoulliLiteral_typed bernoulliLiteral_source
    bernoulliLiteral_safe).2

/-- `bernoulli_E(uniform_E(0, 1))`: the probability is itself an expectation-mode draw. -/
def bernoulliNested : Expr := .bernoulli .E .stochastic (uniform .E)

theorem bernoulliNested_typed : Typed [] bernoulliNested (.float .E) :=
  .bernoulli (uniform_typed .E)

example : bernoulliNested.sourceForm = true := by
  simp [bernoulliNested, uniform, Expr.sourceForm]

/-- Both sites become mean sites: the parameter evaluates to `1/2`, then so does the draw. -/
example : bernoulliNested.determinize =
    .bernoulli .E .mean (.uniform .E .mean (.real 0) (.real 1)) := rfl

example : reduce bernoulliNested.determinize =
    .sample (.E, .mean, .uniform) (uniformFiber .mean 0 1)
      (fun value => .bernoulli .E .mean (.real value)) := by
  simp [bernoulliNested, uniform, Expr.determinize, Expr.determinizeKind, reduce, Expr.isValue,
    realValue?, Action.wrap, Function.comp_def]

/-- `flip` draws a general-mode Bernoulli value and compares it with `0`. -/
example : reduce (.flip (.real (1 / 2))) =
    .sample (.G, .stochastic, .bernoulli) (bernoulliFiber .stochastic (1 / 2))
      (fun value => .lt (.real 0) (.real value)) := by
  simp [Expr.flip, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

/-- `if flip(1/2) then uniform_E(0, 1) else 0`: a Boolean draw chooses between expectation-mode
values. -/
noncomputable def flipBranch : Expr := .ite (.flip (.real (1 / 2))) (uniform .E) (.real 0)

theorem flipBranch_typed : Typed [] flipBranch (.float .E) :=
  .ite (.flip .real) (uniform_typed .E) .real

example : flipBranch.sourceForm = true := by
  simp [flipBranch, uniform, Expr.flip, Expr.sourceForm]

/-- The `flip` is a general-mode site and stays stochastic; only `uniform_E` becomes a mean
site. -/
example : flipBranch.determinize =
    .ite (.flip (.real (1 / 2))) (.uniform .E .mean (.real 0) (.real 1)) (.real 0) := rfl

/-- `flip` needs a general-mode probability: an expectation-mode draw is rejected. -/
example : ¬ Typed [] (.flip (uniform .E)) .bool := by
  intro typed
  unfold Expr.flip at typed
  cases typed with
  | lt _ right =>
      cases right with
      | bernoulli probability =>
          unfold uniform at probability
          cases probability

/-- `discrete_E(1/2, 1/4, 1/4)`: the index `0`, `1` or `2` with the listed weights. -/
noncomputable def discreteSample : Expr := .discrete .E .stochastic [1 / 2, 1 / 4, 1 / 4]

theorem discreteSample_typed : Typed [] discreteSample (.float .E) := .discrete

theorem discreteSample_source : discreteSample.sourceForm = true := by
  simp [discreteSample, Expr.sourceForm, Kind.isStochastic]

/-- Determinization switches the site to its mean site. -/
example : discreteSample.determinize = .discrete .E .mean [1 / 2, 1 / 4, 1 / 4] := rfl

/-- The mean site returns `∑ i, wᵢ · i = 1/4 + 2/4`. -/
example : reduce discreteSample.determinize =
    .sample (.E, .mean, .discrete 3) (Measure.dirac (3 / 4)) .real := by
  simp [discreteSample, Expr.determinize, Expr.determinizeKind, reduce, discreteFiber,
    Fin.sum_univ_three, Fin.forall_fin_succ]
  norm_num

private theorem discreteFiber_stochastic_mass (weights : List ℝ)
    (bounds : (∀ i : Fin weights.length, 0 ≤ weights[i]) ∧
      ∑ i : Fin weights.length, weights[i] = 1) :
    discreteFiber .stochastic weights Set.univ = 1 := by
  rw [discreteFiber, if_pos bounds]
  simp only [Measure.finsetSum_apply, Measure.smul_apply, smul_eq_mul, measure_univ, mul_one]
  rw [← ENNReal.ofReal_sum_of_nonneg fun i _ => bounds.1 i, bounds.2, ENNReal.ofReal_one]

theorem discreteSample_safe : DoesNotGetStuck discreteSample := by
  refine safe_sample (site := (.E, .stochastic, .discrete 3))
    (fiber := discreteFiber .stochastic [1 / 2, 1 / 4, 1 / 4]) (continuation := .real) ?_ ?_ ?_
  · simp [discreteSample, reduce]
  · refine discreteFiber_stochastic_mass _ ⟨?_, ?_⟩
    · intro i
      fin_cases i <;> norm_num
    · norm_num [Fin.sum_univ_three]
  · exact safe_real

/-- The mean of `discrete_E(1/2, 1/4, 1/4)` is preserved along traces by its determinization. -/
example : MeanOnTraces discreteSample discreteSample.determinize :=
  (Traces.soundness .E discreteSample discreteSample_typed discreteSample_source
    discreteSample_safe).2

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
