import Determinize.Theorems

namespace Determinize.Proof.Examples
open MeasureTheory Determinize.Spec.Paper Determinize.Spec.Traces

def uniform (affinity : Affinity) : Expr := .uniform (.sample affinity) (.real 0) (.real 1)

theorem typed_uniform (affinity : Affinity) : Typed context (uniform affinity) (.float affinity) :=
  .uniform .real .real

def nestedAffine : Expr := .uniform (.sample .E) (uniform .E) (.add (.real 2) (.real 3))

def nestedGeneral : Expr := .gaussian (.sample .E) (.real 0) (uniform .G)

def reciprocal : Expr :=
  .letE (uniform .E)
    (.letE (uniform .G)
      (.add (.bvar 1)
        (.div (.real 1) (.bvar 0))))

example : Typed [] nestedAffine (.float .E) := .uniform (typed_uniform .E) (.add .real .real)

example : Typed [] nestedGeneral (.float .E) := .gaussian .real (typed_uniform .G)

theorem typed_reciprocal : Typed [] reciprocal (.float .E) :=
  .letE (typed_uniform .E) (.letE (typed_uniform .G)
    (.add (.bvar (.tail .head)) (.div .real (.bvar .head))))

example : reduce nestedGeneral =
    .sample (.sample .G, .uniform) (uniformFiber (.sample .G) 0 1)
      (fun value ↦ .gaussian (.sample .E) (.real 0) (.real value)) := by
  simp [nestedGeneral, uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

example : reduce nestedGeneral.determinize =
    .sample (.sample .G, .uniform) (uniformFiber (.sample .G) 0 1)
      (fun value ↦ .gaussian .mean (.real 0) (.real value)) := by
  simp [nestedGeneral, uniform, Expr.determinize, DistributionAction.determinize, reduce,
    Expr.isValue,
    realValue?, Action.wrap, Function.comp_def]

example : reduce nestedAffine =
    .sample (.sample .E, .uniform) (uniformFiber (.sample .E) 0 1)
      (fun value ↦ .uniform (.sample .E) (.real value) (.add (.real 2) (.real 3))) := by
  simp [nestedAffine, uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

-- A literal takes either affinity, so a product of literals types at both affinities.
example : Typed [] (.mul (.real 2) (.real 1)) (.float .E) := .mul .real .real
example : Typed [] (.mul (.real 2) (.real 1)) (.float .G) := .mul .real .real

-- The general-affinity factor of an expectation-affinity product stands on the left: an
-- expectation-affinity draw may be scaled from the left, not from the right, and not squared.
example : Typed [] (.mul (.real 2) (uniform .E)) (.float .E) := .mul .real (typed_uniform .E)
private theorem not_typed_uniformE_G : ¬ Typed context (uniform .E) (.float .G) := by
  intro typed
  generalize he : uniform .E = expression at typed
  generalize ht : Ty.float .G = ty at typed
  induction typed
  case sub h sub ih =>
    cases sub <;> cases ht
    exact ih he rfl
  all_goals cases ht <;> simp [uniform] at he

private theorem not_typed_mul_uniformE (right : Expr) :
    ¬ Typed context (.mul (uniform .E) right) ty := by
  intro typed
  generalize he : Expr.mul (uniform .E) right = expression at typed
  induction typed
  case sub h sub ih => exact ih he
  case mul left right ihl ihr =>
    cases he
    exact not_typed_uniformE_G left
  all_goals cases he

example : ¬ Typed [] (.mul (uniform .E) (.real 2)) (.float .E) := not_typed_mul_uniformE _
example : ¬ Typed [] (.mul (uniform .E) (uniform .E)) (.float .E) := not_typed_mul_uniformE _

private theorem domainSafe_of_next {expression next : Expr}
    (reduction : reduce expression = .next next) (safe : DomainSafe next) :
    DomainSafe expression := by
  intro fuel
  cases fuel with
  | zero => trivial
  | succ fuel =>
    rw [DomainSafeAt]
    split
    · trivial
    · rw [reduction]
      exact safe fuel

private theorem domainSafe_of_sample {expression : Expr} {fiber : Measure ℝ}
    {continuation : ℝ → Expr}
    (reduction : reduce expression = .sample site fiber continuation) (mass : fiber Set.univ = 1)
    (safe : ∀ᵐ value ∂fiber, DomainSafe (continuation value)) : DomainSafe expression := by
  intro fuel
  cases fuel with
  | zero => trivial
  | succ fuel =>
    rw [DomainSafeAt]
    split
    · trivial
    · rw [reduction]
      exact ⟨mass, safe.mono (fun _ h ↦ h fuel)⟩

private theorem domainSafe_real (value : ℝ) : DomainSafe (.real value) := by
  intro fuel
  cases fuel <;> simp [DomainSafeAt, Expr.isValue]

private theorem domainSafe_let_uniform (affinity : Affinity) (body : Expr)
    (safe : ∀ᵐ value ∂uniformFiber (.sample affinity) 0 1,
      DomainSafe (body.substHead (.real value))) :
    DomainSafe (.letE (uniform affinity) body) := by
  let μ := uniformFiber (.sample affinity) 0 1
  have reduction : reduce (.letE (uniform affinity) body) =
      .sample (.sample affinity, .uniform) μ
        (fun value ↦ .letE (.real value) body) := by
    simp [uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def, μ]
  apply domainSafe_of_sample reduction
  · simp [μ, uniformFiber, uniformMeasure, Real.volume_Icc]
  · filter_upwards [safe] with value valueSafe
    exact domainSafe_of_next (by simp [reduce, Expr.isValue]) valueSafe

theorem domainSafe_reciprocal : DomainSafe reciprocal := by
  apply domainSafe_let_uniform .E
  apply Filter.Eventually.of_forall
  intro x
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, uniform]
  change DomainSafe (.letE (uniform .G)
    (.add (.real x) (.div (.real 1) (.bvar 0))))
  apply domainSafe_let_uniform .G
  have nonzero : ∀ᵐ y ∂uniformFiber (.sample .G) 0 1, y ≠ 0 := by
    simpa [uniformFiber, uniformMeasure] using
      (ae_restrict_of_ae (s := Set.Icc (0 : ℝ) 1) (volume.ae_ne (0 : ℝ)))
  filter_upwards [nonzero] with y nonzero
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars]
  apply domainSafe_of_next (next := .add (.real x) (.real (1 / y)))
  · simp [reduce, Expr.isValue, realValue?, Action.wrap, nonzero]
  apply domainSafe_of_next (next := .real (x + 1 / y))
  · simp [reduce, Expr.isValue, realValue?]
  exact domainSafe_real _

/-- This concrete trace result requires no global integrability premise. -/
example : Determinize.Proof.Traces.MeanOnTraces reciprocal reciprocal.determinize :=
  (Traces.meanOnTraces_determinize .E reciprocal typed_reciprocal domainSafe_reciprocal).2

/-- A general-affinity draw scales an expectation-affinity draw from the left. -/
def scaledSample : Expr := .letE (uniform .G) (.mul (.bvar 0) (uniform .E))

theorem typed_scaledSample : Typed [] scaledSample (.float .E) :=
  .letE (typed_uniform .G) (.mul (.bvar .head) (typed_uniform .E))

theorem domainSafe_scaledSample : DomainSafe scaledSample := by
  apply domainSafe_let_uniform .G
  apply Filter.Eventually.of_forall
  intro y
  simp [Expr.substHead, Expr.substAt, Expr.shift, Expr.mapVars, uniform]
  change DomainSafe (.mul (.real y) (uniform .E))
  let μ := uniformFiber (.sample .E) 0 1
  refine domainSafe_of_sample (site := (.sample .E, .uniform)) (fiber := μ)
    (continuation := fun value ↦ .mul (.real y) (.real value)) ?_ ?_ ?_
  · simp [uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def, μ]
  · simp [μ, uniformFiber, uniformMeasure, Real.volume_Icc]
  · apply Filter.Eventually.of_forall
    intro value
    apply domainSafe_of_next (next := .real (y * value))
    · simp [reduce, Expr.isValue, realValue?]
    exact domainSafe_real _

example : Determinize.Proof.Traces.MeanOnTraces scaledSample scaledSample.determinize :=
  (Traces.meanOnTraces_determinize .E scaledSample typed_scaledSample domainSafe_scaledSample).2

def loopFunction : Expr :=
  .fix
    (.app (.bvar 1) (.bvar 0))

def loop : Expr := .app loopFunction .unit

example : Typed [] loop (.float .E) :=
  .app (.fix (.app (.bvar (.tail .head)) (.bvar .head))) .unit

theorem reduce_loop : reduce loop = .next loop := by
  simp [loop, loopFunction, reduce, Expr.isValue, Expr.substTwo, Expr.substAt, Expr.shift,
    Expr.mapVars]

example : DomainSafe loop := by
  intro fuel
  induction fuel with
  | zero => trivial
  | succ fuel ih =>
    rw [DomainSafeAt, if_neg (by simp [loop, Expr.isValue]), reduce_loop]
    exact ih

example : traceAndOutputLaw loop = 0 := by
  have h (depth : Nat) : traceAndOutputLawAt depth loop = 0 := by
    induction depth with
    | zero => simp [loop, traceAndOutputLawAt]
    | succ depth ih =>
      rw [Traces.exact_succ_next depth loop loop (by simp [loop, Expr.isValue]) reduce_loop, ih]
  simp [traceAndOutputLaw, h]

def affineMean : Expr :=
  .letE (uniform .E) (.uniform .mean (.bvar 0) (.add (.bvar 0) (.real 2)))

theorem typed_affineMean : Typed [] affineMean (.float .E) :=
  .letE (typed_uniform .E) (.uniformMean (.bvar .head) (.add (.bvar .head) .real))

theorem domainSafe_affineMean : DomainSafe affineMean := by
  apply domainSafe_let_uniform .E
  apply Filter.Eventually.of_forall
  intro x
  simp only [Expr.substHead, Expr.substAt, Expr.mapVars, Expr.shift]
  apply domainSafe_of_next (next := .uniform .mean (.real x) (.real (x + 2)))
  · simp [reduce, Expr.isValue, realValue?, Action.wrap]
  apply domainSafe_of_sample (site := (.mean, .uniform)) (fiber := uniformFiber .mean x (x + 2))
    (continuation := Expr.real)
  · simp [reduce, Expr.isValue, realValue?]
  · simp [uniformFiber]
  · exact Filter.Eventually.of_forall domainSafe_real

example : Determinize.Proof.Traces.MeanOnTraces affineMean affineMean.determinize :=
  (Traces.meanOnTraces_determinize .E affineMean typed_affineMean domainSafe_affineMean).2

example : bigStepMeasure affineMean.determinize Set.univ = bigStepMeasure affineMean Set.univ :=
  Determinize.Theorems.output_mass_preservation affineMean typed_affineMean
    domainSafe_affineMean

def meanDenominator : Expr := .div (uniform .E) (.uniform .mean (.real 1) (.real 3))

example : Typed [] meanDenominator (.float .E) :=
  .div (typed_uniform .E) (.uniformMean .real .real)

example (affinity : Affinity) : Typed [] (.uniform .mean (.real 1) (.real 3)) (.float affinity) :=
  .uniformMean .real .real

def meanWithDraw : Expr := .gaussian .mean (.real 0) (uniform .G)

example : Typed [] meanWithDraw (.float .G) := .gaussianMean .real (typed_uniform .G)

example : reduce meanWithDraw =
    .sample (.sample .G, .uniform) (uniformFiber (.sample .G) 0 1)
      (fun value ↦ .gaussian .mean (.real 0) (.real value)) := by
  simp [meanWithDraw, uniform, reduce, Expr.isValue, realValue?, Action.wrap, Function.comp_def]

example : traceAndOutputLawAt 1 (.uniform .mean (.real 1) (.real 3)) =
    Measure.dirac ([], (2 : ℝ)) := by
  norm_num [traceAndOutputLawAt, reduce, Expr.isValue, realValue?, uniformFiber]
  have recordMean (v : ℝ) : (record (.mean, .uniform) v : Output → Output) = id := rfl
  simp_rw [recordMean, Measure.map_id]
  exact Measure.dirac_bind
    (show Measurable (fun value : ℝ ↦ Measure.dirac (([] : Trace), value)) from
      Measure.measurable_dirac.comp (measurable_const.prodMk measurable_id)) 2

example : traceAndOutputLawAt 1 (.uniform .mean (.real 3) (.real 1)) = 0 := by
  norm_num [traceAndOutputLawAt, reduce, Expr.isValue, realValue?, uniformFiber]

example : ¬ DomainSafe (.uniform .mean (.real 3) (.real 1)) := by
  intro safe
  have h := safe 1
  norm_num [DomainSafeAt, reduce, Expr.isValue, realValue?, uniformFiber] at h

example : Typed [] (.uniform .mean (.real 3) (.real 1)) (.float .E) :=
  .uniformMean .real .real

open scoped ENNReal

private theorem runningProbabilityAt_real (depth : Nat) (r : ℝ) :
    runningProbabilityAt depth (.real r) = 0 := by
  cases depth <;> simp [runningProbabilityAt, Expr.isValue]

private theorem runningProbabilityAt_reject (depth : Nat) :
    runningProbabilityAt depth .reject = 1 := by
  induction depth with
  | zero => rfl
  | succ depth ih => simpa [runningProbabilityAt, reduce, Expr.isValue] using ih

example (r : ℝ) : divergenceProbability (.real r) = 0 := by
  simp [divergenceProbability, runningProbabilityAt_real]

example : divergenceProbability .reject = 1 := by
  simp [divergenceProbability, runningProbabilityAt_reject]

example : divergenceProbability (.div (.real 1) (.real 0)) = 0 := by
  apply le_antisymm _ zero_le
  calc
    divergenceProbability (.div (.real 1) (.real 0)) ≤
        runningProbabilityAt 1 (.div (.real 1) (.real 0)) := iInf_le _ 1
    _ = 0 := by simp [runningProbabilityAt, reduce, Expr.isValue, realValue?]

private noncomputable def halfReject : Expr :=
  .ite (.lt (.bernoulli (.sample .G) (.real (1 / 2))) (.real 1)) (.real 7) .reject

private theorem runningProbabilityAt_halfReject (n : Nat) :
    runningProbabilityAt (n + 3) halfReject = 1 / 2 := by
  norm_num [halfReject, runningProbabilityAt, reduce, Expr.isValue, bernoulliFiber,
    Action.wrap, realValue?, runningProbabilityAt_real, runningProbabilityAt_reject,
      ENNReal.ofReal_div_of_pos]

example : divergenceProbability halfReject = 1 / 2 := by
  apply le_antisymm
  · exact (iInf_le _ 3).trans_eq (runningProbabilityAt_halfReject 0)
  · apply le_iInf
    intro n
    match n with
    | 0 | 1 | 2 =>
      norm_num [halfReject, runningProbabilityAt, reduce, Expr.isValue, bernoulliFiber,
        Action.wrap, realValue?, ENNReal.ofReal_div_of_pos]
    | n + 3 => exact (runningProbabilityAt_halfReject n).ge

example : divergenceProbability loop = 1 := by
  have running (depth : Nat) : runningProbabilityAt depth loop = 1 := by
    induction depth with
    | zero => rfl
    | succ depth ih =>
      simpa [runningProbabilityAt, loop, loopFunction, Expr.isValue, reduce,
        Expr.substTwo, Expr.substAt, Expr.shift, Expr.mapVars] using ih
  simp [divergenceProbability, running]

end Determinize.Proof.Examples
