import Determinize.Proof.SymbolicTraceSteps
import Determinize.Proof.TraceFibers
import Determinize.Proof.SymbolicMoments

/-!
# Trace laws of a symbolic configuration

`actualTraceLaw` is the exact-depth trace/output law of the source realized under the history's
actual law and `targetTraceLaw` that of the target realized at the history's mean environment:
the two laws the lockstep argument compares (`CompactFiberSoundness`). This file unfolds them
along values, deterministic steps and rejections.
-/

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety

noncomputable section

variable {α β γ : Type*} [MeasurableSpace α] [MeasurableSpace β] [MeasurableSpace γ]

theorem map_bind_fun (law : Measure α) (family : α → Measure β) (hm : Measurable family)
    (f : β → γ) (hf : Measurable f) :
    (law.bind family).map f = law.bind (fun a => (family a).map f) :=
  map_bind law ⟨family, hm⟩ f hf


/-- Average `body` over a draw from `law` paired with the parameter (`averageKernel_apply`). -/
def averageKernel (law : Measure β) [SFinite law] (body : SFiniteKernel (α × β) γ) :
    SFiniteKernel α γ := by
  let draw : SFiniteKernel α β := ⟨Kernel.const α law, inferInstance⟩
  let paired := SFiniteKernel.mapWithInput draw id measurable_id
  let := paired.sfinite
  let := body.sfinite
  exact ⟨body.kernel ∘ₖ paired.kernel, inferInstance⟩

theorem averageKernel_apply (law : Measure β) [SFinite law]
    (body : SFiniteKernel (α × β) γ) (parameter : α) :
    (averageKernel law body).kernel parameter = law.bind (fun value => body.kernel (parameter, value)) := by
  rw [averageKernel, Kernel.comp_apply, SFiniteKernel.mapWithInput_apply]
  change (law.map (fun value => (parameter, value))).bind body.kernel = _
  exact bind_map _ _ (measurable_const.prodMk measurable_id) _

/-- The exact-depth trace/output law of the source: `exactMeasure` of the realization,
integrated over the history's actual law. -/
def actualTraceLaw (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) : Measure (Output) :=
  (history.actualMeasure primitiveLaws).bind (fun env => exactMeasure depth (expression.realize env))

/-- The exact-depth trace/output law of the target: `exactMeasure` of the determinized
realization at the history's mean environment. -/
def targetTraceLaw (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) : Measure (Output) :=
  exactMeasure depth (expression.realize (history.meanEnvironment primitiveLaws)).determinize

theorem exact_measurable (depth : Nat) : Measurable (exactMeasure depth) := by
  have eq : exactMeasure depth = (exactKernel depth).kernel := funext fun e => (exactKernel_apply _ _).symm
  rw [eq]
  exact (exactKernel depth).kernel.measurable

theorem generationOp_realize (expression : AffineExpr n) (env : Env n) :
    generationOp (expression.realize env).skeleton = generationOp expression.skeleton := by
  rw [AffineExpr.realize_skeleton]

theorem sampleE_opNone (typed : WellTyped context expression ty)
    (actionEq : symbolicReduce expression = .sampleE op affine general continuation) :
    generationOp expression.skeleton = none := by
  have h := symbolic_generationDraw typed
  rw [actionEq, generationDraw_sampleE] at h
  cases eq : generationOp expression.skeleton <;> simp_all

theorem sampleG_opSome (typed : WellTyped context expression ty)
    (actionEq : symbolicReduce expression = .sampleG site fiber continuation) :
    ∃ op, generationOp expression.skeleton = some op := by
  have h := symbolic_generationDraw typed
  rw [actionEq, generationDraw_sampleG] at h
  exact Option.isSome_iff_exists.mp h

/-- A value has no trace at a positive depth. -/
theorem actualTraceLaw_succ_value (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (value : expression.isValue = true) :
    actualTraceLaw (depth + 1) history expression = 0 := by
  unfold actualTraceLaw
  have h : ∀ env, exactMeasure (depth + 1) (expression.realize env) = 0 := by
    intro env
    rw [exactMeasure, if_pos (by simpa only [AffineExpr.realize_isValue] using value)]
  simp_rw [h]
  simp

/-- A value has no trace at a positive depth, on the target side either. -/
theorem targetTraceLaw_succ_value (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (value : expression.isValue = true) :
    targetTraceLaw (depth + 1) history expression = 0 := by
  rw [targetTraceLaw, exactMeasure,
    if_pos (by simpa only [determinize_isValue, AffineExpr.realize_isValue] using value)]

/-- An expression that is not a value has no trace at depth zero. -/
theorem actualTraceLaw_zero_of_not_value (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (notValue : expression.isValue ≠ true) :
    actualTraceLaw 0 history expression = 0 := by
  unfold actualTraceLaw
  have h : ∀ env, exactMeasure 0 (expression.realize env) = 0 := by
    intro env
    cases expression <;> simp_all [AffineExpr.realize, exactMeasure, AffineExpr.isValue]
  simp_rw [h]
  simp

/-- An expression that is not a value has no trace at depth zero, on the target side either. -/
theorem targetTraceLaw_zero_of_not_value (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) (notValue : expression.isValue ≠ true) :
    targetTraceLaw 0 history expression = 0 := by
  have nv : (expression.realize (history.meanEnvironment primitiveLaws)).determinize.isValue
      ≠ true := by
    simpa only [determinize_isValue, AffineExpr.realize_isValue] using notValue
  unfold targetTraceLaw
  generalize eq : (expression.realize (history.meanEnvironment primitiveLaws)).determinize = e
    at nv ⊢
  cases e <;> first | rfl | simp_all [Expr.isValue]

/-- A deterministic step keeps the trace law, up to the recorded empty event. -/
theorem actualTraceLaw_next (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression next : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true)
    (actionEq : symbolicReduce expression = .next next) :
    actualTraceLaw (depth+1) history expression =
      (actualTraceLaw depth history next).map (prepend none) := by
  rw [actualTraceLaw, actualTraceLaw,
    map_bind_fun _ (fun env => exactMeasure depth (next.realize env)) ((exact_measurable depth).comp next.realize_measurable) (prepend none)
      (show Measurable (prepend none : Output → Output) from
        prepend_measurable.comp (measurable_const.prodMk measurable_id))]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  apply exact_succ_next
  · simpa only [AffineExpr.realize_isValue] using notValue
  · rw [← symbolicReduce_realize typed env, actionEq]
    rfl

/-- A deterministic step keeps the target trace law, up to the recorded empty event. -/
theorem targetTraceLaw_next (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression next : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true)
    (actionEq : symbolicReduce expression = .next next) :
    targetTraceLaw (depth+1) history expression =
      (targetTraceLaw depth history next).map (prepend none) := by
  apply exact_succ_next
  · simpa only [determinize_isValue, AffineExpr.realize_isValue] using notValue
  · rw [← symbolicReduce_targetRealize typed, actionEq]
    rfl

end

end Determinize.Proof.StepTraces
