import Determinize.Proof.SymbolicTraceSteps
import Determinize.Proof.TraceFibers
import Determinize.Proof.SymbolicMoments

namespace Determinize.Proof.StepTraces

open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper Symbolic Symbolic.AffineExpr
open SymbolicSoundness.TargetSafety

noncomputable section

variable {α β γ : Type*} [MeasurableSpace α] [MeasurableSpace β] [MeasurableSpace γ]

theorem map_bind_fun (law : Measure α) (family : α → Measure β) (hm : Measurable family)
    (f : β → γ) (hf : Measurable f) :
    (law.bind family).map f = law.bind (fun a => (family a).map f) :=
  map_bind law ⟨family, hm⟩ f hf


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
  rw [averageKernel, Kernel.comp_apply, sfiniteKernel_mapWithInput_apply]
  change (law.map (fun value => (parameter, value))).bind body.kernel = _
  exact bind_map _ _ (measurable_const.prodMk measurable_id) _

def historyReplay (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) : SFiniteKernel (Trace) ℝ := by
  let : IsProbabilityMeasure (history.actualMeasure primitiveLaws) :=
    ⟨SymbolicSoundness.SampleEnv.actualMeasure_univ_eq_one _ history safe⟩
  exact averageKernel (history.actualMeasure primitiveLaws)
    (SFiniteKernel.pullback (replayKernel depth)
      (fun pair : Trace × Env n => (pair.1, expression.realize pair.2))
      (measurable_fst.prodMk (expression.realize_measurable.comp measurable_snd)))

theorem historyReplay_apply (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression : AffineExpr n) (tape : Trace) :
    (historyReplay depth history safe expression).kernel tape =
      (history.actualMeasure primitiveLaws).bind (fun env => replayMeasure depth (expression.realize env) tape) := by
  rw [historyReplay, averageKernel_apply]
  simp_rw [MeasurableActionFamily.pullback_apply, replayKernel_apply]

def actualTraceLaw (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) : Measure (Output) :=
  (history.actualMeasure primitiveLaws).bind (fun env => exactMeasure depth (expression.realize env))

def targetTraceLaw (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression : AffineExpr n) : Measure (Output) :=
  exactMeasure depth (expression.realize (history.meanEnvironment primitiveLaws)).determinize

theorem exact_measurable (depth : Nat) : Measurable (exactMeasure depth) := by
  have eq : exactMeasure depth = (exactKernel depth).kernel := funext fun e => (exactKernel_apply _ _).symm
  rw [eq]
  exact (exactKernel depth).kernel.measurable

theorem replay_measurable (depth : Nat) (tape : Trace) : Measurable (fun e => replayMeasure depth e tape) := by
  have eq : (fun e => replayMeasure depth e tape) = fun e => (replayKernel depth).kernel (tape,e) :=
    funext fun e => (replayKernel_apply _ _ _).symm
  rw [eq]
  exact (replayKernel depth).kernel.measurable.comp (measurable_const.prodMk measurable_id)

theorem generationOp_realize (expression : AffineExpr n) (env : Env n) :
    generationOp (expression.realize env).skeleton = generationOp expression.skeleton := by
  rw [AffineExpr.realize_skeleton]

theorem next_opNone (typed : WellTyped context expression ty)
    (actionEq : symbolicReduce laws expression = .next next) :
    generationOp expression.skeleton = none := by
  have h := symbolic_generationDraw laws typed
  rw [actionEq, generationDraw_next] at h
  cases eq : generationOp expression.skeleton <;> simp_all

theorem sampleE_opNone (typed : WellTyped context expression ty)
    (actionEq : symbolicReduce laws expression = .sampleE op affine general continuation) :
    generationOp expression.skeleton = none := by
  have h := symbolic_generationDraw laws typed
  rw [actionEq, generationDraw_sampleE] at h
  cases eq : generationOp expression.skeleton <;> simp_all

theorem sampleG_opSome (typed : WellTyped context expression ty)
    (actionEq : symbolicReduce laws expression = .sampleG site fiber continuation) :
    ∃ op, generationOp expression.skeleton = some op := by
  have h := symbolic_generationDraw laws typed
  rw [actionEq, generationDraw_sampleG] at h
  exact Option.isSome_iff_exists.mp h

theorem actualTraceLaw_next (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression next : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true)
    (actionEq : symbolicReduce primitiveLaws expression = .next next) :
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
  · rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
    rfl

theorem targetTraceLaw_next (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (expression next : AffineExpr n) (typed : WellTyped [] expression ty)
    (notValue : expression.isValue ≠ true)
    (actionEq : symbolicReduce primitiveLaws expression = .next next) :
    targetTraceLaw (depth+1) history expression =
      (targetTraceLaw depth history next).map (prepend none) := by
  apply exact_succ_next
  · simpa only [determinize_isValue, AffineExpr.realize_isValue] using notValue
  · rw [← symbolicReduce_targetRealize primitiveLaws typed, actionEq]
    rfl

theorem historyReplay_next (depth : Nat) (history : Symbolic.SampleEnv primitiveLaws n)
    (safe : history.DomainSafe primitiveLaws) (expression next : AffineExpr n)
    (typed : WellTyped [] expression ty) (notValue : expression.isValue ≠ true)
    (actionEq : symbolicReduce primitiveLaws expression = .next next) (tape : Trace) :
    (historyReplay (depth+1) history safe expression).kernel tape =
      (historyReplay depth history safe next).kernel (List.tail tape) := by
  rw [historyReplay_apply, historyReplay_apply]
  apply Measure.bind_congr_right
  filter_upwards [] with env
  apply replay_succ_next
  · simpa only [AffineExpr.realize_isValue] using notValue
  · rw [generationOp_realize, next_opNone typed actionEq]
  · rw [← symbolicReduce_realize primitiveLaws typed env, actionEq]
    rfl

end

end Determinize.Proof.StepTraces
