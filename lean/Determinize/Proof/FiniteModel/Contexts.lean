import Determinize.Proof.FiniteModel.Reification

namespace Determinize.Proof.FiniteModel
open Spec.Paper Determinize.Finite Checking Binding MeasureTheory

def primitiveArity : Op → Nat
  | .discrete _ => 0
  | .poisson | .bernoulli | .exponential => 1
  | .uniform | .gaussian | .beta | .gamma => 2

def FrameShape : Frame → Prop
  | .draw site pending _ arguments =>
      arguments.length + 1 + pending.length = primitiveArity site.2.2
  | _ => True

theorem frame_context (frame : Frame) (shape : FrameShape frame) (hole : Expr)
    (notValue : hole.isValue = false) :
    (frameExpr frame hole).isValue = false ∧
      reduce (frameExpr frame hole) = (reduce hole).wrap (frameExpr frame) := by
  cases frame with
  | unary op =>
      cases op <;> simp [frameExpr, unaryExpr, Expr.isValue, reduce, notValue] <;> rfl
  | left op right environment =>
      cases op <;> simp [frameExpr, binaryExpr, Expr.isValue, reduce, notValue] <;> rfl
  | right op left =>
      cases op <;> simp [frameExpr, binaryExpr, Expr.isValue, reduce, notValue, valueExpr_isValue] <;> rfl
  | choose yes no environment | letBody body environment
  | matchSum left right environment | matchList nilCase consCase environment =>
      simp [frameExpr, Expr.isValue, reduce, notValue]
      rfl
  | draw site pending environment arguments =>
      rcases site with ⟨mode,kind,op⟩
      rcases arguments with _ | ⟨a, _ | ⟨b, arguments⟩⟩ <;>
        rcases pending with _ | ⟨p, _ | ⟨q, pending⟩⟩ <;>
        cases op <;> simp_all [FrameShape, primitiveArity, frameExpr, primitiveExpr,
          Expr.isValue, reduce] <;> first | rfl | omega

theorem wrap_wrap (action : Action) (first second : Expr → Expr) :
    (action.wrap first).wrap second = action.wrap (second ∘ first) := by
  cases action <;> rfl

theorem stack_context (stack : List Frame) (shape : ∀ frame ∈ stack, FrameShape frame)
    (hole : Expr) (notValue : hole.isValue = false) :
    (stackExpr stack hole).isValue = false ∧
      reduce (stackExpr stack hole) = (reduce hole).wrap (stackExpr stack) := by
  induction stack generalizing hole with
  | nil =>
      refine ⟨notValue, ?_⟩
      cases h : reduce hole <;> simp [stackExpr, Action.wrap, h]
      rfl
  | cons frame stack ih =>
      have outerShape : ∀ f ∈ stack, FrameShape f := fun f hf => shape f (by simp [hf])
      have inner := frame_context frame (shape frame (by simp)) hole notValue
      have outer := ih outerShape (frameExpr frame hole) inner.1
      refine ⟨outer.1, ?_⟩
      change reduce (stackExpr stack (frameExpr frame hole)) = _
      rw [outer.2, inner.2, wrap_wrap]
      rfl

/-- Evaluation frames preserve an absorbing nonvalue, including a rejection. -/
theorem frame_absorbing (frame : Frame) (hole : Expr)
    (notValue : hole.isValue = false) (absorbing : reduce hole = .next hole) :
    (frameExpr frame hole).isValue = false ∧
      reduce (frameExpr frame hole) = .next (frameExpr frame hole) := by
  cases frame with
  | unary op =>
      cases op <;> simp [frameExpr, unaryExpr, Expr.isValue, reduce, notValue, absorbing, Action.wrap]
  | left op right environment =>
      cases op <;> simp [frameExpr, binaryExpr, Expr.isValue, reduce, notValue, absorbing, Action.wrap]
  | right op left =>
      cases op <;> simp [frameExpr, binaryExpr, Expr.isValue, reduce, notValue, absorbing,
        valueExpr_isValue, Action.wrap]
  | choose yes no environment | letBody body environment
  | matchSum left right environment | matchList nilCase consCase environment =>
      simp [frameExpr, Expr.isValue, reduce, notValue, absorbing, Action.wrap]
  | draw site pending environment arguments =>
      rcases site with ⟨mode,kind,op⟩
      rcases arguments with _ | ⟨a, _ | ⟨b, arguments⟩⟩ <;>
        rcases pending with _ | ⟨p, _ | ⟨q, pending⟩⟩ <;>
        cases op <;> simp [frameExpr, primitiveExpr, Expr.isValue, reduce,
          notValue, absorbing, Action.wrap]

theorem stack_absorbing (stack : List Frame) (hole : Expr)
    (notValue : hole.isValue = false) (absorbing : reduce hole = .next hole) :
    (stackExpr stack hole).isValue = false ∧
      reduce (stackExpr stack hole) = .next (stackExpr stack hole) := by
  induction stack generalizing hole with
  | nil => exact ⟨notValue, absorbing⟩
  | cons frame stack ih =>
      have frameResult := frame_absorbing frame hole notValue absorbing
      exact ih (frameExpr frame hole) frameResult.1 frameResult.2

theorem absorbing_cumulative_zero (expression : Expr)
    (notValue : expression.isValue = false) (absorbing : reduce expression = .next expression)
    (fuel : Nat) : cumulativeOutputMeasure fuel expression = 0 := by
  induction fuel with
  | zero => cases expression <;> simp_all [Expr.isValue, cumulativeOutputMeasure]
  | succ fuel ih => simpa [cumulativeOutputMeasure, absorbing] using ih

theorem absorbing_safe (expression : Expr)
    (notValue : expression.isValue = false) (absorbing : reduce expression = .next expression) :
    DoesNotGetStuck expression := by
  intro fuel
  induction fuel with
  | zero => trivial
  | succ fuel ih => simpa [DoesNotGetStuckAt, notValue, absorbing] using ih

theorem stack_reject_zero (stack : List Frame) :
    bigStepMeasure (stackExpr stack .reject) = 0 := by
  have absorbing := stack_absorbing stack .reject rfl rfl
  simp [bigStepMeasure, absorbing_cumulative_zero _ absorbing.1 absorbing.2]

theorem stack_reject_safe (stack : List Frame) :
    DoesNotGetStuck (stackExpr stack .reject) := by
  have absorbing := stack_absorbing stack .reject rfl rfl
  exact absorbing_safe _ absorbing.1 absorbing.2

/-- Dropping a machine stack on rejection agrees with the paper output law.
The paper expression need not reduce to a bare rejection. -/
theorem rejection_step (environment : List Value) (stack : List Frame) :
    step (.eval .reject environment stack) = .ok (.next .evaluate [(1,.rejected)]) ∧
    bigStepMeasure (stateExpr (.eval .reject environment stack)) =
      bigStepMeasure (stateExpr .rejected) ∧
    DoesNotGetStuck (stateExpr (.eval .reject environment stack)) := by
  refine ⟨rfl, ?_, ?_⟩
  · simp [stateExpr, interpret, close, Expr.mapLiteral, Expr.mapVars, stack_reject_zero,
      show bigStepMeasure (.reject : Expr) = 0 from stack_reject_zero []]
  · simpa [stateExpr, interpret, close, Expr.mapLiteral, Expr.mapVars] using stack_reject_safe stack

end Determinize.Proof.FiniteModel
