import Determinize.Proof.FiniteModel.Progress
import Determinize.Proof.FiniteModel.Sampling
import Determinize.Proof.FiniteModel.Initial

namespace Determinize.Tests.SemanticBridge
open Spec.Paper Spec.FiniteModel Determinize.Finite Determinize.Checking
open Proof.FiniteModel Proof.FiniteModel.Binding

example :
    reduce (stateExpr (.deliver (.number 3)
      [.right .app (.closure (.lam (.add (.bvar 1) (.bvar 2))) [.number 7])])) =
    .next (.lam (.add (.real 3) (.real 7))) := by
  rw [(rootStep_closure _ _ _).2]
  simp [stateExpr, stackExpr, environmentExpr, valueExpr, close, interpret, Expr.map, Expr.mapVars]

example :
    reduce (stateExpr (.deliver (.number 5)
      [.right .app (.recursive (.app (.bvar 1) (.bvar 0)) [])])) =
    .next (.app (.fix (.app (.bvar 1) (.bvar 0))) (.real 5)) := by
  rw [(rootStep_recursive _ _ _).2]
  simp [stateExpr, stackExpr, environmentExpr, valueExpr, close, interpret, Expr.map, Expr.mapVars]

example :
    reduce (stateExpr (.deliver (.cons (.number 2) (.cons (.number 3) .nil))
      [.matchList (.real 0) (.pair (.bvar 0) (.bvar 1)) []])) =
    .next (.pair (.real 2) (.cons (.real 3) .nil)) := by
  rw [(rootStep_list_cons _ _ _ _ _).2]
  simp [stateExpr, stackExpr, environmentExpr, valueExpr, close, interpret, Expr.map, Expr.mapVars]

example :
    reduce (stateExpr (.deliver (.number 0) [.right .div (.number 7)])) = .stuck := by
  simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr, reduce, Expr.isValue, realValue?]

example :
    bigStepMeasure (.letE (.gaussian (.sample .G) (.real 2) .reject) (.real 7)) = 0 := by
  simpa [stackExpr, frameExpr, primitiveExpr, environmentExpr, close, interpret,
    Expr.map, Expr.mapVars] using
    bigStepMeasure_stackExpr_reject [.draw (.sample .G, .gaussian) [] [] [2], .letBody (.real 7) []]

example :
    DomainSafe (.letE (.gaussian (.sample .G) (.real 2) .reject) (.real 7)) := by
  simpa [stackExpr, frameExpr, primitiveExpr, environmentExpr, close, interpret,
    Expr.map, Expr.mapVars] using
    domainSafe_stackExpr_reject [.draw (.sample .G, .gaussian) [] [] [2], .letBody (.real 7) []]

example : ¬Scoped 0 (.lam (.bvar 1) : Expr) := by simp [Scoped]

private def source : Core := .app (.lam (.bvar 0)) (.uniform (.sample .E) (.real 0) (.real 2))

private theorem typed_source : Typed [] (interpret source) (.float .E) :=
  .app (.lam (.bvar .head)) (.uniform .real .real)

example : stateExpr (initialState source .source) = Subject.source.program source :=
  typed_initial_reification source .source _ typed_source

example : stateExpr (initialState source .determinized) =
    .app (.lam (.bvar 0)) (.uniform .mean (.real 0) (.real 2)) := by
  simpa [Subject.program, source, interpret, Expr.map, Expr.determinize,
    DistributionAction.determinize]
      using typed_initial_reification source .determinized _ typed_source

example : outcomeMeasure [(1, 0), (0, 1)] = MeasureTheory.Measure.dirac (0 : ℝ) := by
  simp [outcomeMeasure]

example : outcomeMeasure [(0, 0), (1, 1)] = MeasureTheory.Measure.dirac (1 : ℝ) := by
  simp [outcomeMeasure]

example : FiniteLawMatches .bernoulli (.sample .G) [0] [(1, 0), (0, 1)] :=
  finiteLaw_sound _ _ _ _ (by decide +kernel)

example : FiniteLawMatches .bernoulli (.sample .G) [1] [(0, 0), (1, 1)] :=
  finiteLaw_sound _ _ _ _ (by decide +kernel)

example : FiniteLawMatches .gamma .mean [3, 2] [(1, 3 / 2)] :=
  finiteLaw_sound _ _ _ _ (by decide +kernel)

example : ∀ outcomes, finiteLaw .bernoulli (.sample .G) [2] ≠ .ok outcomes := by
  intro outcomes
  simp [finiteLaw, bind, Except.bind, pure, Except.pure, throw]

example : ∀ outcomes, finiteLaw .uniform (.sample .G) [0, 0] ≠ .ok outcomes := by
  intro outcomes
  simp [finiteLaw, supportedDraw, bind, Except.bind, pure, Except.pure, throw]

example : reduce (stateExpr (.deliver (.number (1 / 3))
    [.draw (.sample .G, .bernoulli) [] [] [], .letBody .reject []])) =
      .sample (.sample .G, .bernoulli) (outcomeMeasure [(2 / 3, 0), (1 / 3, 1)])
        (fun y ↦ .letE (.real y) .reject) := by
  simpa [stackExpr, frameExpr, Binding.close, interpret, Expr.map, Expr.mapVars] using
    reduce_stateExpr_draw (.sample .G, .bernoulli) [] (1 / 3) [] [.letBody .reject []]
      [(2 / 3, 0), (1 / 3, 1)] (by decide +kernel) (by simp [FrameShape])

example (initial state : State) (shape : StateShape initial)
    (reachable : MachineReachable initial state) : StateShape state :=
  stateShape_of_reachable initial shape state reachable

example : ¬StateShape (.deliver (.number 1)
    [.draw (.sample .G, .bernoulli) [.real 2] [] []]) := by
  simp [StateShape, FrameShape, primitiveArity]

example : StateShape (.eval (.real 2) []
    [.draw (.mean, .uniform) [] [] [1]]) := by
  simp [StateShape, FrameShape, primitiveArity]

example : bookkeepingRank (.deliver (.number 1)
    [.draw (.mean, .uniform) [.add (.real 2) (.real 3)] [] []]) = 7 := by
  decide +kernel

example : BookkeepingTransition
    (.deliver (.number 1) [.draw (.mean, .uniform) [.add (.real 2) (.real 3)] [] []])
    (.eval (.add (.real 2) (.real 3)) [] [.draw (.mean, .uniform) [] [] [1]]) := by
  refine ⟨trivial, .continue, _, 1, rfl, ?_⟩
  simp

example (path : Nat → State) (steps : ∀ i < 4, BookkeepingTransition (path i) (path (i + 1)))
    (start : bookkeepingRank (path 0) = 3) : False := by
  have bound := bookkeepingRank_path_add_le path 4 steps
  omega

example : ¬Bookkeeping (.deliver (.number 1)
    [.draw (.sample .G, .bernoulli) [] [] []]) := by
  simp [Bookkeeping]

example : ¬Bookkeeping (.deliver .unit
    [.right .app (.recursive (.app (.bvar 1) (.bvar 0)) [])]) := by
  simp [Bookkeeping]

#print axioms stateShape_step
#print axioms stateShape_of_reachable_initialState
#print axioms reduce_stateExpr_draw_of_reachable
#print axioms bookkeepingRank_lt
#print axioms not_forall_bookkeepingTransition

#print axioms finiteLaw_sound
#print axioms isProbabilityMeasure_outcomeMeasure
#print axioms reduce_stateExpr_draw
#print axioms reduce_stateExpr_discrete
#print axioms rootStep_closure
#print axioms rootStep_recursive
#print axioms rootStep_list_cons
#print axioms step_eval_reject
#print axioms reduce_stackExpr
#print axioms replay_initial_reification

end Determinize.Tests.SemanticBridge
