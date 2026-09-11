import Determinize.Proof.FiniteModel.Sampling
import Determinize.Proof.FiniteModel.Replay

namespace Determinize.Proof.FiniteModel
open Spec.Paper Spec.FiniteModel Determinize.Finite Checking

def StateShape : State → Prop
  | .eval _ _ stack | .deliver _ stack => ∀ frame ∈ stack, FrameShape frame
  | .rejected => True

private theorem singleton_shape (state : State) (shape : StateShape state)
    (tag evidence : Evidence) (successors : List (Rat × State))
    (action : (Except.ok (.next tag [(1,state)]) : Except Failure Step) = .ok (.next evidence successors))
    (probability : Rat) (after : State) (member : (probability, after) ∈ successors) :
    StateShape after := by
  obtain ⟨rfl, rfl⟩ := Step.next.inj (Except.ok.inj action)
  obtain ⟨rfl, rfl⟩ := Prod.mk.inj (List.mem_singleton.mp member)
  exact shape

private theorem draw_shape (site : DistributionAction × Op) (arguments : List Rat)
    (stack : List Frame) (shape : ∀ frame ∈ stack, FrameShape frame)
    (evidence : Evidence) (successors : List (Rat × State))
    (action : draw site arguments stack = .ok (.next evidence successors))
    (probability : Rat) (after : State) (member : (probability, after) ∈ successors) :
    StateShape after := by
  unfold draw at action
  cases law : finiteLaw site.2 site.1 arguments with
  | error failure => simp [law, bind, Except.bind] at action
  | ok outcomes =>
      simp only [law, bind, Except.bind, pure, Except.pure] at action
      obtain ⟨rfl, rfl⟩ := Step.next.inj (Except.ok.inj action)
      obtain ⟨⟨p,x⟩, _, equal⟩ := List.mem_map.mp member
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj equal
      exact shape

private theorem binary_shape (op : Binary) (left right : Value) (stack : List Frame)
    (shape : ∀ frame ∈ stack, FrameShape frame) (result : State)
    (success : binary op left right stack = .ok result) : StateShape result := by
  cases op <;> cases left <;> cases right <;>
    simp only [binary, reduceCtorEq, Except.ok.injEq] at success
  all_goals subst result
  all_goals exact shape

set_option maxHeartbeats 1000000 in
theorem step_shape (before : State) (shape : StateShape before)
    (evidence : Evidence) (successors : List (Rat × State))
    (action : step before = .ok (.next evidence successors))
    (probability : Rat) (after : State) (member : (probability, after) ∈ successors) :
    StateShape after := by
  cases before with
  | rejected => simp [step] at action
  | eval expression environment stack =>
      cases expression <;>
        simp only [step, pure, bind, Except.bind, Except.pure, throw] at action
      all_goals repeat' (split at action)
      all_goals try first
        | contradiction
        | apply singleton_shape _ ?_ _ _ _ action _ _ member
          clear action member
          simp_all [StateShape, FrameShape, primitiveArity]
      all_goals try exact draw_shape _ _ _ shape _ _ action _ _ member
  | deliver value stack =>
      cases stack with
      | nil => cases value <;> simp [step] at action
      | cons frame stack =>
          have tailShape : ∀ frame ∈ stack, FrameShape frame :=
            fun f hf => shape f (by simp [hf])
          cases frame <;> cases value <;>
            simp only [step, unary, pure, bind, Except.bind, Except.pure, throw] at action
          all_goals repeat' (split at action)
          all_goals try first
            | contradiction
            | apply singleton_shape _ ?_ _ _ _ action _ _ member
              clear action member
              simp_all [StateShape, FrameShape, primitiveArity]
              all_goals omega
          all_goals try exact draw_shape _ _ _ tailShape _ _ action _ _ member
          all_goals try
            apply singleton_shape _ ?_ _ _ _ action _ _ member
            exact binary_shape _ _ _ _ tailShape _ (by assumption)

theorem reachable_shape (initial : State) (shape : StateShape initial) (state : State)
    (reachable : MachineReachable initial state) : StateShape state := by
  induction reachable with
  | initial => exact shape
  | next previous action member positive ih => exact step_shape _ ih _ _ action _ _ member

theorem initial_shape (source : Core) (subject : Subject) :
    StateShape (initialState source subject) := by
  simp [initialState, StateShape]

theorem program_reachable_shape (source : Core) (subject : Subject) (state : State)
    (reachable : MachineReachable (initialState source subject) state) : StateShape state :=
  reachable_shape _ (initial_shape source subject) state reachable

theorem reachable_draw_correspondence (source : Core) (subject : Subject)
    (site : DistributionAction × Op) (arguments : List Rat) (x : Rat)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (reachable : MachineReachable (initialState source subject)
      (.deliver (.number x) (.draw site [] environment arguments :: stack)))
    (success : finiteLaw site.2 site.1 (arguments ++ [x]) = .ok outcomes) :
    reduce (stateExpr (.deliver (.number x) (.draw site [] environment arguments :: stack))) =
      .sample site (outcomeMeasure outcomes) (fun y => stackExpr stack (.real y)) := by
  apply draw_correspondence site arguments x environment stack outcomes success
  have shape := program_reachable_shape source subject _ reachable
  exact fun frame member => shape frame (by simp [member])

theorem reachable_discrete_correspondence (source : Core) (subject : Subject)
    (kind : DistributionAction) (d : FiniteDistribution)
    (environment : List Value) (stack : List Frame) (outcomes : List (Rat × Rat))
    (reachable : MachineReachable (initialState source subject)
      (.eval (.discrete kind d) environment stack))
    (success : finiteLaw (.discrete d) kind [] = .ok outcomes) :
    reduce (stateExpr (.eval (.discrete kind d) environment stack)) =
      .sample (kind, .discrete d) (outcomeMeasure outcomes)
        (fun y => stackExpr stack (.real y)) :=
  discrete_correspondence kind d environment stack outcomes success
    (program_reachable_shape source subject _ reachable)

end Determinize.Proof.FiniteModel
