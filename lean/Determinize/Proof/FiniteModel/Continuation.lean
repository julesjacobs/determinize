import Determinize.Proof.FiniteModel.Transition

namespace Determinize.Proof.FiniteModel
open Spec.Paper Determinize.Finite Checking Binding MeasureTheory

def pushStack (state : State) (stack : List Frame) : State :=
  match state with
  | .eval expression environment saved => .eval expression environment (saved ++ stack)
  | .deliver value saved => .deliver value (saved ++ stack)
  | .rejected => .rejected

theorem pushStack_expr (state : State) (stack : List Frame) (notRejected : state ≠ .rejected) :
    stateExpr (pushStack state stack) = stackExpr stack (stateExpr state) := by
  cases state <;> simp_all [pushStack, stateExpr, stackExpr, List.foldl_append]

theorem lift_root (before after : State) (root : RootStep before after)
    (notValue : (stateExpr before).isValue = false)
    (beforeLive : before ≠ .rejected) (afterLive : after ≠ .rejected)
    (stack : List Frame) (shape : ∀ frame ∈ stack, FrameShape frame) :
    PaperStep (pushStack before stack) [(1,pushStack after stack)] := by
  have context := stack_context stack shape (stateExpr before) notValue
  apply paperStep_next
  · simpa [pushStack_expr before stack beforeLive] using context.1
  · rw [pushStack_expr before stack beforeLive, context.2, root.2]
    simp [Action.wrap, pushStack_expr after stack afterLive]

set_option maxHeartbeats 1200000 in
theorem deliver_stepMeaning (value : Value) (stack : List Frame)
    (shape : ∀ frame ∈ stack, FrameShape frame) (result : Step)
    (action : step (.deliver value stack) = .ok result) : StepMeaning (.deliver value stack) result := by
  cases stack with
  | nil =>
      cases value <;> simp only [step, reduceCtorEq, Except.ok.injEq] at action
      subst result
      exact stepMeaning_returned _
  | cons frame stack =>
      have tailShape : ∀ f ∈ stack, FrameShape f := fun f hf => shape f (by simp [hf])
      cases frame with
      | left operation right environment =>
          obtain rfl := Except.ok.inj action
          exact Or.inl ⟨_, rfl, trivial, sameObservations_of_eq _ _ (left_argument_step _ _ _ _ _).2⟩
      | draw site pending environment arguments =>
          cases value <;> simp only [step, pure, bind, Except.bind, Except.pure, throw] at action
          all_goals try contradiction
          rename_i x
          cases pending with
          | cons next rest =>
              obtain rfl := Except.ok.inj action
              exact Or.inl ⟨_, rfl, trivial, sameObservations_of_eq _ _ (draw_argument_step _ _ _ _ _ _ _).2⟩
          | nil =>
              change draw site (arguments ++ [x]) stack = .ok result at action
              unfold draw at action
              cases law : finiteLaw site.2.2 site.2.1 (arguments ++ [x]) with
              | error failure => simp [law, bind, Except.bind] at action
              | ok outcomes =>
                  simp only [law, bind, Except.bind, pure, Except.pure] at action
                  obtain rfl := Except.ok.inj action
                  apply Or.inr
                  apply paperStep_sample _ site (arguments ++ [x]) outcomes stack law
                  · have nv := (stack_context stack tailShape
                      (primitiveExpr site ((arguments ++ [x]).map fun (q : Rat) => .real (q : ℝ)))
                      (primitiveExpr_notValue _ _)).1
                    simpa only [stateExpr, stackExpr, List.foldl_cons, frameExpr, valueExpr,
                      List.map_append, List.map_cons, List.map_nil] using nv
                  · exact draw_correspondence site arguments x environment stack outcomes law tailShape
      | unary operation =>
          cases operation <;> cases value <;>
            simp only [step, unary, pure, bind, Except.bind, Except.pure, reduceCtorEq] at action
          all_goals obtain rfl := Except.ok.inj action
          all_goals try
            apply Or.inl
            refine ⟨_, rfl, trivial, sameObservations_of_eq _ _ ?_⟩
            simp [stateExpr, stackExpr, frameExpr, unaryExpr, valueExpr]
          all_goals apply Or.inr
          all_goals first
            | apply lift_root _ _ (fst_root _ _) (by simp [stateExpr, stackExpr, frameExpr, unaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (snd_root _ _) (by simp [stateExpr, stackExpr, frameExpr, unaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (neg_root _) (by simp [stateExpr, stackExpr, frameExpr, unaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
      | right operation left =>
          cases operation <;> cases left <;> cases value <;>
            simp only [step, binary, pure, bind, Except.bind, Except.pure, reduceCtorEq] at action
          all_goals obtain rfl := Except.ok.inj action
          all_goals try
            apply Or.inl
            refine ⟨_, rfl, trivial, sameObservations_of_eq _ _ ?_⟩
            simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr]
          all_goals apply Or.inr
          all_goals first
            | apply lift_root _ _ (closure_root _ _ _) (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (recursive_root _ _ _) (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (add_root _ _) (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (mul_root _ _) (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (div_root _ _) (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (lt_root _ _) (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
      | choose yes no environment =>
          cases value <;> simp only [step, pure, bind, Except.bind, Except.pure, throw, reduceCtorEq] at action
          obtain rfl := Except.ok.inj action
          apply Or.inr
          apply lift_root _ _ (branch_root _ _ _ _) (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
      | letBody body environment =>
          obtain rfl := Except.ok.inj action
          apply Or.inr
          apply lift_root _ _ (let_root _ _ _) (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
      | matchSum left right environment =>
          cases value <;> simp only [step, pure, bind, Except.bind, Except.pure, throw, reduceCtorEq] at action
          all_goals obtain rfl := Except.ok.inj action
          all_goals apply Or.inr
          all_goals first
            | apply lift_root _ _ (sum_left_root _ _ _ _) (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (sum_right_root _ _ _ _) (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
      | matchList nilCase consCase environment =>
          cases value <;> simp only [step, pure, bind, Except.bind, Except.pure, throw, reduceCtorEq] at action
          all_goals obtain rfl := Except.ok.inj action
          all_goals apply Or.inr
          all_goals first
            | apply lift_root _ _ (list_nil_root _ _ _) (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape
            | apply lift_root _ _ (list_cons_root _ _ _ _ _) (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
                (by simp) (by simp) stack tailShape

theorem stepMeaning (state : State) (shape : StateShape state) (result : Step)
    (action : step state = .ok result) : StepMeaning state result := by
  cases state with
  | eval expression environment stack => exact eval_stepMeaning expression environment stack shape result action
  | deliver value stack => exact deliver_stepMeaning value stack shape result action
  | rejected =>
      obtain rfl := Except.ok.inj action
      exact stepMeaning_rejected

end Determinize.Proof.FiniteModel
