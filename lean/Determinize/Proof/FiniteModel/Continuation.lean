import Determinize.Proof.FiniteModel.Transition

namespace Determinize.Proof.FiniteModel
open Spec.Paper Determinize.Finite Checking Binding MeasureTheory

def pushStack (state : State) (stack : List Frame) : State :=
  match state with
  | .eval expression environment saved => .eval expression environment (saved ++ stack)
  | .deliver value saved => .deliver value (saved ++ stack)
  | .rejected => .rejected

theorem stateExpr_pushStack (state : State) (stack : List Frame) (notRejected : state ≠ .rejected) :
    stateExpr (pushStack state stack) = stackExpr stack (stateExpr state) := by
  cases state <;> simp_all [pushStack, stateExpr, stackExpr, List.foldl_append]

theorem paperStep_pushStack (before after : State) (root : RootStep before after)
    (notValue : (stateExpr before).isValue = false)
    (beforeLive : before ≠ .rejected) (afterLive : after ≠ .rejected)
    (stack : List Frame) (shape : ∀ frame ∈ stack, FrameShape frame) :
    PaperStep (pushStack before stack) [(1, pushStack after stack)] := by
  have context := reduce_stackExpr stack shape (stateExpr before) notValue
  apply paperStep_next
  · simpa [stateExpr_pushStack before stack beforeLive] using context.1
  · rw [stateExpr_pushStack before stack beforeLive, context.2, root.2]
    simp [Action.wrap, stateExpr_pushStack after stack afterLive]

theorem stepMeaning_deliver (value : Value) (stack : List Frame)
    (shape : ∀ frame ∈ stack, FrameShape frame) (result : Step)
    (action : step (.deliver value stack) = .ok result) :
    StepMeaning (.deliver value stack) result := by
  cases stack with
  | nil =>
    cases value <;> simp only [step, reduceCtorEq, Except.ok.injEq] at action
    subst result
    exact stepMeaning_returned _
  | cons frame stack =>
    have tailShape : ∀ f ∈ stack, FrameShape f := fun f hf ↦ shape f (by simp [hf])
    cases frame with
    | left operation right environment =>
      obtain rfl := Except.ok.inj action
      exact Or.inl
        ⟨_, rfl, trivial, sameObservations_of_eq _ _ (administrativeStep_left_argument _ _ _ _ _).2⟩
    | discrete kind =>
      cases read : value.probabilities? with
      | none =>
        simp only [step, read] at action
        change Except.error (Failure.invalid "expected a list of probabilities") =
          Except.ok result at action
        contradiction
      | some p =>
        simp only [step, read] at action
        change draw (kind, .discrete p.length) p stack = .ok result at action
        unfold draw at action
        cases law : finiteLaw (.discrete p.length) kind p with
        | error failure => simp [law, bind, Except.bind] at action
        | ok outcomes =>
          simp only [law, bind, Except.bind, pure, Except.pure] at action
          obtain rfl := Except.ok.inj action
          apply Or.inr
          apply paperStep_sample _ (kind, .discrete p.length) p outcomes stack law
          · simpa [stateExpr, stackExpr, List.foldl_cons, frameExpr] using
              (reduce_stackExpr stack tailShape (.discrete kind (valueExpr value)) rfl).1
          · exact reduce_stateExpr_discrete kind value p read stack outcomes law tailShape
    | draw site pending environment arguments =>
      cases value <;> simp only [step, pure, bind, Except.bind, Except.pure, throw] at action
      all_goals try contradiction
      rename_i x
      cases pending with
      | cons next rest =>
        obtain rfl := Except.ok.inj action
        exact Or.inl
          ⟨_, rfl, trivial,
            sameObservations_of_eq _ _ (administrativeStep_draw_argument _ _ _ _ _ _ _).2⟩
      | nil =>
        change draw site (arguments ++ [x]) stack = .ok result at action
        unfold draw at action
        cases law : finiteLaw site.2 site.1 (arguments ++ [x]) with
        | error failure => simp [law, bind, Except.bind] at action
        | ok outcomes =>
          simp only [law, bind, Except.bind, pure, Except.pure] at action
          obtain rfl := Except.ok.inj action
          apply Or.inr
          apply paperStep_sample _ site (arguments ++ [x]) outcomes stack law
          · have nv := (reduce_stackExpr stack tailShape
              (primitiveExpr site ((arguments ++ [x]).map fun (q : Rat) ↦ .real (q : ℝ)))
              (isValue_primitiveExpr _ _)).1
            simpa only [stateExpr, stackExpr, List.foldl_cons, frameExpr, valueExpr,
              List.map_append, List.map_cons, List.map_nil] using nv
          · exact reduce_stateExpr_draw site arguments x environment stack outcomes law tailShape
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
        | apply paperStep_pushStack _ _ (rootStep_fst _ _)
            (by simp [stateExpr, stackExpr, frameExpr, unaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_snd _ _)
            (by simp [stateExpr, stackExpr, frameExpr, unaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_neg _)
            (by simp [stateExpr, stackExpr, frameExpr, unaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
    | right operation left =>
      cases operation <;> cases left <;> cases value <;>
        simp only [step, binary, pure, bind, Except.bind, Except.pure, reduceCtorEq] at action
      all_goals try split_ifs at action
      all_goals try simp only [beq_iff_eq] at *
      all_goals obtain rfl := Except.ok.inj action
      all_goals try
        apply Or.inl
        refine ⟨_, rfl, trivial, sameObservations_of_eq _ _ ?_⟩
        simp [stateExpr, stackExpr, frameExpr, binaryExpr, valueExpr]
      all_goals apply Or.inr
      all_goals first
        | apply paperStep_pushStack _ _ (rootStep_closure _ _ _)
            (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_recursive _ _ _)
            (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_add _ _)
            (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_mul _ _)
            (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_div _ _ (by assumption))
            (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_lt _ _)
            (by simp [stateExpr, stackExpr, frameExpr, binaryExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
    | choose yes no environment =>
      cases value <;>
        simp only [step, pure, bind, Except.bind, Except.pure, throw, reduceCtorEq] at action
      obtain rfl := Except.ok.inj action
      apply Or.inr
      apply paperStep_pushStack _ _ (rootStep_branch _ _ _ _)
        (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
        (by simp) (by simp) stack tailShape
    | letBody body environment =>
      obtain rfl := Except.ok.inj action
      apply Or.inr
      apply paperStep_pushStack _ _ (rootStep_let _ _ _)
        (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
        (by simp) (by simp) stack tailShape
    | matchSum left right environment =>
      cases value <;>
        simp only [step, pure, bind, Except.bind, Except.pure, throw, reduceCtorEq] at action
      all_goals obtain rfl := Except.ok.inj action
      all_goals apply Or.inr
      all_goals first
        | apply paperStep_pushStack _ _ (rootStep_sum_left _ _ _ _)
            (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_sum_right _ _ _ _)
            (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
    | matchList nilCase consCase environment =>
      cases value <;>
        simp only [step, pure, bind, Except.bind, Except.pure, throw, reduceCtorEq] at action
      all_goals obtain rfl := Except.ok.inj action
      all_goals apply Or.inr
      all_goals first
        | apply paperStep_pushStack _ _ (rootStep_list_nil _ _ _)
            (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape
        | apply paperStep_pushStack _ _ (rootStep_list_cons _ _ _ _ _)
            (by simp [stateExpr, stackExpr, frameExpr, Expr.isValue])
            (by simp) (by simp) stack tailShape

theorem stepMeaning (state : State) (shape : StateShape state) (result : Step)
    (action : step state = .ok result) : StepMeaning state result := by
  cases state with
  | eval expression environment stack =>
    exact stepMeaning_eval expression environment stack result action
  | deliver value stack => exact stepMeaning_deliver value stack shape result action
  | rejected =>
    obtain rfl := Except.ok.inj action
    exact stepMeaning_rejected

end Determinize.Proof.FiniteModel
