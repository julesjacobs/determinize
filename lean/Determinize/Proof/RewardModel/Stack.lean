import Determinize.Proof.FiniteModel.Continuation
import Determinize.Proof.RewardModel.Measure

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Proof.FiniteModel MeasureTheory

def extend (stack : List Frame) (outcomes : List (Rat × State)) : List (Rat × State) :=
  outcomes.map fun (p,s) => (p, pushStack s stack)

private theorem draw_push (site : Spec.Paper.DistributionAction × Spec.Paper.Op)
    (args : List Rat) (inner outer : List Frame) (tag : Evidence) (outcomes : List (Rat × State))
    (action : draw site args inner = .ok (.next tag outcomes)) :
    draw site args (inner ++ outer) = .ok (.next tag (extend outer outcomes)) := by
  unfold draw at action ⊢
  cases law : finiteLaw site.2 site.1 args with
  | error e => simp [law, bind, Except.bind] at action
  | ok xs =>
    simp only [law, bind, Except.bind, pure, Except.pure, Except.ok.injEq, Step.next.injEq] at action ⊢
    obtain ⟨rfl, rfl⟩ := action
    simp [extend, List.map_map, pushStack, Function.comp_def]

set_option maxHeartbeats 1600000 in
theorem step_push (state : State) (outer : List Frame) (tag : Evidence)
    (outcomes : List (Rat × State)) (action : step state = .ok (.next tag outcomes)) :
    step (pushStack state outer) = .ok (.next tag (extend outer outcomes)) := by
  cases state with
  | rejected => simp [step] at action
  | eval expression env stack =>
    cases expression <;>
      simp only [pushStack, step, pure, bind, Except.bind, Except.pure] at action ⊢
    all_goals repeat' (split at action)
    all_goals simp_all [extend, pushStack]
    all_goals obtain ⟨rfl, rfl⟩ := action
    all_goals rfl
  | deliver value stack =>
    cases stack with
    | nil => cases value <;> simp [step] at action
    | cons frame stack =>
      cases frame with
      | discrete kind =>
        cases pv : value.probabilities? with
        | none =>
          simp only [step, pv] at action
          contradiction
        | some ps =>
          simp only [step, pv] at action
          simpa [pushStack, step, pv, bind, Except.bind, Except.pure] using
            draw_push (kind, .discrete ps.length) ps stack outer tag outcomes action
      | draw site pending env args =>
        cases value <;> simp only [step, pure, bind, Except.bind, Except.pure] at action
        all_goals try contradiction
        rename_i x
        cases pending with
        | nil =>
          simpa [pushStack, step, bind, Except.bind, Except.pure] using
            draw_push site (args ++ [x]) stack outer tag outcomes action
        | cons next rest =>
          obtain ⟨rfl, rfl⟩ := Step.next.inj (Except.ok.inj action)
          rfl
      | unary op =>
        cases op <;> cases value <;>
          simp [step, unary, pure, bind, Except.bind, Except.pure] at action
        all_goals obtain ⟨rfl, rfl⟩ := action
        all_goals rfl
      | right op left =>
        cases op <;> cases left <;> cases value <;>
          simp only [step, binary, pure, bind, Except.bind, Except.pure] at action
        all_goals try split_ifs at action
        all_goals repeat' (split at action)
        all_goals simp_all [step, binary, pushStack, extend, pure, bind, Except.bind, Except.pure]
        all_goals obtain ⟨rfl, rfl⟩ := action
        all_goals subst_vars
        all_goals rfl
      | left op rhs env =>
        obtain ⟨rfl, rfl⟩ := Step.next.inj (Except.ok.inj action)
        rfl
      | letBody body env =>
        obtain ⟨rfl, rfl⟩ := Step.next.inj (Except.ok.inj action)
        rfl
      | choose yes no env =>
        cases value <;> simp [step, pure, bind, Except.bind, Except.pure] at action
        obtain ⟨rfl, rfl⟩ := action
        rfl
      | matchSum left right env =>
        cases value <;> simp [step, pure, bind, Except.bind, Except.pure] at action
        all_goals obtain ⟨rfl, rfl⟩ := action
        all_goals rfl
      | matchList nilCase consCase env =>
        cases value <;> simp [step, pure, bind, Except.bind, Except.pure] at action
        all_goals obtain ⟨rfl, rfl⟩ := action
        all_goals rfl

end Determinize.Proof.RewardModel
