import Determinize.Proof.FiniteModel.Local

namespace Determinize.Proof.FiniteModel
open Spec.Paper Determinize.Finite Checking Binding MeasureTheory

theorem stepMeaning_eval (expression : Core) (environment : List Value) (stack : List Frame)
    (result : Step)
    (action : step (.eval expression environment stack) = .ok result) :
    StepMeaning (.eval expression environment stack) result := by
  cases expression <;>
    simp only [step, pure, bind, Except.bind, Except.pure, throw] at action
  all_goals repeat' (split at action)
  all_goals try contradiction
  all_goals try
    obtain rfl := Except.ok.inj action
    apply Or.inl
    refine ⟨_, rfl, trivial, ?_⟩
    first
    | exact sameObservations_reject environment stack
    | apply sameObservations_of_eq
      first
      | exact (administrativeStep_variable _ _ _ _ (by assumption)).2
      | exact (administrativeStep_binary .app _ _ _ _).2
      | exact (administrativeStep_binary .pair _ _ _ _).2
      | exact (administrativeStep_binary .cons _ _ _ _).2
      | exact (administrativeStep_binary .add _ _ _ _).2
      | exact (administrativeStep_binary .mul _ _ _ _).2
      | exact (administrativeStep_binary .div _ _ _ _).2
      | exact (administrativeStep_binary .lt _ _ _ _).2
      | exact (administrativeStep_unary .fst _ _ _).2
      | exact (administrativeStep_unary .snd _ _ _).2
      | exact (administrativeStep_unary .inl _ _ _).2
      | exact (administrativeStep_unary .inr _ _ _).2
      | exact (administrativeStep_unary .neg _ _ _).2
      | exact (administrativeStep_closure _ _ _ ).2
      | exact (administrativeStep_recursive _ _ _ ).2
      | exact (administrativeStep_let _ _ _ _ ).2
      | exact (administrativeStep_branch _ _ _ _ _ ).2
      | exact (administrativeStep_sum _ _ _ _ _ ).2
      | exact (administrativeStep_list _ _ _ _ _ ).2
      | exact (administrativeStep_number _ _ _ ).2
      | exact (administrativeStep_bool _ _ _ ).2
      | exact (administrativeStep_unit _ _ ).2
      | exact (administrativeStep_nil _ _ ).2
      | exact (administrativeStep_uniform _ _ _ _ _ ).2
      | exact (administrativeStep_gaussian _ _ _ _ _ ).2
      | exact (administrativeStep_beta _ _ _ _ _ ).2
      | exact (administrativeStep_gamma _ _ _ _ _ ).2
      | exact (administrativeStep_poisson _ _ _ _ ).2
      | exact (administrativeStep_bernoulli _ _ _ _ ).2
      | exact (administrativeStep_exponential _ _ _ _ ).2
      | exact (administrativeStep_discrete _ _ _ _).2

end Determinize.Proof.FiniteModel
