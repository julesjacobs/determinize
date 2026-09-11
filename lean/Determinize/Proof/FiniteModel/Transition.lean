import Determinize.Proof.FiniteModel.Local

namespace Determinize.Proof.FiniteModel
open Statement.Paper Determinize.Finite Checking Binding MeasureTheory

set_option maxHeartbeats 800000 in
theorem eval_stepMeaning (expression : Core) (environment : List Value) (stack : List Frame)
    (shape : ∀ frame ∈ stack, FrameShape frame) (result : Step)
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
    | exact reject_same environment stack
    | apply sameObservations_of_eq
      first
      | exact (variable_step _ _ _ _ (by assumption)).2
      | exact (binary_setup .app _ _ _ _).2
      | exact (binary_setup .pair _ _ _ _).2
      | exact (binary_setup .cons _ _ _ _).2
      | exact (binary_setup .add _ _ _ _).2
      | exact (binary_setup .mul _ _ _ _).2
      | exact (binary_setup .div _ _ _ _).2
      | exact (binary_setup .lt _ _ _ _).2
      | exact (unary_setup .fst _ _ _).2
      | exact (unary_setup .snd _ _ _).2
      | exact (unary_setup .inl _ _ _).2
      | exact (unary_setup .inr _ _ _).2
      | exact (unary_setup .neg _ _ _).2
      | exact (closure_setup _ _ _ ).2
      | exact (recursive_setup _ _ _ ).2
      | exact (let_setup _ _ _ _ ).2
      | exact (branch_setup _ _ _ _ _ ).2
      | exact (sum_setup _ _ _ _ _ ).2
      | exact (list_setup _ _ _ _ _ ).2
      | exact (number_setup _ _ _ ).2
      | exact (bool_setup _ _ _ ).2
      | exact (unit_setup _ _ ).2
      | exact (nil_setup _ _ ).2
      | exact (uniform_setup _ _ _ _ _ _ ).2
      | exact (gaussian_setup _ _ _ _ _ _ ).2
      | exact (beta_setup _ _ _ _ _ _ ).2
      | exact (gamma_setup _ _ _ _ _ _ ).2
      | exact (poisson_setup _ _ _ _ _ ).2
      | exact (bernoulli_setup _ _ _ _ _ ).2
      | exact (exponential_setup _ _ _ _ _ ).2
  case discrete mode kind d =>
    unfold draw at action
    cases law : finiteLaw (.discrete d) kind [] with
    | error failure => simp [law, bind, Except.bind] at action
    | ok outcomes =>
        simp only [law, bind, Except.bind, pure, Except.pure] at action
        obtain rfl := Except.ok.inj action
        apply Or.inr
        apply paperStep_sample _ (mode,kind,.discrete d) [] outcomes stack law
        · simpa [stateExpr, close, interpret, Expr.mapLiteral, Expr.mapVars, primitiveExpr] using
            (stack_context stack shape (primitiveExpr (mode,kind,.discrete d) [])
              (primitiveExpr_notValue _ _)).1
        · exact discrete_correspondence mode kind d environment stack outcomes law shape

end Determinize.Proof.FiniteModel
