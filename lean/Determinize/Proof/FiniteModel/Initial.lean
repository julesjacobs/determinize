import Determinize.Proof.FiniteModel.Administrative
import Determinize.Proof.FiniteModel.Replay
import Determinize.Proof.Checking.Elaboration

namespace Determinize.Proof.FiniteModel
open Statement.Paper Statement.FiniteModel Determinize.Finite Determinize.Checking Binding

theorem hasVar_scoped {context : List Ty} {index : Nat} {ty : Ty} (h : HasVar context index ty) :
    index < context.length := by
  induction h <;> simp_all

theorem typed_scoped {context : List Ty} {expression : Expr} {ty : Ty}
    (typed : Typed context expression ty) : Scoped context.length expression := by
  induction typed <;> simp_all [Scoped, Nat.add_comm, Nat.add_left_comm]
  exact hasVar_scoped ‹HasVar _ _ _›

theorem scoped_determinize {α : Type} (expression : Expr α) (depth : Nat) :
    Scoped depth expression.determinize ↔ Scoped depth expression := by
  induction expression generalizing depth <;> simp_all [Scoped, Expr.determinize]

theorem initial_reification (source : Core) (subject : Subject)
    (bounded : Scoped 0 (interpret source)) :
    stateExpr (initialState source subject) = subject.program source := by
  cases subject with
  | source =>
      simpa [stateExpr, initialState, environmentExpr, stackExpr, Subject.program, interpret] using
        close_empty (interpret source) 0 bounded
  | determinized =>
      simp only [stateExpr, initialState, environmentExpr, stackExpr, List.foldl_nil,
        Subject.program, Determinize.Proof.Checking.interpret_determinize]
      exact close_empty (interpret source).determinize 0 ((scoped_determinize _ _).mpr bounded)

theorem typed_initial_reification (source : Core) (subject : Subject) (ty : Ty)
    (typed : Typed [] (interpret source) ty) :
    stateExpr (initialState source subject) = subject.program source :=
  initial_reification source subject (typed_scoped typed)

theorem replay_initial_reification (source : Core) (subject : Subject) (candidate : Candidate)
    (valid : candidate.ReplayValid source subject) :
    stateExpr (candidate.state (candidate.toModel valid).initial) = subject.program source := by
  rw [replay_initial candidate valid]
  apply initial_reification source subject
  exact (scoped_mapLiteral source (fun q : Rat => (q : ℝ)) 0).mpr valid.aligned.source_scoped

end Determinize.Proof.FiniteModel
