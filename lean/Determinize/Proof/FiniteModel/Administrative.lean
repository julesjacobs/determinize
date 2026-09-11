import Determinize.Proof.FiniteModel.Reduction

namespace Determinize.Proof.FiniteModel
open Spec.Paper Determinize.Finite Checking Binding

/-- Machine bookkeeping may leave the represented paper expression unchanged. -/
def AdministrativeStep (evidence : Evidence) (before after : State) : Prop :=
  step before = .ok (.next evidence [(1,after)]) ∧ stateExpr before = stateExpr after

theorem environmentExpr_eq_map (environment : List Value) :
    environmentExpr environment = environment.map valueExpr := by
  induction environment <;> simp_all [environmentExpr]

theorem variable_step (environment : List Value) (stack : List Frame) (index : Nat) (value : Value)
    (lookup : environment[index]? = some value) :
    AdministrativeStep .evaluate (.eval (.bvar index) environment stack) (.deliver value stack) := by
  constructor
  · simp [step, lookup]; rfl
  · simp [stateExpr, interpret, close, Expr.mapLiteral, Expr.mapVars,
      environmentExpr_eq_map, List.getElem?_map, lookup]

theorem binary_setup (operation : Binary) (left right : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (binaryExpr operation left right) environment stack)
      (.eval left environment (.left operation right environment :: stack)) := by
  cases operation <;> refine ⟨rfl, ?_⟩ <;>
    simp [stateExpr, stackExpr, frameExpr, binaryExpr, close, Expr.mapVars, interpret, Expr.mapLiteral]

theorem unary_setup (operation : Unary) (body : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (unaryExpr operation body) environment stack)
      (.eval body environment (.unary operation :: stack)) := by
  cases operation <;> refine ⟨rfl, ?_⟩ <;>
    simp [stateExpr, stackExpr, frameExpr, unaryExpr, close, Expr.mapVars, interpret, Expr.mapLiteral]

theorem left_argument_step (operation : Binary) (right : Core) (left : Value)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .continue (.deliver left (.left operation right environment :: stack))
      (.eval right environment (.right operation left :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr]

theorem closure_setup (body : Core) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.lam body) environment stack)
      (.deliver (.closure body environment) stack) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, valueExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem recursive_setup (body : Core) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.fix body) environment stack)
      (.deliver (.recursive body environment) stack) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, valueExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem let_setup (value body : Core) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.letE value body) environment stack)
      (.eval value environment (.letBody body environment :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem branch_setup (condition yes no : Core) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.ite condition yes no) environment stack)
      (.eval condition environment (.choose yes no environment :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem sum_setup (value left right : Core) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.matchSum value left right) environment stack)
      (.eval value environment (.matchSum left right environment :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem list_setup (value nilCase consCase : Core) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.matchList value nilCase consCase) environment stack)
      (.eval value environment (.matchList nilCase consCase environment :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem draw_argument_step (site : Mode × Kind × Op) (next : Core) (rest : List Core)
    (arguments : List Rat) (value : Rat) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .continue
      (.deliver (.number value) (.draw site (next :: rest) environment arguments :: stack))
      (.eval next environment (.draw site rest environment (arguments ++ [value]) :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, valueExpr, List.map_append, List.append_assoc]

theorem number_setup (value : Rat) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.real value) environment stack) (.deliver (.number value) stack) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, valueExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem bool_setup (value : Bool) (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.bool value) environment stack) (.deliver (.bool value) stack) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, valueExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem unit_setup  (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval .unit environment stack) (.deliver .unit stack) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, valueExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem nil_setup  (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval .nil environment stack) (.deliver .nil stack) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, valueExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem uniform_setup (mode : Mode) (kind : Kind) (left right : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.uniform mode kind left right) environment stack)
      (.eval left environment (.draw (mode,kind,.uniform) [right] environment [] :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, primitiveExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem gaussian_setup (mode : Mode) (kind : Kind) (left right : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.gaussian mode kind left right) environment stack)
      (.eval left environment (.draw (mode,kind,.gaussian) [right] environment [] :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, primitiveExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem beta_setup (mode : Mode) (kind : Kind) (left right : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.beta mode kind left right) environment stack)
      (.eval left environment (.draw (mode,kind,.beta) [right] environment [] :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, primitiveExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem gamma_setup (mode : Mode) (kind : Kind) (left right : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.gamma mode kind left right) environment stack)
      (.eval left environment (.draw (mode,kind,.gamma) [right] environment [] :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, primitiveExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem poisson_setup (mode : Mode) (kind : Kind) (argument : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.poisson mode kind argument) environment stack)
      (.eval argument environment (.draw (mode,kind,.poisson) [] environment [] :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, primitiveExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem bernoulli_setup (mode : Mode) (kind : Kind) (argument : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.bernoulli mode kind argument) environment stack)
      (.eval argument environment (.draw (mode,kind,.bernoulli) [] environment [] :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, primitiveExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

theorem exponential_setup (mode : Mode) (kind : Kind) (argument : Core)
    (environment : List Value) (stack : List Frame) :
    AdministrativeStep .evaluate (.eval (.exponential mode kind argument) environment stack)
      (.eval argument environment (.draw (mode,kind,.exponential) [] environment [] :: stack)) := by
  refine ⟨rfl, ?_⟩
  simp [stateExpr, stackExpr, frameExpr, primitiveExpr, close, interpret, Expr.mapLiteral, Expr.mapVars]

end Determinize.Proof.FiniteModel
