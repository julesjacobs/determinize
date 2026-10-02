import Determinize.Finite.Machine
import Determinize.Proof.FiniteModel.Substitution
import Determinize.Spec.FiniteModel.Certificates

namespace Determinize.Proof.FiniteModel
open Spec.Paper Determinize.Finite Checking Binding

noncomputable section

mutual
def valueExpr : Value → Expr
  | .unit => .unit
  | .bool b => .bool b
  | .number q => .real (q : ℝ)
  | .pair a b => .pair (valueExpr a) (valueExpr b)
  | .inl a => .inl (valueExpr a)
  | .inr a => .inr (valueExpr a)
  | .nil => .nil
  | .cons a b => .cons (valueExpr a) (valueExpr b)
  | .closure body environment => .lam (close (environmentExpr environment) 1 (interpret body))
  | .recursive body environment => .fix (close (environmentExpr environment) 2 (interpret body))
termination_by value => sizeOf value

def environmentExpr : List Value → List Expr
  | [] => []
  | value :: rest => valueExpr value :: environmentExpr rest
termination_by environment => sizeOf environment
end

mutual
theorem scoped_valueExpr (value : Value) : Scoped 0 (valueExpr value) := by
  cases value with
  | unit | bool _ | number _ | nil => simp [valueExpr, Scoped]
  | pair a b | cons a b =>
    simpa only [valueExpr, Scoped] using And.intro (scoped_valueExpr a) (scoped_valueExpr b)
  | inl a | inr a => simpa only [valueExpr, Scoped] using scoped_valueExpr a
  | closure body environment | recursive body environment =>
    simpa only [valueExpr, Scoped, Nat.zero_add]
      using close_scoped (interpret body) (environmentExpr environment)
      (scoped_environmentExpr environment) _
termination_by sizeOf value

theorem scoped_environmentExpr (environment : List Value) :
    ∀ e ∈ environmentExpr environment, Scoped 0 e := by
  cases environment with
  | nil => simp [environmentExpr]
  | cons value rest =>
    intro e member
    simp only [environmentExpr, List.mem_cons] at member
    rcases member with rfl | member
    · exact scoped_valueExpr value
    · exact scoped_environmentExpr rest e member
termination_by sizeOf environment
end

theorem isValue_valueExpr (value : Value) : (valueExpr value).isValue = true := by
  cases value with
  | unit | bool _ | number _ | nil | closure _ _ | recursive _ _ => simp [valueExpr, Expr.isValue]
  | pair a b | cons a b =>
    simp only [valueExpr, Expr.isValue, isValue_valueExpr a, isValue_valueExpr b, Bool.and_self]
  | inl a | inr a => simpa only [valueExpr, Expr.isValue] using isValue_valueExpr a
termination_by sizeOf value

def unaryExpr {α : Type} : Unary → Expr α → Expr α
  | .fst => .fst | .snd => .snd | .inl => .inl | .inr => .inr | .neg => .neg

def binaryExpr {α : Type} : Binary → Expr α → Expr α → Expr α
  | .app => .app | .pair => .pair | .cons => .cons
  | .add => .add | .mul => .mul | .div => .div | .lt => .lt

def primitiveExpr {α : Type} (site : DistributionAction × Op) (arguments : List (Expr α)) :
    Expr α :=
  match site.2, arguments with
  | .uniform, [a, b] => .uniform site.1 a b
  | .gaussian, [a, b] => .gaussian site.1 a b
  | .poisson, [a] => .poisson site.1 a
  | .bernoulli, [a] => .bernoulli site.1 a
  | .exponential, [a] => .exponential site.1 a
  | .beta, [a, b] => .beta site.1 a b
  | .gamma, [a, b] => .gamma site.1 a b
  | .discrete _, probabilities => .discrete site.1 (probabilities.foldr Expr.cons .nil)
  | _, _ => .reject

def frameExpr (frame : Frame) (hole : Expr) : Expr :=
  match frame with
  | .unary op => unaryExpr op hole
  | .left op right environment =>
    binaryExpr op hole (close (environmentExpr environment) 0 (interpret right))
  | .right op left => binaryExpr op (valueExpr left) hole
  | .choose yes no environment =>
    .ite hole (close (environmentExpr environment) 0 (interpret yes))
      (close (environmentExpr environment) 0 (interpret no))
  | .letBody body environment => .letE hole (close (environmentExpr environment) 1 (interpret body))
  | .matchSum left right environment =>
    .matchSum hole (close (environmentExpr environment) 1 (interpret left))
      (close (environmentExpr environment) 1 (interpret right))
  | .matchList nilCase consCase environment =>
    .matchList hole (close (environmentExpr environment) 0 (interpret nilCase))
      (close (environmentExpr environment) 2 (interpret consCase))
  | .discrete action => .discrete action hole
  | .draw site pending environment arguments =>
    primitiveExpr site (arguments.map (fun q ↦ .real (q : ℝ)) ++
      hole :: pending.map (fun e ↦ close (environmentExpr environment) 0 (interpret e)))

def stackExpr (stack : List Frame) (hole : Expr) : Expr :=
  stack.foldl (fun expression frame ↦ frameExpr frame expression) hole

def stateExpr : State → Expr
  | .eval expression environment stack =>
    stackExpr stack (close (environmentExpr environment) 0 (interpret expression))
  | .deliver value stack => stackExpr stack (valueExpr value)
  | .rejected => .reject

theorem substHead_close (body : Core) (environment : List Value) (argument : Value) :
    (close (environmentExpr environment) 1 (interpret body)).substHead (valueExpr argument) =
      close (environmentExpr (argument :: environment)) 0 (interpret body) :=
  by
    simpa only [environmentExpr, Expr.substHead] using
      close_cons_subst (interpret body) (environmentExpr environment)
        (scoped_environmentExpr environment)
        (valueExpr argument) (scoped_valueExpr argument) 0

theorem substTwo_close (body : Core) (environment : List Value) (argument : Value) :
    (close (environmentExpr environment) 2 (interpret body)).substTwo
        (valueExpr argument) (valueExpr (.recursive body environment)) =
      close (environmentExpr (argument :: .recursive body environment :: environment)) 0
          (interpret body) :=
  by
    simpa only [environmentExpr] using
      close_two_subst (interpret body) (environmentExpr environment)
        (scoped_environmentExpr environment)
        (valueExpr argument) (valueExpr (.recursive body environment))
        (scoped_valueExpr argument) (scoped_valueExpr (.recursive body environment))

theorem reduce_app_closure (body : Core) (environment : List Value) (argument : Value) :
    reduce (.app (valueExpr (.closure body environment)) (valueExpr argument)) =
      .next (stateExpr (.eval body (argument :: environment) [])) := by
  simp [valueExpr, reduce, Expr.isValue, isValue_valueExpr, substHead_close,
    stateExpr, stackExpr]

theorem reduce_app_recursive (body : Core) (environment : List Value) (argument : Value) :
    reduce (.app (valueExpr (.recursive body environment)) (valueExpr argument)) =
      .next (stateExpr
        (.eval body (argument :: .recursive body environment :: environment) [])) := by
  simp only [valueExpr, reduce, Expr.isValue, isValue_valueExpr, ↓reduceIte]
  simpa only [stateExpr, stackExpr, List.foldl_nil, valueExpr] using
    congrArg Action.next (substTwo_close body environment argument)

end
end Determinize.Proof.FiniteModel
