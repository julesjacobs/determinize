import Determinize.Checking.Certificate
import Determinize.Statement.FiniteModel.Supported

namespace Determinize.Finite
open Statement.Paper Checking

inductive Value where
  | unit | bool (value : Bool) | number (value : Rat)
  | pair (left right : Value) | inl (value : Value) | inr (value : Value)
  | nil | cons (head tail : Value)
  | closure (body : Core) (environment : List Value)
  | recursive (body : Core) (environment : List Value)
deriving Repr, BEq, Inhabited

inductive Unary where
  | fst | snd | inl | inr | neg
deriving Repr, BEq

inductive Binary where
  | app | pair | cons | add | mul | div | lt
deriving Repr, BEq

inductive Frame where
  | unary (operation : Unary)
  | left (operation : Binary) (right : Core) (environment : List Value)
  | right (operation : Binary) (left : Value)
  | choose (yes no : Core) (environment : List Value)
  | letBody (body : Core) (environment : List Value)
  | matchSum (left right : Core) (environment : List Value)
  | matchList (nilCase consCase : Core) (environment : List Value)
  | draw (site : Mode × Kind × Op) (pending : List Core)
      (environment : List Value) (arguments : List Rat)
deriving Repr, BEq

inductive State where
  | eval (expression : Core) (environment : List Value) (stack : List Frame)
  | deliver (value : Value) (stack : List Frame)
  | rejected
deriving Repr, BEq, Inhabited

inductive Failure where
  | invalid (message : String)
  | unsupported (message : String)
  | nonnumericResult
deriving Repr, BEq

def Failure.message : Failure → String
  | .invalid message => s!"invalid execution: {message}"
  | .unsupported message => s!"unsupported execution: {message}"
  | .nonnumericResult => "export requires a numeric terminal result"

/-- Candidate evidence for replay by the future transition checker. These are tags,
not proofs. The source state and successor row must also be checked. -/
inductive Evidence where
  | evaluate
  | continue
  | sample (site : Mode × Kind × Op) (arguments : List Rat)
  | returned
  | rejected
deriving Repr, BEq

inductive Step where
  | next (evidence : Evidence) (successors : List (Rat × State))
  | returned (reward : Rat)
  | rejected
deriving Repr

/-- Exact finite laws. Operands have already been evaluated, left to right. -/
def finiteLaw (op : Op) (kind : Kind) (arguments : List Rat) :
    Except Failure (List (Rat × Rat)) := do
  let mean ← match op, arguments with
    | .uniform, [a,b] =>
        if a ≤ b then pure ((a+b)/2) else throw (.invalid "uniform bounds")
    | .gaussian, [a,v] =>
        if 0 ≤ v then pure a else throw (.invalid "gaussian variance")
    | .poisson, [a] =>
        if 0 ≤ a then pure a else throw (.invalid "poisson rate")
    | .exponential, [a] =>
        if 0 < a then pure (1/a) else throw (.invalid "exponential rate")
    | .beta, [a,b] =>
        if 0 < a && 0 < b then pure (a/(a+b)) else throw (.invalid "beta parameters")
    | .gamma, [a,b] =>
        if 0 < a && 0 < b then pure (a/b) else throw (.invalid "gamma parameters")
    | .bernoulli, [p] =>
        if 0 ≤ p && p ≤ 1 then pure p else throw (.invalid "bernoulli probability")
    | .discrete d, [] => pure d.mean
    | _, _ => throw (.invalid "primitive arity")
  unless Statement.FiniteModel.supportedDraw kind op do
    throw (.unsupported s!"stochastic {reprStr op}")
  if kind == .mean then return [(1, mean)]
  match op, arguments with
  | .bernoulli, [p] => return [(1-p, 0), (p, 1)]
  | .discrete d, [] => return d.probabilities.zipIdx.map fun (p,i) => (p, (i : Rat))
  | _, _ => throw (.unsupported s!"stochastic {reprStr op}")

def draw (site : Mode × Kind × Op) (arguments : List Rat) (stack : List Frame) :
    Except Failure Step := do
  let outcomes ← finiteLaw site.2.2 site.2.1 arguments
  return .next (.sample site arguments) (outcomes.map fun (p,x) => (p, .deliver (.number x) stack))

def binary (op : Binary) (left right : Value) (stack : List Frame) : Except Failure State :=
  match op, left, right with
  | .app, .closure body saved, argument => .ok (.eval body (argument :: saved) stack)
  | .app, function@(.recursive body saved), argument =>
      .ok (.eval body (argument :: function :: saved) stack)
  | .pair, a, b => .ok (.deliver (.pair a b) stack)
  | .cons, a, b => .ok (.deliver (.cons a b) stack)
  | .add, .number a, .number b => .ok (.deliver (.number (a+b)) stack)
  | .mul, .number a, .number b => .ok (.deliver (.number (a*b)) stack)
  | .div, .number a, .number b => .ok (.deliver (.number (a/b)) stack)
  | .lt, .number a, .number b => .ok (.deliver (.bool (a<b)) stack)
  | _, _, _ => .error (.invalid "binary operand types")

def unary (op : Unary) (value : Value) : Except Failure Value :=
  match op, value with
  | .fst, .pair a _ => .ok a
  | .snd, .pair _ b => .ok b
  | .inl, v => .ok (.inl v)
  | .inr, v => .ok (.inr v)
  | .neg, .number x => .ok (.number (-x))
  | _, _ => .error (.invalid "unary operand type")

/-- One CEK transition; no Float arithmetic or numerical sampling is used. -/
def step : State → Except Failure Step
  | .rejected => .ok .rejected
  | .eval expression environment stack => do
      let state ← match expression with
      | .bvar index => match environment[index]? with
          | some value => pure (.deliver value stack)
          | none => throw (.invalid "unbound variable")
      | .reject => pure .rejected
      | .unit => pure (.deliver .unit stack)
      | .bool b => pure (.deliver (.bool b) stack)
      | .real x => pure (.deliver (.number x) stack)
      | .nil => pure (.deliver .nil stack)
      | .lam body => pure (.deliver (.closure body environment) stack)
      | .fix body => pure (.deliver (.recursive body environment) stack)
      | .app a b | .pair a b | .cons a b | .add a b | .mul a b | .div a b | .lt a b =>
          let op := match expression with
            | .app .. => Binary.app | .pair .. => .pair | .cons .. => .cons
            | .add .. => .add | .mul .. => .mul | .div .. => .div | _ => .lt
          pure (.eval a environment (.left op b environment :: stack))
      | .fst x | .snd x | .inl x | .inr x | .neg x =>
          let op := match expression with
            | .fst .. => Unary.fst | .snd .. => .snd | .inl .. => .inl
            | .inr .. => .inr | _ => .neg
          pure (.eval x environment (.unary op :: stack))
      | .ite condition yes no => pure (.eval condition environment (.choose yes no environment :: stack))
      | .letE value body => pure (.eval value environment (.letBody body environment :: stack))
      | .matchSum value left right => pure (.eval value environment (.matchSum left right environment :: stack))
      | .matchList value nilCase consCase =>
          pure (.eval value environment (.matchList nilCase consCase environment :: stack))
      | .uniform m k a b | .gaussian m k a b | .beta m k a b | .gamma m k a b =>
          let op := match expression with
            | .uniform .. => Op.uniform | .gaussian .. => .gaussian | .beta .. => .beta | _ => .gamma
          pure (.eval a environment (.draw (m,k,op) [b] environment [] :: stack))
      | .poisson m k a | .exponential m k a | .bernoulli m k a =>
          let op := match expression with
            | .poisson .. => Op.poisson | .exponential .. => .exponential | _ => .bernoulli
          pure (.eval a environment (.draw (m,k,op) [] environment [] :: stack))
      | .discrete m k d => return ← draw (m,k,.discrete d) [] stack
      return .next .evaluate [(1,state)]
  | .deliver value [] => match value with
      | .number reward => .ok (.returned reward)
      | _ => .error .nonnumericResult
  | .deliver value (frame :: stack) => do
      let state ← match frame with
      | .unary op => pure (.deliver (← unary op value) stack)
      | .left op right saved => pure (.eval right saved (.right op value :: stack))
      | .right op left => binary op left value stack
      | .letBody body saved => pure (.eval body (value :: saved) stack)
      | .choose yes no saved => match value with
          | .bool b => pure (.eval (if b then yes else no) saved stack)
          | _ => throw (.invalid "non-Boolean condition")
      | .matchSum left right saved => match value with
          | .inl v => pure (.eval left (v :: saved) stack)
          | .inr v => pure (.eval right (v :: saved) stack)
          | _ => throw (.invalid "sum match operand")
      | .matchList nilCase consCase saved => match value with
          | .nil => pure (.eval nilCase saved stack)
          | .cons head tail => pure (.eval consCase (head :: tail :: saved) stack)
          | _ => throw (.invalid "list match operand")
      | .draw site pending saved arguments => match value with
          | .number x => match pending with
              | [] => return ← draw site (arguments ++ [x]) stack
              | next :: rest => pure (.eval next saved (.draw site rest saved (arguments ++ [x]) :: stack))
          | _ => throw (.invalid "nonnumeric primitive parameter")
      return .next .continue [(1,state)]

end Determinize.Finite
