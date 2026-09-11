import Determinize.Runtime.Sampling

namespace Determinize.Runtime
open Statement.Paper Checking

inductive Value where
  | unit | bool (b : Bool) | number (x : Float)
  | pair (a b : Value) | inl (v : Value) | inr (v : Value)
  | nil | cons (h t : Value)
  | closure (body : Expr Float) (env : List Value)
  | recursive (body : Expr Float) (env : List Value)
deriving Inhabited

partial def Value.display : Value → String
  | .unit => "()"
  | .bool b => toString b
  | .number x => toString x
  | .pair a b => s!"({a.display}, {b.display})"
  | .inl a => s!"inl ({a.display})"
  | .inr a => s!"inr ({a.display})"
  | .nil => "[]"
  | .cons h t => s!"{h.display} :: {t.display}"
  | .closure .. | .recursive .. => "<function>"

structure EvalState where
  fuel : Nat
  eSeed : UInt64
  gSeed : UInt64
  draws : Nat := 0
inductive EvalError where
  | rejected
  | failure (message : String)

inductive Outcome where
  | returned (value : Value) (state : EvalState)
  | rejected

abbrev EvalM := StateT EvalState (Except EvalError)

private def tick : EvalM Unit := do
  let s ← get
  if s.fuel == 0 then throw (EvalError.failure "step limit reached")
  set {s with fuel := s.fuel - 1}
private def number : Value → EvalM Float
  | .number x => pure x
  | _ => throw (EvalError.failure "expected a numeric value")
private def bool : Value → EvalM Bool
  | .bool b => pure b
  | _ => throw (EvalError.failure "expected a Boolean value")
private def checkedNumber (x : Float) : EvalM Value := do
  if x.isNaN || x.isInf then throw (EvalError.failure "nonfinite arithmetic result")
  return .number x
private def draw (op : Op) (m : Mode) (k : Kind) (args : List Float) : EvalM Value := do
  let s ← get
  let seed := if m == .G then s.gSeed else s.eSeed
  let (value, next) ← (sample op k args |>.run seed).mapError EvalError.failure
  if m == .G then set {s with gSeed := next, draws := s.draws + (if k == Kind.stochastic then 1 else 0)}
  else set {s with eSeed := next, draws := s.draws + (if k == Kind.stochastic then 1 else 0)}
  return .number value

private partial def eval (env : List Value) (e : Expr Float) : EvalM Value := do
  tick
  match e with
  | .bvar i => match env[i]? with
    | some v => pure v
    | none => throw (EvalError.failure "unbound runtime variable")
  | .reject => throw EvalError.rejected
  | .unit => return .unit
  | .bool b => return .bool b
  | .real x => checkedNumber x
  | .lam b => return .closure b env
  | .fix b => return .recursive b env
  | .app f x =>
    let f ← eval env f; let x ← eval env x
    match f with
    | .closure b saved => eval (x :: saved) b
    | .recursive b saved => eval (x :: f :: saved) b
    | _ => throw (EvalError.failure "application of nonfunction")
  | .pair a b => return .pair (← eval env a) (← eval env b)
  | .fst p => match (← eval env p) with
    | .pair a _ => pure a
    | _ => throw (EvalError.failure "fst of nonpair")
  | .snd p => match (← eval env p) with
    | .pair _ b => pure b
    | _ => throw (EvalError.failure "snd of nonpair")
  | .inl v => return .inl (← eval env v)
  | .inr v => return .inr (← eval env v)
  | .matchSum s a b => match (← eval env s) with
    | .inl v => eval (v :: env) a
    | .inr v => eval (v :: env) b
    | _ => throw (EvalError.failure "sum match of nonsum")
  | .nil => return .nil
  | .cons h t => return .cons (← eval env h) (← eval env t)
  | .matchList s n c => match (← eval env s) with
    | .nil => eval env n
    | .cons h t => eval (h :: t :: env) c
    | _ => throw (EvalError.failure "list match of nonlist")
  | .ite c a b => if (← bool (← eval env c)) then eval env a else eval env b
  | .letE v b => let v ← eval env v; eval (v :: env) b
  | .neg b => checkedNumber (-(← number (← eval env b)))
  | .add a b | .mul a b | .div a b | .lt a b =>
    let a ← number (← eval env a); let b ← number (← eval env b)
    match e with
    | .add .. => checkedNumber (a + b)
    | .mul .. => checkedNumber (a * b)
    | .div .. => checkedNumber (if b == 0 then 0 else a / b)
    | _ => return .bool (a < b)
  | .uniform m k a b | .gaussian m k a b | .beta m k a b | .gamma m k a b =>
    let a ← number (← eval env a); let b ← number (← eval env b)
    let op := match e with
      | .uniform .. => Op.uniform | .gaussian .. => .gaussian | .beta .. => .beta | _ => .gamma
    draw op m k [a,b]
  | .discrete m k d => draw (.discrete d) m k []
  | .bernoulli m k a =>
    let a ← number (← eval env a)
    draw .bernoulli m k [a]
  | .poisson m k a | .exponential m k a =>
    let a ← number (← eval env a)
    draw (match e with | .poisson .. => .poisson | _ => .exponential) m k [a]

/-- Observation rejection is separate from invalid operations and exhausted fuel. -/
def runOutcome (e : Core) (seed : UInt64 := 0) (fuel : Nat := 100000) : Except String Outcome :=
  let expression := e.mapLiteral (fun q => Float.ofInt q.num / Float.ofNat q.den)
  match (eval [] expression).run ⟨fuel, seed ^^^ 0x517cc1b727220a95, seed, 0⟩ with
  | .ok (value, state) => .ok (.returned value state)
  | .error .rejected => .ok .rejected
  | .error (.failure message) => .error message

/-- Compatibility interface for callers requiring a returned value. -/
def run (e : Core) (seed : UInt64 := 0) (fuel : Nat := 100000) : Except String (Value × EvalState) := do
  match ← runOutcome e seed fuel with
  | .returned value state => return (value, state)
  | .rejected => throw "observation rejected"

end Determinize.Runtime
