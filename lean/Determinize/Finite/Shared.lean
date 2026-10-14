import Determinize.Proof.FiniteModel.Quote

namespace Determinize.Finite.Shared
open Lean

structure Definition where
  name : Name
  type : Lean.Expr
  value : Lean.Expr

structure Builder where
  namespacePrefix : Name := `certificateData
  definitions : Array Definition := #[]
  known : ExprMap Lean.Expr := {}
  quoted : ExprMap Lean.Expr := {}

private def sharedType? (e : Lean.Expr) : Option Lean.Expr := do
  let .const name _ := e.getAppFn | none
  let args := e.getAppArgs
  if name.getPrefix == ``Spec.Paper.Expr && args.size ≥ 2 then
    return mkApp2 (mkConst ``Spec.Paper.Expr) args[0]! args[1]!
  if name.getPrefix == ``Value then return mkConst ``Value
  if name.getPrefix == ``Frame then return mkConst ``Frame
  if name.getPrefix == ``State then return mkConst ``State
  if name == ``List.cons || name == ``List.nil then
    let some type := args[0]? | none
    if type == mkConst ``Value || type == mkConst ``Frame ||
        type == toTypeExpr Spec.Paper.Core || type == mkConst ``Rat then
      return mkApp (mkConst ``List [0]) type
  none

private partial def share (e : Lean.Expr) : StateM Builder Lean.Expr := do
  if let some result := (← get).quoted[e]? then return result
  let args ← e.getAppArgs.mapM share
  let body := mkAppN e.getAppFn args
  let some type := sharedType? body | return body
  if let some known := (← get).known[body]? then
    modify fun s ↦ {s with quoted := s.quoted.insert e known}
    return known
  let s ← get
  let name := s.namespacePrefix.str s!"d{s.definitions.size}"
  let result := mkConst name
  set {s with
    definitions := s.definitions.push ⟨name, type, body⟩,
    known := s.known.insert body result,
    quoted := s.quoted.insert e result}
  return result

def quote {α : Type} [ToExpr α] (value : α) : StateM Builder Lean.Expr :=
  share (toExpr value)

def array (type : Lean.Expr) (values : Array Lean.Expr) : Lean.Expr :=
  let nil := mkApp (mkConst ``List.nil [0]) type
  let cons := mkApp (mkConst ``List.cons [0]) type
  let list := values.foldr (fun value rest ↦ mkApp2 cons value rest) nil
  mkApp2 (mkConst ``List.toArray [0]) type list

def rewardCandidate (candidate : Reward.Candidate) : StateM Builder Lean.Expr := do
  let states ← candidate.states.mapM quote
  let rows ← candidate.rows.mapM quote
  return mkApp3 (mkConst ``Reward.Candidate.mk) (toExpr candidate.initial)
    (array (toTypeExpr State) states) (array (toTypeExpr Reward.Row) rows)

def candidate (candidate : Candidate) : StateM Builder Lean.Expr := do
  let states ← candidate.states.mapM quote
  let rows ← candidate.rows.mapM quote
  return mkApp3 (mkConst ``Candidate.mk) (toExpr candidate.initial)
    (array (toTypeExpr State) states) (array (toTypeExpr Row) rows)

private partial def exprText (e : Lean.Expr) : String := Id.run do
  if e.isAppOfArity ``OfNat.ofNat 3 then
    let args := e.getAppArgs
    if args[0]! == mkConst ``Nat then
      if let some n := args[1]!.rawNatLit? then return toString n
  if e.isAppOfArity ``List.toArray 2 then
    let mut list := e.appArg!
    let mut entries := #[]
    while list.isAppOfArity ``List.cons 3 do
      let args := list.getAppArgs
      entries := entries.push (exprText args[1]!)
      list := args[2]!
    if list.isAppOfArity ``List.nil 1 then
      return "#[" ++ String.intercalate ", " entries.toList ++ "]"
  match e with
  | .const name _ => return s!"@{name}"
  | .lit (.natVal n) => return toString n
  | .app .. =>
    return "(" ++ exprText e.getAppFn ++ " " ++
      String.intercalate " " (e.getAppArgs.toList.map exprText) ++ ")"
  | _ => panic! "unexpected expression in shared certificate data"

def Builder.text (s : Builder) (value : Lean.Expr) : String :=
  let args := value.getAppArgs
  let type := value.getAppFn.constName!.getPrefix
  String.join (s.definitions.toList.map fun d ↦
    s!"def {d.name} : {exprText d.type} := {exprText d.value}\n") ++
  s!"\ndef candidate : {type} where\n  initial := {exprText args[0]!}\n" ++
  s!"  states := {exprText args[1]!}\n  rows := {exprText args[2]!}\n"

end Determinize.Finite.Shared
