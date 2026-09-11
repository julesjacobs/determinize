import Determinize.Frontend.Syntax

namespace Determinize.Frontend
open Spec.Paper

private def identStart (c : Char) := c.isAlpha || c == '_'
private def identRest (c : Char) := identStart c || c.isDigit || c == '\''

private partial def comment (cs : List Char) (depth : Nat) : Except String (List Char) :=
  match cs with
  | [] => .error "unterminated comment"
  | '(' :: '*' :: rest => comment rest (depth + 1)
  | '*' :: ')' :: rest => if depth == 1 then .ok rest else comment rest (depth - 1)
  | _ :: rest => comment rest depth

private partial def tokenize (cs : List Char) : Except String (List String) := do
  match cs with
  | [] => return []
  | '(' :: '*' :: rest => tokenize (← comment rest 1)
  | c :: rest =>
    if c.isWhitespace then return ← tokenize rest
    if identStart c then
      let (word, tail) := rest.span identRest
      return String.ofList (c :: word) :: (← tokenize tail)
    if c.isDigit then
      let (whole, tail) := cs.span Char.isDigit
      let (digits, tail) := match tail with
        | '.' :: tail => let (frac, tail) := tail.span Char.isDigit; (whole ++ '.' :: frac, tail)
        | _ => (whole, tail)
      let (digits, tail) ← match tail with
        | e :: tail =>
          if e == 'e' || e == 'E' then do
            let (sign, tail) := match tail with
              | '+' :: t => (['+'], t)
              | '-' :: t => (['-'], t)
              | _ => ([], tail)
            let (exp, tail) := tail.span Char.isDigit
            if exp.isEmpty then throw "missing decimal exponent"
            pure (digits ++ e :: sign ++ exp, tail)
          else pure (digits, e :: tail)
        | [] => pure (digits, [])
      return String.ofList digits :: (← tokenize tail)
    match cs with
    | '=' :: '>' :: tail => return "=>" :: (← tokenize tail)
    | ':' :: ':' :: tail => return "::" :: (← tokenize tail)
    | '<' :: '=' :: tail => return "<=" :: (← tokenize tail)
    | _ =>
      if "()[],|=+-*/<>\\".toList.contains c then
        return String.singleton c :: (← tokenize rest)
      else throw s!"unexpected character {c}"

private def decimal (s : String) : Except String Rat := do
  let parts := s.toLower.splitOn "e"
  let mantissa := parts.head!
  let exp ← match parts with
    | [_] => pure (0 : Int)
    | [_, e] => match (if e.startsWith "+" then e.drop 1 |>.toString else e).toInt? with
        | some n => pure n
        | none => throw "invalid decimal exponent"
    | _ => throw "invalid decimal"
  if exp.natAbs > 10000 then throw "decimal exponent exceeds 10000"
  let pieces := mantissa.splitOn "."
  let frac := (pieces[1]?).getD ""
  let some n := ((pieces.head!) ++ frac).toNat? | throw "invalid decimal"
  let q : Rat := (n : Rat) / (10 ^ frac.length : Nat)
  return if exp ≥ 0 then q * (10 ^ exp.toNat : Nat) else q / (10 ^ exp.natAbs : Nat)

private structure ParserState where
  tokens : Array String
  pos : Nat := 0
private abbrev P := StateT ParserState (Except String)
private def peek : P String := do return ((← get).tokens[(← get).pos]?).getD "<end>"
private def take : P String := do
  let t ← peek
  modify fun s => {s with pos := s.pos + 1}
  return t
private def expect (t : String) : P Unit := do
  let got ← take
  unless got == t do throw s!"expected '{t}', got '{got}' at token {(← get).pos}"
private def name : P String := do
  let t ← take
  unless t.toList.head?.any identStart do throw s!"expected a name, got '{t}'"
  return t
private def node (tag : String) (args : List Surface) : Surface := .node tag [] none args
private def primitives := ["uniform", "gauss", "gaussian", "poisson", "exponential", "gamma", "beta", "flip", "bernoulli", "discrete", "observe"]
private def startsAtom (t : String) : Bool :=
  t == "(" || t == "[" ||
  (t.toList.head?.any (fun c => identStart c || c.isDigit) &&
    !(["in", "then", "else", "with", "let", "if", "match", "fun", "rec"].contains t))

private partial def expr (minPrec : Nat := 0) : P Surface := do
  let t ← take
  let mut left ← match t with
    | "let" => do
      let x ← name; expect "="; let a ← expr; expect "in"
      pure (.node "let" [x] none [a, ← expr])
    | "fun" | "lambda" | "\\" => do
      let x ← name; expect "=>"; pure (.node "lam" [x] none [← expr])
    | "rec" => do
      let f ← name; let x ← name; expect "=>"; pure (.node "fix" [f,x] none [← expr])
    | "if" => do
      let c ← expr; expect "then"; let a ← expr; expect "else"
      pure (node "ite" [c,a,← expr])
    | "match" => do
      let e ← expr; expect "with"
      if (← peek) == "|" then discard take
      if (← peek) == "[" then
        expect "["; expect "]"; expect "=>"; let n ← expr
        expect "|"; let x ← name; expect "::"; let xs ← name; expect "=>"
        pure (.node "matchList" [x,xs] none [e,n,← expr])
      else
        expect "inl"; let x ← name; expect "=>"; let a ← expr
        expect "|"; expect "inr"; let y ← name; expect "=>"
        pure (.node "matchSum" [x,y] none [e,a,← expr])
    | "(" => do
      if (← peek) == ")" then expect ")"; pure (node "unit" [])
      else
        let a ← expr
        if (← peek) == "," then
          expect ","; let b ← expr; expect ")"; pure (node "pair" [a,b])
        else expect ")"; pure a
    | "[" => do expect "]"; pure (node "nil" [])
    | "-" => do pure (node "neg" [← expr 70])
    | "fst" | "snd" | "inl" | "inr" => do pure (node t [← expr 81])
    | "true" | "false" => pure (node t [])
    | _ => do
      if primitives.contains t then
        let mut affinity := none
        if (← peek) == "[" then
          expect "["
          affinity ← match (← take) with
            | "E" => pure (some Affinity.E)
            | "G" => pure (some Affinity.G)
            | x => throw s!"expected E or G, got {x}"
          expect "]"
        expect "("
        let mut args := []
        if (← peek) != ")" then
          args := [← expr]
          while (← peek) == "," do expect ","; args := args ++ [← expr]
        expect ")"
        pure (.node t [] affinity args)
      else if t.toList.head?.any Char.isDigit then
        pure (.number (← decimal t))
      else if t.toList.head?.any identStart then pure (.var t)
      else throw s!"expected expression, got '{t}'"
  repeat
    let op ← peek
    let infixInfo := match op with
      | "<" | "<=" => some (30, 31)
      | "::" => some (40, 40)
      | "+" | "-" => some (50, 51)
      | "*" | "/" => some (60, 61)
      | _ => none
    match infixInfo with
    | some (l,r) =>
      if l < minPrec then break
      discard take
      left := node op [left, ← expr r]
    | none =>
      if minPrec ≤ 80 && startsAtom op then
        left := node "app" [left, ← expr 81]
      else break
  return left

def parse (text : String) : Except String Surface := do
  let tokens ← tokenize text.toList
  let (result, state) ← (expr 0).run { tokens := tokens.toArray }
  unless state.pos == tokens.length do
    throw s!"unexpected trailing token {(state.tokens[state.pos]?).getD "<end>"}"
  return result

end Determinize.Frontend
