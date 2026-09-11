import Determinize.Frontend.Syntax

namespace Determinize.Frontend
open Statement.Paper Checking

def prettyMode : Mode → String | .E => "E" | .G => "G"
def prettyType : Ty → String
  | .unit => "unit"
  | .bool => "bool"
  | .float m => s!"float[{prettyMode m}]"
  | .prod a b => s!"({prettyType a} * {prettyType b})"
  | .sum a b => s!"({prettyType a} + {prettyType b})"
  | .list a => s!"[{prettyType a}]"
  | .arr a b => s!"({prettyType a} -> {prettyType b})"
private partial def decimalPlaces (denominator : Nat) : Option Nat :=
  if denominator == 1 then some 0
  else
    let factor := denominator.gcd 10
    if factor ≤ 1 then none else (· + 1) <$> decimalPlaces (denominator / factor)

private def literal (q : Rat) : String :=
  if q.den == 1 then toString q.num
  else match decimalPlaces q.den with
  | none => s!"({q.num} / {q.den})"
  | some places =>
      let scale := 10 ^ places
      let numerator := q.num.natAbs * (scale / q.den)
      let fraction := toString (numerator % scale)
      let padding := String.ofList (List.replicate (places - fraction.length) '0')
      s!"{if q.num < 0 then "-" else ""}{numerator / scale}.{padding}{fraction}"
private def primitive (name : String) (m : Mode) (k : Kind) (args : List String) : String :=
  let name := if k == .mean then "mean_" ++ name else name
  s!"{name}[{prettyMode m}]({String.intercalate ", " args})"

private def render (env : List String) (depth : Nat) : Core → String
  | .bvar i => (env[i]?).getD s!"unbound_{i}"
  | .reject => "observe(false)"
  | .unit => "()"
  | .bool b => toString b
  | .real q => literal q
  | .lam b => let x := s!"x{depth}"; s!"(fun {x} => {render (x :: env) (depth + 1) b})"
  | .fix b => let f := s!"f{depth}"; let x := s!"x{depth}";
      s!"(rec {f} {x} => {render (x :: f :: env) (depth + 1) b})"
  | .app a b => s!"({render env depth a} {render env depth b})"
  | .pair a b => s!"({render env depth a}, {render env depth b})"
  | .fst a => s!"(fst {render env depth a})"
  | .snd a => s!"(snd {render env depth a})"
  | .inl a => s!"(inl {render env depth a})"
  | .inr a => s!"(inr {render env depth a})"
  | .matchSum e a b => let x := s!"x{depth}";
      s!"(match {render env depth e} with inl {x} => {render (x :: env) (depth + 1) a} | inr {x} => {render (x :: env) (depth + 1) b})"
  | .nil => "[]"
  | .cons h t => s!"({render env depth h} :: {render env depth t})"
  | .matchList e n c => let x := s!"x{depth}"; let xs := s!"xs{depth}";
      s!"(match {render env depth e} with [] => {render env depth n} | {x} :: {xs} => {render (x :: xs :: env) (depth + 1) c})"
  | .ite c .unit .reject => s!"observe({render env depth c})"
  | .ite c a b => s!"(if {render env depth c} then {render env depth a} else {render env depth b})"
  | .letE a b => let x := s!"x{depth}";
      s!"(let {x} = {render env depth a} in {render (x :: env) (depth + 1) b})"
  | .neg a => s!"(-{render env depth a})"
  | .add a b => s!"({render env depth a} + {render env depth b})"
  | .mul a b => s!"({render env depth a} * {render env depth b})"
  | .div a b => s!"({render env depth a} / {render env depth b})"
  | .lt a b => s!"({render env depth a} < {render env depth b})"
  | .uniform m k a b => primitive "uniform" m k [render env depth a, render env depth b]
  | .gaussian m k a b => primitive "gauss" m k [render env depth a, render env depth b]
  | .poisson m k a => primitive "poisson" m k [render env depth a]
  | .discrete m k d =>
      primitive "discrete" m k (d.probabilities.map literal)
  | .bernoulli m k a => primitive "bernoulli" m k [render env depth a]
  | .exponential m k a => primitive "exponential" m k [render env depth a]
  | .beta m k a b => primitive "beta" m k [render env depth a, render env depth b]
  | .gamma m k a b => primitive "gamma" m k [render env depth a, render env depth b]

def pretty (e : Core) : String := render [] 0 e

/-- Constructor syntax for independent kernel checking; every rational is parenthesized. -/
def leanExpression : Core → String
  | .bvar index => s!"(.bvar {index})"
  | .reject => "(.reject)"
  | .unit  => s!"(.unit)"
  | .bool value => s!"(.bool {value})"
  | .real value => s!"(.real (({value.num} : Rat) / {value.den}))"
  | .lam body => s!"(.lam {leanExpression body})"
  | .fix body => s!"(.fix {leanExpression body})"
  | .app fn arg => s!"(.app {leanExpression fn} {leanExpression arg})"
  | .pair left right => s!"(.pair {leanExpression left} {leanExpression right})"
  | .fst pairValue => s!"(.fst {leanExpression pairValue})"
  | .snd pairValue => s!"(.snd {leanExpression pairValue})"
  | .inl value => s!"(.inl {leanExpression value})"
  | .inr value => s!"(.inr {leanExpression value})"
  | .matchSum scrutinee left right => s!"(.matchSum {leanExpression scrutinee} {leanExpression left} {leanExpression right})"
  | .nil  => s!"(.nil)"
  | .cons head tail => s!"(.cons {leanExpression head} {leanExpression tail})"
  | .matchList scrutinee nilCase consCase => s!"(.matchList {leanExpression scrutinee} {leanExpression nilCase} {leanExpression consCase})"
  | .ite condition thenBranch elseBranch => s!"(.ite {leanExpression condition} {leanExpression thenBranch} {leanExpression elseBranch})"
  | .letE value body => s!"(.letE {leanExpression value} {leanExpression body})"
  | .neg body => s!"(.neg {leanExpression body})"
  | .add left right => s!"(.add {leanExpression left} {leanExpression right})"
  | .mul left right => s!"(.mul {leanExpression left} {leanExpression right})"
  | .div left right => s!"(.div {leanExpression left} {leanExpression right})"
  | .lt left right => s!"(.lt {leanExpression left} {leanExpression right})"
  | .uniform mode kind lower upper => s!"(.uniform .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} {leanExpression lower} {leanExpression upper})"
  | .gaussian mode kind mean variance => s!"(.gaussian .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} {leanExpression mean} {leanExpression variance})"
  | .poisson mode kind rate => s!"(.poisson .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} {leanExpression rate})"
  | .discrete mode kind d =>
      let ps := String.intercalate ", " (d.probabilities.map fun p => s!"({p.num} / {p.den} : Rat)")
      s!"(.discrete .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} ⟨[{ps}], by decide +kernel, by decide +kernel⟩)"
  | .bernoulli mode kind probability => s!"(.bernoulli .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} {leanExpression probability})"
  | .exponential mode kind rate => s!"(.exponential .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} {leanExpression rate})"
  | .beta mode kind alpha betaArg => s!"(.beta .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} {leanExpression alpha} {leanExpression betaArg})"
  | .gamma mode kind shape rate => s!"(.gamma .{prettyMode mode} {if kind == .mean then ".mean" else ".stochastic"} {leanExpression shape} {leanExpression rate})"

end Determinize.Frontend
