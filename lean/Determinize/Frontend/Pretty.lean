import Determinize.Frontend.Syntax

namespace Determinize.Frontend
open Spec.Paper Checking

def prettyAffinity : Affinity → String | .E => "E" | .G => "G"
def prettyType : Ty → String
  | .unit => "unit"
  | .bool => "bool"
  | .float m => s!"float[{prettyAffinity m}]"
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
def leanAction : DistributionAction → String
  | .sample affinity => s!"(.sample .{prettyAffinity affinity})"
  | .mean => ".mean"

private def primitive (name : String) (action : DistributionAction) (args : List String) : String :=
  let head := match action with
    | .sample affinity => s!"{name}[{prettyAffinity affinity}]"
    | .mean => "mean_" ++ name
  s!"{head}({String.intercalate ", " args})"

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
  | .uniform k a b => primitive "uniform" k [render env depth a, render env depth b]
  | .gaussian k a b => primitive "gauss" k [render env depth a, render env depth b]
  | .poisson k a => primitive "poisson" k [render env depth a]
  | .discrete k d =>
      primitive "discrete" k (d.probabilities.map literal)
  | .bernoulli k a => primitive "bernoulli" k [render env depth a]
  | .exponential k a => primitive "exponential" k [render env depth a]
  | .beta k a b => primitive "beta" k [render env depth a, render env depth b]
  | .gamma k a b => primitive "gamma" k [render env depth a, render env depth b]

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
  | .uniform action lower upper => s!"(.uniform {leanAction action} {leanExpression lower} {leanExpression upper})"
  | .gaussian action mean variance => s!"(.gaussian {leanAction action} {leanExpression mean} {leanExpression variance})"
  | .poisson action rate => s!"(.poisson {leanAction action} {leanExpression rate})"
  | .discrete action d =>
      let ps := String.intercalate ", " (d.probabilities.map fun p => s!"({p.num} / {p.den} : Rat)")
      s!"(.discrete {leanAction action} ⟨[{ps}], by decide +kernel, by decide +kernel⟩)"
  | .bernoulli action probability => s!"(.bernoulli {leanAction action} {leanExpression probability})"
  | .exponential action rate => s!"(.exponential {leanAction action} {leanExpression rate})"
  | .beta action alpha betaArg => s!"(.beta {leanAction action} {leanExpression alpha} {leanExpression betaArg})"
  | .gamma action shape rate => s!"(.gamma {leanAction action} {leanExpression shape} {leanExpression rate})"

private def leanRequested : Option Affinity → String
  | none => "none"
  | some affinity => s!"(some .{prettyAffinity affinity})"

def leanInput : Input → String
  | .bvar index => s!"(.bvar {index})"
  | .reject => "(.reject)"
  | .unit  => s!"(.unit)"
  | .bool value => s!"(.bool {value})"
  | .real value => s!"(.real (({value.num} : Rat) / {value.den}))"
  | .lam body => s!"(.lam {leanInput body})"
  | .fix body => s!"(.fix {leanInput body})"
  | .app fn arg => s!"(.app {leanInput fn} {leanInput arg})"
  | .pair left right => s!"(.pair {leanInput left} {leanInput right})"
  | .fst pairValue => s!"(.fst {leanInput pairValue})"
  | .snd pairValue => s!"(.snd {leanInput pairValue})"
  | .inl value => s!"(.inl {leanInput value})"
  | .inr value => s!"(.inr {leanInput value})"
  | .matchSum scrutinee left right => s!"(.matchSum {leanInput scrutinee} {leanInput left} {leanInput right})"
  | .nil  => s!"(.nil)"
  | .cons head tail => s!"(.cons {leanInput head} {leanInput tail})"
  | .matchList scrutinee nilCase consCase => s!"(.matchList {leanInput scrutinee} {leanInput nilCase} {leanInput consCase})"
  | .ite condition thenBranch elseBranch => s!"(.ite {leanInput condition} {leanInput thenBranch} {leanInput elseBranch})"
  | .letE value body => s!"(.letE {leanInput value} {leanInput body})"
  | .neg body => s!"(.neg {leanInput body})"
  | .add left right => s!"(.add {leanInput left} {leanInput right})"
  | .mul left right => s!"(.mul {leanInput left} {leanInput right})"
  | .div left right => s!"(.div {leanInput left} {leanInput right})"
  | .lt left right => s!"(.lt {leanInput left} {leanInput right})"
  | .uniform affinity lower upper => s!"(.uniform {leanRequested affinity} {leanInput lower} {leanInput upper})"
  | .gaussian affinity mean variance => s!"(.gaussian {leanRequested affinity} {leanInput mean} {leanInput variance})"
  | .poisson affinity rate => s!"(.poisson {leanRequested affinity} {leanInput rate})"
  | .discrete affinity d =>
      let ps := String.intercalate ", " (d.probabilities.map fun p => s!"({p.num} / {p.den} : Rat)")
      s!"(.discrete {leanRequested affinity} ⟨[{ps}], by decide +kernel, by decide +kernel⟩)"
  | .bernoulli affinity probability => s!"(.bernoulli {leanRequested affinity} {leanInput probability})"
  | .exponential affinity rate => s!"(.exponential {leanRequested affinity} {leanInput rate})"
  | .beta affinity alpha betaArg => s!"(.beta {leanRequested affinity} {leanInput alpha} {leanInput betaArg})"
  | .gamma affinity shape rate => s!"(.gamma {leanRequested affinity} {leanInput shape} {leanInput rate})"

end Determinize.Frontend
