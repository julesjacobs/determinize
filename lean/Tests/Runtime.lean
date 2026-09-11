import Tests.Parsing
import Determinize.Runtime.Eval

namespace Determinize.Tests
open Frontend Checking Statement.Paper

private def value (text : String) (target := true) : IO String := do
  let p ← IO.ofExcept (compile text)
  let e := if target then p.checked.source.determinize else p.checked.source
  let (v, _) ← IO.ofExcept (Runtime.run e 42 10000)
  return v.display

def runtime : IO Unit := do
  for (text, expected) in [
      ("uniform[E](0,2)", "1.000000"),
      ("gauss[E](3,2)", "3.000000"),
      ("poisson[E](4)", "4.000000"),
      ("bernoulli[E](0.3)", "0.300000"),
      ("exponential[E](2)", "0.500000"),
      ("gamma[E](6,2)", "3.000000"),
      ("beta[E](1,3)", "0.250000"),
      ("(fun x => x + 1) 3", "4.000000"),
      ("let fact = rec f n => if n < 1 then 1 else n * f (n-1) in fact 5", "120.000000"),
      ("match 1::[] with [] => 0 | x::xs => x", "1.000000"),
      ("match inr 7 with inl x => 0 | inr y => y", "7.000000"),
      ("let x = 2 in let y = 3 in x <= y", "true"),
      ("1 / 0", "0.000000")] do
    let actual ← value text
    assert (actual == expected) s!"{text}: expected {expected}, got {actual}"
  for text in ["uniform(0,1)", "gauss(0,1)", "poisson(3)", "exponential(2)",
      "gamma(2,3)", "beta(2,3)", "flip(0.4)", "bernoulli(0.6)", "discrete(0.25,0.25,0.5)"] do
    let a ← value text false; let b ← value text false
    assert (a == b) s!"seed replay changed: {text}"
  for text in ["uniform(2,1)", "gauss(1,-1)", "poisson(-1)", "beta(0,1)",
      "exponential(0)", "gamma(1,0)", "flip(2)", "observe(false)",
      "(rec f x => f x) ()"] do
    let p ← IO.ofExcept (compile text)
    assert (Runtime.run p.checked.source 0 100 |> fun r => !r.isOk) s!"bad run returned a value: {text}"
  let p ← IO.ofExcept (compile "gauss[E](1,uniform[G](1,2))")
  let (_, stats) ← IO.ofExcept (Runtime.run p.checked.source.determinize)
  assert (stats.draws == 1) "atomic mean skipped a sampled variance operand"

  let rejected ← IO.ofExcept (compile "let _ = observe(false) in 42")
  for e in [rejected.checked.source, rejected.checked.source.determinize] do
    match ← IO.ofExcept (Runtime.runOutcome e 42 10) with
    | .rejected => pure ()
    | .returned .. => throw (IO.userError "failed observation returned a value")
  let divergent ← IO.ofExcept (compile "(rec f x => f x) ()")
  assert (!(Runtime.runOutcome divergent.checked.source 42 10).isOk)
    "ordinary divergence was classified as observation rejection"
  let condition ← IO.ofExcept (compile "observe(uniform[G](0,1) < 2)")
  let (_, state) ← IO.ofExcept (Runtime.run condition.checked.source)
  assert (state.draws == 1) "observation condition was not evaluated once"

end Determinize.Tests
