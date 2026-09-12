import Determinize.Frontend.Compile
import Determinize.Frontend.Pretty

namespace Determinize.Tests
open Frontend Checking Spec.Paper

def assert (b : Bool) (message : String) : IO Unit :=
  unless b do throw (IO.userError message)

def parsing : IO Unit := do
  let input ← IO.ofExcept (elaborate (← IO.ofExcept (parse "(* outer (* inner *) *) 1.25e-2")))
  assert (input.expression == .real (1/80)) "decimal literals must remain exact"
  assert (parse "uniform[E](0,1)" |>.isOk) "explicit E affinity"
  assert (parse "uniform[Q](0,1)" |> fun r => !r.isOk) "invalid affinity accepted"
  assert (parse "1 garbage )" |> fun r => !r.isOk) "trailing input accepted"
  assert (parse "(* unfinished" |> fun r => !r.isOk) "unterminated comment accepted"
  let coin ← IO.ofExcept (compile "bernoulli[E](0.25)")
  assert (coin.checked.source == .bernoulli (.sample .E) (.real (1/4)))
    "elaboration replaced a Bernoulli source draw"
  assert (coin.checked.source.determinize == .bernoulli .mean (.real (1/4)))
    "Bernoulli determinization did not retain its parameter"
  let categorical ← IO.ofExcept (compile "discrete[E](0.25,0.25,0.5)")
  match categorical.checked.source with
  | .discrete (.sample .E) d =>
      assert (d.probabilities == [1/4, 1/4, 1/2]) "discrete weights changed"
      assert (categorical.checked.source.determinize == .discrete .mean d)
        "discrete determinization changed its weights"
  | _ => throw (IO.userError "elaboration replaced a discrete source draw")
  let program ← IO.ofExcept (compile "let x = 2 in let y = 3 in x <= y")
  assert (program.checked.ty == .bool) "comparison type"
  let recursive ← IO.ofExcept (elaborate (← IO.ofExcept (parse "rec f x => f x")))
  assert (recursive.expression == .fix (.app (.bvar 1) (.bvar 0))) "recursive binder indices"
  for text in ["let x = uniform(0,1) in x + x", "fun x => x", "uniform[G](0,1)", "observe(false)", "observe(true)", "bernoulli[E](0.25)", "bernoulli[G](0.25)", "flip(0.25)", "bernoulli[E](0.00125)", "uniform[E](0.2,0.375)", "discrete[E](0.25,0.25,0.5)", "discrete[G](0,0.25,0.75)", "discrete[E](0,1,0)"] do
    let p ← IO.ofExcept (compile text)
    let q ← IO.ofExcept (compile (pretty p.checked.source))
    assert (eraseAnnotations p.checked.source == eraseAnnotations q.checked.source)
      s!"pretty-printed source changed program: {text} -> {pretty p.checked.source}"

  for text in ["2 * 3", "uniform[E](0,1) * 3", "(2 * 3) * (uniform[G](0,1) * 4)"] do
    let mut p ← IO.ofExcept (compile text)
    for _ in [:3] do
      let q ← IO.ofExcept (compile (pretty p.checked.source))
      assert (p.checked.source == q.checked.source)
        s!"multiplication roundtrip changed program: {text}"
      p := q

end Determinize.Tests
