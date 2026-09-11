import Tests.Parsing

namespace Determinize.Tests
open Frontend Checking Spec.Paper

def inference : IO Unit := do
  let p ← IO.ofExcept (compile "uniform(0,1) + gauss(2,1)")
  assert (sampleAffinities p.checked.source == [.E,.E]) "unconstrained draws should use E"
  let p ← IO.ofExcept (compile "let x = uniform(0,1) in if x < 0.5 then x else 0")
  assert (sampleAffinities p.checked.source == [.G]) "branching must force G"
  let p ← IO.ofExcept (compile "uniform[G](1,2) * uniform[E](0,1)")
  assert (sampleAffinities p.checked.source == [.G,.E]) "general left multiplication"
  let p ← IO.ofExcept (compile "uniform[E](0,1) / uniform[G](1,2)")
  assert (sampleAffinities p.checked.source == [.E,.G]) "general denominator"
  for text in ["let x = uniform[E](0,1) in x*x",
      "uniform[E](0,1) < 0.5", "uniform[G](0,uniform[E](0,1))", "fun x => x x",
      "true + 1", "missing", "flip(uniform[E](0,1))",
      "discrete(1,2,3)", "discrete(0.2,0.3)", "discrete(-0.5,1.5)"] do
    assert (compile text |> fun r => !r.isOk) s!"invalid program accepted: {text}"
  for text in ["fun x => x", "[]", "inl 1", "(1,true)",
      "let x = uniform[G](0,1) in x + uniform[E](0,1)",
      "flip(0.5)", "bernoulli(0.5)", "discrete(0.25,0.25,0.5)", "observe(true)"] do
    assert (compile text |>.isOk) s!"valid program rejected: {text}"

  for text in [
      "uniform[G](0,1) :: uniform[E](0,1) :: []",
      "uniform[E](0,1) :: uniform[G](0,1) :: []",
      "if true then (uniform[G](0,1), true) else (uniform[E](0,1), false)",
      "if true then inl uniform[G](0,1) else inl uniform[E](0,1)",
      "let f = if true then (fun x => uniform[G](0,1)) else (fun x => uniform[E](0,1)) in f 0",
      "let f = if true then (rec f x => if x < 0 then f (x+1) else uniform[G](0,1)) else (fun x => uniform[E](0,1)) in f 0"] do
    let p ← IO.ofExcept (compile text)
    assert ((sampleAffinities p.checked.source).contains .G && (sampleAffinities p.checked.source).contains .E)
      s!"structural subtyping changed requested affinities: {text}"

end Determinize.Tests
