import Tests.Parsing
import Determinize.Finite.Export

namespace Determinize.Tests
open Frontend Checking Spec.Paper Spec.FiniteModel Determinize.Finite

private def graph (text : String) (subject : Subject := .source) : IO Candidate := do
  let p ← IO.ofExcept (compile text)
  match explore p.checked.source subject with
  | .complete candidate => return candidate
  | result => throw (IO.userError s!"exploration of {text}: {reprStr result}")

private def reward (candidate : Candidate) (steps : Nat) : Rat := Id.run do
  let mut values := Array.replicate candidate.states.size (0 : Rat)
  for _ in [:steps] do
    values := candidate.rows.map fun row => match row.kind with
      | .returned r => r
      | .rejected => 0
      | .transient => row.edges.foldl (fun total edge =>
          total + edge.probability * values[edge.target]!) 0
  return values[candidate.initial]!

def explorer : IO Unit := do
  for (text, expected) in [
      ("0.1 + 0.2", (3/10 : Rat)), ("7 / 0", 0),
      ("let x = 4 in let f = fun y => x + y in let x = 100 in f 3", 7),
      ("let fact = rec f n => if n < 1 then 1 else n * f (n-1) in fact 5", 120),
      ("match 2::3::[] with [] => 0 | x::xs => match xs with [] => 0 | y::ys => x*10+y", 23),
      ("match inr 7 with inl x => 0 | inr y => y", 7),
      ("match inl 9 with inl x => x | inr y => 0", 9),
      ("fst (2,3) + snd (4,5)", 7),
      ("if flip(0.25) then 8 else -4", -1),
      ("let _ = observe(flip(0.5)) in 3", 3/2),
      ("let _ = observe(false) in uniform[G](0,1)", 0),
      ("if flip(0) then uniform[G](0,1) else 7", 7),
      ("discrete[G](0,0.25,0,0.75)", 5/2),
      ("bernoulli[G](1)", 1), ("bernoulli[G](0)", 0)] do
    let candidate ← graph text
    assert (reward candidate (candidate.states.size+1) == expected) s!"exact reward: {text}"
    let files ← IO.ofExcept (render candidate)
    let again ← graph text
    assert (files.candidate == candidateText again) "stable state numbering"
  for (text, expected) in [
      ("uniform[E](0,3)", (3/2 : Rat)), ("gauss[E](3,2)", 3),
      ("poisson[E](4)", 4), ("exponential[E](2)", 1/2),
      ("gamma[E](6,2)", 3), ("beta[E](1,3)", 1/4),
      ("bernoulli[E](0.3)", 3/10), ("discrete[E](0,0.25,0,0.75)", 5/2)] do
    let candidate ← graph text .determinized
    assert (reward candidate (candidate.states.size+1) == expected) s!"exact mean: {text}"
  let loop ← graph "let f = rec f x => f x in f 0"
  assert (loop.rows.all fun row => row.kind == .transient) "closed nonterminating loop"
  assert (reward loop 100 == 0) "nontermination contributes zero"
  let retry ← graph "let f = rec f x => if flip(0.5) then 3 else f x in f 0"
  assert (retry.rows.toList.zipIdx.any fun (row,i) => row.edges.any fun e => e.target < i) "retry graph"
  assert (reward retry 100 > 2 && reward retry 100 < 3) "geometric retry reward bound"
  let growing ← IO.ofExcept (compile "let f = rec f x => f (x+1) in f 0")
  match explore growing.checked.source .source {maxStates := 100} with
  | .incomplete .states .. => pure ()
  | _ => throw (IO.userError "growing recursion must exhaust states")
  for (limits, expected) in [
      ({maxStates := 0 : Limits}, Limit.states),
      ({maxEdges := 0 : Limits}, Limit.edges),
      ({maxStateBytes := 0 : Limits}, Limit.stateBytes)] do
    match explore (.real 3) .source limits with
    | .incomplete actual .. => assert (actual == expected) "resource limit classification"
    | _ => throw (IO.userError "expected incomplete exploration")
  for text in ["uniform[G](0,1)", "poisson[G](3)", "gauss[G](0,1)",
      "uniform[E](0,uniform[G](1,2))", "uniform[E](2,1)",
      "bernoulli[G](2)", "true"] do
    let p ← IO.ofExcept (compile text)
    match explore p.checked.source .determinized with
    | .failed .. => pure ()
    | _ => throw (IO.userError s!"expected export failure: {text}")
  let state := State.deliver (.number 1) []
  assert (aggregate [(0,state),(1/3,state),(2/3,state)] == [(1,state)]) "duplicate successors"
  let terminal : Candidate := ⟨0, #[state],
    #[⟨.returned (-3), #[⟨0,1⟩]⟩]⟩
  let files ← IO.ofExcept (render terminal)
  assert (files.transitions == "dtmc\n0 1 1\n1 1 1\n") "once-only terminal reward sink"
  assert (files.positiveRewards == "" && files.negativeRewards == "0 3\n") "signed rewards"
  assert (files.labels == "#DECLARATION\ninit returned rejected done\n#END\n0 init returned\n1 done\n") "initial terminal labels"
  for bad in [
      {terminal with rows := #[]},
      {terminal with initial := 1},
      {terminal with rows := #[⟨.transient,#[⟨0,1/2⟩]⟩]},
      {terminal with rows := #[⟨.transient,#[⟨0,1/2⟩,⟨0,1/2⟩]⟩]},
      {terminal with rows := #[⟨.returned 3,#[⟨1,1⟩]⟩]}] do
    assert (match render bad with | .error _ => true | .ok _ => false) "malformed export rejected"

end Determinize.Tests
