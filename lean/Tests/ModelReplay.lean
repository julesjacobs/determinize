import Tests.Parsing
import Determinize.Checking.FiniteModel

namespace Determinize.Tests
open Frontend Checking Spec.Paper Spec.FiniteModel Determinize.Finite

private def candidateFor (text : String) (subject : Subject := .source) : IO (Core × Candidate) := do
  let p ← IO.ofExcept (compile text)
  match explore p.checked.source subject with
  | .complete candidate => return (p.checked.source, candidate)
  | result => throw (IO.userError s!"expected complete graph: {reprStr result}")

private def accepted (source : Core) (candidate : Candidate) (subject : Subject := .source) : Bool :=
  (checkModel source subject candidate).isSome

private def setRow (candidate : Candidate) (i : Nat) (row : Row) : Candidate :=
  {candidate with rows := candidate.rows.set! i row}

def modelReplay : IO Unit := do
  for text in [
      "0.1 + 0.2", "let x = 4 in (fun y => x + y) 3",
      "let f = rec f x => f x in f 0",
      "let f = rec f x => if flip(0.5) then 3 else f x in f 0",
      "discrete[G](0,0.25,0,0.75)", "bernoulli[G](0)", "bernoulli[G](1)",
      "let _ = observe(flip(0.5)) in 3",
      "match 2::[] with [] => 0 | x::xs => x",
      "let fact = rec f n => if n < 1 then 1 else n * f (n-1) in fact 4"] do
    let (source, candidate) ← candidateFor text
    assert (accepted source candidate) s!"machine replay: {text}"
  let (threeSource, three) ← candidateFor "3"
  let (sevenSource, seven) ← candidateFor "7"
  let some checkedThree := checkModel threeSource .source three
    | throw (IO.userError "expected checked model")
  let some checkedSeven := checkModel sevenSource .source seven
    | throw (IO.userError "expected checked model for seven")
  assert (decide (∃ state, checkedThree.model.kind state = .returned 3)) "extracted reward three"
  assert (decide (∃ state, checkedSeven.model.kind state = .returned 7)) "extracted reward seven"
  let (targetSource, target) ← candidateFor "uniform[E](0,3)" .determinized
  assert (accepted targetSource target .determinized) "determinized mean replay"
  assert (!(checkModelReplay targetSource .source target).isSome) "wrong subject"
  assert (!(checkModelReplay (.real 7) .determinized target).isSome) "wrong source"
  let (coinSource, coin) ← candidateFor "bernoulli[G](0.25)"
  let some checkedCoin := checkModel coinSource .source coin
    | throw (IO.userError "expected checked coin model")
  assert (checkedCoin.model.size == coin.states.size) "extracted model dimensions"
  let accepted := accepted coinSource
  let extra := coin.states.size
  let oneExtra : Candidate :=
    { coin with
      states := coin.states.push .rejected
      rows := coin.rows.push ⟨.rejected,#[⟨extra,1⟩]⟩ }
  assert (accepted oneExtra) "unique unreachable state"
  let duplicate : Candidate :=
    { oneExtra with
      states := oneExtra.states.push .rejected
      rows := oneExtra.rows.push ⟨.rejected,#[⟨extra+1,1⟩]⟩ }
  assert (!accepted duplicate) "duplicate unreachable states"
  let mut sampled := false
  let mut terminal := false
  for i in [:coin.rows.size] do
    let row := coin.rows[i]?.getD ⟨.rejected,#[]⟩
    if row.edges.size == 2 then
      sampled := true
      let first := row.edges[0]?.getD ⟨0,0⟩
      let second := row.edges[1]?.getD ⟨0,0⟩
      for replacement in [
          {row with edges := #[⟨first.target,1⟩]},
          {row with edges := #[⟨first.target,1/2⟩,⟨second.target,1/2⟩]},
          {row with edges := #[⟨first.target,first.probability⟩,⟨first.target,second.probability⟩]},
          {row with edges := #[⟨coin.states.size,1⟩]},
          {row with kind := .returned 99}] do
        assert (!accepted (setRow coin i replacement)) "mutated sampled row"
    match row.kind with
    | .returned reward =>
        terminal := true
        assert (!accepted (setRow coin i {row with kind := .returned (reward+1)})) "changed reward"
        assert (!accepted (setRow coin i {row with edges := #[⟨0,1⟩]})) "nonabsorbing terminal"
        assert (!accepted (setRow coin i {row with kind := .rejected})) "changed outcome"
    | _ => pure ()
  assert (sampled && terminal) "mutation fixtures exercised"
  assert (!accepted {coin with rows := coin.rows.pop}) "truncated rows"
  assert (!accepted {coin with states := coin.states.pop, rows := coin.rows.pop}) "truncated graph"
  assert (!accepted {coin with initial := coin.states.size}) "invalid initial index"
  assert (!accepted {coin with initial := 1}) "wrong initial state"
  assert (!accepted {coin with states := coin.states.set! 0 (.eval (.real 7) [] [])}) "changed initial expression"
  assert (!accepted {coin with states := coin.states.set! 1 (.eval (.bvar 99) [] [])}) "malformed reachable state"
  let unusedFreeVariable : Core := .letE (.lam (.bvar 2)) (.real 3)
  match explore unusedFreeVariable .source with
  | .complete candidate => assert (!(checkModel unusedFreeVariable .source candidate).isSome) "free variable in unused closure"
  | _ => throw (IO.userError "scope fixture must finish exploration")
  let broken : Candidate := ⟨0,#[.eval (.bvar 0) [] []],
    #[⟨.rejected,#[⟨0,1⟩]⟩]⟩
  assert (!(checkModel (.bvar 0) .source broken).isSome) "stuck machine cannot be labeled rejected"

#print axioms Proof.FiniteModel.replay_matches
#print axioms checkModel
#print axioms replay_reachable_covered
#print axioms replay_reachable_no_failure
#print axioms replay_successor_covered
#print axioms replay_transition_weight

end Determinize.Tests
