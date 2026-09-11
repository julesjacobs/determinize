import Tests.FiniteModel
import Tests.Parsing
import Determinize.Finite.Solve

namespace Determinize.Tests
open Statement.FiniteModel

example : Checking.checkResult FiniteModel.fork FiniteModel.forkCertificate = true := by decide +kernel
example : FiniteModel.fork.HasExpectedReward (3/2) :=
  (Checking.checkResult_sound FiniteModel.fork FiniteModel.forkCertificate (by decide +kernel)).2
example : Checking.checkResult FiniteModel.loop (FiniteModel.loopCertificate 7) = false := by
  decide +kernel
example : Checking.checkResult FiniteModel.fork
    {FiniteModel.forkCertificate with values := fun _ => 0} = false := by decide +kernel
example : Checking.checkResult FiniteModel.fork
    {FiniteModel.forkCertificate with escape := 2} = false := by decide +kernel
example : Checking.checkResult FiniteModel.fork
    {FiniteModel.forkCertificate with horizon := 0} = false := by decide +kernel

abbrev retry : Model where
  size := 2
  initial := 0
  kind := fun i => if i = 0 then .transient else .returned (-3)
  transition := fun i j => if i = 0 then 1/2 else if i = j then 1 else 0
  nonnegative := by decide +kernel
  normalized := by decide +kernel
  absorbing := by decide +kernel

def retryResult : ResultCertificate retry := ⟨fun _ => -3, 1, 1/2⟩
example : retry.HasExpectedReward (-3) :=
  (Checking.checkResult_sound retry retryResult (by decide +kernel)).2
example : Checking.checkResult retry {retryResult with escape := 1} = false := by decide +kernel

private def expectAnswer (model : Model) (expected : Rat) : IO Unit := do
  let result ← IO.ofExcept (Finite.solve model)
  assert (result.values model.initial == expected) s!"wrong certified answer: expected {expected}"
  assert (Checking.checkResult model result) "solver returned an invalid certificate"

def results : IO Unit := do
  expectAnswer FiniteModel.fork (3/2)
  expectAnswer (FiniteModel.terminal (-7/3)) (-7/3)
  expectAnswer retry (-3)
  assert (match Finite.solve FiniteModel.loop with | .error _ => true | .ok _ => false) "solver accepted a nonabsorbing loop"
  assert (match Finite.solve FiniteModel.fork {maxStates := 2} with | .error _ => true | .ok _ => false) "solver ignored state limit"

#print axioms Proof.FiniteModel.resultCertificate_sound
#print axioms Checking.checkResult_sound
#print axioms Checking.checked_expectedReward

end Determinize.Tests

#print axioms Determinize.Checking.checked_sourceExpectedReward
