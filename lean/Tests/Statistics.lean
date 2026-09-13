import Determinize.Finite.Statistics
import Determinize.Checking.Statistics
import Tests.FiniteModel
import Tests.Parsing

namespace Determinize.Tests
open Spec.FiniteModel Proof.FiniteModel

abbrev mixedOutcomes : Model where
  size := 5
  initial := 0
  kind := ![.transient, .returned (-2), .returned 4, .rejected, .transient]
  transition := fun i j => if i = 0 then (if j = 0 then 0 else 1/4) else if i = j then 1 else 0
  nonnegative := by decide +kernel
  normalized := by decide +kernel

private def mixedResult := (Finite.solveStatistics mixedOutcomes).toOption.get (by decide +kernel)
private def mixedStatistics := mixedResult.val.statistics mixedOutcomes

example : mixedStatistics = ⟨1/2, 1/2, 5⟩ := by decide +kernel
example : mixedStatistics.conditionalMean = some 1 := by decide +kernel
example : mixedStatistics.conditionalVariance = some 9 := by decide +kernel
example : mixedStatistics.Matches mixedOutcomes.outputMeasure :=
  momentCertificate_sound mixedOutcomes mixedResult.val mixedResult.property

example : Checking.checkStatistics mixedOutcomes mixedResult.val = true := by decide +kernel
example : Checking.checkStatistics mixedOutcomes {mixedResult.val with horizon := 0} = false := by decide +kernel
example : Checking.checkStatistics mixedOutcomes {mixedResult.val with dead := fun _ => true} = false := by decide +kernel
example : Checking.checkStatistics mixedOutcomes {mixedResult.val with dead := fun _ => false} = false := by decide +kernel
example : Checking.checkStatistics mixedOutcomes
    {mixedResult.val with values := fun _ _ => 7} = false := by decide +kernel

private def divergentResult := (Finite.solveStatistics FiniteModel.loop).toOption.get (by decide +kernel)
example : (divergentResult.val.statistics FiniteModel.loop).conditionalMean = none := by decide +kernel
example : (divergentResult.val.statistics FiniteModel.loop).conditionalVariance = none := by decide +kernel
example : (divergentResult.val.statistics FiniteModel.loop) = ⟨0,0,0⟩ := by decide +kernel

def statistics : IO Unit := do
  let result ← IO.ofExcept (Finite.solveStatistics mixedOutcomes)
  assert (result.val.statistics mixedOutcomes == ⟨1/2,1/2,5⟩) "mixed return/rejection/divergence moments"
  assert (match Finite.solveStatistics mixedOutcomes {maxStates := 4} with
    | .error _ => true | _ => false) "statistics state limit"

#print axioms cut_outputMeasure
#print axioms momentCertificate_sound
#print axioms Checking.checked_statistics
#print axioms Checking.checked_conditionalVariance

end Determinize.Tests
