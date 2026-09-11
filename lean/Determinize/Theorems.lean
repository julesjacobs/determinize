import Determinize.Statement.Main
import Determinize.Traces.Main
import Determinize.Proof.Soundness
import Determinize.Proof.Corollaries

/-! The public propositions, proved with no additional hypotheses. -/

namespace Determinize.Theorems

theorem expectationPreservation : Statement.mainThm :=
  Proof.Paper.finiteExpectationSoundness

theorem extendedExpectationPreservation : Statement.extendedExpectationThm :=
  Proof.Paper.extendedExpectationSoundness

theorem jensenInequality : Statement.jensenThm :=
  Proof.Paper.jensenSoundness

theorem outputMassPreservation : Statement.outputMassThm :=
  Proof.Paper.outputMassSoundness

theorem varianceNonIncrease : Statement.varianceThm :=
  Proof.Paper.varianceSoundness

theorem conditionalExpectationPreservation : Statement.conditionalExpectationThm :=
  Proof.Paper.conditionalExpectationSoundness

theorem traceErasure : Traces.correspondenceThm :=
  Proof.Traces.correspondence

theorem traceSoundness : Traces.soundnessThm :=
  Proof.Traces.soundness

theorem traceVarianceDecomposition : Traces.varianceThm :=
  Proof.Traces.varianceSoundness

end Determinize.Theorems

#print axioms Determinize.Theorems.expectationPreservation
#print axioms Determinize.Theorems.extendedExpectationPreservation
#print axioms Determinize.Theorems.jensenInequality
#print axioms Determinize.Theorems.outputMassPreservation
#print axioms Determinize.Theorems.varianceNonIncrease
#print axioms Determinize.Theorems.conditionalExpectationPreservation
#print axioms Determinize.Theorems.traceErasure
#print axioms Determinize.Theorems.traceSoundness
#print axioms Determinize.Theorems.traceVarianceDecomposition
