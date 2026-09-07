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

theorem traceErasure : Traces.correspondenceThm :=
  Proof.Traces.correspondence

theorem traceSoundness : Traces.soundnessThm :=
  Proof.Traces.soundness

end Determinize.Theorems

#print axioms Determinize.Theorems.expectationPreservation
#print axioms Determinize.Theorems.extendedExpectationPreservation
#print axioms Determinize.Theorems.jensenInequality
#print axioms Determinize.Theorems.traceErasure
#print axioms Determinize.Theorems.traceSoundness
