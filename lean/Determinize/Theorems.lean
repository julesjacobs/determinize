import Determinize.Statement.Main
import Determinize.Traces.Main
import Determinize.Proof.Soundness

/-! The public propositions, proved with no additional hypotheses. -/

namespace Determinize.Theorems

theorem expectationPreservation : Statement.mainThm :=
  Proof.Paper.finiteExpectationSoundness

theorem traceErasure : Traces.correspondenceThm :=
  Proof.Traces.correspondence

theorem traceSoundness : Traces.soundnessThm :=
  Proof.Traces.soundness

end Determinize.Theorems

#print axioms Determinize.Theorems.expectationPreservation
#print axioms Determinize.Theorems.traceErasure
#print axioms Determinize.Theorems.traceSoundness
