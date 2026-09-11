import Determinize.Spec.Main
import Determinize.Spec.Traces.Main
import Determinize.Proof.Soundness
import Determinize.Proof.Corollaries
import Determinize.Proof.ConditionalLaw

/-! The public propositions, proved with no additional hypotheses. -/

namespace Determinize.Theorems

theorem expectationPreservation : Spec.mainThm :=
  Proof.Paper.finiteExpectationSoundness

theorem extendedExpectationPreservation : Spec.extendedExpectationThm :=
  Proof.Paper.extendedExpectationSoundness

theorem jensenInequality : Spec.jensenThm :=
  Proof.Paper.jensenSoundness

theorem outputMassPreservation : Spec.outputMassThm :=
  Proof.Paper.outputMassSoundness

theorem varianceNonIncrease : Spec.varianceThm :=
  Proof.Paper.varianceSoundness

theorem conditionalExpectationPreservation : Spec.conditionalExpectationThm :=
  Proof.Paper.conditionalExpectationSoundness

theorem traceErasure : Spec.Traces.correspondenceThm :=
  Proof.Traces.correspondence

theorem traceSoundness : Spec.Traces.soundnessThm :=
  Proof.Traces.soundness

theorem traceConditionalLaw : Spec.Traces.conditionalLawThm :=
  Proof.Traces.conditionalLaw

theorem traceVarianceDecomposition : Spec.Traces.varianceThm :=
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
#print axioms Determinize.Theorems.traceConditionalLaw
#print axioms Determinize.Theorems.traceVarianceDecomposition
