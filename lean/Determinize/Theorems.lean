import Determinize.Proof.RewardModel.FiniteIntegrability
import Determinize.Spec.Main
import Determinize.Spec.Traces.Main
import Determinize.Spec.Inference
import Determinize.Proof.Soundness
import Determinize.Proof.Normalization
import Determinize.Proof.Traces.ConditionalLaw
import Determinize.Proof.Semantics.Termination
import Determinize.Proof.Frontend.Inference

/-! The public propositions, proved with no additional hypotheses. -/

namespace Determinize.Theorems

theorem returnOrDiverge : Spec.returnOrDivergeThm :=
  Proof.Paper.returnOrDiverge

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

theorem traceConditionalLaw : Spec.Traces.conditionalLawThm :=
  Proof.Traces.conditionalLaw

theorem traceVarianceDecomposition : Spec.Traces.varianceThm :=
  Proof.Traces.varianceSoundness

theorem conditionalVarianceNonIncrease : Spec.conditionalVarianceThm :=
  Proof.Paper.conditionalVarianceSoundness

theorem returnedExtendedExpectation : Spec.conditionalExtendedExpectationThm :=
  Proof.Paper.conditionalExtendedExpectationSoundness

theorem conditionalTraceVarianceDecomposition : Spec.Traces.conditionalVarianceThm :=
  Proof.Traces.conditionalVarianceSoundness

theorem finiteRewardIntegrability : Spec.RewardModel.finiteIntegrabilityThm :=
  Proof.RewardModel.finite_integrable

theorem returnedExpectation : Spec.returnedExpectationThm :=
  Proof.Paper.returnedExpectationSoundness

theorem inferenceCorrectness : Spec.inferCorrectThm :=
  Proof.Frontend.inferCorrect

end Determinize.Theorems

#print axioms Determinize.Theorems.expectationPreservation
#print axioms Determinize.Theorems.returnOrDiverge
#print axioms Determinize.Theorems.extendedExpectationPreservation
#print axioms Determinize.Theorems.jensenInequality
#print axioms Determinize.Theorems.outputMassPreservation
#print axioms Determinize.Theorems.varianceNonIncrease
#print axioms Determinize.Theorems.conditionalExpectationPreservation
#print axioms Determinize.Theorems.traceErasure
#print axioms Determinize.Theorems.traceConditionalLaw
#print axioms Determinize.Theorems.traceVarianceDecomposition

#print axioms Determinize.Theorems.conditionalVarianceNonIncrease
#print axioms Determinize.Theorems.returnedExtendedExpectation
#print axioms Determinize.Theorems.conditionalTraceVarianceDecomposition

#print axioms Determinize.Theorems.finiteRewardIntegrability

#print axioms Determinize.Theorems.returnedExpectation

#print axioms Determinize.Theorems.inferenceCorrectness
