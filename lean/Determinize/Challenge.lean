import Determinize.Spec.Main
import Determinize.Spec.Traces.Main
import Determinize.Spec.Inference
import Determinize.Spec.FiniteModel.Termination
import Determinize.Spec.RewardModel.Results

/-! Every public theorem, with `sorry` in place of its proof. This file imports only `Spec`, so
nothing in `Proof` can change what the statements mean. `Theorems.lean` proves the same theorems
under the same names. Lean FRO's comparator (`./check.sh comparator`) checks that each of them is
proved there with the same statement and only the standard axioms, and replays the proofs through
Lean's kernel. The default build does not build this file. -/

namespace Determinize.Theorems

theorem returnOrDiverge : Spec.returnOrDivergeThm := sorry
theorem expectationPreservation : Spec.mainThm := sorry
theorem extendedExpectationPreservation : Spec.extendedExpectationThm := sorry
theorem jensenInequality : Spec.jensenThm := sorry
theorem outputMassPreservation : Spec.outputMassThm := sorry
theorem varianceNonIncrease : Spec.varianceThm := sorry
theorem conditionalExpectationPreservation : Spec.conditionalExpectationThm := sorry
theorem traceErasure : Spec.Traces.correspondenceThm := sorry
theorem traceJointLawFinite : Spec.Traces.finiteJointLawThm := sorry
theorem traceConditionalLaw : Spec.Traces.conditionalLawThm := sorry
theorem traceVarianceDecomposition : Spec.Traces.varianceThm := sorry
theorem conditionalVarianceNonIncrease : Spec.conditionalVarianceThm := sorry
theorem returnedExtendedExpectation : Spec.conditionalExtendedExpectationThm := sorry
theorem conditionalTraceVarianceDecomposition : Spec.Traces.conditionalVarianceThm := sorry
theorem finiteRewardIntegrability : Spec.RewardModel.finiteIntegrabilityThm := sorry
theorem finiteMassBalance : Spec.FiniteModel.massBalanceThm := sorry
theorem returnedExpectation : Spec.returnedExpectationThm := sorry
theorem inferenceCorrectness : Spec.inferCorrectThm := sorry

end Determinize.Theorems
