import Determinize.Proof.RewardModel.FiniteIntegrability
import Determinize.Proof.FiniteModel.Termination
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

theorem finiteMassBalance : Spec.FiniteModel.massBalanceThm :=
  Proof.FiniteModel.massBalance

theorem returnedExpectation : Spec.returnedExpectationThm :=
  Proof.Paper.returnedExpectationSoundness

theorem inferenceCorrectness : Spec.inferCorrectThm :=
  Proof.Frontend.inferCorrect

end Determinize.Theorems

/-! Every proposition `Spec` defines must be the type of a theorem above, and those theorems may
depend only on the standard axioms. Otherwise the build fails. -/

open Lean Elab Command in
run_cmd do
  let env ← getEnv
  let statements := (env.constants.fold (init := #[]) fun found name info =>
    if (`Determinize.Spec).isPrefixOf name && info.isDefinition && info.type.isProp then found.push name
    else found).qsort Name.lt
  let theorems := (env.constants.fold (init := #[]) fun found name info =>
    if (`Determinize.Theorems).isPrefixOf name && info.isTheorem then found.push (name, info.type)
    else found).qsort (Name.lt ·.1 ·.1)
  let standard := [``propext, ``Classical.choice, ``Quot.sound]
  let mut complete := true
  for statement in statements do
    unless theorems.any (·.2 == .const statement []) do
      complete := false
      logError m!"{statement} is stated in Spec but no theorem in Determinize.Theorems proves it"
  for (name, _) in theorems do
    let extra := (← collectAxioms name).filter (!standard.contains ·)
    unless extra.isEmpty do
      complete := false
      logError m!"{name} depends on non-standard axioms {extra}"
  if complete then
    logInfo m!"{statements.size} Spec statements proved by {theorems.size} theorems, using only {standard}"
