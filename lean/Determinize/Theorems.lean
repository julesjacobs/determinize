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
depend only on the standard axioms. The propositions may rely on `Proof` only for proofs, which
cannot change what they mean: Lean treats any two proofs of a proposition as equal. Following the
propositions through the declarations they use, stopping at proofs, must reach nothing declared in
a `Proof` module. Otherwise the build fails. -/

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
  let moduleOf name := (env.getModuleIdxFor? name).bind (env.header.moduleNames[·]?)
  let mut reached : NameSet := {}
  let mut pending := statements
  while !pending.isEmpty do
    let name := pending.back!
    pending := pending.pop
    -- Declarations outside `Determinize` cannot use ours; those in this file have no module index.
    if reached.contains name || !(moduleOf name).all (`Determinize).isPrefixOf then continue
    let some info := env.find? name | continue
    if ← liftTermElabM (Meta.isProp info.type) then continue
    reached := reached.insert name
    if (moduleOf name).any (`Determinize.Proof).isPrefixOf then
      complete := false
      logError m!"{name} is declared in a Proof module and is not a proof, but Spec relies on it"
    let constructors := match info with | .inductInfo type => type.ctors.toArray | _ => #[]
    pending := pending ++ constructors ++ info.type.getUsedConstants ++
      ((info.value? (allowOpaque := true)).map (·.getUsedConstants)).getD #[]
  if complete then
    logInfo m!"{statements.size} Spec statements proved by {theorems.size} theorems, using only {standard}"
    logInfo m!"Spec statements rely on {reached.size} declarations other than proofs, none from Proof"
