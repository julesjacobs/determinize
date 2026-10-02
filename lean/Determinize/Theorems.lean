import Determinize.Spec.Expectation
import Determinize.Spec.Traces.Semantics
import Determinize.Spec.Inference
import Determinize.Spec.FiniteModel.Termination
import Determinize.Spec.RewardModel.Results
import Determinize.Frontend.Infer
import Determinize.Proof.Frontend.Inference
import Determinize.Proof.Semantics.Termination
import Determinize.Proof.Traces.ConditionalLaw
import Determinize.Proof.Normalization
import Determinize.Proof.FiniteModel.Termination
import Determinize.Proof.RewardModel.FiniteIntegrability
import Mathlib.Analysis.Convex.Function
import Mathlib.Probability.Kernel.Disintegration.StandardBorel
import Mathlib.Probability.Moments.Variance

/-!
# The theorems

Every theorem of the development is stated here in full, over the definitions in `Spec/`, and
proved by one term from `Proof/`. A reviewer reads the statements and the definitions they use,
not the proofs: the command at the end of this file fails the build unless every theorem depends
only on the standard axioms and its statement uses nothing from `Proof` except proofs.

The first three sections follow the paper. The output law `bigStepMeasure program` is
unnormalized: its mass is the probability that the program returns a real. The paper states its
global theorems for the output law conditioned on returning, `returnedLaw program`; the
versions for the unnormalized laws follow them.
-/

namespace Determinize.Theorems

open MeasureTheory ProbabilityTheory Spec Spec.Paper Spec.Traces
open scoped ENNReal

/-! ## Affinity inference (paper, Section 3) -/

/-- Inference correctness. Inference fails only when the input has no completion. Otherwise it
returns a completion, typed at the returned type, and every completion lies below it. -/
theorem inferenceCorrectness (input : Input) :
    match Frontend.infer input with
    | .error _ => ¬ ∃ program : Annotated, Completion input program
    | .ok (program, ty) =>
        input.matches program ∧
        Typed [] (interpret program) ty ∧
        ∀ completion : Annotated, Completion input completion → AffinityLE completion program :=
  Proof.Frontend.inferCorrect input

/-! ## Traces (paper, Section 4.1) -/

/-- Erasing terminating traces recovers the ordinary output semantics. -/
theorem traceErasure (program : Expr) :
    (traceAndOutputLaw program).map Prod.snd = bigStepMeasure program :=
  Proof.Traces.correspondence program

/-- Tracewise soundness. The source and target have the same law of terminating G traces. For
almost every such trace, the source's conditional output law has a finite mean and the target's
conditional output law is the Dirac mass at that mean. These are Mathlib's regular conditional
distributions of the joint laws over their trace marginals, unique up to null sets.
No global integrability assumption is required. -/
theorem traceConditionalLaw (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program) :
    DomainSafe program.determinize ∧
      traceLaw program.determinize = traceLaw program ∧
      ∀ᵐ trace ∂traceLaw program,
        Integrable id ((traceAndOutputLaw program).condKernel trace) ∧
        (traceAndOutputLaw program.determinize).condKernel trace =
          Measure.dirac (∫ value : ℝ, value ∂(traceAndOutputLaw program).condKernel trace) :=
  Proof.Traces.conditionalLaw program typed safe

/-! ## Global soundness (paper, Section 4.2 and the appendix) -/

/-- Expectation preservation. Extended expectations conditioned on returning, including infinite
expectations: when the source returns with positive probability and its expectation is
well-defined, so does the target and so is the target's, and the two agree. -/
theorem returnedExtendedExpectation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : bigStepMeasure program Set.univ ≠ 0)
    (defined : HasExpectation (bigStepMeasure program)) :
    bigStepMeasure program.determinize Set.univ ≠ 0 ∧
      HasExpectation (bigStepMeasure program.determinize) ∧
      ((bigStepMeasure program.determinize Set.univ).toReal⁻¹ : EReal) *
          extendedExpectation (bigStepMeasure program.determinize) =
        ((bigStepMeasure program Set.univ).toReal⁻¹ : EReal) *
          extendedExpectation (bigStepMeasure program) :=
  Proof.Paper.conditionalExtendedExpectationSoundness program typed safe positive defined

/-- Expectation preservation for finite expectations. Positive-mass output laws are probability
laws with finite, equal conditional means. -/
theorem returnedExpectation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : bigStepMeasure program Set.univ ≠ 0)
    (integrable : Integrable id (bigStepMeasure program)) :
    bigStepMeasure program.determinize Set.univ ≠ 0 ∧
      IsProbabilityMeasure (returnedLaw program) ∧
      IsProbabilityMeasure (returnedLaw program.determinize) ∧
      Integrable id (returnedLaw program) ∧
      Integrable id (returnedLaw program.determinize) ∧
      (∫ x, x ∂returnedLaw program.determinize) = ∫ x, x ∂returnedLaw program :=
  Proof.Paper.returnedExpectationSoundness program typed safe positive integrable

/-- Variance non-increase. Variance decreases for the probability laws of successful outputs. -/
theorem conditionalVarianceNonIncrease (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : bigStepMeasure program Set.univ ≠ 0)
    (moment : MemLp id 2 (bigStepMeasure program)) :
    bigStepMeasure program.determinize Set.univ ≠ 0 ∧
      MemLp id 2 (returnedLaw program.determinize) ∧
      variance id (returnedLaw program.determinize) ≤ variance id (returnedLaw program) :=
  Proof.Paper.conditionalVarianceSoundness program typed safe positive moment

/-- Convex function inequality. Jensen's inequality: every nonnegative convex function integrates
to at most as much under the determinized output law as under the source output law. -/
theorem jensenInequality (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (φ : ℝ → ℝ) (convex : ConvexOn ℝ Set.univ φ) (nonneg : ∀ value, 0 ≤ φ value) :
    ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure program.determinize ≤
      ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure program :=
  Proof.Paper.jensenSoundness program typed safe φ convex nonneg

/-- Probability preservation, first part. A domain-safe, real-typed program either returns or
continues forever. Rejection counts as divergence. -/
theorem returnOrDiverge (affinity : Affinity) (program : Expr)
    (typed : Typed [] program (.float affinity)) (safe : DomainSafe program) :
    bigStepMeasure program Set.univ + divergenceProbability program = 1 :=
  Proof.Paper.returnOrDiverge affinity program typed safe

/-- Probability preservation, second part. Determinization preserves the output mass. The output
laws are unnormalized: their total mass is the probability of terminating with a real value (and,
once observation exists, of being accepted). The source and target output laws have the same
mass, so together with `expectationPreservation` the expectations conditioned on termination
agree as well. That the target is domain-safe is the first conjunct of `traceConditionalLaw`. -/
theorem outputMassPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program) :
    bigStepMeasure program.determinize Set.univ = bigStepMeasure program Set.univ :=
  Proof.Paper.outputMassSoundness program typed safe

/-- Trace variance decomposition. Total variance conditioned on returning, with the trace law
normalized by output mass. -/
theorem conditionalTraceVarianceDecomposition (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : bigStepMeasure program Set.univ ≠ 0)
    (moment : MemLp id 2 (bigStepMeasure program)) :
    Integrable (fun trace => variance id ((traceAndOutputLaw program).condKernel trace))
        ((bigStepMeasure program Set.univ)⁻¹ • traceLaw program) ∧
      variance id (returnedLaw program) =
        variance id (returnedLaw program.determinize) +
          ∫ trace, variance id ((traceAndOutputLaw program).condKernel trace)
            ∂((bigStepMeasure program Set.univ)⁻¹ • traceLaw program) :=
  Proof.Traces.conditionalVarianceSoundness program typed safe positive moment

/-! ## The unnormalized output laws (not stated in the paper) -/

/-- Determinization preserves finite expectations and operation-domain safety. -/
theorem expectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (integrable : Integrable id (bigStepMeasure program)) :
    DomainSafe program.determinize ∧
      Integrable id (bigStepMeasure program.determinize) ∧
      (∫ value : ℝ, value ∂bigStepMeasure program) =
        ∫ value : ℝ, value ∂bigStepMeasure program.determinize :=
  Proof.Paper.finiteExpectationSoundness program typed safe integrable

/-- Determinization preserves expectations in the extended reals: whenever the source
expectation is well-defined, possibly infinite, so is the target's, and they agree. -/
theorem extendedExpectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (defined : HasExpectation (bigStepMeasure program)) :
    HasExpectation (bigStepMeasure program.determinize) ∧
      extendedExpectation (bigStepMeasure program) =
        extendedExpectation (bigStepMeasure program.determinize) :=
  Proof.Paper.extendedExpectationSoundness program typed safe defined

/-- Determinization preserves the expectation conditioned on acceptance, which is what makes
`observe` meaningful: when output mass is positive, rejection sampling on the determinized
program preserves the conditional expectation. The output
laws are unnormalized; an observation that fails contributes no output mass, so the mass of an
output law is the probability of terminating with a real value and every observation on the
way succeeding. Because every Boolean is G information, the same traces are rejected
in the source and in the target, and both the unnormalized mean (`expectationPreservation`) and
the mass (`outputMassPreservation`) are preserved; hence so is their quotient, the expectation of
the law normalized by its mass (`0` for a program that is always rejected, as `0 / 0 = 0`). -/
theorem conditionalExpectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (integrable : Integrable id (bigStepMeasure program)) :
    (∫ value : ℝ, value ∂bigStepMeasure program.determinize) /
        (bigStepMeasure program.determinize Set.univ).toReal =
      (∫ value : ℝ, value ∂bigStepMeasure program) / (bigStepMeasure program Set.univ).toReal :=
  Proof.Paper.conditionalExpectationSoundness program typed safe integrable

/-- Determinization does not increase the variance: whenever the source output law has a
finite second moment, so does the target output law (first conjunct). The second conjunct
is the raw second moment, `∫ v²` as a Bochner integral, and is Jensen's inequality for the
square (`jensenInequality` with `φ = (· ^ 2)`); the third is Mathlib's `variance`
(`∫ (v - ∫ v)²`, with the unnormalized mean). The output laws are unnormalized, but they have
equal mass (`outputMassPreservation`) and equal mean (`expectationPreservation`), so their
variances differ from their second moments by a term the two laws share and the third conjunct
follows from the second; it is kept as a separate conjunct so that the statement reads off
directly. The same inequalities hold for the laws normalized by their common mass. -/
theorem varianceNonIncrease (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (moment : MemLp id 2 (bigStepMeasure program)) :
    MemLp id 2 (bigStepMeasure program.determinize) ∧
      (∫ value : ℝ, value ^ 2 ∂bigStepMeasure program.determinize) ≤
        ∫ value : ℝ, value ^ 2 ∂bigStepMeasure program ∧
      variance id (bigStepMeasure program.determinize) ≤ variance id (bigStepMeasure program) :=
  Proof.Paper.varianceSoundness program typed safe moment

/-- The law of total variance along traces. When the source output law has a finite second
moment, the variances of the source's output laws given the traces are integrable over the
trace law and the source output variance is the target output variance plus their mean: trace
by trace, determinization discards exactly the variance of the output given the trace. The
output laws are unnormalized, but they have the same mass and, by `traceConditionalLaw`, the
same mean, so the identity holds for Mathlib's `variance` (`∫ (v - ∫ v)²`) without
normalization; dividing both output laws and the trace law by their common mass gives the same
identity for the laws conditioned on termination. -/
theorem traceVarianceDecomposition (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (moment : MemLp id 2 (bigStepMeasure program)) :
    Integrable (fun trace => variance id ((traceAndOutputLaw program).condKernel trace))
        (traceLaw program) ∧
      variance id (bigStepMeasure program) =
        variance id (bigStepMeasure program.determinize) +
          ∫ trace, variance id ((traceAndOutputLaw program).condKernel trace) ∂traceLaw program :=
  Proof.Traces.varianceSoundness program typed safe moment

/-! ## Finite models (not stated in the paper) -/

/-- Return, rejection and divergence probabilities of a finite model sum to one. -/
theorem finiteMassBalance (model : Spec.FiniteModel.Model) :
    model.outputMeasure Set.univ + model.rejectionProbability + model.divergenceProbability = 1 :=
  Proof.FiniteModel.massBalance model

/-- Every finite reward model has finite output moments, without extra premises. -/
theorem finiteRewardIntegrability (model : Spec.RewardModel.Model) : model.IntegrableMoments :=
  Proof.RewardModel.finite_integrable model

end Determinize.Theorems

/-! The theorems above may depend only on the standard axioms, and their statements may rely on
`Proof` only for proofs, which cannot change what they mean: Lean treats any two proofs of a
proposition as equal. Following the statements through the declarations they use, stopping at
proofs, must reach nothing declared in a `Proof` module. Otherwise the build fails. -/

open Lean Elab Command in
run_cmd do
  let env ← getEnv
  let theorems := (env.constants.fold (init := #[]) fun found name info =>
    if (`Determinize.Theorems).isPrefixOf name && info.isTheorem then found.push (name, info.type)
    else found).qsort (Name.lt ·.1 ·.1)
  let standard := [``propext, ``Classical.choice, ``Quot.sound]
  let mut sound := true
  for (name, _) in theorems do
    let extra := (← collectAxioms name).filter (!standard.contains ·)
    unless extra.isEmpty do
      sound := false
      logError m!"{name} depends on non-standard axioms {extra}"
  let moduleOf name := (env.getModuleIdxFor? name).bind (env.header.moduleNames[·]?)
  let mut reached : NameSet := {}
  let mut pending := theorems.flatMap (·.2.getUsedConstants)
  while !pending.isEmpty do
    let name := pending.back!
    pending := pending.pop
    -- Declarations outside `Determinize` cannot use ours; those in this file have no module index.
    if reached.contains name || !(moduleOf name).all (`Determinize).isPrefixOf then continue
    let some info := env.find? name | continue
    if ← liftTermElabM (Meta.isProp info.type) then continue
    reached := reached.insert name
    if (moduleOf name).any (`Determinize.Proof).isPrefixOf then
      sound := false
      logError m!"{name} is declared in a Proof module and is not a proof, but a theorem's statement relies on it"
    let constructors := match info with | .inductInfo type => type.ctors.toArray | _ => #[]
    pending := pending ++ constructors ++ info.type.getUsedConstants ++
      ((info.value? (allowOpaque := true)).map (·.getUsedConstants)).getD #[]
  if sound then
    logInfo m!"{theorems.size} theorems proved, using only {standard}"
    logInfo m!"Their statements rely on {reached.size} declarations other than proofs, none from Proof"
