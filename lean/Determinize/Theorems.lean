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

The first three sections state the paper's theorems, under the paper's names. There `e` is
`program`, `Determinize(e)` is `program.determinize`, the output law `μ_e` is
`bigStepMeasure program`, the joint law `J_e` is `traceAndOutputLaw program` with trace marginal
`traceLaw program`, the conditional law `μ_e(· | τ)` is
`(traceAndOutputLaw program).condKernel τ`, and `q_e`, `𝔼_ret[e]` and `Var_ret[e]` are
`returnProbability`, `returnedExpectation` and `returnedVariance`. The remaining sections state
results the paper does not.
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

/-! ## Tracewise soundness (paper, Section 4.1) -/

/-- The output marginal of the joint law: erasing terminating traces recovers the ordinary
output semantics. -/
theorem traceErasure (program : Expr) :
    (traceAndOutputLaw program).map Prod.snd = bigStepMeasure program :=
  Proof.Traces.correspondence program

/-- Trace preservation. Determinization preserves domain safety and the law of terminating
G traces. -/
theorem tracePreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program) :
    DomainSafe program.determinize ∧ traceLaw program.determinize = traceLaw program :=
  Proof.Traces.tracePreservation program typed safe

/-- Tracewise soundness. For almost every terminating G trace, the source's conditional output
law has a finite mean and the target's conditional output law is the Dirac mass at that mean.
These are Mathlib's regular conditional distributions of the joint laws over their trace
marginals, unique up to null sets. No global integrability assumption is required. -/
theorem tracewiseSoundness (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program) :
    ∀ᵐ trace ∂traceLaw program,
      Integrable id ((traceAndOutputLaw program).condKernel trace) ∧
      (traceAndOutputLaw program.determinize).condKernel trace =
        Measure.dirac (∫ value : ℝ, value ∂(traceAndOutputLaw program).condKernel trace) :=
  Proof.Traces.tracewiseSoundness program typed safe

/-! ## Global soundness (paper, Section 4.2 and the appendix) -/

/-- Output-mass preservation. The source and the target return with the same probability. -/
theorem outputMassPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program) :
    returnProbability program.determinize = returnProbability program :=
  Proof.Paper.outputMassSoundness program typed safe

/-- Expectation preservation. If the source returns with positive probability and its
expectation conditioned on returning is well-defined in the extended reals, then the same holds
for the target, and the two expectations agree. They may be infinite. -/
theorem expectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : 0 < returnProbability program)
    (defined : HasExpectation (bigStepMeasure program)) :
    0 < returnProbability program.determinize ∧
      HasExpectation (bigStepMeasure program.determinize) ∧
      returnedExpectation program.determinize = returnedExpectation program :=
  Proof.Paper.conditionalExtendedExpectationSoundness program typed safe positive defined

/-- Variance non-increase. If the source returns with positive probability and its output law
has a finite second moment, then the same holds for the target, and the target's variance
conditioned on returning is at most the source's. -/
theorem varianceNonIncrease (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : 0 < returnProbability program)
    (moment : MemLp id 2 (bigStepMeasure program)) :
    0 < returnProbability program.determinize ∧
      MemLp id 2 (bigStepMeasure program.determinize) ∧
      returnedVariance program.determinize ≤ returnedVariance program :=
  Proof.Paper.conditionalVarianceSoundness program typed safe positive moment

/-- Convex function inequality. Every nonnegative convex function integrates to at most as much
under the target's output law as under the source's. Both sides may be `+∞`. -/
theorem convexFunctionInequality (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (φ : ℝ → ℝ) (convex : ConvexOn ℝ Set.univ φ) (nonneg : ∀ value, 0 ≤ φ value) :
    ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure program.determinize ≤
      ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure program :=
  Proof.Paper.jensenSoundness program typed safe φ convex nonneg

/-- Probability preservation. The target is domain-safe, the source either returns or diverges
(rejection counts as divergence), and the target returns with the same probability as the
source. -/
theorem probabilityPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program) :
    DomainSafe program.determinize ∧
      returnProbability program + divergenceProbability program = 1 ∧
      returnProbability program.determinize = returnProbability program :=
  Proof.Paper.probabilityPreservation program typed safe

/-- Trace variance decomposition. If the source returns with positive probability and its
output law has a finite second moment, then its variance conditioned on returning is the
target's plus the average, over the trace law normalized by the return probability, of the
variance of the source's output given the trace. That average is finite. -/
theorem traceVarianceDecomposition (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : 0 < returnProbability program)
    (moment : MemLp id 2 (bigStepMeasure program)) :
    Integrable (fun trace ↦ variance id ((traceAndOutputLaw program).condKernel trace))
        ((returnProbability program)⁻¹ • traceLaw program) ∧
      returnedVariance program =
        returnedVariance program.determinize +
          ∫ trace, variance id ((traceAndOutputLaw program).condKernel trace)
            ∂((returnProbability program)⁻¹ • traceLaw program) :=
  Proof.Traces.conditionalVarianceSoundness program typed safe positive moment

/-! ## Further results on returned values (not stated in the paper) -/

/-- The first equation of `probabilityPreservation` at either affinity: a domain-safe,
real-typed program either returns or continues forever. Rejection counts as divergence. -/
theorem returnOrDiverge (affinity : Affinity) (program : Expr)
    (typed : Typed [] program (.float affinity)) (safe : DomainSafe program) :
    returnProbability program + divergenceProbability program = 1 :=
  Proof.Paper.returnOrDiverge affinity program typed safe

/-- `expectationPreservation` for finite expectations. If the source returns with positive
probability and its output law is integrable, then the returned laws of source and target are
probability laws with finite, equal means. -/
theorem finiteExpectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (positive : 0 < returnProbability program)
    (integrable : Integrable id (bigStepMeasure program)) :
    0 < returnProbability program.determinize ∧
      IsProbabilityMeasure (returnedLaw program) ∧
      IsProbabilityMeasure (returnedLaw program.determinize) ∧
      Integrable id (returnedLaw program) ∧
      Integrable id (returnedLaw program.determinize) ∧
      (∫ x, x ∂returnedLaw program.determinize) = ∫ x, x ∂returnedLaw program :=
  Proof.Paper.returnedExpectationSoundness program typed safe positive integrable

/-- Determinization preserves the expectation conditioned on acceptance, which is what makes
`observe` meaningful: rejection sampling on the determinized program preserves the conditional
expectation. An observation that fails contributes no output mass, so the return probability
is the probability of terminating with a real value and every observation on the way
succeeding. Because every Boolean is G information, the same traces are rejected in the source
and in the target, and both the unnormalized mean (`unnormalizedExpectationPreservation`) and
the return probability (`outputMassPreservation`) are preserved; hence so is their quotient
(`0` for a program that is always rejected, as `0 / 0 = 0`). -/
theorem conditionalExpectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (integrable : Integrable id (bigStepMeasure program)) :
    (∫ value : ℝ, value ∂bigStepMeasure program.determinize) /
        (returnProbability program.determinize).toReal =
      (∫ value : ℝ, value ∂bigStepMeasure program) / (returnProbability program).toReal :=
  Proof.Paper.conditionalExpectationSoundness program typed safe integrable

/-! ## The unnormalized output laws (not stated in the paper)

The same results without dividing by the return probability. -/

/-- Determinization preserves finite expectations and operation-domain safety. -/
theorem unnormalizedExpectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (integrable : Integrable id (bigStepMeasure program)) :
    DomainSafe program.determinize ∧
      Integrable id (bigStepMeasure program.determinize) ∧
      (∫ value : ℝ, value ∂bigStepMeasure program) =
        ∫ value : ℝ, value ∂bigStepMeasure program.determinize :=
  Proof.Paper.finiteExpectationSoundness program typed safe integrable

/-- Determinization preserves expectations in the extended reals: whenever the source
expectation is well-defined, possibly infinite, so is the target's, and they agree. -/
theorem unnormalizedExtendedExpectationPreservation (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (defined : HasExpectation (bigStepMeasure program)) :
    HasExpectation (bigStepMeasure program.determinize) ∧
      extendedExpectation (bigStepMeasure program) =
        extendedExpectation (bigStepMeasure program.determinize) :=
  Proof.Paper.extendedExpectationSoundness program typed safe defined

/-- Determinization does not increase the variance: whenever the source output law has a
finite second moment, so does the target output law (first conjunct). The second conjunct
is the raw second moment, `∫ v²` as a Bochner integral, and is `convexFunctionInequality` for
the square; the third is Mathlib's `variance` (`∫ (v - ∫ v)²`, with the unnormalized mean). The
output laws have equal mass (`outputMassPreservation`) and equal mean
(`unnormalizedExpectationPreservation`), so their variances differ from their second moments by
a term the two laws share and the third conjunct follows from the second; it is kept as a
separate conjunct so that the statement reads off directly. -/
theorem unnormalizedVarianceNonIncrease (program : Expr)
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
output laws have the same mass and, by `tracewiseSoundness`, the same mean, so the identity
holds for Mathlib's `variance` (`∫ (v - ∫ v)²`) without normalization. -/
theorem unnormalizedTraceVarianceDecomposition (program : Expr)
    (typed : Typed [] program (.float .E)) (safe : DomainSafe program)
    (moment : MemLp id 2 (bigStepMeasure program)) :
    Integrable (fun trace ↦ variance id ((traceAndOutputLaw program).condKernel trace))
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
  let theorems := (env.constants.fold (init := #[]) fun found name info ↦
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
      logError m!"{name} is declared in a Proof module and is not a proof, but a theorem's \
          statement relies on it"
    let constructors := match info with | .inductInfo type => type.ctors.toArray | _ => #[]
    pending := pending ++ constructors ++ info.type.getUsedConstants ++
      ((info.value? (allowOpaque := true)).map (·.getUsedConstants)).getD #[]
  if sound then
    logInfo m!"{theorems.size} theorems proved, using only {standard}"
    logInfo
      m!"Their statements rely on {reached.size} declarations other than proofs, none from Proof"
