import Determinize.Spec.Semantics
import Mathlib.Analysis.Convex.Function
import Mathlib.Data.EReal.Operations
import Mathlib.Probability.Moments.Variance

/-!
# Soundness of paper determinization

These are the reviewer-facing theorem statements over the fixed paper syntax,
typing judgment, determinization transformation, and measure semantics.

`mainThm` is the expectation theorem for finite (Bochner) expectations.
`extendedExpectationThm` states it in the extended reals, where the expectation of a law
is well-defined as soon as one of its positive and negative parts is finite; it covers
programs such as `1 / uniform(0, 1)` whose expectation is `+∞`. `jensenThm` compares the
two output laws against every nonnegative convex function. `outputMassThm` states that both
output laws have the same total mass, and `varianceThm` that determinization does not increase
the second moment or the variance of the output law. `conditionalExpectationThm` states that the
expectations conditioned on acceptance agree, which is what makes `observe` meaningful.
-/

namespace Determinize.Spec

open MeasureTheory ProbabilityTheory Paper
open scoped ENNReal

/-- Determinization preserves finite expectations and validity of distribution arguments. -/
def mainThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) →
    PrimitiveDomainSafe program →
    Integrable id (bigStepMeasure program) →
    PrimitiveDomainSafe program.determinize ∧
      Integrable id (bigStepMeasure program.determinize) ∧
      (∫ value : ℝ, value ∂bigStepMeasure program) =
        ∫ value : ℝ, value ∂bigStepMeasure program.determinize

/-- The positive part `∫ v⁺ dμ` of the expectation of a real law. -/
noncomputable def posPartIntegral (μ : Measure ℝ) : ℝ≥0∞ := ∫⁻ value, ENNReal.ofReal value ∂μ

/-- The negative part `∫ v⁻ dμ` of the expectation of a real law. -/
noncomputable def negPartIntegral (μ : Measure ℝ) : ℝ≥0∞ :=
  ∫⁻ value, ENNReal.ofReal (-value) ∂μ

/-- The expectation of `μ` is well-defined in `[-∞, ∞]` when at least one part is finite. -/
def HasExpectation (μ : Measure ℝ) : Prop := posPartIntegral μ ≠ ⊤ ∨ negPartIntegral μ ≠ ⊤

/-- The expectation `∫ v⁺ dμ - ∫ v⁻ dμ` in the extended reals; meaningful under
`HasExpectation μ`. -/
noncomputable def extendedExpectation (μ : Measure ℝ) : EReal :=
  (posPartIntegral μ : EReal) - (negPartIntegral μ : EReal)

/-- Determinization preserves expectations in the extended reals: whenever the source
expectation is well-defined, possibly infinite, so is the target's, and they agree. -/
def extendedExpectationThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) →
    PrimitiveDomainSafe program →
    HasExpectation (bigStepMeasure program) →
    HasExpectation (bigStepMeasure program.determinize) ∧
      extendedExpectation (bigStepMeasure program) =
        extendedExpectation (bigStepMeasure program.determinize)

/-- Jensen's inequality: every nonnegative convex function integrates to at most as much
under the determinized output law as under the source output law. -/
def jensenThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) →
    PrimitiveDomainSafe program →
    ∀ φ : ℝ → ℝ, ConvexOn ℝ Set.univ φ → (∀ value, 0 ≤ φ value) →
      ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure program.determinize ≤
        ∫⁻ value, ENNReal.ofReal (φ value) ∂bigStepMeasure program

/-- Determinization preserves the output mass. The output laws are unnormalized: their total
mass is the probability of terminating with a real value (and, once observation exists, of
being accepted). The source and target output laws have the same mass, so together with
`mainThm` the expectations conditioned on termination agree as well. -/
def outputMassThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) →
    PrimitiveDomainSafe program →
    bigStepMeasure program.determinize Set.univ = bigStepMeasure program Set.univ

/-- Determinization does not increase the variance: whenever the source output law has a
finite second moment, so does the target output law (first conjunct). The second conjunct
is the raw second moment, `∫ v²` as a Bochner integral, and is Jensen's inequality for the
square (`jensenThm` with `φ = (· ^ 2)`); the third is Mathlib's `variance`
(`∫ (v - ∫ v)²`, with the unnormalized mean). The output laws are unnormalized, but they have
equal mass (`outputMassThm`) and equal mean (`mainThm`), so their variances differ from their
second moments by a term the two laws share and the third conjunct follows from the second;
it is kept as a separate conjunct so that the statement reads off directly. The same
inequalities hold for the laws normalized by their common mass. -/
def varianceThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) →
    PrimitiveDomainSafe program →
    MemLp id 2 (bigStepMeasure program) →
    MemLp id 2 (bigStepMeasure program.determinize) ∧
      (∫ value : ℝ, value ^ 2 ∂bigStepMeasure program.determinize) ≤
        ∫ value : ℝ, value ^ 2 ∂bigStepMeasure program ∧
      variance id (bigStepMeasure program.determinize) ≤ variance id (bigStepMeasure program)

/-- Determinization preserves the expectation conditioned on acceptance, which is what makes
`observe` meaningful: when output mass is positive, rejection sampling on the determinized
program preserves the conditional expectation. The output
laws are unnormalized; an observation that fails contributes no output mass, so the mass of an
output law is the probability of terminating with a real value and every observation on the
way succeeding. Because every Boolean is G information, the same traces are rejected
in the source and in the target, and both the unnormalized mean (`mainThm`) and the mass
(`outputMassThm`) are preserved; hence so is their quotient, the expectation of the law
normalized by its mass (`0` for a program that is always rejected, as `0 / 0 = 0`). -/
def conditionalExpectationThm : Prop :=
  ∀ (program : Expr),
    Typed [] program (.float .E) →
    PrimitiveDomainSafe program →
    Integrable id (bigStepMeasure program) →
    (∫ value : ℝ, value ∂bigStepMeasure program.determinize) /
        (bigStepMeasure program.determinize Set.univ).toReal =
      (∫ value : ℝ, value ∂bigStepMeasure program) / (bigStepMeasure program Set.univ).toReal

end Determinize.Spec
