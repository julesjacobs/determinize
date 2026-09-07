import Determinize.Proof.Internal.PrimitiveLaws
import Determinize.Proof.Internal.Environment
import Determinize.Statement.Semantics
import Determinize.Proof.Internal.ExpressionSpace
import Mathlib.MeasureTheory.Constructions.Pi
import Mathlib.Tactic.DeriveCountable

/-!
# Semantic objects used by the proof

This module defines the measurable skeleton representation, symbolic sample
histories, certified one-step kernel, exact-depth output semantics, and the
primitive-domain safety invariant. The operational reducer itself is the
reviewer-facing definition in `Statement.Semantics`.
-/

namespace Determinize.Statement.Paper

open MeasureTheory ProbabilityTheory
open scoped ENNReal ProbabilityTheory

/-- Interpret one reduction action as a measure of successor expressions. -/
noncomputable def Action.measure : Action → Measure Expr
  | .next expression => Measure.dirac expression
  | .sample _ fiber continuation => fiber.map continuation
  | .stuck => 0

/-- The measurable transition used by the proof's expression kernels. -/
noncomputable def stepMeasure (expression : Expr) : Measure Expr :=
  (reduce expression).measure

@[match_pattern] abbrev Skeleton.real (mode : Mode) : Skeleton := Expr.real mode ()

namespace Expr

def realArity {Literal : Type} : Expr Literal → Nat
  | .real _ _ => 1
  | .lam x | .fix x | .fst x | .snd x | .inl x
  | .inr x | .promote x | .neg _ x => x.realArity
  | .app l r | .pair l r | .cons l r | .add _ l r | .mul _ l r
  | .div _ l r | .lt l r => l.realArity + r.realArity
  | .matchSum x l r | .ite x l r => x.realArity + l.realArity + r.realArity
  | .matchList x n c => x.realArity + n.realArity + c.realArity
  | .letE x b => x.realArity + b.realArity
  | .sample _ _ affine general => (affine.map realArity).sum + (general.map realArity).sum
  | _ => 0

/-- Modes of real coordinates in structural traversal order. -/
def coordinateModes {Literal : Type} : Expr Literal → List Mode
  | .real mode _ => [mode]
  | .lam x | .fix x | .fst x | .snd x | .inl x
  | .inr x | .promote x | .neg _ x => x.coordinateModes
  | .app l r | .pair l r | .cons l r | .add _ l r | .mul _ l r
  | .div _ l r | .lt l r => l.coordinateModes ++ r.coordinateModes
  | .matchSum x l r | .ite x l r =>
      x.coordinateModes ++ l.coordinateModes ++ r.coordinateModes
  | .matchList x n c => x.coordinateModes ++ n.coordinateModes ++ c.coordinateModes
  | .letE x b => x.coordinateModes ++ b.coordinateModes
  | .sample _ _ affine general =>
      affine.flatMap coordinateModes ++ general.flatMap coordinateModes
  | _ => []

end Expr

end Determinize.Statement.Paper

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory
open Determinize.Statement.Paper
open scoped ENNReal ProbabilityTheory

namespace Symbolic

/-- An affine expression in the ordered E samples. -/
abbrev Affine (sampleCount : Nat) := ℝ × (Fin sampleCount → ℝ)

def Affine.eval (expression : Affine sampleCount) (environment : Env sampleCount) : ℝ :=
  expression.1 + ∑ i, expression.2 i * environment i

/-- Ordered symbolic E-sample environment from the paper. -/
inductive SampleEnv (laws : Determinize.Proof.Paper.PrimitiveLaws) : Nat → Type where
  | nil : SampleEnv laws 0
  | snoc (history : SampleEnv laws n) (op : Determinize.Statement.Paper.Op)
      (affineArgs : Fin (Determinize.Statement.Paper.affineArity op) → Affine n)
      (generalArgs : Fin (Determinize.Statement.Paper.generalArity op) → ℝ) : SampleEnv laws (n + 1)

namespace SampleEnv

noncomputable def actualMeasure (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    {n : Nat} → SampleEnv laws n → Measure (Env n)
  | 0, .nil => Measure.dirac Env.empty
  | _ + 1, .snoc history op affineArgs generalArgs =>
      (actualMeasure laws history).bind fun environment =>
        (laws.kernel op
          (fun i => (affineArgs i).eval environment, generalArgs)).map
            (fun value => Env.cons value environment)

/-- Every recorded primitive call is in-domain almost surely under its prefix law. -/
noncomputable def DomainSafe (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    {n : Nat} → SampleEnv laws n → Prop
  | 0, .nil => True
  | _ + 1, .snoc history op affineArgs generalArgs =>
      DomainSafe laws history ∧
        ∀ᵐ environment ∂actualMeasure laws history,
          Determinize.Statement.Paper.domain op (fun i => (affineArgs i).eval environment, generalArgs)

noncomputable def meanEnvironment (laws : Determinize.Proof.Paper.PrimitiveLaws) :
    {n : Nat} → SampleEnv laws n → Env n
  | 0, .nil => Env.empty
  | _ + 1, .snoc history op affineArgs generalArgs =>
      let environment := meanEnvironment laws history
      let params := (fun i => (affineArgs i).eval environment, generalArgs)
      Env.cons (Determinize.Statement.Paper.meanValue op params) environment

end SampleEnv

end Symbolic

/-- Measurability and subprobability laws for the specified pointwise reduction. -/
structure StepKernel where
  kernel : Kernel Expr Expr
  kernel_eq_stepMeasure : ∀ expression, kernel expression = stepMeasure expression
  kernel_sfinite : IsSFiniteKernel kernel
  mass_le_one : ∀ expression, kernel expression Set.univ ≤ 1
  sample_continuation_measurable : ∀ expression {site} fiber continuation,
    reduce expression = .sample site fiber continuation → Measurable continuation
  terminal_measurable : MeasurableSet terminalFloatSet
  terminal_value_measurable : Measurable terminalFloatValue

noncomputable def nStepMeasure (stepKernel : StepKernel) : Nat → Expr → Measure Expr
  | 0, expression => Measure.dirac expression
  | fuel + 1, expression => stepKernel.kernel ∘ₘ nStepMeasure stepKernel fuel expression

/-- Output that terminates for the first time at exactly this reduction depth. -/
noncomputable def exactOutputMeasure (stepKernel : StepKernel) :
    Nat → Expr → Measure ℝ
  | 0, .real .E value => Measure.dirac value
  | 0, _ => 0
  | fuel + 1, expression =>
      if expression.isValue then 0
      else (stepKernel.kernel expression).bind (exactOutputMeasure stepKernel fuel)

noncomputable def cumulativeOutputMeasure (stepKernel : StepKernel)
    (fuel : Nat) (program : Expr) : Measure ℝ :=
  ((nStepMeasure stepKernel fuel program).restrict terminalFloatSet).map terminalFloatValue

/-- The paper's big-step meaning is the pointwise limit of absorbing value mass. -/
noncomputable def bigStepMeasure (stepKernel : StepKernel)
    (program : Expr) : Measure ℝ :=
  ⨆ fuel, cumulativeOutputMeasure stepKernel fuel program

/--
Primitive-domain safety at a finite reduction depth. Canonical primitive fibers
have mass one exactly on their parameter domain and are zero off-domain.
Structural stuckness is outside this predicate; intrinsic typing supplies
structural progress.
-/
def PrimitiveDomainSafeAt : Nat → Expr → Prop
  | 0, _ => True
  | fuel + 1, expression =>
      if expression.isValue then True
      else match reduce expression with
      | .next next => PrimitiveDomainSafeAt fuel next
      | .sample _ fiber continuation =>
          fiber Set.univ = 1 ∧
            ∀ᵐ value ∂fiber, PrimitiveDomainSafeAt fuel (continuation value)
      | .stuck => True

/-- Every primitive call reached at a finite depth has valid parameters almost surely. -/
def PrimitiveDomainSafe (program : Expr) : Prop :=
  ∀ fuel, PrimitiveDomainSafeAt fuel program
