import Determinize.Checking.Elaboration
import Determinize.Proof.Checking.Typing
import Determinize.Theorems

namespace Determinize.Proof.Checking
open Determinize.Checking Spec.Paper MeasureTheory
open Determinize.Spec.Traces

theorem interpret_determinize (e : Core) :
    interpret e.determinize = (interpret e).determinize := by
  induction e <;> simp_all [interpret, Expr.mapLiteral, Expr.determinize]

theorem interpret_sourceForm (e : Core) :
    (interpret e).sourceForm = e.sourceForm := by
  induction e <;> simp_all [interpret, Expr.mapLiteral, Expr.sourceForm]

theorem certified_alignment {input} (p : Certified input) :
    input.matches p.source = true := p.aligned

theorem certified_trace_conditional_law {input} (p : Certified input)
    (m : Affinity) (hTy : p.ty = .float m)
    (safe : PrimitiveDomainSafe (interpret p.source)) :
    PrimitiveDomainSafe (interpret p.source.determinize) ∧
      traceLaw (interpret p.source.determinize) = traceLaw (interpret p.source) ∧
      ∀ᵐ trace ∂traceLaw (interpret p.source),
        Integrable id ((traceAndOutputLaw (interpret p.source)).condKernel trace) ∧
        (traceAndOutputLaw (interpret p.source.determinize)).condKernel trace =
          Measure.dirac (∫ value : ℝ, value ∂(traceAndOutputLaw (interpret p.source)).condKernel trace) := by
  rw [interpret_determinize]
  have typed : Typed [] (interpret p.source) (.float .E) := by
    have h : Typed [] (interpret p.source) (.float m) := hTy ▸ p.typed
    cases m with
    | E => exact h
    | G => exact .sub h .general
  exact Determinize.Theorems.traceConditionalLaw (interpret p.source)
    typed ((interpret_sourceForm _).trans p.sourceOnly) safe

theorem certified_expectation {input} (p : Certified input)
    (m : Affinity) (hTy : p.ty = .float m)
    (safe : PrimitiveDomainSafe (interpret p.source))
    (integrable : Integrable id (bigStepMeasure (interpret p.source))) :
    PrimitiveDomainSafe (interpret p.source.determinize) ∧
      Integrable id (bigStepMeasure (interpret p.source.determinize)) ∧
      (∫ value : ℝ, value ∂bigStepMeasure (interpret p.source)) =
        ∫ value : ℝ, value ∂bigStepMeasure (interpret p.source.determinize) := by
  rw [interpret_determinize]
  have typed : Typed [] (interpret p.source) (.float .E) := by
    have h : Typed [] (interpret p.source) (.float m) := hTy ▸ p.typed
    cases m with
    | E => exact h
    | G => exact .sub h .general
  exact Determinize.Theorems.expectationPreservation (interpret p.source)
    typed ((interpret_sourceForm _).trans p.sourceOnly) safe integrable

end Determinize.Proof.Checking
