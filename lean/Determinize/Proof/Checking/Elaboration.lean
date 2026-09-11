import Determinize.Checking.Elaboration
import Determinize.Proof.Checking.Typing
import Determinize.Theorems

namespace Determinize.Proof.Checking
open Determinize.Checking Statement.Paper MeasureTheory
open Determinize.Traces

theorem interpret_determinize (e : Core) :
    interpret e.determinize = (interpret e).determinize := by
  induction e <;> simp_all [interpret, Expr.mapLiteral, Expr.determinize]

theorem interpret_sourceForm (e : Core) :
    (interpret e).sourceForm = e.sourceForm := by
  induction e <;> simp_all [interpret, Expr.mapLiteral, Expr.sourceForm]

theorem certified_alignment {input modes} (p : Certified input modes) :
    eraseAnnotations p.source = eraseAnnotations input := p.aligned

theorem certified_trace_soundness {input modes} (p : Certified input modes)
    (m : Mode) (hTy : p.ty = .float m)
    (safe : DoesNotGetStuck (interpret p.source)) :
    DoesNotGetStuck (interpret p.source.determinize) ∧
      traceAndOutputLaw (interpret p.source) =
        traceThenOutput (traceLaw (interpret p.source)) (outputGivenTrace (interpret p.source)) ∧
      traceAndOutputLaw (interpret p.source.determinize) =
        traceThenOutput (traceLaw (interpret p.source))
          (outputGivenTrace (interpret p.source.determinize)) ∧
      ∀ᵐ trace ∂traceLaw (interpret p.source),
        Integrable id (outputGivenTrace (interpret p.source) trace) ∧
        outputGivenTrace (interpret p.source.determinize) trace =
          Measure.dirac (∫ value : ℝ, value ∂outputGivenTrace (interpret p.source) trace) := by
  rw [interpret_determinize]
  exact Determinize.Theorems.traceSoundness m (interpret p.source)
    (hTy ▸ p.typed) ((interpret_sourceForm _).trans p.sourceOnly) safe

theorem certified_expectation {input modes} (p : Certified input modes)
    (m : Mode) (hTy : p.ty = .float m)
    (safe : DoesNotGetStuck (interpret p.source))
    (integrable : Integrable id (bigStepMeasure (interpret p.source))) :
    DoesNotGetStuck (interpret p.source.determinize) ∧
      Integrable id (bigStepMeasure (interpret p.source.determinize)) ∧
      (∫ value : ℝ, value ∂bigStepMeasure (interpret p.source)) =
        ∫ value : ℝ, value ∂bigStepMeasure (interpret p.source.determinize) := by
  rw [interpret_determinize]
  exact Determinize.Theorems.expectationPreservation m (interpret p.source)
    (hTy ▸ p.typed) ((interpret_sourceForm _).trans p.sourceOnly) safe integrable

end Determinize.Proof.Checking
