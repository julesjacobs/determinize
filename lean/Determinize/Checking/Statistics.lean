import Determinize.Checking.Result
import Determinize.Proof.FiniteModel.Statistics

namespace Determinize.Checking
open Spec.FiniteModel Proof.FiniteModel MeasureTheory ProbabilityTheory

/-- Check external values against the original model, including the divergent boundary. -/
def checkStatistics (model : Model) (certificate : MomentCertificate model) : Bool :=
  decide (ClosedDivergence model certificate.dead) &&
    decide (∀ state : Fin model.size, (survivalVector (cut model certificate.dead) certificate.horizon)[state] < 1) &&
    decide (∀ moment, (certificate.result model moment).Equations (certificate.model model moment))

theorem checkStatistics_valid (model : Model) (certificate : MomentCertificate model) :
    checkStatistics model certificate = true ↔ certificate.Valid model := by
  simp only [checkStatistics, Bool.and_eq_true, decide_eq_true_eq, survivalVector_correct,
    MomentCertificate.Valid, and_assoc]

theorem checked_statistics {source : Core} {subject : Subject}
    (checked : CheckedModel source subject) (certificate : MomentCertificate checked.model)
    (accepted : checkStatistics checked.model certificate = true) :
    (certificate.statistics checked.model).Matches (Spec.Paper.bigStepMeasure (subject.program source)) := by
  rw [← checked.correct.2]
  exact momentCertificate_sound checked.model certificate ((checkStatistics_valid _ _).mp accepted)

theorem checked_conditionalVariance {source : Core} {subject : Subject}
    (checked : CheckedModel source subject) (certificate : MomentCertificate checked.model)
    (accepted : checkStatistics checked.model certificate = true)
    (positive : 0 < (certificate.statistics checked.model).returnMass) :
    variance id (((Spec.Paper.bigStepMeasure (subject.program source)) Set.univ)⁻¹ •
      Spec.Paper.bigStepMeasure (subject.program source)) =
      (((certificate.statistics checked.model).secondMoment / (certificate.statistics checked.model).returnMass -
        ((certificate.statistics checked.model).firstMoment / (certificate.statistics checked.model).returnMass) ^ 2 : Rat) : ℝ) := by
  rw [← checked.correct.2]
  exact statistics_conditional_variance checked.model _
    (momentCertificate_sound _ _ ((checkStatistics_valid _ _).mp accepted)) positive

end Determinize.Checking
