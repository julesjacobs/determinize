import Determinize.Proof.FiniteModel.Result
import Determinize.Theorems
import Determinize.Checking.FiniteModel

namespace Determinize.Checking
open Statement.FiniteModel

/-- Materialize each horizon once during evaluation. -/
def survivalVector (model : Model) : Nat → Vector Rat model.size
  | 0 => Vector.ofFn fun state => if model.kind state = .transient then 1 else 0
  | n + 1 =>
      let previous := survivalVector model n
      Vector.ofFn fun state => if model.kind state = .transient then
        ∑ next, model.transition state next * previous[next] else 0

theorem survivalVector_correct (model : Model) (n : Nat) (state : Fin model.size) :
    (survivalVector model n)[state] = model.survivalWithin n state := by
  induction n generalizing state with
  | zero => simp [survivalVector, Model.survivalWithin]
  | succ n ih => simp [survivalVector, Model.survivalWithin, ih]

def checkResult (model : Model) (certificate : ResultCertificate model) : Bool :=
  decide (certificate.Equations model) &&
  decide (0 < certificate.horizon ∧ 0 < certificate.escape ∧ certificate.escape ≤ 1) &&
  let survival := survivalVector model certificate.horizon
  decide (∀ state : Fin model.size, survival[state] ≤ 1 - certificate.escape)

theorem checkResult_valid (model : Model) (certificate : ResultCertificate model) :
    checkResult model certificate = true ↔ certificate.Valid model := by
  simp only [checkResult, Bool.and_eq_true, decide_eq_true_eq,
    survivalVector_correct, ResultCertificate.Valid, ResultCertificate.Absorption]
  tauto

theorem checkResult_sound (model : Model) (certificate : ResultCertificate model)
    (accepted : checkResult model certificate = true) :
    certificate.Valid model ∧ model.HasExpectedReward (certificate.values model.initial) := by
  have valid := (checkResult_valid model certificate).mp accepted
  exact ⟨valid, Proof.FiniteModel.resultCertificate_sound model certificate valid⟩

/-- The extracted model and checked result certify the caller-selected program law. -/
theorem checked_expectedReward {source : Core} {subject : Subject}
    (checked : CheckedModel source subject) (certificate : ResultCertificate checked.model)
    (accepted : checkResult checked.model certificate = true) :
    MeasureTheory.Integrable id (Statement.Paper.bigStepMeasure (subject.program source)) ∧
      (∫ value : ℝ, value ∂Statement.Paper.bigStepMeasure (subject.program source)) =
        (certificate.values checked.model.initial : ℝ) := by
  rw [← checked.correct.2]
  exact (checkResult_sound checked.model certificate accepted).2

/-- Transfer a checked determinized answer under the source theorem's premises. -/
theorem checked_sourceExpectedReward {source : Core}
    (checked : CheckedModel source .determinized)
    (certificate : ResultCertificate checked.model)
    (typed : Statement.Paper.Typed [] (Subject.source.program source) (.float .E))
    (sourceForm : (Subject.source.program source).sourceForm = true)
    (safe : Statement.Paper.DoesNotGetStuck (Subject.source.program source))
    (integrable : MeasureTheory.Integrable id
      (Statement.Paper.bigStepMeasure (Subject.source.program source)))
    (accepted : checkResult checked.model certificate = true) :
    (∫ value : ℝ, value ∂Statement.Paper.bigStepMeasure (Subject.source.program source)) =
      (certificate.values checked.model.initial : ℝ) := by
  have preservation := Theorems.expectationPreservation (Subject.source.program source)
    typed sourceForm safe integrable
  exact preservation.2.2.trans (checked_expectedReward checked certificate accepted).2

end Determinize.Checking
