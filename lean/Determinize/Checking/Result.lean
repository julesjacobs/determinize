import Determinize.Proof.FiniteModel.Result
import Determinize.Proof.FiniteModel.Contracts
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

theorem checkResult_sound : ResultCheckerSound checkResult := by
  intro model certificate accepted
  have valid := (checkResult_valid model certificate).mp accepted
  exact ⟨valid, Proof.FiniteModel.resultCertificate_sound model certificate valid⟩

/-- Both model replay and result checking are instantiated by proved checkers. -/
theorem checked_expectedReward (source : Core) (subject : Subject) (model : Model)
    (candidate : Finite.Candidate) (certificate : ResultCertificate model)
    (modelAccepted : checkModelCertificate source subject model candidate = true)
    (resultAccepted : checkResult model certificate = true) :
    MeasureTheory.Integrable id (Statement.Paper.bigStepMeasure (subject.program source)) ∧
      (∫ value : ℝ, value ∂Statement.Paper.bigStepMeasure (subject.program source)) =
        (certificate.values model.initial : ℝ) :=
  Proof.FiniteModel.endToEnd checkModelCertificate checkResult
    checkModelCertificate_sound checkResult_sound source subject model candidate certificate
    modelAccepted resultAccepted

end Determinize.Checking
