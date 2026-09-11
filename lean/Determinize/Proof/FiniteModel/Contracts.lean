import Determinize.Statement.FiniteModel.Certificates
import Determinize.Theorems

namespace Determinize.Proof.FiniteModel
open Statement.FiniteModel MeasureTheory

/-- Composes the two future checker correctness proofs; it does not implement either checker. -/
theorem endToEnd {Certificate : Type}
    (checkModel : Statement.Paper.Expr Rat → Subject → Model → Certificate → Bool)
    (checkResult : (model : Model) → ResultCertificate model → Bool) :
    EndToEnd checkModel checkResult := by
  intro modelSound resultSound source subject model modelCertificate resultCertificate
    modelAccepted resultAccepted
  have representation := modelSound source subject model modelCertificate modelAccepted
  obtain ⟨_, integrable, answer⟩ := resultSound model resultCertificate resultAccepted
  have sameLaw := representation.2
  rw [← sameLaw]
  exact ⟨integrable, answer⟩

theorem sourceEndToEnd {Certificate : Type}
    (checkModel : Statement.Paper.Expr Rat → Subject → Model → Certificate → Bool)
    (checkResult : (model : Model) → ResultCertificate model → Bool) :
    SourceEndToEnd checkModel checkResult := by
  intro modelSound resultSound source model modelCertificate resultCertificate
    typed sourceForm safe integrable modelAccepted resultAccepted
  have target := endToEnd checkModel checkResult modelSound resultSound source .determinized
    model modelCertificate resultCertificate modelAccepted resultAccepted
  have preservation := Theorems.expectationPreservation (Subject.source.program source)
    typed sourceForm safe integrable
  exact preservation.2.2.trans target.2

end Determinize.Proof.FiniteModel
