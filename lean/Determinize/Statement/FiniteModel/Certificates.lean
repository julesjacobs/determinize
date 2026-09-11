import Determinize.Statement.FiniteModel.Model

namespace Determinize.Statement.FiniteModel
open MeasureTheory Paper

/-- Selects the core program whose output law a model certificate must represent. -/
inductive Subject where
  | source
  | determinized
deriving DecidableEq, Repr

def Subject.program (subject : Subject) (source : Expr Rat) : Expr :=
  let realSource := source.mapLiteral (fun q : Rat => (q : ℝ))
  match subject with
  | .source => realSource
  | .determinized => realSource.determinize

/-- Candidate state values and a uniform finite-step absorption bound.
The certificate is data: these fields do not carry validity proofs. -/
structure ResultCertificate (model : Model) where
  values : Fin model.size → Rat
  horizon : Nat
  escape : Rat

/-- The value equations alone need not determine the expected reward. -/
def ResultCertificate.Equations (model : Model) (certificate : ResultCertificate model) : Prop :=
  ∀ state, certificate.values state = match model.kind state with
    | .returned reward => reward
    | .rejected => 0
    | .transient => ∑ next, model.transition state next * certificate.values next

/-- From every model state, at least `escape` probability reaches a returned or
rejected state within `horizon` transitions. -/
def ResultCertificate.Absorption (model : Model) (certificate : ResultCertificate model) : Prop :=
  0 < certificate.horizon ∧ 0 < certificate.escape ∧ certificate.escape ≤ 1 ∧
    ∀ state, model.survivalWithin certificate.horizon state ≤ 1 - certificate.escape

def ResultCertificate.Valid (model : Model) (certificate : ResultCertificate model) : Prop :=
  certificate.Equations model ∧ certificate.Absorption model

instance (model : Model) (certificate : ResultCertificate model) :
    Decidable (certificate.Equations model) := inferInstanceAs (Decidable (∀ _, _))
instance (model : Model) (certificate : ResultCertificate model) :
    Decidable (certificate.Absorption model) := inferInstanceAs (Decidable (_ ∧ _ ∧ _ ∧ ∀ _, _))
instance (model : Model) (certificate : ResultCertificate model) :
    Decidable (certificate.Valid model) := inferInstanceAs (Decidable (_ ∧ _))

/-- Obligation for a future model checker. Acceptance must establish equality of
output laws, including successor coverage and unbounded execution. -/
def ModelCheckerSound {Certificate : Type}
    (check : Expr Rat → Subject → Model → Certificate → Bool) : Prop :=
  ∀ source subject model certificate, check source subject model certificate = true →
    model.Matches (subject.program source)

/-- Obligation for a future result checker: accepted algebraic data must satisfy the
absorption condition and denote the model's actual expected terminal reward. -/
def ResultCheckerSound
    (check : (model : Model) → ResultCertificate model → Bool) : Prop :=
  ∀ model certificate, check model certificate = true →
    certificate.Valid model ∧ model.HasExpectedReward (certificate.values model.initial)

/-- The end-to-end claim for the explicitly selected program. The checker soundness
premises are obligations, not assumptions supplied by Storm. -/
def EndToEnd {Certificate : Type}
    (checkModel : Expr Rat → Subject → Model → Certificate → Bool)
    (checkResult : (model : Model) → ResultCertificate model → Bool) : Prop :=
  ModelCheckerSound checkModel → ResultCheckerSound checkResult →
  ∀ source subject model modelCertificate resultCertificate,
    checkModel source subject model modelCertificate = true →
    checkResult model resultCertificate = true →
    Integrable id (bigStepMeasure (subject.program source)) ∧
      (∫ value : ℝ, value ∂bigStepMeasure (subject.program source)) =
        (resultCertificate.values model.initial : ℝ)

/-- A model of the determinized program also answers the source query when the
existing determinization theorem's typing, source-form, safety, and integrability
premises are available. None of these premises are discharged by Storm. -/
def SourceEndToEnd {Certificate : Type}
    (checkModel : Expr Rat → Subject → Model → Certificate → Bool)
    (checkResult : (model : Model) → ResultCertificate model → Bool) : Prop :=
  ModelCheckerSound checkModel → ResultCheckerSound checkResult →
  ∀ source model modelCertificate resultCertificate,
    Typed [] (Subject.source.program source) (.float .E) →
    (Subject.source.program source).sourceForm = true →
    DoesNotGetStuck (Subject.source.program source) →
    Integrable id (bigStepMeasure (Subject.source.program source)) →
    checkModel source .determinized model modelCertificate = true →
    checkResult model resultCertificate = true →
    (∫ value : ℝ, value ∂bigStepMeasure (Subject.source.program source)) =
      (resultCertificate.values model.initial : ℝ)

end Determinize.Statement.FiniteModel
