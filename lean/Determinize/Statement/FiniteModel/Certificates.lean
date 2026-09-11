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

end Determinize.Statement.FiniteModel
