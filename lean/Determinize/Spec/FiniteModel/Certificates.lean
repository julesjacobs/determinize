import Determinize.Spec.FiniteModel.Model

/-!
# Certificates for the expected reward of a finite model

A certificate consists of candidate values and an absorption bound. `ResultCertificate.Valid`
says when it is correct; validity is decidable.
-/

namespace Determinize.Spec.FiniteModel
open MeasureTheory Paper

/-- Selects the core program whose output law a model certificate must represent. -/
inductive Subject where
  | source
  | determinized
deriving DecidableEq, Repr

/-- The paper program a certificate is about: the source with its rational literals read as
reals, or the determinization of that. -/
def Subject.program (subject : Subject) (source : Expr Rat) : Expr :=
  let realSource := source.map (fun q : Rat ↦ (q : ℝ)) id
  match subject with
  | .source => realSource
  | .determinized => realSource.determinize

/-- Candidate state values and a uniform finite-step absorption bound.
The certificate is data: these fields do not carry validity proofs. -/
structure ResultCertificate (model : Model) where
  /-- The candidate expected reward from each state. -/
  values : Fin model.size → Rat
  /-- A number of steps within which every state stops with positive probability. -/
  horizon : Nat

/-- The value equations alone need not determine the expected reward. -/
def ResultCertificate.Equations (model : Model) (certificate : ResultCertificate model) : Prop :=
  ∀ state, certificate.values state = match model.kind state with
    | .returned reward => reward
    | .rejected => 0
    | .transient => ∑ next, model.transition state next * certificate.values next

/-- Every state has a positive probability of reaching a returned or rejected state
within the supplied horizon. Finiteness makes the absorption bound uniform. -/
def ResultCertificate.Absorption (model : Model) (certificate : ResultCertificate model) : Prop :=
  ∀ state, model.survivalWithin certificate.horizon state < 1

/-- A certificate is valid when its values satisfy the value equations and its horizon is an
absorption bound. -/
def ResultCertificate.Valid (model : Model) (certificate : ResultCertificate model) : Prop :=
  certificate.Equations model ∧ certificate.Absorption model

instance (model : Model) (certificate : ResultCertificate model) :
    Decidable (certificate.Equations model) := inferInstanceAs (Decidable (∀ _, _))
instance (model : Model) (certificate : ResultCertificate model) :
    Decidable (certificate.Absorption model) := inferInstanceAs (Decidable (∀ _, _))
instance (model : Model) (certificate : ResultCertificate model) :
    Decidable (certificate.Valid model) := inferInstanceAs (Decidable (_ ∧ _))

end Determinize.Spec.FiniteModel
