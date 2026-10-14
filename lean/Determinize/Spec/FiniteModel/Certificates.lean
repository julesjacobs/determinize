import Determinize.Spec.FiniteModel.Model

/-!
# The program a finite-model certificate is about
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

end Determinize.Spec.FiniteModel
