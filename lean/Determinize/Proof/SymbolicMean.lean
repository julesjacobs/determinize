import Determinize.Proof.Symbolic

namespace Determinize.Proof.Paper.Symbolic.AffineExpr

open MeasureTheory ProbabilityTheory Determinize.Spec.Paper

open scoped Classical

noncomputable section

/-- The evaluated parameters of a symbolic primitive call. -/
def meanParams (op : Op) (affine : List (Affine n)) (general : List ℝ)
    (environment : Env n) : Params op :=
  (fun i => (affine.getD i.1 0).eval environment, fun i => general.getD i.1 0)

theorem eval_meanAffine (op : Op) (affine : List (Affine n)) (general : List ℝ)
    (environment : Env n) :
    (meanAffine op affine general).eval environment =
      meanValue op (meanParams op affine general environment) := by
  exact Affine.eval_primitiveMean op _ _ environment

theorem primitiveFiber_mean_formula (op : Op) (affine : List (Affine n)) (general : List ℝ)
    (ha : affine.length = affineArity op) (hg : general.length = generalArity op)
    (environment : Env n) :
    primitiveFiber .mean op (affine.map (Affine.eval · environment)) general =
      if domain op (meanParams op affine general environment) then
        Measure.dirac ((meanAffine op affine general).eval environment) else 0 := by
  have ha' : (affine.map (Affine.eval · environment)).length = affineArity op := by simpa using ha
  unfold primitiveFiber parseParams
  rw [dif_pos ha', dif_pos hg]
  simp only
  have paramsEq : ((fun i : Fin (affineArity op) =>
      (affine.map (Affine.eval · environment))[i.1]'(ha'.symm ▸ i.2)),
      fun i : Fin (generalArity op) => general[i.1]'(hg.symm ▸ i.2)) =
      meanParams op affine general environment := by
    apply Prod.ext <;> funext i
    · simp [meanParams, List.getD_eq_getElem?_getD, ha]
    · simp [meanParams, List.getD_eq_getElem?_getD, hg]
  rw [paramsEq, eval_meanAffine]

theorem primitiveFiber_mean_mass (op : Op) (affine : List (Affine n)) (general : List ℝ)
    (ha : affine.length = affineArity op) (hg : general.length = generalArity op)
    (environment : Env n) :
    primitiveFiber .mean op (affine.map (Affine.eval · environment)) general Set.univ = 1 ↔
      domain op (meanParams op affine general environment) := by
  rw [primitiveFiber_mean_formula op affine general ha hg environment]
  split_ifs <;> simp_all

theorem mean_continuation_realize
    (typed : SymbolicAction.WellTyped ty (.mean op affine general continuation : SymbolicAction n))
    (environment : Env n) :
    (continuation (meanAffine op affine general)).realize environment =
      (continuation ((meanAffine op affine general).eval environment, 0)).realize environment :=
  (SymbolicAction.wellTyped_mean_iff.mp typed).2.2.2 _ _

end
end Determinize.Proof.Paper.Symbolic.AffineExpr
