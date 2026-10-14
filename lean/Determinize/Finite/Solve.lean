import Determinize.Proof.FiniteModel.Queries
import Determinize.Proof.LinearAlgebra.Solve

namespace Determinize.Finite
open Spec.FiniteModel

structure SolveLimits where
  maxStates : Nat := 256

/-- The equations are established by elimination, without checking a proposed answer. -/
def solveValues (model : Model) (limits : SolveLimits := {}) :
    Except String
      {values : Fin model.size → Rat // Proof.FiniteModel.ValueEquations model values} := do
  if model.size > limits.maxStates then
    throw s!"exact solver state limit exceeded ({model.size} > {limits.maxStates})"
  let A := fun state next : Fin model.size ↦
    let identity : Rat := if state = next then 1 else 0
    if model.kind state = StateKind.transient then
      identity - model.transition state next
    else identity
  let b := fun state ↦ match model.kind state with | .returned reward => reward | _ => 0
  let some solution := Proof.LinearAlgebra.solve model.size A b
    | throw "singular value equations; no absorption certificate"
  have equations : ∀ state, solution.val state = match model.kind state with
      | .returned reward => reward
      | .rejected => 0
      | .transient => ∑ next, model.transition state next * solution.val next := by
    intro state
    have h := solution.property state
    cases kind : model.kind state <;>
      simp only [kind, ↓reduceIte, sub_mul, ite_mul, one_mul, zero_mul,
        Finset.sum_sub_distrib, Finset.sum_ite_eq, Finset.mem_univ, A, b, reduceCtorEq] at h ⊢
    all_goals first | exact sub_eq_zero.mp h | exact h
  return ⟨solution.val, equations⟩

end Determinize.Finite
