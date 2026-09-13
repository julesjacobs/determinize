import Determinize.Proof.FiniteModel.Result
import Determinize.Proof.LinearAlgebra.Solve

namespace Determinize.Finite
open Spec.FiniteModel

structure SolveLimits where
  maxStates : Nat := 256

private def absorption (model : Model) (remaining : Nat) (horizon : Nat)
    (survival : Vector Rat model.size)
    (correct : ∀ state, survival[state] = model.survivalWithin horizon state) :
    Option {n : Nat // ∀ state, model.survivalWithin n state < 1} :=
  if h : ∀ state : Fin model.size, survival[state] < 1 then
    some ⟨horizon, by simpa only [correct] using h⟩
  else match remaining with
    | 0 => none
    | remaining + 1 =>
      let next := Vector.ofFn fun state => if model.kind state = StateKind.transient then
        ∑ successor, model.transition state successor * survival[successor] else 0
      absorption model remaining (horizon + 1) next (by
        intro state
        simp [next, Model.survivalWithin, correct])

/-- The equations are established by elimination, without checking a proposed answer. -/
def solveCertified (model : Model) (limits : SolveLimits := {}) :
    Except String {certificate : ResultCertificate model // certificate.Valid model} := do
  if model.size > limits.maxStates then
    throw s!"exact solver state limit exceeded ({model.size} > {limits.maxStates})"
  let A := fun state next : Fin model.size =>
    let identity : Rat := if state = next then 1 else 0
    if model.kind state = StateKind.transient then identity - model.transition state next else identity
  let b := fun state => match model.kind state with | .returned reward => reward | _ => 0
  let some solution := Proof.LinearAlgebra.solve model.size A b
    | throw "singular value equations; no absorption certificate"
  have equations : ∀ state, solution.val state = match model.kind state with
      | .returned reward => reward
      | .rejected => 0
      | .transient => ∑ next, model.transition state next * solution.val next := by
    intro state
    have h := solution.property state
    cases kind : model.kind state <;>
      simp [A, b, kind, sub_mul, Finset.sum_sub_distrib] at h ⊢
    all_goals first | exact sub_eq_zero.mp h | exact h
  let initial := Vector.ofFn fun state => if model.kind state = StateKind.transient then (1 : Rat) else 0
  let some bound := absorption model model.size 0 initial (by simp [initial, Model.survivalWithin])
    | throw "no uniform absorption bound: some state cannot reach a terminal state"
  return ⟨⟨solution.val, bound.val⟩, equations, bound.property⟩

def solve (model : Model) (limits : SolveLimits := {}) : Except String (ResultCertificate model) :=
  (solveCertified model limits).map Subtype.val

theorem solve_sound (model : Model) (limits : SolveLimits) (certificate : ResultCertificate model)
    (success : solve model limits = .ok certificate) : certificate.Valid model := by
  unfold solve at success
  cases h : solveCertified model limits with
  | error message => simp [h, Except.map] at success
  | ok result =>
      simp [h, Except.map] at success
      exact success ▸ result.property

theorem solve_expectedReward (model : Model) (limits : SolveLimits)
    (certificate : ResultCertificate model) (success : solve model limits = .ok certificate) :
    model.expectedReward = (certificate.values model.initial : ℝ) :=
  Proof.FiniteModel.resultCertificate_sound model certificate
    (solve_sound model limits certificate success)

end Determinize.Finite
