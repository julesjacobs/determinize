import Determinize.Spec.RewardModel.Control
import Determinize.Finite.Statistics
import Determinize.Proof.RewardModel.Boundary
import Determinize.Proof.RewardModel.Certificates

namespace Determinize.Finite.Reward
open Proof.FiniteModel

def equations (model : Spec.FiniteModel.Model) (rhs values : Fin model.size → Rat) : Prop :=
  ∀ i, values i = rhs i + if model.kind i = .transient then
    ∑ j, model.transition i j * values j else 0

instance (model : Spec.FiniteModel.Model) (rhs values : Fin model.size → Rat) :
    Decidable (equations model rhs values) := inferInstanceAs (Decidable (∀ _, _))

private def solveRhs (model : Spec.FiniteModel.Model) (rhs : Fin model.size → Rat) :
    Except String {values : Fin model.size → Rat // equations model rhs values} := do
  let A := fun i j : Fin model.size =>
    let identity : Rat := if i = j then 1 else 0
    if model.kind i = .transient then identity - model.transition i j else identity
  let some solution := Proof.LinearAlgebra.solve model.size A rhs
    | throw "singular additive reward equations"
  return ⟨solution.val, by
    intro i
    have h := solution.property i
    by_cases transient : model.kind i = .transient
    · simp [A, transient, sub_mul, Finset.sum_sub_distrib] at h
      simp only [transient, ↓reduceIte]
      exact (sub_eq_iff_eq_add.mp h)
    · simpa [A, transient] using h⟩

def firstRhs (model : Spec.RewardModel.Model) (dead : Fin model.size → Bool)
    (mass : Fin model.size → Rat) (i : Fin model.size) : Rat :=
  if dead i then 0 else match model.kind i with
  | .returned b => b
  | .rejected => 0
  | .transient => ((model.edges i).map fun e => e.probability * e.reward * mass e.target).sum

def secondRhs (model : Spec.RewardModel.Model) (dead : Fin model.size → Bool)
    (mass first : Fin model.size → Rat) (i : Fin model.size) : Rat :=
  if dead i then 0 else match model.kind i with
  | .returned b => b*b
  | .rejected => 0
  | .transient => ((model.edges i).map fun e =>
      e.probability * (2*e.reward*first e.target + e.reward*e.reward*mass e.target)).sum

/-- Checked linear equations; source correspondence and integral interpretation are separate. -/
structure Solution (model : Spec.RewardModel.Model) where
  boundary : Boundary model.control
  paths : Paths (cut model.control boundary.dead)
  pathsValid : paths.Valid (cut model.control boundary.dead)
  mass : Fin model.size → Rat
  massValid : (⟨mass, 0⟩ : Spec.FiniteModel.ResultCertificate
    (rewards (cut model.control boundary.dead) Moment.mass.rational)).Equations _
  first : Fin model.size → Rat
  firstValid : equations (cut model.control boundary.dead) (firstRhs model boundary.dead mass) first
  second : Fin model.size → Rat
  secondValid : equations (cut model.control boundary.dead) (secondRhs model boundary.dead mass first) second
  rejection : Fin model.size → Rat
  rejectionValid : (⟨rejection, 0⟩ : Spec.FiniteModel.ResultCertificate
    (rejectionQuery model.control boundary.dead)).Equations _

theorem Solution.momentsValid {model : Spec.RewardModel.Model} (solution : Solution model) : Proof.RewardModel.MomentEquations (Proof.RewardModel.cut model solution.boundary.dead)
    (fun moment => match moment with | .mass => solution.mass | .first => solution.first | .second => solution.second) := by
  intro moment i
  have mass := solution.massValid i
  have first := solution.firstValid i
  have second := solution.secondValid i
  cases moment <;> cases dead : solution.boundary.dead i <;> cases kind : model.kind i <;>
    simp_all [Proof.RewardModel.cut, cut, rewards, Spec.RewardModel.Model.control,
      firstRhs, secondRhs, Proof.RewardModel.translatedValue,
      Moment.rational, mul_add, List.sum_map_add, ← Proof.RewardModel.control_sum,
      pow_two, mul_assoc, add_comm, add_assoc]

def solve (model : Spec.RewardModel.Model) (limits : SolveLimits := {}) : Except String (Solution model) := do
  if model.size > limits.maxStates then
    throw s!"exact solver state limit exceeded ({model.size} > {limits.maxStates})"
  let boundary ← analyze model.control
  let stopped := cut model.control boundary.dead
  let paths ← findPaths stopped boundary.rank
  let mass ← solveValues (rewards stopped Moment.mass.rational) limits
  let first ← solveRhs stopped (firstRhs model boundary.dead mass.val)
  let second ← solveRhs stopped (secondRhs model boundary.dead mass.val first.val)
  let rejection ← solveValues (rejectionQuery model.control boundary.dead) limits
  return ⟨boundary, paths.val, paths.property, mass.val, mass.property,
    first.val, first.property, second.val, second.property, rejection.val, rejection.property⟩

def Solution.statistics {model : Spec.RewardModel.Model} (solution : Solution model) :
    Spec.FiniteModel.OutputStatistics :=
  ⟨solution.mass model.initial, solution.first model.initial, solution.second model.initial⟩

end Determinize.Finite.Reward
