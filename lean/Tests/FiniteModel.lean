import Determinize.Statement.FiniteModel.Certificates
import Determinize.Statement.FiniteModel.Supported
import Determinize.Proof.FiniteModel.Model

namespace Determinize.Tests.FiniteModel
open Statement.FiniteModel MeasureTheory

example : supportedDraw .stochastic .uniform = false := rfl
example : supportedDraw .mean .uniform = true := rfl
example : supportedDraw .stochastic .poisson = false := rfl
example : supportedDraw .stochastic .bernoulli = true := rfl

abbrev fork : Model where
  size := 3
  initial := 0
  kind := fun i => if i = 0 then .transient else if i = 1 then .returned 3 else .rejected
  transition := fun i j => if i = 0 then (if j = 0 then 0 else 1/2)
    else if i = j then 1 else 0
  nonnegative := by decide +kernel
  normalized := by decide +kernel
  absorbing := by decide +kernel

def forkCertificate : ResultCertificate fork where
  values := fun i => if i.val = 0 then 3/2 else if i.val = 1 then 3 else 0
  horizon := 1

example : forkCertificate.Valid fork := by decide +kernel
example : fork.rewardWithin 0 fork.initial = 0 := by decide +kernel
example : fork.rewardWithin 1 fork.initial = 3/2 := by decide +kernel
example : fork.rewardWithin 20 fork.initial = 3/2 := by decide +kernel
example : fork.survivalWithin 1 fork.initial = 0 := by decide +kernel
example : ¬ ({forkCertificate with values := fun _ => 3} : ResultCertificate fork).Valid fork := by
  decide +kernel
example : ¬ ({forkCertificate with horizon := 0} : ResultCertificate fork).Valid fork := by
  decide +kernel

theorem fork_outputWithin (steps : Nat) :
    fork.outputWithin (steps + 1) fork.initial =
      ENNReal.ofReal (1/2 : ℝ) • Measure.dirac (3 : ℝ) := by
  change (∑ next : Fin 3, ENNReal.ofReal ((if next = 0 then 0 else 1/2 : Rat) : ℝ) •
    fork.outputWithin steps next) = _
  norm_num [Fin.sum_univ_succ]
  rw [Proof.FiniteModel.returned_outputWithin fork _ 3 rfl,
    Proof.FiniteModel.rejected_outputWithin fork _ rfl]
  simp

theorem fork_output : fork.outputMeasure =
    ENNReal.ofReal (1/2 : ℝ) • Measure.dirac (3 : ℝ) := by
  apply le_antisymm
  · apply iSup_le
    intro steps
    cases steps with
    | zero => exact bot_le
    | succ steps => rw [fork_outputWithin]
  · exact le_iSup_of_le 1 (by rw [fork_outputWithin])

example : fork.expectedReward = (3/2 : ℝ) := by
  unfold Model.expectedReward
  rw [fork_output]
  norm_num [integral_smul_measure]

def terminal (reward : Rat) : Model where
  size := 1
  initial := 0
  kind := fun _ => .returned reward
  transition := fun _ _ => 1
  nonnegative := by decide +kernel
  normalized := by decide +kernel
  absorbing := by
    intro i _ j
    have same : i = j := Subsingleton.elim _ _
    simp [same]

example (steps : Nat) : (terminal (-3)).rewardWithin steps (terminal (-3)).initial = -3 := by
  cases steps <;> rfl

example : (terminal (-3)).expectedReward = (-3 : ℝ) := by
  unfold Model.expectedReward
  rw [Proof.FiniteModel.returned_output (terminal (-3)) (-3) rfl]
  simp

abbrev loop : Model where
  size := 1
  initial := 0
  kind := fun _ => .transient
  transition := fun _ _ => 1
  nonnegative := by decide +kernel
  normalized := by decide +kernel
  absorbing := by decide +kernel

def loopCertificate (answer : Rat) : ResultCertificate loop where
  values := fun _ => answer
  horizon := 1

example (answer : Rat) : (loopCertificate answer).Equations loop := by
  intro state
  change answer = ∑ _ : Fin 1, (1 : Rat) * answer
  simp

theorem loop_survival (steps : Nat) (state : Fin loop.size) :
    loop.survivalWithin steps state = 1 := by
  induction steps generalizing state with
  | zero => rfl
  | succ steps ih =>
      change (∑ next : Fin loop.size, 1 * loop.survivalWithin steps next) = 1
      simp only [ih, mul_one]
      change (∑ _ : Fin 1, (1 : Rat)) = 1
      simp

example (certificate : ResultCertificate loop) : ¬ certificate.Valid loop := by
  rintro ⟨_, bound⟩
  have h := bound loop.initial
  rw [loop_survival] at h
  linarith

theorem loop_outputWithin (steps : Nat) (state : Fin loop.size) :
    loop.outputWithin steps state = 0 := by
  induction steps generalizing state with
  | zero => rfl
  | succ steps ih =>
      change (∑ next : Fin loop.size, ENNReal.ofReal ((1 : Rat) : ℝ) • loop.outputWithin steps next) = 0
      simp only [ih, smul_zero, Finset.sum_const_zero]

example : loop.outputMeasure = 0 := by
  simp [Model.outputMeasure, loop_outputWithin]

example : loop.expectedReward = 0 := by
  simp [Model.expectedReward, Model.outputMeasure, loop_outputWithin]


end Determinize.Tests.FiniteModel
