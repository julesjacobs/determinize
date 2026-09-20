import Determinize.Proof.RewardModel.Translation

namespace Determinize.Proof.RewardModel
open Determinize.Finite Determinize.Finite.Reward Determinize.Proof.FiniteModel
open MeasureTheory Spec.RewardModel

private def residual : State → State
  | .eval e env stack => .eval e env (splitStack stack).1
  | .deliver v stack => .deliver v (splitStack stack).1
  | .rejected => .rejected

private def offsets : State → List Rat
  | .eval _ _ stack | .deliver _ stack => (splitStack stack).2
  | .rejected => []

private theorem state_decomposition (state : State) :
    pushStack (residual state) (additionStack (offsets state)) = state := by
  cases state <;> simp [residual, offsets, pushStack, additionStack, splitStack_reconstruct]

private theorem normalize_description (state : State) :
    Reward.normalize state = ((offsets state).sum,
      pushStack (residual state) (if offsets state = [] then [] else additionStack [0])) := by
  cases state <;> simp [Reward.normalize, normalizeStack, residual, offsets, pushStack,
    additionStack, Reward.guard]

/-- Extraction can only bring successful output forward in the finite horizon. -/
theorem normalize_output_upper (state : State) (n : Nat) :
    machineOutput n state ≤ shift (Reward.normalize state).1
      (machineOutput n (Reward.normalize state).2) := by
  rw [normalize_description]
  simp only
  by_cases empty : offsets state = []
  · have decomp := state_decomposition state
    simp only [empty, additionStack, List.map_nil] at decomp
    simp only [empty, ↓reduceIte, List.sum_nil, shift_zero, decomp]
    exact le_rfl
  · simp only [empty, ↓reduceIte]
    have h := addition_guard_upper (offsets state) empty n (residual state)
    simpa only [state_decomposition] using h

/-- The erased context always finishes after finitely many ordinary CEK transitions. -/
theorem normalize_output_lower (state : State) (n : Nat) :
    ∃ horizon, shift (Reward.normalize state).1
      (machineOutput n (Reward.normalize state).2) ≤ machineOutput horizon state := by
  refine ⟨n + (offsets state).length, ?_⟩
  rw [normalize_description]
  simp only
  by_cases empty : offsets state = []
  · have decomp := state_decomposition state
    simp only [empty, additionStack, List.map_nil] at decomp
    simp only [empty, ↓reduceIte, List.sum_nil, shift_zero, decomp, List.length_nil, Nat.add_zero]
    exact le_rfl
  · simp only [empty, ↓reduceIte]
    have upper := addition_output_upper [0] n (residual state)
    simp only [List.sum_cons, List.sum_nil, add_zero, shift_zero] at upper
    exact (shift_mono _ upper).trans (by
      simpa only [state_decomposition] using addition_output_lower (offsets state) n (residual state))

end Determinize.Proof.RewardModel
