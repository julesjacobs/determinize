import Determinize.Proof.RewardModel.Control
import Determinize.Proof.RewardModel.Measure
import Determinize.Proof.FiniteModel.Boundary

namespace Determinize.Proof.RewardModel
open MeasureTheory Spec.RewardModel

abbrev cut (model : Model) (dead : Fin model.size → Bool) : Model :=
  {model with kind := fun state => if dead state then .rejected else model.kind state}

private theorem dead_outputWithin (model : Model) (dead : Fin model.size → Bool)
    (closed : FiniteModel.ClosedDivergence model.control dead) (n : Nat) (i : Fin model.size)
    (isDead : dead i = true) : model.outputWithin n i = 0 := by
  have transient := (closed i isDead).1
  change model.kind i = .transient at transient
  induction n generalizing i with
  | zero => simp [Model.outputWithin, transient]
  | succ n ih =>
    simp only [Model.outputWithin, transient]
    apply List.sum_eq_zero
    intro x hx
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp hx
    by_cases positive : 0 < e.probability
    · have target := (closed i isDead).2 e.target (positive.trans_le (edge_le_controlWeight model i e he))
      rw [ih e.target target (by exact (closed e.target target).1), shift_zero_measure, smul_zero]
    · have zero : e.probability = 0 := le_antisymm (le_of_not_gt positive) (model.nonnegative i e he)
      simp [zero]

theorem cut_outputWithin (model : Model) (dead : Fin model.size → Bool)
    (closed : FiniteModel.ClosedDivergence model.control dead) (n : Nat) (i : Fin model.size) :
    (cut model dead).outputWithin n i = model.outputWithin n i := by
  induction n generalizing i with
  | zero =>
      by_cases isDead : dead i = true
      · simp [Model.outputWithin, isDead, show model.kind i = .transient from (closed i isDead).1]
      · simp [Model.outputWithin, isDead]
  | succ n ih =>
      by_cases isDead : dead i = true
      · rw [dead_outputWithin model dead closed _ i isDead]
        simp [Model.outputWithin, isDead]
      · simp only [Model.outputWithin, cut, isDead, Bool.false_eq_true, ↓reduceIte]
        cases model.kind i with
        | returned b => rfl
        | rejected => rfl
        | transient => simp only [ih]

theorem cut_outputAt (model : Model) (dead : Fin model.size → Bool)
    (closed : FiniteModel.ClosedDivergence model.control dead) (i : Fin model.size) :
    (cut model dead).outputAt i = model.outputAt i := by
  simp only [Model.outputAt, cut_outputWithin model dead closed]

end Determinize.Proof.RewardModel
