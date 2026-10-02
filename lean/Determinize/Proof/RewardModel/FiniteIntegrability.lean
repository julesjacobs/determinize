import Determinize.Proof.FiniteModel.LinearBounds
import Determinize.Proof.FiniteModel.BoundaryExistence
import Determinize.Proof.RewardModel.Boundary
import Determinize.Proof.RewardModel.Integrability
import Determinize.Spec.RewardModel.Results

namespace Determinize.Proof.RewardModel
open MeasureTheory Spec.RewardModel FiniteModel

private theorem nonnegative_edge_solution (model : Model) (paths : Paths model.control)
    (valid : paths.Valid model.control) (terminal : Rat → Rat) (extra : Edge model.size → Rat)
    (terminal_nonnegative : ∀ b, 0 ≤ terminal b)
    (extra_nonnegative : ∀ i e, e ∈ model.edges i → 0 ≤ extra e) :
    ∃ v : Fin model.size → Rat, (∀ i, 0 ≤ v i) ∧ ∀ i, v i = match model.kind i with
      | .returned b => terminal b
      | .rejected => 0
      | .transient => ((model.edges i).map fun (e : Edge model.size) => e.probability * (v e.target + extra e)).sum := by
  let rhs := fun (i : Fin model.size) => match model.kind i with
    | .returned b => terminal b
    | .rejected => 0
    | .transient => ((model.edges i).map fun (e : Edge model.size) => e.probability * extra e).sum
  obtain ⟨v, eqs⟩ := linear_exists model.control paths valid rhs
  have nonnegative : ∀ i, 0 ≤ rhs i := by
    intro i
    dsimp [rhs]
    split
    · exact terminal_nonnegative _
    · rfl
    · apply List.sum_nonneg
      intro x hx
      obtain ⟨e, he, rfl⟩ := List.mem_map.mp hx
      exact mul_nonneg (model.nonnegative i e he) (extra_nonnegative i e he)
  refine ⟨v, linear_nonnegative model.control paths valid rhs v nonnegative eqs, ?_⟩
  intro i
  have h := eqs i
  cases kind : model.kind i <;>
    simpa [rhs, Model.control, kind, mul_add, List.sum_map_add, control_sum, add_comm] using h

/-- Moment bounds exist for every finite reward graph with paths to a boundary. -/
theorem momentBounds_exist (model : Model) (paths : Paths model.control)
    (valid : paths.Valid model.control) : Nonempty (MomentBounds model) := by
  obtain ⟨a, nonnegative_a, eqs_a⟩ := nonnegative_edge_solution model paths valid
    (fun b => |b|) (fun e => |e.reward|) abs_nonneg (fun _ _ _ => abs_nonneg _)
  obtain ⟨b, nonnegative_b, eqs_b⟩ := nonnegative_edge_solution model paths valid
    (fun b => b^2) (fun e => 2*|e.reward| * a e.target + e.reward^2) sq_nonneg
    (fun _ e _ => add_nonneg (mul_nonneg (mul_nonneg (by norm_num) (abs_nonneg _))
      (nonnegative_a e.target)) (sq_nonneg _))
  refine ⟨⟨a, b, nonnegative_a, nonnegative_b, ?_, ?_⟩⟩
  · intro i
    exact (eqs_a i).symm.le
  · intro i
    apply le_of_eq
    cases kind : model.kind i <;> simpa [kind, ← add_assoc] using (eqs_b i).symm

/-- Finiteness alone suffices: no moment bounds or termination assumptions are required. -/
theorem finite_integrable (model : Model) : model.IntegrableMoments := by
  obtain ⟨dead, closed, paths, valid⟩ := boundary_paths_exist model.control
  obtain ⟨bounds⟩ := momentBounds_exist (cut model dead) paths valid
  intro i
  have h := outputAt_integrable (cut model dead) bounds i
  rwa [cut_outputAt model dead closed] at h

end Determinize.Proof.RewardModel
