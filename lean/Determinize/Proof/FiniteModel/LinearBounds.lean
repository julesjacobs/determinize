import Determinize.Proof.FiniteModel.Paths
import Mathlib.LinearAlgebra.FiniteDimensional.Basic

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel

/-- A finite chain with paths to a boundary has an invertible boundary-value operator. -/
theorem linear_exists (model : Model) (paths : Paths model) (valid : paths.Valid model)
    (rhs : Fin model.size → Rat) :
    ∃ v : Fin model.size → Rat, ∀ i, v i = rhs i +
      if model.kind i = .transient then ∑ j, model.transition i j * v j else 0 := by
  let L : (Fin model.size → Rat) →ₗ[Rat] (Fin model.size → Rat) :=
    { toFun := fun v i => v i - if model.kind i = .transient then
          ∑ j, model.transition i j * v j else 0
      map_add' := by
        intro v w
        funext i
        dsimp
        split_ifs <;> simp [mul_add, Finset.sum_add_distrib]
        ring
      map_smul' := by
        intro c v
        funext i
        dsimp
        split_ifs <;> simp [Finset.mul_sum, mul_sub, mul_left_comm] }
  have injective : Function.Injective L := by
    apply (LinearMap.ker_eq_bot).mp
    apply LinearMap.ker_eq_bot'.mpr
    intro v zero
    have eqs (i) : v i = if model.kind i = .transient then
        ∑ j, model.transition i j * v j else 0 := by
      have h := congrFun zero i
      exact sub_eq_zero.mp h
    have h := paths_unique model paths valid (fun i => (v i : ℝ)) (by
      intro i
      rw [eqs i]
      split_ifs <;> simp)
    funext i
    exact_mod_cast h i
  obtain ⟨v, hv⟩ := LinearMap.surjective_of_injective injective rhs
  refine ⟨v, fun i => ?_⟩
  have h := congrFun hv i
  change v i - _ = rhs i at h
  exact sub_eq_iff_eq_add.mp h

/-- The inverse boundary-value operator preserves nonnegativity. -/
theorem linear_nonnegative (model : Model) (paths : Paths model) (valid : paths.Valid model)
    (rhs v : Fin model.size → Rat) (nonnegative : ∀ i, 0 ≤ rhs i)
    (eqs : ∀ i, v i = rhs i + if model.kind i = .transient then
      ∑ j, model.transition i j * v j else 0) : ∀ i, 0 ≤ v i := by
  obtain ⟨smallest, _, bound⟩ := Finset.exists_min_image Finset.univ v
    ⟨model.initial, Finset.mem_univ _⟩
  let M := v smallest
  have bounded : ∀ i, M ≤ v i := fun i => bound i (Finset.mem_univ _)
  have minimum_next (i) (transient : model.kind i = .transient) (minimum : v i = M) :
      v (paths.next i) = M := by
    have h := eqs i
    rw [if_pos transient, minimum] at h
    have sumDiff : (∑ j, model.transition i j * (v j - M)) =
        (∑ j, model.transition i j * v j) - M := by
      simp only [mul_sub, Finset.sum_sub_distrib, ← Finset.sum_mul, model.normalized, one_mul]
    have term := Finset.single_le_sum
      (fun j (_ : j ∈ Finset.univ) => mul_nonneg (model.nonnegative i j)
        (sub_nonneg.mpr (bounded j))) (Finset.mem_univ (paths.next i))
    rw [sumDiff] at term
    have positive := (valid i transient).1
    have hn := nonnegative i
    have hb := bounded (paths.next i)
    nlinarith
  have descend : ∀ n i, paths.rank i = n → v i = M → 0 ≤ M := by
    intro n
    induction n using Nat.strong_induction_on with
    | h n ih =>
      intro i rank minimum
      by_cases transient : model.kind i = .transient
      · exact ih (paths.rank (paths.next i)) (rank ▸ (valid i transient).2)
          (paths.next i) rfl (minimum_next i transient minimum)
      · have h := eqs i
        rw [if_neg transient, add_zero, minimum] at h
        exact h ▸ nonnegative i
  exact fun i => (descend _ smallest rfl rfl).trans (bounded i)

end Determinize.Proof.FiniteModel
