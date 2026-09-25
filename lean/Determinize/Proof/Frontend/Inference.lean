import Determinize.Proof.Frontend.Soundness
import Determinize.Proof.Frontend.Completeness
import Determinize.Proof.Frontend.Decompose
import Determinize.Proof.Frontend.Unify

/-!
# Soundness, optimality, and completeness of affinity inference

The statement of `Spec/Inference.lean`, assembled from the lemmas of the other files in
`Proof/Frontend/`:

* `solveInput_typed` (soundness): the most general shapes and the greatest solution of the
  affinity constraints solve the subtyping constraints of the draft (Lemma S′), so the read-back
  program is typed (Lemma S). The draft's type may keep shape variables after decoration; they
  are unconstrained, and any instance of them types the program.
* `solveInput_complete` (completeness and optimality): a typed completion solves the subtyping
  constraints (Lemma C). Its shapes unify the shape equations, so `unify` succeeds and they factor
  through the most general shapes; its affinities, read off at the leaves of the decorated shapes,
  solve the atomic constraints (Lemma C′), so `solveAffinities` succeeds and returns a larger
  solution (Lemma P); read-back is monotone.
-/

namespace Determinize.Proof.Frontend
open Determinize.Frontend Spec Spec.Paper Checking Ground

/-! ## The phases of inference -/

/-- The shape equations of a draft. -/
abbrev shapeEquations (d : Draft) : List (Shape × Shape) :=
  d.relations.map fun x => (x.1.shape, x.2.shape)

/-- The atomic affinity constraints of a draft under the shapes `θ`. -/
abbrev affinityConstraints (d : Draft) (θ : Nat → Shape) : List (AffinityConstraint AffinityVar) :=
  d.relations.flatMap fun x => decompose (x.1.decorate θ) (x.2.decorate θ)

theorem solveInput_ok {input : Input} {s : Solution} (h : solveInput input = .ok s) :
    ∃ n', (generate [] input).run 0 = .ok (s.draft, n') ∧
      unify (shapeEquations s.draft) = some s.shapes ∧
      solveAffinities (affinityConstraints s.draft s.shapes) = some s.affinities := by
  unfold solveInput at h
  simp only [bind_eq_ok] at h
  obtain ⟨⟨d, n'⟩, hg, h⟩ := h
  split at h
  · rename_i θ hu
    split at h
    · rename_i ρ ha
      rw [pure_eq_ok] at h
      subst h
      exact ⟨n', hg, hu, ha⟩
    · cases h
  · cases h

theorem solveInput_of {input : Input} {d : Draft} {n' : Nat} {θ : Nat → Shape}
    {ρ : AffinityVar → Affinity} (hg : (generate [] input).run 0 = .ok (d, n'))
    (hu : unify (shapeEquations d) = some θ)
    (ha : solveAffinities (affinityConstraints d θ) = some ρ) :
    solveInput input = .ok ⟨d, θ, ρ⟩ := by
  unfold solveInput
  simp only [hg, ok_bind]
  simp only [shapeEquations, affinityConstraints] at hu ha
  simp only [hu, ha]
  rfl

theorem infer_ok {input : Input} {program : Core} {ty : Ty} :
    Frontend.infer input = .ok (program, ty) ↔ ∃ s, solveInput input = .ok s ∧
      program = s.draft.program s.affinities ∧ ty = s.type s.draft.ty := by
  simp only [Frontend.infer, bind_eq_ok, pure_eq_ok, Prod.mk.injEq]
  exact ⟨fun ⟨s, hs, h₁, h₂⟩ => ⟨s, hs, h₁.symm, h₂.symm⟩,
    fun ⟨s, hs, h₁, h₂⟩ => ⟨s, hs, h₁.symm, h₂.symm⟩⟩

/-! ## Soundness -/

/-- The inferred program is typed at every instance of the decorated type of the draft. -/
theorem solveInput_typed {input : Input} {s : Solution} (h : solveInput input = .ok s)
    (v : Nat → Ty) :
    input.matches (s.draft.program s.affinities) = true ∧
      Typed [] (interpret (s.draft.program s.affinities))
        ((s.draft.ty.decorate s.shapes).instantiate v s.affinities) := by
  obtain ⟨n', hg, hu, ha⟩ := solveInput_ok h
  let σ : Ground := ⟨fun α => ((UType.var α).decorate s.shapes).instantiate v s.affinities,
    s.affinities⟩
  have inst : ∀ u, σ.inst u = (u.decorate s.shapes).instantiate v s.affinities :=
    fun u => (instantiate_decorate _ _ _ u).symm
  have solves : σ.Solves s.draft.relations := by
    intro p mem
    rw [inst, inst]
    refine decompose_sound v s.affinities _ _ ?_ ?_
    · rw [shape_decorate, shape_decorate]
      exact unify_sound hu (p.1.shape, p.2.shape) (List.mem_map.2 ⟨p, mem, rfl⟩)
    · exact fun c hc => solveAffinities_sound ha c (List.mem_flatMap.2 ⟨p, mem, hc⟩)
  have := generate_sound σ input [] 0 s.draft n' hg solves
  rw [inst] at this
  exact this

/-! ## Completeness and optimality -/

/-- A typed completion determines a solution of inference above it. -/
theorem solveInput_complete {input : Input} {completion : Core} {T : Ty}
    (hm : input.matches completion = true) (ht : Typed [] (interpret completion) T) :
    ∃ s, solveInput input = .ok s ∧ AffinityLE completion (s.draft.program s.affinities) := by
  -- Lemma C: the completion solves the subtyping constraints.
  obtain ⟨d, n', hg, -, σ₁, -, F⟩ := generate_complete input [] [] 0 ⟨fun _ => .unit, fun _ => .G⟩
    completion T (fun _ _ => List.Forall₂.nil) hm ht
  -- Its shapes unify the shape equations.
  let δ : Nat → Shape := fun α => shapeOf (σ₁.types α)
  obtain ⟨S₁, -, -⟩ := F σ₁ (Agree.refl _ _)
  have unifies : Unifies δ (shapeEquations d) := by
    intro e he
    obtain ⟨p, mem, rfl⟩ := List.mem_map.1 he
    have := shapeOf_sub (S₁ p mem)
    rwa [Ground.inst, Ground.inst, shape_instantiate, shape_instantiate] at this
  obtain ⟨θ, hu⟩ := unify_complete unifies
  have factor := unify_mostGeneral hu unifies
  -- Its affinities, read off at the leaves, solve the atomic constraints.
  let ρ : AffinityVar → Affinity := fun
    | .generated j => σ₁.affinities (.generated j)
    | .leaf α p => affinityAt (σ₁.types α) p
  let σ₂ : Ground := ⟨σ₁.types, ρ⟩
  obtain ⟨S₂, -, program⟩ := F σ₂ fun _ _ => ⟨rfl, rfl⟩
  have satisfies : Satisfies ρ (affinityConstraints d θ) := by
    intro c hc'
    obtain ⟨p, mem, hc⟩ := List.mem_flatMap.1 hc'
    have approx := approx_decorate (θ := θ) (types := σ₁.types) (ρ := ρ) factor fun _ _ => rfl
    exact decompose_complete (S₂ p mem) _ _ (approx p.1) (approx p.2) c hc
  obtain ⟨ρmax, ha⟩ := solveAffinities_complete satisfies
  refine ⟨_, solveInput_of hg hu ha, ?_⟩
  rw [← program]
  exact program_le (solveAffinities_greatest ha satisfies) d

/-! ## The statement -/

theorem inferCorrect : inferCorrectThm := by
  intro input
  split
  · rintro ⟨completion, hm, T, ht⟩
    obtain ⟨s, hs, -⟩ := solveInput_complete hm ht
    rename_i h
    rw [infer_ok.2 ⟨s, hs, rfl, rfl⟩] at h
    cases h
  · rename_i program ty h
    obtain ⟨s, hs, rfl, rfl⟩ := infer_ok.1 h
    obtain ⟨hm, ht⟩ := solveInput_typed hs fun _ => .unit
    refine ⟨hm, ht, ?_⟩
    rintro completion ⟨hm', T, ht'⟩
    obtain ⟨s', hs', le⟩ := solveInput_complete hm' ht'
    rw [hs, Except.ok.injEq] at hs'
    subst hs'
    exact le

end Determinize.Proof.Frontend
