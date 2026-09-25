import Determinize.Frontend.Infer
import Determinize.Proof.Frontend.Affinity
import Determinize.Proof.Semantics.Subtyping

/-!
# Decoration and decomposition

Inference turns a subtyping constraint `s <: t` between types with type variables into atomic
affinity constraints: it replaces every type variable by its decorated most general shape and
decomposes the two sides. This file relates the atomic constraints to `Ty.Sub`, in both
directions (Lemmas S′ and C′ in `notes/inference-optimality/1-claims.md`):

* `decompose_sound`: if two types have the same shape, a solution of their decomposition makes
  every instance of the first a subtype of the same instance of the second.
* `decompose_complete`: if a ground instance of `s` is a subtype of the same instance of `t`,
  the affinities that the instance has at the floats of the decorated shapes solve the
  decomposition. The ground instance may have more structure than the decorated shapes where
  these have shape variables; such positions carry no constraint.

The shape of a ground type is its erasure `shapeOf`, and subtyping preserves it.
-/

namespace Determinize.Proof.Frontend
open Determinize.Frontend Spec.Paper

/-! ## Shapes -/

/-- The shape of a type: the type without its affinities. -/
def shapeOf : Ty → Shape
  | .unit => .unit
  | .bool => .bool
  | .float _ => .float
  | .prod a b => .prod (shapeOf a) (shapeOf b)
  | .sum a b => .sum (shapeOf a) (shapeOf b)
  | .list a => .list (shapeOf a)
  | .arr a b => .arr (shapeOf a) (shapeOf b)

theorem shapeOf_sub {a b : Ty} (h : Ty.Sub a b) : shapeOf a = shapeOf b := by
  induction h <;> simp_all [shapeOf]

theorem shape_instantiate (types : Nat → Ty) (ρ : AffinityVar → Affinity) (u : UType) :
    shapeOf (u.instantiate types ρ) = u.shape.subst fun α => shapeOf (types α) := by
  induction u <;> simp_all [UType.instantiate, UType.shape, shapeOf, Shape.subst]

theorem Shape.shape_decorate (S : Shape) (leaf : List Nat → AffinityVar) :
    (S.decorate leaf).shape = S := by
  induction S generalizing leaf <;> simp_all [Shape.decorate, UType.shape]

theorem shape_decorate (θ : Nat → Shape) (u : UType) : (u.decorate θ).shape = u.shape.subst θ := by
  induction u <;> simp_all [UType.decorate, UType.shape, Shape.subst, Shape.shape_decorate]

/-- Instantiating a decorated type is instantiating the type, with each type variable replaced
by the instance of its decoration. -/
theorem instantiate_decorate (θ : Nat → Shape) (types : Nat → Ty) (ρ : AffinityVar → Affinity)
    (u : UType) :
    (u.decorate θ).instantiate types ρ =
      u.instantiate (fun α => ((UType.var α).decorate θ).instantiate types ρ) ρ := by
  induction u <;> simp_all [UType.decorate, UType.instantiate]

/-! ## Soundness of decomposition (Lemma S′) -/

theorem satisfies_nil {V : Type} (ρ : V → Affinity) :
    Satisfies ρ ([] : List (AffinityConstraint V)) :=
  fun _ h => nomatch h

theorem satisfies_append {V : Type} {ρ : V → Affinity} {front rest : List (AffinityConstraint V)} :
    Satisfies ρ (front ++ rest) ↔ Satisfies ρ front ∧ Satisfies ρ rest := by
  simp only [Satisfies, List.mem_append]
  exact ⟨fun h => ⟨fun c m => h c (.inl m), fun c m => h c (.inr m)⟩,
    fun h c m => m.elim (h.1 c) (h.2 c)⟩

theorem satisfies_singleton {V : Type} {ρ : V → Affinity} {c : AffinityConstraint V} :
    Satisfies ρ [c] ↔ Ty.Sub (.float (c.1.eval ρ)) (.float (c.2.eval ρ)) := by
  simp [Satisfies]

/-- Lemma S′: a solution of the decomposition of two types of the same shape makes every
instance of the first a subtype of the same instance of the second. -/
theorem decompose_sound (types : Nat → Ty) (ρ : AffinityVar → Affinity) :
    ∀ u u' : UType, u.shape = u'.shape → Satisfies ρ (decompose u u') →
      Ty.Sub (u.instantiate types ρ) (u'.instantiate types ρ) := by
  intro u u'
  induction u, u' using decompose.induct with
  | case1 a b =>
    intro _ h
    rw [decompose, satisfies_singleton] at h
    exact h
  | case2 a b c d iha ihb | case3 a b c d iha ihb =>
    intro shape h
    simp only [UType.shape, Shape.prod.injEq, Shape.sum.injEq] at shape
    rw [decompose, satisfies_append] at h
    constructor
    · exact iha shape.1 h.1
    · exact ihb shape.2 h.2
  | case4 a b ih =>
    intro shape h
    simp only [UType.shape, Shape.list.injEq] at shape
    rw [decompose] at h
    exact .list (ih shape h)
  | case5 a b c d iha ihb =>
    intro shape h
    simp only [UType.shape, Shape.arr.injEq] at shape
    rw [decompose, satisfies_append] at h
    exact .arr (iha shape.1.symm h.1) (ihb shape.2 h.2)
  | case6 u u' float prod sum list arr =>
    intro shape _
    cases u <;> cases u' <;> simp only [UType.shape, reduceCtorEq, Shape.var.injEq] at shape
    all_goals first
      | (subst shape; exact .refl _)
      | exact .refl _
      | exact (float _ _ rfl rfl).elim
      | exact (prod _ _ _ _ rfl rfl).elim
      | exact (sum _ _ _ _ rfl rfl).elim
      | exact (list _ _ rfl rfl).elim
      | exact (arr _ _ _ _ rfl rfl).elim

/-! ## Completeness of decomposition (Lemma C′) -/

/-- The affinity of a type at a position, in the convention of `Shape.decorate`: child indices
from the root, 0 and 1 for the two children of `prod`, `sum` and `arr`, 0 for a list element. -/
def affinityAt : Ty → List Nat → Affinity
  | .float m, [] => m
  | .prod a _, 0 :: p | .sum a _, 0 :: p | .list a, 0 :: p | .arr a _, 0 :: p => affinityAt a p
  | .prod _ b, 1 :: p | .sum _ b, 1 :: p | .arr _ b, 1 :: p => affinityAt b p
  | _, _ => .G

/-- The type `T` has the structure of `u` wherever `u` is not a variable or a base type, and
the affinity of `u` under `ρ` at every float of `u`. -/
def Approx (ρ : AffinityVar → Affinity) : UType → Ty → Prop
  | .float a, T => T = .float (a.eval ρ)
  | .prod a b, T => ∃ A B, T = .prod A B ∧ Approx ρ a A ∧ Approx ρ b B
  | .sum a b, T => ∃ A B, T = .sum A B ∧ Approx ρ a A ∧ Approx ρ b B
  | .list a, T => ∃ A, T = .list A ∧ Approx ρ a A
  | .arr a b, T => ∃ A B, T = .arr A B ∧ Approx ρ a A ∧ Approx ρ b B
  | _, _ => True

/-- A decorated shape approximates every type of that shape whose affinities the leaf variables
read off. -/
theorem approx_shape_decorate {ρ : AffinityVar → Affinity} {δ : Nat → Shape} :
    ∀ (S : Shape) (T : Ty) (leaf : List Nat → AffinityVar), S.subst δ = shapeOf T →
      (∀ p, ρ (leaf p) = affinityAt T p) → Approx ρ (S.decorate leaf) T := by
  intro S
  induction S with
  | var | unit | bool => intros; simp [Shape.decorate, Approx]
  | float =>
    intro T leaf shape hleaf
    cases T <;> simp [Shape.subst, shapeOf] at shape
    simp [Shape.decorate, Approx, AffinityTerm.eval, hleaf, affinityAt]
  | prod a b iha ihb =>
    intro T leaf shape hleaf
    cases T <;> simp [Shape.subst, shapeOf] at shape
    exact ⟨_, _, rfl, iha _ _ shape.1 fun p => hleaf (0 :: p),
      ihb _ _ shape.2 fun p => hleaf (1 :: p)⟩
  | sum a b iha ihb =>
    intro T leaf shape hleaf
    cases T <;> simp [Shape.subst, shapeOf] at shape
    exact ⟨_, _, rfl, iha _ _ shape.1 fun p => hleaf (0 :: p),
      ihb _ _ shape.2 fun p => hleaf (1 :: p)⟩
  | list a ih =>
    intro T leaf shape hleaf
    cases T <;> simp [Shape.subst, shapeOf] at shape
    exact ⟨_, rfl, ih _ _ shape fun p => hleaf (0 :: p)⟩
  | arr a b iha ihb =>
    intro T leaf shape hleaf
    cases T <;> simp [Shape.subst, shapeOf] at shape
    exact ⟨_, _, rfl, iha _ _ shape.1 fun p => hleaf (0 :: p),
      ihb _ _ shape.2 fun p => hleaf (1 :: p)⟩

/-- If the shapes `θ` factor the shapes of `types`, and the leaf variables read off the
affinities of `types`, the decoration of every type approximates its instance. -/
theorem approx_decorate {θ : Nat → Shape} {types : Nat → Ty} {ρ : AffinityVar → Affinity}
    (factor : ∀ α, (θ α).subst (fun β => shapeOf (types β)) = shapeOf (types α))
    (leaves : ∀ α p, ρ (.leaf α p) = affinityAt (types α) p) :
    ∀ u : UType, Approx ρ (u.decorate θ) (u.instantiate types ρ) := by
  intro u
  induction u with
  | var α => exact approx_shape_decorate _ _ _ (factor α) (leaves α)
  | unit | bool => simp [UType.decorate, Approx]
  | float a => simp [UType.decorate, UType.instantiate, Approx]
  | prod a b iha ihb | sum a b iha ihb | arr a b iha ihb => exact ⟨_, _, rfl, iha, ihb⟩
  | list a ih => exact ⟨_, rfl, ih⟩

/-- Lemma C′: the affinities of a subtyping between ground types solve the decomposition of any
two types that approximate them. -/
theorem decompose_complete {ρ : AffinityVar → Affinity} {T U : Ty} (sub : Ty.Sub T U) :
    ∀ u u' : UType, Approx ρ u T → Approx ρ u' U → Satisfies ρ (decompose u u') := by
  induction sub with
  | unit | bool =>
    intro u u' hu hu'
    cases u <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu <;>
      cases u' <;> simp only [decompose] <;> exact satisfies_nil _
  | float m =>
    intro u u' hu hu'
    cases u <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu <;>
      cases u' <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu' <;>
      simp only [decompose] <;> try exact satisfies_nil _
    rw [satisfies_singleton]
    simp only [Ty.float.injEq] at hu hu'
    rw [← hu, ← hu']
    exact .refl _
  | general =>
    intro u u' hu hu'
    cases u <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu <;>
      cases u' <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu' <;>
      simp only [decompose] <;> try exact satisfies_nil _
    rw [satisfies_singleton]
    simp only [Ty.float.injEq] at hu hu'
    rw [← hu, ← hu']
    exact .general
  | prod _ _ iha ihb | sum _ _ iha ihb =>
    intro u u' hu hu'
    cases u <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu <;>
      cases u' <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu' <;>
      simp only [decompose] <;> try exact satisfies_nil _
    all_goals
      obtain ⟨_, _, ⟨⟩, hu₁, hu₂⟩ := hu
      obtain ⟨_, _, ⟨⟩, hu₁', hu₂'⟩ := hu'
      exact satisfies_append.2 ⟨iha _ _ hu₁ hu₁', ihb _ _ hu₂ hu₂'⟩
  | list _ ih =>
    intro u u' hu hu'
    cases u <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu <;>
      cases u' <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu' <;>
      simp only [decompose] <;> try exact satisfies_nil _
    obtain ⟨_, ⟨⟩, hu⟩ := hu
    obtain ⟨_, ⟨⟩, hu'⟩ := hu'
    exact ih _ _ hu hu'
  | arr _ _ iha ihb =>
    intro u u' hu hu'
    cases u <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu <;>
      cases u' <;> simp only [Approx, reduceCtorEq, false_and, exists_false] at hu' <;>
      simp only [decompose] <;> try exact satisfies_nil _
    obtain ⟨_, _, ⟨⟩, hu₁, hu₂⟩ := hu
    obtain ⟨_, _, ⟨⟩, hu₁', hu₂'⟩ := hu'
    exact satisfies_append.2 ⟨iha _ _ hu₁' hu₁, ihb _ _ hu₂ hu₂'⟩

end Determinize.Proof.Frontend
