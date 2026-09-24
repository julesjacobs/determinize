import Determinize.Frontend.Compile
import Determinize.Proof.Frontend.Typing

/-! A mean site is typed by the rule of the sample site it replaces, at any affinity allowed by
its operands. -/

namespace Determinize.Tests.MeanTyping
open Determinize.Spec.Paper Determinize.Checking Determinize.Frontend Determinize.Proof.Frontend

private theorem typed_uniformMean_inv {Γ T} {l u : Expr} (h : Typed Γ (.uniform .mean l u) T) :
    ∃ m, Typed Γ l (.float m) ∧ Typed Γ u (.float m) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.uniform .mean l u = e at h
  induction h with
  | uniformMean hl hu => cases he; exact ⟨_, hl, hu, .refl _⟩
  | sub _ s ih => obtain ⟨m, hl, hu, s'⟩ := ih he; exact ⟨m, hl, hu, s'.trans s⟩
  | _ => cases he

private theorem typed_gaussianMean_inv {Γ T} {a b : Expr} (h : Typed Γ (.gaussian .mean a b) T) :
    ∃ m, Typed Γ a (.float m) ∧ Typed Γ b (.float .G) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.gaussian .mean a b = e at h
  induction h with
  | gaussianMean ha hb => cases he; exact ⟨_, ha, hb, .refl _⟩
  | sub _ s ih => obtain ⟨m, ha, hb, s'⟩ := ih he; exact ⟨m, ha, hb, s'.trans s⟩
  | _ => cases he

private theorem typed_discreteMean_inv {Γ T} {p : Expr} (h : Typed Γ (.discrete .mean p) T) :
    ∃ m, Typed Γ p (.list (.float m)) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.discrete .mean p = e at h
  induction h with
  | discreteMean hp => cases he; exact ⟨_, hp, .refl _⟩
  | sub _ s ih => obtain ⟨m, hp, s'⟩ := ih he; exact ⟨m, hp, s'.trans s⟩
  | _ => cases he

/-- A variable of type `A` has no type that `A` is not below. -/
private theorem not_typed_var {Γ A T} (h : Typed (A :: Γ) (.bvar 0) T) (n : ¬Ty.Sub A T) : False := by
  obtain ⟨_, hv, s⟩ := typed_bvar_inv h
  cases hv
  exact n s

private def meanExpr : Expr := .uniform .mean (.bvar 0) (.real 1)
example : Typed [.float .E] meanExpr (.float .E) := .uniformMean (.bvar .head) .real
example : Typed [.float .G] meanExpr (.float .G) := .uniformMean (.bvar .head) .real
example : ¬Typed [.float .E] meanExpr (.float .G) := by
  intro h
  obtain ⟨m, hl, _, s⟩ := typed_uniformMean_inv h
  cases s
  exact not_typed_var hl nofun
example : (Expr.uniform (.sample .E) (.real 0) (.real 2)).determinize =
    .uniform .mean (.real 0) (.real 2) := rfl
example : (Expr.uniform (.sample .G) (.real 0) (.real 2)).determinize =
    .uniform (.sample .G) (.real 0) (.real 2) := rfl

example (T : Ty) : ¬Typed [.float .E] (.gaussian .mean (.real 0) (.bvar 0)) T := by
  intro h
  obtain ⟨_, _, hb, _⟩ := typed_gaussianMean_inv h
  exact not_typed_var hb nofun

example : sampleAffinities (.uniform .mean (.poisson (.sample .E) (.real 2)) (.real 3)) = [.E] := rfl

private def discreteMean : Expr := .discrete .mean (.bvar 0)
example : Typed [.list (.float .E)] discreteMean (.float .E) := .discreteMean (.bvar .head)
example : Typed [.list (.float .G)] discreteMean (.float .G) := .discreteMean (.bvar .head)
example : ¬Typed [.list (.float .E)] discreteMean (.float .G) := by
  intro h
  obtain ⟨m, hp, s⟩ := typed_discreteMean_inv h
  cases s
  exact not_typed_var hp fun | .list s => nomatch s

end Determinize.Tests.MeanTyping
