import Determinize.Checking.Typing

namespace Determinize.Proof.Checking
open Determinize.Checking Spec.Paper

theorem check_sound {Γ e τ c} (h : (check Γ e τ c).isSome = true) :
    Typed Γ (interpret e) τ := by
  cases hc : check Γ e τ c with
  | none => simp [hc] at h
  | some proof => exact proof.down

end Determinize.Proof.Checking
