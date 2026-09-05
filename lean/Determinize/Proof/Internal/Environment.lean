import Mathlib.Data.Fin.Basic
import Mathlib.Data.Real.Basic

/-! # Environment operations used by the symbolic proof -/

namespace Determinize.Proof.Paper

/-- An environment of real values indexed by de Bruijn level. -/
abbrev Env (n : Nat) := Fin n → ℝ

namespace Env

def empty : Env 0 := Fin.elim0

/-- Add the most recently sampled value at index zero. -/
def cons {n : Nat} (head : ℝ) (tail : Env n) : Env (n + 1) :=
  Fin.cases head tail

@[simp] theorem cons_zero {n : Nat} (head : ℝ) (tail : Env n) :
    cons head tail 0 = head := rfl

@[simp] theorem cons_succ {n : Nat} (head : ℝ) (tail : Env n) (i : Fin n) :
    cons head tail i.succ = tail i := rfl

end Env

end Determinize.Proof.Paper
