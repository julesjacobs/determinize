import Mathlib.Data.Fin.Basic
import Mathlib.Data.Real.Basic

/-!
# Environments of E-affinity draws

`Env n = Fin n → ℝ` is a vector of the first `n` E-affinity draws, indexed by de Bruijn
index, and `Env.cons` prepends the newest draw (`cons_zero`, `cons_succ` are its simp lemmas).
The symbolic proof evaluates affine forms `Symbolic.Affine n` in such environments
(`Proof/Internal/Semantics.lean`), and the actual law of a sample history is a measure on
`Env n`.
-/

namespace Determinize.Proof.Paper

/-- An environment of real values indexed by de Bruijn index. -/
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
