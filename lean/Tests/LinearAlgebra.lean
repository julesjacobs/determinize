import Determinize.Proof.LinearAlgebra.Solve
import Tests.Parsing

namespace Determinize.Tests

private def solveEquations {n : Nat} (A : Fin n → Fin n → Rat) (b : Fin n → Rat) : Option (List Rat) :=
  (Proof.LinearAlgebra.solve n A b).map fun solution => List.ofFn solution.val

example : solveEquations (n := 0) Fin.elim0 Fin.elim0 = some [] := by decide +kernel
example : solveEquations ![![0, 2], ![3, 4]] ![6, 15] = some [1, 3] := by decide +kernel
example : solveEquations ![![0, 2, -1], ![1, 0, 3], ![4, -2, 0]] ![-8, 17, 11] =
    some [2, -3/2, 5] := by decide +kernel
example : solveEquations ![![1, 2], ![2, 4]] ![3, 6] = none := by decide +kernel
example : solveEquations ![![1, 2], ![2, 4]] ![3, 7] = none := by decide +kernel
example : solveEquations ![![0, 1], ![0, 2]] ![1, 2] = none := by decide +kernel

def linearAlgebra : IO Unit := do
  for n in [1, 2, 5, 12, 24] do
    let A := fun i j : Fin n =>
      let row := n - 1 - i.val
      if row = j.val then (row+1 : Rat) else if row < j.val then (j.val+1 : Rat) / 3 else 0
    let expected := fun i : Fin n => (i.val : Rat) / 2 - 3
    let rhs := fun i => ∑ j, A i j * expected j
    let some answer := solveEquations A rhs | throw (IO.userError s!"solver rejected size {n}")
    assert (answer == List.ofFn expected) s!"incorrect solution at size {n}"

#print axioms Proof.LinearAlgebra.solve

end Determinize.Tests
