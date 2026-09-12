import Determinize.Spec.Types

namespace Determinize.Spec.Paper

theorem Ty.Sub.refl (a : Ty) : Sub a a := by
  induction a <;> constructor <;> assumption

theorem Ty.Sub.trans (h : Sub a b) (k : Sub b c) : Sub a c := by
  induction b generalizing a c <;> cases h <;> cases k <;>
    first | exact .general | constructor <;> solve_by_elim

end Determinize.Spec.Paper
