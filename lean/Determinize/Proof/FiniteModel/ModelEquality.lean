import Determinize.Statement.FiniteModel.Model

namespace Determinize.Statement.FiniteModel

instance : DecidableEq Model := fun a b => by
  rcases a with ⟨asize, ai, ak, atr, an, az, aa⟩
  rcases b with ⟨bsize, bi, bk, bt, bn, bz, ba⟩
  by_cases sizeEqual : asize = bsize
  · subst bsize
    exact decidable_of_iff (ai = bi ∧ ak = bk ∧ atr = bt) (by
      constructor
      · rintro ⟨rfl, rfl, rfl⟩; rfl
      · intro equal
        cases equal
        exact ⟨rfl, rfl, rfl⟩)
  · exact isFalse (fun equal => sizeEqual (congrArg Model.size equal))

end Determinize.Statement.FiniteModel
