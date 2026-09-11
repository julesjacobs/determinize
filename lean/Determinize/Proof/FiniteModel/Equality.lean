import Determinize.Finite.Machine

namespace Determinize.Finite

-- Avoid casts between Decidable types: they obstruct independent kernel reduction.
mutual
def valueDecEq (a b : Value) : Decidable (a = b) := by
  cases a <;> cases b <;> try (apply isFalse; intro h; cases h; done)
  case unit.unit => exact isTrue rfl
  case nil.nil => exact isTrue rfl
  case bool.bool a b => exact decidable_of_iff (a = b) (by simp)
  case number.number a b => exact decidable_of_iff (a = b) (by simp)
  case pair.pair a b c d =>
    letI := valueDecEq a c
    letI := valueDecEq b d
    exact decidable_of_iff (a = c ∧ b = d) (by simp)
  case cons.cons a b c d =>
    letI := valueDecEq a c
    letI := valueDecEq b d
    exact decidable_of_iff (a = c ∧ b = d) (by simp)
  case inl.inl a b =>
    letI := valueDecEq a b
    exact decidable_of_iff (a = b) (by simp)
  case inr.inr a b =>
    letI := valueDecEq a b
    exact decidable_of_iff (a = b) (by simp)
  case closure.closure a b c d =>
    letI := valuesDecEq b d
    exact decidable_of_iff (a = c ∧ b = d) (by simp)
  case recursive.recursive a b c d =>
    letI := valuesDecEq b d
    exact decidable_of_iff (a = c ∧ b = d) (by simp)
termination_by sizeOf a
def valuesDecEq (a b : List Value) : Decidable (a = b) := by
  cases a with
  | nil => cases b with
    | nil => exact isTrue rfl
    | cons _ _ => exact isFalse (by intro h; cases h)
  | cons x xs => cases b with
    | nil => exact isFalse (by intro h; cases h)
    | cons y ys =>
        letI := valueDecEq x y
        letI := valuesDecEq xs ys
        exact decidable_of_iff (x = y ∧ xs = ys) (by simp)
termination_by sizeOf a
end
instance : DecidableEq Value := valueDecEq
deriving instance DecidableEq for Unary, Binary, Frame, State, Evidence, Failure, Step
end Determinize.Finite
