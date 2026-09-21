import Determinize.Spec.Semantics
import Mathlib.MeasureTheory.Constructions.Pi

/-!
# Expression coordinates for measurability proofs

An expression is encoded as a real-free skeleton together with the list of its
real literals. The skeleton carries the discrete sigma-algebra; the real
coordinates are measured through their length and their zero-padded entries,
exactly like the trace layer measures a trace. This structure is absent from
the direct evaluators and the public theorem statements.
-/

namespace Determinize.Spec.Paper

/-- The same syntax with real values erased. -/
abbrev Skeleton := Expr Unit

namespace Expr

def skeleton {Literal : Type} : Expr Literal → Skeleton
  | .discrete kind d => .discrete kind d.skeleton
  | .bvar i => .bvar i | .reject => .reject | .unit => .unit | .bool b => .bool b
  | .real _ => .real () | .lam b => .lam b.skeleton
  | .fix b => .fix b.skeleton | .app f x => .app f.skeleton x.skeleton
  | .pair l r => .pair l.skeleton r.skeleton | .fst x => .fst x.skeleton
  | .snd x => .snd x.skeleton | .inl x => .inl x.skeleton
  | .inr x => .inr x.skeleton
  | .matchSum x l r => .matchSum x.skeleton l.skeleton r.skeleton
  | .nil => .nil | .cons h t => .cons h.skeleton t.skeleton
  | .matchList x n c => .matchList x.skeleton n.skeleton c.skeleton
  | .ite c t e => .ite c.skeleton t.skeleton e.skeleton
  | .letE x b => .letE x.skeleton b.skeleton
  | .neg x => .neg x.skeleton
  | .add l r => .add l.skeleton r.skeleton | .mul l r => .mul l.skeleton r.skeleton
  | .div l r => .div l.skeleton r.skeleton | .lt l r => .lt l.skeleton r.skeleton
  | .uniform k l r => .uniform k l.skeleton r.skeleton
  | .gaussian k l r => .gaussian k l.skeleton r.skeleton
  | .poisson k x => .poisson k x.skeleton
  | .bernoulli k x => .bernoulli k x.skeleton
  | .exponential k x => .exponential k x.skeleton
  | .beta k l r => .beta k l.skeleton r.skeleton
  | .gamma k l r => .gamma k l.skeleton r.skeleton

def realCoordinates {Literal : Type} : Expr Literal → List Literal
  | .real value => [value]
  | .lam x | .fix x | .fst x | .snd x | .inl x
  | .inr x | .neg x => x.realCoordinates
  | .app l r | .pair l r | .cons l r | .add l r | .mul l r
  | .div l r | .lt l r => l.realCoordinates ++ r.realCoordinates
  | .matchSum x l r | .ite x l r =>
      x.realCoordinates ++ l.realCoordinates ++ r.realCoordinates
  | .matchList x n c => x.realCoordinates ++ n.realCoordinates ++ c.realCoordinates
  | .letE x b => x.realCoordinates ++ b.realCoordinates
  | .uniform _ l r | .gaussian _ l r | .beta _ l r | .gamma _ l r =>
      l.realCoordinates ++ r.realCoordinates
  | .poisson _ x | .bernoulli _ x | .exponential _ x | .discrete _ x => x.realCoordinates
  | _ => []

/-- The number of literals in a list value of literals, a cons-chain of literals ending in
`nil`, read off the syntax alone; `none` for any other expression. On an `Expr` this is the
length of the list `realListValue?` returns, and it only depends on the skeleton, so the arity
of a `discrete` site is a function of the skeleton of its evaluated weights. -/
def literalListArity? {Literal : Type} : Expr Literal → Option Nat
  | .nil => some 0
  | .cons (.real _) tail => (literalListArity? tail).map (· + 1)
  | _ => none

theorem literalListArity?_skeleton (expression : Expr) :
    expression.skeleton.literalListArity? = expression.literalListArity? := by
  induction expression with
  | cons head tail _ ih => cases head <;> simp [literalListArity?, skeleton, ih]
  | _ => simp [literalListArity?, skeleton]

end Expr

/-- `realListValue?` succeeds exactly on a cons-chain of real literals, with one real per
literal (`Expr.literalListArity?`). -/
theorem realListValue?_map_length (expression : Expr) :
    (realListValue? expression).map List.length = expression.literalListArity? := by
  induction expression with
  | cons head tail _ ih =>
      cases head <;> simp only [realListValue?, realValue?, Expr.literalListArity?, ← ih] <;>
        cases realListValue? tail <;> simp
  | _ => simp [realListValue?, Expr.literalListArity?]

/-- The reals of a list value of literals are its real coordinates, in order. -/
theorem realCoordinates_of_realListValue? {expression : Expr} {values : List ℝ}
    (equation : realListValue? expression = some values) :
    expression.realCoordinates = values := by
  induction expression generalizing values with
  | cons head tail _ ih =>
      cases head <;> simp only [realListValue?, realValue?] at equation <;>
        cases tailEq : realListValue? tail <;> simp [tailEq] at equation
      rw [← equation, Expr.realCoordinates, Expr.realCoordinates, ih tailEq]
      rfl
  | _ => simp_all [realListValue?, Expr.realCoordinates]

theorem realValue?_determinize (expression : Expr) :
    realValue? expression.determinize = realValue? expression := by
  cases expression <;> simp [Expr.determinize, realValue?]

/-- Determinization does not change the weights a `discrete` site reads: it maps literals,
`cons` and `nil` to themselves. -/
theorem realListValue?_determinize (expression : Expr) :
    realListValue? expression.determinize = realListValue? expression := by
  induction expression with
  | cons head tail _ ih => simp [Expr.determinize, realListValue?, realValue?_determinize, ih]
  | _ => simp [Expr.determinize, realListValue?]

structure RealCoordinates where values : List ℝ

/-- Length and zero-padded coordinates, mirroring the measurable structure of traces
in `Determinize.Spec.Traces`. The length is recorded because padding alone is not injective. -/
def RealCoordinates.code (coordinates : RealCoordinates) : Nat × (Nat → ℝ) :=
  (coordinates.values.length, fun index => coordinates.values.getD index 0)

instance : MeasurableSpace RealCoordinates :=
  MeasurableSpace.comap RealCoordinates.code inferInstance

/-- A discrete skeleton paired with real coordinates. Every expression with a fixed
skeleton has a fixed number of real literals, so under `code` its coordinates are that
length together with zero-padded entries, measured as in the trace layer. -/
abbrev Code := Skeleton × RealCoordinates

def code (expression : Expr) : Code :=
  (expression.skeleton, ⟨expression.realCoordinates⟩)

instance : MeasurableSpace Skeleton := ⊤
instance : MeasurableSpace Expr := MeasurableSpace.comap code inferInstance

def terminalFloatSet : Set Expr := {expression | ∃ value, expression = .real value}

def terminalFloatValue : Expr → ℝ
  | .real value => value
  | _ => 0

end Determinize.Spec.Paper
