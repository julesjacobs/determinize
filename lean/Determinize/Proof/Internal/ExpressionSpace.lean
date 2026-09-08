import Determinize.Statement.Syntax
import Mathlib.MeasureTheory.Constructions.Pi

/-!
# Expression coordinates for measurability proofs

An expression is encoded as a real-free skeleton together with the list of its
real literals. The skeleton carries the discrete sigma-algebra; the real
coordinates are measured through their length and their zero-padded entries,
exactly like the trace layer measures a trace. This structure is absent from
the direct evaluators and the public theorem statements.
-/

namespace Determinize.Statement.Paper

/-- The same syntax with real values erased. -/
abbrev Skeleton := Expr Unit

namespace Expr

def skeleton : Expr → Skeleton
  | .bvar i => .bvar i | .unit => .unit | .bool b => .bool b
  | .real m _ => .real m () | .lam b => .lam b.skeleton
  | .fix b => .fix b.skeleton | .app f x => .app f.skeleton x.skeleton
  | .pair l r => .pair l.skeleton r.skeleton | .fst x => .fst x.skeleton
  | .snd x => .snd x.skeleton | .inl x => .inl x.skeleton
  | .inr x => .inr x.skeleton
  | .matchSum x l r => .matchSum x.skeleton l.skeleton r.skeleton
  | .nil => .nil | .cons h t => .cons h.skeleton t.skeleton
  | .matchList x n c => .matchList x.skeleton n.skeleton c.skeleton
  | .ite c t e => .ite c.skeleton t.skeleton e.skeleton
  | .letE x b => .letE x.skeleton b.skeleton
  | .promote x => .promote x.skeleton | .neg m x => .neg m x.skeleton
  | .add m l r => .add m l.skeleton r.skeleton | .mul m l r => .mul m l.skeleton r.skeleton
  | .div m l r => .div m l.skeleton r.skeleton | .lt l r => .lt l.skeleton r.skeleton
  | .sample m op a g => .sample m op (a.map skeleton) (g.map skeleton)

def realCoordinates : Expr → List ℝ
  | .real _ value => [value]
  | .lam x | .fix x | .fst x | .snd x | .inl x
  | .inr x | .promote x | .neg _ x => x.realCoordinates
  | .app l r | .pair l r | .cons l r | .add _ l r | .mul _ l r
  | .div _ l r | .lt l r => l.realCoordinates ++ r.realCoordinates
  | .matchSum x l r | .ite x l r =>
      x.realCoordinates ++ l.realCoordinates ++ r.realCoordinates
  | .matchList x n c => x.realCoordinates ++ n.realCoordinates ++ c.realCoordinates
  | .letE x b => x.realCoordinates ++ b.realCoordinates
  | .sample _ _ a g => a.flatMap realCoordinates ++ g.flatMap realCoordinates
  | _ => []

end Expr

structure RealCoordinates where values : List ℝ

/-- Length and zero-padded coordinates, mirroring the measurable structure of traces
in `Determinize.Traces`. The length is recorded because padding alone is not injective. -/
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

def terminalFloatSet : Set Expr := {expression | ∃ value, expression = .real .E value}

def terminalFloatValue : Expr → ℝ
  | .real .E value => value
  | _ => 0

end Determinize.Statement.Paper
