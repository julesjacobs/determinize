import Determinize.Spec.Syntax

namespace Determinize.Spec.Paper

/-!
# Bunched extension

The rules below formalize the scalar declarative fragment of `extensions/main.tex`.
Right now just a small fragment of the language that excludes pairs, lists, functions, recursion.
-/
namespace Bunched

/-- The scalar types admitted by this fragment. -/
def scalar : Ty → Bool
  | .float _ | .bool | .unit => true
  | _ => false

/-- The paper's sigma types, restricted to the scalar fragment. -/
def shareable : Ty → Bool
  | .float .G | .bool | .unit => true
  | _ => false

inductive Bunch where
  | empty
  | bind (index : Nat) (ty : Ty)
  | add (left right : Bunch)
  | sep (left right : Bunch)
deriving DecidableEq, Repr

namespace Bunch

/-- Collects all bindings in a bunch into a list -/
def bindings : Bunch → List (Nat × Ty)
  | .empty => []
  | .bind i t => [(i, t)]
  | .add l r | .sep l r => l.bindings ++ r.bindings

/-- Implements the recursive well-formedness condition. -/
def wf : Bunch → Bool
  | .empty => true
  | .bind _ _ => true
  | .add l r => l.wf && r.wf && l.bindings.all (fun (i, t) ↦
      r.bindings.all (fun (j, u) ↦ i != j || t == u))
  | .sep l r => l.wf && r.wf && l.bindings.all (fun (i, _) ↦
      r.bindings.all (fun (j, _) ↦ i != j))

def shift : Bunch → Bunch
  | .empty => .empty
  | .bind i t => .bind (i + 1) t
  | .add l r => .add l.shift r.shift
  | .sep l r => .sep l.shift r.shift

/-- Defines proofs that two bunches are structurally equivalent. -/
inductive Equiv : Bunch → Bunch → Prop
  | refl : Equiv g g
  | symm : Equiv g h → Equiv h g
  | trans : Equiv g h → Equiv h k → Equiv g k
  | addUnit : Equiv (.add g .empty) g
  | sepUnit : Equiv (.sep g .empty) g
  | addComm : Equiv (.add g h) (.add h g)
  | sepComm : Equiv (.sep g h) (.sep h g)
  | addAssoc : Equiv (.add (.add g h) k) (.add g (.add h k))
  | sepAssoc : Equiv (.sep (.sep g h) k) (.sep g (.sep h k))
  | addCongr : Equiv g g' → Equiv h h' → Equiv (.add g h) (.add g' h')
  | sepCongr : Equiv g g' → Equiv h h' → Equiv (.sep g h) (.sep g' h')

end Bunch

/-- Represents the one-hole bunched context Γ_0. -/
inductive Hole where
  | hole
  | addLeft (outer : Hole) (right : Bunch)
  | addRight (left : Bunch) (outer : Hole)
  | sepLeft (outer : Hole) (right : Bunch)
  | sepRight (left : Bunch) (outer : Hole)
deriving Repr

def Hole.fill : Hole → Bunch → Bunch
  | .hole, g => g
  | .addLeft h r, g => .add (h.fill g) r
  | .addRight l h, g => .add l (h.fill g)
  | .sepLeft h r, g => .sep (h.fill g) r
  | .sepRight l h, g => .sep l (h.fill g)

def Hole.shift : Hole → Hole
  | .hole => .hole
  | .addLeft h r => .addLeft h.shift r.shift
  | .addRight l h => .addRight l.shift h.shift
  | .sepLeft h r => .sepLeft h.shift r.shift
  | .sepRight l h => .sepRight l.shift h.shift

abbrev Shared := List (Nat × Ty)

def shiftShared (d : Shared) : Shared := d.map fun (i, t) ↦ (i + 1, t)

/-- Checks the complete context Δ | Γ. Requires bunch.wf, among other things. This is stronger than checking wf(Γ) alone ebcause it also checks the ordinary context. -/
def contextWF (d : Shared) (g : Bunch) : Bool :=
  g.wf && (d.map Prod.fst).Nodup && d.all (fun (_, t) ↦ shareable t) &&
    d.all (fun (i, _) ↦ g.bindings.all (fun (j, _) ↦ i != j))

/-- Subtyping. -/
inductive Sub : Ty → Ty → Prop
  | refl : scalar t = true → Sub t t
  | general : Sub (.float .G) (.float .E)

/-- Repersents Δ | Γ ⊢ e : τ. -/
inductive Typed : Shared → Bunch → Expr → Ty → Prop
  | varShared : contextWF shared .empty = true → (index, ty) ∈ shared →
      Typed shared .empty (.bvar index) ty
  | varBunch : contextWF shared (.bind index ty) = true →
      Typed shared (.bind index ty) (.bvar index) ty
  | real : contextWF shared .empty = true → Typed shared .empty (.real value) (.float affinity)
  | bool : contextWF shared .empty = true → Typed shared .empty (.bool value) .bool
  | unit : contextWF shared .empty = true → Typed shared .empty .unit .unit
  | add : Typed shared bunch left (.float affinity) → Typed shared bunch right (.float affinity) →
      Typed shared bunch (.add left right) (.float affinity)
  | mulInd : Typed shared leftBunch left (.float .E) → Typed shared rightBunch right (.float .E) →
      contextWF shared (.sep leftBunch rightBunch) = true →
      Typed shared (.sep leftBunch rightBunch) (.mul left right) (.float .E)
  | mulG : Typed shared .empty left (.float .G) → Typed shared bunch right (.float affinity) →
      Typed shared bunch (.mul left right) (.float affinity)
  | lt : Typed shared .empty left (.float .G) → Typed shared .empty right (.float .G) →
      Typed shared .empty (.lt left right) .bool
  | letE {outer : Hole} : Typed shared bunch value valueTy →
      Typed (shiftShared shared) (outer.shift.fill (.bind 0 valueTy)) body result →
      contextWF shared (outer.fill bunch) = true →
      Typed shared (outer.fill bunch) (.letE value body) result
  | letG : shareable valueTy = true → Typed shared .empty value valueTy →
      Typed ((0, valueTy) :: shiftShared shared) bunch.shift body result →
      Typed shared bunch (.letE value body) result
  | ite : Typed shared .empty condition .bool → Typed shared bunch thenBranch result →
      Typed shared bunch elseBranch result →
      Typed shared bunch (.ite condition thenBranch elseBranch) result
  | uniform : Typed shared bunch lower (.float affinity) →
      Typed shared bunch upper (.float affinity) →
      Typed shared bunch (.uniform (.sample affinity) lower upper) (.float affinity)
  | uniformMean : Typed shared bunch lower (.float affinity) →
      Typed shared bunch upper (.float affinity) →
      Typed shared bunch (.uniform .mean lower upper) (.float affinity)
  | gaussian : Typed shared bunch mean (.float affinity) →
      Typed shared bunch variance (.float .G) →
      Typed shared bunch (.gaussian (.sample affinity) mean variance) (.float affinity)
  | gaussianMean : Typed shared bunch mean (.float affinity) →
      Typed shared bunch variance (.float .G) →
      Typed shared bunch (.gaussian .mean mean variance) (.float affinity)
  | flipG : Typed shared .empty probability (.float .G) →
      Typed shared .empty (.lt (.real 0) (.bernoulli (.sample .G) probability)) .bool
  | weakening {outer : Hole} : Typed shared (outer.fill bunch) expression ty →
      contextWF shared (outer.fill (.add bunch extra)) = true →
      Typed shared (outer.fill (.add bunch extra)) expression ty
  | contraction {outer : Hole} : Typed shared (outer.fill (.add bunch bunch)) expression ty →
      Typed shared (outer.fill bunch) expression ty
  | conversion {outer : Hole} : Typed shared (outer.fill (.add leftBunch rightBunch)) expression ty →
      contextWF shared (outer.fill (.sep leftBunch rightBunch)) = true →
      Typed shared (outer.fill (.sep leftBunch rightBunch)) expression ty
  | equiv : Bunch.Equiv bunch bunch' → Typed shared bunch expression ty → Typed shared bunch' expression ty
  | sub : Typed shared bunch expression a → Sub a b → Typed shared bunch expression b

end Bunched

end Determinize.Spec.Paper
