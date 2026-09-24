import Determinize.Frontend.Syntax
import Determinize.Frontend.Unify
import Determinize.Frontend.Affinity

/-!
# Affinity inference

`infer` fills the omitted sample affinities of a resolved program with the greatest affinities
that make it typable. It follows the reference algorithm of
`notes/inference-optimality/1-claims.md`:

1. *Generation* (`generate`): give every node a type over type variables and affinity variables,
   and record every use of subsumption as a cast in a `Draft`. The casts are the subtyping
   constraints (`Draft.relations`). A sample site gets a fresh affinity variable, or its requested
   affinity; an operand that the typing rules require to be G gets `general`.
2. *Shapes* (`unify`): unify the shapes of the two sides of every constraint. Structural subtyping
   relates only types of the same shape.
3. *Decoration* (`UType.decorate`): replace every type variable `α` by its most general shape,
   with the affinity variable `leaf α position` at each float. The remaining shape variables are
   unconstrained; read-back sets them to `unit`. Type variables with equal shapes still get
   different affinity variables.
4. *Decomposition* (`decompose`): split every constraint between decorated types into atomic
   constraints between affinities: covariant, except in function arguments.
5. *Affinities* (`solveAffinities`): the greatest solution of the atomic constraints.
6. *Read-back* (`Draft.program`, `Solution.type`): the input with every site annotated by the
   solution, and its type.

`Spec/Inference.lean` states what `infer` guarantees. Every definition here is total, so that
the proofs can unfold them.
-/

namespace Determinize.Frontend
open Spec.Paper Checking

/-- Affinity variables: one for each float created during generation, and one for each float in
the decorated shape of a type variable. -/
inductive AffinityVar where
  | generated (index : Nat)
  | leaf (typeVar : Nat) (position : List Nat)
deriving DecidableEq, Repr

/-- Types during inference, over type variables and affinity terms. -/
inductive UType where
  | var (index : Nat)
  | unit | bool | float (affinity : AffinityTerm AffinityVar)
  | prod (a b : UType) | sum (a b : UType) | list (a : UType) | arr (a b : UType)
deriving Repr, Inhabited

/-- The type of a shape, with the affinity variable `leaf position` at the float at each position
(the list of child indices from the root). Shape variables stay type variables. -/
def Shape.decorate (leaf : List Nat → AffinityVar) : Shape → UType
  | .var i => .var i
  | .unit => .unit
  | .bool => .bool
  | .float => .float (.var (leaf []))
  | .prod a b => .prod (a.decorate (leaf <| 0 :: ·)) (b.decorate (leaf <| 1 :: ·))
  | .sum a b => .sum (a.decorate (leaf <| 0 :: ·)) (b.decorate (leaf <| 1 :: ·))
  | .list a => .list (a.decorate (leaf <| 0 :: ·))
  | .arr a b => .arr (a.decorate (leaf <| 0 :: ·)) (b.decorate (leaf <| 1 :: ·))

namespace UType

def shape : UType → Shape
  | var i => .var i
  | unit => .unit
  | bool => .bool
  | float _ => .float
  | prod a b => .prod a.shape b.shape
  | sum a b => .sum a.shape b.shape
  | list a => .list a.shape
  | arr a b => .arr a.shape b.shape

/-- Replace every type variable `α` by its shape `θ α`, decorated with the affinity variables
of `α`. -/
def decorate (θ : Nat → Shape) : UType → UType
  | var α => (θ α).decorate (.leaf α)
  | unit => unit
  | bool => bool
  | float a => float a
  | prod a b => prod (a.decorate θ) (b.decorate θ)
  | sum a b => sum (a.decorate θ) (b.decorate θ)
  | list a => list (a.decorate θ)
  | arr a b => arr (a.decorate θ) (b.decorate θ)

/-- The type with every type variable and every affinity variable replaced. -/
def instantiate (types : Nat → Ty) (affinities : AffinityVar → Affinity) : UType → Ty
  | var α => types α
  | unit => .unit
  | bool => .bool
  | float a => .float (a.eval affinities)
  | prod a b => .prod (a.instantiate types affinities) (b.instantiate types affinities)
  | sum a b => .sum (a.instantiate types affinities) (b.instantiate types affinities)
  | list a => .list (a.instantiate types affinities)
  | arr a b => .arr (a.instantiate types affinities) (b.instantiate types affinities)

/-- The affinity of a float type under `ρ`. Sample sites have float types; other types give G. -/
def affinity (ρ : AffinityVar → Affinity) : UType → Affinity
  | float a => a.eval ρ
  | _ => .G

end UType

/-- The atomic constraints under which the first of two types of the same shape is a subtype of
the second. -/
def decompose : UType → UType → List (AffinityConstraint AffinityVar)
  | .float a, .float b => [(a, b)]
  | .prod a b, .prod c d | .sum a b, .sum c d => decompose a c ++ decompose b d
  | .list a, .list b => decompose a b
  | .arr a b, .arr c d => decompose c a ++ decompose b d
  | _, _ => []
termination_by a b => sizeOf a + sizeOf b

/-- The program with a type for every node, and a cast wherever its typing uses subsumption. -/
inductive Draft where
  | node (expression : Input) (ty : UType) (children : List Draft)
  | cast (body : Draft) (ty : UType)

namespace Draft

def ty : Draft → UType
  | node _ t _ | cast _ t => t

/-- The subtyping constraints: the type of every cast body is a subtype of the cast type. -/
def relations : Draft → List (UType × UType)
  | node _ _ children => children.flatMap relations
  | cast body t => (body.ty, t) :: body.relations

end Draft

/-! ## Generation -/

/-- Generation draws type and affinity variables from one counter. It fails only on an unbound
variable. -/
abbrev Generate := StateT Nat (Except String)

def fresh : Generate UType :=
  modifyGet fun n => (.var n, n + 1)

def freshFloat : Generate UType :=
  modifyGet fun n => (.float (.var (.generated n)), n + 1)

/-- The type of an operand that the typing rules require to be G. -/
def general : UType := .float (.fixed .G)

/-- The type of a sample site: its requested affinity, or a fresh variable. -/
def site : Option Affinity → Generate UType
  | some affinity => pure (.float (.fixed affinity))
  | none => freshFloat

/-- Phase 1: the draft of `e` in the context `Γ`. -/
def generate (Γ : List UType) (e : Input) : Generate Draft := do
  let node := fun t children => Draft.node e t children
  match e with
  | .bvar i =>
    let some t := Γ[i]? | throw s!"unbound core variable {i}"
    return node t []
  | .reject => return node (← fresh) []
  | .unit => return node .unit []
  | .bool _ => return node .bool []
  | .real _ => return node (← freshFloat) []
  | .lam b =>
    let a ← fresh
    let b ← generate (a :: Γ) b
    return node (.arr a b.ty) [b]
  | .fix b =>
    let a ← fresh; let r ← fresh
    let b ← generate (a :: .arr a r :: Γ) b
    return node (.arr a r) [b.cast r]
  | .app f x =>
    let f ← generate Γ f; let x ← generate Γ x
    let a ← fresh; let r ← fresh
    return node r [f.cast (.arr a r), x.cast a]
  | .pair a b =>
    let a ← generate Γ a; let b ← generate Γ b
    return node (.prod a.ty b.ty) [a, b]
  | .fst p | .snd p =>
    let a ← fresh; let b ← fresh
    let p ← generate Γ p
    return node (match e with | .fst _ => a | _ => b) [p.cast (.prod a b)]
  | .inl v | .inr v =>
    let v ← generate Γ v; let other ← fresh
    return node (match e with | .inl _ => .sum v.ty other | _ => .sum other v.ty) [v]
  | .matchSum s a b =>
    let l ← fresh; let r ← fresh; let t ← fresh
    let s ← generate Γ s
    let a ← generate (l :: Γ) a
    let b ← generate (r :: Γ) b
    return node t [s.cast (.sum l r), a.cast t, b.cast t]
  | .nil => return node (.list (← fresh)) []
  | .cons h t =>
    let a ← fresh
    let h ← generate Γ h; let t ← generate Γ t
    return node (.list a) [h.cast a, t.cast (.list a)]
  | .matchList s n c =>
    let a ← fresh; let t ← fresh
    let s ← generate Γ s
    let n ← generate Γ n
    let c ← generate (a :: .list a :: Γ) c
    return node t [s.cast (.list a), n.cast t, c.cast t]
  | .ite c a b =>
    let t ← fresh
    let c ← generate Γ c; let a ← generate Γ a; let b ← generate Γ b
    return node t [c.cast .bool, a.cast t, b.cast t]
  | .letE v b =>
    let v ← generate Γ v; let b ← generate (v.ty :: Γ) b
    return node b.ty [v, b]
  | .neg b =>
    let t ← freshFloat
    let b ← generate Γ b
    return node t [b.cast t]
  | .add a b | .mul a b | .div a b | .lt a b =>
    let t ← freshFloat
    let ta := match e with | .mul .. | .lt .. => general | _ => t
    let tb := match e with | .div .. | .lt .. => general | _ => t
    let a ← generate Γ a; let b ← generate Γ b
    return node (match e with | .lt .. => .bool | _ => t) [a.cast ta, b.cast tb]
  | .uniform requested a b | .gaussian requested a b | .beta requested a b
  | .gamma requested a b =>
    let t ← site requested
    let ta := match e with | .beta .. => general | _ => t
    let tb := match e with | .uniform .. => t | _ => general
    let a ← generate Γ a; let b ← generate Γ b
    return node t [a.cast ta, b.cast tb]
  | .discrete requested probabilities =>
    let t ← site requested
    let probabilities ← generate Γ probabilities
    return node t [probabilities.cast (.list t)]
  | .poisson requested a | .bernoulli requested a | .exponential requested a =>
    let t ← site requested
    let ta := match e with | .poisson .. | .bernoulli .. => t | _ => general
    let a ← generate Γ a
    return node t [a.cast ta]

/-! ## Read-back -/

namespace Draft

/-- The core node for the input node `e` with the given children, sampling at affinity `m` if
it is a sample site. -/
def rebuild (e : Input) (m : Affinity) : List Core → Core
  | [] => match e with
    | .bvar i => .bvar i
    | .reject => .reject
    | .unit => .unit
    | .bool b => .bool b
    | .real q => .real q
    | .nil => .nil
    | _ => .unit
  | [b] => match e with
    | .lam _ => .lam b
    | .fix _ => .fix b
    | .fst _ => .fst b
    | .snd _ => .snd b
    | .inl _ => .inl b
    | .inr _ => .inr b
    | .neg _ => .neg b
    | .poisson .. => .poisson (.sample m) b
    | .discrete .. => .discrete (.sample m) b
    | .bernoulli .. => .bernoulli (.sample m) b
    | .exponential .. => .exponential (.sample m) b
    | _ => .unit
  | [a, b] => match e with
    | .app .. => .app a b
    | .pair .. => .pair a b
    | .cons .. => .cons a b
    | .letE .. => .letE a b
    | .add .. => .add a b
    | .mul .. => .mul a b
    | .div .. => .div a b
    | .lt .. => .lt a b
    | .uniform .. => .uniform (.sample m) a b
    | .gaussian .. => .gaussian (.sample m) a b
    | .beta .. => .beta (.sample m) a b
    | .gamma .. => .gamma (.sample m) a b
    | _ => .unit
  | [a, b, c] => match e with
    | .matchSum .. => .matchSum a b c
    | .matchList .. => .matchList a b c
    | .ite .. => .ite a b c
    | _ => .unit
  -- `generate` gives every node the children listed above.
  | _ => .unit

/-- The input with every sample site annotated by the affinity of its type under `ρ`. -/
def program (ρ : AffinityVar → Affinity) : Draft → Core
  | cast body _ => body.program ρ
  | node e t children => rebuild e (t.affinity ρ) (children.map (program ρ))

/-- The typing certificate: the type of every node, and a subsumption step at every cast that
changes the type. -/
def certificate (ty : UType → Ty) : Draft → Certificate
  | cast body t =>
    let c := body.certificate ty
    if c.ty == ty t then c else .sub (ty t) c
  | node _ t children => .node (ty t) (children.map (certificate ty))

end Draft

/-! ## Inference -/

/-- The outcome of phases 1 to 5: the draft, the most general shapes of its type variables, and
the greatest solution of the affinity constraints. -/
structure Solution where
  draft : Draft
  shapes : Nat → Shape
  affinities : AffinityVar → Affinity

/-- The type of a draft type in the solution. The shape variables left after decoration are
unconstrained and become `unit`. -/
def Solution.type (s : Solution) (t : UType) : Ty :=
  (t.decorate s.shapes).instantiate (fun _ => .unit) s.affinities

/-- Phases 1 to 5. Fails on an unbound variable, if the shapes have no unifier, or if the affinity
constraints have no solution. -/
def solveInput (input : Input) : Except String Solution := do
  let (draft, _) ← (generate [] input).run 0
  let relations := draft.relations
  let some shapes := unify (relations.map fun (s, t) => (s.shape, t.shape))
    | throw "incompatible or infinite type shapes"
  let constraints :=
    relations.flatMap fun (s, t) => decompose (s.decorate shapes) (t.decorate shapes)
  let some affinities := solveAffinities constraints
    | throw "inconsistent E/G constraints"
  return { draft, shapes, affinities }

/-- The annotated program and its type. `Spec/Inference.lean` states what it guarantees. -/
def infer (input : Input) : Except String (Core × Ty) := do
  let s ← solveInput input
  return (s.draft.program s.affinities, s.type s.draft.ty)

/-- Inference together with the typing certificate that `certify` checks. -/
def inferWithCertificate (input : Input) : Except String (Core × Certificate) := do
  let s ← solveInput input
  return (s.draft.program s.affinities, s.draft.certificate s.type)

end Determinize.Frontend
