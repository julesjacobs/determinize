import Determinize.Frontend.Infer

/-!
# Bunched affinity inference

The inference of `extensions/main.tex`, alongside the verified `infer` of `Frontend/Infer.lean`.
Only generation differs: a node also has its dependencies, the set of sample sites it depends
on, and generation records two more kinds of constraint, that a site has its requested affinity
and that the operands of a product are separate. Shapes, decomposition, the affinity solver and
read-back are those of `infer`.

No theorem about `infer` applies here, and there is no correspondence proof yet against
`Spec.Paper.Bunched.Typed` (whose Mul-G rule is left-sided, while inference treats both operands
of a product alike).
-/

namespace Determinize.Frontend.Bunched
open Spec.Paper

/-- The sample sites a node depends on, without duplicates. A site is identified by its affinity
variable. -/
abbrev Dependencies := List AffinityVar

/-- `a = m`, as `a ≤ m` and `m ≤ a`. -/
def equal (a : AffinityVar) (m : Affinity) : List (AffinityConstraint AffinityVar) :=
  [(.var a, .fixed m), (.fixed m, .var a)]

/-- The reduction of `sep(B₁, B₂)`: every shared site is G. In the order `G ≤ E`, `a ≤ G` is
`a = G`. -/
def sep (B₁ B₂ : Dependencies) : List (AffinityConstraint AffinityVar) :=
  (B₁ ∩ B₂).map fun a ↦ (.var a, .fixed .G)

/-- `Frontend.Generate` that also records the constraints that are not subtyping. -/
abbrev Generate := StateT (List (AffinityConstraint AffinityVar)) Frontend.Generate

def require (constraints : List (AffinityConstraint AffinityVar)) : Generate Unit :=
  modify (· ++ constraints)

/-- A sample site: a fresh affinity variable, which identifies the site in dependency sets, set
equal to the requested affinity if there is one. Unlike `Frontend.site`, a requested affinity is
not put in the type directly, since the site would then have no variable to be identified by. -/
def site (requested : Option Affinity) : Generate (UType × AffinityVar) := do
  let a ← (modifyGet fun n ↦ (AffinityVar.generated n, n + 1) : Frontend.Generate _)
  if let some m := requested then require (equal a m)
  return (.float (.var a), a)

/-- Phase 1: the draft of `e` and its dependencies in the context `Δ` of types and dependencies.
Fails on constructs that the paper has no rule for. -/
def generate (Δ : List (UType × Dependencies)) (e : Input) : Generate (Draft × Dependencies) := do
  let node := fun t children ↦ Draft.node e t children
  match e with
  | .bvar i =>
    let some (t, B) := Δ[i]? | throw s!"unbound core variable {i}"
    return (node t [], B)
  | .unit => return (node .unit [], [])
  | .bool _ => return (node .bool [], [])
  | .real _ => return (node (← freshFloat) [], [])
  | .ite c a b =>
    let t ← fresh
    let (c, B₀) ← generate Δ c; let (a, B₁) ← generate Δ a; let (b, B₂) ← generate Δ b
    return (node t [c.cast .bool, a.cast t, b.cast t], B₀ ∪ B₁ ∪ B₂)
  | .letE v b =>
    let (v, B₁) ← generate Δ v; let (b, B₂) ← generate ((v.ty, B₁) :: Δ) b
    return (node b.ty [v, b], B₂)
  | .add a b | .mul a b | .lt a b =>
    let t ← freshFloat
    let operand := match e with | .lt .. => general | _ => t
    let (a, B₁) ← generate Δ a; let (b, B₂) ← generate Δ b
    if e matches .mul .. then require (sep B₁ B₂)
    return (node (match e with | .lt .. => .bool | _ => t) [a.cast operand, b.cast operand],
      B₁ ∪ B₂)
  | .uniform requested a b | .gaussian requested a b =>
    let (t, s) ← site requested
    let tb := match e with | .uniform .. => t | _ => general
    let (a, B₁) ← generate Δ a; let (b, B₂) ← generate Δ b
    return (node t [a.cast t, b.cast tb], B₁ ∪ B₂ ∪ [s])
  | .bernoulli requested a =>
    let (t, s) ← site requested
    let (a, B) ← generate Δ a
    return (node t [a.cast t], B ∪ [s])
  | _ => throw "not supported yet"

/-- Phases 1 to 5, as in `Frontend.solveInput`, with the recorded constraints added. -/
def solveInput (input : Input) : Except String Solution := do
  let ((draft, _), recorded) ← ((generate [] input).run []).run' 0
  let relations := draft.relations
  let some shapes := unify (relations.map fun (s, t) ↦ (s.shape, t.shape))
    | throw "incompatible or infinite type shapes"
  let constraints :=
    relations.flatMap (fun (s, t) ↦ decompose (s.decorate shapes) (t.decorate shapes)) ++ recorded
  let some affinities := solveAffinities constraints
    | throw "inconsistent E/G or separation constraints"
  return { draft, shapes, affinities }

/-- The annotated program and its type. -/
def infer (input : Input) : Except String (Annotated × Ty) := do
  let s ← solveInput input
  return (s.draft.program s.affinities, s.type s.draft.ty)

end Determinize.Frontend.Bunched
