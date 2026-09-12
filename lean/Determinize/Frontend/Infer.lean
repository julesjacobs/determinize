import Determinize.Frontend.Syntax

namespace Determinize.Frontend
open Spec.Paper Checking

private inductive UType where
  | var (id : Nat)
  | unit | bool | float (affinity : Nat)
  | prod (a b : UType) | sum (a b : UType) | list (a : UType) | arr (a b : UType)
deriving Repr, Inhabited

private inductive Draft where
  | node (expression : Input) (ty : UType) (children : List Draft)
  | cast (body : Draft) (ty : UType)
private def Draft.ty : Draft → UType
  | .node _ t _ | .cast _ t => t

private structure InferState where
  types : Array (Option UType) := #[]
  affinities : Array (Option Affinity) := #[]
  constraints : List (Nat × Nat) := []
private abbrev M := StateT InferState (Except String)

private def fresh : M UType := do
  let i := (← get).types.size
  modify fun s => {s with types := s.types.push none}
  return .var i
private def freshAffinity : M Nat := do
  let i := (← get).affinities.size
  modify fun s => {s with affinities := s.affinities.push none}
  return i
private def float : M UType := return .float (← freshAffinity)
private def force (i : Nat) (m : Affinity) : M Unit := do
  match ((← get).affinities[i]?).getD none with
  | some n => unless n == m do throw "inconsistent E/G requirements"
  | none => modify fun s => {s with affinities := s.affinities.set! i (some m)}
private def general : M UType := do
  let i ← freshAffinity; force i .G; return .float i
private partial def resolve : UType → M UType
  | .var i => do
    match ((← get).types[i]?).getD none with
    | none => return .var i
    | some t => resolve t
  | t => pure t
private partial def occurs (i : Nat) (t : UType) : M Bool := do
  match (← resolve t) with
  | .var j => return i == j
  | .prod a b | .sum a b | .arr a b => return (← occurs i a) || (← occurs i b)
  | .list a => occurs i a
  | _ => return false
private def bindType (i : Nat) (t : UType) : M Unit := do
  if (← occurs i t) then throw "infinite type (occurs check)"
  modify fun s => {s with types := s.types.set! i (some t)}
private def leAffinity (a b : Nat) : M Unit :=
  modify fun s => {s with constraints := (a,b) :: s.constraints}

private partial def freshAffinities (t : UType) : M UType := do
  match ← resolve t with
  | .float _ => float
  | .prod a b => return .prod (← freshAffinities a) (← freshAffinities b)
  | .sum a b => return .sum (← freshAffinities a) (← freshAffinities b)
  | .list a => return .list (← freshAffinities a)
  | .arr a b => return .arr (← freshAffinities a) (← freshAffinities b)
  | t => return t

/-- Structural subtyping constraints, with contravariant function arguments. -/
private partial def relate (a b : UType) : M Unit := do
  let a ← resolve a; let b ← resolve b
  match a,b with
  | .var i, .var j => if i != j then bindType i b
  | .var i, _ =>
      let shape ← freshAffinities b
      bindType i shape
      relate shape b
  | _, .var i =>
      let shape ← freshAffinities a
      bindType i shape
      relate a shape
  | .unit,.unit | .bool,.bool => pure ()
  | .float a,.float b => leAffinity a b
  | .prod a b,.prod c d | .sum a b,.sum c d => relate a c; relate b d
  | .arr a b,.arr c d => relate c a; relate b d
  | .list a,.list b => relate a b
  | _,_ => throw s!"incompatible types {repr a} and {repr b}"
private def require (d : Draft) (t : UType) : M Draft := do
  relate d.ty t
  return .cast d t

private def siteAffinity (requested : Option Affinity) : M Nat := do
  let i ← freshAffinity
  if let some affinity := requested then force i affinity
  return i

private partial def inferExpr (Γ : List UType) (e : Input) : M Draft := do
  let node := fun t cs => Draft.node e t cs
  match e with
  | .bvar i =>
    let some t := Γ[i]? | throw s!"unbound core variable {i}"
    return node t []
  | .reject => return node (← fresh) []
  | .unit => return node .unit []
  | .bool _ => return node .bool []
  | .real _ => return node (← float) []
  | .lam b =>
    let a ← fresh; let b ← inferExpr (a :: Γ) b
    return node (.arr a b.ty) [b]
  | .fix b =>
    let a ← fresh; let r ← fresh
    let b ← require (← inferExpr (a :: .arr a r :: Γ) b) r
    return node (.arr a r) [b]
  | .app f x =>
    let f ← inferExpr Γ f; let x ← inferExpr Γ x
    let a ← fresh; let r ← fresh
    let f ← require f (.arr a r); let x ← require x a
    return node r [f,x]
  | .pair a b =>
    let a ← inferExpr Γ a; let b ← inferExpr Γ b
    return node (.prod a.ty b.ty) [a,b]
  | .fst p | .snd p =>
    let a ← fresh; let b ← fresh
    let p ← require (← inferExpr Γ p) (.prod a b)
    return node (match e with | .fst _ => a | _ => b) [p]
  | .inl v | .inr v =>
    let v ← inferExpr Γ v; let other ← fresh
    return node (match e with | .inl _ => .sum v.ty other | _ => .sum other v.ty) [v]
  | .matchSum s a b =>
    let l ← fresh; let r ← fresh; let t ← fresh
    let s ← require (← inferExpr Γ s) (.sum l r)
    let a ← require (← inferExpr (l :: Γ) a) t
    let b ← require (← inferExpr (r :: Γ) b) t
    return node t [s,a,b]
  | .nil => return node (.list (← fresh)) []
  | .cons h t =>
    let a ← fresh
    let h ← require (← inferExpr Γ h) a
    let t ← require (← inferExpr Γ t) (.list a)
    return node (.list a) [h,t]
  | .matchList s n c =>
    let a ← fresh; let t ← fresh
    let s ← require (← inferExpr Γ s) (.list a)
    let n ← require (← inferExpr Γ n) t
    let c ← require (← inferExpr (a :: .list a :: Γ) c) t
    return node t [s,n,c]
  | .ite c a b =>
    let t ← fresh
    let c ← require (← inferExpr Γ c) .bool
    let a ← require (← inferExpr Γ a) t
    let b ← require (← inferExpr Γ b) t
    return node t [c,a,b]
  | .letE v b =>
    let v ← inferExpr Γ v; let b ← inferExpr (v.ty :: Γ) b
    return node b.ty [v,b]
  | .neg b =>
    let t ← float
    return node t [← require (← inferExpr Γ b) t]
  | .add a b | .mul a b | .div a b | .lt a b =>
    let t ← float; let g ← general
    let ta := match e with | .mul .. | .lt .. => g | _ => t
    let tb := match e with | .div .. | .lt .. => g | _ => t
    let a ← require (← inferExpr Γ a) ta
    let b ← require (← inferExpr Γ b) tb
    return node (match e with | .lt .. => .bool | _ => t) [a,b]
  | .uniform requested a b | .gaussian requested a b | .beta requested a b | .gamma requested a b =>
    let i ← siteAffinity requested; let t := UType.float i; let g ← general
    let ta := match e with | .beta .. => g | _ => t
    let tb := match e with | .uniform .. => t | _ => g
    let a ← require (← inferExpr Γ a) ta
    let b ← require (← inferExpr Γ b) tb
    return node t [a,b]
  | .discrete requested _ => do
    let i ← siteAffinity requested
    return node (.float i) []
  | .poisson requested a | .bernoulli requested a | .exponential requested a =>
    let i ← siteAffinity requested; let t := UType.float i; let g ← general
    let ta := match e with | .poisson .. | .bernoulli .. => t | _ => g
    return node t [← require (← inferExpr Γ a) ta]

private def solve : M Unit := do
  for _ in [:((← get).affinities.size + 1)] do
    for (a,b) in (← get).constraints do
      if ((← get).affinities[a]?).getD none == some .E then force b .E
      if ((← get).affinities[b]?).getD none == some .G then force a .G
  for i in [:((← get).affinities.size)] do
    if ((← get).affinities[i]?).getD none == none then force i .E
  for (a,b) in (← get).constraints do
    if ((← get).affinities[a]?).getD none == some .E && ((← get).affinities[b]?).getD none == some .G then
      throw "inconsistent E/G constraints"
private partial def finalType (t : UType) : M Ty := do
  match (← resolve t) with
  | .var _ => return .unit
  | .unit => return .unit
  | .bool => return .bool
  | .float i => return .float (((← get).affinities[i]?).getD none |>.getD .E)
  | .prod a b => return .prod (← finalType a) (← finalType b)
  | .sum a b => return .sum (← finalType a) (← finalType b)
  | .arr a b => return .arr (← finalType a) (← finalType b)
  | .list a => return .list (← finalType a)

private partial def finish : Draft → M (Core × Certificate)
  | .cast b t => do
    let (e,c) ← finish b; let t ← finalType t
    if c.ty == t then return (e,c)
    return (e, .sub t c)
  | .node e t ds => do
    let t ← finalType t
    let pairs ← ds.mapM finish
    let es := pairs.map Prod.fst; let cs := pairs.map Prod.snd
    let m := match t with | .float m => m | _ => .G
    let e ← match e,es with
      | .bvar i,[] => pure (.bvar i)
      | .reject,[] => pure .reject
      | .unit,[] => pure .unit
      | .bool b,[] => pure (.bool b)
      | .real q,[] => pure (.real q)
      | .nil,[] => pure .nil
      | .lam _,[b] => pure (.lam b)
      | .fix _,[b] => pure (.fix b)
      | .app ..,[a,b] => pure (.app a b)
      | .pair ..,[a,b] => pure (.pair a b)
      | .fst _,[b] => pure (.fst b)
      | .snd _,[b] => pure (.snd b)
      | .inl _,[b] => pure (.inl b)
      | .inr _,[b] => pure (.inr b)
      | .matchSum ..,[s,a,b] => pure (.matchSum s a b)
      | .cons ..,[a,b] => pure (.cons a b)
      | .matchList ..,[s,n,c] => pure (.matchList s n c)
      | .ite ..,[c,a,b] => pure (.ite c a b)
      | .letE ..,[a,b] => pure (.letE a b)
      | .neg _,[b] => pure (.neg b)
      | .add ..,[a,b] => pure (.add a b)
      | .mul ..,[a,b] => pure (.mul a b)
      | .div ..,[a,b] => pure (.div a b)
      | .lt ..,[a,b] => pure (.lt a b)
      | .uniform _ ..,[a,b] => pure (.uniform (.sample m) a b)
      | .gaussian _ ..,[a,b] => pure (.gaussian (.sample m) a b)
      | .poisson _ ..,[a] => pure (.poisson (.sample m) a)
      | .discrete _ d,[] => pure (.discrete (.sample m) d)
      | .bernoulli _ ..,[a] => pure (.bernoulli (.sample m) a)
      | .exponential _ ..,[a] => pure (.exponential (.sample m) a)
      | .beta _ ..,[a,b] => pure (.beta (.sample m) a b)
      | .gamma _ ..,[a,b] => pure (.gamma (.sample m) a b)
      | _,_ => throw "internal annotation shape mismatch"
    return (e,.node t cs)

def infer (input : Input) : Except String (Core × Certificate) := do
  let (result, _) ← (do
    let draft ← inferExpr [] input
    solve
    finish draft).run {}
  return result

end Determinize.Frontend
