import Determinize.Frontend.Compile
import Determinize.Frontend.Pretty

/-!
# Differential test of the step-3 rewrite of `infer`

Compares `Frontend.inferWithCertificate` with the implementation that step 3 replaced, copied
below as `OldInfer` from commit `465895c` with only its namespace changed. The inputs are every
program of the brute-force checker's enumerations (`Checker.lean`), its hand-written programs and
the corpus, each together with every completion (every E/G assignment to its unannotated sites,
written as explicit annotations). The two implementations must return the same annotated
program and the same certificate, or both fail; error messages may differ.

Run from `lean/`:

    lake env lean --run ../notes/inference-optimality/Differential.lean --targeted
    lake env lean --run ../notes/inference-optimality/Differential.lean --corpus
    lake env lean --run ../notes/inference-optimality/Differential.lean --enumerate core 6
    lake env lean --run ../notes/inference-optimality/Differential.lean --enumerate full 5

The corpus run skips the completions of programs with more than 10 unannotated sites. The file
can go when `inferWithCertificate` does (step 5).
-/

namespace OldInfer
open Determinize Determinize.Spec.Paper Determinize.Checking

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
  relations : List (UType × UType) := []
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

private def relate (a b : UType) : M Unit :=
  modify fun s => {s with relations := (a,b) :: s.relations}

private partial def unifyShape (a b : UType) : M Unit := do
  let a ← resolve a; let b ← resolve b
  match a,b with
  | .var i, .var j => if i != j then bindType i b
  | .var i, _ => bindType i b
  | _, .var i => bindType i a
  | .unit,.unit | .bool,.bool | .float _,.float _ => pure ()
  | .prod a b,.prod c d | .sum a b,.sum c d | .arr a b,.arr c d =>
      unifyShape a c; unifyShape b d
  | .list a,.list b => unifyShape a b
  | _,_ => throw "incompatible type shapes"

private def freshHead : UType → M UType
  | .unit => pure .unit
  | .bool => pure .bool
  | .float _ => float
  | .prod _ _ => return .prod (← fresh) (← fresh)
  | .sum _ _ => return .sum (← fresh) (← fresh)
  | .arr _ _ => return .arr (← fresh) (← fresh)
  | .list _ => return .list (← fresh)
  | .var _ => throw "expected type constructor"

private partial def lowerRelations
    (queue parked : List (UType × UType)) : M Unit := do
  match queue with
  | [] => pure ()
  | (a,b) :: rest =>
    let a ← resolve a; let b ← resolve b
    match a,b with
    | .var i, .var j =>
      lowerRelations rest (if i == j then parked else (a,b) :: parked)
    | .var i, _ =>
      bindType i (← freshHead b)
      lowerRelations ((a,b) :: (parked ++ rest)) []
    | _, .var i =>
      bindType i (← freshHead a)
      lowerRelations ((a,b) :: (parked ++ rest)) []
    | .unit,.unit | .bool,.bool => lowerRelations rest parked
    | .float p,.float q => leAffinity p q; lowerRelations rest parked
    | .prod a b,.prod c d | .sum a b,.sum c d =>
      lowerRelations ((a,c) :: (b,d) :: rest) parked
    | .arr a b,.arr c d => lowerRelations ((c,a) :: (b,d) :: rest) parked
    | .list a,.list b => lowerRelations ((a,b) :: rest) parked
    | _,_ => throw "incompatible types"

private def solveRelations : M Unit := do
  let s ← get
  -- Weak unification ensures finite shapes before expansion (Traytel et al., APLAS 2011).
  -- Its substitution must stay separate: equal shapes may have different affinities.
  let _ ← (s.relations.forM fun (a,b) => unifyShape a b).run
    {types := Array.replicate s.types.size none}
  lowerRelations s.relations []

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
  | .discrete requested probabilities => do
    let i ← siteAffinity requested
    let probabilities ← require (← inferExpr Γ probabilities) (.list (.float i))
    return node (.float i) [probabilities]
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
      | .discrete _ ..,[p] => pure (.discrete (.sample m) p)
      | .bernoulli _ ..,[a] => pure (.bernoulli (.sample m) a)
      | .exponential _ ..,[a] => pure (.exponential (.sample m) a)
      | .beta _ ..,[a,b] => pure (.beta (.sample m) a b)
      | .gamma _ ..,[a,b] => pure (.gamma (.sample m) a b)
      | _,_ => throw "internal annotation shape mismatch"
    return (e,.node t cs)

/-- Inference together with the typing certificate that `certify` checks. -/
def inferWithCertificate (input : Input) : Except String (Core × Certificate) := do
  let (result, _) ← (do
    let draft ← inferExpr [] input
    solveRelations
    solve
    finish draft).run {}
  return result

/-- The annotated program and its type. `Spec/Inference.lean` states what it guarantees. -/
def infer (input : Input) : Except String (Core × Ty) := do
  let (source, certificate) ← inferWithCertificate input
  return (source, certificate.ty)

end OldInfer

open Determinize Determinize.Frontend Determinize.Checking Determinize.Spec.Paper

namespace Differential
/-- Requested affinities of the sample sites, in the order used by `sampleAffinities`. -/
def sites : Input → List (Option Affinity)
  | .bvar _ | .reject | .unit | .bool _ | .real _ | .nil => []
  | .lam b | .fix b | .fst b | .snd b | .inl b | .inr b | .neg b => sites b
  | .app a b | .pair a b | .cons a b | .letE a b | .add a b | .mul a b | .div a b | .lt a b =>
      sites a ++ sites b
  | .matchSum a b c | .matchList a b c | .ite a b c => sites a ++ sites b ++ sites c
  | .uniform r a b | .gaussian r a b | .beta r a b | .gamma r a b => r :: (sites a ++ sites b)
  | .poisson r a | .discrete r a | .bernoulli r a | .exponential r a => r :: sites a

private def takeSite (r : Option Affinity) : List Affinity → Affinity × List Affinity
  | as => match r with
    | some a => (a, as)
    | none => match as with
      | a :: as => (a, as)
      | [] => (.E, [])

/-- Fill the unannotated sites, in site order, from the list; explicit sites keep their annotation. -/
def fill : Input → List Affinity → Core × List Affinity
  | .bvar i, as => (.bvar i, as)
  | .reject, as => (.reject, as)
  | .unit, as => (.unit, as)
  | .bool b, as => (.bool b, as)
  | .real q, as => (.real q, as)
  | .nil, as => (.nil, as)
  | .lam b, as => let (b, as) := fill b as; (.lam b, as)
  | .fix b, as => let (b, as) := fill b as; (.fix b, as)
  | .fst b, as => let (b, as) := fill b as; (.fst b, as)
  | .snd b, as => let (b, as) := fill b as; (.snd b, as)
  | .inl b, as => let (b, as) := fill b as; (.inl b, as)
  | .inr b, as => let (b, as) := fill b as; (.inr b, as)
  | .neg b, as => let (b, as) := fill b as; (.neg b, as)
  | .app a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.app a b, as)
  | .pair a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.pair a b, as)
  | .cons a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.cons a b, as)
  | .letE a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.letE a b, as)
  | .add a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.add a b, as)
  | .mul a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.mul a b, as)
  | .div a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.div a b, as)
  | .lt a b, as => let (a, as) := fill a as; let (b, as) := fill b as; (.lt a b, as)
  | .matchSum a b c, as =>
      let (a, as) := fill a as; let (b, as) := fill b as; let (c, as) := fill c as
      (.matchSum a b c, as)
  | .matchList a b c, as =>
      let (a, as) := fill a as; let (b, as) := fill b as; let (c, as) := fill c as
      (.matchList a b c, as)
  | .ite a b c, as =>
      let (a, as) := fill a as; let (b, as) := fill b as; let (c, as) := fill c as
      (.ite a b c, as)
  | .uniform r a b, as =>
      let (m, as) := takeSite r as; let (a, as) := fill a as; let (b, as) := fill b as
      (.uniform (.sample m) a b, as)
  | .gaussian r a b, as =>
      let (m, as) := takeSite r as; let (a, as) := fill a as; let (b, as) := fill b as
      (.gaussian (.sample m) a b, as)
  | .beta r a b, as =>
      let (m, as) := takeSite r as; let (a, as) := fill a as; let (b, as) := fill b as
      (.beta (.sample m) a b, as)
  | .gamma r a b, as =>
      let (m, as) := takeSite r as; let (a, as) := fill a as; let (b, as) := fill b as
      (.gamma (.sample m) a b, as)
  | .poisson r a, as => let (m, as) := takeSite r as; let (a, as) := fill a as; (.poisson (.sample m) a, as)
  | .discrete r a, as => let (m, as) := takeSite r as; let (a, as) := fill a as; (.discrete (.sample m) a, as)
  | .bernoulli r a, as => let (m, as) := takeSite r as; let (a, as) := fill a as; (.bernoulli (.sample m) a, as)
  | .exponential r a, as => let (m, as) := takeSite r as; let (a, as) := fill a as; (.exponential (.sample m) a, as)

private def explicit : DistributionAction → Option Affinity
  | .sample a => some a
  | .mean => none

/-- The input whose every site is explicitly annotated as in the core term. -/
def toInput : Core → Input
  | .bvar i => .bvar i
  | .reject => .reject
  | .unit => .unit
  | .bool b => .bool b
  | .real q => .real q
  | .nil => .nil
  | .lam b => .lam (toInput b)
  | .fix b => .fix (toInput b)
  | .fst b => .fst (toInput b)
  | .snd b => .snd (toInput b)
  | .inl b => .inl (toInput b)
  | .inr b => .inr (toInput b)
  | .neg b => .neg (toInput b)
  | .app a b => .app (toInput a) (toInput b)
  | .pair a b => .pair (toInput a) (toInput b)
  | .cons a b => .cons (toInput a) (toInput b)
  | .letE a b => .letE (toInput a) (toInput b)
  | .add a b => .add (toInput a) (toInput b)
  | .mul a b => .mul (toInput a) (toInput b)
  | .div a b => .div (toInput a) (toInput b)
  | .lt a b => .lt (toInput a) (toInput b)
  | .matchSum a b c => .matchSum (toInput a) (toInput b) (toInput c)
  | .matchList a b c => .matchList (toInput a) (toInput b) (toInput c)
  | .ite a b c => .ite (toInput a) (toInput b) (toInput c)
  | .uniform k a b => .uniform (explicit k) (toInput a) (toInput b)
  | .gaussian k a b => .gaussian (explicit k) (toInput a) (toInput b)
  | .beta k a b => .beta (explicit k) (toInput a) (toInput b)
  | .gamma k a b => .gamma (explicit k) (toInput a) (toInput b)
  | .poisson k a => .poisson (explicit k) (toInput a)
  | .discrete k a => .discrete (explicit k) (toInput a)
  | .bernoulli k a => .bernoulli (explicit k) (toInput a)
  | .exponential k a => .exponential (explicit k) (toInput a)

def allAssignments : Nat → List (List Affinity)
  | 0 => [[]]
  | n + 1 => (allAssignments n).flatMap fun as => [.E :: as, .G :: as]

structure Grammar where
  data : Bool := false

def leaves (g : Grammar) (depth : Nat) : List Input :=
  (List.range depth).map .bvar ++ [.real 1, .bool true] ++ (if g.data then [.nil, .reject] else [])

/-- `table[s][d]`: every term with `s` constructor nodes under `d` bound variables. -/
def build (g : Grammar) (maxSize : Nat) : Array (Array (List Input)) := Id.run do
  let maxDepth := 2 * maxSize + 1
  let mut table : Array (Array (List Input)) :=
    Array.replicate (maxSize + 1) (Array.replicate (maxDepth + 1) [])
  for s in [1:maxSize + 1] do
    -- a term of size `s` is only ever placed under at most `2 * (maxSize - s)` binders
    for d in [0:2 * (maxSize - s) + 1] do
      let mut acc : List Input := []
      if s == 1 then
        acc := leaves g d
      else
        let get := fun (s' d' : Nat) => ((table[s']?.getD #[])[d']?).getD []
        for c in get (s - 1) (d + 1) do acc := .lam c :: acc
        for c in get (s - 1) (d + 2) do acc := .fix c :: acc
        for c in get (s - 1) d do
          acc := .neg c :: .poisson none c :: .exponential none c :: acc
          if g.data then
            acc := .fst c :: .snd c :: .inl c :: .inr c :: .discrete none c :: .bernoulli none c ::
              .poisson (some .G) c :: .exponential (some .E) c :: acc
        for i in [1:s - 1] do
          let j := s - 1 - i
          for a in get i d do
            for b in get j d do
              acc := .app a b :: .add a b :: .mul a b :: .div a b :: .lt a b ::
                .uniform none a b :: .uniform (some .E) a b :: .uniform (some .G) a b ::
                .gaussian none a b :: acc
              if g.data then
                acc := .pair a b :: .cons a b :: .beta none a b :: .gamma none a b ::
                  .gaussian (some .E) a b :: acc
            for b in get j (d + 1) do acc := .letE a b :: acc
        for i in [1:s - 2] do
          for j in [1:s - 1 - i] do
            let k := s - 1 - i - j
            for a in get i d do
              for b in get j d do
                for c in get k d do acc := .ite a b c :: acc
                if g.data then
                  for c in get k (d + 2) do acc := .matchList a b c :: acc
              if g.data then
                for b in get j (d + 1) do
                  for c in get k (d + 1) do acc := .matchSum a b c :: acc
      table := table.set! s ((table[s]?.getD #[]).set! d acc)
  return table

/-! ## Hand-written programs -/

def targeted : List String := [
  -- forcing through first-order and higher-order functions
  "let f = fun x => x * x in f (uniform(0,1))",
  "(fun g => g (uniform(0,1))) (fun x => x * x)",
  "let apply = fun f => f (uniform(0,1)) in apply (fun x => x * x) + apply (fun x => x)",
  "let apply = fun f => f (uniform(0,1)) in apply (fun x => x) + apply (fun x => x * x)",
  "let f = fun x => x in f (uniform(0,1)) + f (uniform(0,1))",
  "let f = fun x => x in f (uniform(0,1)) * f (uniform(0,1))",
  "let f = fun x => x in f (uniform(0,1)) * f (uniform[E](0,1))",
  "let use = fun f => fun x => f x + f (uniform(0,1)) + x*x in use (fun z => z) (uniform[G](0,1))",
  "let use = fun f => fun x => f (uniform(0,1)) + f x + x*x in use (fun z => z) (uniform(0,1))",
  "let twice = fun f => fun x => f (f x) in twice (fun y => y * y) (uniform(0,1))",
  "let twice = fun f => fun x => f (f x) in twice (fun y => y + 1) (uniform(0,1))",
  "let compose = fun f => fun g => fun x => f (g x) in compose (fun y => y * y) (fun z => z + 1) (uniform(0,1))",
  "let compose = fun f => fun g => fun x => f (g x) in compose (fun y => y + 1) (fun z => z * z) (uniform(0,1))",
  "let h = fun k => k (fun x => x * x) in h (fun f => f (uniform(0,1)))",
  "let h = fun k => k (fun x => x + x) in h (fun f => f (uniform(0,1)))",
  -- recursion
  "(rec f x => if x < 1 then uniform(0,1) else f (x - 1)) 3",
  "(rec f x => if x < 1 then x else f (uniform(0,1))) 3",
  "(rec f x => if x < 1 then x else f (x + uniform(0,1))) 3",
  "(rec f x => if x < 1 then uniform(0,1) else uniform(0,1) + f (x - 1)) 3",
  "let f = rec f x => if x < 1 then x else f (x - 1) in f (uniform(0,1)) + uniform(0,1)",
  "let f = rec f x => if x < 1 then 0 else f (x - 1) + 1 in f (uniform(0,1)) * uniform(0,1)",
  -- branch order and functions in branches
  "let f = if true then (fun x => x * x) else (fun x => x) in f (uniform(0,1))",
  "let f = if true then (fun x => x) else (fun x => x * x) in f (uniform(0,1))",
  "let f = if uniform(0,1) < 0.5 then (fun x => x) else (fun x => x * x) in f (uniform(0,1))",
  "if true then uniform(0,1) else uniform(0,1) * uniform(0,1)",
  "if true then uniform(0,1) * uniform(0,1) else uniform(0,1)",
  "let x = uniform(0,1) in if x < 0.5 then x else 0",
  "let x = uniform(0,1) in if 0 < 0.5 then x else 0",
  -- data structures
  "let p = (uniform(0,1), uniform(0,1)) in fst p * snd p",
  "let p = (uniform(0,1), uniform(0,1)) in snd p * fst p",
  "let xs = uniform(0,1) :: uniform(0,1) :: [] in match xs with [] => 0 | y :: ys => y * y",
  "let xs = uniform(0,1) :: uniform(0,1) :: [] in match xs with [] => 0 | y :: ys => y + 1",
  "match (if true then inl (uniform(0,1)) else inr 1) with inl x => x * x | inr y => y",
  "match (if true then inl (uniform(0,1)) else inr 1) with inl x => x + x | inr y => y",
  "let xs = uniform(0,1) :: uniform[G](0,1) :: [] in match xs with [] => 0 | y :: ys => y",
  "let xs = uniform[G](0,1) :: uniform(0,1) :: [] in match xs with [] => 0 | y :: ys => y",
  "discrete_list(uniform(0,1) :: []) + uniform(0,1)",
  "discrete_list(uniform(0,1) :: []) * uniform(0,1)",
  "uniform(0,1) * discrete_list(uniform(0,1) :: [])",
  -- sinks and explicit annotations
  "uniform(0,1) * uniform(0,1)",
  "uniform(0,1) / uniform(0,1)",
  "gauss(uniform(0,1), uniform(0,1))",
  "exponential(uniform(0,1)) + uniform(0,1)",
  "beta(uniform(0,1), uniform(0,1))",
  "gamma(uniform(0,1), uniform(0,1))",
  "poisson(uniform(0,1)) + bernoulli(uniform(0,1))",
  "uniform(uniform(0,1), uniform[G](0,1))",
  "uniform[G](uniform(0,1), 1)",
  "uniform[E](uniform(0,1), 1) * uniform(0,1)",
  "let x = uniform[E](0,1) in uniform(x, 1) * uniform(0,1)",
  "let x = uniform[E](0,1) in x * x",
  "uniform[E](0,1) < 0.5",
  "flip(uniform(0,1))",
  "if flip(0.5) then uniform(0,1) else uniform(0,1) + 1",
  "observe(uniform(0,1) < 0.5)",
  "let x = uniform(0,1) in observe(x < 0.5)",
  "let x = uniform(0,1) in let y = observe(x < 0.5) in x",
  "fun x => x x",
  "fun x => let f = fun y => x :: y in f x",
  "fun f => let g = fun x => f x in g f",
  -- non-float results
  "fun x => x",
  "(uniform(0,1), uniform(0,1) * uniform(0,1))",
  "uniform(0,1) :: uniform(0,1) * uniform(0,1) :: []",
  "inl (uniform(0,1) * uniform(0,1))",
  "fun x => uniform(0,1) * x",
  "fun x => uniform(0,1) * uniform(0,1)",
  "(fun x => uniform(0,1)) 1 + (fun x => uniform(0,1) * uniform(0,1)) 1"
]


/-- Compare the old and the new inference exactly (program and certificate; errors only as errors). -/
def same (e : Input) : Bool :=
  match OldInfer.inferWithCertificate e, inferWithCertificate e with
  | .ok (p, c), .ok (p', c') => p == p' && c == c'
  | .error _, .error _ => true
  | _, _ => false

def describe (e : Input) : String :=
  let render := fun (r : Except String (Core × Certificate)) => match r with
    | .ok (p, c) => s!"ok {Frontend.pretty p} : {Frontend.prettyType c.ty} / {reprStr c}"
    | .error m => s!"error {m}"
  s!"{reprStr e}\n  old: {render (OldInfer.inferWithCertificate e)}\n  new: {render (inferWithCertificate e)}"

/-- Every completion of `e` as a fully annotated input. -/
def completions (e : Input) : List Input :=
  let free := ((sites e).filter Option.isNone).length
  (allAssignments free).map fun as => toInput (fill e as).1

structure Count where
  programs : Nat := 0
  inputs : Nat := 0
  ok : Nat := 0
  mismatches : Nat := 0

def checkOne (c : Count) (e : Input) (withCompletions : Bool) : IO Count := do
  let mut c := { c with programs := c.programs + 1 }
  let inputs := if withCompletions then e :: completions e else [e]
  for x in inputs do
    c := { c with inputs := c.inputs + 1 }
    if (inferWithCertificate x).isOk then c := { c with ok := c.ok + 1 }
    unless same x do
      if c.mismatches < 10 then IO.println s!"MISMATCH {describe x}"
      c := { c with mismatches := c.mismatches + 1 }
  return c

end Differential

open Differential in
def main (args : List String) : IO UInt32 := do
  let mut c : Count := {}
  match args with
  | ["--enumerate", data, size] =>
    let g : Grammar := { data := data == "full" }
    let n := size.toNat!
    let table := build g n
    for s in [1:n + 1] do
      for e in ((table[s]?.getD #[])[0]?).getD [] do
        c ← checkOne c e true
  | ["--targeted"] =>
    for text in targeted do
      match parse text >>= elaborate with
      | .ok e => c ← checkOne c e true
      | .error m => IO.println s!"cannot elaborate {text}: {m}"
  | ["--corpus"] =>
    for root in ["../tests", "../examples"] do
      for f in (← System.FilePath.walkDir root) do
        if f.extension == some "det" then
          match parse (← IO.FS.readFile f) >>= elaborate with
          | .ok e => c ← checkOne c e (((sites e).filter Option.isNone).length ≤ 10)
          | .error _ => pure ()
  | _ => IO.println "usage: --enumerate core|full SIZE | --targeted | --corpus"; return 2
  IO.println s!"programs {c.programs}, inputs compared {c.inputs}, accepted {c.ok}, mismatches {c.mismatches}"
  return (if c.mismatches == 0 then 0 else 1)
