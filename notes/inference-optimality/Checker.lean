import Determinize.Frontend.Compile
import Determinize.Frontend.Pretty
import Determinize.Checking.Elaboration

/-!
# Brute-force checker for the affinity-inference optimality claims

Run from `lean/` (the interpreter is fast enough; no build target is needed):

    lake env lean --run ../notes/inference-optimality/Checker.lean            # size ≤ 5, core grammar, then targeted
    lake env lean --run ../notes/inference-optimality/Checker.lean --enumerate --size 7   # enumeration only, ~5 min
    lake env lean --run ../notes/inference-optimality/Checker.lean --data     # pairs, sums, lists too
    lake env lean --run ../notes/inference-optimality/Checker.lean --targeted # only the hand-written programs
    lake env lean --run ../notes/inference-optimality/Checker.lean --verbose  # print every typable program
    lake env lean --run ../notes/inference-optimality/Checker.lean --corpus   # every .det under tests/ and examples/
    lake env lean --run ../notes/inference-optimality/Checker.lean --max-free 16 --corpus   # include the 16-site example

For every enumerated `Input` `e` the checker

1. enumerates every completion `ê` (every E/G assignment to the unannotated sites);
2. decides, independently of `infer`, whether `ê` is typable at some closed type:
   an affinity-free shape unifier (`shapes`) computes the most general shape of every
   binder, and `synth` then tries every E/G decoration of those shapes and computes the
   least type of every subterm under them;
3. validates every positive answer of `synth` with the verified checker `certify`
   (so a "typable" verdict is backed by an actual `Typed` proof term), and cross-checks
   every verdict against `infer` run on the fully annotated `ê`;
4. runs `infer e` and compares:
   * claim 1: `infer` succeeds ⟹ its output passes `certify` and is one of the
     completions `synth` accepts;
   * claim 2: `infer` succeeds ⟹ every typable completion is pointwise `≤` (G ≤ E) the
     inferred one, i.e. the inferred completion is the greatest;
   * claim 3: `infer` fails ⟹ no completion is typable;
   * lattice: the typable completions are closed under pointwise join and meet;
   * the `float E` reading of the step-1 prompt: typability at `float E` is decided with the
     result shape unified with `float` first, and the programs where a `float E` completion
     exists although `infer`'s certificate type is not a float are counted (and printed with
     `--verbose`).

Binder shapes do not depend on the completion (structural subtyping relates only types
of the same shape), and a shape variable left unconstrained takes part in no affinity
constraint, so instantiating it with `unit` loses no completion. Hence `synth` decides
typability exactly, provided the shape unifier is right; a wrong shape would surface as a
disagreement with `infer` on the fully annotated program.
-/

open Determinize Determinize.Frontend Determinize.Checking Determinize.Spec.Paper

namespace OptimalityChecker

/-! ## Affinities and types -/

/-- `G ≤ E`, the direction of `Ty.Sub.general`. -/
def Affinity.le : Affinity → Affinity → Bool
  | .G, _ => true
  | .E, .E => true
  | .E, .G => false

def Affinity.join : Affinity → Affinity → Affinity
  | .G, .G => .G
  | _, _ => .E

def Affinity.meet : Affinity → Affinity → Affinity
  | .E, .E => .E
  | _, _ => .G

/-- Decidable `Ty.Sub`, taken from the verified checker. -/
def sub (a b : Ty) : Bool := (checkSubtype a b).isSome

mutual
/-- Least upper bound of two types of the same shape (arguments contravariant). -/
partial def Ty.join : Ty → Ty → Option Ty
  | .unit, .unit => some .unit
  | .bool, .bool => some .bool
  | .float a, .float b => some (.float (Affinity.join a b))
  | .prod a b, .prod c d => do return .prod (← Ty.join a c) (← Ty.join b d)
  | .sum a b, .sum c d => do return .sum (← Ty.join a c) (← Ty.join b d)
  | .list a, .list b => do return .list (← Ty.join a b)
  | .arr a b, .arr c d => do return .arr (← Ty.meet a c) (← Ty.join b d)
  | _, _ => none
partial def Ty.meet : Ty → Ty → Option Ty
  | .unit, .unit => some .unit
  | .bool, .bool => some .bool
  | .float a, .float b => some (.float (Affinity.meet a b))
  | .prod a b, .prod c d => do return .prod (← Ty.meet a c) (← Ty.meet b d)
  | .sum a b, .sum c d => do return .sum (← Ty.meet a c) (← Ty.meet b d)
  | .list a, .list b => do return .list (← Ty.meet a b)
  | .arr a b, .arr c d => do return .arr (← Ty.join a c) (← Ty.meet b d)
  | _, _ => none
end

def isFloat : Ty → Bool
  | .float _ => true
  | _ => false

/-- All subterms of a type. -/
partial def subterms : Ty → List Ty
  | t@(.prod a b) | t@(.sum a b) | t@(.arr a b) => t :: (subterms a ++ subterms b)
  | t@(.list a) => t :: subterms a
  | t => [t]

/-- Every affinity re-decoration of a type. -/
partial def decorations : Ty → List Ty
  | .unit => [.unit]
  | .bool => [.bool]
  | .float _ => [.float .E, .float .G]
  | .prod a b => (decorations a).flatMap fun a => (decorations b).map (.prod a ·)
  | .sum a b => (decorations a).flatMap fun a => (decorations b).map (.sum a ·)
  | .list a => (decorations a).map .list
  | .arr a b => (decorations a).flatMap fun a => (decorations b).map (.arr a ·)

/-! ## Sites and completions -/

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

def leAff (a b : List Affinity) : Bool := (a.zip b).all fun (x, y) => Affinity.le x y
def joinAff (a b : List Affinity) : List Affinity := (a.zip b).map fun (x, y) => Affinity.join x y
def meetAff (a b : List Affinity) : List Affinity := (a.zip b).map fun (x, y) => Affinity.meet x y

/-- The affinities a core term carries at the input's unannotated sites. -/
def freeAffinities (e : Input) (c : Core) : List Affinity :=
  ((sites e).zip (sampleAffinities c)).filterMap fun (r, a) => if r.isNone then some a else none

/-! ## Independent shape inference

Affinity-free unification gives every binder its most general shape. The hints are the
annotation shapes of the binder nodes (`lam`, `fix`, `nil`, `reject`, `inl`, `inr`) in
preorder, node before children. -/

inductive SType where
  | var (n : Nat) | unit | bool | float
  | prod (a b : SType) | sum (a b : SType) | list (a : SType) | arr (a b : SType)
deriving Repr, BEq, Inhabited

structure SState where
  bind : Array (Option SType) := #[]

abbrev SM := StateT SState (Except String)

def sfresh : SM SType := do
  let s ← get
  set { s with bind := s.bind.push none }
  return .var s.bind.size

partial def sresolve : SType → SM SType
  | .var n => do
    match (← get).bind[n]? with
    | some (some t) => sresolve t
    | _ => return .var n
  | t => return t

partial def soccurs (n : Nat) (t : SType) : SM Bool := do
  match (← sresolve t) with
  | .var m => return n == m
  | .prod a b | .sum a b | .arr a b => return (← soccurs n a) || (← soccurs n b)
  | .list a => soccurs n a
  | _ => return false

partial def sunify (a b : SType) : SM Unit := do
  let a ← sresolve a
  let b ← sresolve b
  match a, b with
  | .var i, .var j => if i != j then modify fun s => { s with bind := s.bind.set! i (some b) }
  | .var i, t | t, .var i => do
    if (← soccurs i t) then throw "infinite shape"
    modify fun s => { s with bind := s.bind.set! i (some t) }
  | .unit, .unit | .bool, .bool | .float, .float => pure ()
  | .prod a b, .prod c d | .sum a b, .sum c d | .arr a b, .arr c d => do sunify a c; sunify b d
  | .list a, .list b => sunify a b
  | _, _ => throw "shape mismatch"

partial def shapes (Γ : List SType) : Input → SM (SType × List SType)
  | .bvar i => match Γ[i]? with
    | some t => return (t, [])
    | none => throw "unbound variable"
  | .reject => do let t ← sfresh; return (t, [t])
  | .unit => return (.unit, [])
  | .bool _ => return (.bool, [])
  | .real _ => return (.float, [])
  | .nil => do let t ← sfresh; return (.list t, [t])
  | .lam b => do
    let a ← sfresh
    let (r, hs) ← shapes (a :: Γ) b
    return (.arr a r, a :: hs)
  | .fix b => do
    let a ← sfresh; let r ← sfresh
    let (t, hs) ← shapes (a :: .arr a r :: Γ) b
    sunify t r
    return (.arr a r, .arr a r :: hs)
  | .app f x => do
    let (tf, hf) ← shapes Γ f; let (tx, hx) ← shapes Γ x
    let r ← sfresh
    sunify tf (.arr tx r)
    return (r, hf ++ hx)
  | .pair a b => do
    let (ta, ha) ← shapes Γ a; let (tb, hb) ← shapes Γ b
    return (.prod ta tb, ha ++ hb)
  | .fst p => do
    let (t, h) ← shapes Γ p
    let a ← sfresh; let b ← sfresh
    sunify t (.prod a b)
    return (a, h)
  | .snd p => do
    let (t, h) ← shapes Γ p
    let a ← sfresh; let b ← sfresh
    sunify t (.prod a b)
    return (b, h)
  | .inl v => do
    let o ← sfresh
    let (t, h) ← shapes Γ v
    return (.sum t o, o :: h)
  | .inr v => do
    let o ← sfresh
    let (t, h) ← shapes Γ v
    return (.sum o t, o :: h)
  | .matchSum s l r => do
    let (ts, hs) ← shapes Γ s
    let a ← sfresh; let b ← sfresh
    sunify ts (.sum a b)
    let (tl, hl) ← shapes (a :: Γ) l
    let (tr, hr) ← shapes (b :: Γ) r
    sunify tl tr
    return (tl, hs ++ hl ++ hr)
  | .cons h t => do
    let (th, hh) ← shapes Γ h; let (tt, ht) ← shapes Γ t
    sunify tt (.list th)
    return (.list th, hh ++ ht)
  | .matchList s n c => do
    let (ts, hs) ← shapes Γ s
    let a ← sfresh
    sunify ts (.list a)
    let (tn, hn) ← shapes Γ n
    let (tc, hc) ← shapes (a :: .list a :: Γ) c
    sunify tn tc
    return (tn, hs ++ hn ++ hc)
  | .ite c a b => do
    let (tc, hc) ← shapes Γ c; let (ta, ha) ← shapes Γ a; let (tb, hb) ← shapes Γ b
    sunify tc .bool
    sunify ta tb
    return (ta, hc ++ ha ++ hb)
  | .letE v b => do
    let (tv, hv) ← shapes Γ v
    let (tb, hb) ← shapes (tv :: Γ) b
    return (tb, hv ++ hb)
  | .neg x => do
    let (t, h) ← shapes Γ x
    sunify t .float
    return (.float, h)
  | .add a b | .mul a b | .div a b => do
    let (ta, ha) ← shapes Γ a; let (tb, hb) ← shapes Γ b
    sunify ta .float; sunify tb .float
    return (.float, ha ++ hb)
  | .lt a b => do
    let (ta, ha) ← shapes Γ a; let (tb, hb) ← shapes Γ b
    sunify ta .float; sunify tb .float
    return (.bool, ha ++ hb)
  | .uniform _ a b | .gaussian _ a b | .beta _ a b | .gamma _ a b => do
    let (ta, ha) ← shapes Γ a; let (tb, hb) ← shapes Γ b
    sunify ta .float; sunify tb .float
    return (.float, ha ++ hb)
  | .poisson _ a | .bernoulli _ a | .exponential _ a => do
    let (ta, ha) ← shapes Γ a
    sunify ta .float
    return (.float, ha)
  | .discrete _ p => do
    let (tp, hp) ← shapes Γ p
    sunify tp (.list .float)
    return (.float, hp)

/-- Fully resolve a shape; unconstrained variables become `unit`. -/
partial def sground : SType → SM Ty
  | t => do
    match (← sresolve t) with
    | .var _ => return .unit
    | .unit => return .unit
    | .bool => return .bool
    | .float => return .float .E
    | .prod a b => return .prod (← sground a) (← sground b)
    | .sum a b => return .sum (← sground a) (← sground b)
    | .list a => return .list (← sground a)
    | .arr a b => return .arr (← sground a) (← sground b)

/-- The candidate annotations of every binder node, or `none` if the input has no shape.
With `top`, the program's own shape is first unified with `top`: this decides typability at
`float E`, where an unconstrained result shape (as in `reject` or `fst reject`) must become
`float` rather than the default `unit`. -/
def binderHints (e : Input) (top : Option SType := none) : Option (Array (List Ty)) :=
  match (do
      let (t, hs) ← shapes [] e
      if let some top := top then sunify t top
      hs.mapM sground : SM (List Ty)).run {} with
  | .ok (hs, _) => some (hs.map decorations).toArray
  | .error _ => none

/-! ## Independent typability decision

`synth hints idx Γ ê` lists the least types (with certificates) of `ê` under `Γ`, one per
choice of decorations at the binder nodes; `idx` is the index of the first binder node of
`ê` among the hints. Literals type at `float G`, the least float type. Every other
construct has a least type given the least types of its parts (joins at `ite`, `match`,
`cons`, `add`; the argument of an application only has to lie below the function's
parameter). `let` binders take the least type of the bound value, which is complete
because typing is preserved when a context entry is replaced by a subtype (narrowing). -/

/-- Number of binder nodes (those that consume a hint). -/
def binders : Core → Nat
  | .lam b | .fix b => 1 + binders b
  | .nil | .reject => 1
  | .inl b | .inr b => 1 + binders b
  | .fst b | .snd b | .neg b => binders b
  | .app a b | .pair a b | .cons a b | .letE a b | .add a b | .mul a b | .div a b | .lt a b =>
      binders a + binders b
  | .matchSum a b c | .matchList a b c | .ite a b c => binders a + binders b + binders c
  | .uniform _ a b | .gaussian _ a b | .beta _ a b | .gamma _ a b => binders a + binders b
  | .poisson _ a | .discrete _ a | .bernoulli _ a | .exponential _ a => binders a
  | _ => 0

def cast (τ : Ty) (tc : Ty × Certificate) : Certificate :=
  if tc.1 == τ then tc.2 else .sub τ tc.2

def dedupTypes (l : List (Ty × Certificate)) : List (Ty × Certificate) :=
  l.foldl (fun acc tc => if acc.any (·.1 == tc.1) then acc else acc ++ [tc]) []

partial def synth (hints : Array (List Ty)) (idx : Nat) (Γ : List Ty) : Core → List (Ty × Certificate)
  | .bvar i => match Γ[i]? with
    | some t => [(t, .node t [])]
    | none => []
  | .reject => (hints[idx]?.getD []).map fun t => (t, .node t [])
  | .unit => [(.unit, .node .unit [])]
  | .bool _ => [(.bool, .node .bool [])]
  | .real _ => [(.float .G, .node (.float .G) [])]
  | .nil => (hints[idx]?.getD []).map fun t => (.list t, .node (.list t) [])
  | .lam b => dedupTypes <| (hints[idx]?.getD []).flatMap fun A =>
      (synth hints (idx + 1) (A :: Γ) b).map fun (R, cb) => (.arr A R, .node (.arr A R) [cb])
  | .fix b => dedupTypes <| (hints[idx]?.getD []).flatMap fun F =>
      match F with
      | .arr A R =>
        (synth hints (idx + 1) (A :: .arr A R :: Γ) b).filterMap fun (T, cb) =>
          if sub T R then some (.arr A R, .node (.arr A R) [cast R (T, cb)]) else none
      | _ => []
  | .app f x => dedupTypes <| (synth hints idx Γ f).flatMap fun (F, cf) =>
      (synth hints (idx + binders f) Γ x).filterMap fun (S, cx) =>
        match F with
        | .arr A R => if sub S A then some (R, .node R [cf, cast A (S, cx)]) else none
        | _ => none
  | .pair a b => dedupTypes <| (synth hints idx Γ a).flatMap fun (A, ca) =>
      (synth hints (idx + binders a) Γ b).map fun (B, cb) => (.prod A B, .node (.prod A B) [ca, cb])
  | .fst p => dedupTypes <| (synth hints idx Γ p).filterMap fun (P, cp) =>
      match P with
      | .prod A _ => some (A, .node A [cp])
      | _ => none
  | .snd p => dedupTypes <| (synth hints idx Γ p).filterMap fun (P, cp) =>
      match P with
      | .prod _ B => some (B, .node B [cp])
      | _ => none
  | .inl v => dedupTypes <| (hints[idx]?.getD []).flatMap fun B =>
      (synth hints (idx + 1) Γ v).map fun (V, cv) => (.sum V B, .node (.sum V B) [cv])
  | .inr v => dedupTypes <| (hints[idx]?.getD []).flatMap fun A =>
      (synth hints (idx + 1) Γ v).map fun (V, cv) => (.sum A V, .node (.sum A V) [cv])
  | .matchSum s l r => dedupTypes <| (synth hints idx Γ s).flatMap fun (S, cs) =>
      match S with
      | .sum A B => (synth hints (idx + binders s) (A :: Γ) l).flatMap fun (L, cl) =>
          (synth hints (idx + binders s + binders l) (B :: Γ) r).filterMap fun (R, cr) =>
            (Ty.join L R).map fun T => (T, .node T [cs, cast T (L, cl), cast T (R, cr)])
      | _ => []
  | .cons h t => dedupTypes <| (synth hints idx Γ h).flatMap fun (H, ch) =>
      (synth hints (idx + binders h) Γ t).filterMap fun (T, ct) =>
        match T with
        | .list E => (Ty.join H E).map fun E' =>
            (.list E', .node (.list E') [cast E' (H, ch), cast (.list E') (T, ct)])
        | _ => none
  | .matchList s n c => dedupTypes <| (synth hints idx Γ s).flatMap fun (S, cs) =>
      match S with
      | .list A => (synth hints (idx + binders s) Γ n).flatMap fun (N, cn) =>
          (synth hints (idx + binders s + binders n) (A :: .list A :: Γ) c).filterMap fun (C, cc) =>
            (Ty.join N C).map fun T => (T, .node T [cs, cast T (N, cn), cast T (C, cc)])
      | _ => []
  | .ite c a b => dedupTypes <| (synth hints idx Γ c).flatMap fun (C, cc) =>
      if C == .bool then
        (synth hints (idx + binders c) Γ a).flatMap fun (A, ca) =>
          (synth hints (idx + binders c + binders a) Γ b).filterMap fun (B, cb) =>
            (Ty.join A B).map fun T => (T, .node T [cc, cast T (A, ca), cast T (B, cb)])
      else []
  | .letE v b => dedupTypes <| (synth hints idx Γ v).flatMap fun (V, cv) =>
      (synth hints (idx + binders v) (V :: Γ) b).map fun (B, cb) => (B, .node B [cv, cb])
  | .neg x => dedupTypes <| (synth hints idx Γ x).filterMap fun (X, cx) =>
      match X with
      | .float a => some (.float a, .node (.float a) [cx])
      | _ => none
  | .add l r => dedupTypes <| (synth hints idx Γ l).flatMap fun (L, cl) =>
      (synth hints (idx + binders l) Γ r).filterMap fun (R, cr) =>
        match L, R with
        | .float a, .float b =>
            let m := Affinity.join a b
            some (.float m, .node (.float m) [cast (.float m) (L, cl), cast (.float m) (R, cr)])
        | _, _ => none
  | .mul l r => dedupTypes <| (synth hints idx Γ l).flatMap fun (L, cl) =>
      (synth hints (idx + binders l) Γ r).filterMap fun (R, cr) =>
        match L, R with
        | .float .G, .float b => some (.float b, .node (.float b) [cl, cr])
        | _, _ => none
  | .div l r => dedupTypes <| (synth hints idx Γ l).flatMap fun (L, cl) =>
      (synth hints (idx + binders l) Γ r).filterMap fun (R, cr) =>
        match L, R with
        | .float a, .float .G => some (.float a, .node (.float a) [cl, cr])
        | _, _ => none
  | .lt l r => dedupTypes <| (synth hints idx Γ l).flatMap fun (L, cl) =>
      (synth hints (idx + binders l) Γ r).filterMap fun (R, cr) =>
        match L, R with
        | .float .G, .float .G => some (.bool, .node .bool [cl, cr])
        | _, _ => none
  | .uniform (.sample m) l u => dedupTypes <| (synth hints idx Γ l).flatMap fun (L, cl) =>
      (synth hints (idx + binders l) Γ u).filterMap fun (U', cu) =>
        match L, U' with
        | .float a, .float b =>
            if Affinity.le a m && Affinity.le b m then
              some (.float m, .node (.float m) [cast (.float m) (L, cl), cast (.float m) (U', cu)])
            else none
        | _, _ => none
  | .gaussian (.sample m) mean var => dedupTypes <| (synth hints idx Γ mean).flatMap fun (M, cm) =>
      (synth hints (idx + binders mean) Γ var).filterMap fun (V, cv) =>
        match M, V with
        | .float a, .float .G =>
            if Affinity.le a m then some (.float m, .node (.float m) [cast (.float m) (M, cm), cv])
            else none
        | _, _ => none
  | .poisson (.sample m) r => dedupTypes <| (synth hints idx Γ r).filterMap fun (R, cr) =>
      match R with
      | .float a => if Affinity.le a m then some (.float m, .node (.float m) [cast (.float m) (R, cr)]) else none
      | _ => none
  | .bernoulli (.sample m) p => dedupTypes <| (synth hints idx Γ p).filterMap fun (P, cp) =>
      match P with
      | .float a => if Affinity.le a m then some (.float m, .node (.float m) [cast (.float m) (P, cp)]) else none
      | _ => none
  | .discrete (.sample m) p => dedupTypes <| (synth hints idx Γ p).filterMap fun (P, cp) =>
      match P with
      | .list (.float a) =>
          if Affinity.le a m then some (.float m, .node (.float m) [cast (.list (.float m)) (P, cp)]) else none
      | _ => none
  | .exponential (.sample m) r => dedupTypes <| (synth hints idx Γ r).filterMap fun (R, cr) =>
      match R with
      | .float .G => some (.float m, .node (.float m) [cr])
      | _ => none
  | .beta (.sample m) a b => dedupTypes <| (synth hints idx Γ a).flatMap fun (A, ca) =>
      (synth hints (idx + binders a) Γ b).filterMap fun (B, cb) =>
        match A, B with
        | .float .G, .float .G => some (.float m, .node (.float m) [ca, cb])
        | _, _ => none
  | .gamma (.sample m) s r => dedupTypes <| (synth hints idx Γ s).flatMap fun (S, cs) =>
      (synth hints (idx + binders s) Γ r).filterMap fun (R, cr) =>
        match S, R with
        | .float a, .float .G =>
            if Affinity.le a m then some (.float m, .node (.float m) [cast (.float m) (S, cs), cr]) else none
        | _, _ => none
  | _ => []   -- mean sites never occur in a completion

/-! ## Rendering -/

private def req : Option Affinity → String
  | none => ""
  | some a => s!"[{prettyAffinity a}]"

private def lit (q : Rat) : String := if q.den == 1 then toString q.num else s!"{q.num}/{q.den}"

partial def showInput (env : List String) (depth : Nat) : Input → String
  | .bvar i => (env[i]?).getD s!"unbound_{i}"
  | .reject => "observe(false)"
  | .unit => "()"
  | .bool b => toString b
  | .real q => lit q
  | .lam b => let x := s!"x{depth}"; s!"(fun {x} => {showInput (x :: env) (depth + 1) b})"
  | .fix b => let f := s!"f{depth}"; let x := s!"x{depth}"
      s!"(rec {f} {x} => {showInput (x :: f :: env) (depth + 1) b})"
  | .app a b => s!"({showInput env depth a} {showInput env depth b})"
  | .pair a b => s!"({showInput env depth a}, {showInput env depth b})"
  | .fst a => s!"(fst {showInput env depth a})"
  | .snd a => s!"(snd {showInput env depth a})"
  | .inl a => s!"(inl {showInput env depth a})"
  | .inr a => s!"(inr {showInput env depth a})"
  | .matchSum e a b => let x := s!"x{depth}"
      s!"(match {showInput env depth e} with inl {x} => {showInput (x :: env) (depth + 1) a} | inr {x} => {showInput (x :: env) (depth + 1) b})"
  | .nil => "[]"
  | .cons h t => s!"({showInput env depth h} :: {showInput env depth t})"
  | .matchList e n c => let x := s!"x{depth}"; let xs := s!"xs{depth}"
      s!"(match {showInput env depth e} with [] => {showInput env depth n} | {x} :: {xs} => {showInput (x :: xs :: env) (depth + 1) c})"
  | .ite c a b => s!"(if {showInput env depth c} then {showInput env depth a} else {showInput env depth b})"
  | .letE a b => let x := s!"x{depth}"
      s!"(let {x} = {showInput env depth a} in {showInput (x :: env) (depth + 1) b})"
  | .neg a => s!"(-{showInput env depth a})"
  | .add a b => s!"({showInput env depth a} + {showInput env depth b})"
  | .mul a b => s!"({showInput env depth a} * {showInput env depth b})"
  | .div a b => s!"({showInput env depth a} / {showInput env depth b})"
  | .lt a b => s!"({showInput env depth a} < {showInput env depth b})"
  | .uniform r a b => s!"uniform{req r}({showInput env depth a}, {showInput env depth b})"
  | .gaussian r a b => s!"gauss{req r}({showInput env depth a}, {showInput env depth b})"
  | .beta r a b => s!"beta{req r}({showInput env depth a}, {showInput env depth b})"
  | .gamma r a b => s!"gamma{req r}({showInput env depth a}, {showInput env depth b})"
  | .poisson r a => s!"poisson{req r}({showInput env depth a})"
  | .discrete r a => s!"discrete_list{req r}({showInput env depth a})"
  | .bernoulli r a => s!"bernoulli{req r}({showInput env depth a})"
  | .exponential r a => s!"exponential{req r}({showInput env depth a})"

def showAff (as : List Affinity) : String := String.intercalate "" (as.map prettyAffinity)

/-! ## Checking one program -/

structure Outcome where
  inferOk : Bool := false
  typable : Bool := false
  free : Nat := 0
  completions : Nat := 0
  forced : Bool := false
  higherOrder : Bool := false
  inferredAffinities : String := ""
  /-- `infer`'s certificate type is a float. -/
  floatInferred : Bool := false
  /-- Some completion is typable at `float E` (the formulation in the step-1 prompt). -/
  floatCompletion : Bool := false
  problems : List String := []

partial def higherOrder : Input → Bool
  | .lam _ | .fix _ => true
  | .fst b | .snd b | .inl b | .inr b | .neg b => higherOrder b
  | .app a b | .pair a b | .cons a b | .letE a b | .add a b | .mul a b | .div a b | .lt a b =>
      higherOrder a || higherOrder b
  | .matchSum a b c | .matchList a b c | .ite a b c => higherOrder a || higherOrder b || higherOrder c
  | .uniform _ a b | .gaussian _ a b | .beta _ a b | .gamma _ a b => higherOrder a || higherOrder b
  | .poisson _ a | .discrete _ a | .bernoulli _ a | .exponential _ a => higherOrder a
  | _ => false

def checkProgram (e : Input) : Outcome := Id.run do
  let text := showInput [] 0 e
  let free := ((sites e).filter Option.isNone).length
  let inferred := infer e
  let hints := binderHints e
  let floatHints := binderHints e (some .float)
  let mut typable : List (List Affinity) := []
  let mut floatTypable : List (List Affinity) := []
  let mut problems : List String := []
  if hints.isNone && inferred.isOk then
    problems := s!"shape inference fails but infer accepts {text}" :: problems
  for as in allAssignments free do
    let (candidate, rest) := fill e as
    unless rest.isEmpty && e.matches candidate do
      problems := s!"fill is broken for {text}" :: problems
    let typings := match hints with
      | some hints => synth hints 0 [] candidate
      | none => []
    let floatTypings := match floatHints with
      | some hints => synth hints 0 [] candidate
      | none => []
    for (τ, c) in typings ++ floatTypings do
      if (certify e candidate c).isNone then
        problems := s!"synth built an invalid certificate at {prettyType τ} for {pretty candidate}" :: problems
    let annotated := infer (toInput candidate)
    if typings.isEmpty == annotated.isOk then
      problems := s!"synth ({typings.length} typings) and infer on the annotated program ({if annotated.isOk then "ok" else "fails"}) disagree for {pretty candidate}" :: problems
    unless typings.isEmpty do typable := typable ++ [as]
    if floatTypings.any (fun (τ, _) => sub τ (.float .E)) then floatTypable := floatTypable ++ [as]
  for a in typable do
    for b in typable do
      unless typable.contains (joinAff a b) do
        problems := s!"typable completions of {text} are not join-closed: {showAff a} ∨ {showAff b}" :: problems
      unless typable.contains (meetAff a b) do
        problems := s!"typable completions of {text} are not meet-closed: {showAff a} ∧ {showAff b}" :: problems
  -- A completion typable at `float E` is typable, and the greatest typable completion is
  -- typable at `float E` whenever any completion is (the top-level affinity is unconstrained).
  for a in floatTypable do
    unless typable.contains a do
      problems := s!"float-E typable completion {showAff a} of {text} is not typable" :: problems
  unless floatTypable.isEmpty || typable.isEmpty do
    for a in typable do
      unless floatTypable.contains a do
        problems := s!"completion {showAff a} of {text} is typable but not at float E although {showAff floatTypable.head!} is" :: problems
  let mut outcome : Outcome := { free, typable := !typable.isEmpty, completions := typable.length,
                                 higherOrder := higherOrder e, floatCompletion := !floatTypable.isEmpty }
  match inferred with
  | .ok (candidate, c) =>
    outcome := { outcome with inferOk := true, inferredAffinities := showAff (sampleAffinities candidate),
                              floatInferred := isFloat c.ty }
    let aff := freeAffinities e candidate
    if (certify e candidate c).isNone then
      problems := s!"claim 1: infer's output does not certify for {text}" :: problems
    if isFloat c.ty && !floatTypable.contains aff then
      problems := s!"claim 1 (float E): infer types {text} at {prettyType c.ty} but synth finds no float-E typing of its output" :: problems
    unless typable.contains aff do
      problems := s!"claim 1: synth finds no typing of infer's output {pretty candidate}" :: problems
    for as in typable do
      unless leAff as aff do
        problems := s!"claim 2: completion {showAff as} of {text} is not below the inferred {showAff aff}" :: problems
    outcome := { outcome with forced := aff.contains .G }
  | .error msg =>
    unless typable.isEmpty do
      problems := s!"claim 3: infer rejects {text} ({msg}) but {typable.length} completions are typable, e.g. {showAff typable.head!}" :: problems
  return { outcome with problems := problems.reverse }

/-! ## Enumeration

Terms are counted by constructor nodes. The core grammar has leaves `x` (every variable in
scope), `1` and `true`; unary `-e`, `poisson(e)`, `exponential(e)`, `fun x => e`, `rec f x => e`;
binary `e e`, `e + e`, `e * e`, `e / e`, `e < e`, `let x = e in e`, `uniform(e, e)`,
`uniform[E](e, e)`, `uniform[G](e, e)`, `gauss(e, e)`; and `if e then e else e`. The full
grammar (`--data`) adds the leaves `[]` and `observe(false)` (the core `reject`), the unary
`fst`, `snd`, `inl`, `inr`, `discrete_list`, `bernoulli`, `poisson[G]`, `exponential[E]`, the
binary pairs, `::`, `beta`, `gamma`, `gauss[E]`, and both `match` forms. -/

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

/-! ## Main -/

structure Stats where
  programs : Nat := 0
  inferOk : Nat := 0
  typable : Nat := 0
  withSites : Nat := 0
  multi : Nat := 0
  forced : Nat := 0
  higherOrder : Nat := 0
  higherOrderTypable : Nat := 0
  /-- `infer` succeeds with a float type. -/
  floatInferred : Nat := 0
  /-- some completion is typable at `float E`. -/
  floatCompletion : Nat := 0
  /-- `infer` succeeds with a non-float type although a completion is typable at `float E`. -/
  floatMismatch : Nat := 0
  problems : Nat := 0

def record (st : Stats) (o : Outcome) : Stats :=
  { programs := st.programs + 1
    inferOk := st.inferOk + (if o.inferOk then 1 else 0)
    typable := st.typable + (if o.typable then 1 else 0)
    withSites := st.withSites + (if o.free > 0 then 1 else 0)
    multi := st.multi + (if o.completions ≥ 2 then 1 else 0)
    forced := st.forced + (if o.forced then 1 else 0)
    higherOrder := st.higherOrder + (if o.higherOrder then 1 else 0)
    higherOrderTypable := st.higherOrderTypable + (if o.higherOrder && o.typable then 1 else 0)
    floatInferred := st.floatInferred + (if o.floatInferred then 1 else 0)
    floatCompletion := st.floatCompletion + (if o.floatCompletion then 1 else 0)
    floatMismatch := st.floatMismatch + (if o.inferOk && !o.floatInferred && o.floatCompletion then 1 else 0)
    problems := st.problems + o.problems.length }

def report (st : Stats) : String :=
  s!"programs {st.programs}, infer ok {st.inferOk}, typable {st.typable}, with free sites {st.withSites}, " ++
  s!"≥2 typable completions {st.multi}, some site forced G {st.forced}, higher-order {st.higherOrder} " ++
  s!"(typable {st.higherOrderTypable}), float-typed by infer {st.floatInferred}, " ++
  s!"float-E completion exists {st.floatCompletion} (of which infer non-float {st.floatMismatch}), problems {st.problems}"

def runEnumeration (g : Grammar) (maxSize : Nat) (verbose : Bool) : IO Bool := do
  let table := build g maxSize
  let mut st : Stats := {}
  let mut shown := 0
  for s in [1:maxSize + 1] do
    let programs := ((table[s]?.getD #[])[0]?).getD []
    let mut sizeStats : Stats := {}
    for e in programs do
      let o := checkProgram e
      sizeStats := record sizeStats o
      st := record st o
      for p in o.problems do
        if shown < 40 then IO.println s!"  PROBLEM: {p}"
        shown := shown + 1
      if verbose && o.free > 0 && o.typable then
        IO.println s!"  {showInput [] 0 e} => {o.inferredAffinities} ({o.completions} typable completions)"
      if verbose && o.inferOk && !o.floatInferred && o.floatCompletion then
        IO.println s!"  float-E completion exists but infer's type is not a float: {showInput [] 0 e}"
    IO.println s!"size {s}: {report sizeStats}"
  IO.println s!"total: {report st}"
  return st.problems == 0

def runTargeted : IO Bool := do
  let mut ok := true
  for text in targeted do
    match parse text >>= elaborate with
    | .error msg => IO.println s!"  cannot elaborate {text}: {msg}"; ok := false
    | .ok e =>
      let o := checkProgram e
      let verdict := if o.inferOk then s!"infer: {o.inferredAffinities}" else "infer: rejects"
      IO.println s!"  {text}\n      {verdict}; typable completions: {o.completions} of {2 ^ o.free}"
      for p in o.problems do
        IO.println s!"      PROBLEM: {p}"
        ok := false
  return ok

/-- Check the repository's `.det` programs (paths relative to `lean/`). -/
def runCorpus (roots : List String) (maxFree : Nat) : IO Bool := do
  let mut ok := true
  let mut count := 0
  let mut skipped := 0
  for root in roots do
    let files ← System.FilePath.walkDir root
    for f in files.qsort (fun a b => a.toString < b.toString) do
      if f.extension == some "det" then
        let text ← IO.FS.readFile f
        match parse text >>= elaborate with
        | .error msg => IO.println s!"  {f}: cannot elaborate ({msg})"
        | .ok e =>
          let free := ((sites e).filter Option.isNone).length
          if free > maxFree then
            IO.println s!"  {f}: skipped, {free} free sites"
            skipped := skipped + 1
          else
            let o := checkProgram e
            count := count + 1
            let verdict := if o.inferOk then s!"infer: {o.inferredAffinities}" else "infer: rejects"
            IO.println s!"  {f}: {verdict}; typable completions: {o.completions} of {2 ^ o.free}"
            for p in o.problems do
              IO.println s!"      PROBLEM: {p}"
              ok := false
  IO.println s!"corpus: {count} programs checked, {skipped} skipped"
  return ok

end OptimalityChecker

open OptimalityChecker in
def main (args : List String) : IO UInt32 := do
  let mut size := 5
  let mut grammar : Grammar := {}
  let mut verbose := false
  let mut enumerate := true
  let mut targetedRun := true
  let mut maxFree := 12
  let mut rest := args
  while !rest.isEmpty do
    match rest with
    | "--size" :: n :: tail => size := n.toNat!; rest := tail
    | "--max-free" :: n :: tail => maxFree := n.toNat!; rest := tail
    | "--data" :: tail => grammar := { data := true }; rest := tail
    | "--verbose" :: tail => verbose := true; rest := tail
    | "--targeted" :: tail => enumerate := false; rest := tail
    | "--corpus" :: _ =>
      let ok ← runCorpus ["../tests", "../examples"] maxFree
      IO.println (if ok then "ALL CLAIMS HOLD on the corpus" else "VIOLATIONS FOUND")
      return (if ok then 0 else 1)
    | "--enumerate" :: tail => targetedRun := false; rest := tail
    | arg :: _ => IO.println s!"unknown argument {arg}"; return 2
    | [] => pure ()
  let mut ok := true
  if enumerate then
    IO.println s!"== enumeration (size ≤ {size}, {if grammar.data then "full" else "core"} grammar) =="
    ok := (← runEnumeration grammar size verbose) && ok
  if targetedRun then
    IO.println "== targeted programs =="
    ok := (← runTargeted) && ok
  IO.println (if ok then "ALL CLAIMS HOLD on the checked programs" else "VIOLATIONS FOUND")
  return (if ok then 0 else 1)
