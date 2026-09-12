import Determinize.Checking.Input

namespace Determinize.Checking.Input

def mapVars (replace : Nat → Nat → Input) (depth : Nat) :
    Input → Input
  | .bvar index => replace depth index
  | .unit => .unit
  | .reject => .reject
  | .discrete affinity d => .discrete affinity d
  | .bool value => .bool value
  | .real value => .real value
  | .lam body => .lam (body.mapVars replace (depth + 1))
  | .fix body => .fix (body.mapVars replace (depth + 2))
  | .app f x => .app (f.mapVars replace depth) (x.mapVars replace depth)
  | .pair l r => .pair (l.mapVars replace depth) (r.mapVars replace depth)
  | .fst x => .fst (x.mapVars replace depth)
  | .snd x => .snd (x.mapVars replace depth)
  | .inl x => .inl (x.mapVars replace depth)
  | .inr x => .inr (x.mapVars replace depth)
  | .matchSum x l r => .matchSum (x.mapVars replace depth)
      (l.mapVars replace (depth + 1)) (r.mapVars replace (depth + 1))
  | .nil => .nil
  | .cons h t => .cons (h.mapVars replace depth) (t.mapVars replace depth)
  | .matchList x n c => .matchList (x.mapVars replace depth)
      (n.mapVars replace depth) (c.mapVars replace (depth + 2))
  | .ite c t e => .ite (c.mapVars replace depth) (t.mapVars replace depth)
      (e.mapVars replace depth)
  | .letE x b => .letE (x.mapVars replace depth)
      (b.mapVars replace (depth + 1))
  | .neg x => .neg (x.mapVars replace depth)
  | .add l r => .add (l.mapVars replace depth) (r.mapVars replace depth)
  | .mul l r => .mul (l.mapVars replace depth) (r.mapVars replace depth)
  | .div l r => .div (l.mapVars replace depth) (r.mapVars replace depth)
  | .lt l r => .lt (l.mapVars replace depth) (r.mapVars replace depth)
  | .uniform k l r => .uniform k (l.mapVars replace depth) (r.mapVars replace depth)
  | .gaussian k l r => .gaussian k (l.mapVars replace depth) (r.mapVars replace depth)
  | .poisson k x => .poisson k (x.mapVars replace depth)
  | .bernoulli k x => .bernoulli k (x.mapVars replace depth)
  | .exponential k x => .exponential k (x.mapVars replace depth)
  | .beta k l r => .beta k (l.mapVars replace depth) (r.mapVars replace depth)
  | .gamma k l r => .gamma k (l.mapVars replace depth) (r.mapVars replace depth)

abbrev shift (amount cutoff : Nat) : Input → Input :=
  mapVars (fun cutoff index => .bvar (if cutoff ≤ index then index + amount else index)) cutoff

end Determinize.Checking.Input
