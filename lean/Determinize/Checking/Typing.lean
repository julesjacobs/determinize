import Determinize.Checking.Certificate

namespace Determinize.Checking
open Statement.Paper

private def checkVariable (Γ : List Ty) (i : Nat) (τ : Ty) : Option (PLift (HasVar Γ i τ)) :=
  match Γ, i with
  | [], _ => none
  | t :: _, 0 => if h : t = τ then some ⟨h ▸ HasVar.head⟩ else none
  | _ :: ts, i + 1 => do
      let h ← checkVariable ts i τ
      return ⟨HasVar.tail h.down⟩

/-- Decidable structural subtyping, returning evidence for the declarative rule. -/
def checkSubtype (a b : Ty) : Option (PLift (Ty.Sub a b)) :=
  match a, b with
  | .unit, .unit => some ⟨.unit⟩
  | .bool, .bool => some ⟨.bool⟩
  | .float .G, .float .E => some ⟨.general⟩
  | .float m, .float n => if h : m = n then some ⟨h ▸ .float m⟩ else none
  | .prod a b, .prod c d => do
      let h ← checkSubtype a c; let k ← checkSubtype b d
      return ⟨.prod h.down k.down⟩
  | .sum a b, .sum c d => do
      let h ← checkSubtype a c; let k ← checkSubtype b d
      return ⟨.sum h.down k.down⟩
  | .list a, .list b => do
      let h ← checkSubtype a b
      return ⟨.list h.down⟩
  | .arr a b, .arr c d => do
      let h ← checkSubtype c a; let k ← checkSubtype b d
      return ⟨.arr h.down k.down⟩
  | _, _ => none
termination_by sizeOf a + sizeOf b

/-- A successful check carries a proof of the existing typing judgment. -/
def check (Γ : List Ty) (e : Core) (τ : Ty) (c : Certificate) :
    Option (PLift (Typed Γ (interpret e) τ)) :=
  if c.ty != τ then none else
  match e, τ, c with
  | e, τ, .sub _ child => do
      let h ← check Γ e child.ty child
      let sub ← checkSubtype child.ty τ
      return ⟨h.down.sub sub.down⟩
  | .bvar i, τ, .node _ [] => do
      let h ← checkVariable Γ i τ
      return ⟨Typed.bvar h.down⟩
  | .reject, _, .node _ [] => some ⟨Typed.reject⟩
  | .unit, .unit, .node _ [] => some ⟨Typed.unit⟩
  | .bool b, .bool, .node _ [] => some ⟨Typed.bool⟩
  | .real q, .float m, .node _ [] => some ⟨Typed.real⟩
  | .lam b, .arr a r, .node _ [cb] => do
      let hb ← check (a :: Γ) b r cb
      return ⟨Typed.lam hb.down⟩
  | .fix b, .arr a r, .node _ [cb] => do
      let hb ← check (a :: .arr a r :: Γ) b r cb
      return ⟨Typed.fix hb.down⟩
  | .app f x, r, .node _ [cf, cx] => do
      let hf ← check Γ f (.arr cx.ty r) cf
      let hx ← check Γ x cx.ty cx
      return ⟨Typed.app hf.down hx.down⟩
  | .pair a b, .prod ta tb, .node _ [ca, cb] => do
      let ha ← check Γ a ta ca
      let hb ← check Γ b tb cb
      return ⟨Typed.pair ha.down hb.down⟩
  | .fst p, τ, .node _ [cp] => match cp.ty with
      | .prod a b => do
          let hp ← check Γ p (.prod a b) cp
          if h : a = τ then return ⟨h ▸ Typed.fst hp.down⟩ else none
      | _ => none
  | .snd p, τ, .node _ [cp] => match cp.ty with
      | .prod a b => do
          let hp ← check Γ p (.prod a b) cp
          if h : b = τ then return ⟨h ▸ Typed.snd hp.down⟩ else none
      | _ => none
  | .inl e, .sum a b, .node _ [c]  => do
      let h ← check Γ e a c
      return ⟨Typed.inl h.down⟩
  | .inr e, .sum a b, .node _ [c]  => do
      let h ← check Γ e b c
      return ⟨Typed.inr h.down⟩
  | .matchSum e l r, τ, .node _ [ce, cl, cr] => match ce.ty with
      | .sum a b => do
          let he ← check Γ e (.sum a b) ce
          let hl ← check (a :: Γ) l τ cl
          let hr ← check (b :: Γ) r τ cr
          return ⟨Typed.matchSum he.down hl.down hr.down⟩
      | _ => none
  | .nil, .list _, .node _ [] => some ⟨Typed.nil⟩
  | .cons a b, .list t, .node _ [ca, cb] => do
      let ha ← check Γ a t ca
      let hb ← check Γ b (.list t) cb
      return ⟨Typed.cons ha.down hb.down⟩
  | .matchList e n c, τ, .node _ [ce, cn, cc] => match ce.ty with
      | .list t => do
          let he ← check Γ e (.list t) ce
          let hn ← check Γ n τ cn
          let hc ← check (t :: .list t :: Γ) c τ cc
          return ⟨Typed.matchList he.down hn.down hc.down⟩
      | _ => none
  | .ite e a b, τ, .node _ [ce, ca, cb] => do
      let he ← check Γ e .bool ce
      let ha ← check Γ a τ ca
      let hb ← check Γ b τ cb
      return ⟨Typed.ite he.down ha.down hb.down⟩
  | .letE e b, τ, .node _ [ce, cb] => do
      let he ← check Γ e ce.ty ce
      let hb ← check (ce.ty :: Γ) b τ cb
      return ⟨Typed.letE he.down hb.down⟩
  | .neg e, .float m, .node _ [ce] => do
      let he ← check Γ e (.float m) ce
      return ⟨Typed.neg he.down⟩
  | .add a b, .float m, .node _ [ca, cb] => do
      let ha ← check Γ a (.float m) ca
      let hb ← check Γ b (.float m) cb
      return ⟨Typed.add ha.down hb.down⟩
  | .mul a b, .float m, .node _ [ca, cb] => do
      let ha ← check Γ a (.float .G) ca
      let hb ← check Γ b (.float m) cb
      return ⟨Typed.mul ha.down hb.down⟩
  | .div a b, .float m, .node _ [ca, cb] => do
      let ha ← check Γ a (.float m) ca
      let hb ← check Γ b (.float .G) cb
      return ⟨Typed.div ha.down hb.down⟩
  | .lt a b, .bool, .node _ [ca, cb] => do
      let ha ← check Γ a (.float .G) ca
      let hb ← check Γ b (.float .G) cb
      return ⟨Typed.lt ha.down hb.down⟩
  | .uniform site k a b, .float m, .node _ [ca, cb] => do
      if h : site = m then
        let ha ← check Γ a (.float m) ca
        let hb ← check Γ b (.float m) cb
        return ⟨by subst site; exact Typed.uniform ha.down hb.down⟩
      else none
  | .gaussian site k a b, .float m, .node _ [ca, cb] => do
      if h : site = m then
        let ha ← check Γ a (.float m) ca
        let hb ← check Γ b (.float .G) cb
        return ⟨by subst site; exact Typed.gaussian ha.down hb.down⟩
      else none
  | .poisson site k a, .float m, .node _ [ca] => do
      if h : site = m then
        let ha ← check Γ a (.float m) ca
        return ⟨by subst site; exact Typed.poisson ha.down⟩
      else none
  | .discrete site k d, .float m, .node _ [] => do
      if h : site = m then
        return ⟨by subst site; exact Typed.discrete⟩
      else none
  | .bernoulli site k a, .float m, .node _ [ca] => do
      if h : site = m then
        let ha ← check Γ a (.float m) ca
        return ⟨by subst site; exact Typed.bernoulli ha.down⟩
      else none
  | .exponential site k a, .float m, .node _ [ca] => do
      if h : site = m then
        let ha ← check Γ a (.float .G) ca
        return ⟨by subst site; exact Typed.exponential ha.down⟩
      else none
  | .beta site k a b, .float m, .node _ [ca, cb] => do
      if h : site = m then
        let ha ← check Γ a (.float .G) ca
        let hb ← check Γ b (.float .G) cb
        return ⟨by subst site; exact Typed.beta ha.down hb.down⟩
      else none
  | .gamma site k a b, .float m, .node _ [ca, cb] => do
      if h : site = m then
        let ha ← check Γ a (.float m) ca
        let hb ← check Γ b (.float .G) cb
        return ⟨by subst site; exact Typed.gamma ha.down hb.down⟩
      else none
  | _, _, _ => none

termination_by sizeOf c

end Determinize.Checking
