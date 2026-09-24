import Determinize.Spec.Syntax
import Determinize.Proof.Semantics.Subtyping

/-!
# Inversion of the typing relation

A derivation of `Typed Γ e T` ends with the rule for the head constructor of `e`, followed by
any number of subsumption steps. Each lemma here inverts one constructor: the premises of its
rule hold for some type below `T`. Mean sites are omitted: completions contain none.
-/

namespace Determinize.Proof.Frontend
open Spec.Paper

variable {Γ : List Ty} {T : Ty}

theorem typed_bvar_inv {i : Nat} (h : Typed Γ (.bvar i) T) :
    ∃ T₀, HasVar Γ i T₀ ∧ Ty.Sub T₀ T := by
  generalize he : Expr.bvar i = e at h
  induction h with
  | bvar hv => cases he; exact ⟨_, hv, .refl _⟩
  | sub _ s ih => obtain ⟨T₀, hv, s'⟩ := ih he; exact ⟨T₀, hv, s'.trans s⟩
  | _ => cases he

theorem typed_unit_inv (h : Typed Γ .unit T) : Ty.Sub .unit T := by
  generalize he : (Expr.unit : Expr) = e at h
  induction h with
  | unit => exact .unit
  | sub _ s ih => exact (ih he).trans s
  | _ => cases he

theorem typed_bool_inv {b : Bool} (h : Typed Γ (.bool b) T) : Ty.Sub .bool T := by
  generalize he : Expr.bool b = e at h
  induction h with
  | bool => exact .bool
  | sub _ s ih => exact (ih he).trans s
  | _ => cases he

theorem typed_real_inv {q : ℝ} (h : Typed Γ (.real q) T) : ∃ m, Ty.Sub (.float m) T := by
  generalize he : Expr.real q = e at h
  induction h with
  | real => exact ⟨_, .refl _⟩
  | sub _ s ih => obtain ⟨m, s'⟩ := ih he; exact ⟨m, s'.trans s⟩
  | _ => cases he

theorem typed_lam_inv {b : Expr} (h : Typed Γ (.lam b) T) :
    ∃ A R, Typed (A :: Γ) b R ∧ Ty.Sub (.arr A R) T := by
  generalize he : Expr.lam b = e at h
  induction h with
  | lam hb => cases he; exact ⟨_, _, hb, .refl _⟩
  | sub _ s ih => obtain ⟨A, R, hb, s'⟩ := ih he; exact ⟨A, R, hb, s'.trans s⟩
  | _ => cases he

theorem typed_fix_inv {b : Expr} (h : Typed Γ (.fix b) T) :
    ∃ A R, Typed (A :: .arr A R :: Γ) b R ∧ Ty.Sub (.arr A R) T := by
  generalize he : Expr.fix b = e at h
  induction h with
  | fix hb => cases he; exact ⟨_, _, hb, .refl _⟩
  | sub _ s ih => obtain ⟨A, R, hb, s'⟩ := ih he; exact ⟨A, R, hb, s'.trans s⟩
  | _ => cases he

theorem typed_app_inv {f x : Expr} (h : Typed Γ (.app f x) T) :
    ∃ A R, Typed Γ f (.arr A R) ∧ Typed Γ x A ∧ Ty.Sub R T := by
  generalize he : Expr.app f x = e at h
  induction h with
  | app hf hx => cases he; exact ⟨_, _, hf, hx, .refl _⟩
  | sub _ s ih => obtain ⟨A, R, hf, hx, s'⟩ := ih he; exact ⟨A, R, hf, hx, s'.trans s⟩
  | _ => cases he

theorem typed_pair_inv {l r : Expr} (h : Typed Γ (.pair l r) T) :
    ∃ A B, Typed Γ l A ∧ Typed Γ r B ∧ Ty.Sub (.prod A B) T := by
  generalize he : Expr.pair l r = e at h
  induction h with
  | pair hl hr => cases he; exact ⟨_, _, hl, hr, .refl _⟩
  | sub _ s ih => obtain ⟨A, B, hl, hr, s'⟩ := ih he; exact ⟨A, B, hl, hr, s'.trans s⟩
  | _ => cases he

theorem typed_fst_inv {p : Expr} (h : Typed Γ (.fst p) T) :
    ∃ A B, Typed Γ p (.prod A B) ∧ Ty.Sub A T := by
  generalize he : Expr.fst p = e at h
  induction h with
  | fst hp => cases he; exact ⟨_, _, hp, .refl _⟩
  | sub _ s ih => obtain ⟨A, B, hp, s'⟩ := ih he; exact ⟨A, B, hp, s'.trans s⟩
  | _ => cases he

theorem typed_snd_inv {p : Expr} (h : Typed Γ (.snd p) T) :
    ∃ A B, Typed Γ p (.prod A B) ∧ Ty.Sub B T := by
  generalize he : Expr.snd p = e at h
  induction h with
  | snd hp => cases he; exact ⟨_, _, hp, .refl _⟩
  | sub _ s ih => obtain ⟨A, B, hp, s'⟩ := ih he; exact ⟨A, B, hp, s'.trans s⟩
  | _ => cases he

theorem typed_inl_inv {v : Expr} (h : Typed Γ (.inl v) T) :
    ∃ A B, Typed Γ v A ∧ Ty.Sub (.sum A B) T := by
  generalize he : Expr.inl v = e at h
  induction h with
  | inl hv => cases he; exact ⟨_, _, hv, .refl _⟩
  | sub _ s ih => obtain ⟨A, B, hv, s'⟩ := ih he; exact ⟨A, B, hv, s'.trans s⟩
  | _ => cases he

theorem typed_inr_inv {v : Expr} (h : Typed Γ (.inr v) T) :
    ∃ A B, Typed Γ v B ∧ Ty.Sub (.sum A B) T := by
  generalize he : Expr.inr v = e at h
  induction h with
  | inr hv => cases he; exact ⟨_, _, hv, .refl _⟩
  | sub _ s ih => obtain ⟨A, B, hv, s'⟩ := ih he; exact ⟨A, B, hv, s'.trans s⟩
  | _ => cases he

theorem typed_matchSum_inv {s l r : Expr} (h : Typed Γ (.matchSum s l r) T) :
    ∃ A B R, Typed Γ s (.sum A B) ∧ Typed (A :: Γ) l R ∧ Typed (B :: Γ) r R ∧ Ty.Sub R T := by
  generalize he : Expr.matchSum s l r = e at h
  induction h with
  | matchSum hs hl hr => cases he; exact ⟨_, _, _, hs, hl, hr, .refl _⟩
  | sub _ sub ih =>
    obtain ⟨A, B, R, hs, hl, hr, s'⟩ := ih he; exact ⟨A, B, R, hs, hl, hr, s'.trans sub⟩
  | _ => cases he

theorem typed_nil_inv (h : Typed Γ .nil T) : ∃ A, Ty.Sub (.list A) T := by
  generalize he : (Expr.nil : Expr) = e at h
  induction h with
  | nil => exact ⟨_, .refl _⟩
  | sub _ s ih => obtain ⟨A, s'⟩ := ih he; exact ⟨A, s'.trans s⟩
  | _ => cases he

theorem typed_cons_inv {hd tl : Expr} (h : Typed Γ (.cons hd tl) T) :
    ∃ A, Typed Γ hd A ∧ Typed Γ tl (.list A) ∧ Ty.Sub (.list A) T := by
  generalize he : Expr.cons hd tl = e at h
  induction h with
  | cons hh ht => cases he; exact ⟨_, hh, ht, .refl _⟩
  | sub _ s ih => obtain ⟨A, hh, ht, s'⟩ := ih he; exact ⟨A, hh, ht, s'.trans s⟩
  | _ => cases he

theorem typed_matchList_inv {s n c : Expr} (h : Typed Γ (.matchList s n c) T) :
    ∃ A R, Typed Γ s (.list A) ∧ Typed Γ n R ∧ Typed (A :: .list A :: Γ) c R ∧ Ty.Sub R T := by
  generalize he : Expr.matchList s n c = e at h
  induction h with
  | matchList hs hn hc => cases he; exact ⟨_, _, hs, hn, hc, .refl _⟩
  | sub _ sub ih =>
    obtain ⟨A, R, hs, hn, hc, s'⟩ := ih he; exact ⟨A, R, hs, hn, hc, s'.trans sub⟩
  | _ => cases he

theorem typed_ite_inv {c a b : Expr} (h : Typed Γ (.ite c a b) T) :
    ∃ R, Typed Γ c .bool ∧ Typed Γ a R ∧ Typed Γ b R ∧ Ty.Sub R T := by
  generalize he : Expr.ite c a b = e at h
  induction h with
  | ite hc ha hb => cases he; exact ⟨_, hc, ha, hb, .refl _⟩
  | sub _ s ih => obtain ⟨R, hc, ha, hb, s'⟩ := ih he; exact ⟨R, hc, ha, hb, s'.trans s⟩
  | _ => cases he

theorem typed_letE_inv {v b : Expr} (h : Typed Γ (.letE v b) T) :
    ∃ V R, Typed Γ v V ∧ Typed (V :: Γ) b R ∧ Ty.Sub R T := by
  generalize he : Expr.letE v b = e at h
  induction h with
  | letE hv hb => cases he; exact ⟨_, _, hv, hb, .refl _⟩
  | sub _ s ih => obtain ⟨V, R, hv, hb, s'⟩ := ih he; exact ⟨V, R, hv, hb, s'.trans s⟩
  | _ => cases he

theorem typed_neg_inv {b : Expr} (h : Typed Γ (.neg b) T) :
    ∃ m, Typed Γ b (.float m) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.neg b = e at h
  induction h with
  | neg hb => cases he; exact ⟨_, hb, .refl _⟩
  | sub _ s ih => obtain ⟨m, hb, s'⟩ := ih he; exact ⟨m, hb, s'.trans s⟩
  | _ => cases he

theorem typed_add_inv {l r : Expr} (h : Typed Γ (.add l r) T) :
    ∃ m, Typed Γ l (.float m) ∧ Typed Γ r (.float m) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.add l r = e at h
  induction h with
  | add hl hr => cases he; exact ⟨_, hl, hr, .refl _⟩
  | sub _ s ih => obtain ⟨m, hl, hr, s'⟩ := ih he; exact ⟨m, hl, hr, s'.trans s⟩
  | _ => cases he

theorem typed_mul_inv {l r : Expr} (h : Typed Γ (.mul l r) T) :
    ∃ m, Typed Γ l (.float .G) ∧ Typed Γ r (.float m) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.mul l r = e at h
  induction h with
  | mul hl hr => cases he; exact ⟨_, hl, hr, .refl _⟩
  | sub _ s ih => obtain ⟨m, hl, hr, s'⟩ := ih he; exact ⟨m, hl, hr, s'.trans s⟩
  | _ => cases he

theorem typed_div_inv {l r : Expr} (h : Typed Γ (.div l r) T) :
    ∃ m, Typed Γ l (.float m) ∧ Typed Γ r (.float .G) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.div l r = e at h
  induction h with
  | div hl hr => cases he; exact ⟨_, hl, hr, .refl _⟩
  | sub _ s ih => obtain ⟨m, hl, hr, s'⟩ := ih he; exact ⟨m, hl, hr, s'.trans s⟩
  | _ => cases he

theorem typed_lt_inv {l r : Expr} (h : Typed Γ (.lt l r) T) :
    Typed Γ l (.float .G) ∧ Typed Γ r (.float .G) ∧ Ty.Sub .bool T := by
  generalize he : Expr.lt l r = e at h
  induction h with
  | lt hl hr => cases he; exact ⟨hl, hr, .bool⟩
  | sub _ s ih => obtain ⟨hl, hr, s'⟩ := ih he; exact ⟨hl, hr, s'.trans s⟩
  | _ => cases he

variable {m : Affinity}

theorem typed_uniform_inv {l u : Expr} (h : Typed Γ (.uniform (.sample m) l u) T) :
    Typed Γ l (.float m) ∧ Typed Γ u (.float m) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.uniform (.sample m) l u = e at h
  induction h with
  | uniform hl hu => cases he; exact ⟨hl, hu, .refl _⟩
  | sub _ s ih => obtain ⟨hl, hu, s'⟩ := ih he; exact ⟨hl, hu, s'.trans s⟩
  | _ => cases he

theorem typed_gaussian_inv {a b : Expr} (h : Typed Γ (.gaussian (.sample m) a b) T) :
    Typed Γ a (.float m) ∧ Typed Γ b (.float .G) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.gaussian (.sample m) a b = e at h
  induction h with
  | gaussian ha hb => cases he; exact ⟨ha, hb, .refl _⟩
  | sub _ s ih => obtain ⟨ha, hb, s'⟩ := ih he; exact ⟨ha, hb, s'.trans s⟩
  | _ => cases he

theorem typed_poisson_inv {a : Expr} (h : Typed Γ (.poisson (.sample m) a) T) :
    Typed Γ a (.float m) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.poisson (.sample m) a = e at h
  induction h with
  | poisson ha => cases he; exact ⟨ha, .refl _⟩
  | sub _ s ih => obtain ⟨ha, s'⟩ := ih he; exact ⟨ha, s'.trans s⟩
  | _ => cases he

theorem typed_discrete_inv {a : Expr} (h : Typed Γ (.discrete (.sample m) a) T) :
    Typed Γ a (.list (.float m)) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.discrete (.sample m) a = e at h
  induction h with
  | discrete ha => cases he; exact ⟨ha, .refl _⟩
  | sub _ s ih => obtain ⟨ha, s'⟩ := ih he; exact ⟨ha, s'.trans s⟩
  | _ => cases he

theorem typed_bernoulli_inv {a : Expr} (h : Typed Γ (.bernoulli (.sample m) a) T) :
    Typed Γ a (.float m) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.bernoulli (.sample m) a = e at h
  induction h with
  | bernoulli ha => cases he; exact ⟨ha, .refl _⟩
  | sub _ s ih => obtain ⟨ha, s'⟩ := ih he; exact ⟨ha, s'.trans s⟩
  | _ => cases he

theorem typed_exponential_inv {a : Expr} (h : Typed Γ (.exponential (.sample m) a) T) :
    Typed Γ a (.float .G) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.exponential (.sample m) a = e at h
  induction h with
  | exponential ha => cases he; exact ⟨ha, .refl _⟩
  | sub _ s ih => obtain ⟨ha, s'⟩ := ih he; exact ⟨ha, s'.trans s⟩
  | _ => cases he

theorem typed_beta_inv {a b : Expr} (h : Typed Γ (.beta (.sample m) a b) T) :
    Typed Γ a (.float .G) ∧ Typed Γ b (.float .G) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.beta (.sample m) a b = e at h
  induction h with
  | beta ha hb => cases he; exact ⟨ha, hb, .refl _⟩
  | sub _ s ih => obtain ⟨ha, hb, s'⟩ := ih he; exact ⟨ha, hb, s'.trans s⟩
  | _ => cases he

theorem typed_gamma_inv {a b : Expr} (h : Typed Γ (.gamma (.sample m) a b) T) :
    Typed Γ a (.float m) ∧ Typed Γ b (.float .G) ∧ Ty.Sub (.float m) T := by
  generalize he : Expr.gamma (.sample m) a b = e at h
  induction h with
  | gamma ha hb => cases he; exact ⟨ha, hb, .refl _⟩
  | sub _ s ih => obtain ⟨ha, hb, s'⟩ := ih he; exact ⟨ha, hb, s'.trans s⟩
  | _ => cases he

/-- A variable has the type at its index in the context. -/
theorem hasVar_iff {i : Nat} {A : Ty} : HasVar Γ i A ↔ Γ[i]? = some A := by
  induction Γ generalizing i with
  | nil => simp only [List.getElem?_nil, reduceCtorEq, iff_false]; intro h; cases h
  | cons B Γ ih =>
    constructor
    · intro h
      cases h with
      | head => rfl
      | tail h => exact ih.1 h
    · intro h
      cases i with
      | zero => simp at h; subst h; exact .head
      | succ i => exact .tail (ih.2 (by simpa using h))

end Determinize.Proof.Frontend
