import Determinize.Proof.Frontend.Ground
import Determinize.Proof.Frontend.Typing

/-!
# Completeness of generation (Lemma C)

A typed completion determines a solution of the subtyping constraints of the draft: if `ê` fills
the omitted affinities of `e` and has type `T` in a context `Γ'` above the instance of `Γ`, then
`generate` succeeds, and the substitution extends to the fresh variables of the draft such that
every constraint holds, the draft's type is below `T`, and the read-back program is `ê` itself.

The extension is chosen from the typing derivation, one inversion lemma per constructor. The
conclusion is robust: it holds for every substitution that agrees with the extension below the
final counter, so that the variables of later subterms can still be chosen freely.
-/

namespace Determinize.Proof.Frontend
open Determinize.Frontend Spec.Paper Ground

/-- What Lemma C establishes about the draft `d` of a completion `ê` at the type `T`. -/
def Fits (d : Draft) (ê : Core) (T : Ty) (σ : Ground) : Prop :=
  σ.Solves d.relations ∧ Ty.Sub (σ.inst d.ty) T ∧ d.program σ.affinities = ê

/-! ## Fresh variables

Each lemma gives the fresh variables that `generate` draws at the counter `n` the values chosen
from the typing derivation, and states these values under every substitution that agrees with
the extension below the next counter. The counters are written as `generate` produces them. -/

theorem fresh₁ (σ : Ground) (n : Nat) (A : Ty) :
    ∃ σ₀, σ.Agree n σ₀ ∧ ∀ σ', σ₀.Agree (n + 1) σ' → σ'.types n = A :=
  ⟨σ.setType n A, (Agree.refl _ _).setType le_rfl A,
    fun _ h => by rw [h.type (by omega)]; simp⟩

theorem fresh₂ (σ : Ground) (n : Nat) (A B : Ty) :
    ∃ σ₀, σ.Agree n σ₀ ∧
      ∀ σ', σ₀.Agree (n + 1 + 1) σ' → σ'.types n = A ∧ σ'.types (n + 1) = B :=
  ⟨(σ.setType n A).setType (n + 1) B, ((Agree.refl _ _).setType le_rfl A).setType (by omega) B,
    fun _ h => ⟨by rw [h.type (by omega)]; simp, by rw [h.type (by omega)]; simp⟩⟩

theorem fresh₃ (σ : Ground) (n : Nat) (A B C : Ty) :
    ∃ σ₀, σ.Agree n σ₀ ∧ ∀ σ', σ₀.Agree (n + 1 + 1 + 1) σ' →
      σ'.types n = A ∧ σ'.types (n + 1) = B ∧ σ'.types (n + 1 + 1) = C :=
  ⟨((σ.setType n A).setType (n + 1) B).setType (n + 1 + 1) C,
    (((Agree.refl _ _).setType le_rfl A).setType (by omega) B).setType (by omega) C,
    fun _ h => ⟨by rw [h.type (by omega)]; simp [show n ≠ n + 1 + 1 by omega],
      by rw [h.type (by omega)]; simp, by rw [h.type (by omega)]; simp⟩⟩

theorem freshAffinity (σ : Ground) (n : Nat) (m : Affinity) :
    ∃ σ₀, σ.Agree n σ₀ ∧ ∀ σ', σ₀.Agree (n + 1) σ' → σ'.affinities (.generated n) = m :=
  ⟨σ.setAffinity n m, (Agree.refl _ _).setAffinity le_rfl m,
    fun _ h => by rw [h.affinity (by omega)]; simp⟩

/-- The type of a sample site that the completion samples at `m` becomes `float m`. -/
theorem site_complete {r : Option Affinity} {m : Affinity} (hr : (r.isNone || r == some m) = true)
    (n : Nat) (σ : Ground) :
    ∃ t n₁, (site r).run n = .ok (t, n₁) ∧ n ≤ n₁ ∧ ∃ σ₁, σ.Agree n σ₁ ∧
      Robust n₁ σ₁ fun σ' => σ'.inst t = .float m ∧ t.affinity σ'.affinities = m := by
  cases r with
  | none =>
    refine ⟨_, _, rfl, Nat.le_succ n, σ.setAffinity n m, (Agree.refl _ _).setAffinity le_rfl m,
      fun σ' h => ?_⟩
    have : σ'.affinities (.generated n) = m := by rw [h.affinity (Nat.lt_succ_self n)]; simp
    simp [UType.affinity, AffinityTerm.eval, this]
  | some m' =>
    simp only [Option.isNone_some, Bool.false_or, beq_iff_eq, Option.some.injEq] at hr
    subst hr
    exact ⟨_, _, rfl, le_rfl, σ, Agree.refl _ _, fun _ _ => by
      simp [UType.affinity, AffinityTerm.eval]⟩

/-- The type of a draft node. -/
macro "draft_type" : tactic => `(tactic|
  simp only [Draft.ty, inst_var, inst_unit, inst_bool, inst_float, inst_prod, inst_sum,
    inst_list, inst_arr, inst_general, AffinityTerm.eval])

/-- Split a completion that matches the input node. -/
macro "invert_matches" ê:ident hm:ident : tactic => `(tactic|
  (cases $ê:ident <;> (try cases ‹DistributionAction›) <;>
    simp only [Input.matches, Bool.and_eq_true, Bool.false_eq_true, decide_eq_true_eq]
      at $hm:ident))

/-- Lemma C. -/
theorem generate_complete : ∀ (e : Input) (Γ : List UType) (Γ' : List Ty) (n : Nat) (σ : Ground)
    (ê : Core) (T : Ty), Robust n σ (Context Γ Γ') → e.matches ê = true →
    Typed Γ' (interpret ê) T →
    ∃ d n', (generate Γ e).run n = .ok (d, n') ∧ n ≤ n' ∧
      ∃ σ₁, σ.Agree n σ₁ ∧ Robust n' σ₁ (Fits d ê T) := by
  intro e
  induction e with
  | bvar i =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    subst hm
    obtain ⟨T₀, hv, s⟩ := typed_bvar_inv ht
    obtain ⟨t, hi, -⟩ := context_getElem? (hΓ σ (Agree.refl _ _)) (hasVar_iff.1 hv)
    refine ⟨_, _, by rw [generate]; simp only [hi, StateT.run_pure]; rfl, le_rfl, σ,
      Agree.refl _ _, fun σ' h' => ⟨by relations, ?_, by read_back⟩⟩
    obtain ⟨t', hi', s'⟩ := context_getElem? (hΓ σ' h') (hasVar_iff.1 hv)
    rw [hi, Option.some.injEq] at hi'
    subst hi'
    exact s'.trans s
  | reject =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₁ σ n T
    refine ⟨_, _, by unfold_generate; rfl, Nat.le_succ n, σ₀, a₀,
      fun σ' h' => ⟨by relations, ?_, by read_back⟩⟩
    draft_type
    rw [e₀ σ' h']
    exact .refl _
  | unit =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    exact ⟨_, _, by unfold_generate; rfl, le_rfl, σ, Agree.refl _ _,
      fun σ' h' => ⟨by relations, by draft_type; exact typed_unit_inv ht, by read_back⟩⟩
  | bool b =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    subst hm
    exact ⟨_, _, by unfold_generate; rfl, le_rfl, σ, Agree.refl _ _,
      fun σ' h' => ⟨by relations, by draft_type; exact typed_bool_inv ht, by read_back⟩⟩
  | real q =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    subst hm
    obtain ⟨m, sT⟩ := typed_real_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := freshAffinity σ n m
    refine ⟨_, _, by unfold_generate; rfl, Nat.le_succ n, σ₀, a₀,
      fun σ' h' => ⟨by relations, ?_, by read_back⟩⟩
    draft_type
    rw [e₀ σ' h']
    exact sT
  | nil =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, sT⟩ := typed_nil_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₁ σ n A
    refine ⟨_, _, by unfold_generate; rfl, Nat.le_succ n, σ₀, a₀,
      fun σ' h' => ⟨by relations, ?_, by read_back⟩⟩
    draft_type
    rw [e₀ σ' h']
    exact sT
  | lam b ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, R, tb, sT⟩ := typed_lam_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₁ σ n A
    obtain ⟨db, n₁, hb, le₁, σ₁, a₁, F₁⟩ := ih (.var n :: Γ) (A :: Γ') (n + 1) σ₀ _ _
      ((hΓ.extend a₀ (by omega)).cons fun σ' h => by simp only [inst_var, e₀ σ' h]; exact .refl _)
      hm tb
    refine ⟨_, _, by unfold_generate; simp only [hb, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ (by omega), fun σ' h' => ?_⟩
    obtain ⟨Sb, sb, pb⟩ := F₁ σ' h'
    have eA := e₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; exact Sb
    · draft_type; rw [eA]; exact (Ty.Sub.arr (.refl _) sb).trans sT
    · read_back; rw [pb]
  | fix b ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, R, tb, sT⟩ := typed_fix_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₂ σ n A R
    obtain ⟨db, n₁, hb, le₁, σ₁, a₁, F₁⟩ :=
      ih (.var n :: .arr (.var n) (.var (n + 1)) :: Γ) (A :: .arr A R :: Γ') (n + 1 + 1) σ₀ _ _
      (((hΓ.extend a₀ (by omega)).cons fun σ' h => by
          simp only [inst_arr, inst_var, (e₀ σ' h).1, (e₀ σ' h).2]; exact .refl _).cons
        fun σ' h => by simp only [inst_var, (e₀ σ' h).1]; exact .refl _)
      hm tb
    refine ⟨_, _, by unfold_generate; simp only [hb, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ (by omega), fun σ' h' => ?_⟩
    obtain ⟨Sb, sb, pb⟩ := F₁ σ' h'
    obtain ⟨eA, eR⟩ := e₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eR]; exact ⟨sb, Sb⟩
    · draft_type; rw [eA, eR]; exact sT
    · read_back; rw [pb]
  | app f x ihf ihx =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, R, tf, tx, sR⟩ := typed_app_inv ht
    obtain ⟨df, n₁, hf, le₁, σ₁, a₁, F₁⟩ := ihf Γ Γ' n σ _ _ hΓ hm.1 tf
    obtain ⟨dx, n₂, hx, le₂, σ₂, a₂, F₂⟩ := ihx Γ Γ' n₁ σ₁ _ _ (hΓ.extend a₁ le₁) hm.2 tx
    obtain ⟨σ₃, a₃, e₃⟩ := fresh₂ σ₂ n₂ A R
    refine ⟨_, _, by unfold_generate; simp only [hf, hx, ok_bind]; rfl, by omega,
      σ₃, a₁.step (a₂.step a₃ le₂) le₁, fun σ' h' => ?_⟩
    have h₂ := a₃.step h' (by omega)
    obtain ⟨Sf, sf, pf⟩ := F₁ σ' (a₂.step h₂ le₂)
    obtain ⟨Sx, sx, px⟩ := F₂ σ' h₂
    obtain ⟨eA, eR⟩ := e₃ σ' h'
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eA, eR]; exact ⟨⟨sf, Sf⟩, sx, Sx⟩
    · draft_type; rw [eR]; exact sR
    · read_back; rw [pf, px]
  | pair a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, B, ta, tb, sT⟩ := typed_pair_inv ht
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := iha Γ Γ' n σ _ _ hΓ hm.1 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ := ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend a₁ le₁) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [ha, hb, ok_bind]; rfl, by omega,
      σ₂, a₁.step a₂ le₁, fun σ' h' => ?_⟩
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' (a₂.step h' le₂)
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    refine ⟨?_, ?_, ?_⟩
    · relations; exact ⟨Sa, Sb⟩
    · draft_type; exact (Ty.Sub.prod sa sb).trans sT
    · read_back; rw [pa, pb]
  | fst p ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, B, tp, sT⟩ := typed_fst_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₂ σ n A B
    obtain ⟨dp, n₁, hp, le₁, σ₁, a₁, F₁⟩ :=
      ih Γ Γ' (n + 1 + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm tp
    refine ⟨_, _, by unfold_generate; simp only [hp, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ (by omega), fun σ' h' => ?_⟩
    obtain ⟨Sp, sp, pp⟩ := F₁ σ' h'
    obtain ⟨eA, eB⟩ := e₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eA, eB]; exact ⟨sp, Sp⟩
    · draft_type; rw [eA]; exact sT
    · read_back; rw [pp]
  | snd p ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, B, tp, sT⟩ := typed_snd_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₂ σ n A B
    obtain ⟨dp, n₁, hp, le₁, σ₁, a₁, F₁⟩ :=
      ih Γ Γ' (n + 1 + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm tp
    refine ⟨_, _, by unfold_generate; simp only [hp, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ (by omega), fun σ' h' => ?_⟩
    obtain ⟨Sp, sp, pp⟩ := F₁ σ' h'
    obtain ⟨eA, eB⟩ := e₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eA, eB]; exact ⟨sp, Sp⟩
    · draft_type; rw [eB]; exact sT
    · read_back; rw [pp]
  | inl v ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, B, tv, sT⟩ := typed_inl_inv ht
    obtain ⟨dv, n₁, hv, le₁, σ₁, a₁, F₁⟩ := ih Γ Γ' n σ _ _ hΓ hm tv
    obtain ⟨σ₂, a₂, e₂⟩ := fresh₁ σ₁ n₁ B
    refine ⟨_, _, by unfold_generate; simp only [hv, ok_bind]; rfl, by omega, σ₂,
      a₁.step a₂ le₁, fun σ' h' => ?_⟩
    obtain ⟨Sv, sv, pv⟩ := F₁ σ' (a₂.step h' (by omega))
    have eB := e₂ σ' h'
    refine ⟨?_, ?_, ?_⟩
    · relations; exact Sv
    · draft_type; rw [eB]; exact (Ty.Sub.sum sv (.refl _)).trans sT
    · read_back; rw [pv]
  | inr v ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, B, tv, sT⟩ := typed_inr_inv ht
    obtain ⟨dv, n₁, hv, le₁, σ₁, a₁, F₁⟩ := ih Γ Γ' n σ _ _ hΓ hm tv
    obtain ⟨σ₂, a₂, e₂⟩ := fresh₁ σ₁ n₁ A
    refine ⟨_, _, by unfold_generate; simp only [hv, ok_bind]; rfl, by omega, σ₂,
      a₁.step a₂ le₁, fun σ' h' => ?_⟩
    obtain ⟨Sv, sv, pv⟩ := F₁ σ' (a₂.step h' (by omega))
    have eA := e₂ σ' h'
    refine ⟨?_, ?_, ?_⟩
    · relations; exact Sv
    · draft_type; rw [eA]; exact (Ty.Sub.sum (.refl _) sv).trans sT
    · read_back; rw [pv]
  | matchSum s a b ihs iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, B, R, ts, ta, tb, sT⟩ := typed_matchSum_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₃ σ n A B R
    obtain ⟨ds, n₁, hs, le₁, σ₁, a₁, F₁⟩ :=
      ihs Γ Γ' (n + 1 + 1 + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm.1.1 ts
    obtain ⟨da, n₂, ha, le₂, σ₂, a₂, F₂⟩ := iha (.var n :: Γ) (A :: Γ') n₁ σ₁ _ _
      ((hΓ.extend (a₀.step a₁ (by omega)) (by omega)).cons fun σ' h => by
        simp only [inst_var, (e₀ σ' (a₁.step h le₁)).1]; exact .refl _) hm.1.2 ta
    obtain ⟨db, n₃, hb, le₃, σ₃, a₃, F₃⟩ := ihb (.var (n + 1) :: Γ) (B :: Γ') n₂ σ₂ _ _
      ((hΓ.extend (a₀.step (a₁.step a₂ le₁) (by omega)) (by omega)).cons fun σ' h => by
        simp only [inst_var, (e₀ σ' (a₁.step (a₂.step h le₂) le₁)).2.1]; exact .refl _) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [hs, ha, hb, ok_bind]; rfl, by omega, σ₃,
      a₀.step (a₁.step (a₂.step a₃ le₂) le₁) (by omega), fun σ' h' => ?_⟩
    have h₂ := a₃.step h' le₃
    have h₁ := a₂.step h₂ le₂
    obtain ⟨Ss, ss, ps⟩ := F₁ σ' h₁
    obtain ⟨Sa, sa, pa⟩ := F₂ σ' h₂
    obtain ⟨Sb, sb, pb⟩ := F₃ σ' h'
    obtain ⟨eA, eB, eR⟩ := e₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eA, eB, eR]; exact ⟨⟨ss, Ss⟩, ⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [eR]; exact sT
    · read_back; rw [ps, pa, pb]
  | cons hd tl ihh iht =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, th, tt, sT⟩ := typed_cons_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₁ σ n A
    obtain ⟨dh, n₁, hh, le₁, σ₁, a₁, F₁⟩ :=
      ihh Γ Γ' (n + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm.1 th
    obtain ⟨dt, n₂, htl, le₂, σ₂, a₂, F₂⟩ :=
      iht Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ (by omega)) (by omega)) hm.2 tt
    refine ⟨_, _, by unfold_generate; simp only [hh, htl, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) (by omega), fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sh, sh, ph⟩ := F₁ σ' h₁
    obtain ⟨St, st, pt⟩ := F₂ σ' h'
    have eA := e₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eA]; exact ⟨⟨sh, Sh⟩, st, St⟩
    · draft_type; rw [eA]; exact sT
    · read_back; rw [ph, pt]
  | matchList s nc cc ihs ihn ihc =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨A, R, ts, tn, tc, sT⟩ := typed_matchList_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₂ σ n A R
    obtain ⟨ds, n₁, hs, le₁, σ₁, a₁, F₁⟩ :=
      ihs Γ Γ' (n + 1 + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm.1.1 ts
    obtain ⟨dn, n₂, hn, le₂, σ₂, a₂, F₂⟩ :=
      ihn Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ (by omega)) (by omega)) hm.1.2 tn
    obtain ⟨dc, n₃, hc, le₃, σ₃, a₃, F₃⟩ :=
      ihc (.var n :: .list (.var n) :: Γ) (A :: .list A :: Γ') n₂ σ₂ _ _
      (((hΓ.extend (a₀.step (a₁.step a₂ le₁) (by omega)) (by omega)).cons fun σ' h => by
          simp only [inst_list, inst_var, (e₀ σ' (a₁.step (a₂.step h le₂) le₁)).1]
          exact .refl _).cons
        fun σ' h => by simp only [inst_var, (e₀ σ' (a₁.step (a₂.step h le₂) le₁)).1]; exact .refl _)
      hm.2 tc
    refine ⟨_, _, by unfold_generate; simp only [hs, hn, hc, ok_bind]; rfl, by omega, σ₃,
      a₀.step (a₁.step (a₂.step a₃ le₂) le₁) (by omega), fun σ' h' => ?_⟩
    have h₂ := a₃.step h' le₃
    have h₁ := a₂.step h₂ le₂
    obtain ⟨Ss, ss, ps⟩ := F₁ σ' h₁
    obtain ⟨Sn, sn, pn⟩ := F₂ σ' h₂
    obtain ⟨Sc, sc, pc⟩ := F₃ σ' h'
    obtain ⟨eA, eR⟩ := e₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eA, eR]; exact ⟨⟨ss, Ss⟩, ⟨sn, Sn⟩, sc, Sc⟩
    · draft_type; rw [eR]; exact sT
    · read_back; rw [ps, pn, pc]
  | ite c a b ihc iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨R, tc, ta, tb, sT⟩ := typed_ite_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := fresh₁ σ n R
    obtain ⟨dc, n₁, hc, le₁, σ₁, a₁, F₁⟩ :=
      ihc Γ Γ' (n + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm.1.1 tc
    obtain ⟨da, n₂, ha, le₂, σ₂, a₂, F₂⟩ :=
      iha Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ (by omega)) (by omega)) hm.1.2 ta
    obtain ⟨db, n₃, hb, le₃, σ₃, a₃, F₃⟩ :=
      ihb Γ Γ' n₂ σ₂ _ _ (hΓ.extend (a₀.step (a₁.step a₂ le₁) (by omega)) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [hc, ha, hb, ok_bind]; rfl, by omega, σ₃,
      a₀.step (a₁.step (a₂.step a₃ le₂) le₁) (by omega), fun σ' h' => ?_⟩
    have h₂ := a₃.step h' le₃
    have h₁ := a₂.step h₂ le₂
    obtain ⟨Sc, sc, pc⟩ := F₁ σ' h₁
    obtain ⟨Sa, sa, pa⟩ := F₂ σ' h₂
    obtain ⟨Sb, sb, pb⟩ := F₃ σ' h'
    have eR := e₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [eR]; exact ⟨⟨sc, Sc⟩, ⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [eR]; exact sT
    · read_back; rw [pc, pa, pb]
  | letE v b ihv ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨V, R, tv, tb, sT⟩ := typed_letE_inv ht
    obtain ⟨dv, n₁, hv, le₁, σ₁, a₁, F₁⟩ := ihv Γ Γ' n σ _ _ hΓ hm.1 tv
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ := ihb (dv.ty :: Γ) (V :: Γ') n₁ σ₁ _ _
      ((hΓ.extend a₁ le₁).cons fun σ' h => (F₁ σ' h).2.1) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [hv, hb, ok_bind]; rfl, by omega,
      σ₂, a₁.step a₂ le₁, fun σ' h' => ?_⟩
    obtain ⟨Sv, -, pv⟩ := F₁ σ' (a₂.step h' le₂)
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    refine ⟨?_, ?_, ?_⟩
    · relations; exact ⟨Sv, Sb⟩
    · draft_type; exact sb.trans sT
    · read_back; rw [pv, pb]
  | neg b ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨m, tb, sT⟩ := typed_neg_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := freshAffinity σ n m
    obtain ⟨db, n₁, hb, le₁, σ₁, a₁, F₁⟩ := ih Γ Γ' (n + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm tb
    refine ⟨_, _, by unfold_generate; simp only [hb, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ (by omega), fun σ' h' => ?_⟩
    obtain ⟨Sb, sb, pb⟩ := F₁ σ' h'
    have em := e₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [em]; exact ⟨sb, Sb⟩
    · draft_type; rw [em]; exact sT
    · read_back; rw [pb]
  | add a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨m, ta, tb, sT⟩ := typed_add_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := freshAffinity σ n m
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ :=
      iha Γ Γ' (n + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm.1 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ (by omega)) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [ha, hb, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) (by omega), fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h₁
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    have em := e₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [em]; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [em]; exact sT
    · read_back; rw [pa, pb]
  | mul a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨m, ta, tb, sT⟩ := typed_mul_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := freshAffinity σ n m
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ :=
      iha Γ Γ' (n + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm.1 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ (by omega)) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [ha, hb, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) (by omega), fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h₁
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    have em := e₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [em]; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [em]; exact sT
    · read_back; rw [pa, pb]
  | div a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨m, ta, tb, sT⟩ := typed_div_inv ht
    obtain ⟨σ₀, a₀, e₀⟩ := freshAffinity σ n m
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ :=
      iha Γ Γ' (n + 1) σ₀ _ _ (hΓ.extend a₀ (by omega)) hm.1 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ (by omega)) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [ha, hb, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) (by omega), fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h₁
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    have em := e₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [em]; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [em]; exact sT
    · read_back; rw [pa, pb]
  | lt a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, tb, sT⟩ := typed_lt_inv ht
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ :=
      iha Γ Γ' (n + 1) σ _ _ (hΓ.extend (Agree.refl _ _) (by omega)) hm.1 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend ((Agree.refl n σ).step a₁ (by omega)) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [ha, hb, ok_bind]; rfl, by omega, σ₂,
      (Agree.refl n σ).step (a₁.step a₂ le₁) (by omega), fun σ' h' => ?_⟩
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' (a₂.step h' le₂)
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    refine ⟨?_, ?_, ?_⟩
    · relations; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; exact sT
    · read_back; rw [pa, pb]
  | uniform r a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, tb, sT⟩ := typed_uniform_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1.1 n σ
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := iha Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.1.2 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ le₀) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [hsite, ha, hb, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) le₀, fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h₁
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [et]; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pa, pb]
  | gaussian r a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, tb, sT⟩ := typed_gaussian_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1.1 n σ
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := iha Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.1.2 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ le₀) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [hsite, ha, hb, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) le₀, fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h₁
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [et]; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pa, pb]
  | beta r a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, tb, sT⟩ := typed_beta_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1.1 n σ
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := iha Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.1.2 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ le₀) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [hsite, ha, hb, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) le₀, fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h₁
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pa, pb]
  | gamma r a b iha ihb =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, tb, sT⟩ := typed_gamma_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1.1 n σ
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := iha Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.1.2 ta
    obtain ⟨db, n₂, hb, le₂, σ₂, a₂, F₂⟩ :=
      ihb Γ Γ' n₁ σ₁ _ _ (hΓ.extend (a₀.step a₁ le₀) (by omega)) hm.2 tb
    refine ⟨_, _, by unfold_generate; simp only [hsite, ha, hb, ok_bind]; rfl, by omega, σ₂,
      a₀.step (a₁.step a₂ le₁) le₀, fun σ' h' => ?_⟩
    have h₁ := a₂.step h' le₂
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h₁
    obtain ⟨Sb, sb, pb⟩ := F₂ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h₁ le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [et]; exact ⟨⟨sa, Sa⟩, sb, Sb⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pa, pb]
  | discrete r p ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨tp, sT⟩ := typed_discrete_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1 n σ
    obtain ⟨dp, n₁, hp, le₁, σ₁, a₁, F₁⟩ := ih Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.2 tp
    refine ⟨_, _, by unfold_generate; simp only [hsite, hp, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ le₀, fun σ' h' => ?_⟩
    obtain ⟨Sp, sp, pp⟩ := F₁ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [et]; exact ⟨sp, Sp⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pp]
  | poisson r a ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, sT⟩ := typed_poisson_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1 n σ
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := ih Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.2 ta
    refine ⟨_, _, by unfold_generate; simp only [hsite, ha, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ le₀, fun σ' h' => ?_⟩
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [et]; exact ⟨sa, Sa⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pa]
  | bernoulli r a ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, sT⟩ := typed_bernoulli_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1 n σ
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := ih Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.2 ta
    refine ⟨_, _, by unfold_generate; simp only [hsite, ha, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ le₀, fun σ' h' => ?_⟩
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; rw [et]; exact ⟨sa, Sa⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pa]
  | exponential r a ih =>
    intro Γ Γ' n σ ê T hΓ hm ht
    invert_matches ê hm
    obtain ⟨ta, sT⟩ := typed_exponential_inv ht
    obtain ⟨t, n₀, hsite, le₀, σ₀, a₀, F₀⟩ := site_complete hm.1 n σ
    obtain ⟨da, n₁, ha, le₁, σ₁, a₁, F₁⟩ := ih Γ Γ' n₀ σ₀ _ _ (hΓ.extend a₀ le₀) hm.2 ta
    refine ⟨_, _, by unfold_generate; simp only [hsite, ha, ok_bind]; rfl, by omega, σ₁,
      a₀.step a₁ le₀, fun σ' h' => ?_⟩
    obtain ⟨Sa, sa, pa⟩ := F₁ σ' h'
    obtain ⟨et, em⟩ := F₀ σ' (a₁.step h' le₁)
    refine ⟨?_, ?_, ?_⟩
    · relations; exact ⟨sa, Sa⟩
    · draft_type; rw [et]; exact sT
    · read_back; rw [em, pa]

end Determinize.Proof.Frontend
