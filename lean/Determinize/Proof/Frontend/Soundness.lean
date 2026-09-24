import Determinize.Proof.Frontend.Ground
import Determinize.Proof.Frontend.Typing

/-!
# Soundness of generation (Lemma S)

Every ground substitution that solves the subtyping constraints of a draft types its read-back
program at the instance of the draft's type, and the read-back program fills exactly the omitted
affinities of the input. Each cast becomes one subsumption step.
-/

namespace Determinize.Proof.Frontend
open Determinize.Frontend Spec.Paper Checking Ground

/-- Lemma S. -/
theorem generate_sound (σ : Ground) : ∀ (e : Input) (Γ : List UType) (n : Nat) (d : Draft)
    (n' : Nat), (generate Γ e).run n = .ok (d, n') → σ.Solves d.relations →
      e.matches (d.program σ.affinities) = true ∧
        Typed (Γ.map σ.inst) (interpret (d.program σ.affinities)) (σ.inst d.ty) := by
  intro e
  induction e with
  | bvar i =>
    intro Γ n d n' h _
    rw [generate] at h
    cases hi : Γ[i]? with
    | none => simp [hi, throw, throwThe, MonadExceptOf.throw, StateT.run, StateT.lift, bind,
        Except.bind] at h
    | some t =>
      simp only [hi, StateT.run_pure, pure_eq_ok, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      read_back
      exact ⟨by simp [Input.matches], .bvar (hasVar_iff.2 (by simp [hi]))⟩
  | reject =>
    intro Γ n d n' h _
    unfold_generate h
    obtain ⟨rfl, rfl⟩ := h
    read_back
    exact ⟨rfl, .reject⟩
  | unit =>
    intro Γ n d n' h _
    unfold_generate h
    obtain ⟨rfl, rfl⟩ := h
    read_back
    exact ⟨rfl, .unit⟩
  | bool b =>
    intro Γ n d n' h _
    unfold_generate h
    obtain ⟨rfl, rfl⟩ := h
    read_back
    exact ⟨by simp [Input.matches], .bool⟩
  | real q =>
    intro Γ n d n' h _
    unfold_generate h
    obtain ⟨rfl, rfl⟩ := h
    read_back
    exact ⟨by simp [Input.matches], .real⟩
  | nil =>
    intro Γ n d n' h _
    unfold_generate h
    obtain ⟨rfl, rfl⟩ := h
    read_back
    exact ⟨rfl, .nil⟩
  | lam b ih =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨db, n₁⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨mb, tb⟩ := ih _ _ _ _ hb hσ
    read_back
    exact ⟨by simpa [Input.matches] using mb, .lam tb⟩
  | fix b ih =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨db, n₁⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨sb, hσb⟩ := hσ
    obtain ⟨mb, tb⟩ := ih _ _ _ _ hb hσb
    read_back
    exact ⟨by simpa [Input.matches] using mb, .fix (tb.sub sb)⟩
  | app f x ihf ihx =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨df, n₁⟩, hf, ⟨dx, n₂⟩, hx, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨sf, hσf⟩, sx, hσx⟩ := hσ
    obtain ⟨mf, tf⟩ := ihf _ _ _ _ hf hσf
    obtain ⟨mx, tx⟩ := ihx _ _ _ _ hx hσx
    read_back
    exact ⟨by simp [Input.matches, mf, mx], .app (tf.sub sf) (tx.sub sx)⟩
  | pair a b iha ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨hσa, hσb⟩ := hσ
    obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, ma, mb], .pair ta tb⟩
  | fst p ih =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨dp, n₁⟩, hp, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨sp, hσp⟩ := hσ
    obtain ⟨mp, tp⟩ := ih _ _ _ _ hp hσp
    read_back
    exact ⟨by simpa [Input.matches] using mp, .fst (tp.sub sp)⟩
  | snd p ih =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨dp, n₁⟩, hp, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨sp, hσp⟩ := hσ
    obtain ⟨mp, tp⟩ := ih _ _ _ _ hp hσp
    read_back
    exact ⟨by simpa [Input.matches] using mp, .snd (tp.sub sp)⟩
  | inl v ih =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨dv, n₁⟩, hv, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨mv, tv⟩ := ih _ _ _ _ hv hσ
    read_back
    exact ⟨by simpa [Input.matches] using mv, .inl tv⟩
  | inr v ih =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨dv, n₁⟩, hv, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨mv, tv⟩ := ih _ _ _ _ hv hσ
    read_back
    exact ⟨by simpa [Input.matches] using mv, .inr tv⟩
  | matchSum s a b ihs iha ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨ds, n₁⟩, hs, ⟨da, n₂⟩, ha, ⟨db, n₃⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨ss, hσs⟩, ⟨sa, hσa⟩, sb, hσb⟩ := hσ
    obtain ⟨ms, ts⟩ := ihs _ _ _ _ hs hσs
    obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, ms, ma, mb], .matchSum (ts.sub ss) (ta.sub sa) (tb.sub sb)⟩
  | cons hd tl ihh iht =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨dh, n₁⟩, hh, ⟨dt, n₂⟩, ht, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨sh, hσh⟩, st, hσt⟩ := hσ
    obtain ⟨mh, th⟩ := ihh _ _ _ _ hh hσh
    obtain ⟨mt, tt⟩ := iht _ _ _ _ ht hσt
    read_back
    exact ⟨by simp [Input.matches, mh, mt], .cons (th.sub sh) (tt.sub st)⟩
  | matchList s nc cc ihs ihn ihc =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨ds, n₁⟩, hs, ⟨dn, n₂⟩, hn, ⟨dc, n₃⟩, hc, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨ss, hσs⟩, ⟨sn, hσn⟩, sc, hσc⟩ := hσ
    obtain ⟨ms, ts⟩ := ihs _ _ _ _ hs hσs
    obtain ⟨mn, tn⟩ := ihn _ _ _ _ hn hσn
    obtain ⟨mc, tc⟩ := ihc _ _ _ _ hc hσc
    read_back
    exact ⟨by simp [Input.matches, ms, mn, mc], .matchList (ts.sub ss) (tn.sub sn) (tc.sub sc)⟩
  | ite c a b ihc iha ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨dc, n₁⟩, hc, ⟨da, n₂⟩, ha, ⟨db, n₃⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨sc, hσc⟩, ⟨sa, hσa⟩, sb, hσb⟩ := hσ
    obtain ⟨mc, tc⟩ := ihc _ _ _ _ hc hσc
    obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, mc, ma, mb], .ite (tc.sub sc) (ta.sub sa) (tb.sub sb)⟩
  | letE v b ihv ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨dv, n₁⟩, hv, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨hσv, hσb⟩ := hσ
    obtain ⟨mv, tv⟩ := ihv _ _ _ _ hv hσv
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, mv, mb], .letE tv tb⟩
  | neg b ih =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨db, n₁⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨sb, hσb⟩ := hσ
    obtain ⟨mb, tb⟩ := ih _ _ _ _ hb hσb
    read_back
    exact ⟨by simpa [Input.matches] using mb, .neg (tb.sub sb)⟩
  | add a b iha ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
    obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, ma, mb], .add (ta.sub sa) (tb.sub sb)⟩
  | mul a b iha ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
    obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, ma, mb], .mul (ta.sub sa) (tb.sub sb)⟩
  | div a b iha ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
    obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, ma, mb], .div (ta.sub sa) (tb.sub sb)⟩
  | lt a b iha ihb =>
    intro Γ n d n' h hσ
    unfold_generate h
    obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
    relations at hσ
    obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
    obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
    obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
    read_back
    exact ⟨by simp [Input.matches, ma, mb], .lt (ta.sub sa) (tb.sub sb)⟩
  | uniform r a b iha ihb =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
      obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
      obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
      read_back
      exact ⟨by simp [Input.matches, ma, mb, UType.affinity, AffinityTerm.eval],
        .uniform (ta.sub sa) (tb.sub sb)⟩
  | gaussian r a b iha ihb =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
      obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
      obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
      read_back
      exact ⟨by simp [Input.matches, ma, mb, UType.affinity, AffinityTerm.eval],
        .gaussian (ta.sub sa) (tb.sub sb)⟩
  | beta r a b iha ihb =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
      obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
      obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
      read_back
      exact ⟨by simp [Input.matches, ma, mb, UType.affinity, AffinityTerm.eval],
        .beta (ta.sub sa) (tb.sub sb)⟩
  | gamma r a b iha ihb =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨da, n₁⟩, ha, ⟨db, n₂⟩, hb, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨⟨sa, hσa⟩, sb, hσb⟩ := hσ
      obtain ⟨ma, ta⟩ := iha _ _ _ _ ha hσa
      obtain ⟨mb, tb⟩ := ihb _ _ _ _ hb hσb
      read_back
      exact ⟨by simp [Input.matches, ma, mb, UType.affinity, AffinityTerm.eval],
        .gamma (ta.sub sa) (tb.sub sb)⟩
  | discrete r p ih =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨dp, n₁⟩, hp, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨sp, hσp⟩ := hσ
      obtain ⟨mp, tp⟩ := ih _ _ _ _ hp hσp
      read_back
      exact ⟨by simp [Input.matches, mp, UType.affinity, AffinityTerm.eval],
        .discrete (tp.sub sp)⟩
  | poisson r a ih =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨da, n₁⟩, ha, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨sa, hσa⟩ := hσ
      obtain ⟨ma, ta⟩ := ih _ _ _ _ ha hσa
      read_back
      exact ⟨by simp [Input.matches, ma, UType.affinity, AffinityTerm.eval],
        .poisson (ta.sub sa)⟩
  | bernoulli r a ih =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨da, n₁⟩, ha, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨sa, hσa⟩ := hσ
      obtain ⟨ma, ta⟩ := ih _ _ _ _ ha hσa
      read_back
      exact ⟨by simp [Input.matches, ma, UType.affinity, AffinityTerm.eval],
        .bernoulli (ta.sub sa)⟩
  | exponential r a ih =>
    intro Γ n d n' h hσ
    cases r <;>
    · unfold_generate h
      obtain ⟨⟨da, n₁⟩, ha, rfl, rfl⟩ := h
      relations at hσ
      obtain ⟨sa, hσa⟩ := hσ
      obtain ⟨ma, ta⟩ := ih _ _ _ _ ha hσa
      read_back
      exact ⟨by simp [Input.matches, ma, UType.affinity, AffinityTerm.eval],
        .exponential (ta.sub sa)⟩

end Determinize.Proof.Frontend
