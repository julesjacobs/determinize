import Determinize.Frontend.Affinity

/-!
# Correctness of the affinity solver

The solutions of a list of affinity constraints are closed under pointwise join and meet
(`satisfies_join`, `satisfies_meet`; Lemma L in `notes/inference-optimality/1-claims.md`).
`solveAffinities` succeeds exactly when there is a solution, and then returns one that lies above
every solution (`solveAffinities_spec`; Lemma P).
-/

namespace Determinize.Proof.Frontend
open Determinize.Frontend Spec.Paper

variable {V : Type}

/-- `ρ` satisfies every constraint, in the order `G ≤ E` of `Ty.Sub`. -/
def Satisfies (ρ : V → Affinity) (constraints : List (AffinityConstraint V)) : Prop :=
  ∀ c ∈ constraints, Ty.Sub (.float (c.1.eval ρ)) (.float (c.2.eval ρ))

theorem sub_float_iff {a b : Affinity} :
    Ty.Sub (.float a) (.float b) ↔ a = b ∨ (a = .G ∧ b = .E) := by
  constructor
  · intro h; cases h <;> simp
  · rintro (rfl | ⟨rfl, rfl⟩)
    · exact .float a
    · exact .general

theorem sub_float_E (a : Affinity) : Ty.Sub (.float a) (.float .E) := by
  cases a
  · exact .float _
  · exact .general

theorem eq_G_of_sub_float_G {a : Affinity} (h : Ty.Sub (.float a) (.float .G)) : a = .G := by
  cases h; rfl

theorem affinityLE_iff {a b : Affinity} : affinityLE a b = true ↔ Ty.Sub (.float a) (.float b) := by
  cases a <;> cases b <;> simp [affinityLE, sub_float_iff]

/-! ## Lemma L -/

/-- The maximum in the order `G ≤ E`. -/
def affinityJoin : Affinity → Affinity → Affinity
  | .G, .G => .G
  | _, _ => .E

/-- The minimum in the order `G ≤ E`. -/
def affinityMeet : Affinity → Affinity → Affinity
  | .E, .E => .E
  | _, _ => .G

theorem eval_join (ρ ρ' : V → Affinity) (t : AffinityTerm V) :
    t.eval (fun v => affinityJoin (ρ v) (ρ' v)) = affinityJoin (t.eval ρ) (t.eval ρ') := by
  cases t with
  | var => rfl
  | fixed m => cases m <;> rfl

theorem eval_meet (ρ ρ' : V → Affinity) (t : AffinityTerm V) :
    t.eval (fun v => affinityMeet (ρ v) (ρ' v)) = affinityMeet (t.eval ρ) (t.eval ρ') := by
  cases t with
  | var => rfl
  | fixed m => cases m <;> rfl

theorem satisfies_join {ρ ρ' : V → Affinity} {constraints : List (AffinityConstraint V)}
    (h : Satisfies ρ constraints) (h' : Satisfies ρ' constraints) :
    Satisfies (fun v => affinityJoin (ρ v) (ρ' v)) constraints := by
  intro c mem
  have hc := h c mem
  have hc' := h' c mem
  rw [eval_join, eval_join]
  revert hc hc'
  cases c.1.eval ρ <;> cases c.2.eval ρ <;> cases c.1.eval ρ' <;> cases c.2.eval ρ' <;>
    simp [sub_float_iff, affinityJoin]

theorem satisfies_meet {ρ ρ' : V → Affinity} {constraints : List (AffinityConstraint V)}
    (h : Satisfies ρ constraints) (h' : Satisfies ρ' constraints) :
    Satisfies (fun v => affinityMeet (ρ v) (ρ' v)) constraints := by
  intro c mem
  have hc := h c mem
  have hc' := h' c mem
  rw [eval_meet, eval_meet]
  revert hc hc'
  cases c.1.eval ρ <;> cases c.2.eval ρ <;> cases c.1.eval ρ' <;> cases c.2.eval ρ' <;>
    simp [sub_float_iff, affinityMeet]

/-! ## Lemma P -/

variable [DecidableEq V]

theorem mem_below {constraints : List (AffinityConstraint V)} {general : Finset V} {v : V} :
    v ∈ below constraints general ↔
      ∃ upper, (.var v, upper) ∈ constraints ∧ upper.isGeneral general = true := by
  simp only [below, List.mem_toFinset, List.mem_filterMap]
  constructor
  · rintro ⟨⟨lower, upper⟩, mem, h⟩
    cases lower with
    | var w =>
      simp only [Option.ite_none_right_eq_some, Option.some.injEq] at h
      obtain ⟨g, rfl⟩ := h
      exact ⟨upper, mem, g⟩
    | fixed => simp at h
  · rintro ⟨upper, mem, h⟩
    exact ⟨_, mem, by simp [h]⟩

/-- A solution is G on every term that `isGeneral` accepts, if it is G on `general`. -/
theorem eval_of_isGeneral {ρ : V → Affinity} {general : Finset V}
    (h : ∀ v ∈ general, ρ v = .G) {t : AffinityTerm V} (ht : t.isGeneral general = true) :
    t.eval ρ = .G := by
  cases t with
  | var w => exact h w (by simpa [AffinityTerm.isGeneral] using ht)
  | fixed m => simpa [AffinityTerm.isGeneral, AffinityTerm.eval] using ht

/-- Every solution is G on the variables that `forcedGeneral` collects. -/
theorem forcedGeneral_general {ρ : V → Affinity} {constraints : List (AffinityConstraint V)}
    (hρ : Satisfies ρ constraints) {general : Finset V} (h : ∀ v ∈ general, ρ v = .G) :
    ∀ v ∈ forcedGeneral constraints general, ρ v = .G := by
  induction general using forcedGeneral.induct constraints with
  | case1 general stable => rw [forcedGeneral, if_pos stable]; exact h
  | case2 general unstable ih =>
    rw [forcedGeneral, if_neg unstable]
    refine ih fun v mem => (Finset.mem_union.1 mem).elim (h v) fun mem => ?_
    obtain ⟨upper, mem, isGeneral⟩ := mem_below.1 mem
    have := hρ _ mem
    rw [eval_of_isGeneral h isGeneral] at this
    exact eq_G_of_sub_float_G this

/-- The variables that `forcedGeneral` collects are closed under `below`. -/
theorem below_forcedGeneral (constraints : List (AffinityConstraint V)) (general : Finset V) :
    below constraints (forcedGeneral constraints general) ⊆ forcedGeneral constraints general := by
  induction general using forcedGeneral.induct constraints with
  | case1 general stable => rw [forcedGeneral, if_pos stable]; exact stable
  | case2 general unstable ih => rw [forcedGeneral, if_neg unstable]; exact ih

/-- The assignment that `solveAffinities` checks. -/
private def candidate (constraints : List (AffinityConstraint V)) (v : V) : Affinity :=
  if v ∈ forcedGeneral constraints ∅ then .G else .E

private theorem solveAffinities_eq (constraints : List (AffinityConstraint V)) :
    solveAffinities constraints =
      if constraints.all fun c =>
          affinityLE (c.1.eval (candidate constraints)) (c.2.eval (candidate constraints))
      then some (candidate constraints) else none := rfl

theorem solveAffinities_sound {constraints : List (AffinityConstraint V)} {ρ : V → Affinity}
    (h : solveAffinities constraints = some ρ) : Satisfies ρ constraints := by
  rw [solveAffinities_eq] at h
  split at h
  · cases h
    exact fun c mem => affinityLE_iff.1 (List.all_eq_true.1 ‹_› c mem)
  · cases h

/-- The result of `solveAffinities` lies above every solution. -/
theorem solveAffinities_greatest {constraints : List (AffinityConstraint V)} {ρ ρ' : V → Affinity}
    (h : solveAffinities constraints = some ρ) (h' : Satisfies ρ' constraints) (v : V) :
    Ty.Sub (.float (ρ' v)) (.float (ρ v)) := by
  rw [solveAffinities_eq] at h
  split at h
  · cases h
    unfold candidate
    split
    · rw [forcedGeneral_general h' (general := ∅) (by simp) v ‹_›]; exact .float _
    · exact sub_float_E _
  · cases h

theorem solveAffinities_complete {constraints : List (AffinityConstraint V)} {ρ' : V → Affinity}
    (h' : Satisfies ρ' constraints) : ∃ ρ, solveAffinities constraints = some ρ := by
  refine ⟨candidate constraints, ?_⟩
  suffices ∀ lower upper, (lower, upper) ∈ constraints →
      affinityLE (lower.eval (candidate constraints)) (upper.eval (candidate constraints)) by
    rw [solveAffinities_eq, if_pos (List.all_eq_true.2 fun c mem => this c.1 c.2 mem)]
  intro lower upper mem
  have general := forcedGeneral_general h' (general := ∅) (by simp)
  -- An upper side that the candidate sets to E bounds everything.
  cases hu : upper.eval (candidate constraints) with
  | E => cases lower.eval (candidate constraints) <;> rfl
  | G =>
    have isGeneral : upper.isGeneral (forcedGeneral constraints ∅) = true := by
      cases upper with
      | var w =>
        simp only [AffinityTerm.eval, candidate] at hu
        split at hu <;> simp_all [AffinityTerm.isGeneral]
      | fixed m => simpa [AffinityTerm.eval, AffinityTerm.isGeneral] using hu
    cases lower with
    | var v =>
      have : v ∈ forcedGeneral constraints ∅ :=
        below_forcedGeneral constraints ∅ (mem_below.2 ⟨upper, mem, isGeneral⟩)
      simp [AffinityTerm.eval, candidate, this, affinityLE]
    | fixed m =>
      have := h' _ mem
      rw [eval_of_isGeneral general isGeneral] at this
      have : m = .G := eq_G_of_sub_float_G this
      subst this
      rfl

/-- Lemma P: `solveAffinities` succeeds exactly when the constraints have a solution, and then
returns a solution that lies above every solution. -/
theorem solveAffinities_spec (constraints : List (AffinityConstraint V)) :
    ((∃ ρ, Satisfies ρ constraints) ↔ ∃ ρ, solveAffinities constraints = some ρ) ∧
      ∀ ρ, solveAffinities constraints = some ρ →
        Satisfies ρ constraints ∧
          ∀ ρ', Satisfies ρ' constraints → ∀ v, Ty.Sub (.float (ρ' v)) (.float (ρ v)) :=
  ⟨⟨fun ⟨_, h'⟩ => solveAffinities_complete h', fun ⟨ρ, h⟩ => ⟨ρ, solveAffinities_sound h⟩⟩,
    fun _ h => ⟨solveAffinities_sound h, fun _ h' => solveAffinities_greatest h h'⟩⟩

end Determinize.Proof.Frontend
