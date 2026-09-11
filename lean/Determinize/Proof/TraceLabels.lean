import Determinize.Proof.Measurability
import Determinize.Proof.Internal.StepTraces

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Spec.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper
noncomputable section

instance : MeasurableSpace (Option Op) := ⊤
instance : MeasurableSingletonClass (Option Op) := ⟨fun _ => trivial⟩

/-- The primitive a site records: a general-affinity stochastic draw and nothing else. -/
def siteOp : DistributionAction × Op → Option Op
  | (.sample .G, op) => some op
  | _ => none

@[simp] theorem siteOp_determinize (kind : DistributionAction) (op : Op) :
    siteOp (kind.determinize, op) = siteOp (kind, op) := by
  cases kind with
  | sample affinity => cases affinity <;> rfl
  | mean => rfl

/-- The active generation-affinity sampling site, following call-by-value evaluation order. -/
def generationOp : Skeleton → Option Op
  | .pair l r | .cons l r | .app l r
  | .add l r | .mul l r | .div l r | .lt l r =>
      if l.isValue then generationOp r else generationOp l
  | .fst x | .snd x | .inl x | .inr x
  | .neg x => generationOp x
  | .matchSum x _ _ | .matchList x _ _ | .ite x _ _
  | .letE x _ => generationOp x
  | .uniform kind l r =>
      if l.isValue then if r.isValue then siteOp (kind, .uniform) else generationOp r
      else generationOp l
  | .gaussian kind l r =>
      if l.isValue then if r.isValue then siteOp (kind, .gaussian) else generationOp r
      else generationOp l
  | .poisson kind x => if x.isValue then siteOp (kind, .poisson) else generationOp x
  | .discrete kind d => siteOp (kind, .discrete d)
  | .bernoulli kind x => if x.isValue then siteOp (kind, .bernoulli) else generationOp x
  | .exponential kind x =>
      if x.isValue then siteOp (kind, .exponential) else generationOp x
  | .beta kind l r =>
      if l.isValue then if r.isValue then siteOp (kind, .beta) else generationOp r
      else generationOp l
  | .gamma kind l r =>
      if l.isValue then if r.isValue then siteOp (kind, .gamma) else generationOp r
      else generationOp l
  | _ => none

def entry (op : Option Op) (value : ℝ) : Event := op.map (fun op => (op, value))

theorem generationEvent_eq_entry (site : DistributionAction × Op) (value : ℝ) :
    generationEvent site value = entry (siteOp site) value := by
  rcases site with ⟨kind, op⟩
  cases kind with
  | sample affinity => cases affinity <;> rfl
  | mean => rfl

theorem event_some_measurable : Measurable (some : Op × ℝ → Event) := by
  apply measurable_comap_iff.mpr
  exact measurable_inl

def eventValue (event : Event) : ℝ := event.elim 0 Prod.snd

theorem eventValue_measurable : Measurable eventValue := by
  have h : Measurable (Sum.elim (Prod.snd : Op × ℝ → ℝ) (fun _ : PUnit.{1} => (0 : ℝ))) :=
    measurable_snd.sumElim measurable_const
  have eq : eventValue = (Sum.elim (Prod.snd : Op × ℝ → ℝ) (fun _ : PUnit.{1} => (0 : ℝ))) ∘
      Equiv.optionEquivSumPUnit.{0} (Op × ℝ) := by
    funext event
    cases event <;> rfl
  rw [eq]
  exact h.comp (comap_measurable (Equiv.optionEquivSumPUnit.{0} (Op × ℝ)))

theorem entry_measurable : Measurable (fun pair : Option Op × ℝ => entry pair.1 pair.2) := by
  apply measurable_from_prod_countable_right
  intro op
  cases op with
  | none => exact measurable_const
  | some op => exact event_some_measurable.comp (measurable_const.prodMk measurable_id)

theorem generationEvent_measurable (site : DistributionAction × Op) : Measurable (generationEvent site) := by
  have eq : generationEvent site = entry (siteOp site) := funext (generationEvent_eq_entry site)
  rw [eq]
  exact entry_measurable.comp (measurable_const.prodMk measurable_id)

theorem generationOp_value {skeleton : Skeleton} (value : skeleton.isValue = true) :
    generationOp skeleton = none := by
  fun_induction Expr.isValue skeleton <;> simp_all [generationOp]

set_option linter.unusedSimpArgs false in
set_option maxHeartbeats 800000 in
theorem reduce_site {expression : Expr} {site : DistributionAction × Op} {fiber : Measure ℝ}
    {continuation : ℝ → Expr} (reduction : reduce expression = .sample site fiber continuation) :
    generationOp expression.skeleton = siteOp site := by
  cases expression <;> rw [reduce.eq_def] at reduction
  all_goals dsimp only at reduction
  all_goals repeat' first | contradiction | split at reduction
  all_goals simp_all only [Expr.skeleton, generationOp,
    ← isValue_eq_skeletonIsValue, Bool.true_eq, ↓reduceIte]
  all_goals first
    | (cases reduction; rfl)
    | (obtain ⟨inner, h, _⟩ := Action.wrap_eq_sample reduction
       exact reduce_site h)
termination_by sizeOf expression
decreasing_by
  all_goals subst_vars
  all_goals simp_wf
  all_goals omega

end
end Determinize.Proof.StepTraces
