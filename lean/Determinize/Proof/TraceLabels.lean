import Determinize.Proof.Measurability
import Determinize.Proof.Internal.StepTraces

namespace Determinize.Proof.StepTraces
open MeasureTheory ProbabilityTheory Determinize.Statement.Paper Determinize.Proof.StepTraces
open Determinize.Proof.Paper
noncomputable section

instance : MeasurableSpace (Option Op) := ⊤
instance : MeasurableSingletonClass (Option Op) := ⟨fun _ => trivial⟩

/-- The generation-mode primitive inside the first unevaluated operand. -/
def operandOp : List (Bool × Option Op) → Option (Option Op)
  | [] => none
  | (value, op) :: tail => if value then operandOp tail else some op

/-- The active generation-mode sampling site, following call-by-value evaluation order. -/
def generationOp : Skeleton → Option Op
  | .pair l r | .cons l r | .app l r
  | .add _ l r | .mul _ l r | .div _ l r | .lt l r =>
      if l.isValue then generationOp r else generationOp l
  | .fst x | .snd x | .inl x | .inr x
  | .promote x | .neg _ x => generationOp x
  | .matchSum x _ _ | .matchList x _ _ | .ite x _ _
  | .letE x _ => generationOp x
  | .sample mode tag affine general =>
      match operandOp (affine.map (fun x => (x.isValue, generationOp x)) ++
          general.map (fun x => (x.isValue, generationOp x))) with
      | some op => op
      | none => match mode, tag with
        | .G, .stochastic op => some op
        | _, _ => none
  | _ => none

def entry (op : Option Op) (value : ℝ) : Event := op.map (fun op => (op, value))


def siteOp : Mode × Tag → Option Op
  | (.G, .stochastic op) => some op
  | _ => none

theorem generationEvent_eq_entry (site : Mode × Tag) (value : ℝ) :
    generationEvent site value = entry (siteOp site) value := by
  rcases site with ⟨mode, tag⟩
  cases mode <;> cases tag <;> rfl

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

theorem generationEvent_measurable (site : Mode × Tag) : Measurable (generationEvent site) := by
  have eq : generationEvent site = entry (siteOp site) := funext (generationEvent_eq_entry site)
  rw [eq]
  exact entry_measurable.comp (measurable_const.prodMk measurable_id)

theorem generationOp_value {skeleton : Skeleton} (value : skeleton.isValue = true) :
    generationOp skeleton = none := by
  fun_induction Expr.isValue skeleton <;> simp_all [generationOp]

theorem operandOp_append (left right : List (Bool × Option Op)) :
    operandOp (left ++ right) = (operandOp left).or (operandOp right) := by
  induction left with
  | nil => rfl
  | cons head tail ih =>
      rcases head with ⟨value, op⟩
      cases value <;> simp [operandOp, ih]

theorem operandOp_expr (arguments : List Expr) :
    operandOp (arguments.map (fun e => (e.skeleton.isValue, generationOp e.skeleton))) =
      (firstNonValue arguments).map (fun found => generationOp found.2.1.skeleton) := by
  induction arguments with
  | nil => rfl
  | cons head tail ih =>
      simp only [← isValue_eq_skeletonIsValue] at ih ⊢
      simp only [List.map_cons, operandOp, firstNonValue]
      cases headValue : head.isValue <;> simp only [Bool.false_eq_true, ↓reduceIte]
      · rfl
      · rw [ih]
        cases firstNonValue tail <;> rfl

theorem generationOp_sample (mode : Mode) (tag : Tag) (affine general : List Expr) :
    generationOp (.sample mode tag (affine.map Expr.skeleton) (general.map Expr.skeleton)) =
      match firstNonValue affine with
      | some (_, current, _) => generationOp current.skeleton
      | none => match firstNonValue general with
        | some (_, current, _) => generationOp current.skeleton
        | none => siteOp (mode, tag) := by
  rw [generationOp.eq_def]
  simp only [List.map_map, Function.comp_def, operandOp_append, operandOp_expr]
  cases firstNonValue affine <;> cases firstNonValue general <;> simp [siteOp]
  all_goals cases mode <;> cases tag <;> rfl

set_option linter.unusedSimpArgs false in
set_option maxHeartbeats 800000 in
theorem reduce_site {expression : Expr} {site : Mode × Tag} {fiber : Measure ℝ}
    {continuation : ℝ → Expr} (reduction : reduce expression = .sample site fiber continuation) :
    generationOp expression.skeleton = siteOp site := by
  cases expression <;> rw [reduce.eq_def] at reduction
  all_goals dsimp only at reduction
  all_goals repeat' first | contradiction | split at reduction
  all_goals simp_all only [Expr.skeleton, generationOp_sample, generationOp,
    ← isValue_eq_skeletonIsValue, Bool.true_eq, ↓reduceIte]
  all_goals first
    | (cases reduction; rfl)
    | (obtain ⟨inner, h, _⟩ := Action.wrap_eq_sample reduction
       exact reduce_site h)
termination_by sizeOf expression
decreasing_by
  all_goals subst_vars
  all_goals simp_wf
  all_goals first
    | omega
    | (apply Nat.lt_trans (List.sizeOf_lt_of_mem (firstNonValue_current_mem (by assumption)))
       simp_wf <;> omega)

end
end Determinize.Proof.StepTraces
