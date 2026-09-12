import Determinize.Proof.Measurability

/-! # Type safety for the paper semantics -/

set_option linter.unusedSimpArgs false
set_option linter.unusedTactic false
set_option linter.unreachableTactic false
set_option linter.unnecessarySeqFocus false
set_option linter.unnecessarySimpa false
set_option linter.style.haveILetI false
set_option linter.unusedVariables false

namespace Determinize.Proof.Paper

open MeasureTheory ProbabilityTheory
open Determinize.Statement.Paper

namespace Typing

theorem hasVar_prepend (h : HasVar suffix index ty) :
    HasVar (inserted ++ suffix) (index + inserted.length) ty := by
  induction inserted with
  | nil => simpa
  | cons head inserted ih =>
      simpa [Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using
        HasVar.tail (head := head) ih

theorem hasVar_shift (h : HasVar (before ++ suffix) index ty) :
    HasVar (before ++ inserted ++ suffix)
      (if before.length ≤ index then index + inserted.length else index) ty := by
  induction before generalizing index with
  | nil =>
      simp only [List.nil_append, List.length_nil, Nat.zero_le, ↓reduceIte]
      exact hasVar_prepend h
  | cons head before ih =>
      cases h with
      | head =>
          simp only [List.cons_append, List.length_cons, Nat.not_succ_le_zero,
            ↓reduceIte]
          exact .head
      | tail h =>
          rename_i index
          simp only [List.cons_append, List.length_cons, Nat.succ_le_succ_iff]
          by_cases condition : before.length ≤ index
          · simp only [condition, ↓reduceIte]
            have shifted := ih h
            simp only [condition, ↓reduceIte] at shifted
            convert HasVar.tail (head := head) shifted using 1 <;> omega
          · simp only [condition, ↓reduceIte]
            have shifted := ih h
            simp only [condition, ↓reduceIte] at shifted
            exact HasVar.tail (head := head) shifted

theorem typed_shift (h : Typed (before ++ suffix) expression ty) :
    Typed (before ++ inserted ++ suffix)
      (expression.shift inserted.length before.length) ty := by
  generalize hcontext : before ++ suffix = context at h
  induction h generalizing before suffix with
  | bvar hvar =>
      rw [← hcontext] at hvar
      rw [Expr.shift, Expr.mapVars]
      exact .bvar (hasVar_shift hvar)
  | unit => rw [Expr.shift, Expr.mapVars]; exact .unit
  | bool => rw [Expr.shift, Expr.mapVars]; exact .bool
  | real => rw [Expr.shift, Expr.mapVars]; exact .real
  | lam h ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .lam (ih (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
  | fix h ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .fix (ih (before := _ :: _ :: before) (suffix := suffix) (by simpa using hcontext))
  | app hf hx ihf ihx =>
      rw [Expr.shift, Expr.mapVars]
      exact .app (ihf (before := before) (suffix := suffix) hcontext)
        (ihx (before := before) (suffix := suffix) hcontext)
  | pair hl hr ihl ihr =>
      rw [Expr.shift, Expr.mapVars]
      exact .pair (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | fst hp ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .fst (ih (before := before) (suffix := suffix) hcontext)
  | snd hp ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .snd (ih (before := before) (suffix := suffix) hcontext)
  | inl hv ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .inl (ih (before := before) (suffix := suffix) hcontext)
  | inr hv ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .inr (ih (before := before) (suffix := suffix) hcontext)
  | matchSum hs hl hr ihs ihl ihr =>
      rw [Expr.shift, Expr.mapVars]
      exact .matchSum (ihs (before := before) (suffix := suffix) hcontext)
        (ihl (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
        (ihr (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
  | nil => rw [Expr.shift, Expr.mapVars]; exact .nil
  | cons hh ht ihh iht =>
      rw [Expr.shift, Expr.mapVars]
      exact .cons (ihh (before := before) (suffix := suffix) hcontext)
        (iht (before := before) (suffix := suffix) hcontext)
  | matchList hs hn hc ihs ihn ihc =>
      rw [Expr.shift, Expr.mapVars]
      exact .matchList (ihs (before := before) (suffix := suffix) hcontext)
        (ihn (before := before) (suffix := suffix) hcontext)
        (ihc (before := _ :: _ :: before) (suffix := suffix) (by simpa using hcontext))
  | ite hc ht he ihc iht ihe =>
      rw [Expr.shift, Expr.mapVars]
      exact .ite (ihc (before := before) (suffix := suffix) hcontext)
        (iht (before := before) (suffix := suffix) hcontext)
        (ihe (before := before) (suffix := suffix) hcontext)
  | letE hv hb ihv ihb =>
      rw [Expr.shift, Expr.mapVars]
      exact .letE (ihv (before := before) (suffix := suffix) hcontext)
        (ihb (before := _ :: before) (suffix := suffix) (by simpa using hcontext))
  | observe hv ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .observe (ih (before := before) (suffix := suffix) hcontext)
  | promote hv ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .promote (ih (before := before) (suffix := suffix) hcontext)
  | neg hv ih =>
      rw [Expr.shift, Expr.mapVars]
      exact .neg (ih (before := before) (suffix := suffix) hcontext)
  | add hl hr ihl ihr =>
      rw [Expr.shift, Expr.mapVars]
      exact .add (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | mul hl hr ihl ihr =>
      rw [Expr.shift, Expr.mapVars]
      exact .mul (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | div hl hr ihl ihr =>
      rw [Expr.shift, Expr.mapVars]
      exact .div (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | lt hl hr ihl ihr =>
      rw [Expr.shift, Expr.mapVars]
      exact .lt (ihl (before := before) (suffix := suffix) hcontext)
        (ihr (before := before) (suffix := suffix) hcontext)
  | uniform hl hr ihl ihr | gaussian hl hr ihl ihr | beta hl hr ihl ihr | gamma hl hr ihl ihr =>
      rw [Expr.shift, Expr.mapVars]
      first
      | exact .uniform (ihl (before := before) (suffix := suffix) hcontext)
          (ihr (before := before) (suffix := suffix) hcontext)
      | exact .gaussian (ihl (before := before) (suffix := suffix) hcontext)
          (ihr (before := before) (suffix := suffix) hcontext)
      | exact .beta (ihl (before := before) (suffix := suffix) hcontext)
          (ihr (before := before) (suffix := suffix) hcontext)
      | exact .gamma (ihl (before := before) (suffix := suffix) hcontext)
          (ihr (before := before) (suffix := suffix) hcontext)
  | poisson hv ih | exponential hv ih | bernoulli hv ih | discrete hv ih =>
      rw [Expr.shift, Expr.mapVars]
      first
      | exact .poisson (ih (before := before) (suffix := suffix) hcontext)
      | exact .exponential (ih (before := before) (suffix := suffix) hcontext)
      | exact .bernoulli (ih (before := before) (suffix := suffix) hcontext)
      | exact .discrete (ih (before := before) (suffix := suffix) hcontext)

theorem hasVar_subst (h : HasVar (before ++ binder :: suffix) index ty) :
    (index = before.length ∧ ty = binder) ∨
      (index ≠ before.length ∧ HasVar (before ++ suffix)
        (if before.length < index then index - 1 else index) ty) := by
  induction before generalizing index with
  | nil =>
      cases h with
      | head => exact .inl ⟨rfl, rfl⟩
      | tail h =>
          right
          exact ⟨by simp only [List.length_nil]; omega, by simpa using h⟩
  | cons head before ih =>
      cases h with
      | head =>
          right
          simp only [List.cons_append, List.length_cons, Nat.zero_lt_succ,
            ↓reduceIte]
          exact ⟨by omega, .head⟩
      | tail h =>
          rcases ih h with equal | shifted
          · left
            exact ⟨congrArg Nat.succ equal.1, equal.2⟩
          · right
            rcases shifted with ⟨notEqual, shifted⟩
            rename_i index
            constructor
            · simp only [List.length_cons]
              omega
            simp only [List.cons_append, List.length_cons, Nat.succ_lt_succ_iff]
            by_cases condition : before.length < index
            · simp only [condition, ↓reduceIte]
              have shifted := shifted
              simp only [condition, ↓reduceIte] at shifted
              have shifted' := HasVar.tail (head := head) shifted
              convert shifted' using 1 <;> omega
            · simp only [condition, ↓reduceIte]
              have shifted := shifted
              simp only [condition, ↓reduceIte] at shifted
              exact HasVar.tail (head := head) shifted

theorem typed_substAt (h : Typed (before ++ binder :: suffix) expression ty)
    (replacementTyped : Typed suffix replacement binder) :
    Typed (before ++ suffix)
      (Expr.substAt before.length replacement expression) ty := by
  generalize hcontext : before ++ binder :: suffix = context at h
  induction h generalizing before suffix with
  | bvar hvar =>
      rw [← hcontext] at hvar
      rcases hasVar_subst hvar with equal | shifted
      · rcases equal with ⟨rfl, rfl⟩
        rw [Expr.substAt, Expr.mapVars, if_pos rfl]
        simpa only [List.nil_append, List.append_assoc, List.length_nil] using
          (typed_shift (before := []) (suffix := suffix) (inserted := before)
            replacementTyped)
      · rcases shifted with ⟨notEqual, shifted⟩
        rw [Expr.substAt, Expr.mapVars, if_neg notEqual]
        exact .bvar shifted
  | unit => rw [Expr.substAt, Expr.mapVars]; exact .unit
  | bool => rw [Expr.substAt, Expr.mapVars]; exact .bool
  | real => rw [Expr.substAt, Expr.mapVars]; exact .real
  | lam h ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .lam (ih replacementTyped (before := _ :: before) (suffix := suffix)
        (by simpa using hcontext))
  | fix h ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .fix (ih replacementTyped (before := _ :: _ :: before) (suffix := suffix)
        (by simpa using hcontext))
  | app hf hx ihf ihx =>
      rw [Expr.substAt, Expr.mapVars]
      exact .app (ihf replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihx replacementTyped (before := before) (suffix := suffix) hcontext)
  | pair hl hr ihl ihr =>
      rw [Expr.substAt, Expr.mapVars]
      exact .pair (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | fst hp ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .fst (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | snd hp ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .snd (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | inl hv ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .inl (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | inr hv ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .inr (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | matchSum hs hl hr ihs ihl ihr =>
      rw [Expr.substAt, Expr.mapVars]
      exact .matchSum
        (ihs replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihl replacementTyped (before := _ :: before) (suffix := suffix)
          (by simpa using hcontext))
        (ihr replacementTyped (before := _ :: before) (suffix := suffix)
          (by simpa using hcontext))
  | nil => rw [Expr.substAt, Expr.mapVars]; exact .nil
  | cons hh ht ihh iht =>
      rw [Expr.substAt, Expr.mapVars]
      exact .cons (ihh replacementTyped (before := before) (suffix := suffix) hcontext)
        (iht replacementTyped (before := before) (suffix := suffix) hcontext)
  | matchList hs hn hc ihs ihn ihc =>
      rw [Expr.substAt, Expr.mapVars]
      exact .matchList
        (ihs replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihn replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihc replacementTyped (before := _ :: _ :: before) (suffix := suffix)
          (by simpa using hcontext))
  | ite hc ht he ihc iht ihe =>
      rw [Expr.substAt, Expr.mapVars]
      exact .ite (ihc replacementTyped (before := before) (suffix := suffix) hcontext)
        (iht replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihe replacementTyped (before := before) (suffix := suffix) hcontext)
  | letE hv hb ihv ihb =>
      rw [Expr.substAt, Expr.mapVars]
      exact .letE (ihv replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihb replacementTyped (before := _ :: before) (suffix := suffix)
          (by simpa using hcontext))
  | observe hv ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .observe (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | promote hv ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .promote (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | neg hv ih =>
      rw [Expr.substAt, Expr.mapVars]
      exact .neg (ih replacementTyped (before := before) (suffix := suffix) hcontext)
  | add hl hr ihl ihr =>
      rw [Expr.substAt, Expr.mapVars]
      exact .add (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | mul hl hr ihl ihr =>
      rw [Expr.substAt, Expr.mapVars]
      exact .mul (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | div hl hr ihl ihr =>
      rw [Expr.substAt, Expr.mapVars]
      exact .div (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | lt hl hr ihl ihr =>
      rw [Expr.substAt, Expr.mapVars]
      exact .lt (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
        (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | uniform hl hr ihl ihr | gaussian hl hr ihl ihr | beta hl hr ihl ihr | gamma hl hr ihl ihr =>
      rw [Expr.substAt, Expr.mapVars]
      first
      | exact .uniform (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
          (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
      | exact .gaussian (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
          (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
      | exact .beta (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
          (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
      | exact .gamma (ihl replacementTyped (before := before) (suffix := suffix) hcontext)
          (ihr replacementTyped (before := before) (suffix := suffix) hcontext)
  | poisson hv ih | exponential hv ih | bernoulli hv ih | discrete hv ih =>
      rw [Expr.substAt, Expr.mapVars]
      first
      | exact .poisson (ih replacementTyped (before := before) (suffix := suffix) hcontext)
      | exact .exponential (ih replacementTyped (before := before) (suffix := suffix) hcontext)
      | exact .bernoulli (ih replacementTyped (before := before) (suffix := suffix) hcontext)
      | exact .discrete (ih replacementTyped (before := before) (suffix := suffix) hcontext)
theorem typed_substHead (bodyTyped : Typed (binder :: suffix) body ty)
    (replacementTyped : Typed suffix replacement binder) :
    Typed suffix (Expr.substHead body replacement) ty := by
  simpa [Expr.substHead] using
    typed_substAt (before := []) bodyTyped replacementTyped

theorem typed_substTwo
    (bodyTyped : Typed (argumentTy :: functionTy :: suffix) body resultTy)
    (argumentTyped : Typed suffix argument argumentTy)
    (functionTyped : Typed suffix function functionTy) :
    Typed suffix (Expr.substTwo body argument function) resultTy := by
  rw [Expr.substTwo]
  apply typed_substHead
  · exact typed_substAt (before := [argumentTy]) bodyTyped functionTyped
  · exact argumentTyped

/-- The actions a closed well-typed expression can take: a well-typed successor, a sample
with well-typed continuations, or a rejection; never `stuck`. -/
inductive ActionTyped (ty : Ty) : Action → Prop
  | next : Typed [] expression ty → ActionTyped ty (.next expression)
  | sample : (∀ value, Typed [] (continuation value) ty) →
      ActionTyped ty (.sample site fiber continuation)
  | reject : ActionTyped ty .reject

theorem ActionTyped.wrap (actionTyped : ActionTyped childTy action)
    (wrapTyped : ∀ expression, Typed [] expression childTy →
      Typed [] (context expression) resultTy) :
    ActionTyped resultTy (action.wrap context) := by
  cases actionTyped with
  | next typed => exact .next (wrapTyped _ typed)
  | sample typed =>
      exact .sample fun value => wrapTyped _ (typed value)
  | reject => exact .reject

@[simp] theorem noVar_nil : ¬ HasVar [] index ty := by
  intro h
  cases h

theorem typed_arr_value (typed : Typed [] expression (.arr argument result))
    (value : expression.isValue = true) :
    (∃ body, expression = .lam body ∧
      Typed [argument] body result) ∨
    (∃ body, expression = .fix body ∧
      Typed [argument, .arr argument result] body result) := by
  cases typed <;> simp_all [Expr.isValue]

theorem typed_prod_value (typed : Typed [] expression (.prod leftTy rightTy))
    (value : expression.isValue = true) :
    ∃ left right, expression = .pair left right ∧
      Typed [] left leftTy ∧ Typed [] right rightTy := by
  cases typed <;> simp_all [Expr.isValue]

theorem typed_sum_value (typed : Typed [] expression (.sum leftTy rightTy))
    (value : expression.isValue = true) :
    (∃ body, expression = .inl body ∧
      Typed [] body leftTy) ∨
    (∃ body, expression = .inr body ∧
      Typed [] body rightTy) := by
  cases typed <;> simp_all [Expr.isValue]

theorem typed_list_value (typed : Typed [] expression (.list element))
    (value : expression.isValue = true) :
    expression = .nil ∨
      ∃ head tail, expression = .cons head tail ∧
        Typed [] head element ∧ Typed [] tail (.list element) := by
  cases typed <;> simp_all [Expr.isValue]

theorem typed_bool_value (typed : Typed [] expression .bool)
    (value : expression.isValue = true) :
    ∃ result, expression = .bool result := by
  cases typed <;> simp_all [Expr.isValue]

theorem typed_real_value (typed : Typed [] expression (.float mode))
    (value : expression.isValue = true) :
    ∃ result, expression = .real result := by
  cases typed <;> simp_all [Expr.isValue]

/-- A closed list value of floats is a cons-chain of real literals, which `realListValue?`
reads off; so a `discrete` site with evaluated well-typed weights never gets stuck. -/
theorem typed_realList_value (typed : Typed [] expression (.list (.float mode)))
    (value : expression.isValue = true) :
    ∃ values, realListValue? expression = some values := by
  induction expression with
  | nil => exact ⟨[], rfl⟩
  | cons head tail _ ih =>
      cases typed with
      | cons headTyped tailTyped =>
          simp only [Expr.isValue, Bool.and_eq_true] at value
          obtain ⟨coordinate, rfl⟩ := typed_real_value headTyped value.1
          obtain ⟨values, valuesEq⟩ := ih tailTyped value.2
          exact ⟨coordinate :: values, by simp [realListValue?, realValue?, valuesEq]⟩
  | _ => cases typed <;> simp_all [Expr.isValue]

theorem reduce_typed_closed
    (typed : Typed [] expression ty) : ActionTyped ty (reduce expression) := by
  generalize hcontext : ([] : List Ty) = context at typed
  induction typed with
  | bvar hvar =>
      rw [← hcontext] at hvar
      exact (noVar_nil hvar).elim
  | unit => rw [reduce]; exact .next .unit
  | bool => rw [reduce]; exact .next .bool
  | real => rw [reduce]; exact .next .real
  | lam bodyTyped ih =>
      cases hcontext
      rw [reduce]
      exact .next (.lam bodyTyped)
  | fix bodyTyped ih =>
      cases hcontext
      rw [reduce]
      exact .next (.fix bodyTyped)
  | app functionTyped argumentTyped ihf iha =>
      cases hcontext
      rename_i function argumentTy result operand
      rw [MeasurableActionFamily.reduce_app_eq]
      by_cases functionValue : function.isValue = true
      · simp only [functionValue, ↓reduceIte]
        by_cases argumentValue : operand.isValue = true
        · simp only [argumentValue, ↓reduceIte]
          rcases typed_arr_value functionTyped functionValue with function | function
          · rcases function with ⟨body, rfl, bodyTyped⟩
            simpa using ActionTyped.next (typed_substHead bodyTyped argumentTyped)
          · rcases function with ⟨body, rfl, bodyTyped⟩
            simpa using ActionTyped.next
              (typed_substTwo bodyTyped argumentTyped (.fix bodyTyped))
        · simp only [argumentValue, ↓reduceIte]
          exact (iha rfl).wrap fun next nextTyped => .app functionTyped nextTyped
      · simp only [functionValue, ↓reduceIte]
        exact (ihf rfl).wrap fun next nextTyped => .app nextTyped argumentTyped
  | pair leftTyped rightTyped ihl ihr =>
      cases hcontext
      rw [reduce]
      split
      · split
        · exact .next (.pair leftTyped rightTyped)
        · exact (ihr rfl).wrap fun next nextTyped => .pair leftTyped nextTyped
      · exact (ihl rfl).wrap fun next nextTyped => .pair nextTyped rightTyped
  | fst pairTyped ih =>
      cases hcontext
      rename_i pair leftTy rightTy
      rw [MeasurableActionFamily.reduce_fst_eq]
      by_cases pairValue : pair.isValue = true
      · simp only [pairValue, ↓reduceIte]
        rcases typed_prod_value pairTyped pairValue with ⟨left, right, rfl, leftTyped, rightTyped⟩
        simpa using ActionTyped.next leftTyped
      · simp only [pairValue, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .fst nextTyped
  | snd pairTyped ih =>
      cases hcontext
      rename_i pair leftTy rightTy
      rw [MeasurableActionFamily.reduce_snd_eq]
      by_cases pairValue : pair.isValue = true
      · simp only [pairValue, ↓reduceIte]
        rcases typed_prod_value pairTyped pairValue with ⟨left, right, rfl, leftTyped, rightTyped⟩
        simpa using ActionTyped.next rightTyped
      · simp only [pairValue, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .snd nextTyped
  | inl valueTyped ih =>
      cases hcontext
      rw [reduce]
      split
      · exact .next (.inl valueTyped)
      · exact (ih rfl).wrap fun next nextTyped => .inl nextTyped
  | inr valueTyped ih =>
      cases hcontext
      rw [reduce]
      split
      · exact .next (.inr valueTyped)
      · exact (ih rfl).wrap fun next nextTyped => .inr nextTyped
  | matchSum scrutineeTyped leftTyped rightTyped ihs ihl ihr =>
      cases hcontext
      rename_i scrutinee leftTy rightTy left result right
      rw [MeasurableActionFamily.reduce_matchSum_eq]
      by_cases scrutineeValue : scrutinee.isValue = true
      · simp only [scrutineeValue, ↓reduceIte]
        rcases typed_sum_value scrutineeTyped scrutineeValue with sum | sum
        · rcases sum with ⟨value, rfl, valueTyped⟩
          simpa using ActionTyped.next (typed_substHead leftTyped valueTyped)
        · rcases sum with ⟨value, rfl, valueTyped⟩
          simpa using ActionTyped.next (typed_substHead rightTyped valueTyped)
      · simp only [scrutineeValue, ↓reduceIte]
        exact (ihs rfl).wrap fun next nextTyped =>
          .matchSum nextTyped leftTyped rightTyped
  | nil => rw [reduce]; exact .next .nil
  | cons headTyped tailTyped ihh iht =>
      cases hcontext
      rw [reduce]
      split
      · split
        · exact .next (.cons headTyped tailTyped)
        · exact (iht rfl).wrap fun next nextTyped => .cons headTyped nextTyped
      · exact (ihh rfl).wrap fun next nextTyped => .cons nextTyped tailTyped
  | matchList scrutineeTyped nilTyped consTyped ihs ihn ihc =>
      cases hcontext
      rename_i scrutinee element nilCase result consCase
      rw [MeasurableActionFamily.reduce_matchList_eq]
      by_cases scrutineeValue : scrutinee.isValue = true
      · simp only [scrutineeValue, ↓reduceIte]
        rcases typed_list_value scrutineeTyped scrutineeValue with nil | cons
        · rw [nil]
          simpa using ActionTyped.next nilTyped
        · rcases cons with ⟨head, tail, rfl, headTyped, tailTyped⟩
          simpa using ActionTyped.next (typed_substTwo consTyped headTyped tailTyped)
      · simp only [scrutineeValue, ↓reduceIte]
        exact (ihs rfl).wrap fun next nextTyped =>
          .matchList nextTyped nilTyped consTyped
  | ite conditionTyped thenTyped elseTyped ihc iht ihe =>
      cases hcontext
      rename_i condition thenBranch result elseBranch
      rw [MeasurableActionFamily.reduce_ite_eq]
      by_cases conditionValue : condition.isValue = true
      · simp only [conditionValue, ↓reduceIte]
        rcases typed_bool_value conditionTyped conditionValue with ⟨result, rfl⟩
        cases result
        · simpa using ActionTyped.next elseTyped
        · simpa using ActionTyped.next thenTyped
      · simp only [conditionValue, ↓reduceIte]
        exact (ihc rfl).wrap fun next nextTyped => .ite nextTyped thenTyped elseTyped
  | letE valueTyped bodyTyped ihv ihb =>
      cases hcontext
      rename_i value valueTy body result
      rw [MeasurableActionFamily.reduce_let_eq]
      by_cases valueCondition : value.isValue = true
      · simp only [valueCondition, ↓reduceIte]
        exact .next (typed_substHead bodyTyped valueTyped)
      · simp only [valueCondition, ↓reduceIte]
        exact (ihv rfl).wrap fun next nextTyped => .letE nextTyped bodyTyped
  | observe conditionTyped ih =>
      cases hcontext
      rename_i condition
      rw [MeasurableActionFamily.reduce_observe_eq]
      by_cases conditionValue : condition.isValue = true
      · simp only [conditionValue, ↓reduceIte]
        rcases typed_bool_value conditionTyped conditionValue with ⟨result, rfl⟩
        cases result
        · simpa using ActionTyped.reject (ty := .unit)
        · simpa using ActionTyped.next (Typed.unit (context := []))
      · simp only [conditionValue, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .observe nextTyped
  | promote valueTyped ih =>
      cases hcontext
      rename_i value
      rw [MeasurableActionFamily.reduce_promote_eq]
      by_cases valueCondition : value.isValue = true
      · simp only [valueCondition, ↓reduceIte]
        rcases typed_real_value valueTyped valueCondition with ⟨coordinate, rfl⟩
        simpa using ActionTyped.next (Typed.real (value := coordinate))
      · simp only [valueCondition, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .promote nextTyped
  | neg valueTyped ih =>
      cases hcontext
      rename_i value mode
      rw [MeasurableActionFamily.reduce_neg_eq]
      by_cases valueCondition : value.isValue = true
      · simp only [valueCondition, ↓reduceIte]
        rcases typed_real_value valueTyped valueCondition with ⟨coordinate, rfl⟩
        simpa using ActionTyped.next (Typed.real (value := -coordinate))
      · simp only [valueCondition, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .neg nextTyped
  | add leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left mode right
      rw [MeasurableActionFamily.reduce_add_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .next .real
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .add leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .add nextTyped rightTyped
  | mul leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left right mode
      rw [MeasurableActionFamily.reduce_mul_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .next .real
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .mul leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .mul nextTyped rightTyped
  | div leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left mode right
      rw [MeasurableActionFamily.reduce_div_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .next .real
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .div leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .div nextTyped rightTyped
  | lt leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left right
      rw [MeasurableActionFamily.reduce_lt_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .next .bool
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .lt leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .lt nextTyped rightTyped
  | uniform leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left mode right kind
      rw [MeasurableActionFamily.reduce_uniform_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .sample fun value => .real
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .uniform leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .uniform nextTyped rightTyped
  | gaussian leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left mode right kind
      rw [MeasurableActionFamily.reduce_gaussian_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .sample fun value => .real
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .gaussian leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .gaussian nextTyped rightTyped
  | poisson valueTyped ih =>
      cases hcontext
      rename_i value mode kind
      rw [MeasurableActionFamily.reduce_poisson_eq]
      by_cases valueCondition : value.isValue = true
      · simp only [valueCondition, ↓reduceIte]
        rcases typed_real_value valueTyped valueCondition with ⟨coordinate, rfl⟩
        exact .sample fun value => .real
      · simp only [valueCondition, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .poisson nextTyped
  | exponential valueTyped ih =>
      cases hcontext
      rename_i value mode kind
      rw [MeasurableActionFamily.reduce_exponential_eq]
      by_cases valueCondition : value.isValue = true
      · simp only [valueCondition, ↓reduceIte]
        rcases typed_real_value valueTyped valueCondition with ⟨coordinate, rfl⟩
        exact .sample fun value => .real
      · simp only [valueCondition, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .exponential nextTyped
  | bernoulli valueTyped ih =>
      cases hcontext
      rename_i value mode kind
      rw [MeasurableActionFamily.reduce_bernoulli_eq]
      by_cases valueCondition : value.isValue = true
      · simp only [valueCondition, ↓reduceIte]
        rcases typed_real_value valueTyped valueCondition with ⟨coordinate, rfl⟩
        exact .sample fun value => .real
      · simp only [valueCondition, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .bernoulli nextTyped
  | discrete weightsTyped ih =>
      cases hcontext
      rename_i weights mode kind
      rw [MeasurableActionFamily.reduce_discrete_eq]
      by_cases weightsValue : weights.isValue = true
      · simp only [weightsValue, ↓reduceIte]
        rcases typed_realList_value weightsTyped weightsValue with ⟨values, valuesEq⟩
        simp only [valuesEq]
        exact .sample fun value => .real
      · simp only [weightsValue, ↓reduceIte]
        exact (ih rfl).wrap fun next nextTyped => .discrete nextTyped
  | beta leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left right mode kind
      rw [MeasurableActionFamily.reduce_beta_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .sample fun value => .real
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .beta leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .beta nextTyped rightTyped
  | gamma leftTyped rightTyped ihl ihr =>
      cases hcontext
      rename_i left mode right kind
      rw [MeasurableActionFamily.reduce_gamma_eq]
      by_cases leftValue : left.isValue = true
      · simp only [leftValue, ↓reduceIte]
        by_cases rightValue : right.isValue = true
        · simp only [rightValue, ↓reduceIte]
          rcases typed_real_value leftTyped leftValue with ⟨left, rfl⟩
          rcases typed_real_value rightTyped rightValue with ⟨right, rfl⟩
          exact .sample fun value => .real
        · simp only [rightValue, ↓reduceIte]
          exact (ihr rfl).wrap fun next nextTyped => .gamma leftTyped nextTyped
      · simp only [leftValue, ↓reduceIte]
        exact (ihl rfl).wrap fun next nextTyped => .gamma nextTyped rightTyped

theorem doesNotGetStuckAt_imp_primitiveDomainSafeAt
    (safe : Determinize.Statement.Paper.DoesNotGetStuckAt fuel expression) :
    PrimitiveDomainSafeAt fuel expression := by
  induction fuel generalizing expression with
  | zero => trivial
  | succ fuel ih =>
      simp only [DoesNotGetStuckAt, PrimitiveDomainSafeAt] at safe ⊢
      split <;> rename_i value
      · trivial
      · have valueFalse := Bool.eq_false_of_not_eq_true value
        rw [valueFalse] at safe
        cases equation : reduce expression with
        | next next =>
            rw [equation] at safe
            simp only at safe ⊢
            exact ih safe
        | sample site fiber continuation =>
            rw [equation] at safe
            simp only at safe ⊢
            refine ⟨safe.1, ?_⟩
            filter_upwards [safe.2] with coordinate coordinateSafe
            exact ih coordinateSafe
        | stuck =>
            rw [equation] at safe
            contradiction
        | reject => trivial

theorem primitiveDomainSafeAt_imp_doesNotGetStuckAt
    (typed : Typed [] expression ty)
    (safe : PrimitiveDomainSafeAt fuel expression) :
    Determinize.Statement.Paper.DoesNotGetStuckAt fuel expression := by
  induction fuel generalizing expression ty with
  | zero => trivial
  | succ fuel ih =>
      simp only [PrimitiveDomainSafeAt, DoesNotGetStuckAt] at safe ⊢
      split <;> rename_i value
      · trivial
      · have valueFalse := Bool.eq_false_of_not_eq_true value
        rw [valueFalse] at safe
        have actionTyped := reduce_typed_closed typed
        cases equation : reduce expression with
        | next next =>
            rw [equation] at safe
            simp only at safe ⊢
            rw [equation] at actionTyped
            cases actionTyped with
            | next nextTyped => exact ih nextTyped safe
        | sample site fiber continuation =>
            rw [equation] at safe
            simp only at safe ⊢
            rw [equation] at actionTyped
            cases actionTyped with
            | sample continuationTyped =>
                refine ⟨safe.1, ?_⟩
                filter_upwards [safe.2] with coordinate coordinateSafe
                exact ih (continuationTyped coordinate) coordinateSafe
        | stuck =>
            rw [equation] at actionTyped
            cases actionTyped
        | reject => trivial

theorem primitiveDomainSafe_iff_doesNotGetStuck
    (typed : Typed [] expression ty) :
    PrimitiveDomainSafe expression ↔ Determinize.Statement.Paper.DoesNotGetStuck expression := by
  constructor
  · intro safe fuel
    exact primitiveDomainSafeAt_imp_doesNotGetStuckAt typed (safe fuel)
  · intro safe fuel
    exact doesNotGetStuckAt_imp_primitiveDomainSafeAt (safe fuel)

theorem doesNotGetStuckAt_of_value (fuel : Nat) (value : expression.isValue = true) :
    DoesNotGetStuckAt fuel expression := by
  cases fuel with
  | zero => trivial
  | succ fuel => rw [DoesNotGetStuckAt, if_pos value]; trivial

/-- Promoting a well-typed float program cannot introduce stuckness: the promotion step
only rewrites a real value. -/
theorem doesNotGetStuckAt_promote (typed : Typed [] expression (.float mode))
    (safe : DoesNotGetStuckAt fuel expression) :
    DoesNotGetStuckAt fuel (.promote expression) := by
  induction fuel generalizing expression with
  | zero => trivial
  | succ fuel ih =>
      rw [DoesNotGetStuckAt, if_neg (by simp [Expr.isValue]),
        MeasurableActionFamily.reduce_promote_eq]
      by_cases value : expression.isValue = true
      · rcases typed_real_value typed value with ⟨coordinate, rfl⟩
        simp only [value, ↓reduceIte]
        exact doesNotGetStuckAt_of_value fuel rfl
      · have valueFalse := Bool.eq_false_of_not_eq_true value
        rw [DoesNotGetStuckAt, valueFalse] at safe
        rw [valueFalse]
        simp only [Bool.false_eq_true, ↓reduceIte] at safe ⊢
        have actionTyped := reduce_typed_closed typed
        cases equation : reduce expression with
        | next next =>
            rw [equation] at safe actionTyped
            simp only [Action.wrap] at safe ⊢
            cases actionTyped with
            | next nextTyped => exact ih nextTyped safe
        | sample site fiber continuation =>
            rw [equation] at safe actionTyped
            simp only [Action.wrap, Function.comp_apply] at safe ⊢
            cases actionTyped with
            | sample continuationTyped =>
                refine ⟨safe.1, ?_⟩
                filter_upwards [safe.2] with coordinate coordinateSafe
                exact ih (continuationTyped coordinate) coordinateSafe
        | stuck =>
            rw [equation] at safe
            simp at safe
        | reject => simp [Action.wrap]

/-- Conversely, a program whose promotion never gets stuck never gets stuck itself. -/
theorem doesNotGetStuckAt_of_promote
    (safe : DoesNotGetStuckAt fuel (.promote expression)) :
    DoesNotGetStuckAt fuel expression := by
  induction fuel generalizing expression with
  | zero => trivial
  | succ fuel ih =>
      by_cases value : expression.isValue = true
      · exact doesNotGetStuckAt_of_value _ value
      · have valueFalse := Bool.eq_false_of_not_eq_true value
        rw [DoesNotGetStuckAt, if_neg (by simp [Expr.isValue]),
          MeasurableActionFamily.reduce_promote_eq, valueFalse] at safe
        rw [DoesNotGetStuckAt, valueFalse]
        simp only [Bool.false_eq_true, ↓reduceIte] at safe ⊢
        cases equation : reduce expression with
        | next next =>
            rw [equation] at safe
            simp only [Action.wrap] at safe ⊢
            exact ih safe
        | sample site fiber continuation =>
            rw [equation] at safe
            simp only [Action.wrap, Function.comp_apply] at safe ⊢
            exact ⟨safe.1, safe.2.mono fun coordinate coordinateSafe => ih coordinateSafe⟩
        | stuck =>
            rw [equation] at safe
            simp [Action.wrap] at safe
        | reject => trivial

theorem doesNotGetStuck_promote_iff (typed : Typed [] expression (.float mode)) :
    DoesNotGetStuck (.promote expression) ↔ DoesNotGetStuck expression :=
  ⟨fun safe fuel => doesNotGetStuckAt_of_promote (safe fuel),
    fun safe fuel => doesNotGetStuckAt_promote typed (safe fuel)⟩

end Typing

end Determinize.Proof.Paper
