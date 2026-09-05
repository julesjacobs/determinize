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
  | sample op ha hg hta htg iha ihg =>
      rw [Expr.shift, Expr.mapVars]
      apply Typed.sample op
      · simpa using ha
      · simpa using hg
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact iha original member (before := before) (suffix := suffix) hcontext
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact ihg original member (before := before) (suffix := suffix) hcontext
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
  | sample op ha hg hta htg iha ihg =>
      rw [Expr.substAt, Expr.mapVars]
      apply Typed.sample op
      · simpa using ha
      · simpa using hg
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact iha original member replacementTyped (before := before) (suffix := suffix) hcontext
      · intro child hchild
        rw [List.mem_map] at hchild
        rcases hchild with ⟨original, member, rfl⟩
        exact ihg original member replacementTyped (before := before) (suffix := suffix) hcontext
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

inductive ActionTyped (ty : Ty) : Action → Prop
  | next : Typed [] expression ty → ActionTyped ty (.next expression)
  | sample : (∀ value, Typed [] (continuation value) ty) →
      ActionTyped ty (.sample site fiber continuation)

theorem ActionTyped.wrap (actionTyped : ActionTyped childTy action)
    (wrapTyped : ∀ expression, Typed [] expression childTy →
      Typed [] (context expression) resultTy) :
    ActionTyped resultTy (action.wrap context) := by
  cases actionTyped with
  | next typed => exact .next (wrapTyped _ typed)
  | sample typed =>
      exact .sample fun value => wrapTyped _ (typed value)

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
    ∃ result, expression = .real mode result := by
  cases mode <;> cases typed <;> simp_all [Expr.isValue]

theorem firstNonValue_eq_none_iff :
    firstNonValue expressions = none ↔
      ∀ expression ∈ expressions, expression.isValue = true := by
  induction expressions with
  | nil => simp [firstNonValue]
  | cons head tail ih =>
      simp only [firstNonValue]
      by_cases headValue : head.isValue = true
      · simp only [headValue, ↓reduceIte, ih]
        aesop
      · have headFalse : head.isValue = false := Bool.eq_false_of_not_eq_true headValue
        simp [headFalse]

theorem firstNonValue_eq_some_append
    (found : firstNonValue expressions = some (front, current, suffix)) :
    expressions = front ++ current :: suffix := by
  induction expressions generalizing front current suffix with
  | nil => simp [firstNonValue] at found
  | cons head tail ih =>
      simp only [firstNonValue] at found
      by_cases headValue : head.isValue = true
      · simp only [headValue, ↓reduceIte] at found
        split at found
        · contradiction
        · rename_i tailFront tailCurrent tailSuffix equation
          simp only [Option.some.injEq, Prod.mk.injEq] at found
          rcases found with ⟨rfl, rfl, rfl⟩
          simp [ih equation]
      · have headFalse : head.isValue = false := Bool.eq_false_of_not_eq_true headValue
        simp only [headFalse, Bool.false_eq_true, ↓reduceIte, Option.some.injEq,
          Prod.mk.injEq] at found
        rcases found with ⟨rfl, rfl, rfl⟩
        rfl

theorem allRealValues_of_typed_values
    (typed : ∀ expression ∈ expressions, Typed [] expression (.float mode))
    (values : ∀ expression ∈ expressions, expression.isValue = true) :
    ∃ coordinates, allRealValues? expressions = some coordinates := by
  induction expressions with
  | nil => exact ⟨[], rfl⟩
  | cons head tail ih =>
      have headTyped := typed head (by simp)
      have headValue := values head (by simp)
      rcases typed_real_value headTyped headValue with ⟨coordinate, rfl⟩
      rcases ih (fun expression member => typed expression (by simp [member]))
          (fun expression member => values expression (by simp [member])) with
        ⟨coordinates, equation⟩
      exact ⟨coordinate :: coordinates, by simp [allRealValues?, equation]⟩

theorem typed_list_replace
    (typed : ∀ expression ∈ front ++ current :: suffix,
      Typed [] expression ty)
    (nextTyped : Typed [] next ty) :
    ∀ expression ∈ front ++ next :: suffix, Typed [] expression ty := by
  intro expression member
  simp only [List.mem_append, List.mem_cons] at member ⊢
  rcases member with member | rfl | member
  · exact typed expression (by simp [member])
  · exact nextTyped
  · exact typed expression (by simp [member])

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
      rename_i left mode right
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
  | sample op affineLength generalLength affineTyped generalTyped iha ihg =>
      cases hcontext
      rename_i affine mode general
      rw [MeasurableActionFamily.reduce_sample_eq]
      generalize found : firstNonValue affine = result
      cases result with
      | some triple =>
        rcases triple with ⟨front, current, suffix⟩
        have currentMember := firstNonValue_current_mem found
        have currentAction := iha current currentMember rfl
        apply currentAction.wrap
        intro next nextTyped
        apply Typed.sample op
        · rw [← affineLength, firstNonValue_eq_some_append found]
          simp
        · exact generalLength
        · exact typed_list_replace
            (by simpa [firstNonValue_eq_some_append found] using affineTyped) nextTyped
        · exact generalTyped
      | none =>
        generalize foundGeneral : firstNonValue general = generalResult
        cases generalResult with
        | some triple =>
          rcases triple with ⟨front, current, suffix⟩
          have currentMember := firstNonValue_current_mem foundGeneral
          have currentAction := ihg current currentMember rfl
          apply currentAction.wrap
          intro next nextTyped
          apply Typed.sample op
          · exact affineLength
          · rw [← generalLength, firstNonValue_eq_some_append foundGeneral]
            simp
          · exact affineTyped
          · exact typed_list_replace
              (by simpa [firstNonValue_eq_some_append foundGeneral] using generalTyped) nextTyped
        | none =>
          have affineValues := firstNonValue_eq_none_iff.mp found
          have generalValues := firstNonValue_eq_none_iff.mp foundGeneral
          rcases allRealValues_of_typed_values affineTyped affineValues with ⟨av, ha⟩
          rcases allRealValues_of_typed_values generalTyped generalValues with ⟨gv, hg⟩
          simp only [ha, hg]
          exact .sample fun value => .real
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

theorem primitiveDomainSafe_iff_doesNotGetStuck
    (typed : Typed [] expression ty) :
    PrimitiveDomainSafe expression ↔ Determinize.Statement.Paper.DoesNotGetStuck expression := by
  constructor
  · intro safe fuel
    exact primitiveDomainSafeAt_imp_doesNotGetStuckAt typed (safe fuel)
  · intro safe fuel
    exact doesNotGetStuckAt_imp_primitiveDomainSafeAt (safe fuel)

end Typing

end Determinize.Proof.Paper
