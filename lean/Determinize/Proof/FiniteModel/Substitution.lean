import Determinize.Spec.Syntax

namespace Determinize.Proof.FiniteModel
open Spec.Paper

namespace Binding

/-- All free variables fit within the given number of available binders. -/
def Scoped {α : Type} (depth : Nat) : Expr α → Prop
  | .bvar i => i < depth
  | .reject | .unit | .bool _ | .real _ | .nil => True
  | .lam body => Scoped (depth + 1) body
  | .fix body => Scoped (depth + 2) body
  | .fst body | .snd body | .inl body | .inr body | .neg body
  | .discrete _ body | .poisson _ body | .bernoulli _ body | .exponential _ body =>
    Scoped depth body
  | .app a b | .pair a b | .cons a b | .add a b | .mul a b | .div a b | .lt a b
  | .uniform _ a b | .gaussian _ a b | .beta _ a b | .gamma _ a b =>
    Scoped depth a ∧ Scoped depth b
  | .letE a b => Scoped depth a ∧ Scoped (depth + 1) b
  | .ite c a b => Scoped depth c ∧ Scoped depth a ∧ Scoped depth b
  | .matchSum c a b => Scoped depth c ∧ Scoped (depth + 1) a ∧ Scoped (depth + 1) b
  | .matchList c a b => Scoped depth c ∧ Scoped depth a ∧ Scoped (depth + 2) b

def scopedDecision {α : Type} :
    (depth : Nat) → (expression : Expr α) → Decidable (Scoped depth expression)
  | depth, .bvar i => inferInstanceAs (Decidable (i < depth))
  | _, .reject | _, .unit | _, .bool _ | _, .real _ | _, .nil => isTrue True.intro
  | depth, .lam body => scopedDecision (depth + 1) body
  | depth, .fix body => scopedDecision (depth + 2) body
  | depth, .fst body | depth, .snd body | depth, .inl body | depth, .inr body
  | depth, .neg body | depth, .discrete _ body | depth, .poisson _ body
  | depth, .bernoulli _ body | depth, .exponential _ body =>
    scopedDecision depth body
  | depth, .app left right | depth, .pair left right | depth, .cons left right
  | depth, .add left right | depth, .mul left right | depth, .div left right
  | depth, .lt left right | depth, .uniform _ left right | depth, .gaussian _ left right
  | depth, .beta _ left right | depth, .gamma _ left right =>
    letI := scopedDecision depth left
    letI := scopedDecision depth right
    inferInstanceAs (Decidable (_ ∧ _))
  | depth, .letE left right =>
    letI := scopedDecision depth left
    letI := scopedDecision (depth + 1) right
    inferInstanceAs (Decidable (_ ∧ _))
  | depth, .ite c x y =>
    letI := scopedDecision depth c
    letI := scopedDecision depth x
    letI := scopedDecision depth y
    inferInstanceAs (Decidable (_ ∧ _ ∧ _))
  | depth, .matchSum c x y =>
    letI := scopedDecision depth c
    letI := scopedDecision (depth + 1) x
    letI := scopedDecision (depth + 1) y
    inferInstanceAs (Decidable (_ ∧ _ ∧ _))
  | depth, .matchList c x y =>
    letI := scopedDecision depth c
    letI := scopedDecision depth x
    letI := scopedDecision (depth + 2) y
    inferInstanceAs (Decidable (_ ∧ _ ∧ _))

instance {α : Type} (depth : Nat) (expression : Expr α) : Decidable (Scoped depth expression) :=
  scopedDecision depth expression

theorem scoped_map {α β : Type} (expression : Expr α) (f : α → β) (depth : Nat) :
    Scoped depth (expression.map f id) ↔ Scoped depth expression := by
  induction expression generalizing depth <;> simp_all [Scoped, Expr.map]

/-- Instantiate free variables with closed expressions; missing entries use
rejection. For scoped source expressions no missing entry is consulted. -/
def close {α : Type} (environment : List (Expr α)) (depth : Nat) (expression : Expr α) : Expr α :=
  expression.mapVars (fun depth i ↦
    if i < depth then .bvar i else environment[i - depth]?.getD .reject) depth

theorem scoped_mono {α : Type} (expression : Expr α) {a b : Nat}
    (bounded : Scoped a expression) (le : a ≤ b) : Scoped b expression := by
  induction expression generalizing a b with
  | bvar i => exact Nat.lt_of_lt_of_le bounded le
  | reject  => trivial
  | unit  => trivial
  | bool v => trivial
  | real v => trivial
  | nil  => trivial
  | discrete k d ih => exact ih bounded le
  | lam body ih => exact ih bounded (Nat.add_le_add_right le 1)
  | fix body ih => exact ih bounded (Nat.add_le_add_right le 2)
  | fst body ih => exact ih bounded le
  | snd body ih => exact ih bounded le
  | inl body ih => exact ih bounded le
  | inr body ih => exact ih bounded le
  | neg body ih => exact ih bounded le
  | poisson k body ih => exact ih bounded le
  | bernoulli k body ih => exact ih bounded le
  | exponential k body ih => exact ih bounded le
  | app left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | pair left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | cons left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | add left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | mul left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | div left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | lt left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | uniform k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | gaussian k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | beta k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | gamma k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | letE value body ihv ihb => exact ⟨ihv bounded.1 le, ihb bounded.2 (Nat.add_le_add_right le 1)⟩
  | ite c x y ihc ihx ihy => exact ⟨ihc bounded.1 le, ihx bounded.2.1 le, ihy bounded.2.2 le⟩
  | matchSum c x y ihc ihx ihy =>
    exact ⟨ihc bounded.1 le, ihx bounded.2.1 (Nat.add_le_add_right le 1),
      ihy bounded.2.2 (Nat.add_le_add_right le 1)⟩
  | matchList c x y ihc ihx ihy =>
    exact ⟨ihc bounded.1 le, ihx bounded.2.1 le, ihy bounded.2.2 (Nat.add_le_add_right le 2)⟩

theorem mapVars_scoped {α : Type} (expression : Expr α) (depth : Nat)
    (replace : Nat → Nat → Expr α)
    (identity : ∀ d i, i < d → replace d i = .bvar i)
    (bounded : Scoped depth expression) :
    expression.mapVars replace depth = expression := by
  induction expression generalizing depth <;> simp_all [Scoped, Expr.mapVars]

theorem shift_closed {α : Type} (expression : Expr α) (closed : Scoped 0 expression)
    (amount cutoff : Nat) : expression.shift amount cutoff = expression := by
  apply mapVars_scoped expression cutoff
  · intro d i lt
    simp [show ¬d ≤ i by omega]
  · exact scoped_mono expression closed (Nat.zero_le _)

theorem subst_closed {α : Type} (expression : Expr α) (closed : Scoped 0 expression)
    (replacement : Expr α) (depth : Nat) :
    Expr.substAt depth replacement expression = expression := by
  apply mapVars_scoped expression depth
  · intro d i lt
    simp [show i ≠ d by omega, show ¬d < i by omega]
  · exact scoped_mono expression closed (Nat.zero_le _)

theorem environment_lookup_scoped {α : Type} (environment : List (Expr α))
    (closed : ∀ e ∈ environment, Scoped 0 e) (i depth : Nat) :
    Scoped depth (environment[i]?.getD .reject) := by
  cases h : environment[i]? with
  | none => simp [Scoped]
  | some e =>
    simp only [Option.getD_some]
    exact scoped_mono e (closed e (List.mem_of_getElem? h)) (Nat.zero_le _)

theorem close_scoped {α : Type} (expression : Expr α) (environment : List (Expr α))
    (closed : ∀ e ∈ environment, Scoped 0 e) (depth : Nat) :
    Scoped depth (close environment depth expression) := by
  induction expression generalizing depth <;>
    simp_all only [close, Expr.mapVars, Scoped, and_self]
  split
  · assumption
  · exact environment_lookup_scoped environment closed _ depth

theorem close_empty {α : Type} (expression : Expr α) (depth : Nat)
    (bounded : Scoped depth expression) : close [] depth expression = expression := by
  apply mapVars_scoped expression depth
  · intro d i lt
    simp [lt]
  · exact bounded

theorem close_cons_subst {α : Type} (expression : Expr α) (environment : List (Expr α))
    (closed : ∀ e ∈ environment, Scoped 0 e) (value : Expr α) (valueClosed : Scoped 0 value)
    (depth : Nat) :
    Expr.substAt depth value (close environment (depth + 1) expression) =
      close (value :: environment) depth expression := by
  induction expression generalizing depth <;>
    simp_all only [close, Expr.mapVars, Order.lt_add_one_iff, Nat.add_assoc, Nat.reduceAdd]
  rename_i i
  by_cases below : i < depth
  · simp [below, show i ≤ depth by omega, Expr.substAt, Expr.mapVars, show i ≠ depth by omega,
      show ¬depth < i by omega]
  · by_cases equal : i = depth
    · subst i
      simp [Expr.mapVars, shift_closed value valueClosed]
    · have above : depth < i := by omega
      have notBelow : ¬i ≤ depth := by omega
      have diff : i - depth = (i - (depth + 1)) + 1 := by omega
      simp only [notBelow, ↓reduceIte, below, diff, List.getElem?_cons_succ]
      exact subst_closed _ (environment_lookup_scoped environment closed _ 0) value depth

theorem close_two_subst {α : Type} (expression : Expr α) (environment : List (Expr α))
    (closed : ∀ e ∈ environment, Scoped 0 e)
    (argument function : Expr α) (argumentClosed : Scoped 0 argument)
    (functionClosed : Scoped 0 function) :
    (close environment 2 expression).substTwo argument function =
      close (argument :: function :: environment) 0 expression := by
  unfold Expr.substTwo
  rw [show 2 = 1 + 1 from rfl,
    close_cons_subst expression environment closed function functionClosed 1]
  exact close_cons_subst expression (function :: environment)
    (by
      intro e he
      rcases List.mem_cons.mp he with rfl | he
      · exact functionClosed
      · exact closed e he)
    argument argumentClosed 0

end Binding
end Determinize.Proof.FiniteModel
