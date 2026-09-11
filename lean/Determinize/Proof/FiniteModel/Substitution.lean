import Determinize.Statement.Syntax

namespace Determinize.Proof.FiniteModel
open Statement.Paper

namespace Binding

/-- All free variables fit within the given number of available binders. -/
def Scoped {α : Type} (depth : Nat) : Expr α → Prop
  | .bvar i => i < depth
  | .reject | .unit | .bool _ | .real _ | .nil | .discrete .. => True
  | .lam body => Scoped (depth+1) body
  | .fix body => Scoped (depth+2) body
  | .fst body | .snd body | .inl body | .inr body | .neg body
  | .poisson _ _ body | .bernoulli _ _ body | .exponential _ _ body => Scoped depth body
  | .app a b | .pair a b | .cons a b | .add a b | .mul a b | .div a b | .lt a b
  | .uniform _ _ a b | .gaussian _ _ a b | .beta _ _ a b | .gamma _ _ a b =>
      Scoped depth a ∧ Scoped depth b
  | .letE a b => Scoped depth a ∧ Scoped (depth+1) b
  | .ite c a b => Scoped depth c ∧ Scoped depth a ∧ Scoped depth b
  | .matchSum c a b => Scoped depth c ∧ Scoped (depth+1) a ∧ Scoped (depth+1) b
  | .matchList c a b => Scoped depth c ∧ Scoped depth a ∧ Scoped (depth+2) b

def scopedDecision {α : Type} (depth : Nat) (expression : Expr α) : Decidable (Scoped depth expression) := by
  cases expression with
  | bvar i => exact inferInstanceAs (Decidable (i < depth))
  | reject  => exact isTrue True.intro
  | unit  => exact isTrue True.intro
  | bool v => exact isTrue True.intro
  | real v => exact isTrue True.intro
  | nil  => exact isTrue True.intro
  | discrete m k d => exact isTrue True.intro
  | lam body => exact scopedDecision (depth+1) body
  | fix body => exact scopedDecision (depth+2) body
  | fst body => exact scopedDecision depth body
  | snd body => exact scopedDecision depth body
  | inl body => exact scopedDecision depth body
  | inr body => exact scopedDecision depth body
  | neg body => exact scopedDecision depth body
  | poisson m k body => exact scopedDecision depth body
  | bernoulli m k body => exact scopedDecision depth body
  | exponential m k body => exact scopedDecision depth body
  | app left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | pair left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | cons left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | add left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | mul left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | div left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | lt left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | uniform m k left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | gaussian m k left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | beta m k left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | gamma m k left right =>
      letI := scopedDecision depth left
      letI := scopedDecision depth right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | letE left right =>
      letI := scopedDecision depth left
      letI := scopedDecision (depth+1) right
      exact inferInstanceAs (Decidable (_ ∧ _))
  | ite c x y =>
      letI := scopedDecision depth c
      letI := scopedDecision depth x
      letI := scopedDecision depth y
      exact inferInstanceAs (Decidable (_ ∧ _ ∧ _))
  | matchSum c x y =>
      letI := scopedDecision depth c
      letI := scopedDecision (depth+1) x
      letI := scopedDecision (depth+1) y
      exact inferInstanceAs (Decidable (_ ∧ _ ∧ _))
  | matchList c x y =>
      letI := scopedDecision depth c
      letI := scopedDecision depth x
      letI := scopedDecision (depth+2) y
      exact inferInstanceAs (Decidable (_ ∧ _ ∧ _))
termination_by sizeOf expression

instance {α : Type} (depth : Nat) (expression : Expr α) : Decidable (Scoped depth expression) :=
  scopedDecision depth expression

theorem scoped_mapLiteral {α β : Type} (expression : Expr α) (f : α → β) (depth : Nat) :
    Scoped depth (expression.mapLiteral f) ↔ Scoped depth expression := by
  induction expression generalizing depth <;> simp_all [Scoped, Expr.mapLiteral]

/-- Instantiate free variables with closed expressions; missing entries use
rejection. For scoped source expressions no missing entry is consulted. -/
def close {α : Type} (environment : List (Expr α)) (depth : Nat) (expression : Expr α) : Expr α :=
  expression.mapVars (fun depth i =>
    if i < depth then .bvar i else environment[i-depth]?.getD .reject) depth

theorem scoped_mono {α : Type} (expression : Expr α) {a b : Nat}
    (bounded : Scoped a expression) (le : a ≤ b) : Scoped b expression := by
  induction expression generalizing a b with
  | bvar i => exact Nat.lt_of_lt_of_le bounded le
  | reject  => trivial
  | unit  => trivial
  | bool v => trivial
  | real v => trivial
  | nil  => trivial
  | discrete m k d => trivial
  | lam body ih => exact ih bounded (Nat.add_le_add_right le 1)
  | fix body ih => exact ih bounded (Nat.add_le_add_right le 2)
  | fst body ih => exact ih bounded le
  | snd body ih => exact ih bounded le
  | inl body ih => exact ih bounded le
  | inr body ih => exact ih bounded le
  | neg body ih => exact ih bounded le
  | poisson m k body ih => exact ih bounded le
  | bernoulli m k body ih => exact ih bounded le
  | exponential m k body ih => exact ih bounded le
  | app left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | pair left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | cons left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | add left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | mul left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | div left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | lt left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | uniform m k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | gaussian m k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | beta m k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | gamma m k left right ihl ihr => exact ⟨ihl bounded.1 le, ihr bounded.2 le⟩
  | letE value body ihv ihb => exact ⟨ihv bounded.1 le, ihb bounded.2 (Nat.add_le_add_right le 1)⟩
  | ite c x y ihc ihx ihy => exact ⟨ihc bounded.1 le, ihx bounded.2.1 le, ihy bounded.2.2 le⟩
  | matchSum c x y ihc ihx ihy => exact ⟨ihc bounded.1 le, ihx bounded.2.1 (Nat.add_le_add_right le 1), ihy bounded.2.2 (Nat.add_le_add_right le 1)⟩
  | matchList c x y ihc ihx ihy => exact ⟨ihc bounded.1 le, ihx bounded.2.1 le, ihy bounded.2.2 (Nat.add_le_add_right le 2)⟩

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
    simp_all [close, Expr.mapVars, Scoped]
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
    Expr.substAt depth value (close environment (depth+1) expression) =
      close (value :: environment) depth expression := by
  induction expression generalizing depth <;>
    simp_all [close, Expr.mapVars, Nat.add_assoc]
  rename_i i
  by_cases below : i < depth
  · simp [below, show i ≤ depth by omega, Expr.substAt, Expr.mapVars, show i ≠ depth by omega,
      show ¬depth < i by omega]
  · by_cases equal : i = depth
    · subst i
      simp [Expr.mapVars, shift_closed value valueClosed]
    · have above : depth < i := by omega
      have notBelow : ¬i ≤ depth := by omega
      have diff : i-depth = (i-(depth+1))+1 := by omega
      simp only [notBelow, ↓reduceIte, below, diff, List.getElem?_cons_succ]
      exact subst_closed _ (environment_lookup_scoped environment closed _ 0) value depth

theorem close_two_subst {α : Type} (expression : Expr α) (environment : List (Expr α))
    (closed : ∀ e ∈ environment, Scoped 0 e)
    (argument function : Expr α) (argumentClosed : Scoped 0 argument) (functionClosed : Scoped 0 function) :
    (close environment 2 expression).substTwo argument function =
      close (argument :: function :: environment) 0 expression := by
  unfold Expr.substTwo
  rw [show 2 = 1+1 from rfl, close_cons_subst expression environment closed function functionClosed 1]
  exact close_cons_subst expression (function :: environment)
    (by intro e he; rcases List.mem_cons.mp he with rfl | he; exact functionClosed; exact closed e he)
    argument argumentClosed 0

end Binding
end Determinize.Proof.FiniteModel
