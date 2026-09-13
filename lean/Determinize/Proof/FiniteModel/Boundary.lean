import Determinize.Proof.FiniteModel.Result

namespace Determinize.Proof.FiniteModel
open Spec.FiniteModel MeasureTheory

/-- A closed set of transient states contributes no terminal output. -/
def ClosedDivergence (model : Model) (dead : Fin model.size → Bool) : Prop :=
  ∀ state, dead state = true → model.kind state = .transient ∧
    ∀ next, 0 < model.transition state next → dead next = true

instance (model : Model) (dead : Fin model.size → Bool) : Decidable (ClosedDivergence model dead) :=
  inferInstanceAs (Decidable (∀ _, _))

/-- Cutting off a closed divergent region preserves the output law. -/
abbrev cut (model : Model) (dead : Fin model.size → Bool) : Model :=
  {model with kind := fun state => if dead state then .rejected else model.kind state}

theorem dead_outputWithin (model : Model) (dead : Fin model.size → Bool)
    (closed : ClosedDivergence model dead) (n : Nat) (state : Fin model.size)
    (isDead : dead state = true) : model.outputWithin n state = 0 := by
  have transient := (closed state isDead).1
  induction n generalizing state with
  | zero => simp [Model.outputWithin, transient]
  | succ n ih =>
      simp only [Model.outputWithin, transient]
      apply Finset.sum_eq_zero
      intro next _
      by_cases positive : 0 < model.transition state next
      · rw [ih next ((closed state isDead).2 next positive) ((closed next ((closed state isDead).2 next positive)).1), smul_zero]
      · have zero : model.transition state next = 0 :=
          le_antisymm (le_of_not_gt positive) (model.nonnegative state next)
        simp [zero]

theorem cut_outputWithin (model : Model) (dead : Fin model.size → Bool)
    (closed : ClosedDivergence model dead) (n : Nat) (state : Fin model.size) :
    (cut model dead).outputWithin n state = model.outputWithin n state := by
  induction n generalizing state with
  | zero =>
      by_cases isDead : dead state = true
      · simp [Model.outputWithin, isDead, (closed state isDead).1]
      · simp [Model.outputWithin, isDead]
  | succ n ih =>
      by_cases isDead : dead state = true
      · rw [dead_outputWithin model dead closed _ state isDead]
        simp [Model.outputWithin, isDead]
      · simp only [Model.outputWithin, cut, isDead, Bool.false_eq_true, ↓reduceIte]
        cases model.kind state with
        | returned r => rfl
        | rejected => rfl
        | transient =>
          change (∑ next, ENNReal.ofReal (model.transition state next : ℝ) •
            (cut model dead).outputWithin n next) = _
          simp only [ih]

theorem cut_outputMeasure (model : Model) (dead : Fin model.size → Bool)
    (closed : ClosedDivergence model dead) : (cut model dead).outputMeasure = model.outputMeasure := by
  change (⨆ n, (cut model dead).outputWithin n model.initial) = _
  simp only [cut_outputWithin model dead closed, Model.outputMeasure]

structure Boundary (model : Model) where
  dead : Fin model.size → Bool
  closed : ClosedDivergence model dead
  rank : Fin model.size → Nat

def reachStep (model : Model) (reachable : Vector Bool model.size) : Vector Bool model.size :=
  Vector.ofFn fun state => decide (model.kind state ≠ .transient ∨
    ∃ next, 0 < model.transition state next ∧ reachable[next] = true)

theorem stable_closed (model : Model) (reachable : Vector Bool model.size)
    (stable : ∀ state : Fin model.size, (reachStep model reachable)[state] = reachable[state]) :
    ClosedDivergence model (fun state => !reachable[state]) := by
  intro state isDead
  have unreachable : reachable[state] = false := by simpa using isDead
  have h := stable state
  simp [reachStep, unreachable] at h
  refine ⟨h.1, ?_⟩
  intro next positive
  have hn := h.2 next positive
  simpa using hn

private def findBoundary (model : Model) : Nat → Nat → Vector Bool model.size →
    Vector Nat model.size → Option (Boundary model)
  | 0, _, _, _ => none
  | fuel+1, level, reachable, rank =>
    let next := reachStep model reachable
    if stable : ∀ state : Fin model.size, next[state] = reachable[state] then
      some ⟨fun state => !reachable[state], stable_closed model reachable stable, fun state => rank[state]⟩
    else
      let ranks := Vector.ofFn fun state => if reachable[state] then rank[state] else if next[state] then level else 0
      findBoundary model fuel (level+1) next ranks

def analyze (model : Model) : Except String (Boundary model) :=
  match findBoundary model (model.size+2) 0 (Vector.replicate model.size false)
      (Vector.replicate model.size 0) with
  | some boundary => .ok boundary
  | none => .error "terminal reachability did not stabilize"

end Determinize.Proof.FiniteModel
