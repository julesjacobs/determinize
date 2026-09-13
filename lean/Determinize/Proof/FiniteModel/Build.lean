import Determinize.Proof.FiniteModel.Replay
import Std.Data.HashMap.Lemmas

namespace Determinize.Finite.Builder

local instance : BEq State := ⟨fun a b => decide (a = b)⟩
local instance : LawfulBEq State where
  eq_of_beq := by simp
  rfl := by simp

variable [Hashable State]

structure Table where
  states : Array State
  indices : Std.HashMap State Nat
  lookup : ∀ (state : State) (i : Nat), indices[state]? = some i ↔ states[i]? = some state

structure Extension (before : Table) (state : State) where
  table : Table
  index : Fin table.states.size
  located : table.states[index] = state
  size_le : before.states.size ≤ table.states.size
  preserves : ∀ (i : Nat) (s : State), before.states[i]? = some s → table.states[i]? = some s

theorem Table.injective (table : Table) :
    Function.Injective (fun i : Fin table.states.size => table.states[i]) := by
  intro i j eq
  have hi := (table.lookup table.states[i] i).mpr (by simp)
  have hj := (table.lookup table.states[i] j).mpr (by simp [eq])
  have : i.val = j.val := Option.some.inj (hi.symm.trans hj)
  exact Fin.ext this

def Table.insert (before : Table) (state : State) : Extension before state :=
  match found : before.indices[state]? with
  | some i =>
      let h := (Array.getElem?_eq_some_iff.mp ((before.lookup state i).mp found))
      ⟨before, ⟨i, h.1⟩, h.2, le_rfl, fun _ _ h => h⟩
  | none =>
      let after := before.states.push state
      have lookup : ∀ (s : State) (i : Nat), (before.indices.insert state before.states.size)[s]? = some i ↔
          after[i]? = some s := by
        intro s i
        rw [Std.HashMap.getElem?_insert]
        by_cases eq : state = s
        · subst s
          simp only [beq_self_eq_true, ↓reduceIte, Option.some.injEq]
          rw [Array.getElem?_push]
          by_cases atEnd : i = before.states.size
          · simp [atEnd]
          · simp only [atEnd, ↓reduceIte]
            constructor
            · intro h; exact (atEnd h.symm).elim
            · intro h
              have := (before.lookup state i).mpr h
              simp [found] at this
        · simp only [beq_iff_eq, eq, ↓reduceIte]
          rw [before.lookup, Array.getElem?_push]
          by_cases atEnd : i = before.states.size
          · simp [atEnd, eq]
          · simp [atEnd]
      ⟨⟨after, before.indices.insert state before.states.size, lookup⟩,
        ⟨before.states.size, by simp [after]⟩, by simp [after], by simp [after], by
          intro i s h
          have lt := (Array.getElem?_eq_some_iff.mp h).1
          simpa [after, Array.getElem?_push, Nat.ne_of_lt lt] using h⟩

structure Expansion (before : Table) (successors : List (Rat × State)) where
  table : Table
  size_le : before.states.size ≤ table.states.size
  preserves : ∀ (i : Nat) (s : State), before.states[i]? = some s → table.states[i]? = some s
  covered : ∀ outcome ∈ successors, 0 < outcome.1 →
    ∃ i : Fin table.states.size, table.states[i] = outcome.2

def Table.insertMany (before : Table) : (successors : List (Rat × State)) → Expansion before successors
  | [] => ⟨before, le_rfl, fun _ _ h => h, by simp⟩
  | (p,s) :: rest =>
    if positive : 0 < p then
      let inserted := before.insert s
      let tail := inserted.table.insertMany rest
      ⟨tail.table, inserted.size_le.trans tail.size_le,
        fun i s h => tail.preserves i s (inserted.preserves i s h), by
          intro outcome member hp
          rcases List.mem_cons.mp member with eq | member
          · subst outcome
            have found := tail.preserves inserted.index s (Array.getElem?_eq_some_iff.mpr ⟨inserted.index.isLt, inserted.located⟩)
            obtain ⟨lt, eq⟩ := Array.getElem?_eq_some_iff.mp found
            exact ⟨⟨inserted.index, lt⟩, eq⟩
          · exact tail.covered outcome member hp⟩
    else
      let tail := before.insertMany rest
      ⟨tail.table, tail.size_le, tail.preserves, by
        intro outcome member hp
        rcases List.mem_cons.mp member with eq | member
        · subst outcome; exact (positive hp).elim
        · exact tail.covered outcome member hp⟩

structure Action (state : State) where
  kind : Spec.FiniteModel.StateKind
  successors : List (Rat × State)
  correct : match step state with
    | .error _ => False
    | .ok (.returned reward) => kind = .returned reward ∧ successors = [(1,state)]
    | .ok .rejected => kind = .rejected ∧ successors = [(1,state)]
    | .ok (.next _ outcomes) => kind = .transient ∧ successors = outcomes
  nonnegative : ∀ outcome ∈ successors, 0 ≤ outcome.1
  normalized : (successors.map Prod.fst).sum = 1

def readAction (state : State) : Except Failure (Action state) :=
  match h : step state with
  | .error failure => .error failure
  | .ok (.returned reward) => .ok ⟨.returned reward, [(1,state)], by simp [h], by simp, by simp⟩
  | .ok .rejected => .ok ⟨.rejected, [(1,state)], by simp [h], by simp, by simp⟩
  | .ok (.next _ outcomes) =>
    if nonnegative : ∀ outcome ∈ outcomes, 0 ≤ outcome.1 then
      if normalized : (outcomes.map Prod.fst).sum = 1 then
        .ok ⟨.transient, outcomes, by simp [h], nonnegative, normalized⟩
      else .error (.invalid "transition probabilities do not sum to one")
    else .error (.invalid "negative transition probability")

structure Record where
  state : State
  action : Action state

structure Work where
  root : State
  table : Table
  initial : table.states[0]? = some root
  rows : Array Record
  size_le : rows.size ≤ table.states.size
  aligned : ∀ i : Fin rows.size, table.states[i.val]? = some rows[i].state
  closed : ∀ record ∈ rows, ∀ outcome ∈ record.action.successors, 0 < outcome.1 →
    ∃ i : Fin table.states.size, table.states[i] = outcome.2

def Work.expand (before : Work) (pending : before.rows.size < before.table.states.size)
    (action : Action before.table.states[before.rows.size]) : Work :=
  let grown := before.table.insertMany action.successors
  let record : Record := ⟨before.table.states[before.rows.size], action⟩
  ⟨before.root, grown.table, grown.preserves 0 before.root before.initial, before.rows.push record, by
    simp only [Array.size_push]
    exact (Nat.succ_le_of_lt pending).trans grown.size_le, by
    intro i
    by_cases old : i.val < before.rows.size
    · simpa [Array.getElem_push, old] using grown.preserves i.val _ (before.aligned ⟨i.val, old⟩)
    · have atEnd : i.val = before.rows.size := by have := i.isLt; simp only [Array.size_push] at this; omega
      simpa [Array.getElem_push, old, atEnd, record] using
        grown.preserves before.rows.size _ (by simp [pending])
    , by
    intro row member outcome inRow positive
    rcases Array.mem_push.mp member with old | eq
    · obtain ⟨i, hi⟩ := before.closed row old outcome inRow positive
      have found := grown.preserves i outcome.2 (Array.getElem?_eq_some_iff.mpr ⟨i.isLt, hi⟩)
      obtain ⟨lt, eq⟩ := Array.getElem?_eq_some_iff.mp found
      exact ⟨⟨i,lt⟩,eq⟩
    · subst row
      exact grown.covered outcome inRow positive⟩

def start (state : State) : Work where
  root := state
  table := ⟨#[state], ({} : Std.HashMap State Nat).insert state 0, by
    intro s i
    simp only [Std.HashMap.getElem?_insert, Std.HashMap.getElem?_empty, beq_iff_eq]
    by_cases eq : state = s
    · subst s; cases i <;> simp
    · cases i <;> simp [eq, eq_comm]⟩
  initial := by simp
  rows := #[]
  size_le := by simp
  aligned := by intro i; exact Fin.elim0 i
  closed := by simp

end Determinize.Finite.Builder
