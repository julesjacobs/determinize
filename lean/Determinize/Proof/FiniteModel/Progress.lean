import Determinize.Proof.FiniteModel.Invariants

namespace Determinize.Proof.FiniteModel
open Statement.Paper Determinize.Finite Checking

def expressionWork : Core → Nat
  | .app a b | .pair a b | .cons a b | .add a b | .mul a b | .div a b | .lt a b =>
      expressionWork a + expressionWork b + 3
  | .fst a | .snd a | .inl a | .inr a | .neg a => expressionWork a + 2
  | .letE a _ | .ite a _ _ | .matchSum a _ _ | .matchList a _ _ => expressionWork a + 2
  | .uniform _ _ a b | .gaussian _ _ a b | .beta _ _ a b | .gamma _ _ a b =>
      expressionWork a + expressionWork b + 3
  | .poisson _ _ a | .bernoulli _ _ a | .exponential _ _ a => expressionWork a + 2
  | _ => 1

def frameWork : Frame → Nat
  | .left _ right _ => expressionWork right + 2
  | .draw _ pending _ _ => (pending.map fun e => expressionWork e + 1).sum + 1
  | _ => 1

def bookkeepingRank : State → Nat
  | .eval expression _ stack => expressionWork expression + (stack.map frameWork).sum
  | .deliver _ stack => (stack.map frameWork).sum
  | .rejected => 0

/-- States whose next transition only evaluates operands, delivers values, or rejects. -/
def Bookkeeping : State → Prop
  | .eval (.discrete ..) _ _ => False
  | .eval _ _ _ => True
  | .deliver _ (.left .. :: _) => True
  | .deliver _ (.unary .inl :: _) | .deliver _ (.unary .inr :: _) => True
  | .deliver _ (.right .pair _ :: _) | .deliver _ (.right .cons _ :: _) => True
  | .deliver _ (.draw _ (_ :: _) _ _ :: _) => True
  | _ => False

private theorem singleton_decreases (before state : State)
    (decrease : bookkeepingRank state < bookkeepingRank before)
    (tag evidence : Evidence) (successors : List (Rat × State))
    (action : (Except.ok (.next tag [(1,state)]) : Except Failure Step) = .ok (.next evidence successors))
    (probability : Rat) (after : State) (member : (probability, after) ∈ successors) :
    bookkeepingRank after < bookkeepingRank before := by
  obtain ⟨rfl, rfl⟩ := Step.next.inj (Except.ok.inj action)
  obtain ⟨rfl, rfl⟩ := Prod.mk.inj (List.mem_singleton.mp member)
  exact decrease

set_option maxHeartbeats 600000 in
theorem bookkeeping_decreases (before : State) (bookkeeping : Bookkeeping before)
    (evidence : Evidence) (successors : List (Rat × State))
    (action : step before = .ok (.next evidence successors))
    (probability : Rat) (after : State) (member : (probability, after) ∈ successors) :
    bookkeepingRank after < bookkeepingRank before := by
  cases before with
  | rejected => contradiction
  | eval expression environment stack =>
      cases expression <;> simp only [Bookkeeping] at bookkeeping
      all_goals simp only [step, pure, bind, Except.bind, Except.pure, throw] at action
      all_goals repeat' (split at action)
      all_goals first
        | contradiction
        | apply singleton_decreases _ _ ?_ _ _ _ action _ _ member
          simp [bookkeepingRank, expressionWork, frameWork] <;> omega
  | deliver value stack =>
      cases stack with
      | nil => contradiction
      | cons frame stack =>
          cases frame <;> simp only [Bookkeeping] at bookkeeping
          case left =>
            apply singleton_decreases _ _ ?_ _ _ _ action _ _ member
            simp [bookkeepingRank, frameWork]
            omega
          case unary operation =>
            cases operation <;> simp only at bookkeeping
            all_goals apply singleton_decreases _ _ ?_ _ _ _ action _ _ member
            all_goals simp [bookkeepingRank, frameWork]
          case right operation left =>
            cases operation <;> simp only at bookkeeping
            all_goals apply singleton_decreases _ _ ?_ _ _ _ action _ _ member
            all_goals simp [bookkeepingRank, frameWork]
          case draw site pending environment arguments =>
            cases pending with
            | nil => contradiction
            | cons next rest =>
                cases value <;> simp only [step, pure, bind, Except.bind, Except.pure, throw] at action
                all_goals first
                  | contradiction
                  | apply singleton_decreases _ _ ?_ _ _ _ action _ _ member
                    simp [bookkeepingRank, frameWork]
                    omega

def BookkeepingTransition (before after : State) : Prop :=
  Bookkeeping before ∧ ∃ evidence successors probability,
    step before = .ok (.next evidence successors) ∧ (probability, after) ∈ successors

theorem bookkeeping_transition_decreases (before after : State)
    (transition : BookkeepingTransition before after) :
    bookkeepingRank after < bookkeepingRank before := by
  obtain ⟨bookkeeping, evidence, successors, probability, action, member⟩ := transition
  exact bookkeeping_decreases before bookkeeping evidence successors action probability after member

theorem bookkeeping_path_bound (path : Nat → State) (length : Nat)
    (steps : ∀ i < length, BookkeepingTransition (path i) (path (i+1))) :
    bookkeepingRank (path length) + length ≤ bookkeepingRank (path 0) := by
  induction length with
  | zero => simp
  | succ n ih =>
      have previous := ih (fun i hi => steps i (by omega))
      have decrease := bookkeeping_transition_decreases _ _ (steps n (by omega))
      omega

theorem no_infinite_bookkeeping (path : Nat → State) :
    ¬∀ i, BookkeepingTransition (path i) (path (i+1)) := by
  intro steps
  have bound := bookkeeping_path_bound path (bookkeepingRank (path 0) + 1) (fun i _ => steps i)
  omega

end Determinize.Proof.FiniteModel
