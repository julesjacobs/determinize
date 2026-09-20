import Determinize.Finite.Machine

namespace Determinize.Finite.Reward

/-- A numeric check remains even when the extracted translation is zero. -/
def guard : Frame := .right .add (.number 0)

/-- Split off only the outermost, already evaluated additive context. -/
def splitStack : List Frame → List Frame × List Rat
  | [] => ([], [])
  | frame :: stack =>
      let (inner, offsets) := splitStack stack
      match inner, frame with
      | [], .right .add (.number c) => ([], c :: offsets)
      | _, _ => (frame :: inner, offsets)

def normalizeStack (stack : List Frame) : Rat × List Frame :=
  let (inner, offsets) := splitStack stack
  (offsets.sum, inner ++ if offsets.isEmpty then [] else [guard])

def normalize : State → Rat × State
  | .eval expression environment stack =>
      let (reward, stack) := normalizeStack stack
      (reward, .eval expression environment stack)
  | .deliver value stack =>
      let (reward, stack) := normalizeStack stack
      (reward, .deliver value stack)
  | .rejected => (0, .rejected)

structure Outcome where
  probability : Rat
  state : State
  reward : Rat
deriving Repr

inductive Step where
  | next (evidence : Evidence) (successors : List Outcome)
  | returned (value : Rat)
  | rejected
deriving Repr

/-- One CEK transition, followed by outer-context normalization. -/
def step (state : State) : Except Failure Step := do
  match ← Finite.step state with
  | .returned value => return .returned value
  | .rejected => return .rejected
  | .next evidence successors =>
      return .next evidence (successors.map fun (p, state) =>
        let (r, state) := normalize state
        ⟨p, state, r⟩)

end Determinize.Finite.Reward
