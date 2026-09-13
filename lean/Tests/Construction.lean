import Determinize.Finite.Explore
import Determinize.Proof.FiniteModel.Soundness
import Tests.Parsing

set_option maxRecDepth 100000
set_option maxHeartbeats 0

namespace Determinize.Tests
open Determinize.Finite Spec.FiniteModel

private def collisions : Nat × Nat :=
  letI : Hashable State := ⟨fun _ => 0⟩
  let first := (Builder.start (.deliver (.number 1) [])).table
  let second := (first.insert (.deliver (.number 2) [])).table
  let repeated := second.insert (.deliver (.number 1) [])
  (repeated.table.states.size, repeated.index.val)

example : collisions = (2,0) := by decide +kernel

#print axioms Builder.Work.valid
#print axioms Proof.FiniteModel.replay_matches

end Determinize.Tests
