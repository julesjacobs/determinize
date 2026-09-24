import Determinize.Finite.Reward.Explore
import Determinize.Finite.Reward.Solve
import Determinize.Proof.RewardModel.Normalization
import Determinize.Proof.RewardModel.Moments
import Determinize.Proof.RewardModel.Soundness
import Tests.Parsing

namespace Determinize.Tests
open Determinize.Finite

private def parallelRewards : Spec.RewardModel.Model where
  size := 2
  initial := 0
  kind := fun i => if i = 0 then .transient else .returned 0
  edges := fun i => if i = 0 then [⟨1, 1/2, 0⟩, ⟨1, 1/2, 2⟩] else [⟨1, 1, 0⟩]
  nonnegative := by decide +kernel
  normalized := by decide +kernel

private def rewardCase (text : String) (mass first second : Rat) : IO Unit := do
  let program ← IO.ofExcept (Frontend.compile text)
  match Reward.explore program.source .source {maxStates := 100} with
  | .complete candidate valid =>
      let solution ← IO.ofExcept (Reward.solve (candidate.toModel valid))
      assert (solution.statistics == ⟨mass, first, second⟩) s!"additive equations: {text}"
  | .incomplete _ _ _ _ => throw (IO.userError s!"additive exploration did not finish: {text}")
  | .failed _ failure => throw (IO.userError s!"additive exploration failed: {failure.message}")

def rewardModels : IO Unit := do
  let parallel ← IO.ofExcept (Reward.solve parallelRewards)
  assert (parallel.statistics == ⟨1, 1, 2⟩) "distinct rewards with the same target"
  rewardCase "let f = rec f u => if flip(0.5) then 0 else 1 + f u in f ()" 1 1 3
  rewardCase "let f = rec f u => if flip(0.5) then -1 else 1 + f u in f ()" 1 0 2
  rewardCase "let f = rec f u => if flip(0.5) then 0 else (-1) + f u in f ()" 1 (-1) 3
  rewardCase "let f = rec f u => if flip(0.5) then 0 else (1 + 2*bernoulli[G](0.5)) + f u in f ()" 1 2 13
  rewardCase "let f = rec f u => if flip(0.5) then 1 + f u else if flip(0.5) then 0 else (let _ = observe(false) in 0) in f ()" (1/2) (1/2) (3/2)
  rewardCase "let d = rec d u => d u in let f = rec f u => if flip(0.5) then 1 + f u else if flip(0.5) then 0 else d () in f ()" (1/2) (1/2) (3/2)
  rewardCase "let f = rec f u => 1 + f u in f ()" 0 0 0
  rewardCase "1 + (-2)" 1 (-1) 1
  rewardCase "if flip(0.5) then 1 + 0 else 3 + (let _ = observe(false) in 0)" (1/2) (1/2) (1/2)
  for text in [
      "let f = rec f u => if flip(0.5) then 0 else 1 + f u in 2 * f ()",
      "let f = rec f u => if flip(0.5) then 0 else f u + 1 in f ()",
      "let f = rec f a => if flip(0.5) then a else f (a+1) in f 0"] do
    let program ← IO.ofExcept (Frontend.compile text)
    match Reward.explore program.source .source {maxStates := 100} with
    | .incomplete .states _ _ _ => pure ()
    | _ => throw (IO.userError s!"unexpected additive extraction across a barrier: {text}")

example : Reward.normalize (.deliver (.bool true)
    [.right .add (.number 3), .right .add (.number (-3))]) =
      (0, .deliver (.bool true) [Reward.guard]) := by decide +kernel

example : ∃ failure, Reward.step (.deliver (.bool true) [Reward.guard]) = .error failure :=
  ⟨.invalid "binary operand types", rfl⟩

example : Reward.normalize (.deliver (.number 2)
    [.right .add (.number 3), .right .mul (.number 4)]) =
      (0, .deliver (.number 2) [.right .add (.number 3), .right .mul (.number 4)]) := by
  decide +kernel

end Determinize.Tests

#print axioms Determinize.Proof.RewardModel.finite_integrable
#print axioms Determinize.Proof.RewardModel.replay_matches
#print axioms Determinize.Proof.RewardModel.solution_result
#print axioms Determinize.Proof.RewardModel.solution_conditional_variance
#print axioms Determinize.Proof.RewardModel.solution_termination
