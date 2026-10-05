import Determinize.Finite.Explore
import Determinize.Finite.Export
import Determinize.Finite.Graph
import Determinize.Finite.Machine
import Determinize.Finite.Reward.Explore
import Determinize.Finite.Reward.Export
import Determinize.Finite.Reward.Graph
import Determinize.Finite.Reward.Normalize
import Determinize.Finite.Reward.Solve
import Determinize.Finite.Solve
import Determinize.Finite.Statistics
import Determinize.Finite.Supported

/-!
# Finite-state exploration

Verified graph construction and expected-reward solving for programs with finitely many reachable
states, with model export.

- `Determinize.Finite.Machine`: the executable machine whose states are explored.
- `Determinize.Finite.Explore`, `Determinize.Finite.Graph`: exploration into a finite graph,
  with evidence of its correspondence with the machine.
- `Determinize.Finite.Solve`, `Determinize.Finite.Statistics`: the exact expected reward,
  moments and termination probabilities of an explored model.
- `Determinize.Finite.Export`: an explored model and its certificates as Lean files for the
  kernel to check.
- `Determinize.Finite.Supported`: the sampling calls that exploration supports.
- `Determinize/Finite/Reward/`: the same for reward models.
-/
