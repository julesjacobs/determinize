import Determinize.Spec.Expectation
import Determinize.Spec.FiniteDistribution
import Determinize.Spec.FiniteDistributionMeasure
import Determinize.Spec.FiniteModel
import Determinize.Spec.Frontend
import Determinize.Spec.Inference
import Determinize.Spec.Primitives
import Determinize.Spec.RewardModel
import Determinize.Spec.Semantics
import Determinize.Spec.Syntax
import Determinize.Spec.Traces
import Determinize.Spec.Types

/-!
# Specification

The definitions that the theorems in `Determinize.Theorems` are stated over. No module here
imports `Determinize.Proof` or the front end.

- `Determinize.Spec.Types`, `Determinize.Spec.Syntax`: affinities, types and programs.
- `Determinize.Spec.Primitives`: the primitive distributions, at stochastic and at mean sites.
- `Determinize.Spec.Semantics`: reduction and the output law of a program.
- `Determinize.Spec.Expectation`: the return probability, and the expectation and variance
  conditioned on returning.
- `Determinize.Spec.Traces`: traces and the joint law of trace and output.
- `Determinize.Spec.Frontend`, `Determinize.Spec.Inference`: the programs of the front end, and
  what affinity inference is measured against.
- `Determinize.Spec.FiniteDistribution`, `Determinize.Spec.FiniteDistributionMeasure`: finite
  distributions with rational probabilities, and their measures.
- `Determinize.Spec.FiniteModel`, `Determinize.Spec.RewardModel`: finite models and reward
  models, which the checked results are about.
-/
