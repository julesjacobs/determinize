import Determinize.Spec.FiniteModel.Certificates
import Determinize.Spec.FiniteModel.Model
import Determinize.Spec.FiniteModel.Statistics
import Determinize.Spec.FiniteModel.Termination

/-!
# Finite models

Finite Markov chains with rational transition probabilities, whose states return a rational
reward or reject.

- `Determinize.Spec.FiniteModel.Model`: the chain, its output law, and `Model.Matches`, which
  relates it to a program.
- `Determinize.Spec.FiniteModel.Termination`: return, rejection and divergence probabilities.
- `Determinize.Spec.FiniteModel.Statistics`: rational statistics of an output law.
- `Determinize.Spec.FiniteModel.Certificates`: certificates for the expected reward, and their
  decidable validity.
-/
