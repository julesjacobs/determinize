import Determinize.Checking.FiniteDistribution
import Determinize.Checking.FiniteModel
import Determinize.Checking.Result
import Determinize.Checking.Statistics

/-!
# Checkers

Executable checkers for finite distributions and for finite-model result, moment and
termination certificates, with their soundness theorems. A checked model and certificate give a
theorem about the program that Lean's kernel checks.

- `Determinize.Checking.FiniteDistribution`: literal probabilities of finite distributions.
- `Determinize.Checking.FiniteModel`: a candidate graph against the program (`checkModel`).
- `Determinize.Checking.Result`: expected-reward certificates.
- `Determinize.Checking.Statistics`: moment and termination certificates.
-/
