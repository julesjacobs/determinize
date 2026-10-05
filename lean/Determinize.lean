import Determinize.Checking
import Determinize.Finite
import Determinize.Frontend
import Determinize.Proof
import Determinize.Spec
import Determinize.Theorems

/-!
# Determinize

A probabilistic language whose sample sites are labelled E or G. Determinizing a program turns
its E samples into the means of their distributions and keeps its G samples.

A reviewer reads the statements of the theorems and the definitions they use, not the proofs.
[`lean/README.md`](https://github.com/julesjacobs/determinize/blob/main/lean/README.md) explains
what to review and how the development differs from the paper.

- `Determinize.Theorems`: every theorem, stated in full over the definitions in
  `Determinize.Spec` and proved by one term from `Determinize.Proof`, the paper's theorems first
  and under the paper's names. Start here.
- `Determinize.Spec`: what the theorems are about, from syntax, typing, the primitive
  distributions and determinization to the output and trace laws
  (`Determinize.Spec.Traces.Semantics`) and expectations and variances
  (`Determinize.Spec.Expectation`).
- `Determinize.Proof`: the proofs. The build fails if a theorem uses an axiom other than
  `propext`, `Classical.choice` and `Quot.sound`, or if its statement relies on anything from
  these modules other than proofs.
- `Determinize.Frontend`: parsing, name resolution and affinity inference
  (`Determinize.Frontend.Infer`, the subject of `Determinize.Theorems.inference_correctness`).
- `Determinize.Finite` and `Determinize.Checking`: exact exploration of finite-state programs,
  and the checks that turn an explored model and candidate results into a theorem about the
  program, checked by Lean's kernel (`Determinize.Checking.Result`).

The default build also checks the coverage examples in `Determinize.Proof.Examples`. The API
documentation covers every module except the command-line entry point `Main`, the numerical
evaluator in `Determinize/Runtime/`, and the tests.
-/
