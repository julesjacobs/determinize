import Determinize.Proof.FiniteModel.Progress
import Determinize.Proof.FiniteModel.Sampling
import Determinize.Proof.FiniteModel.Initial
import Determinize.Checking.Result
import Determinize.Checking.FiniteModel
import Determinize.Proof.FiniteModel.Model
import Determinize.Proof.Primitives.DiscreteLaws
import Determinize.Proof.Semantics.Rejection
import Determinize.Theorems
import Determinize.Proof.Examples
import Determinize.Proof.InterfaceChecks
import Determinize.Proof.RewardModel.Moments
import Determinize.Proof.RewardModel.Soundness

/-!
# Determinize

A probabilistic language whose sample sites are labelled E or G. Determinizing a program turns
its E samples into the means of their distributions and keeps its G samples.

Start with `Determinize.Theorems`. It states every theorem in full over the definitions in
`Spec/`, the paper's theorems first and under the paper's names, and proves each by one term from
`Proof/`. A reviewer reads the statements and the definitions they use, not the proofs.
[`lean/README.md`](https://github.com/julesjacobs/determinize/blob/main/lean/README.md) explains
what to review and how the development differs from the paper.

- `Spec/`: what the theorems are about, from syntax, typing, the primitive distributions and
  determinization to the output and trace laws (`Determinize.Spec.Traces.Semantics`) and
  expectations and variances (`Determinize.Spec.Expectation`).
- `Proof/`: the proofs. The build fails if a theorem uses an axiom other than `propext`,
  `Classical.choice` and `Quot.sound`, or if its statement relies on anything from `Proof/` other
  than proofs.
- `Frontend/`: affinity inference (`Determinize.Frontend.Infer`), the subject of
  `Determinize.Theorems.inference_correctness`.
- `Finite/` and `Checking/`: exact exploration of finite-state programs, and the checks that turn an
  explored model and candidate results into a theorem about the program, checked by Lean's
  kernel (`Determinize.Checking.Result`).

The default build also checks the coverage examples in `Determinize.Proof.Examples`. The API
documentation covers the modules imported here; the command-line tool, its numerical evaluator
and the tests are not among them.
-/
