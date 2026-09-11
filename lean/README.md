Review these three entry points and the definitions they import:

- `Determinize/Statement/Main.lean` defines the expectation-preservation propositions directly: `mainThm` (finite expectations), `extendedExpectationThm` (expectations in the extended reals, infinite values included), `jensenThm` (Jensen's inequality between the two output laws), `outputMassThm` (equal output mass), `varianceThm` (non-increasing second moment and variance) and `conditionalExpectationThm` (equal expectations conditioned on acceptance, the statement behind `observe`). `Statement` contains ordinary syntax, typing, primitive distributions and means, determinization, and semantics.
- `Determinize/Traces/Main.lean` defines trace erasure (`correspondenceThm`), trace soundness (`soundnessThm`) and the law of total variance along traces (`Traces.varianceThm`). `Traces/Semantics.lean` defines the operational traces, the joint law `traceAndOutputLaw` of a program's trace and output, and `outputGivenTrace`, the program replayed along a trace.
- `Determinize/Theorems.lean` proves all nine propositions without additional hypotheses and prints their axioms.

Run `lake build --wfail` from this directory; the build is warning-free and contains no `sorry`. Check that all nine axiom reports contain only `propext`, `Classical.choice`, and `Quot.sound` (`.claude/scripts/check.sh lean` performs both checks). With Lean's kernel and these standard axioms trusted, reviewers can omit the proof bodies in `Proof`. `Statement` contains the specification; any proof imports there supply proof-irrelevant evidence. `Traces` imports no proof modules.

Source expressions need not be in ANF. Each primitive distribution is its own constructor with the paper's operands (`uniform mode kind lower upper`, `gaussian mode kind mean variance`, and so on); a site carries its mode and whether it still samples or already returns the primitive's mean. Operands may contain nested sampling and are evaluated left to right; a mean site also evaluates every operand exactly once, including a Gaussian's variance. `Expr.sourceForm` excludes mean sites. `Statement/Primitives.lean` gives each primitive one fiber: its law at a stochastic site, the Dirac mass at its mean at a mean site, and the zero measure outside the parameter domain. Expressions have no type annotations; `Typed` assigns types separately. Expressions carry E/G labels only on sample sites; literals and arithmetic are mode-free and a literal types at either mode, as in the paper's `FloatLit`; subtyping is silent; variables are de Bruijn indices. `Typed` enforces the mode restrictions: E multiplication requires a G left operand, division a G denominator, and comparisons G operands. Arithmetic uses real numbers, with `x / 0 = 0`. The theorems quantify over closed float programs of either mode; the proof uses subsumption to assign an expectation-mode result type to the same program.

Output laws are defined directly by recursion over reduction depth. Deterministic actions continue evaluation; sampling actions integrate the continuation over the primitive measure on reals. Expressions of every type may occur during evaluation, but only terminal reals contribute output. Neither evaluator requires a measurable structure on expressions. `Proof` introduces one internally to establish measurability of the evaluators.

A trace is a list of `(primitive, value)` pairs recording only stochastic G draws. Deterministic steps and E draws add no entry. Trace soundness factors the actual joint trace/output measures over `traceLaw source`, the trace marginal of the source joint law: the source output is a Markov kernel indexed by the trace, and the target output is a measurable function of the trace. For almost every trace, the source fiber is an integrable probability measure and the target output equals its mean. The target therefore has the same trace law and the same termination probability as the source. No global integrability is required. Detailed traces with one entry per reduction step live in `Proof/Internal/StepTraces.lean`; `Proof/CompactTrace.lean` proves that replaying a determinized program's G draws reconstructs those detailed traces almost surely.

The output-mass theorem preserves acceptance/termination mass; dividing the preserved integral by that mass gives the conditional-expectation theorem. The variance theorems bound the target variance and decompose source variance along replay traces. The finite expectation theorem adds source integrability and proves target integrability and equal integrals. The extended-real theorem only assumes that one of `∫ v⁺` and `∫ v⁻` under the source output law is finite, and concludes the same for the target and equal extended-real expectations. Jensen's inequality bounds `∫ φ` under the target output law by `∫ φ` under the source output law for every nonnegative convex `φ : ℝ → ℝ`. Both corollaries are derived from trace soundness in `Proof/Corollaries.lean`. Output measures are unnormalized: divergence contributes no output mass, and expectations are not conditioned on termination.

The default build checks nested sampling, the `x + 1/y` example, a general-mode draw scaling an expectation-mode draw from the left, and a sampled value captured by a function. `Proof/InterfaceChecks.lean` checks the direct evaluator using only public imports and verifies that these imports provide no measurable structure on expressions.

## Deviations from the paper

The statements follow the paper's theorems, not its letter. Reviewers comparing against `tex/` should know:

- **Mode labels on sample sites.** The paper's transformation `⟦e : τ⟧` is type-directed; here `Expr.determinize` is a function on terms, so every sample site carries its mode and that label decides whether the site is switched to its mean (`determinizeKind`). Literals and arithmetic carry no labels.
- **Multiplication and division.** Lean, the paper, and the simulator use a G left operand for multiplication and a G denominator for division. Lean allows silent structural subtyping; the frontend also puts a literal scaling factor on the left. See `mul-div-typing.md` for the rule and its history.

- **Primitive domains.** Sampling outside a primitive's parameter domain (`uniform(a, b)` with `a > b`, a negative Gaussian variance, and so on) yields the zero measure and counts as stuck, and so does a mean site outside the same domain; the paper's mean table is unconditional. `uniform(a, a)` is the Dirac measure at `a`; the paper's table has no such row.
- **Validity hypothesis.** Every theorem assumes `DoesNotGetStuck program`: almost surely, at every reduction depth, no off-domain sample occurs, E draws included. The paper's "valid G-trace" is informal. The hypothesis is necessary: a source that loses mass with positive probability on an off-domain E parameter has a different expectation than its determinization.
- **Expectations.** `Traces.soundnessThm` needs no integrability hypothesis and proves that almost every fiber is integrable, which the paper assumes as "integrable σ" and states as the open lemma "Mean valuation correctness". `mainThm` covers finite expectations; `extendedExpectationThm` is the paper's extended-real global theorem; `jensenThm` is the paper's global Jensen corollary restricted to real-valued convex functions.

## Lean command-line implementation

From `lean/`:

```sh
lake build --wfail
.lake/build/bin/determinize --samples 1000 ../det/foldr.det
.lake/build/bin/determinize --certificate /tmp/Certificate.lean ../det/foldr.det
lake env lean /tmp/Certificate.lean
./test.sh
```

The frontend infers sampling modes and supplies a typing certificate checked in Lean.
Determinization uses the formal core transform. Numerical execution uses floating-point
arithmetic and a pseudorandom generator and is outside the semantic proof.

Discrete probabilities are nonnegative literals with exact sum one. Observation
lowers to `if condition then () else reject`, with zero output mass on rejection.
