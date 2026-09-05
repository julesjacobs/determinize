Review these three entry points and the definitions they import:

- `Determinize/Statement/Main.lean` defines the expectation-preservation proposition directly. `Statement` contains ordinary syntax, typing, primitive distributions and means, determinization, and semantics.
- `Determinize/Traces/Main.lean` defines trace erasure and trace soundness. `Traces/Semantics.lean` defines the operational traces.
- `Determinize/Theorems.lean` proves all three propositions without additional hypotheses and prints their axioms.

Run `lake build` from this directory. Check that all three axiom reports contain only `propext`, `Classical.choice`, and `Quot.sound`. With Lean's kernel and these standard axioms trusted, reviewers can omit the proof bodies in `Proof`. `Statement` imports no tracing or proof modules; `Traces` imports no proof modules.

Source expressions need not be in ANF. Primitive arguments may contain nested sampling and are evaluated left to right. Atomic mean primitives also evaluate every argument exactly once, including a Gaussian's variance argument. `Expr.sourceForm` excludes internal mean primitives. Expressions have no type annotations; `Typed` assigns types separately. The syntax retains explicit E/G labels, promotion, and de Bruijn indices. `Typed` enforces the mode restrictions: E multiplication requires a G right operand, division a G denominator, and comparisons G operands. Arithmetic uses real numbers, with `x / 0 = 0`.

Output laws are defined directly by recursion over reduction depth. Deterministic actions continue evaluation; sampling actions integrate the continuation over the primitive measure on reals. Expressions of every type may occur during evaluation, but only terminal E-mode reals contribute output. Neither evaluator requires a measurable structure on expressions. `Proof` introduces one internally to establish measurability of the evaluators.

A trace is a list of `(primitive, value)` pairs recording only stochastic G draws. Deterministic steps and E draws add no entry. Trace soundness factors the actual joint trace/output measures over a common trace measure. For almost every trace, the source fiber is an integrable probability measure and the target output equals its mean. No global integrability is required. Detailed traces with one entry per reduction step live in `Proof/Internal/StepTraces.lean`; `Proof/CompactTrace.lean` proves that replaying a determinized program's G draws reconstructs those detailed traces almost surely.

The expectation theorem adds source integrability and proves target integrability and equal integrals. Output measures are unnormalized: divergence contributes no output mass, and expectations are not conditioned on termination.

The default build checks nested sampling, the `x + 1/y` example, and a sampled value captured by a function. `Proof/InterfaceChecks.lean` checks the direct evaluator using only public imports and verifies that these imports provide no measurable structure on expressions.
