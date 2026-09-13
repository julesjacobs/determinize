# Proposed paper structure

See [the drafting checklist](tex/FIGURE_PLAN.md). Use examples alongside the
definitions and rules they explain.

## Introduction

- Introduce the full pipeline through an example: program, inferred E/G annotations,
  determinization, and guarantees.

## Language and Determinization

- Syntax: `uniform[E]`, `uniform[G]`, etc.
- Semantics: the one that doesn't require a measure space over `Expr`.
- Determinization transform: easy because of the `[E]`, `[G]` annotations; independent of typing rules.

## Type System and Inference

- Typing rules and intuitive explanations.
- Infer valid E/G annotations from the typing rules.
- Explicit annotations as user constraints.

## Soundness

### Expectation and Variance

- Expectation preservation and variance non-increase.
- Defined expectations, domain safety, and output-mass preservation.

### When Expectations Are Undefined

- Refined semantics.
- Refined theorem.

## Proof

- Refined trace theorem implies the main one.
- Symbolic semantics and proof strategy.

## Implementation

- Describes the Lean implementation, verified checkers, and formalization.
- Storm, sampling, MCMC.

## Evaluation

- Benchmarks.

## Related work

## Appendix

- LaTeX proofs + references to their Lean equivalents.
