# Proposed paper structure

## Introduction

## Language

- Syntax: `uniform[E]`, `uniform[G]`, etc.
- Semantics: the one that doesn't require a measure space over `Expr`.
- Determinization transform: easy because of the `[E]`, `[G]` annotations; independent of typing rules.

## Examples

- A few motivating examples.

## Type system + theorem statement

- Typing rules and intuitive explanations.
- Soundness theorem statement: the main one without traces.
- Variance non-increase theorem statement.

## Traces

- Refined semantics.
- Refined theorem.

## Proof

- Refined trace theorem implies the main one.
- Symbolic semantics and proof strategy.

## Implementation

- Describes OCaml + Lean setup.
- Storm, sampling, MCMC.

## Evaluation

- Benchmarks.

## Related work

## Appendix

- LaTeX proofs + references to their Lean equivalents.
