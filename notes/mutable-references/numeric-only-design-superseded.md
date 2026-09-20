> Historical rejected recommendation; superseded by general `ref τ` in `design.md`.

# Recommended mutable-reference extension

Pro consultation: https://chatgpt.com/c/6aaf0ba2-c96c-83ea-8214-03bf2ece8659

Status: design only; no reference code or proof has been implemented. The complete substantive response is in `pro-response.md`.

## Recommendation

Start with numeric cells whose E/G content mode is fixed for their lifetime. Allow aliases, closures capturing references, higher-order functions outside the heap, and recursion. Use append-only heaps and natural-number locations; defer cells holding closures/references, deallocation, flow-sensitive modes and garbage collection.

Reference types must be invariant. The existing G-to-E numeric coercion must not make `ref[G]` a subtype of `ref[E]`, because the latter would permit writes that violate a G alias's guarantees. All aliases share a location and a mode. Already loaded numbers retain their values after a write; closures capturing a reference read the current heap.

The proof should extend the existing symbolic affine construction to the **whole configuration**: heap, residual expression and saved computation share one joint E-sample history. A cell contains an affine form in those coordinates. Reading copies the form; writing replaces the entry; aliases use the same entry; adding a new E sample weakens every existing form. G cells have zero E coefficients. This accounts for correlation without requiring independent cell contents.

The compiler still replaces eligible E sampling occurrences by mean nodes. It never separately averages dereferences. Fixed cell modes conservatively force any cell used in nonlinear arithmetic or control to G. A flow-sensitive effect analysis is unnecessary for this base structural transformation, but necessary for later heap-sensitive simplifications.

## Theorem/proof changes

Extend runtime semantics from expressions to heap/expression configurations and typing to store-typing/variable-context judgments. Prove store extension/preservation and source/target realization commutation, then migrate compact replay to recover the existing conditional-mean theorem. Keep its assumptions unchanged: do not add global integrability merely because configurations have heaps.

The generic measure arguments above trace factorization should then continue to provide finite/extended expectation, equal return mass, Jensen, variance and conditional results with their existing source hypotheses. No global heap-integrability hypothesis is needed. Measurability follows the existing discrete-skeleton/real-coordinate representation, now including heap length and cell coordinates.

For the executable bridge, replace `stateExpr` with heap-bearing `stateConfig`, retain location handles in closed closures, and include heaps in equality/hashing/replay. Each probabilistic branch must carry its own heap value. Reference operations are semantic steps; bookkeeping ranks remain syntactic. Storm itself consumes the same rational graph once extraction succeeds.

Pro also proposes explicit first-hit rejection and genuine-divergence observations. These are useful additional public claims, but can be a separate milestone: the current paper convention treats rejection as divergence. Do not make this extra semantic refactoring a prerequisite for preserving existing output-law and trace contracts.

## First proof experiment

```
let r = ref 0 in
let s = r in
let read = fun () => !s in
r := bernoulli[E](1/2);
let a = read () in
r := !r + bernoulli[E](1/4);
a + read ()
```

Prove actual execution laws: source `(3/8)δ₀ + (1/8)δ₁ + (3/8)δ₂ + (1/8)δ₃`, target `δ_(5/4)`. This exercises aliases, a captured reference, an immutable loaded value, reading before a write, and weakening across the second draw. Replacing final addition with multiplication must reject explicit E annotations or infer G.

## Scope and interaction with addition rewards

A mutable unbounded accumulator remains infinite-state. Unbounded fresh allocation also remains infinite-state. Heap renaming does not solve either problem by itself; dead-heap collection and abstraction require separate observational proofs.

Already evaluated numeric addition frames can still be extracted with their heap retained. Pure-looking RHS syntax is insufficient for folding in a stateful language: even a deterministic read can change meaning when moved before a write. Effect analysis must precede that extension.

## Independent checks performed

- Confirmed `flip[E]` is rejected in `Frontend/Elaborate.lean`; the numeric example must use `bernoulli`.
- Confirmed `Typed.mul` requires G on the left and comparisons require G operands.
- Confirmed `AffineExpr.WellTyped.realG` requires zero coefficients and existing affine moment lemmas are in `Proof/Symbolic/Moments.lean`.
- Confirmed `Proof/Soundness.lean` derives expectation from trace factorization, supporting the proposed reuse boundary.
- Confirmed `check.sh` explicitly scans only current Main/Traces theorem files; new Spec/Refs statement files need registration.
- Checked the decisive example's four probabilities and mean 5/4 by elementary enumeration.

These checks support the architecture, but do not constitute a Lean verification of the extension. The main unproved work is heap-aware symbolic preservation and trace factorization under recursion and effects.
