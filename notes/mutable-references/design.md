# General mutable references

This replaces the rejected numeric-only recommendation. User requirement: general `ref τ`, with `float[m]` as one possible contained type. Pro replacement: https://chatgpt.com/c/6aaf0ba2-c96c-83ea-8214-03bf2ece8659; substantive response in `pro-general-response.md`. No reference implementation or Lean proof has been added.

## Type and semantic design

Add `ref τ` for all existing value types, including products, sums, lists, nested references and functions. E/G remains on floats. A location has a fixed complete content type; reference subtyping requires equality of that type. Ordinary value/arrow subtyping still applies after a read and before a write. This prevents aliases from installing an E-dependent value or function into a cell used through a G-demanding type.

Store signature `Σ : List Ty`; typing `Typed Σ Γ e τ`. Each heap entry is a closed value typed at Σ[ℓ] under the full current Σ. This supports back edges and cycles through captured locations. Typing stops at location leaves; it never recursively checks the referenced cell. Store extension adds entries without changing existing types.

Use finite expression values in the specification and finite code/environment closures in the machine. A heap is a list of general values. Allocation appends; reads return the current value; assignment saves the evaluated target location before evaluating RHS and updates that location in the resulting heap. Closures capture locations, not heap snapshots. Cyclic heaps are finite collections of syntax with location leaves, not cyclic host-language values.

## Symbolic proof

Extend `AffineExpr n` with reference operations and locations, then carry a heap of general symbolic values alongside the expression. All heap entries, stored code captures and active computation share the same joint E-sample history. Realization maps their finite syntax and evaluates affine literals; it stops at locations and does not integrate functions.

Configuration determinization transforms code in both the heap and active expression, including latent sampling sites in stored functions. New E draws weaken the entire symbolic configuration, including stored lambda bodies. Use one symbolic typing and one store signature independent of the realized sample coordinates; concrete per-realization typings would lose E/G provenance.

Read/write/append commute with realization and determinization. Generalize existing source/target symbolic reduction commutation, preservation and `SafeConfigAt` to configurations. Syntactic typing/realization induction plus existing operational-depth induction is the proposed proof method. No new step-indexed semantic interpretation is needed for this specific structural transformation: stored code remains finite syntax, and each recursive call through the store consumes operational steps. This remains a design argument until those lemmas are proved.

Functions may have latent writes without adding effect annotations to arrow types. Body typing enforces cell invariants, and simulation tracks the full heap even for G/unit-returning calls. Optimization effect analysis remains separate.

## Theorems and implementation

Migrate compact replay/FiberSound to heap configurations and recover the same conditional-mean factorization. Generic measure arguments should preserve every current expectation, mass, Jensen, variance, conditional and variance-decomposition result under its existing source moment hypotheses. No acyclic-store, pure-callback, finite-state, whole-heap integrability or almost-sure termination premise is added. Preserve current rejection-as-divergence paper convention; a separate rejection observation remains additional work.

Measurability extends the existing countable real-free-skeleton/finite-real-coordinate representation to heap plus expression, including stored function syntax. No semantic function-space measure is introduced.

Stages:

1. General syntax, invariant subtyping, heap/store typing, substitution/preservation and freshness; functions/nested references/cycles included from the start.
2. General symbolic configurations, stored-code realization, whole-state weakening and source/target commutation.
3. Measurable configuration kernels, exact-depth semantics and safety.
4. Heap-aware trace factorization and all current public corollaries.
5. Heap-bearing CEK, `stateConfig` reification, progress, frontend/checker, full-state replay/serialization and Storm integration.

Full heap values and saved locations participate in replay/equality. Reference commits are semantic steps; recursion through stored functions cannot count as administrative stuttering. Completed rational graph format for Storm need not change. General references do not make an unbounded accumulator or unbounded allocation finite-state.

Already evaluated numeric additive frames remain eligible for reward extraction while retaining heap/state; folding deferred reads or calls requires separate effect reasoning.

## Decisive example

The replacement response gives a stored function that captures itself, a nested reference to an E numeric cell, a G counter, and an earlier E sample X. It runs twice, adds fresh Bernoulli(1/4) samples Y₁,Y₂ to the cell, then returns X+Y₁+Y₂, where X is Bernoulli(1/2).

Prove actual execution laws: source probabilities `(9,15,7,1)/32` on outputs `0,1,2,3`, mean 1, variance 5/8; target constant 1. This tests cyclic stored functions, aliases, nested references, latent writes, and weakening of a captured sample across later draws. Also distinguish a stored closure capturing one sample read twice from a stored closure sampling freshly on every call.

## Assessment

The revised design satisfies the requested generality. The soundness-critical requirement is invariance of the entire mutable content type, including types of stored function arguments/results. The main proof burden is full-configuration symbolic preservation and factorization, rather than new integration machinery.

Independently checked against source: existing finite syntax/closure representations, affine symbolic functions/data, G zero-coefficient rule, ordinary arrow variance, trace-factorization interface and public moment hypotheses support this architecture. The decisive example's exact distribution and variance agree by enumeration. These checks do not establish the new Lean theorems.
