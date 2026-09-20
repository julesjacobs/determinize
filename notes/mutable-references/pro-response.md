> Superseded: the user rejected the numeric-only restriction. See `pro-general-response.md` and `design.md` for the replacement general-reference design.

# Pro mutable-reference design

Source: https://chatgpt.com/c/6aaf0ba2-c96c-83ea-8214-03bf2ece8659

GPT-6 Pro completed its response after 16m33s. This is a substantive transcription/synthesis of that response, not a verbatim export. Proposed declarations below are not implemented or checked.

## Minimal example and acceptance boundary

Numeric sampling is `bernoulli[E](p)`: the existing `flip` is Boolean and forces G. For

```
let r = ref 0 in
let x = bernoulli[E](p) in
r := x;
!r + !r
```

source output law is `(1-p) δ₀ + p δ₂`; target law is `δ_(2p)`. Only the sampling occurrence transforms. Reads reuse the stored value; they do not sample independently. An alias `s = r` denotes the same cell and has identical behavior.

The initial exported example propositions should establish actual execution laws, domain safety, integrability, and expectation equality for all `0 ≤ p ≤ 1`; do not leave safety/integrability as unproved hypotheses in those examples. Existing `bernoulli_integral`, `bernoulli_mean`, `bernoulli_probability` in `Proof/Primitives/DiscreteLaws.lean` supply the primitive calculations.

After `r := X` for a fair Bernoulli:

- `!r + !r`: E is admissible, expectation 1.
- `!r * !r`: source mean 1/2 versus blind mean substitution 1/4; retain G.
- `if !r < 3/4 then 0 else 1`: source mean 1/2 versus substituted 0; retain G.
- `observe(!r < 3/4); 1`: source output mass 1/2 versus substituted 1; retain G.
- `if !r < 3/4 then 1 else Ω`: source return mass 1/2 versus substituted 1; retain G.
- `r := 7; !r`: both return 7, so the earlier X may be E.

Distinct cells initialized from the same X remain correlated. Overwriting kills the dependency in that cell, not in a previously loaded value: `let y = !r in r := 0; y*y` still squares X. A closure `fun () => !r` instead reads the current cell. Dead-store elimination is separate: evaluating a subsequently overwritten RHS can diverge, reject, fail, or perform effects.

## Shared symbolic heap invariant

Extend the existing joint E-sample history, rather than building per-cell distributions or relating unconditional cell means.

Existing ingredients: `Symbolic.Affine n`; `SampleEnv.actualMeasure` (joint dependent sampling); `SampleEnv.meanEnvironment`; `SymbolicSoundness.SampleEnv.integrable_affine` and `integral_affine`.

Maintain a symbolic history η, heap S of affine forms, and symbolic residual expression e. For μ = η.actualMeasure and m = η.meanEnvironment, realize the whole configuration with the same coordinate environment u:

```
C(u) = (map (fun a => a.eval u) S, e.realize u)
```

The source component has law `C_* μ`; the target is the determinization of `C(m)`. Retained G-trace weights remain separate, as in the current proof. In a CEK formulation the same realization covers saved operands, environments and continuations.

Required equations:

- Lookup commutes with realization; reading introduces no sample coordinate.
- Updating an affine heap at location ℓ commutes with realization; aliases share that entry.
- Allocation appends an affine form and returns a fresh index, independent of numeric contents.
- A new E draw weakens **all** affine forms, including the entire heap and pending computation, before introducing the fresh coordinate. Existing `SymbolicAction.wrap` already accounts for lifted expression contexts; extend it to stores.

A minimal affine-store theorem can state that an affine map of a probability law with integrable coordinates has coordinate means given by applying the map to those means. Include heap entries and saved numeric values together. Assignment acts on the joint law of heap and RHS, never a product of their marginals.

This is only the local calculation: the whole-language proof must show accepted executions remain affine in E coordinates and control does not depend on them.

Unconditional heap means are insufficient even when the G marginal is unchanged. If `G=X` and `H[ℓ]=X`, retaining G but replacing the cell with 1/2 changes E[G H[ℓ]] from 1/2 to 1/4. In the actual invariant, G information is constant in E coordinates in each fixed G-replay component; `realG` requires zero E coefficients.

## Recommended first language

Support numeric cells, unrestricted aliases, closures capturing references, higher-order functions outside the heap, and recursion. Initially prohibit cells containing references or closures.

Mathematical heap: `List ℝ`; executable heap: `Array Rat` or `List Rat`; locations: natural-number indices. Append allocation. Initially no deallocation, reuse, address arithmetic, allocation failure or concurrency; runs start with empty heaps.

Add `Ty.ref m` where m is the fixed E/G mode of contents. Typing becomes `Σ; Γ ⊢ e : τ`, with store typing `Σ : List Affinity`:

```
e : float[m]                  => ref e : ref[m]
e : ref[m]                    => !e : float[m]
e1 : ref[m], e2 : float[m]    => e1 := e2 : unit
```

Runtime location typing consults Σ; allocation extends Σ. Reference subtyping is invariant: float[G] <: float[E] must not lift covariantly through references. Otherwise an E write can contaminate a G alias used for control. Reading a G cell may still produce a value coerced to E.

The existing multiplication rule demands a G left operand, so reading the same cell on both sides forces that cell to G. Comparisons also force G. All aliases and writes share the fixed mode.

This deliberately misses flow-sensitive overwrite opportunities: writing a random value, overwriting with 0, then comparing the cell forces the initial write to G in the fixed-mode system. A later versioned alias-aware store analysis could recover it; strong update requires a precise target, and saved values retain old dependencies.

A flow-sensitive effect system is unnecessary for the structural E-to-mean transformation under invariant store typing and whole-heap simulation. It is necessary for later rewrites: G/unit expressions can mutate E cells. Track read/write/allocation and latent function effects before commuting, folding or caching them. Unknown calls invalidate relevant heap facts.

Extend `Frontend.Infer` with reference shapes/equal mode constraints, and `Checking.check` with certified declarative rules. Inference remains untrusted; `Certified` checks typing and structural alignment.

Closures capture location handles, not saved stores. Invocations use the current heap, including reentrant callbacks. General-value stores are a separate extension using intensional syntax/closure values, not arbitrary semantic functions or mean closures. The syntactic simulation might extend without a new step-indexed logical relation, but that requires its own store-typing/measurability proof.

## Operational semantics and public theorems

Add runtime `.loc ℓ` and source allocation/read/write; forbid location literals in source. Reduction acts on `(H,e)` configurations. Actions carry next configurations or sample continuations returning configurations.

Left-to-right evaluation:

- `ref e`: evaluate e, allocate in resulting heap.
- `!e`: evaluate e, read from resulting heap.
- `e1 := e2`: evaluate target then RHS, update the heap **resulting from the RHS**, not a saved earlier heap.

Invalid locations/shapes are stuck, not rejection. Rejection prevents later effects and is not rollback. Closed numeric observation discards heaps on unsuccessful execution. Mean nodes still evaluate and validate parameters.

Proposed expectation theorem retains existing hypotheses:

```
Typed [] [] program (float E) →
DomainSafe (initial program) →
Integrable id (runLaw program) →
DomainSafe (initial program.determinize) ∧
Integrable id (runLaw program.determinize) ∧
∫ x, x ∂runLaw program = ∫ x, x ∂runLaw program.determinize
```

The main proof target is the analogue of `Spec.Traces.conditionalLawThm`: target safety, equal terminating retained-G-trace laws, and almost-everywhere integrable source conditional output with target conditional law a Dirac at its mean. Do not add global integrability to that trace theorem. Existing generic factorization arguments then give expectation, mass, Jensen, extended expectation, variance, and conditional corollaries.

Migration:

- `Spec/{Syntax,Types}.lean`: reference syntax/types and store-indexed typing.
- `Spec/Semantics.lean`: heap-threaded reduction, exact-depth semantics and safety.
- `Proof/Semantics/Typing.lean`: substitution/preservation under store extension.
- `Proof/Symbolic/Syntax.lean`: symbolic configurations, whole-configuration weakening/realization.
- `symbolicReduce_realize`, `symbolicReduce_targetRealize`: configuration reduction commutation.
- `symbolicReduce_wellTyped`, `SafeConfigAt`: heap well-formedness and zero E coefficients in G cells.
- `Proof/Traces/{CompactFiberSoundness,CompactSoundness}.lean`: heap-bearing induction and factorization.

Deterministic reference steps use realization equations; E draws extend/weakening the shared history; G draws use E-independent parameters and existing measure swapping; termination applies `integral_affine`. Uniformity under contexts/substitution/recursion/updates is the hard work.

Source and target allocate in lockstep, so base proof can use identical indices. Separately state renaming invariance by a bijection of allocated locations and extend that bijection on allocation. A fixed arbitrary permutation of naturals does not commute with append allocation. Garbage collection/unequal domains need stronger later relations.

The paper reducer represents rejection as a self-loop and current `returnOrDivergeThm` counts it as divergence; finite models distinguish it. Preserve that convention initially. Pro proposes a separate first-hit rejection observation (including rejection under active contexts), return+reject+genuine-divergence mass conservation and preservation of each. Output-law equality alone cannot distinguish rejection from divergence. Reference-free conservativity should cover output laws, trace laws and safety; relate old divergence to rejection plus genuine divergence.

## Measurability and integrability

Numeric heap space is the countable disjoint union of ℝ^k. Length/locations are discrete and cell contents use the ordinary real measurable structure. Lookup/update/append are measurable on valid-location pieces.

Extend current real-free skeleton plus finite-real-coordinate representation: locations and heap length belong to skeleton; numeric contents to coordinates. Do not use a discrete sigma algebra for real heaps. Measurable configuration representations can remain proof machinery unless an exported theorem mentions them, in which case relevant definitions belong in Spec/.

Conditional output remains real-valued, so no disintegration of semantic function spaces is required.

No global integrability of the whole heap is required. Need affine-coordinate integrability under each finite safe E history, output integrability for finite expectation, and output `MemLp id 2` for variance. Unbounded allocation/loops/G-dependent coefficients do not guarantee integrability; irrelevant large heap values do not invalidate an integrable returned scalar.

`Proof/Normalization.lean` normalizes probability laws, not programs. `normalized_probability` and `normalized_variance` remain generic and unchanged once program results supply equal mass/means and second-moment bounds. Keep zero return mass explicit.

## CEK, certificates, finite extraction, Storm

Extend `Finite.Value` with locations and `Finite.State` with heap; add allocation/read/assignment frames. Closures save handles; each probabilistic successor has its own heap value (never a shared mutable host heap).

Bridge changes `stateExpr : State → Expr` to `stateConfig : State → Config`. Closing environments retains location leaves; it does not inline current cell contents. Administrative equality compares full configurations. Committed reference operations are semantic steps. Extend location validity invariants through environments, expressions and frames; exclude forged source location literals.

Extend syntactic `expressionWork`/`frameWork`; heap size cannot be decreasing because allocation increases it. Migrate `SameObservations`, `StepMeaning`, horizon comparisons and `execution_output_eq` to configurations.

Structural equality/hash/serialization/replay include heap and saved locations. Tampering with cell values, location handles, write destinations, freshness or successor heaps must fail replay. Full finite-model replay still establishes safety/output-law equality without requiring determinization typing or termination; typing is needed when transferring target expectations to source.

Unbounded cell contents, heap size, environments and continuations still cause infinite state. Renaming does not collapse unbounded live cells. Garbage collection plus canonical renaming requires a root reachability/equivalence proof through captured values and saved environments. Sampling support boundaries remain unchanged.

Storm's transition-matrix interpretation need not change. Source/target selection remains explicit; a finite target model does not prove source safety/integrability, and target variance is not source variance. The rejection bridge needs its separate observation contract.

Already evaluated numeric additive frames remain candidates for reward extraction with a nonempty heap. Keep the heap and add-zero guard. Do not abstract mutable accumulators merely because updates use addition. For later RHS folding, test:

```
(* Initially r=0 *)
!r + (r := 1; !r)       (* 1 *)
(r := 1; !r) + !r       (* 2 *)
```

Deterministic evaluation alone does not justify early reads or operand reordering.

## Stages and decisive experiment

1. One-cell semantics, exact example laws and heap realization equations, plus negative examples.
2. Store preservation, whole-configuration weakening/commutation/safety, compact replay factorization, conditional-mean theorem and existing corollaries.
3. Heap CEK, reification/progress/replay, frontend/checker/runtime/serialization/Storm coverage. Export reference determinization certificates only with the corresponding theorem.
4. Independently improve overwrite precision, effects, general-value stores, garbage collection and finite-state quotienting.

All restrictions/eligibility/observations/renaming contracts/exported claims belong in Spec/. Affine heaps and replay induction can be in Proof/. Extend theorem-registration auditing if adding separate Spec/Refs propositions; the current script's explicit file list would miss them.

First experiment:

```
let r = ref 0 in
let s = r in
let read = fun () => !s in
r := bernoulli[E](1/2);
let a = read () in
r := !r + bernoulli[E](1/4);
a + read ()
```

Source returns `2X+Y`, law `(3/8)δ₀ + (1/8)δ₁ + (3/8)δ₂ + (1/8)δ₃`; target returns 5/4. Prove actual execution laws. This exercises aliasing, captured-reference reads, immutable saved read, read-before-write and heap/pending-operand weakening for the second draw. Replacing final addition with multiplication must reject explicit E annotations or infer the necessary G mode.
