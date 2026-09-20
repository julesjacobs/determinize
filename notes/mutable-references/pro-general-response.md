# Pro replacement: general references

Source: https://chatgpt.com/c/6aaf0ba2-c96c-83ea-8214-03bf2ece8659

GPT-6 Pro completed the replacement response after 10m57s. This substantive synthesis supersedes the numeric-only recommendation in `pro-response.md`. Proposed definitions and claims are not implemented or Lean-checked.

## General types and heap typing

Add `Ty.ref τ` for every existing type τ: float[m], unit, bool, products, sums, lists, functions and references. Affinities remain on floats. General references and stored functions are baseline, including cyclic heaps through captured locations.

Keep existing structural subtyping for values, including G-to-E numeric coercion and contravariant arrow arguments. Add only reflexive reference subtyping; prove `ref τ <: ref σ ↔ τ = σ`. Equality covers the complete contained type, including affinities under arrows/data/nested references. After reading a function its value may use ordinary arrow subtyping. A subtype value may be subsumed to the fixed content type before writing. References containing those functions remain invariant.

Store signature `Σ : List Ty`; typing `Typed Σ Γ e τ`. Rules:

```
Σ[ℓ] = τ                    => ℓ : ref τ
Typed Σ Γ e τ                => ref e : ref τ
Typed Σ Γ e (ref τ)          => !e : τ
Typed Σ Γ a (ref τ), Typed Σ Γ v τ => a := v : unit
```

Use the current expression-as-value representation: `Heap Literal := List (Expr Literal)`, configuration = heap plus expression. `HeapTyped Σ H` means equal lengths and every entry is a closed value of type Σ[ℓ] under the **full current Σ**. Typing stops at location leaves and never unfolds the heap. Full-signature typing supports back edges/cycles.

Keep monomorphic lets. Inference shares content constraints across aliases and implements reference subtyping as equality, including nested affinities. `Checking.checkSubtype` supplies the declarative proof independently.

## Latent writes and invariance

Ordinary arrows allow effects. A function of type `float[E] → float[G]` may write its E argument to `ref (float[E])` and return G zero. Returning G is no purity claim. Writing that E argument to a G cell is ill-typed, regardless of whether the function is stored/passed/called later. No effect annotation is needed for this structural determinization proof because body typing and store preservation constrain the eventual writes. Optimization effect summaries remain separate.

A decisive negative example is treating `q : ref (float[E] → unit)` as `ref (float[G] → unit)`, then installing a function that writes its argument to a G cell and calling through q with an E draw. Arrow value subtyping allows the corresponding value conversion, but lifting it covariantly through ref leaks E into G. Full reference invariance blocks both argument-position and result-position leaks.

## Operational semantics

Add runtime `.loc` and source allocation/dereference/assignment. Reduce configurations `(H,e)` with `next Config`, `sample ... (ℝ → Config)`, `stuck`. Roots:

```
(H, ref v) -> (H ++ [v], loc H.length)
(H, !loc ℓ) -> (H, H[ℓ])
(H, loc ℓ := v) -> (H[ℓ := v], unit)
```

v is any value, including stored functions and structured values with location leaves. Invalid addresses/shapes or malformed non-value slots are stuck. Evaluation contexts wrap the resulting expression while threading the resulting heap.

Evaluate initializer before allocating. Assignment evaluates and saves the address, then evaluates RHS, then updates that saved address in the resulting heap. Example: `a=ref 0; b=ref 10; p=ref a; (!p):=(p:=b;1)` updates a, leaving `(a,b)=(1,10)`.

Reading copies a value; nested locations retain identities. Reading a stored function snapshots code and captured immutable values, while captured references read the current heap. `let old = !f in f := replacement; old()` invokes the old code. Similarly, `(!f) argument` selects f before argument effects.

Store recursion works without recursive types:

```
let f : ref (unit → float[E]) = ref (fun () => 0) in
f := (fun u => (!f) u);
(!f) ()
```

Heap `[fun u => (!loc 0) u]` is finite syntax with a cyclic reference graph. Typing and realization stop at loc 0; execution diverges through semantic dereference/application steps. `r := r` still requires an infinite type equation and is ill-typed in the existing nonrecursive type grammar. No heap-acyclicity restriction is introduced.

## General symbolic heap

Existing `AffineExpr n` already contains functions and data with affine literals. Extend reference constructors and define `SymbolicHeap n := List (AffineExpr n)` and heap/expression `SymbolicConfig n`. Each entry must be a symbolic value and has a full-signature typing derivation.

Realization maps finite syntax in **every heap entry and active expression**:

```
R_u(loc ℓ) = loc ℓ
R_u(lam body) = lam (R_u body)
R_u(real a) = real (a.eval u)
R_u(H,e) = (H.map R_u, R_u e)
```

It never follows a location into its cell. Captured E values inside stored lambdas are realized as literals without evaluating/averaging functions.

Configuration determinization is `D(H,e) = (H.map D, D e)`, including latent E sampling sites inside stored code/data. For empty-heap programs transformed stored code arises naturally; transforming the heap explicitly is needed for the induction and nonempty initial configurations.

Preserve the existing joint dependent `SampleEnv` history η. A symbolic C represents the source family `R_u C` under η.actualMeasure and target `D(R_m C)` where m is η.meanEnvironment. One shared u realizes all cells, code captures and active computation. `SafeConfigAt` carries history safety, **one symbolic typing and one Σ independent of u**, and almost-everywhere configuration safety. Per-realization concrete typing is insufficient because concrete real literals can be assigned either affinity, losing provenance.

Lookup/update/append commute with R and D for all value constructors. New E draws weaken the whole configuration, including stored lambda bodies and all old captures; lift existing `weakenSamples`/`WellTyped.mapAffine` over the heap. Read creates no fresh coordinate.

Generalize symbolic action cases `next`, `sampleE`, `mean`, `sampleG`, `stuck` to configurations. Prove configuration analogues of `symbolicReduce_realize` and `symbolicReduce_targetRealize`; mean-continuation naturality compares whole configurations.

No step-indexed semantic type interpretation is proposed. Typing and realization are structural over finite syntax and stop at locations. One-step dereference returns a value; application exposes a body, and further execution consumes operational depth. Extend the existing arrow-value inversion/substitution lemmas with Σ. This argument establishes this particular syntactic transformation, not arbitrary contextual equivalence for higher-order store.

## Preservation, safety and measurability

Store extension is prefix extension `Σ' = Σ ++ Δ`; old content types never change. A well-typed deterministic step has a well-typed successor under some extending Σ'; sample continuations retain Σ. As currently, typing preservation alone permits a stuck action and does not prove primitive-domain safety.

Prove ready well-typed reference reads/writes/allocations succeed. Allocation weakens typing for all existing entries and initializer. Assignment preserves typing of all stored closures because location types remain unchanged. Mirror for symbolic heaps.

Configuration `DomainSafeAt` retains existing meaning. Primitive safety at means still uses `domain_at_meanEnvironment`. New reference operations add no numeric domain predicates. Target-safety can be generalized to any result type; expectation claims remain numeric.

Canonical append allocation yields lockstep source/target indices. Renaming traverses all heap entries/code/environments, permutes cells and stops at locations; extend the bijection with fresh locations on allocation. No quotient space is necessary.

Measurable representation: `(heap of Expr Unit, active Expr Unit)` skeleton plus all real coordinates from heap and expression. Each skeleton has finitely many coordinates; skeletons form a countable family. Stored functions are finite syntax, and cycles are discrete location edges. Extend `Proof/Semantics/ExpressionSpace.lean`; prove lookup/update/append/realization/determinization/substitution/generated continuations measurable. Do not use discrete real values or extensional function quotients.

No mean of a semantic function, heap norm integrability, expected allocation bound or almost-sure termination premise is introduced. Only affine numeric coordinate integrability and existing source output moment hypotheses are needed.

## Trace factorization and public results

Use configuration definitions internally and keep program wrappers from empty heaps for `bigStepMeasure`, `traceAndOutputLaw`, `DomainSafe`. Public typing gains empty Σ: `Typed [] [] program τ`. Arbitrary deterministic initial configurations transform both heap and expression.

Central theorem: for typed safe configuration C returning float[E], target DC is safe, terminating G-trace marginals agree, and almost every conditional source output is integrable with target conditional law a Dirac at its mean. No global integrability assumption is added.

Within one symbolic configuration, E coordinates cannot affect location identities, constructors, selected code structure or Boolean values. G primitive parameters have zero E coefficients. Stored calls preserve this via body typing; G-returning calls may still update E-containing cells, accounted for in the continuation. References add deterministic steps, no compact G-trace entries.

Generalize `compact_exactDepth_fiberSound` over Σ and symbolic configurations. Deterministic case uses store simulation, E case weakens full state, G case reuses independent-fiber integration, terminal real uses `integral_affine`. Then recover compact replay factorization/conditional kernel identification. Generalize `TraceFactorization` to configuration laws or separate measure-only corollaries to avoid duplication.

Every current exported result retains its moment hypotheses:

- `returnOrDiverge`: mass balance for safe typed numeric result, E/G.
- `traceErasure`: joint law projection, no typing/safety prerequisite.
- `traceConditionalLaw`: target safety, equal trace marginals, conditional Dirac-at-mean.
- `expectationPreservation`: source integrability implies target safety/integrability/equal integrals.
- `extendedExpectationPreservation`: HasExpectation preserves defined extended expectation.
- `outputMassPreservation`: equal return mass.
- `jensenInequality`: nonnegative convex test integral decreases.
- `varianceNonIncrease`: source MemLp 2 gives target MemLp 2, second moment/variance inequality.
- `conditionalExpectationPreservation`: equal integral/mass quotient with current zero-mass convention.
- `conditionalVarianceNonIncrease`: positive mass and MemLp 2 give normalized result.
- `returnedExtendedExpectation`: positive mass and HasExpectation give normalized extended equality.
- `traceVarianceDecomposition` and `conditionalTraceVarianceDecomposition`: same integrated conditional-variance decomposition, normalized where applicable.

No no-functions, acyclic-store, finite-state or pure-callback premise is needed. Keep current paper convention counting rejection within divergence. Separating first-hit rejection from genuine divergence remains an additional observation/bridge task, not implied by output equality.

## Examples and decisive full experiment

Captured sample versus latent sample:

```
let x=bernoulli[E](1/2) in let f=ref(fun()=>x) in (!f)()+(!f)()
let f=ref(fun()=>bernoulli[E](1/2)) in (!f)()+(!f)()
```

Source laws respectively `(1/2)δ₀+(1/2)δ₂` and `(1/4)δ₀+(1/2)δ₁+(1/4)δ₂`; both targets δ₁. Reading code does not resample captures or execute latent samples.

Nested aliases: `r=ref X; p=ref r; s=!p; r:=!r+1; !s+!(!p)` returns `2X+2`. Redirecting p does not redirect s. Escaped immutable captures survive overwrite; a stored closure capturing `x=!r` can still square X after r is reset, while a closure reading r sees its new contents.

First full proof target:

```
let r = ref 0 in
let box = ref r in
let count = ref 2 in
let f = ref (fun () => 0) in
let x = bernoulli[E](1/2) in
f := (fun () =>
  if !count < 1 then x + !(!box)
  else (
    count := !count - 1;
    (!box) := !(!box) + bernoulli[E](1/4);
    (!f) ()
  ));
let alias = f in
(!alias) ()
```

Types: r `ref(float[E])`; box `ref(ref(float[E]))`; count `ref(float[G])`; f `ref(unit→float[E])`. Captured f creates a cycle; captured X must survive weakening for two subsequent draws. Source returns `X+Y₁+Y₂` with law `(9/32)δ₀+(15/32)δ₁+(7/32)δ₂+(1/32)δ₃`, mean 1 and variance 5/8; target δ₁. Prove actual execution, typing/safety/moments.

## Implementation/proof stages

1. General ref syntax/types, invariant subtyping, general heap/config semantics, store typing/substitution/freshness/renaming/preservation, cyclic functions included.
2. General symbolic configurations, typed stored code, realization, whole-state weakening and commutation/preservation.
3. Configuration measurable kernels/exact-depth semantics and target safety.
4. Heap-aware compact replay/FiberSound, conditional-law factorization and every existing theorem.
5. Heap CEK, configuration reification/progress, frontend/checker, full-state replay/serialization/tests/Storm.

Executable values remain finite with `.loc Nat`; closures hold code/environments, not heap snapshots. Reification stops at locations. Equality/hashing/replay compare full heaps. Dereference/write/allocation commits are semantic steps; store recursion cannot hide inside bookkeeping.

General refs do not imply finite-state exploration. Storm's completed rational graph format stays unchanged; source safety/integrability are not discharged by a target graph. Already evaluated additive numeric frames may still be extracted retaining the heap; deferred reads/calls cannot be folded across writes without effect reasoning.

Keep all restrictions/rules/observations/public claims in Spec/, symbolic construction and proof machinery in Proof/. Do not hide higher-order-store soundness behind an opaque assumed eligibility predicate. Extend theorem registration for new Spec files; no added axioms/sorry.
