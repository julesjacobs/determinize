# Paper vs. Lean formalization: comparison and a proposed paper structure

Date: 2026-09-10. Compared: `tex/*.tex` (sections 1 to 7; `8_old.tex` is dead) against
`lean/Determinize/**` (trusted `Statement/`, `Traces/`, `Theorems.lean`; proofs in `Proof/`),
plus `lean/README.md` and `lean/mul-div-typing.md`.

Part 1 is a concept-by-concept map. Part 2 lists similarities, Part 3 differences
(grouped, each with file pointers). Part 4 proposes a paper structure that covers everything
that is defined and proved today, ordered for a reader who has not seen the project. Part 5
lists the decisions the authors have to make before the paper and the Lean can be aligned.

---

## 1. Concept map

Legend for the status columns: **done** = defined/proved; **stated** = statement only, proof
missing or marked TODO; **informal** = prose or sketch; **absent** = not present.

| Concept | Paper (tex) | Lean |
|---|---|---|
| Modes `E`, `G`; types with `float[m]`, products, sums, lists, arrows | done, `2_syntax.tex` | done, `Statement/Types.lean` |
| Expression syntax | done (grammar lists only `uniform`, `gaussian`; the typing rules add poisson, exponential, beta, gamma; `/` appears in typing/semantics but not in the grammar; `flip` appears in examples but nowhere else) | done, `Statement/Syntax.lean` (`Expr`: all six primitives, `div`, `promote`, de Bruijn variables, no `flip`/`discrete`) |
| Typing judgment | done, `3_typing.tex` (subsumption + structural subtyping, literal-based `Mul-*`, `Div` with literal denominator) | done, `Typed` in `Statement/Syntax.lean` (explicit `promote`, no subtyping, `mul`: left G, `div`: G denominator) |
| Mode preorder `G ≼ E` | done | encoded by `promote` only |
| Type inference (constraints, greatest solution) | done (algorithm description + examples; one example "With lists" is empty), `4_inference.tex` | absent (by design: not needed for soundness) |
| Determinization `E(e : τ)` | done, `5_determinization.tex`, type-directed table, two figures with the same label `fig:determinization` | done, `Expr.determinize` (term-directed via mode label on sample sites; `Kind.stochastic → Kind.mean`) |
| Mean operators `mean_D` | done (six formulas; note "Consider divide by 0") | done, mean fibers in `Statement/Primitives.lean`, `meanValue` in `Proof/Internal/PrimitiveLaws.lean` |
| Small-step operational semantics | done as tables (`tab:small-step-semantics`, `tab:mean-small-step-semantics`); as a kernel on a measurable space of expressions | done, `reduce : Expr → Action` (`next` / `sample site fiber k` / `stuck`), `Statement/Semantics.lean` |
| Measurable space of expressions | stated, "TODO define a measurable space" | done in the proof only (`Proof/Internal/ExpressionSpace.lean`, `Proof/Measurability.lean`, about 5000 lines); deliberately absent from the trusted statements |
| Step is a Markov/sub-Markov kernel | stated, no proof | done, `StepKernel`, `MeasurableActionFamily.stepKernel` |
| Type preservation | stated (as "well-typed set has full measure"), no proof | done, `reduce_typed_closed` (`Proof/Typing.lean`) |
| Progress / stuckness | absent (values absorbing only) | done, `primitiveDomainSafe_iff_doesNotGetStuck`: for typed closed programs the only stuckness is an off-domain primitive |
| `n`-step semantics, values absorbing, monotone mass | done (proof of monotonicity), "TODO prove kernel" | done, `nStepMeasure`, `cumulativeOutputMeasure` (direct recursion on fuel) |
| Big-step output law | done as pointwise limit ("TODO: prove" existence) | done, `bigStepMeasure := ⨆ fuel, cumulativeOutputMeasure fuel` (unnormalized, terminal reals only) |
| Validity of executions | informal ("valid G-mode trace": no undefined arithmetic, no invalid parameter) | done, `DoesNotGetStuck` (a.s. at every depth; covers E draws too) |
| Symbolic residual expressions `sym(a)`, affine `Aff(U)` | done, `6_soundness.tex` §Symbolic | done, `AffineExpr n`, `Affine n = ℝ × (Fin n → ℝ)` (`Proof/Symbolic.lean`) |
| Symbolic sample environment `σ` (E draws only, affine parameters in earlier draws) | done | done, `SampleEnv laws n` (`snoc history op affineArgs generalArgs`) |
| `⟦σ⟧_env` (sampling law of the environment) | done | done, `SampleEnv.actualMeasure` |
| `⟦σ⟧_mean` (sequential conditional means) | done | done, `SampleEnv.meanEnvironment` |
| Symbolic typing (`Sym` rule) | done (no side condition on G-typed symbolic values) | done, `AffineExpr.WellTyped`; `realG` requires zero coefficients on E draws (the "G-constancy" invariant) |
| `lift` / `init`, `realize_ρ`, `erealize_ρ` | done (`realize_()(lift e) = e` proved; `erealize_()(lift e) = E(e)` proved) | done, `AffineExpr.ofExpr`, `realize`, `realize_ofExpr`; expected realization is `realize` at the mean environment followed by `determinize` (`targetRealize`) |
| Symbolic small-step semantics | done as table (let, if, add, lt, uniform only; other primitives "etc.") | done, `symbolicReduce : AffineExpr n → SymbolicAction` with actions `next` / `sampleE` (extends the history) / `sampleG` (branches on a real) / `stuck` |
| Actual interpretation commutes with one symbolic step | done (if-case written out; "remaining cases analogous") | done, `symbolicReduce_realize` (all cases, by induction on typing) |
| Actual interpretation of `n`-step and of initialized semantics | done | subsumed by the depth induction in `exactDepth_fiberSound` |
| Expected interpretation commutes with one symbolic step | stated, "TODO" | done, `symbolicReduce_targetRealize` |
| Expected interpretation of `n`-step / initialized | stated, "TODO" / done modulo the previous | subsumed by the depth induction |
| Symbolic preservation (typing is preserved by symbolic steps) | absent | done, `symbolicReduce_wellTyped` |
| Mean valuation agreement (`E_env[u_i] = ⟦σ⟧_mean(u_i)`) | stated, "TODO"; assumes "integrable σ" and "mean affine in the parameters" | done, `integral_affine`, together with `integrable_affine` which *proves* integrability from validity and `PrimitiveMomentBounds` (`Proof/SymbolicMoments.lean`, `Proof/PrimitiveMoments.lean`) |
| Validity transfers to the determinized program | absent | done, `determinize_primitiveDomainSafe_of_typed_source`, via convexity of parameter domains (`domain_valueSet_convex`, `domain_at_meanEnvironment`) |
| Determinization preserves typing | absent (implicit in the table's `: τ` column) | done, `typed_determinize` |
| Tracewise agreement of interpretations | done (proof assumes `σ` integrable; refers to a nonexistent label `thm:pointwise-agreement-of-interpretations`) | done as `exactZero_fiberSound` (the value case) inside `exactDepth_fiberSound` |
| Tracewise soundness (fixed G trace, finite and equal expectations) | stated; proof "follows partly from"; the tracewise semantics is explicitly skipped ("NOTE: Skipping many steps") | done and stronger: `Traces.soundnessThm` / `MeanOnTraces` (joint law factorization over the source trace law, Markov fiber, a.s. integrable fiber with mean equal to the target output) |
| Trace erasure (marginalizing traces gives the output law) | absent | done, `Traces.correspondenceThm` |
| Same trace law / same termination probability for source and target | absent | done (consequence of `MeanOnTraces`: the target joint law is the pushforward of the source trace law) |
| Global convex domination (Jensen) | done for one symbolic-state distribution `μ`; the `n`-step and big-step versions "straightforward?" | done, `jensenThm` (`Proof/Corollaries.lean`), for real-valued nonnegative convex `φ` |
| Global agreement in extended reals | done for `μ`; `n`-step and big-step versions written, well-definedness "the same" | done, `extendedExpectationThm` |
| Finite (Bochner) expectation theorem | absent as a separate statement | done, `mainThm` |
| Examples | trace figure (`figures/trace.tex`), tracewise/global figure (`figures/soundness_trace.tex`), inference examples | `Proof/Examples.lean`: nested sampling, `x + 1/y`, left-scaled E draw, a captured sample, a divergent loop with zero output mass |
| Discrete primitives (`flip`, `bernoulli`, `discrete`), `observe`, `sub`, `leq` | absent (OCaml only) | absent |

---

## 2. Similarities

1. **Same core idea, same invariant.** Both prove that replacing E-moded draws by their
   means preserves expectations because every E-typed value is an *affine* function of the E
   draws whose coefficients depend only on G values, and every G value is independent of the E
   draws. The paper carries this in `sym(a)`, `Aff(U)` and `⟦σ⟧_mean`; the Lean carries it in
   `Affine n`, `SampleEnv`, `WellTyped.realG` (zero coefficients) and `meanEnvironment`.

2. **Same symbolic-environment discipline.** In both, the environment records only E draws, in
   order, with parameters affine in earlier E draws; G draws are resolved immediately (paper:
   the `uniform[G]` row samples and substitutes `sym(v)`; Lean: `sampleG` branches on a real
   and leaves the history unchanged).

3. **Same mean environment.** `⟦σ, u ~ D(a)⟧_mean` evaluates `a` at the previous means and
   takes the mean of `D` there; `meanEnvironment (snoc history op args)` does exactly this.

4. **Same two "interpretations" of a symbolic state.** Paper: `act` (sample `σ`, realize) and
   `exp` (means, erealize). Lean: `realize` under `actualMeasure` (source side) and
   `realize` at `meanEnvironment` followed by `determinize` (target side, `targetRealize`).
   The paper's Lemma "actual interpretation of symbolic small-step" is
   `symbolicReduce_realize`; the paper's (unproved) Theorem "expected interpretation of
   symbolic small-step" is `symbolicReduce_targetRealize`.

5. **Same mean-operator design.** Both make `mean_D` an atomic construct of the target
   language (paper: `mean_D(e₁..e_k)`; Lean: `Kind.mean` on the same constructor), so the
   determinized program has the same shape as the source and the same evaluation order of
   operands. Both note that this is what makes the expected interpretation commute with steps.

6. **Same six primitives and the same table of means and mode requirements.** The paper's
   typing rules for the primitives (which parameters may be E) coincide with Lean's
   `affineArity`/`generalArity` split: uniform bounds, Gaussian mean, Poisson rate and gamma
   shape may be E; Gaussian variance, exponential rate, beta parameters and gamma rate must
   be G. Comparison needs G operands in both.

7. **Same two-level statement of soundness.** Both distinguish a *tracewise* statement (fix
   the G draws, expectations are finite and equal) from a *global* statement (integrate over
   G draws, expectations may be infinite, extended reals, Jensen). The
   `figures/soundness_trace.tex` example (`uniform_E(1,2) / uniform_G(0,1)`) is exactly the
   Lean `reciprocal` example in `Proof/Examples.lean`.

8. **Same semantics style.** Small-step, call-by-value, left-to-right, values absorbing,
   output law as a monotone limit over step counts, no normalization by termination
   probability, product/sum/list/function values, `rec`/`fix`, `let`, `if`.

9. **Same restriction of the theorems to closed float programs** at either mode.

---

## 3. Differences

### 3.1 Language and typing

- **Subsumption vs. explicit promotion.** Paper: `Subsumption` with `Float[G] <: Float[E]`
  and structural subtyping on pairs, sums, lists, arrows. Lean: one coercion `promote :
  Float[G] → Float[E]`, nothing structural. A paper-typed program must be elaborated by
  inserting `promote` (and eta-expanding / mapping through data structures); this
  elaboration is not formalized, so the Lean language is a fragment of the paper's
  (`lean/README.md`, "Explicit coercions").
- **Mode annotations.** Paper: the transformation is type-directed on a fully annotated
  term (`e : τ` at every node). Lean: expressions carry a mode only on sample sites; literals
  and arithmetic are mode-free; `Typed` assigns types separately and a literal types at
  either mode.
- **Multiplication.** Paper: `Mul-G` (both G) and `Mul-ConstL`/`Mul-ConstR` (a *literal* on
  one side at mode ≤ m). Lean: `left : G, right : m`, for arbitrary expressions. So `y × x`
  with `y` a G expression and `x` E is accepted by Lean, by the paper only for literal `y`;
  `x × 2` is accepted by the paper, rejected by Lean. `lean/mul-div-typing.md` derives the
  maximal sound rule (symmetric, one G factor) and recommends adopting it everywhere.
- **Division.** Paper: literal denominator typed ≤ G. Lean: any G denominator, `x / 0 = 0`.
  The paper leaves "Consider divide by 0" open.
- **Addition.** Paper: `m₁ ≼ m, m₂ ≼ m`. Lean: both operands at exactly `m` (use `promote`).
- **Variables.** Named vs. de Bruijn; `rec f x. e` vs. `fix` with two binders. Cosmetic.
- **Grammar completeness in the paper.** The grammar in `2_syntax.tex` lacks `/`, `()`,
  poisson/exponential/beta/gamma and `flip`, all of which are used later. Lean's `Expr` is
  the complete list.

### 3.2 Semantics

- **Object of the semantics.** Paper: a kernel on a measurable space `Expr_τ` (not
  constructed) with output read off `Val_τ`. Lean: `reduce : Expr → Action` is a plain
  function; sampling actions carry a *measure on ℝ* and a continuation, and the output
  measure is defined by recursion on fuel *without* any σ-algebra on expressions
  (`cumulativeOutputMeasure`). Measurability is a proof-internal construction
  (skeleton + real coordinates). The paper's "Lemma [Measurable space of expressions]" and
  "Step is a Markov kernel" become internal lemmas, not trusted definitions.
- **Off-domain parameters.** Paper: `uniform(v₁,v₂)` with `v₁ > v₂` is the zero measure;
  `v₁ = v₂` is not covered; other primitives and all `mean_D` rows are unconditional. Lean:
  every primitive, stochastic *or mean*, is the zero measure outside its domain
  (`uniform` needs `a ≤ b`, Gaussian `0 ≤ σ²`, Poisson `0 ≤ λ`, exponential `0 < λ`, beta
  `0 < α, β`, gamma `0 < k, θ`), `uniform(a,a)` is `dirac a`. Off-domain = stuck = lost mass.
- **Validity as an explicit hypothesis.** Lean's theorems assume `DoesNotGetStuck program`
  (almost surely, at every depth, every primitive is in-domain, E draws included) and
  *prove* `DoesNotGetStuck program.determinize`. The paper's "valid G-mode trace" is
  informal and mentions only G draws. The hypothesis is necessary: `let x = uniform_E(0,1)
  in uniform_E(x, 1/2)` loses half its mass in the source but none after determinization
  (`mul-div-typing.md` §8). Lean also shows that structural stuckness never occurs for typed
  closed programs, so validity reduces to primitive-domain safety.
- **Output type.** Paper: measures on `Expr_τ` restricted to `Val_τ` for arbitrary `τ`.
  Lean: output is a measure on ℝ; only terminal reals contribute; other types occur during
  evaluation but not as output (the theorems are about float programs anyway).

### 3.3 Determinization

- Paper: defined on typed terms by the mode of the *result* type of each sample site.
  Lean: `determinizeKind mode kind` on the site's own label, and `sourceForm` excludes mean
  sites from inputs. Same function on programs annotated by the paper's inference, but the
  paper's version is not a function on raw terms.
- Lean additionally proves that determinization preserves typing (`typed_determinize`),
  evaluation order and validity; the paper implicitly assumes these.

### 3.4 Proof architecture (the largest difference)

**Paper route (global-first).** Build `μ_n = ⟨init(e)⟩̂_n`, a distribution over symbolic
states. Show `act` and `exp` commute with symbolic steps, so `⟨e⟩_n = μ_n >>= act` and
`⟨E(e)⟩_n = μ_n >>= exp`. Prove pointwise (per symbolic state) that `act` and `exp` have
equal expectations (and Jensen domination) on `Val_τ`, integrate over `μ_n`, then take
`n → ∞` with a monotone-convergence lemma for extended-real expectations. The tracewise
theorem is stated separately and "follows partly" from the pointwise agreement.

**Lean route (tracewise-first).** Induct on reduction depth with the invariant
`SafeConfigAt fuel history expression` (history domain-safe, expression well-typed at
`Float[E]`, source realization a.s. domain-safe for `fuel` more steps). At each depth prove
`FiberSound (historyReplay depth history e) (actualTraceLaw depth history e)
(targetTraceLaw depth history e)`: the *joint* law of (G trace, output) of the source at
exactly this depth equals the target joint law pushed through a kernel that replays the G
draws from the trace and re-samples the E draws, and the kernel is a.s. an integrable
probability measure with mean equal to the target output. The three actions `next`,
`sampleE` (extend the history; `integral_affine` gives the mean), `sampleG` (mix over the
drawn value and prepend it to the trace) are the three inductive cases
(`Proof/SymbolicTraceSoundness.lean`). Summing over depths and factorizing
(`FiberSound.factorization`) gives `MeanOnTraces`; the finite, extended-real and Jensen
theorems are *corollaries* of `MeanOnTraces` by integrating over the trace law
(`Proof/Soundness.lean`, `Proof/Corollaries.lean`).

Consequences of the difference:

- The Lean never forms a distribution over symbolic states, so it never needs a
  measurable space of symbolic states or the "restriction of bind" / "expectation of bind"
  lemmas of the paper. It does need measurability of `realize`, of the replay kernel, and of
  trace operations, which the paper never mentions.
- The paper's global theorems silently use "σ is integrable" for every symbolic state in the
  support of `μ` (the convex-domination proof says "Since σ is integrable"), which is not
  among their hypotheses. The Lean discharges this: `integrable_affine` proves integrability
  from validity plus first-moment bounds on the six primitives (`PrimitiveMomentBounds`).
- The Lean's trace theorem is strictly stronger than the paper's tracewise theorem: it is a
  statement about *laws* (the target's trace law equals the source's; same termination
  probability), holds for a.e. trace without any integrability hypothesis, and it identifies
  the target output as a measurable function of the trace.
- The paper's `n`-step then limit argument (`lem:extended-expectation-increasing-limits`)
  has no Lean counterpart; the Lean sums exact-depth measures instead of taking limits of
  cumulative ones (`jointMeasure := Measure.sum (exactMeasure · program)`), and proves that
  this sum's marginal is `bigStepMeasure` (`correspondence`).
- The Lean splits traces into a *detailed* internal trace (one event per step, `Proof/Internal/StepTraces.lean`)
  used for the induction, and the *compact* public trace (G draws only, `Traces/Semantics.lean`),
  with a decode/replay argument (`Proof/CompactTrace.lean`) showing that the compact trace
  of the determinized program a.s. determines the detailed one. The paper has neither.
- G-mode programs: the Lean proves them by promoting to `Float[E]` internally
  (`jointMeasure_promote`, `doesNotGetStuck_promote_iff`); the paper treats `m ∈ {E, G}`
  uniformly by having `sym(a)` typed at any mode.

### 3.5 Theorem statements

| | Paper | Lean |
|---|---|---|
| Hypotheses | closed, well-typed, `τ = Float[m]`; tracewise: "valid γ", "σ integrable"; global: `E[⟨e⟩]` well-defined | `Typed [] p (.float m)`, `p.sourceForm`, `DoesNotGetStuck p`; `mainThm` adds `Integrable id (bigStepMeasure p)`; `extendedExpectationThm` adds `HasExpectation` |
| Tracewise conclusion | finite expectations, equal | `DoesNotGetStuck p.determinize ∧ MeanOnTraces p p.determinize` (factorization over the source trace law, a.s. integrable Markov fiber with mean = target output) |
| Global conclusion | target expectation well-defined and equal (extended reals); Jensen for `φ : ℝ → [0,∞]` measurable convex | same in `EReal`; Jensen for `φ : ℝ → ℝ` convex nonnegative (finite-valued); plus the finite Bochner version |
| Extra | | trace erasure; same termination probability; validity of the target |

### 3.6 Things that exist only on one side

Only in the paper: type inference (§4) with the greatest-solution rule and the worked
examples; subsumption and structural subtyping; the small-step tables as a presentation
device; the monotone-mass lemma; related work; the two example figures.

Only in the Lean: measurable structure on expressions; kernel representation of `reduce`;
progress; `promote` handling of G programs; symbolic preservation with the G-constancy side
condition; validity transfer to the target and domain convexity; integrability from
validity; the moment bounds and mean formulas for Mathlib's six distributions; trace
erasure; detailed vs compact traces; replay/decoding; the finite-expectation theorem;
executable examples with `#print axioms` (only `propext`, `Classical.choice`, `Quot.sound`).

### 3.7 Paper-internal issues noticed on the way (not Lean-related, worth fixing)

- Two figures share `\label{fig:determinization}` (the "NEW; REFER TO THIS" one is the
  keeper).
- `\Cref{thm:pointwise-agreement-of-interpretations}` has no target (should be
  `thm:tracewise-agreement-of-interpretations`).
- Poisson rows in the determinization table use `m_3` where the type is `m_2`.
- The `Sym` typing rule assigns `sym(a)` any mode without requiring the affine expression
  to be constant when the mode is G; without that side condition the G-sample row
  `Uniform(a₁, a₂)` is not well-defined (the bounds would depend on unsampled E variables).
- The symbolic residual grammar includes `−` (binary), which the source grammar lacks.
- `\nocite{*}`, placeholder authors, `acmYear{2025}`.
- The "Introduction" is three lines; there is no example section, no contributions list, no
  conclusion; the related work is a bullet list of citations.

---

## 4. Proposed paper structure

Goal: a reader with a PL/probability background should understand *why* the transformation
is sound after Section 2, see the full theorem in Section 6, and be able to audit the Lean
in Section 9. Everything defined or proved in either artifact has a home below; proof
bodies that are routine or mechanized go to the appendix with a pointer to the Lean module.

### 1 Introduction
- Problem: programs mixing sampling and expectations; replacing a draw by its mean is
  sometimes right (`E[x + y] = E[x] + E[y]`) and sometimes wrong (`E[x·x]`, `E[1/x]`,
  `E[if x < c ...]`).
- The idea: a mode system that marks each float as E (only its expectation is used, affinely)
  or G (its full distribution is used), and a transformation that replaces E draws by means.
- Contributions: (i) the mode system and its inference; (ii) the determinization; (iii) a
  trace-based soundness theorem that needs no integrability hypothesis and yields finite,
  extended-real and Jensen versions; (iv) a Lean/Mathlib mechanization with standard axioms
  only; (v) implementation (OCaml, browser simulator, Storm export).

### 2 Overview by example
- Start with `figures/soundness_trace.tex`: `uniform_E(1,2) / uniform_G(0,1)`. Walk one
  G trace (finite, equal expectations), then integrate (both infinite).
- Introduce the *pre-sampled G tape* picture from `mul-div-typing.md` §2: fix the G draws;
  the source is a distribution over E draws, the target a number; the number is the mean.
  State informally the two invariants that make this work: (1) G values never depend on E
  draws; (2) E values are affine in the E draws with G-measurable coefficients.
- Show the negative examples (`x·x`, `1/x`, `x < c`, `uniform_G(0, x)` with `x : E`) and
  which typing rule rejects each.
- Show a validity failure (`uniform_E(x, 1/2)`) to motivate the non-stuckness hypothesis.
- One larger example with `let`, `if`, a list fold or recursion, to show that control flow
  and data structures are unrestricted as long as they are driven by G values.

### 3 The language
- 3.1 Syntax: the complete grammar (all six primitives, `/`, unit, `promote` or subsumption;
  decide per §5 below). One figure.
- 3.2 Types and modes; the mode order `G ≼ E` and what it means ("a G value may be used as
  an E value, never the converse").
- 3.3 Typing rules, grouped: core, data, control, arithmetic, primitives. State the general
  principle from `mul-div-typing.md` §5 in prose: *an operator may take an E operand
  exactly in the argument positions in which its result is affine in that argument*, and
  present the primitive table (which parameter may be E, and the mean formula) as one table
  that is reused by §4.
- 3.4 The two invariants as lemmas about the type system (no semantics yet): "G values are
  E-free" and "E values are affine". These are `WellTyped.realG` / `wellTyped_realG_coefficients`
  and the shape of `AffineExpr.WellTyped` in the Lean; in the paper they become the
  motivation for every non-standard rule.

### 4 Determinization
- The `mean_D` operators as target-language constructs, with the table from §3.3.
- The transformation as a function on mode-annotated terms (the Lean's `determinize`,
  which coincides with the type-directed table once sample sites carry their result mode).
  Keep only the "NEW" figure.
- Lemma: determinization preserves types and shape (`typed_determinize`, `determinize_isValue`).
- Worked examples (the two from `5_determinization.tex`).

### 5 Mode inference (algorithmic section)
- Constraint generation and the greatest-solution rule from `4_inference.tex`, with the
  examples completed (the "With lists" example and the `exponential(uniform*uniform)` one
  are placeholders today).
- Theorem (to write; not in Lean): the inferred annotation is a valid typing, and it is the
  greatest one, so it determinizes the most sites. Say explicitly that inference is outside
  the mechanized part and why (soundness is about the annotated program).
- If the symmetric `[Mul]` rule is adopted, describe the deterministic tie-break used by
  inference (`mul-div-typing.md` §7).

### 6 Operational semantics
- 6.1 Reduction as an *action*: `next e'`, `sample(site, μ, k)`, `stuck` (the Lean
  presentation). One figure with the reduction rules; the current `tab:small-step-semantics`
  and `tab:mean-small-step-semantics` become that figure or go to the appendix.
- 6.2 Primitive fibers: the law at a stochastic site, `dirac(mean)` at a mean site, zero
  off-domain; `uniform(a,a)`; `x/0 = 0`. One table (the third use of the primitive table).
- 6.3 Output law: `cumulativeOutputMeasure` by recursion on fuel and `bigStepMeasure` as its
  supremum; unnormalized; no σ-algebra on expressions needed. Remark that this replaces
  the kernel-on-expressions presentation and why (trusted base stays small).
- 6.4 Traces: the compact trace (list of G draws with their primitive), `jointMeasure`,
  `traceLaw`, and the erasure theorem (`correspondenceThm`).
- 6.5 Validity: `DoesNotGetStuck`; type safety (preservation and progress) showing that
  validity is exactly primitive-domain safety for typed closed programs; the promotion lemma
  for G-mode programs.

### 7 Soundness: statements
- 7.1 Main theorem (trace soundness): for a closed, well-typed, valid source program, the
  determinized program is valid and `MeanOnTraces` holds. Explain each conjunct in words:
  same trace law, source output is a Markov kernel of the trace, target output is a
  function of the trace, and a.s. that function is the fiber's mean and the fiber is
  integrable. Note: no integrability hypothesis.
- 7.2 Corollaries: finite expectations (`mainThm`), extended-real expectations
  (`extendedExpectationThm`), Jensen (`jensenThm`), equal termination probability. Give the
  one-line derivations (integrate the fiber identity over the trace law).
- 7.3 Discussion of the hypotheses: why validity is needed (example), why validity of the
  target is a conclusion, why the output law is unnormalized, why `φ` is real-valued in
  Jensen.

### 8 Soundness: proof
- 8.1 Symbolic expressions and environments: `Affine`, `AffineExpr`, `SampleEnv`,
  `actualMeasure`, `meanEnvironment`, realization; `realize_ofExpr`.
- 8.2 Symbolic reduction and its two realizations: `symbolicReduce` with actions `next`,
  `sampleE`, `sampleG`; the two commutation lemmas (`symbolicReduce_realize`,
  `symbolicReduce_targetRealize`) and symbolic preservation (`symbolicReduce_wellTyped`,
  including G-constancy). This is where the current `lem:symbolic-step` proof and the
  `thm:expected-symbolic-step` TODO go; write one case each in the text, the rest in the
  appendix or "by the Lean".
- 8.3 Means of affine coordinates: integrability from validity (`integrable_affine`, needs
  the first-moment bounds of the six laws) and the mean identity (`integral_affine`), i.e.
  the current "Agreement of mean and sampling environments" lemma, now proved. State the
  moment bounds as a lemma about the six distributions.
- 8.4 Validity transfer: the mean parameters are in-domain when the sampled ones are a.s.
  in-domain (convex domains), hence the target never gets stuck.
- 8.5 Fiber soundness by induction on depth: define `FiberSound`, state the three cases
  (`next`, `sampleE`, `sampleG`) and the value case (which is the current "tracewise
  agreement of interpretations" argument), then sum over depths and factorize.
- 8.6 From detailed to compact traces: replaying the G draws reconstructs the detailed trace
  a.s. (two paragraphs; appendix for the decoding argument).
- 8.7 Corollaries (the integration step from §7.2, written out).

### 9 Mechanization
- Trusted base: `Statement/`, `Traces/`, `Theorems.lean`; what a reviewer must read and what
  they may skip; the `#print axioms` output; build instructions; size of each layer.
- Deviations from the paper presentation (the `lean/README.md` list) and how each is
  justified: explicit `promote`, mode labels on sample sites, `x/0 = 0`, off-domain zero
  measures, real-valued Jensen.
- Engineering notes worth a paragraph each: measurability via skeleton + coordinates without
  a σ-algebra in the statements; s-finite kernels; the detailed/compact trace split.

### 10 Implementation and experiments
- The `.det` language and the OCaml pipeline (parse → infer → determinize → interpret,
  Storm export for discrete programs); the additional constructs (`flip`, `bernoulli`,
  `discrete`, `observe`, `-`, `<=`) and their status relative to the theory.
- The browser simulator with a coupled-trace runtime (the "shared G tape" made literal).
- Examples from `det/` and `examples/`: the sample means vs determinized values.

### 11 Related work
- Turn the current bullet list into prose organized by the three threads already chosen
  (generating functions / exact inference, moment analyses and expectation transformers,
  hybrid exact/sampling systems such as delayed sampling and semi-symbolic inference), and
  add the Rao–Blackwellization angle explicitly: determinization is a syntactic,
  type-directed Rao–Blackwellization along the E draws.

### 12 Conclusion and future work
- TODO.md items: more distributions, subtraction/division status, `discrete` branching;
  the symmetric multiplication rule; mechanizing inference; conditioning (`observe`).

### Appendices
- A: full typing rules including the surface subtyping and its elaboration into `promote`.
- B: full reduction rules (the current two tables) and the symbolic reduction table.
- C: measurability (summary of `Proof/Measurability.lean`).
- D: detailed traces, replay and decoding.
- E: the analytic facts about the six distributions used (`Proof/PrimitiveMoments.lean`,
  `Proof/Internal/PrimitiveLaws.lean`).

### Mapping from proposed sections to existing material

| Proposed | Existing tex | Lean |
|---|---|---|
| 2 | `figures/trace.tex`, `figures/soundness_trace.tex`, `mul-div-typing.md` §2 | `Proof/Examples.lean` |
| 3 | `2_syntax.tex`, `3_typing.tex` | `Statement/Types.lean`, `Statement/Syntax.lean` |
| 4 | `5_determinization.tex` | `Expr.determinize`, `typed_determinize` |
| 5 | `4_inference.tex` | none |
| 6 | `6_soundness.tex` §Concrete semantics | `Statement/Semantics.lean`, `Statement/Primitives.lean`, `Traces/Semantics.lean`, `Proof/Typing.lean` |
| 7 | `6_soundness.tex` §Relating | `Statement/Main.lean`, `Traces/Main.lean`, `Theorems.lean` |
| 8 | `6_soundness.tex` §Symbolic, §Tracewise, §Global | `Proof/Symbolic.lean`, `Proof/SymbolicSoundness.lean`, `Proof/SymbolicMoments.lean`, `Proof/SymbolicTrace*.lean`, `Proof/TraceFibers.lean`, `Proof/TraceSoundness.lean`, `Proof/CompactSoundness.lean`, `Proof/Corollaries.lean` |
| 9 | `lean/README.md` | whole tree |
| 10 | none | none (`ocaml/`, `sim/`, `det/`) |

---

## 5. Decisions needed before aligning the artifacts

1. **Multiplication and division rules.** Adopt the symmetric `[Mul]` and G-denominator
   `[Div]` everywhere (Option A of `mul-div-typing.md`), or one asymmetric rule everywhere.
   This changes `3_typing.tex`, `4_inference.tex`, `ocaml/infer.ml`, `sim/src/compiler/infer.js`
   and `Typed.mul`/`Typed.div` (+ proofs). The paper's `Mul-ConstL/R` and literal `Div` are
   strictly weaker than what is proved.
2. **Subsumption vs. `promote`.** Recommended: present the core calculus with explicit
   `promote` (as mechanized) and give subsumption plus structural subtyping as a surface
   layer elaborated by inference; state that the elaboration is not mechanized.
3. **Where mode annotations live.** Recommended: on sample sites only (as mechanized), which
   is also what inference produces; the type-directed table then reads as a definition on
   annotated terms.
4. **Semantics presentation.** Recommended: adopt the action-based reducer and the
   fuel-indexed output measure of the Lean in the main text, moving the kernel/measurable
   space presentation to the appendix. This removes four TODO lemmas from the paper.
5. **Primary theorem.** Recommended: make trace soundness the main theorem and derive the
   expectation theorems as corollaries, as the Lean does. The paper's current global route
   (distributions over symbolic states, `n`-step then limit) is then unnecessary and its
   hidden "σ integrable" assumption disappears.
6. **Validity.** Define `DoesNotGetStuck` in the paper, include E draws, and add the
   counterexample; decide whether `x/0 = 0` is kept (it is harmless for soundness).
7. **Off-domain and degenerate primitives.** Adopt the Lean's zero-measure convention and
   `uniform(a,a) = δ_a` in the paper's tables.
8. **Scope of the language in the paper.** Either add `flip`/`discrete`/`observe`/`-`/`<=`
   to the theory (discrete primitives are easy: `flip(p)` is a Bernoulli law with mean `p`,
   `p` may be E) or state clearly that the implementation is a superset.
