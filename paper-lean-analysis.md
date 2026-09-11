# Paper plan review and pending changes across the repository

Historical design record; see `migration-audit.md` and `migration-plan.md` for the integrated implementation.

Date: 2026-09-10, updated 2026-09-11 after the authors took decisions D1 to D7 (Part 3) and
the Lean side was changed accordingly (Part 0). Supersedes the earlier paper-vs-Lean
comparison (git history has it).
Inputs: `tex/`, `lean/` (statements, proofs, `README.md`, `mul-div-typing.md`), `ocaml/`,
`sim/`, `det/`, `examples/`, `run.sh`, `det.sh`, `.claude/skills/{det-lang,storm}`, `TODO.md`,
and `pipeline-in-lean-analysis.md` (the D7 study).

Part 0 records the decisions and what has been done for them. Part 1 checks the intended
paper structure section by section and says what each section must contain and what already
exists for it. Part 2 lists, per component, every change that is still pending for the
repository to match the plan. Part 3 restates the cross-cutting decisions with their outcome.
Part 4 is a suggested order of work.

---

## 0. Decisions of 2026-09-11 and what changed

The Lean formalization is authoritative for the language and the theorems. Nothing in `tex/`,
`ocaml/` or `sim/` was changed on 2026-09-11; their pending alignment is listed in Part 2.

| Decision | Outcome | Lean change |
|---|---|---|
| D1 multiplication, division | Lean rules: `e₁ × e₂ : Float[m]` iff `e₁ : Float[G]`, `e₂ : Float[m]`; `e₁ / e₂ : Float[m]` iff `e₁ : Float[m]`, `e₂ : Float[G]`; no implicit `G → E` cast | none; decision recorded in `lean/mul-div-typing.md` §7 |
| D2 subsumption | explicit `promote`; the paper remarks that it could be made implicit by elaboration | none |
| D3 mode labels | on sample sites only; determinization is a function on untyped terms | none |
| D4 validity, degenerate cases | as in Lean (`DoesNotGetStuck`, zero measure off-domain, `uniform(a,a) = δ_a`, `x/0 = 0`) | none |
| D5 discrete primitives | added: `bernoulli` (probability at the site's mode, determinized to `p`), `discrete` over literal weights (determinized to `∑ i·pᵢ`), `flip` as the sugar `0 < bernoulli_G(p)` (Boolean, never determinized, G probability) | commit 72ea1d1 |
| D6 `observe` | added as a constructor; `observe false` rejects with zero output and trace mass and is not stuckness; every Boolean is general-mode information, so source and target reject the same traces; new `conditionalExpectationThm` (mass-normalized expectations agree) | see Part 3 D6 |
| Variance | new `outputMassThm`, `varianceThm` (second moment and Mathlib `variance` do not increase) and `Traces.varianceThm` (law of total variance along traces: source variance = target variance + mean fiber variance) | commit 04963c0 |
| D7 pipeline in Lean | analysis only: `pipeline-in-lean-analysis.md` recommends the Lean+OCaml certificate split now (leaf-only `Certificate.lean` with a decidable typing checker, then an OCaml emitter into one generated file checked by `lake build`), staged so that it can grow into an all-Lean pipeline; Rocq is not recommended | none |

Priorities used below: **P0** = blocks writing the section or is a correctness problem;
**P1** = needed for a complete and consistent paper; **P2** = polish.

---

## 1. The plan, checked

### 1 Introduction
Content: problem (mixed sampling/expectation programs; replacing a draw by its mean is
sometimes right, sometimes wrong), the idea (modes E/G, mode-directed determinization), the
guarantee in one sentence (trace-by-trace the determinized output is the conditional mean of
the source output), contributions, and a pointer to the mechanization.
Exists: three lines in `tex/1_introduction.tex`. Verdict: fine as a section; everything is
still to be written.

### 2 Overview of the language
Content: the surface language as the OCaml tool accepts it (functions, data, `let`/`if`/
`match`, arithmetic, the primitives), modes explained informally, one small program with its
inferred modes and its determinization, the "pre-sampled G tape" intuition
(`lean/mul-div-typing.md` §2). Verdict: good place to also state the *two invariants*
informally (G values never depend on E draws; E values are affine in E draws) because
Sections 4 to 6 all rest on them.
Exists: `tex/2_syntax.tex` (grammar only, incomplete: no `/`, `()`, poisson/exponential/
beta/gamma, `flip`), `det-lang` skill (accurate description of the implemented language).

### 3 Examples
Content: the running examples `examples/paper/ex1..ex6.det`, the positive/negative pairs
(`x·x` rejected, `1/x` rejected, `x < c` rejected, `uniform_G(0, x)` with `x : E` rejected),
the tracewise/global example `uniform_E(1,2) / uniform_G(0,1)` (`tex/figures/soundness_trace.tex`),
a validity failure (`uniform_E(x, 1/2)`), and one structured example (list fold or
recursion) showing that control flow and data are unrestricted when driven by G values.
Verdict: good. Two remarks. (a) Examples before the type system means the mode annotations
must be presented as *given* (or as produced by the tool); say so. (b) `det/funny.det`, the
fold example, is currently *unsound as written*: `uniform(x, gauss(acc, 2))` has an
off-domain lower bound with positive probability, the OCaml interpreter silently swaps the
bounds, and the `.dout` shows `program mean 0.9998` vs `determinized mean 0.84375`. It is a
perfect *validity* counterexample for this section, but must not be presented as a working
example.

### 4 Type System
Content: types, modes, the mode order, all typing rules, the primitive table (which
parameters may be E and the mean formula, reused by Sections 5 and 7), the determinization
transformation, and mode inference (constraint generation, greatest solution). Two theorems:
determinization preserves typing (mechanized: `typed_determinize`); inference computes a
valid and greatest annotation (not mechanized, to be proved on paper).
Verdict: the plan has no explicit home for *determinization* or for *inference*. Both must
precede Section 5 (the theorem is about `E(e)`), so put them here as subsections 4.3 and 4.4,
or make determinization its own short section. The section states the Lean rules for `×`
and `/` and the explicit `promote` (Part 3, D1 and D2), which the OCaml and the sim still
have to adopt.
Exists: `tex/3_typing.tex`, `tex/4_inference.tex` (with two placeholder examples),
`tex/5_determinization.tex` (two figures sharing one label; Poisson rows use `m_3` for `m_2`).

### 5 Semantics and the global main theorem
Content: the reducer (`next` / `sample(site, μ, k)` / `stuck`), the primitive fibers
(law at a stochastic site, Dirac at the mean at a mean site, zero off-domain,
`uniform(a,a) = δ_a`, `x/0 = 0`), the output law as a supremum over fuel of the
cumulative output measure, unnormalized; validity (`DoesNotGetStuck`) with its
counterexample; type safety (preservation, and "for typed closed programs stuckness is
exactly an off-domain primitive"). Then the global theorem in the extended-real form
(`extendedExpectationThm`) with Jensen (`jensenThm`) and the finite form (`mainThm`) as
its two faces, plus "determinization preserves validity" as part of the conclusion.
Verdict: correct that the global theorem is the weaker one; it is exactly what the Lean
derives from trace soundness. Present the semantics in the Lean's action style, not as a
kernel on a measurable space of expressions: it removes four TODO lemmas from the current
draft (measurable space, Markov kernel, `n`-step kernel, existence of the limit) and matches
the trusted base. The measurable-space construction becomes an appendix item.
Exists: `tex/6_soundness.tex` up to "Big-step semantics" (tables, monotone-mass lemma),
"Relating original and determinized" (extended reals, absolute moments, restriction/
expectation-of-bind lemmas, global theorems); Lean `Statement/Semantics.lean`,
`Statement/Primitives.lean`, `Statement/Main.lean`, `Proof/Typing.lean`.

### 6 Traces, the stronger main theorem, and its proof
Content: compact traces (list of G draws with their primitive), `jointMeasure`, `traceLaw`,
trace erasure (`correspondenceThm`); the theorem `Traces.soundnessThm`: the target is valid
and `MeanOnTraces`: the source joint law factors over the source trace law with a Markov
fiber, the target joint law is the pushforward of the *same* trace law along a measurable
output function, and almost surely the fiber is integrable with mean equal to the output.
Spell out the consequences in words: same trace law, same termination probability, no
integrability hypothesis. High-level proof following the Lean layers: symbolic expressions
and environments; symbolic reduction with actions `next`/`sampleE`/`sampleG`; the two
commutation lemmas (`symbolicReduce_realize`, `symbolicReduce_targetRealize`) and symbolic
preservation with the G-constancy side condition; integrability and the mean identity for
affine coordinates (`integrable_affine`, `integral_affine`, needing the first-moment bounds
of the six laws); validity transfer via convex domains; fiber soundness by induction on
depth (`exactDepth_fiberSound`); summation over depths and factorization. Then the theorem
"stronger implies weaker": integrate the fiber identity over the trace law
(`MeanOnTraces.finite_expectation`, `.extended_expectation`, `.lintegral_convex_le`).
Verdict: this is the right shape and matches the Lean exactly. The current draft's route
(distributions over symbolic states, `n`-step then limit, "σ integrable" assumed) should be
retired: it duplicates what Section 6 proves and carries a hidden hypothesis. Keep the
tracewise/global figure. The detailed-vs-compact trace argument (replay/decode) is one
paragraph here and an appendix item.
Exists: `tex/6_soundness.tex` symbolic semantics (types, `lift`, `realize`, `erealize`,
one commutation proof with the `if` case written out, TODOs for the expected side and for
mean-valuation agreement), tracewise theorems (the tracewise semantics "skipped");
Lean `Traces/`, `Proof/Symbolic*.lean`, `Proof/Trace*.lean`, `Proof/Compact*.lean`,
`Proof/Soundness.lean`, `Proof/Corollaries.lean`.

### 7 Implementations
7.1 Lean formalization: trusted base (`Statement/`, `Traces/`, `Theorems.lean`), axioms,
layering and sizes, deviations from the paper presentation and why, engineering notes
(measurability without a σ-algebra in the statements, s-finite kernels, detailed vs compact
traces). Exists: `lean/README.md` is a good draft of this subsection.

7.2 OCaml pipeline "output + certificate". Reading of the plan: for a `.det` program the
tool should produce (i) the mode-annotated program, (ii) the determinized program and its
evaluation, and (iii) a *certificate*: a Lean file that instantiates the trusted theorem
for this program, so that `lake env lean` on it checks, for this program, that the
determinized program's outputs are the conditional means. Concretely the certificate must
contain the program as a Lean `Expr` term, a `Typed [] p (.float m)` derivation, a proof of
`p.sourceForm = true` (by evaluation), a proof of `DoesNotGetStuck p` (see Part 2, OCaml),
and the instantiation `Theorems.traceSoundness m p ...` (and the expectation corollaries
under their hypotheses). `lean/Proof/Examples.lean` is a hand-written instance of exactly
this shape (`reciprocal`, `scaledSample`) and is the template. Exists: none of the emitter;
the OCaml AST is not the Lean `Expr` (named variables, subsumption, extra constructs,
different multiplication rule), so an elaboration pass is required first.

7.3 Storm / sampling / other estimators: the explicit-DTMC export (`ocaml/to_mc.ml`,
discrete programs only, `--limit` cut-off), the Monte Carlo evaluator (`ocaml/interp.ml`,
fixed 100 trials, seed 0, `observe` by rejection), the browser simulator with the coupled
G-tape runtime (`sim/`). Exists: all three, undocumented in the paper; no variance-reduced
or stratified estimators beyond plain sampling.

Verdict: fine. Say explicitly which parts are covered by the theorem (since 2026-09-11 the
core language including `bernoulli`, `discrete` over literal weights, `flip` as sugar and
`observe`; `-` and `<=` are sugar not yet stated as lemmas) and which are implementation
extensions (Monte Carlo evaluation, the Storm export, floating point).

### 8 Evaluation / benchmarking
Content: research questions (how often does inference find E sites; how much variance does
determinization remove at equal sample budget, i.e. the Rao–Blackwellization gain; agreement
with exact values from Storm or closed forms; cost of the pipeline and of certificate
checking), benchmark set (`examples/loops`, `examples/paper`, `examples/symbolic`, the
`examples/baselines/*.sgcl` programs which are Genfer/SGCL encodings and would need `.det`
translations, `clickGraph.det` exists), baselines (plain Monte Carlo, Storm exact where
discrete, possibly Genfer/PSI on the discrete or closed-form subset).
Exists: nothing: no harness, no trial/seed control, no CSV/plots, no reference values, no
timing. This section is the largest gap in the repository.

### 9 Related work
Content: prose organized by the three threads already listed in `tex/7_related_work.tex`
(generating functions / exact inference; moment analyses and expectation transformers;
hybrid exact/sampling such as delayed sampling, semi-symbolic inference, Rao–Blackwellized
samplers), positioning determinization as a type-directed, syntactic Rao–Blackwellization
along the E draws, with a certificate. Exists: a bullet list of citations, `\nocite{*}`.

### 10 Conclusion and future work
Content: `TODO.md` items (more distributions, subtraction/division, `discrete` branching),
symmetric multiplication rule, mechanized inference, conditioning, higher moments.
Exists: nothing.

### 11 Appendix
Content: full typing rules including surface subtyping and its elaboration to `promote`;
full reduction tables (the current two tables) and the symbolic reduction table;
measurability construction; detailed traces, replay and decoding; analytic facts about the
six distributions (means, first-moment bounds, domains); the retired global-first proof if
the authors want to keep it. Exists: most of this is in `6_soundness.tex` already, the
rest in Lean docstrings.

### Gaps in the plan itself
- No home for determinization and inference (put in 4).
- No home for the *validity* hypothesis and its counterexample (put in 5, referenced in 3).
- Section 5 should also state type safety, otherwise "valid = no off-domain primitive" has
  no justification.
- Decide whether Section 6's proof is "high-level" only with the appendix carrying the
  written cases, or whether the paper relies on the Lean for the cases. Either is fine; the
  current draft's fully written monadic calculations (the `if` case, tracewise agreement)
  are appendix material either way.

---

## 2. Pending changes per component

### 2.1 Paper (`tex/`)

- **P0 Restructure files to the plan.** New section files `1_intro`, `2_overview`,
  `3_examples`, `4_types` (merging `3_typing`, `4_inference`, `5_determinization`),
  `5_semantics`, `6_traces`, `7_implementation`, `8_evaluation`, `9_related`,
  `10_conclusion`, `A_*` appendices; update `main.tex`; delete `8_old.tex` (dead) and the
  unused `fig_symbolic_coupling.svg`, or include the latter in Section 7.3.
- **P0 Write Sections 1, 2, 3, 7, 8, 9, 10** (currently absent or placeholders).
- **P0 Section 5: replace the kernel presentation by the action-based reducer and
  fuel-indexed output measure** (`Statement/Semantics.lean`); add the fiber table
  (off-domain zero, `uniform(a,a)`, `x/0 = 0`); define `DoesNotGetStuck` with the E-draw
  counterexample; state preservation and the stuckness characterization; state the three
  global theorems in the Lean's form (validity of the target is part of the conclusion).
- **P0 Section 6: write trace soundness as the main theorem** and the high-level proof
  along the Lean layers; write "stronger implies weaker" with its three one-paragraph proofs;
  retire the global-first route (or move it to the appendix). Add trace erasure and the
  same-trace-law / same-termination-probability remarks.
- **P0 Section 4: adopt the Lean typing rules (D1)**: `Mul` with `e₁ : Float[G]`,
  `e₂ : Float[m]`, `Div` with a `Float[G]` denominator, replacing `Mul-G`, `Mul-ConstL`,
  `Mul-ConstR` and the literal `Div`; explicit `promote` with a sentence saying that the cast
  could be made implicit by an elaboration pass (D2); mode labels on sample sites (D3);
  complete the grammar (the eight primitives including `bernoulli` and `discrete` over literal
  weights, `flip` as sugar, `observe`, `/`, unit), and fix the known errors: duplicate
  `\label{fig:determinization}`, `m_3` vs `m_2` in the Poisson rows, dangling
  `\Cref{thm:pointwise-agreement-of-interpretations}`, `Sym` rule missing the G-constancy
  side condition, the stray binary `−` in the symbolic grammar.
- **P0 Sections 5 and 6: state the new theorems.** Section 5 gets `outputMassThm` (equal
  output mass, hence equal termination and acceptance probability), `varianceThm` (second
  moment and variance do not increase) and, once `observe` is in the grammar, the
  conditional-expectation corollary that justifies rejection sampling on the determinized
  program. Section 6 gets the law of total variance along traces (`Traces.varianceThm`:
  source variance = target variance + mean fiber variance, so determinization discards
  exactly the fiber variance, trace by trace), stated for every trace factorization. Section 5
  must also say that output laws are unnormalized, that `observe(false)` contributes no mass,
  and that the variance statements are read on the mass-normalized laws (same mass and same
  mean make the two readings agree).
- **P1 Complete the inference examples** ("With lists", `exponential(uniform*uniform)` are
  empty) and add the inference soundness/greatest-solution theorem with a proof.
- **P1 Reuse one primitive table** (E-allowed parameters, mean, domain) in Sections 4, 5, 7.
- **P1 Bibliography:** remove `\nocite{*}`, cite from the text, add Genfer/SGCL, Storm,
  Mathlib, delayed sampling, Rao–Blackwellization references; fix front matter
  (authors, `acmYear{2025}`, `acmArticle{TODO}`).
- **P2** Move the fully written monadic proofs (`lem:symbolic-step` `if` case, tracewise
  agreement, restriction/expectation of bind, increasing limits) to the appendix.

### 2.2 Lean (`lean/`)

- **Done 2026-09-11.** D1 to D4 needed no Lean change. Variance and mass theorems
  (`outputMassThm`, `varianceThm`, `Traces.varianceThm`, commit 04963c0), the discrete
  primitives `bernoulli`, `discrete`, `flip` (commit 72ea1d1) and `observe` with
  `conditionalExpectationThm` (Part 3 D6) are in the trusted surface, exported in
  `Theorems.lean` with standard axioms only, and described in `lean/README.md`. `MeanOnTraces`
  is now `∃ fiber output, TraceFactorization …`, so the variance decomposition can quantify
  over every factorization.
- **P1 Certificate interface for the OCaml emitter** (stage 1 of
  `pipeline-in-lean-analysis.md` §6, about one week, leaf-only, no `Statement/` change):
  `Determinize/Certificate.lean` with a Bool typing checker `check : List Ty → Expr → Ty →
  Bool` and its soundness lemma (so `Typed` is discharged by `decide`; `native_decide` is
  excluded because it adds an axiom), a `decide`-able `sourceForm`, the public `safe_*`
  lemma library for `DoesNotGetStuck` (the private helpers in `Proof/Examples.lean` are the
  seed; they must cover all eight primitives, `let`, `if`, application, `observe`), and a
  wrapper theorem `certify (p q) (typed) (src) (det : p.determinize = q) : DoesNotGetStuck p →
  (mainThm, extendedExpectationThm, jensenThm, varianceThm conclusions and MeanOnTraces p q)`,
  so that a certificate is one `example` per program. `p.determinize = q` closes by `rfl`
  today; `decide` would need a decidable literal type (`Rat` or mantissa/exponent).
- **P1 Extended-real Jensen** is stated for real-valued `φ`; the paper draft has
  `φ : ℝ → [0,∞]`. Either generalize the Lean or state the real-valued version in the paper
  (recommended: paper follows Lean).
- **P2 Subtraction and `<=`** as syntactic sugar lemmas (`a − b = a + (−b)`, `a ≤ b = ¬(b < a)`)
  so that the implementation's operators are covered without new rules, the way `flip` is.
- **P2 `discrete` over case expressions.** The OCaml AST allows `discrete` over arbitrary
  expressions (`(float * expr) list`); the parser only produces the index form, which is what
  the Lean formalizes. A mixture of E-typed expressions is sound by linearity but is not an
  affine function of the E draws, so it would need a new symbolic rule; keep it out unless
  the surface syntax grows.
- **P2** `lake build --wfail` and the axiom check are green today (nine theorems); keep the CI
  gate (`.claude/scripts/check.sh lean`) and add the certificate check to it once it exists.

### 2.3 OCaml (`ocaml/`)

- **P0 Interpreter semantics must match the theorem's semantics.** `interp.ml` swaps
  uniform bounds (`lo = min a b`), clamps `rand_u01` away from zero, and does not fail on
  off-domain parameters (negative variance, non-positive rates). Under the paper's semantics
  those runs are *stuck* and contribute no output. Make off-domain draws raise (like
  `ObserveFailure`) and report the stuck fraction in the `.dout`; this is what makes
  `det/funny.det` a validity failure instead of a silent wrong answer.
- **P0 Typing rules per D1/D2.** `infer.ml` currently has the literal-based `Mul-*`, literal
  `Div` plus non-literal `G/G`; replace them by the Lean rules (`Mul`: left operand G, right
  operand at the result mode; `Div`: denominator G), with the deterministic inference choice
  that the left factor is the G one. Add an elaboration output with explicit `promote` (D2)
  so that the certificate can be emitted. Two further mismatches found while formalizing D5:
  `infer.ml` types the probability of `flip` at a fresh mode variable (the Lean requires G,
  since a Boolean feeds control flow; an E probability would be replaced by its mean inside a
  `flip`, which is unsound), and `discrete`'s weights are normalized by their total in
  `interp.ml` but summed unnormalized in `determinize.ml` (the Lean requires the weights to
  sum to one; either reject other weight lists or normalize in both places).
- **P1 `observe` semantics.** The interpreter's rejection sampling is the mass-normalized
  reading of the Lean semantics (`observe(false)` contributes no mass) and is now justified
  by `conditionalExpectationThm`; report the rejection fraction in the `.dout` and make the
  Storm export treat rejection as an absorbing zero-reward state so that both sides agree.
- **P1 Certificate emitter (`--certificate`).** New module (e.g. `to_lean.ml`): convert the
  elaborated typed AST to the Lean `Expr` (de Bruijn indices, `promote` at every G-to-E
  coercion, mode label and `Kind.stochastic` on every sample site, reject or desugar
  `Sub`/`Leq`, map `flip` to `Expr.flip` and `bernoulli`/`discrete`/`observe` to their
  constructors), rely on the Lean checker for typing, print the `sourceForm` and
  `DoesNotGetStuck` obligations, and the final `example : MeanOnTraces p p.determinize`.
  Also emit the determinized program as a Lean term and check `p.determinize = q` by
  `rfl`, so the certificate ties the OCaml output to the Lean transformation. Per the D7
  study, emit all programs into one generated `lean/Determinize/Generated/Certificates.lean`
  listed in `Determinize.lean` (stage 2 of `pipeline-in-lean-analysis.md` §6, about 1.5
  weeks), checked by `check.sh lean`; a per-program `lake env lean` check costs about 4 s
  warm and is an acceptable `./certify.sh FILE.det` for interactive use.
- **P1 Evaluation controls for Section 8.** `--trials N`, `--seed S`, per-trial values (or
  mean, standard error, and a confidence interval) in the `.dout`, wall-clock timing,
  stuck/rejected counts, and a machine-readable output (CSV or JSON) next to the report.
  Keep the current deterministic 100-trial report for the golden tests.
- **P1 Reference values.** For discrete programs use `--storm` (fix the stale `.lab` files
  noted in the `storm` skill; regenerate); for continuous programs with closed-form
  expectations add an `expected:` annotation convention in the `.det` comment header that
  the harness reads.
- **P2** `symbolic_coupling.ml` (standalone prototype, not compiled) either becomes the
  coupled-trace runtime in OCaml for Section 7.3 or is deleted.
- **P2** `to_mc.ml` state keying via `Marshal` and the `--limit` semantics should be
  described in Section 7.3; consider exporting rewards for the determinized program too so
  that Storm checks both sides.

### 2.4 Simulator (`sim/`)

- **P0 Typing rule per D1.** `sim/src/compiler/infer.js` has `left : m, right : G`; the
  Lean rule is the mirror image (`left : G, right : m`), so the sim's G factor moves to the
  left, and the division rule (`right : G`) is already the Lean's. Run `sync-sim` and
  `spec-impl-checker` after the OCaml change; rebuild `app.bundle.js`, bump `?v=`.
- **P1 Off-domain draws** in `sim/src/runtime/distributions.js` must match the OCaml/Lean
  convention (stuck, reported), same as 2.3; `observe(false)` is a rejected trial, and
  `flip`'s probability must be forced to G as in the Lean.
- **P1 Section 7.3 material:** a short description of the coupled G-tape runtime
  (`runtime/semantics.js`, `affine.js`) and one screenshot or trace rendering
  (`traceRender.js`) for the paper; make sure `examples.js` contains the Section 3 examples.
- **P2** The discrete primitives are now in the theory (D5); the sim already runs them, so
  nothing to do beyond the rule alignment and the `discrete` weight normalization of 2.3.

### 2.5 Example programs (`det/`, `examples/`)

- **P0 `det/funny.det`:** either rewrite it to be valid (e.g. `uniform(x, x + gauss(...)²)`
  is not expressible; use `gauss(acc, 2)` as an additive term instead of a bound) or keep it
  explicitly as the validity counterexample with a comment; regenerate its `.dout` after the
  interpreter change.
- **P1 Section 3 examples as files:** the negative examples (`x·x`, `1/x`, `x < c`,
  `uniform_G(0, x)`) should exist as `.det` files whose `.dout` shows the type error; the
  tracewise/global example `uniform(1,2)/uniform(0,1)` as a file; `examples/paper/ex1..6`
  need `.dout` outputs (none are checked in) and their header comments must match the Lean
  typing rules of D1 (they currently annotate literal-based multiplication); add one
  `observe` example and one `bernoulli_E`/`discrete_E` example that the theory now covers.
- **P1 Benchmark set for Section 8:** `.det` translations of `examples/baselines/*.sgcl`
  (`clickGraph.det` exists, `clinicalTrial` does not), expected values where known, a
  manifest (`examples/benchmarks.txt` or JSON) listing program, category, reference value,
  reference source.
- **P1 Housekeeping:** `examples/simple.det` is missing but `simple.det.dout/.tra/.lab/
  .state.rew` exist; `examples/loops/example.det` has no output; regenerate all outputs with
  `./det.sh` after the interpreter change and commit them together.

### 2.6 Tooling, CI, Nix (`flake-modules/`, `.claude/`, scripts)

- **P1** `check.sh`: add areas `cert` (emit + check certificates for `det/*.det`) and
  `bench` (smoke-run the harness on one program); extend the Stop hook mapping.
- **P1** Storm: not in nixpkgs; document the Docker fallback in the harness and make the
  harness skip Storm-based references gracefully when `storm` is absent.
- **P2** A `bench.sh` (or `nix run .#bench`) that produces the CSV and the plots
  (matplotlib or gnuplot in a small `.#bench` devshell) consumed by `tex/8_evaluation.tex`
  via `\input` of generated tables, so numbers in the paper are never typed by hand.
- **P2** `TODO.md`: replace the three open items by the P0/P1 items of this document
  (the file is the task list; items here are not yet in it).

### 2.7 Documentation

- **P1** `lean/README.md`: keep as the source of Section 7.1; add the certificate
  interface once it exists.
- **P1** `CLAUDE.md` "Change semantics in four places" and the known-disagreement note must
  be updated when the OCaml and the sim adopt the Lean rules (D1); the `det-lang` skill's
  "Constructs that force G" line must then say: the left factor of `*`, the denominator of
  `/`, comparison operands, `flip`'s probability, `gauss` variance, `exponential` rate,
  `gamma` rate, `beta` parameters; and its transform line must list `observe` as kept.
- **P2** A top-level `README.md` (none exists) describing the artifact layout for reviewers
  (paper artifact evaluation will need it).

---

## 3. Cross-cutting decisions (taken 2026-09-11)

- **D1 Multiplication and division rules.** Decided: the Lean rules. `e₁ × e₂ : Float[m]`
  requires `e₁ : Float[G]` and `e₂ : Float[m]`; `e₁ / e₂ : Float[m]` requires `e₁ : Float[m]`
  and `e₂ : Float[G]`. There is no implicit `G → E` cast, so a general-mode factor on the
  right of `×` has to be promoted explicitly. The symmetric rule of `lean/mul-div-typing.md`
  §3 stays a remark (the paper may mention it as a possible extension); `<=` stays outside
  the core language. Still to do: paper (2.1), OCaml (2.3), sim (2.4), examples (2.5),
  docs (2.7). Lean: nothing.
- **D2 Subsumption vs explicit `promote`.** Decided: explicit `promote` as mechanized; the
  paper adds a sentence that the coercion could be made implicit by an elaboration pass
  inserting `promote` at every `G`-to-`E` position. Lean: nothing.
- **D3 Where mode annotations live.** Decided: on sample sites only; determinization is a
  function on untyped annotated terms, as in the Lean. Lean: nothing.
- **D4 Validity and degenerate cases.** Decided: as in the Lean (`DoesNotGetStuck` with E
  draws included, zero measure off-domain at stochastic and mean sites, `uniform(a,a) = δ_a`,
  `x/0 = 0`). Lean: nothing; both interpreters still have to follow (2.3 P0, 2.4 P1).
- **D5 Discrete primitives in the theory.** Decided: add them. Done in Lean (commit
  72ea1d1): `bernoulli mode kind p` with the probability at the site's mode, `discrete mode
  kind weights` over literal weights (the parser's only form), `flip p` as the sugar
  `0 < bernoulli_G(p)`. Answer to "do they get determinized": `bernoulli_E(p)` becomes its
  mean `p` and `discrete_E(p₁…pₙ)` becomes `∑ i·pᵢ`, both covered by every theorem;
  `flip` is Boolean, has no mean, feeds control flow, and is therefore never determinized
  and needs a general-mode probability (`ocaml/infer.ml` currently types it at a fresh mode
  variable, which is unsound and must change). A mixture over case expressions (the OCaml
  AST's general `discrete`) is not formalized.
- **D6 `observe`.** Decided: add it so that the theorems keep working. Done in Lean (see
  Part 0): no sub-distribution machinery was needed, because the output laws were already
  unnormalized sub-probability measures and `MeanOnTraces` already says that the target has
  the same trace law and therefore the same mass. `observe(c)` needs `c : bool`, and every
  Boolean is general-mode information (comparisons take G operands, `promote` only goes
  `G → E`), so source and target reject exactly the same G traces; `observe(false)`
  contributes no output and no trace mass and is not stuckness. The new
  `conditionalExpectationThm` says that the mass-normalized expectations agree, which is what
  justifies rejection sampling on the determinized program. What would break: an `observe`
  on an E-dependent condition (conditioning does not commute with replacing a draw by its
  mean), which the type system rules out. Proof-internal detail worth knowing for the
  paper's appendix: the one-step kernel of `Proof/Internal/Semantics.lean` sends a rejection
  to the sink value `unit`, which never yields a real output, so that the kernel of a valid
  program stays a probability kernel and the existing "not stuck iff no mass lost"
  characterization survives; the public definitions send `reject` to the zero measure.
- **D7 Certificate granularity and a pipeline in Lean.** Decided: study only. The study
  (`pipeline-in-lean-analysis.md`) recommends the Lean+OCaml split now, staged: (1) a
  leaf-only `Certificate.lean` with a decidable typing checker and a wrapper theorem; (2) an
  OCaml emitter writing all benchmarks into one generated `Certificates.lean` checked by
  `lake build --wfail` (already a real certificate, conditional on validity, enough for the
  paper's Section 7); (3) validity automation; (4) optionally a Mathlib-free literal-polymorphic
  core and a `lake exe determinize` so the OCaml emitter leaves the trust base. Rocq with
  certified extraction is not recommended (library gaps, no reuse of the 16K-line proof,
  12 to 24 person-months). `native_decide` must not be used in certificates (it adds an axiom).

---

## 4. Suggested order of work

1. Lean: the certificate interface (2.2 P1, D7 stage 1); keep `lake build --wfail` green.
2. OCaml: interpreter validity (2.3 P0), typing alignment with the Lean rules including
   `flip` and `discrete` (2.3 P0), elaboration with `promote`, then the emitter and the
   generated certificate file (D7 stage 2); regenerate `det/` outputs; sync the sim (2.4).
3. Examples: fix `funny.det`, add the Section 3 files and the `observe`/`bernoulli`/`discrete`
   examples, build the benchmark manifest (2.5).
4. Harness and reference values (2.3 P1, 2.6); produce the tables for Section 8.
5. Paper: restructure files, write Sections 4 to 6 from the Lean statements first (they are
   stable and now include mass, variance, total variance and conditional expectation), then
   2, 3, 7, 8, then 1, 9, 10; run `paper-reviewer` and `spec-impl-checker` before each round.
6. Record the open items of Part 2 in `TODO.md` (2.6 P2) and update `CLAUDE.md` when the
   OCaml and the sim have adopted D1 (2.7).
