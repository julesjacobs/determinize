# Paper plan review and pending changes across the repository

Date: 2026-09-10. Supersedes the earlier paper-vs-Lean comparison (git history has it).
Inputs: `tex/`, `lean/` (statements, proofs, `README.md`, `mul-div-typing.md`), `ocaml/`,
`sim/`, `det/`, `examples/`, `run.sh`, `det.sh`, `.claude/skills/{det-lang,storm}`, `TODO.md`.

Part 1 checks the intended paper structure section by section and says what each section
must contain and what already exists for it. Part 2 lists, per component, every change that
is still pending for the repository to match the plan. Part 3 lists the cross-cutting
decisions that block several of those changes. Part 4 is a suggested order of work.

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
or make determinization its own short section. The section must also settle, in one place,
the rules that currently differ between artifacts (see Part 3, D1 and D2).
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

Verdict: fine. Say explicitly which parts are covered by the theorem (the core language
without `flip`/`bernoulli`/`discrete`/`observe`/`-`/`<=`) and which are implementation
extensions.

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
- **P0 Section 4: align the typing rules with the decision in Part 3 D1** (`Mul`, `Div`),
  present `promote` vs subsumption per D2, put mode labels on sample sites per D3, complete
  the grammar (all six primitives, `/`, unit), and fix the known errors: duplicate
  `\label{fig:determinization}`, `m_3` vs `m_2` in the Poisson rows, dangling
  `\Cref{thm:pointwise-agreement-of-interpretations}`, `Sym` rule missing the G-constancy
  side condition, the stray binary `−` in the symbolic grammar.
- **P1 Complete the inference examples** ("With lists", `exponential(uniform*uniform)` are
  empty) and add the inference soundness/greatest-solution theorem with a proof.
- **P1 Reuse one primitive table** (E-allowed parameters, mean, domain) in Sections 4, 5, 7.
- **P1 Bibliography:** remove `\nocite{*}`, cite from the text, add Genfer/SGCL, Storm,
  Mathlib, delayed sampling, Rao–Blackwellization references; fix front matter
  (authors, `acmYear{2025}`, `acmArticle{TODO}`).
- **P2** Move the fully written monadic proofs (`lem:symbolic-step` `if` case, tracewise
  agreement, restriction/expectation of bind, increasing limits) to the appendix.

### 2.2 Lean (`lean/`)

- **P0 Decide and implement the multiplication rule (D1).** If the symmetric rule is
  adopted, restore the mirrored `Typed.mulLeftG`/`WellTyped.mulGE` cases (commit `f9cc30d`
  had them) or add a general `[Mul]` with a mode side condition; the note says
  `Affine.mul?` already accepts a constant factor on either side. If an asymmetric rule is
  adopted, no change.
- **P1 Certificate interface for the OCaml emitter.** Add a small public module (say
  `Determinize/Certificate.lean`) with: a `decide`-able `sourceForm` lemma helper; tactic
  or lemma library for `DoesNotGetStuck` on programs whose primitive parameters are
  literals in-domain or otherwise syntactically safe (the private `safe_next`,
  `safe_sample`, `safe_real`, `safe_let_uniform` helpers in `Proof/Examples.lean` are the
  seed; they must become public and cover all six primitives, `let`, `if`, application);
  a wrapper theorem taking `(p : Expr) (typed : Typed [] p (.float m)) (src : p.sourceForm
  = true) (safe : DoesNotGetStuck p)` and returning the three expectation statements and
  `MeanOnTraces`, so that a certificate is one `example` per program.
- **P1 Decidable typing.** A `Typed` derivation is currently built by hand
  (`Proof/Examples.lean`). Either make `Typed` decidable for closed terms (a checker
  function with a soundness lemma, `decide`/`native_decide`), or have the OCaml emitter
  print the derivation term. The checker is the smaller certificate and the better
  engineering; it also gives the paper's "inference produces a valid typing" a checkable
  form.
- **P1 Extended-real Jensen** is stated for real-valued `φ`; the paper draft has
  `φ : ℝ → [0,∞]`. Either generalize the Lean or state the real-valued version in the paper
  (recommended: paper follows Lean).
- **P2 Discrete primitives** (`flip`, `bernoulli`, `discrete`) if D5 says they enter the
  theory: new `Op` constructors with Bernoulli/finite-support laws, means `p` and
  `Σ pᵢ·i`, `affineArity` positions for `p`, plus the moment bounds. `observe` would need a
  different theorem (conditioning) and is out of scope unless D6 says otherwise.
- **P2 Subtraction and `<=`** as syntactic sugar lemmas (`a − b = a + (−b)`, `a ≤ b = ¬(b < a)`)
  so that the implementation's operators are covered without new rules.
- **P2** `lake build --wfail` and the axiom check are green today; keep the CI gate
  (`.claude/scripts/check.sh lean`) and add the certificate check to it once it exists.

### 2.3 OCaml (`ocaml/`)

- **P0 Interpreter semantics must match the theorem's semantics.** `interp.ml` swaps
  uniform bounds (`lo = min a b`), clamps `rand_u01` away from zero, and does not fail on
  off-domain parameters (negative variance, non-positive rates). Under the paper's semantics
  those runs are *stuck* and contribute no output. Make off-domain draws raise (like
  `ObserveFailure`) and report the stuck fraction in the `.dout`; this is what makes
  `det/funny.det` a validity failure instead of a silent wrong answer.
- **P0 Typing rules per D1/D2.** `infer.ml` currently has the literal-based `Mul-*`, literal
  `Div` plus non-literal `G/G`; align with the decision. Add an elaboration output with
  explicit `promote` (D2) so that the certificate can be emitted.
- **P1 Certificate emitter (`--certificate`).** New module (e.g. `to_lean.ml`): convert the
  elaborated typed AST to the Lean `Expr` (de Bruijn indices, `promote` at every G-to-E
  coercion, mode label and `Kind.stochastic` on every sample site, reject or desugar
  `Sub`/`Leq`, reject `Flip`/`Bernoulli`/`Discrete`/`Observe` until D5/D6), print the
  typing derivation or rely on the Lean checker, print the `sourceForm` and
  `DoesNotGetStuck` obligations, and the final `example : MeanOnTraces p p.determinize`.
  Also emit the determinized program as a Lean term and check `p.determinize = q` by
  `rfl`/`decide`, so the certificate ties the OCaml output to the Lean transformation.
  Add a `./certify.sh FILE.det` that runs the emitter and `lake env lean` on the result,
  and a golden test in `det/`.
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

- **P0 Typing rule per D1.** `sim/src/compiler/infer.js` has `left : m, right : G`; align.
  Run `sync-sim` and `spec-impl-checker` after the OCaml change; rebuild `app.bundle.js`,
  bump `?v=`.
- **P1 Off-domain draws** in `sim/src/runtime/distributions.js` must match the OCaml/Lean
  convention (stuck, reported), same as 2.3.
- **P1 Section 7.3 material:** a short description of the coupled G-tape runtime
  (`runtime/semantics.js`, `affine.js`) and one screenshot or trace rendering
  (`traceRender.js`) for the paper; make sure `examples.js` contains the Section 3 examples.
- **P2** If discrete primitives enter the theory (D5), the sim already runs them; nothing
  to do beyond the rule alignment.

### 2.5 Example programs (`det/`, `examples/`)

- **P0 `det/funny.det`:** either rewrite it to be valid (e.g. `uniform(x, x + gauss(...)²)`
  is not expressible; use `gauss(acc, 2)` as an additive term instead of a bound) or keep it
  explicitly as the validity counterexample with a comment; regenerate its `.dout` after the
  interpreter change.
- **P1 Section 3 examples as files:** the negative examples (`x·x`, `1/x`, `x < c`,
  `uniform_G(0, x)`) should exist as `.det` files whose `.dout` shows the type error; the
  tracewise/global example `uniform(1,2)/uniform(0,1)` as a file; `examples/paper/ex1..6`
  need `.dout` outputs (none are checked in) and their header comments must match the final
  typing rules (they currently annotate literal-based multiplication).
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
  be updated when D1 is implemented; the `det-lang` skill's "Constructs that force G" line
  must follow the new multiplication rule.
- **P2** A top-level `README.md` (none exists) describing the artifact layout for reviewers
  (paper artifact evaluation will need it).

---

## 3. Cross-cutting decisions (each blocks items above)

- **D1 Multiplication and division rules.** Paper/OCaml: literal-based `Mul-ConstL/R`,
  literal `Div`; sim: `left : m, right : G`; Lean: `left : G, right : m`, G denominator.
  `lean/mul-div-typing.md` derives that the maximal sound rule is symmetric (one G factor,
  any G denominator) and recommends adopting it in all four artifacts (Option A), with a
  deterministic tie-break in inference. Blocks 2.1, 2.2, 2.3, 2.4, 2.5, 2.7.
- **D2 Subsumption vs explicit `promote`.** Recommended: core calculus with `promote` (as
  mechanized) in Sections 4 to 6, surface subtyping as an elaboration performed by
  inference and described in Section 4/appendix, stated as not mechanized. Blocks the
  certificate emitter and the Section 4 rules.
- **D3 Where mode annotations live.** Recommended: on sample sites only (Lean), which is
  what inference outputs; the determinization table then reads as a definition on annotated
  terms. Blocks Section 4 and the emitter.
- **D4 Validity and degenerate cases.** Adopt `DoesNotGetStuck` (E draws included), zero
  measure off-domain for stochastic and mean sites, `uniform(a,a) = δ_a`, `x/0 = 0`, in the
  paper and in both interpreters. Blocks 2.3 P0, 2.4 P1, 2.5.
- **D5 Discrete primitives in the theory.** `flip`/`bernoulli`/`discrete` are cheap to add
  to the Lean (finite-support laws, affine means) and would let Storm-checked programs be
  certified; without them Section 7.3's Storm export only applies to programs the theorem
  does not cover once continuous E draws are determinized away. Recommended: add
  `bernoulli` (float-valued, mean `p`, `p` may be E) and `flip` (boolean, `p` must be G);
  `discrete` with literal probabilities is a finite mixture and also fits.
- **D6 `observe`.** Out of the theorem; the interpreter's rejection sampling on the
  determinized program is *not* justified by the theory (conditioning does not commute
  with replacing draws by means). Either exclude `observe` from the evaluation and say so,
  or make it future work with a precise statement of what breaks.
- **D7 Certificate granularity.** Per-program Lean file checked by `lake env lean`
  (simple, slow: Mathlib import per check) vs a single generated `Certificates.lean`
  built with the project (fast, one `lake build`). Recommended: one generated file under
  `lean/Determinize/Generated/`, listed in `Determinize.lean`, so `lake build --wfail`
  checks all certificates and the axiom report covers them.

---

## 4. Suggested order of work

1. Take decisions D1 to D7 (one meeting; record them in `TODO.md` and `CLAUDE.md`).
2. Lean: implement D1 and the certificate interface (2.2 P0/P1); keep `lake build --wfail`
   green.
3. OCaml: interpreter validity (2.3 P0), typing alignment, elaboration with `promote`,
   then the emitter and `certify.sh`; regenerate `det/` outputs; sync the sim (2.4).
4. Examples: fix `funny.det`, add the Section 3 files, build the benchmark manifest (2.5).
5. Harness and reference values (2.3 P1, 2.6); produce the tables for Section 8.
6. Paper: restructure files, write Sections 4 to 6 from the Lean statements first (they are
   stable), then 2, 3, 7, 8, then 1, 9, 10; run `paper-reviewer` and `spec-impl-checker`
   before each round.
