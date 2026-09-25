# Review of Section 6 (Implementation), second round

Reviewed on 25 September 2026 against commit `90394b1`. Line numbers refer to
`tex/sections/6_implementation.tex` at that commit; figure and theorem numbers refer to
the compiled PDF. I checked every statement about the code in this review against the Lean
sources and tools in this repository.

## How to read this review

It is written the way a journal referee would write it, but with more explanation than a
referee usually gives. Each finding says what the paper claims, what the code actually does,
why the difference matters to a reader, and what I suggest. The findings are ordered by
importance. At the end, "Claims I checked and found accurate" lists what you do not need to
worry about.

## Overall assessment

The first round's small problems are fixed. The section no longer says something false about
Lean's proof checking, it states the hypotheses of the soundness theorems, it describes
determinization correctly, and it cites Storm.

The larger problems remain, and a closer look at the code turned up new ones. In short:

1. The section still does not say precisely which code must be trusted, which is proved and
   which is merely checked, and line 9 contradicts how the code is organized.
2. It calls several components "verified" without saying what was proved about them.
3. It never explains how the pieces combine into a guarantee about the user's program, or
   that two hypotheses of that guarantee are never checked by the tool.
4. Its description of the Storm route is inaccurate.
5. **New:** the verified exact solver handles only 256 states by default and stores its
   equations as a full matrix, while Table 1 reports models with up to 43,219 states. Section 7
   also says that the timing runs skipped the certificate check. Taken together, the paper does
   not yet show that its central claim, exact results backed by a certificate that Lean has
   checked, holds for the benchmarks it reports.
6. The finite-state route, the additive reward mode and the floating-point interpreter each
   get one or two sentences, although Section 7 depends on their details.
7. The section reads as a caption for Figure 12 rather than as a section of a journal paper.

My recommendation for this section is still a major revision. None of these problems lies in
the underlying work, which is stronger than the text suggests. They are problems of precision
and completeness.

## Major findings

### 1. What must be trusted is still unclear, and line 9 contradicts the code

**What the paper says.** Lines 8–11 divide the code into a "trusted surface/specifications"
and "Lean-checked proofs and procedures". Line 9 says the trusted surface holds the syntax, the
typing rules, the semantics, determinization and the theorem statements, and "additionally ...
the non-verified functions like the parser or floating-point interpreter." Line 17 repeats that
the parser is "part of the trusted surface".

**What the code does.** The trusted part of the code is the directory `lean/Determinize/Spec`.
It holds the definitions line 9 lists, but neither the parser (`Frontend/Parser.lean`) nor the
floating-point interpreter (`Runtime/`). No theorem depends on the interpreter at all.

In the other direction, `Spec` holds things line 9 does not mention:

- what a finite model and its certificates mean (`Spec/FiniteModel/`, `Spec/RewardModel/`);
- the statement about inference (`Spec/Inference.lean`), which names the inference function
  but does not require reading its body.

These statements in turn rely on Mathlib's definitions, such as the Gaussian distribution,
conditional distributions and variance, and a reader has to accept those as well.

**Why it matters.** The split exists to tell a skeptical reader exactly which code they must
read to believe the results. This matters all the more because the section discloses that the
code was mostly written by language models. The current text merges three different kinds of
code into one category:

- **Specification.** A human must read it and agree with it, because a theorem means only as
  much as the definitions it is about. `Spec` is this kind.
- **Unverified code whose output carries no guarantee.** The floating-point interpreter is this
  kind. No theorem relies on it, and its output is simply an estimate.
- **Untrusted code whose output is checked.** Storm and its Python wrapper are this kind. They
  could compute a wrong answer, but the checker would then reject it.

The parser does not fit neatly. It is unverified, yet any claim about the program *text* the
user wrote depends on it, because the theorems speak about the parsed program.

The section also leaves out one more thing a reader must trust: the Lean compiler. When the
command-line tool runs the verified inference or the verified solver, it runs compiled code.
The proofs are about the Lean definitions, and the compiler is trusted to execute those
definitions faithfully. The tool can also write its result as a certificate file (`.result.lean`
or `.storm.lean`). Lean's kernel checks such a file with `decide +kernel`, so a result obtained
that way depends only on the kernel, not on the compiler. That distinction works in your favor
and deserves a sentence.

**The figure has the same problem.** Figure 12 draws "Determinize" and "Measure-theoretic
semantics" in the "Verified in Lean" style. Both are definitions, part of the specification;
nothing about them is verified. The theorems *about* them are what is verified. A legend with
four styles (specification, proved, checked, unverified) would make the figure accurate.

**Line 6 disagrees with line 17 and the figure.** Line 6 says that "only the parser, the
floating-point interpreter and the external Storm solver are unverified". Line 17 and the
figure speak of parsing *and elaboration*. The pretty-printer, the command-line driver and the
Python wrapper around Storm are unverified as well. The wrapper's output is checked, so this
weakens no guarantee, but the sentence as written is inaccurate.

**Suggestion.** Replace the two-item list with a short paragraph or table that says:

- what must be read: `Spec`, about 1,300 lines, plus the Mathlib definitions it uses;
- what must be trusted without reading: Lean's kernel and its three standard axioms, and the
  Lean compiler for results the tool computes directly rather than writes as certificates;
- what is untrusted but checked: Storm and its wrapper;
- what is unverified: the parser, the elaborator and the interpreter.

Then change the figure's legend to match.

### 2. The section calls components "verified" but never says what was proved

Four places need the actual guarantee.

**Inference (line 19).** The paper says: "The code is verified to produce a correct typing as
well as mode annotation, and the program is furthermore shown to assign as many E-sites as
possible." Theorem 3.1 (`inferCorrectThm`) says more, and the section should cite it:

- The result is a *completion* of the input. It fills in exactly the annotations the user left
  out, and it keeps every constructor, literal, variable and explicit annotation unchanged
  (`Input.matches`). Inference therefore cannot silently change the program.
- The result is well-typed at the type inference returns.
- It is the *greatest* completion. Every other valid way of filling in the annotations is at or
  below it at every sampling site, in the order G ≤ E. "As many E-sites as possible" undersells
  this: "greatest" means best at every site at once, not merely best in total count.
- Inference fails only when no completion exists.

It is also worth saying that nothing is checked at run time. The `compile` function stores the
proofs of typing and input preservation inside the compiled program, in the `aligned` and
`typed` fields of `Program`, which it obtains from `Theorems.inferenceCorrectness`. The guarantee
therefore holds by construction and costs nothing when the tool runs.

**Determinization (line 21).** The paper says the determinized program is well-typed. Lean
proves this (`typed_determinize` in `Proof/Semantics/Ordinary.lean`), but the paper neither
says so nor links the proof.

**Finite models and the solver (lines 29–31, and the figure).** The figure marks graph
construction and the solver as verified, and the text speaks of "our own verified matrix
solver". What is actually proved should appear as a numbered theorem with a Lean link, roughly:

> If the checker accepts a certificate for the finite model built from a program, then the
> program is domain-safe, its output distribution has finite mass and a finite second moment,
> and its probability of returning, first moment and second moment are exactly the rational
> numbers in the certificate. When the probability of returning is positive, the mean and
> variance conditioned on returning follow.

In the code, `Model.Matches` (`Spec/FiniteModel/Model.lean`) states what it means for a model to
represent a program exactly: the program is domain-safe, and the model's output distribution
equals the program's. `checked_statistics` and `checked_conditionalVariance`
(`Checking/Statistics.lean`) turn an accepted certificate into the statement above.

This has a consequence the paper could use and currently does not. A matching model implies
domain safety, and finite models always have finite moments. So exploring the *source*
program with the tool's `--subject source` option proves the source domain-safe and gives its
exact statistics. For source programs that are finite-state to begin with, the tool therefore
discharges the soundness theorems' hypotheses itself. For the more interesting case, a source
with continuous E draws that becomes finite only after determinization, it does not; see
finding 3.

**The certificate checker (line 35).** See finding 4.

### 3. The path from the user's program to a guarantee is never explained

A reader needs to see how the pieces fit together. In plain words:

1. The program text is parsed and elaborated. This step is unverified. The result is an
   `Input`: a program with rational literals, in which the annotations the user omitted are
   placeholders.
2. Inference fills the placeholders, with the guarantee of Theorem 3.1.
3. The annotated program is converted to a "core" program with rational literals, and the tool
   determinizes that.
4. The soundness theorems are stated about programs with *real* literals. The function
   `interpret` maps rationals to reals, and the lemma `interpret_determinize`
   (`Proof/FiniteModel/Initial.lean`) shows that converting to reals and determinizing can be
   done in either order. The theorems therefore apply to the program the tool actually runs.

The whole pipeline pays off in one step: "the exact answer we computed for e_det is also the
answer for e_src." That step is Theorem 4.1, and it needs two hypotheses: the source program is
domain-safe, and its expectation is defined (or finite, for the finite version of the theorem).
Typing establishes neither, and the tool checks neither. The introduction and Section 7 say
this, but Section 6, which describes the pipeline, does not. Figure 12 also has no arrow for
this transfer, although it is the point of the pipeline.

**Suggestion.** Add a short paragraph, "What the pipeline guarantees", that walks through the
four steps above and states the two hypotheses. Add an arrow in the figure from the exact result
for e_det back to e_src, labelled with those hypotheses.

### 4. The Storm description is inaccurate (line 35)

**What the paper says.** "The certificate that Storm produces is checked and it is verified that
any such certificate corresponds to the correct expected value for the input program."

**What happens.**

1. The Lean tool explores the program and writes out its finite model.
2. `tools/storm.py` loads the model into Storm through Storm's Python interface as a Markov
   chain with exact rational probabilities. For each quantity it asks Storm for the expected
   total reward collected before reaching a terminal state (the property `R=? [ F "done" ]`).
   The quantities are the probability of returning, the positive and negative parts of the
   output (their difference is the first moment), the square of the output (the second moment)
   and the probability of rejection. Storm returns one exact rational number per state for
   each.
3. The wrapper then computes two things in Python. It finds the "dead" states, which can never
   reach a terminal state. For every other state it records a path towards a terminal, as a rank
   and a next state.
4. It writes all of this into a Lean file as a `MomentCertificate`, with one small theorem per
   state, and runs Lean on the file. The kernel checks three things: that the dead states really
   form a region the program cannot leave, that the recorded paths are valid, and that Storm's
   numbers satisfy the model's linear equations.

The paths deserve a sentence in the paper, because they are what makes the checker sound. When
some states can loop forever, the linear equations alone can have many solutions. The paths
show that from every state that is not dead, the program reaches a terminal state with positive
probability. That makes the solution unique, so numbers that satisfy the equations must be the
true values.

**So, concretely:**

- Storm does not produce a certificate. The wrapper assembles one from Storm's numbers plus a
  witness it computes itself.
- The certificate covers more than "the correct expected value": it gives the probability of
  returning, the first and second moments and the probability of rejection. The mean and
  variance conditioned on returning follow from these.
- "For the input program" is imprecise. The certificate is for whichever program was explored,
  the source or the determinized one. The tool explores the determinized program by default.

**A simpler story is available.** The built-in solver produces exactly the same kind of
certificate: `solveStatistics` returns a `MomentCertificate` together with its proof of validity.
So the section could say "two solvers, one kind of certificate, one checker", which is easier to
follow than the current two unrelated paragraphs.

### 5. The verified solver cannot handle the reported benchmarks, and the certificates were not all checked (new)

**The built-in solver is small by default.** `SolveLimits.maxStates` in `Finite/Solve.lean`
defaults to 256 states; the option `--max-result-states` raises it. The solver stores the linear
system as a full n × n matrix of rational numbers and solves it by Gaussian elimination
(`Proof/LinearAlgebra/Solve.lean`). Its running time therefore grows roughly with the cube of the
number of states and its memory with the square, and the rational numbers can also grow large
along the way.

Table 1 reports models with 43,219 states (`dreckon`), 27,836 (`workload`) and 15,192 (`pack`).
Those results can only have come from Storm. The paper should say that the built-in solver is
meant for small models and Storm for large ones. Otherwise a reader will assume that the
verified solver produced the table.

**Section 7 says the certificates were not all checked.** It states: "The timing runs generate
certificates but skip independent kernel checking; they do not establish certificate coverage
for all table entries." For the reported benchmarks, then, the one route that is verified end
to end (Storm followed by a certificate checked by the kernel) may never have been completed.
Checking a certificate for tens of thousands of states, with one kernel `decide` per state, may
also be slow, and the paper gives no timing.

**Why it matters.** Exact results with a Lean-checked certificate are the central selling point
of the implementation. A reviewer will ask whether the certificates for Table 1 were checked, and
how long checking took.

**Suggestion.** Run the kernel check for every entry of Table 1, report its time next to
Storm's, and say in Section 6 how certificate checking scales with the number of states.

### 6. The finite-state route is described too briefly

Section 7.1 rests on this route, yet the paper gives it two sentences. It should explain:

- **What a "machine state" is.** `Finite/Machine.lean` defines an abstract machine whose state
  is an expression to evaluate, an environment of values and a stack of pending work (a
  CEK-style machine), with exact rational numbers and closures. Executions that reach the same
  machine state are merged. A program is finite-state when exploration reaches only finitely many
  machine states.
- **Which programs qualify.** Only Bernoulli and discrete draws may remain random. Mean sites of
  any distribution are fine, because they are deterministic. A remaining continuous draw, or a
  Poisson draw, which has infinitely many outcomes, stops exploration with an "unsupported"
  error. This is the precise condition under which determinization "enables exact analysis",
  and the paper never states it.
- **The limits.** By default, exploration stops at 10,000 states, 100,000 edges or 1 MB per
  state. The options `--max-states`, `--max-edges` and `--max-state-bytes` change these limits.
- **Which program is explored.** Either the source or the determinized program (the
  `--subject` option); the default is the determinized one.
- **What is computed.** The output value is collected once, when the program returns.
  Rejection is an absorbing state. States that can loop forever are identified as dead. The
  reported mean and variance are conditioned on returning.
- **Additive mode.** With `--additive` (specified in `Spec/RewardModel/`), the tool handles
  additions that are waiting for the rest of the computation and whose left operand is already a
  number, as in `x + loop …`. It moves such numbers out of the machine state and into a reward on
  the transition. A loop that accumulates a sum this way, such as the geometric-addition example
  in the README, then becomes finite-state. Other shapes, such as `f() + 1` or a multiplication
  around a recursive call, are not handled (`lean/finite-model-contract.md` gives the exact
  rule). Section 7 relies on this mode (it calls `coin_flip_unif` an "additive-extraction
  control case"), but the paper never introduces it.

### 7. The floating-point interpreter is described too briefly

Section 7.2 measures variances with this interpreter, so a reader needs these details:

- **Random numbers.** The generator is SplitMix64. Gaussians come from the Box–Muller method,
  gamma draws from the Marsaglia–Tsang method (which gives up after 100,000 attempts), and
  Poisson draws from multiplying uniform numbers in chunks of rate 20 (rates above one million
  are rejected).
- **Fuel.** Each run stops after 100,000 steps by default and reports "step limit reached". This
  is what Section 7 means by "fuel-bounded executions".
- **Failures.** A non-finite arithmetic result, such as an overflow, aborts the run. Rejection is
  reported separately from failure.
- **Literals.** Rational literals are converted to floating-point numbers before running.
- **Separate random streams for G and E draws.** The interpreter keeps two generator states,
  `gSeed` and `eSeed` in `Runtime/Eval.lean`. With the same seed, the source program and the
  determinized program therefore see the same G draws. This is a good design, since it pairs up
  the two runs, and it is worth mentioning. Section 7.2 should then say whether it runs both
  programs with the same seeds, because paired runs change how differences between their
  estimates should be read.
- **Testing.** The interpreter is unverified, so say how it is tested. The repository has
  statistical tests against analytical expectations (`tests/statistical/`).

### 8. "Every theorem of the previous sections is mechanized" needs qualifying (line 6)

For Sections 3 and 4 this is true, and each theorem links to its Lean statement. Section 5 is
different. Its lemmas and theorems are proved in Lean, but in another form, and none of them is
linked.

- Lemma 5.1 ("Realization of initialization") is `realize_ofExpr`.
- Theorems 5.4 and 5.5 (the two interpretations of initialized symbolic execution) appear only
  as intermediate steps inside the proof of `compact_exactDepth_source_fiberSound`. They are also
  stated about traces that record every reduction step, not about the paper's traces, which
  record only G draws.
- Theorem 5.6 ("Agreement of interpretations") is proved as an invariant called `FiberSound`. It
  describes the source's output given a trace by an explicit replay function rather than by
  conditional distributions. The link to conditional distributions is made once, at the very
  end (`Proof/Traces/ConditionalLaw.lean`).
- Theorem 5.6 assumes an "integrable" symbolic configuration. In Lean this is not an assumption
  but a lemma: every primitive distribution has finite moments (`primitiveMomentBounds`).

**Suggestion.** Either narrow the claim ("every theorem of Sections 3 and 4 is stated and proved
in Lean, and the proof of Section 5 is mechanized in a closely related form") or bring
Section 5 in line with the Lean proof and add links.

### 9. Facts a journal paper is expected to report are missing

- **Size.** `Spec` has 18 files and 1,348 lines. The proofs span 103 files and 25,456 lines, with
  about 990 theorem and lemma declarations. The executable parts are the front end (1,141
  lines), finite exploration and solving (1,030), certificate checking (164) and the interpreter
  (267). `Theorems.lean` exports 17 theorems. A reviewer will appreciate the ratio: about 1,300
  lines to read against 25,000 lines checked by machine.
- **Versions.** Lean 4.33.1 and Mathlib at commit `0df444a`.
- **Build time.** Not reported. Measure it once and add it.
- **Availability.** The introduction still says "[TODO]" where the link to the code should be.
- **Design decisions.** Several interesting ones are documented in `lean/README.md` but not in
  the paper:
  - the semantics needs no measurable structure on expressions;
  - the output distribution is a sum over exact termination depths;
  - invalid parameters give the zero measure, so a program that gets stuck loses probability
    mass instead of producing output;
  - mean sites still check their parameters' domains;
  - rejection is a step that loops back to itself.
- **The disclosure about language models (line 6).** As a bare remark it invites suspicion.
  Paired with a precise account of what must be trusted and how large it is (finding 1), and a
  sentence on how the specification was reviewed and by whom, it becomes a strength and even a
  methodological point. Otherwise, move it to the acknowledgments.

### 10. Structure

The section is still a sequence of one- or two-sentence paragraphs that walk through Figure 12.
Two structural problems stand out:

- The semantics (line 25) comes after the soundness theorems (line 23), although the theorems are
  stated about the semantics.
- Line 29 ends with "in two ways:" and is followed by two `\paragraph` headings rather than a
  list.

A structure that would hold the material above:

1. Overview and trust model (finding 1, and the size figures from finding 9)
2. From source text to a verified program (findings 2 and 3)
3. Formalizing the semantics and theorems (the design decisions from finding 9, and finding 8)
4. Exact analysis of finite-state programs (findings 4, 5 and 6)
5. Sampling (finding 7)

## Minor comments, line by line

- **Line 2.** `\clearpage` at the start of the section is a drafting leftover.
- **Line 6.**
  - Name the version: "Lean 4.33.1".
  - "In order to make it clear which parts of the pipeline need human review and which ones can
    be left to the Lean kernel" can be shortened to "To make clear which parts need human review
    and which can be left to Lean's kernel".
- **Line 9.** "Trusted surface/specifications" gives two names for one thing. Choose one; the
  code calls it `Spec`.
- **Line 10.** "Procedures" is vague. "Verified executable functions" says what is meant.
- **Line 13.** "We will briefly describe" can be "We describe".
- **Line 17.**
  - "Source code" should be "source program".
  - The paper never introduces the surface language, although its examples use `fun`,
    `rec f x =>`, `flip` and subtraction, and its prose mentions `observe`. This is the place
    to say what desugaring does, for example that `flip(p)` becomes `0 < bernoulli[G](p)` and
    `a - b` becomes `a + -b`.
- **Line 19.**
  - See finding 2.
  - "Mode annotation" is a third name for what the paper elsewhere calls E/G annotations.
  - "The user has a well-typed program e_src": the inferred type may be `real^G`, while the
    soundness theorems require `real^E`. Subtyping bridges the two; say so.
- **Line 21.** Cite `typed_determinize` for "well-typed".
- **Line 25.**
  - "Small-step and big-step semantics": Figure 6 calls these "deterministic reduction" and
    "output measures", and "big-step" appears nowhere else in the paper.
  - The Lean definition also has no evaluation contexts: its step function finds the next
    reducible expression directly.
  - Move the paragraph before the soundness paragraph.
- **Line 27.** "In order to be able to run the program" can be "To run programs".
- **Line 29.**
  - "Finite state graph" should be "finite-state graph".
  - Turn "allows one to continue with it in two ways:" and the two headings that follow into a
    list or subsections.
- **Line 31.** Say that the mean and variance are conditioned on returning, and that the solver
  is meant for small models (finding 5).
- **Line 33.** Say that Storm computes with exact rational arithmetic, and which property it is
  asked to compute (finding 4).
- **Line 35.** See finding 4.
- **Figure 12.**
  - The legend needs more styles (finding 1), and the figure needs the transfer arrow
    (finding 3).
  - The caption is one line, while the `\Description` holds the useful explanation; move some of
    it into the caption.
  - The "Soundness theorems" box links only to expectation preservation.
  - "Exact mean and variance" should say "conditioned on returning".
- **Terminology.** The paper says "mode" and the Lean code says "affinity". Readers who follow
  the Lean links will meet `Affinity`, so state once that the two are the same.

## A problem outside the text: the Lean links in the review build

`main.tex` builds in review and anonymous mode. In that mode the Lean links point to a snapshot
on apndx.org, not to GitHub. That snapshot predates the verified inference: it has no
`Spec/Inference.lean` (the page returns 404). Since commit `90394b1`, the links' line numbers
refer to the current code. Until the snapshot is regenerated from commit `29ee1e7`, most Lean
links in the review PDF therefore land on the wrong lines. The GitHub links used by the
camera-ready build are correct.

## Claims I checked and found accurate

- **Line 6:** the formalization does use Mathlib's Giry monad (the output measure is built with
  `Measure.bind`), its conditional distributions (`condKernel`), and its Gaussian, exponential,
  beta, gamma and Poisson distributions.
- **Line 10:** the build check is exactly as described. The last command of `Theorems.lean`
  fails the build unless every proposition in `Spec` is proved using only `propext`,
  `Classical.choice` and `Quot.sound`.
- **Line 21:** the tool runs the same `Expr.determinize` definition that the theorems are about.
  It is generic in the type of literals, and the tool uses it with rationals.
- **Line 23:** the stated hypotheses match Theorems 4.1, 4.2 and 4.4.
- **Line 25:** the semantics uses the Giry monad and is marked noncomputable in Lean.
- **Line 27:** the interpreter is unverified, and no theorem depends on it.
- **Line 31:** the built-in solver computes with exact rationals and returns its certificate
  together with a proof that the certificate is valid.
- **Section 7's tables:** the static sampling-site counts in both tables match what the tool
  reports for all 24 benchmarks.
