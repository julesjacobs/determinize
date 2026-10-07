# Review of Section 6 (Implementation), third round

Reviewed on 5 October 2026 against `main` at `96eefdd`. Line numbers refer to
`tex/sections/6_implementation.tex` at that commit. Figure, table and theorem numbers refer
to the PDF built from it: Section 6 is pp. 19–21 and the architecture figure is Fig. 13. I
checked every statement about the code against the sources at `96eefdd`. Under `lean/`,
these are identical to `8f89190`, the revision the paper's Lean links pin. Where I ran the
tool, the program and its output are quoted.

## How to read this review

The section is reviewed as one section of an OOPSLA submission, so two constraints apply.

**Page budget.** OOPSLA 2027 allows "at most 23 pages using the template. This page limit
does not include required statements, references, or supplementary material (such as
appendices). However, papers must be self-contained; reviewers are under no obligation to read
the supplementary material" [1]. Round 1 closes on 14 October 2026.

The body now ends on p. 25, while the abstract is a placeholder, the introduction is still
bullet points and RQ3 is unfinished. The paper is therefore over the limit and will grow.
Section 6 cannot grow: it takes about 2 pages now (including Fig. 13), and 1.5 should be the
target. Because the appendix is supplementary, only detail that the claims do not depend on
can move there. Each suggestion carries its cost:

- **[+]** adds main-text space;
- **[0]** is about neutral;
- **[−]** saves space;
- **[A]** goes to the appendix or the artifact.

Where a complete fix is too long, I say what moves out of the main text.

**Reviewers in 2026.** An OOPSLA PC member who reads "mostly carried out by Claude … and
GPT" is likely to ask three questions:

1. Are the statements the right ones?
2. Did a human check them?
3. What exactly does the tool guarantee about my program?

The section should answer all three explicitly. At present it answers the third only in part
(line 23 gives the theorems' hypotheses but not what the tool's results mean) and leaves the
first two to the reader's inference.

Findings are ordered by how likely they are to change a PC decision. Each states what the
paper says, what the code does, why it matters, and one or more ways to fix it.

## Verdict

Major revision.

**What changed since round 2.** The section changed in one place: the description of the build
check (line 10). The new text is mostly accurate but overclaims in one clause (M4). The rest of
the paper moved on:

- Section 3 now states inference correctness exactly (Thm. 3.1).
- Section 4 states the theorems for expectations conditioned on returning.
- Section 7 reports exact results computed with Storm alone.

Section 6 reflects little of this. Line 23 matches Section 4's hypotheses, but line 19 still
paraphrases inference in weaker terms, and nothing in Section 6 corresponds to Section 7's route.

**Three problems are the most likely to cost the paper.** M1 and M3 need work; M2 is a
sentence or two that changes how a reviewer reads everything else.

- **M1.** Nothing in the repository shows that the certificates behind Table 1 were checked.
  The committed results come from a related run in which they were generated but not checked,
  and the run behind the table is not committed.
- **M2.** The paper does not say that the specification was reviewed, by whom, or against
  what. Next to the disclosure of LLM authorship, a skeptical reader will assume it was not.
- **M3.** The exact result is certified for `e_det`, and the tool labels it so. Section 6
  states the theorems' hypotheses (line 23) but never connects them to the exact result, nor
  says that the tool does not check them. Two concrete programs below show that the result
  can fail to carry over to `e_src`.

Everything else concerns precision, space and presentation. The underlying work is stronger
than the section suggests: the build check, the embedded inference proofs and the certificate
design are all good, and the section undersells them. One example: the explored Markov chain
is replayed against the semantics, while the closest prior work leaves the construction of its
model unverified [8].

**If only the time until 14 October is left,** in this order:

1. M1-A: check the certificates and commit the results Table 1 is built from.
2. M3-A: state the guarantee and the transfer hypotheses.
3. M4-A and M5-B: one table for the trust model and the architecture.
4. M2-A: say in the paper who reviewed the specification and against which parts of it.
5. M6-A: add the missing links and narrow the claim.
6. Regenerate the anonymous snapshot and add the Data-Availability Statement.

Everything else can wait for the revision round, which may go up to 25 pages [1].

## Status of the round-2 findings

| # | Round-2 finding | Status | Where now |
|---|---|---|---|
| 1 | Trust split; line 9 contradicts the code | Open. Line 10 is new and overclaims | M4 |
| 2 | "Verified" without saying what was proved | Partly fixed. Thm. 3.1 now states it; line 19 still paraphrases a weaker claim; the finite route still has no stated theorem | M4, minor L19 |
| 3 | No path from program to guarantee | Partly fixed. The introduction now names the source hypotheses; Section 6 and Fig. 13 do not | M3 |
| 4 | Storm description inaccurate | Open; line 35 is unchanged | M7 |
| 5 | Solver too small; certificates unchecked | Open. Section 7 no longer says the timing runs skipped the check, and says nothing instead | M1 |
| 6 | Finite route described too briefly | Open | M7, M8 |
| 7 | Interpreter described too briefly | Open | M8, cross-section |
| 8 | "Every theorem … is mechanized" | Open. Section 5 was rewritten; Thm. 5.5 no longer assumes integrability | M6 |
| 9 | Facts a journal paper reports | Open; numbers updated below | M8 |
| 10 | Structure | Open | M8 |
| — | Lean links in the review build | Open, and worse | Cross-section |

**Obsolete round-2 statements.** These no longer describe the code:

- the "every proposition in `Spec` is proved" mechanism. `Spec/Main.lean` is gone; the statements
  now live in `Theorems.lean`, and a dependency walk checks them (M4);
- the size of `Spec`, which is now 16 files and 1,240 lines;
- 17 exported theorems, which are now 19;
- the names `inferCorrectThm`/`inferenceCorrectness`, now `inference_correctness`;
- `primitiveMomentBounds`, now `primitiveMomentBounds_primitiveLaws`;
- the theorem numbers. Expectation is now Thm. 4.6, variance Thm. 4.7 and tracewise soundness
  Thm. 4.5.

## Major findings

### M1. Nothing on record shows that Table 1's certificates were checked

**What the paper says.** Section 6 presents two exact routes: "our own verified matrix solver"
(line 31) and Storm followed by a certificate checker (lines 33–35). The introduction lists
"verified matrix solving" as a contribution. Section 7 (line 23) computes Table 1 with Storm
and gives "Storm itself takes at most 2.23 seconds" (line 34). The caption of Table 1 says:
"Runtime excludes both the initial build and Lean certificate checking."

**What the repository shows.**

- **The verified solver produced no reported number.** It stops at 256 states by default
  (`SolveLimits.maxStates`, `Finite/Solve.lean:7-8`). It is dense Gaussian elimination over
  `Fin n → Fin n → Rat` (`Proof/LinearAlgebra/Solve.lean:29-49`), so it needs n² memory and
  n³ rational operations, with coefficient growth on top. Table 1 goes up to 43,219 states.
- **No checked certificate is on record.** `tools/bench.py:288-289` passes
  `--skip-certificate` unless `--check-certificates` is given. The committed
  `results/finite-after-det.json` has `"check_certificates": false`, and every completed entry
  is "generated, unchecked". Neither Section 6 nor Section 7 says whether any certificate
  behind Table 1 was checked. The certificates may well have been checked in another run; the
  point is that neither the paper nor the repository shows it.
- **The committed results come from a different run than the table.** Most state counts agree,
  but not all:
  - In the JSON, `dreckon` fails because model export times out at 180 s. The table reports it
    at 43,219 states and 178.001 s, 2 s under the timeout.
  - `retransmit` has 205 states in the JSON and 182 in the table.
  - The times for `addNoise` differ as well.
- **Fig. 13 links a checker the evaluation did not use.** Every Table 1 run uses `--additive`
  (`tools/bench.py:284`). That route checks a `Reward.Solution` with whole-model
  `decide +kernel` obligations (`tools/storm_additive.py:71-88`). Fig. 13 and line 35 point to
  the non-additive `checkStatistics` (`Checking/Statistics.lean:9-17`).
- **"Storm" in Table 1 is more than Storm.** The column times the worker subprocess
  (`tools/bench.py:327`, `tools/storm.py:299`): starting Python, importing stormpy and building
  the model, as well as Storm's computation. Section 7's "Storm itself takes at most 2.23
  seconds" should say so. Certificate generation happens after the worker and is not included.
- **The exact values are printed as decimals.** Table 1 shows `E[sum]=6.0` and `E[x]=2.0` for
  values the tool computes as exact rationals.

**Why it matters.** A reader takes Section 6 together with Table 1 to mean "exact and certified".
If the artifact evaluators run `bench.py` with its defaults, they get unchecked certificates,
and at least two rows that differ from the table. A mismatch like that can cost the paper at
artifact evaluation, and after publication it is hard to fix.

**Ways to fix it.**

- **A [0]. Minimum; do it in any case.**
  - Run `bench.py --check-certificates` for every row.
  - Commit the results file the table is generated from, and generate the table from it with a
    script.
  - Report the time of kernel checking, as a column or as one sentence ("checking all
    certificates took X s; at most Y s per benchmark").
  - Print exact values as rationals.
  - If some certificate does not check in reasonable time, mark that row and confine the claim
    "certified" to the rest.
  - Lean 4's kernel computes `Nat` arithmetic on literals with "a widely-trusted, efficient
    arbitrary-precision integer library (usually GMP)" [4], so whole-model `decide +kernel`
    checks may be fast enough. Measure it rather than guess. For comparison, the verified
    Isabelle checker of [8] "completes within a few seconds on MDPs with up to ≈10^5 states".
- **B [−]. One route instead of two.** Drop "verified matrix solver" from Fig. 13 and from the
  contributions. Present one pipeline: an untrusted solver (Storm with exact rationals, or the
  small built-in solver) produces a certificate, and Lean's kernel checks it. This is the
  certifying-algorithm pattern [7]. In CompCert's words, "the combination of a verified
  validator … with an unverified compiler … does provide formal guarantees as strong as those
  provided by a verified compiler" [6]. It is easier to defend and shorter to explain. The
  built-in solver can stay as a dependency-free fallback that already returns its certificate
  together with a proof.
- **C [+]. Make the built-in solver scale.** Options include SCC decomposition, sparse
  elimination, or the state elimination used by parametric model checkers. This would be worth
  doing only if the paper wants a Storm-free result. I would not do it before the deadline.

### M2. The section does not say that the specification was reviewed

**What the paper says.** Line 6 says that the "final implementation was mostly carried out by" three
named LLMs, and that the code was split "to make it clear which parts of the pipeline need
human review". Line 9 says that the "trusted surface" "requires the most scrutiny".

A charitable reader may infer from this that the authors reviewed the specification. The text
does not say so, and a skeptical reader has three reasons not to infer it:

- **The wording addresses the reader.** "Need human review" and "requires the most scrutiny"
  say what a reader should check, not what the authors did.
- **The LLM sentence covers the specification too.** "The final implementation" does not
  exclude `Spec/`. Without LLMs, authors who wrote the definitions could be assumed to
  understand them; with LLM authorship disclosed, that assumption no longer holds, and
  reviewers know it [10].
- **The section describes the specification inaccurately.** It places the parser and the
  floating-point interpreter in the trusted surface, which `Spec/` does not contain (M4). A
  reviewer who notices this may doubt that the trusted part was read closely, although the
  error is only in the prose.

**What the repository shows.**

- The review of the specification is recorded nowhere a reader can see. The paper does not
  mention it. The only trace in the repository, `tex/FIGURE_PLAN.md:20` ("- [ ] Author review
  of the mathematical definitions and statements."), still lists it as open. Tick that item
  so that the repository agrees with the paper.
- **What a reader must trust:**
  - the statements in `Theorems.lean`: 19 theorems, 10 of them the paper's;
  - `Spec/`: 16 files. Together with `Theorems.lean` that is 1,535 lines;
  - the Mathlib definitions those use: the Gaussian, beta, gamma and Poisson measures,
    `Measure.condKernel` and `variance`;
  - the type of `Frontend.infer`.

  The build reports "Their statements rely on 454 declarations other than proofs, none from
  Proof" (`Theorems.lean:262`). That is not the reading list: it counts only `Determinize`
  declarations, none from Mathlib, and it includes the body of `infer` and its helpers, which
  need not be read. The doc-gen4 pages are the practical way to walk the actual list.
- **Mitigations exist but the paper does not mention them:**
  - the doc-gen4 API documentation, in which every name in a statement links to its
    definition;
  - `lean/README.md`'s list of known differences between the paper and Lean: the surface
    syntax, `discrete` dropping its last argument, and the appendix's `d_{Det(e)} = d_e`, which
    no theorem states.

**Why it matters.** Kernel checking makes the *proofs* trustworthy whoever wrote them. It
says nothing about whether the *statements and definitions* are the intended ones. Lean's
own guide makes the distinction ("does the theorem have a valid proof" versus "what does the
theorem statement mean"), and it lists "un-reviewed AI-generated proofs and programs" among
the cases that need care [4].

This gap is a documented failure mode of LLM-generated verification. An ICFP 2026
experience report with Claude Code-written Rocq proofs found "the LLM silently modifying proof
statements to make them easier to prove … if that statement has been weakened, the guarantee
may be vacuous" [10]. A study of formal benchmarks finds "vacuous theorems, and unsound axioms"
that the kernel cannot detect [11].

A disclosure with no account of review invites a PC member to discount the mechanization.
Even a sympathetic reviewer may ask "who read `Spec`?" Artifact evaluation
asks the same question under the name "Fidelity: Do the mechanized definitions and theorems
correspond precisely to those in the paper?" [3].

**Ways to fix it.** These can be combined.

- **A [0 to +0.1 page]. Report the review.**
  - One or two sentences: who read `Spec/` and the statements in `Theorems.lean` (1,535
    lines), against which parts of the paper (Figs. 6–8, Defs. 2.1, 4.1 and 4.3), and what
    changed as a result.
  - Name the revision that was reviewed, or say that the artifact is that revision. A reader
    can then tell that the reviewed specification is the one the artifact proves things about.
  - A possible wording, if it is accurate:

    > We reviewed every definition in `Spec/` and every statement in `Theorems.lean` line by
    > line against Sections 2–4; the language models wrote the proofs and the executable code.

    If the models also drafted parts of `Spec/`, say so and keep the first clause. That makes
    the sentence about the review more important, not less.
  - **Placement of the disclosure.** ACM requires that AI use "must be fully disclosed in the
    Work" and suggests the acknowledgments [2]. OOPSLA's FAQ, however, advises suppressing
    acknowledgments "entirely until camera-ready" in double-blind submissions [1]. ACM's FAQ
    also allows disclosure "elsewhere in the Work prominently", at a level "commensurate with
    the proportion of new text or content generated" [2].

    So keep the disclosure in the body for submission. Section 6 is the right place, because
    it is the only place where the disclosure connects to the trust argument. Make it say what
    the models produced and what humans wrote or reviewed. At camera-ready, add a full
    statement in the acknowledgments.
  - Name the period in which the models were used. Model names alone date the paper and are
    hard to verify.
  - **If LLMs also drafted paper text,** the same policy applies. For whole sections, the FAQ
    asks to "disclose which sections and which tools and tool versions" [2]. OOPSLA adds two
    rules [1]:
    - "citations/references to non-existed work … are grounds for desk rejection. We run
      automated checks on references". Check every entry of `references.bib` by hand, the
      2025–2026 ones first.
    - Prose "so verbose and formulaic that reading it is not worth the effort, may be rejected
      for that reason alone".
- **B [A]. A correspondence table in the appendix.** Each paper object, its Lean name, and a
  link. `tex/lean-links.tex` already has 32 of the entries. The proof-artifact guidelines that
  SPLASH artifact evaluation links ask for exactly this "paper-to-artifact correspondence
  guide" [3].
- **C [0]. Tests of the specification in Lean, under the same build check.** The paper then needs
  one sentence: "N further theorems check the specification against examples."
  - *Non-vacuity:* a concrete program that is typed `real^E` and domain-safe, has `q>0` and a
    finite second moment, and whose variance strictly decreases. For example,
    `uniform[E](0,1)` goes from variance 1/12 to 0.
  - *Negative examples from the paper:*
    - `let x = uniform[E](0,1) in x*x` is not typable;
    - the non-domain-safe `uniform[E](0, gaussian[E](0,1))` changes `E_ret`, which shows the
      hypothesis is needed;
    - Example 4.2 has no expectation.
  - *Spot checks of the semantics:* `bigStepMeasure` of `uniform[G](0,1)` is Lebesgue measure on
    [0,1], and `bernoulli` and `discrete` give the right weights.

  These help answer "is the specification vacuous or wrong?" in the place reviewers will look,
  at no cost in pages. There is precedent: the Liquid Tensor Experiment kept a folder of examples
  meant to "form convincing evidence that we did not make a mistake in formalizing the
  necessary definitions" [12].
- **D [0]. Have the proofs re-checked independently.**
  - `lake env leanchecker --fresh` has been part of the toolchain since v4.28. It replays the
    environment through the kernel.
  - Lean's `comparator` goes further. It "ensures that the proved theorem statements match
    those in the trusted challenge file" and can use the independent Rust checker nanoda [4].

  `Theorems.lean` is already almost a challenge file. Split it into statements, which import
  only `Spec` and `Frontend.infer`, and proofs. Comparator would then also close the part of
  the elaboration gap of M4 that comes from `Proof` imports. It costs one sentence in the
  paper; the work is in CI.
- **E [+0.3 page, optional]. Make the method a contribution.** OOPSLA readers are interested in
  how to make LLM-written mechanizations trustworthy. Possible content:
  - the ratio of specification to proof (1.5k to 26k lines);
  - how the build check stops the specification drifting;
  - any attempts by the agents to weaken a statement that the check or a review caught.

  Do this only with honest data. Without data, A plus C is enough.

### M3. Section 6 never connects the certified result for `e_det` to `e_src`

**What the paper says.** Line 23 states the soundness theorems' hypotheses: `e_src` is typed
and domain-safe, and its expectation is defined. Lines 29–35 then describe "exact mean and
variance" (line 31; Fig. 13) without saying that these are numbers for `e_det`, that they are
`e_src`'s only under line 23's hypotheses, or that the tool checks neither hypothesis. The
introduction says the exact value "equals the source expectation under the source safety and
integrability hypotheses". Thm. 4.6 requires `e_src` to be domain-safe, `q_e>0` and
`E_ret[e_src]` to be well-defined.

**What the tool does.** By default it explores `e_det`. `Model.Matches` proves that the
*explored* program is domain-safe (`Spec/FiniteModel/Model.lean:67-68`). Nothing checks
either hypothesis for `e_src`. Two runs of the tool show the consequence:

1. **A domain-safe, typed source whose expectation is undefined:**

   ```
   ( rec f x => if flip(0.5) then x else f (2 * gaussian(x, 1)) ) 0
   ```

   The tool infers `gauss[E]` and explores 30 states. It prints "Expected terminal reward
   (kernel-checkable certificate generated) (…determinized): 0". Let N be the number of
   rounds, so P(N=n) = 2^{-(n+1)}. Then x_N given N=n is N(0, (4^{n+1}−4)/3), so
   E|X| = Σ 2^{-(n+1)}·Θ(2^n) = ∞. By symmetry E[X⁺] = E[X⁻] = ∞. This is the doubling
   pattern of Example 4.2, but here the determinized program is finite-state. The certified 0
   is the average over traces of the conditional means (Thm. 4.5). It is not an expectation of
   the source, which has none.
2. **A non-domain-safe source, the paper's own appendix example:**

   ```
   let u = gaussian(0, 1) in uniform(0, u)
   ```

   The exact route prints "… : 0" with a certificate. The true `E_ret` is 1/√(2π) ≈ 0.399,
   and the unnormalized first moment, which is what the CLI prints, is half of that.
   The tool's own sampling mode on the same file reports "Source: 10058/20000 runs returned a
   value … mean 0.395854 … first failure: uniform requires lower ≤ upper".

**More imprecision.** The exact result goes by four names:

- "Expected terminal reward" (CLI);
- "exact mean and variance" (line 31, Fig. 13);
- "exact mean and variance conditioned on returning" (`\Description` of Fig. 13);
- "expectation value" (introduction).

The sampling mode already prints "domain safety and integrability are not established by
typing". The exact route prints nothing of the kind, although its output looks more
authoritative.

The tool is honest about what it computed: the output names the determinized subject. The
problem is the paper. The introduction sells the number as the source's expectation, and
Section 6 says nothing about the conditions.

**Why it matters.** "Exact, kernel-checked" next to a number that is not the source's
expectation is a bad look for a verification paper. A reviewer who tries the appendix example
would find it in a minute.

**Ways to fix it.**

- **A [+3 lines]. Minimum.** Say in Section 6:
  - what is proved about the explored program: domain safety, and its exact return probability,
    mean and variance conditioned on returning;
  - that the result transfers to `e_src` under Thm. 4.6's hypotheses, which the tool does not
    check.

  Add the transfer arrow to Fig. 13 and label it with those hypotheses.
- **B [0, a Lean corollary]. A guarantee that needs only domain safety.** From Thm. 4.5 and
  Lemma 4.4:

  `E_ret[e_det] = (1/q) ∫ E[e_src | τ] dμ_tr(τ)`,

  whenever the right-hand side is defined. State this in `Theorems.lean` and in the paper.
  Then the certified number has a meaning even when `E[e_src]` is undefined, as in program 1,
  and integrability is needed only to call it "the expectation".
- **C [0, tool].**
  - Print the open hypotheses next to every exact result, as sampling mode already does.
  - When `--subject source` is used and the source is itself finite-state, both hypotheses are
    discharged: `Model.Matches` implies `DomainSafe`, and `finite_reward_integrability`
    supplies the finite moments. Say so in one sentence.
  - As a cheap smoke test, run the source through the interpreter and report domain failures,
    as the sampling mode already does. This is not a proof, but it would have caught program 2.
- **D [future work]. Static conditions.**
  - *Domain safety:* if every parameter with a restricted domain (the bounds of `uniform`, the
    variance of `gaussian`, and so on) had to be G, its value could not depend on E draws. Then
    the domain safety that exploration proves for `e_det` might carry over to `e_src`. This is
    a conjecture to check, and it costs E-sites. An interval analysis on the values that reach
    those parameters is an alternative.
  - *Integrability* is harder. Program 1 breaks it with a constant factor 2 inside recursion,
    so a sufficient condition must bound the growth of values against the probability of
    termination. The moment-bound analyses that Section 8 cites (`kura2019`, `wang2021central`)
    are candidates.

  Mention these as limitations if they are not done.

### M4. The trust model is still described wrongly, and the new sentence about the build check overclaims

**What the paper says.**

- Line 6 says "only the parser, the floating-point interpreter and the external Storm solver are
  unverified".
- Line 9 places "the parser or floating-point interpreter" in the "trusted surface".
- Line 10 says the build fails "unless its statement uses nothing from this part except proofs",
  where "this part" means "proofs and procedures".

**What the code does.**

- **Line 9.** The parser and the interpreter are in `Frontend/Parser.lean` and `Runtime/`, not in
  `Spec/`. `Spec/` imports only `Spec` and Mathlib, and no theorem depends on the interpreter.
  `lean/README.md` itself lists line 9 as a paper error.
- **Line 10.** The check (`Theorems.lean:261-295`) forbids only declarations from
  `Determinize.Proof.*`. `inference_correctness` mentions the procedure `Frontend.infer`
  (`Theorems.lean:44`). That is the right design, because the statement is *about* `infer`
  and its body need not be read. But the sentence as written is inaccurate.
- **What the check does not cover.** A Lean-literate reviewer will ask about these.
  - *The certificate route has no theorem on the reviewed surface.* Only theorems in the
    namespace `Determinize.Theorems` are checked. `checked_statistics` and
    `checked_conditionalVariance` (`Checking/Statistics.lean:21`, `:29`) are outside it, as are
    `typed_determinize` and `interpret_determinize`. The statement of `checked_statistics`
    mentions `MomentCertificate`, `checkStatistics` and `CheckedModel`, which are built from
    `Proof` definitions. So the correctness of the certificate checker, which Fig. 13 marks
    "Verified in Lean", is outside what the build check covers. `lean/README.md` points
    reviewers to the `Spec` files for finite models, but the theorem that connects them to a
    certificate is not in `Theorems.lean`. The two finite-model theorems that are in
    `Theorems.lean` relate no model or certificate to a program:
    - `finite_mass_balance` says that a model's return, rejection and divergence
      probabilities sum to 1;
    - `finite_reward_integrability` says that every reward model has finite moments.

    A single result does not depend on the general theorem, though. Each certificate file
    states its result for the program it was written for, and the kernel checks it:
    `outputStatistics` says `statistics.Matches (bigStepMeasure (checkedSubject.program
    checkedSource))`, with `Matches`, `bigStepMeasure` and `Subject.program` from `Spec/` and
    the program as the literal `checkedSource`. Only `statistics` is defined outside `Spec/`,
    as the initial state's entries of the certificate's vectors. On the Storm routes,
    `reportedStatistics` fixes its three fields to the printed rationals by `decide +kernel`.
    The built-in solver's certificates (`Finite/Export.lean`, `Finite/Reward/Export.lean`)
    have no such theorem, so relating the printed numbers to the theorem takes the compiled
    CLI or an evaluation of `statistics` by hand.
  - *Imports can change how statements elaborate.* Coercions, notations or instance priorities
    from an import leave no trace in the statements. `lean/README.md` says so; the paper
    should too, in half a sentence. M2-D removes the gap.
  - *The Lean compiler.* When the CLI computes a result directly (inference, exploration, the
    built-in solver), it runs compiled code, and the proofs are about the definitions. For
    native binaries, "the TCB is extended with the Lean compiler …, the Lean runtime … and the
    code generation backend" [5]. A certificate file checked with `decide +kernel` depends only
    on the kernel [4]. No `native_decide`, `implemented_by`, `@[extern]` or `sorry` occurs in
    `lean/` or `tools/`, so the repository adds no hand-written replacements of its own. Those
    of Lean's core library remain part of the compiler's trusted base [5]. That is worth one
    clause.
- **Line 6's list is incomplete.** These are unverified too:
  - the elaborator and desugarer, the pretty-printer and the CLI driver;
  - the certificate generators (`Finite/Export.lean`, `Finite/Reward/Export.lean`);
  - `tools/storm.py`, `tools/storm_additive.py` and `tools/bench.py`.

  Their outputs are checked, so no guarantee weakens, but "only" is inaccurate.

**Ways to fix it.**

- **A [−, recommended]. Replace lines 6–11 with a four-row table.** It is shorter than the
  current enumerate.

  | | What | Basis |
  |---|---|---|
  | Read | statements in `Theorems.lean` and definitions in `Spec/` (1,535 lines) and the Mathlib definitions they use | human review (M2) |
  | Trusted, not read | Lean's kernel and `propext`, `Classical.choice`, `Quot.sound`; the Lean compiler for results the CLI computes directly | standard |
  | Untrusted, checked | exploration (replayed against the semantics, `checkModel`), Storm and its wrappers (certificates checked by the kernel) | wrong output is rejected |
  | Unverified | parser and elaborator; floating-point interpreter | no guarantee; tested statistically (`tests/statistical/`) |

  Comparable papers do the same:
  - Zar puts "the specifications of cwp … and equidistribution" explicitly in its TCB [13].
  - SampCert reports the size of its trusted additions ("only 57 lines of C++") [14].

  Follow the table with one exact sentence on the check:

  > The last command of `Theorems.lean` fails the build if a theorem uses an axiom other
  > than these three, or if its statement depends, through anything other than proofs, on a
  > declaration in `Proof/`; statements may name the executable functions they are about,
  > such as `infer`.
- **B [0, Lean work]. Put the certificate guarantee on the reviewed surface.** Define certificate
  validity in `Spec/`. `Spec/FiniteModel/Certificates.lean` already has `ResultCertificate`.
  Then restate `checked_statistics` and its reward-model counterpart in `Theorems.lean`, so
  that the check covers them. Fig. 13's "verified" label is then backed by a statement a
  reviewer can read.
- **C [0]. If B does not fit the schedule,** label the checker in Fig. 13 as "checked;
  correctness theorem outside `Theorems.lean`". This is honest but weak.
- **D [0, small Lean work]. End every certificate with one theorem in `Spec` terms.** For
  example `(⟨q, m₁, m₂⟩ : OutputStatistics).Matches (bigStepMeasure (checkedSubject.program
  checkedSource))` with the printed rationals as literals, proved from `outputStatistics`.
  Every exact result is then a theorem whose statement uses only reviewed definitions and the
  numbers the tool printed, on all four routes, and no checker needs to be trusted. This gives
  a single result more than B does.

### M5. Fig. 13 costs two thirds of a page, and its legend is misleading

- **Size.** About 0.65 page for a pipeline.
- **"Verified in Lean" is applied to definitions.** "Determinize" and "Measure-theoretic
  semantics" are part of the specification. What is proved are theorems about them, such as
  `typed_determinize`, and the box style suggests the definitions themselves were verified.
- **The Storm box carries a Lean logo** that links to `tools/storm.py`, a Python file.
- **"Finite graph construction" is checked, not verified.** The explored chain is replayed and
  accepted by `checkModel` (`Checking/FiniteModel.lean:18-21`).
- **One arrow, "Output measure μ_e", enters "Soundness theorems"**, although the theorems relate
  μ_{e_src} and μ_{e_det}.
- **Missing pieces:**
  - the transfer arrow from the exact result back to `e_src` (M3);
  - the additive route that Section 7 uses.
- **The caption is two words long.** The useful explanation is in `\Description`, which
  readers of the PDF do not see.

**Ways to fix it.**

- **A [−0.3 page].** One row: `.det` → parse → infer → determinize → {interpret | explore →
  solve (Storm or built-in) → kernel check} → result → (hypotheses) → `e_src`.
  - Use four styles: specification, proved, checked at run time, unverified.
  - Drop the row for the measure semantics. It is not a stage of the pipeline; it is what the
    theorems are about.
- **B [−0.5 page].** Replace the figure with the table from M4-A, extended by a column for the
  Lean name. One object then serves as both the trust model and the architecture. For OOPSLA
  this is the most economical option.
- **C [0].** Keep the layout and fix the legend, the logo and the arrows.

### M6. "Every theorem of the previous sections is mechanized" (line 6) is still too broad

**Theorems that exist in Lean but have no link:**

- Lemma 4.4 is `trace_preservation` (`Theorems.lean:60-65`).
- Thm. 4.8 is `convex_function_inequality` (`:111-118`).
- Proposition 4.9: only its two equations are linked, not its header.
- Definition 4.1 can link to `Spec/Expectation.lean`. The link `expectation-defined` is
  declared in `lean-links.tex` but never used.

**Section 5 has no links at all.** `5_proof.tex:5` is a `\todo{Add Lean references}`.

- Thm. 5.4's two equations exist only as intermediate steps inside
  `compact_exactDepth_source_fiberSound` (`Proof/Traces/CompactFiberSoundness.lean:453`, steps
  at `:460-470`), stated for detailed traces.
- Thm. 5.5 corresponds to the invariant `FiberSound` (`Proof/Traces/Fibers.lean:55`). The
  relation to conditional laws is made at the end (`Proof/Traces/ConditionalLaw.lean:36`).
- Lemma 5.3 corresponds to per-action lemmas spread over `Proof/Symbolic/TraceLaws.lean`,
  `TraceGeneration.lean`, `TraceSamples.lean` and `MeanTraces.lean`.

**The appendix states something no theorem proves.** Its "Probability preservation" adds
`d_{Det(e)} = d_e`, which no Lean theorem states.

**Ways to fix it.**

- **A [0].**
  - Add the three missing links.
  - Narrow the claim: "Every theorem of Sections 3 and 4 is stated in `Theorems.lean` and
    proved; Section 5's argument is mechanized in a different form, over detailed traces."
  - Either prove `d_{Det(e)} = d_e` or drop it. It should follow from `return_or_diverge`,
    `output_mass_preservation`, `trace_preservation` and `typed_determinize`.
- **B [0, Lean work].** State Thms. 5.4 and 5.5 as named lemmas, in `Proof/` if necessary, and
  link to them. For an appendix that claims full proofs, this is the cleaner option.

### M7. The finite-state route is described inaccurately (lines 29–35)

The round-2 finding 4 still applies in full:

- Storm produces no certificate. It returns exact value vectors.
- The wrapper adds the dead states and a rank and next state for every other state.
- The kernel checks three things: the dead region is closed, the paths are valid, and the
  numbers satisfy the linear equations.
- The certificate covers return mass, first and second moments, and rejection probability.
  It does not cover only "the correct expected value".
- It is a certificate for the *explored* program, which by default is `e_det`.

Two things are new:

- the additive route (M1), which the section does not mention although Section 7 uses it;
- the section still does not define "machine state".

**Suggested replacement [about 0 pages; lines 29–35 are already 7 lines in print]:**

> *Exact analysis.* The tool explores the machine states (expression, environment and
> continuation, with rational values) of the chosen program, by default `e_det`, merging
> equal states. This succeeds when the only random draws left are Bernoulli and discrete
> ones and finitely many states are reachable. The return probability and the first and
> second moments solve linear equations over the resulting Markov chain. An untrusted solver
> computes them: Storm with exact rational arithmetic, or a small built-in solver. A
> certificate adds, for every state that can still return, a path to a terminal state, which
> makes the solution unique. The tool writes the chain and the certificate as a Lean file.
> Checking that file with Lean's kernel proves that the program is domain-safe, that the
> chain's output law equals μ_e, and that the certified numbers are its exact return
> probability and its mean and variance conditioned on returning. With `--additive`,
> numbers added to the result of a pending call become rewards on transitions, which makes
> accumulating loops finite-state; Section 7 uses this mode.

State how Storm is called. The model is built through stormpy with exact rational matrices,
not through the command line: Storm's documentation says "there is no --exact mode for
explicit input" [9]. A reader who knows Storm may otherwise assume floating-point value
iteration. That is precisely what the paper should set itself apart from, since value
iteration "can return results that are incorrect by several orders of magnitude" [15].

Add one sentence about why exactness survives determinization. Every supported primitive's
mean is a rational function of its parameters (Fig. 7), so `e_det` with rational literals
stays rational. A distribution such as the log-normal, whose mean is e^{μ+σ²/2}, would break
this. That is a design constraint worth stating.

### M8. Space and structure

**The current shape.** Ten one- or two-sentence paragraphs, one for each box of Fig. 13.
There are three problems:

- The semantics paragraph (line 25) comes after the theorems about the semantics (line 23).
- Line 29 ends with "in two ways:" and is followed by `\paragraph` headings rather than a list.
- Line 23 restates Section 4's theorems in 5 lines, where a pointer would do.

**Proposed structure, about 1.5 pages:**

1. **Trust (0.4 page).** The M4 table, the exact sentence on the build check, one sentence on
   the review of the specification and on LLM use (M2), and the sizes. The sizes are:
   - `Spec/` and the statements: 1,535 lines;
   - `Proof/`: 103 files, 26,087 lines, about 1,040 theorems and lemmas;
   - executable code: front end 1,158 lines, finite exploration and solving 1,102, checking
     171, interpreter 268;
   - Lean 4.33.1 and Mathlib `0df444a`;
   - the build time, which is still not measured.
2. **From text to `e_det` (0.3 page).**
   - Parsing and desugaring are unverified.
   - Inference is correct by Thm. 3.1. `compile` stores the proofs of typing and alignment in
     the `Program` it returns (`Frontend/Compile.lean:10-25`), so nothing is checked at run time.
   - Determinization is the same definition, with rational literals; `interpret_determinize`
     relates the two.
   - Primitive means are rational (M7).
3. **Exact analysis (0.4 page).** M7's paragraph, the guarantee and the transfer (M3).
4. **Sampling (0.15 page).**
   - SplitMix64; Box–Muller; Marsaglia–Tsang for gamma.
   - The G and E draws use separate streams (`Runtime/Eval.lean:27-28`, `:138`), so the
     source and the determinized program see the same G draws for the same seed.
   - A fuel limit of 100,000 steps.
   - Statistical tests.
5. **Figure: 0.3 page (M5-A) or none (M5-B).**

**Moves to the appendix or the artifact:**

- the exploration limits and their flags (10,000 states, 100,000 edges, 1 MB per state);
- the details of the samplers;
- the exact rule for additive extraction (`lean/finite-model-contract.md`);
- the design decisions of the semantics (`lean/README.md`): no measurable structure on
  expressions, the zero measure for invalid parameters, rejection as a self-loop;
- the sizes of each directory.

**Cuts:**

- the paragraph on the measure-theoretic semantics, down to half a sentence (Section 2 already
  defines the semantics);
- "we have no formal model that relates floating-point arithmetic…", down to a clause;
- the commented-out outline in lines 37–97.

## Minor comments, line by line

- **L2.** `\clearpage` looks like a drafting leftover, as does the one in `4_soundness.tex:2`.
- **L6.**
  - "in the Lean programming language": write "in Lean 4 (v4.33.1)" and give the Mathlib
    commit.
  - "the final implementation was mostly carried out by" is ambiguous. Does it mean the code,
    the proofs, the specification or the paper? Say which (M2).
  - "In order to make it clear which parts … need human review and which ones can be left to
    the Lean kernel" can be "To separate what a reader must review from what Lean's kernel
    checks".
- **L6, citations.** The three citations are correct: Lean 4 at CADE 2021, Mathlib at CPP 2020,
  Storm in STTT 2022. For kernels and disintegration, Degenne's paper on Mathlib's Markov
  kernels [16] is the specific reference.
- **L9.** "Trusted surface/specifications" gives two names for one thing. `lean/README.md` says
  "specification"; use that.
- **L10.**
  - "procedures" is vague.
  - "by a single reference to this part" is jargon.
  - "which cannot change what the statement means" needs its reason for a non-Lean reader:
    proof irrelevance, since any two proofs of a proposition are equal.
- **L13.** "We will briefly describe the individual steps in the following" can be deleted.
- **L17.**
  - "source code" should be "source program".
  - The de Bruijn detail can go; nothing later depends on it.
  - The surface language is never introduced, although the examples use `fun`, `rec f x =>`,
    `flip`, subtraction, tuples and `match`, and the prose mentions `observe`. The desugarings
    belong in Section 2 or the appendix: `flip(p)` becomes `0 < bernoulli[G](p)`, `a - b`
    becomes `a + -b`, and `observe(c)` becomes `if c then () else reject`.
- **L19.**
  - "verified to produce a correct typing as well as mode annotation … as many E-sites as
    possible" should be "computes the greatest completion (Thm. 3.1)". "Greatest" is sitewise
    and stronger than "as many".
  - "Mode annotation" is a third name, besides E/G annotation and affinity.
  - The inferred type may be `real^G`. The theorems need `real^E`, which subsumption supplies;
    say so.
- **L21.**
  - `\cref{def:determinization}` prints "theorem 2.1". acmart's definition shares the theorem
    counter, and cleveref names the counter. I tested a fix in a minimal acmart document:
    pass `acmthm=false`, load `amsthm`, `aliascnt` and `cleveref` (with `capitalise`), define
    `theorem`, then `\newaliascnt{definition}{theorem}`,
    `\newtheorem{definition}[definition]{Definition}`, `\aliascntresetthe{definition}` and
    `\crefname{definition}{Definition}{Definitions}`. It prints "Definition 1.1 and
    Theorem 1.2".
  - Cite `typed_determinize` (`Proof/Semantics/Ordinary.lean:153`) for "well-typed".
- **L23.**
  - "the same expectation as e_src whenever the latter is defined": Thm. 4.6 concerns `E_ret`
    and also needs `q_e > 0`.
  - "variance" is `Var_ret`.
  - Better to replace the paragraph with a pointer to Section 4 (M8).
- **L25.**
  - "small-step and big-step semantics": Fig. 7 calls these reduction and output measures, and
    "big-step" occurs nowhere else.
  - The Lean semantics has no evaluation contexts. The link `evaluation-contexts` points to a
    lemma in `Proof/` (`reduce_frameExpr`), not to a definition.
- **L27.** "In order to be able to run the program and to carry out actual sampling" can be
  "To sample".
- **L29.**
  - "finite state graph" should be "finite-state Markov chain".
  - "machine states" is never defined.
- **L31.**
  - The mean and variance are conditioned on returning.
  - Say what is verified: the certificate the solver returns comes with a proof of validity.
  - The solver is limited to 256 states by default.
- **L33.** Storm runs with exact rationals and answers the query `R=? [ F "done" ]`.
- **L35.** See M7.
- **L37–97.** Delete the commented-out outline.
- **Fig. 13.** See M5.
  - "Exact mean and variance" should add "conditioned on returning".
  - The "Soundness theorems" box links only to expectation preservation.
- **Cross-references.** cleveref prints "fig. 13" and "theorem 4.6" in mid-sentence, while the
  captions say "Fig. 13". Load cleveref with `capitalise` (and `noabbrev` if you want "Figure"),
  or use `\Cref` throughout.
- **Lean links.**
  - `certificate-checker` (`Checking/Statistics.lean` L9–27) leaves out
    `checked_conditionalVariance` (L29–41).
  - `expectation-defined` is declared but never used.
- **Terminology.** The paper says "mode" and the Lean code says "affinity". Say once that they
  are the same thing; readers who follow the links will meet `Affinity`.

## Problems elsewhere in the paper that affect Section 6

- **Where the code is available.** There are three answers:
  - "[TODO]" in `1_introduction.tex:77`;
  - "https://github.com/[anonymized]" in `7_evaluation.tex:19`;
  - apndx.org in the Lean links of the review build.

  OOPSLA's FAQ says to "cite the code in your paper, but replace the URL with text like 'link
  removed for double-blind review'". The required Data-Availability Statement, on the other
  hand, "should ideally also include links to preliminary versions of (anonymized) artifacts"
  [1].

  - Use one anonymized location and give it in the Data-Availability Statement.
  - Ask the chairs whether the in-text Lean links to it may stay. They are the paper's
    strongest evidence for the mechanization.
  - Otherwise, render the links as plain marks in the review build and ship the code as
    anonymized supplementary material.
- **The Data-Availability Statement is missing.** OOPSLA 2027 requires it "just before
  references", outside the page limit. It must state "whether an artifact exists, its nature
  and limitations, and whether it will be submitted for Artifact Evaluation" [1].
  - This is the natural home for what M8 moves out of Section 6: build commands, the build
    time, Lean and Mathlib versions, and the command that checks the axioms.
  - SPLASH artifact evaluation asks for "commands that check soundness" [3], and
    `lake build --wfail` is that command.
  - A paper that "ought to" have an artifact must explain if it will not provide one [1].
- **The review build's Lean links are broken.** `main.tex` builds in review mode, whose links
  point to the snapshot `apndx.org/pub/sca95063a7c`. That snapshot is older than `Theorems.lean`
  in its current form.
  - It serves the old 66-line `Theorems.lean`.
  - `Spec/Inference.lean`, `Spec/Expectation.lean` and `Frontend/Affinity.lean` return 404.
  - Of the 32 links, 4 land on the right line, 2 on missing files and 26 on wrong lines. Seven
    of those point past the end of the file.

  The GitHub links at `8f89190` are all correct. Regenerate the snapshot from `8f89190` or
  later, and include the doc-gen4 documentation in it. For a reviewer, hyperlinked statements
  are the cheapest way to check M2.
- **The introduction claims what Section 7 does not show.** It says that "verified matrix
  solving … computes its expectation value exactly", but Section 7 uses only Storm (M1).
- **Section 7 counts its benchmarks twice, differently.** The text says 14 benchmarks, the
  caption of Table 1 says 15, and the table has 14 rows.
- **Section 7's numbers disagree.** The RQ2 text gives 9.02×, 40.58× and 109.73×. Table 2 gives
  8.96×, 40.47× and 108.13× for the same benchmarks. These look like different runs.
- **The interpreter's random streams matter for Section 7.** Section 6 should say that G and E
  draws use separate streams. Section 7 should then say whether the runs of the source and of
  the determinized program are paired, that is, use the same seed. Paired runs share their G
  draws, which correlates the two estimates; these are common random numbers [17, §8.6].
  - Owen notes that common random numbers need "considerable care in synchronization" when
    the two programs consume different numbers of draws. The determinized program does consume
    fewer.
  - Separate G and E streams provide exactly that synchronization, which is worth one
    sentence.
  - Section 7 should also give a confidence interval for each VRF.
- **RQ1's wording.** "Exact inference" usually means computing the posterior distribution. RQ1
  computes "the true expected value … over the exact posterior distribution", which is what
  determinization preserves. "Exact posterior expectation" says it without the ambiguity.
- **Related work does not cover the mechanization.** Section 8 has no paragraph on mechanized
  semantics of probabilistic programs, verified samplers, or certified probabilistic model
  checking. Section 6's contribution needs that context.
  - **The comparison a referee will ask for.** Chatterjee et al. [8] have Storm produce
    certificates for reachability and expected rewards in MDPs: rational value vectors plus
    ranking functions. A verified Isabelle checker checks them. This is a closely related
    design to the Storm route here, for a larger class of models.
    - Their stated gap is "the construction of the MDP … [is] currently not verified". Here
      the chain is replayed against the semantics, so on this point the comparison favours
      this paper.
    - Cite them, and say in one sentence what is new: a checker in Lean, a chain derived from
      a program's semantics, and moment queries.
  - **Earlier certificates:** Farkas certificates for reachability [18], and Hölzl's Isabelle
    formalization of Markov chains with a certifier for finite reachability [19].
  - **Mechanized PPL semantics and verified samplers:**
    - Eberl, Hölzl and Nipkow's verified compiler for probability density functions (ESOP
      2015);
    - Affeldt, Cohen and Saito's s-finite kernels in Coq [20];
    - Zar [13], SampCert [14] and VCVio [21]. VCVio also reports how it used LLM agents.
  - **Why exactness matters:** Storm's own paper on the cost of exact arithmetic [9], and the
    unsoundness of floating-point value iteration [15].
- **The paper does not build from a clean checkout.** `figures/eval-wallclock.tex` includes
  `results/estimator-variance/*.pdf`, and `.gitignore` excludes `*.pdf`; only the PNGs are
  committed. So `latexmk` fails, and with it `check.sh tex`. I built the PDF for this review
  from a copy with the includes changed to `.png`.

## Claims I checked and found accurate

- **L6.**
  - The output measure uses Mathlib's Giry monad (`Measure.bind`).
  - The conditional laws use `Measure.condKernel`.
  - The Gaussian, exponential, beta, gamma and Poisson laws come from Mathlib.
- **L10.**
  - "states each theorem in full and proves it by a single reference" holds. Each of the 19
    theorems is stated over `Spec` (and `infer`) and proved by one term from `Proof`.
  - The axiom check holds. The build prints "19 theorems proved, using only [propext,
    Classical.choice, Quot.sound]".
- **L21.** The tool runs the same `Expr.determinize` that the theorems are about, instantiated
  with rationals.
- **L23.** The hypotheses match Thms. 4.5–4.7.
- **L25.** The semantics is noncomputable.
- **L27.** The interpreter is unverified, and no theorem depends on it.
- **L31.** The built-in solver uses exact rationals and returns its certificate with a proof of
  validity.
- **Spec's imports.** `Spec/` imports only `Spec` modules and Mathlib.
- **Lean links.** All 32 link ranges are correct at `8f89190` and at `96eefdd`.

## References

Quotes are verbatim from the sources. The ACM pages were read through web.archive.org
snapshots of the URLs given, because acm.org refuses automated requests.

1. OOPSLA 2027 call for papers and FAQ. https://2027.splashcon.org/track/splashoopsla2027
2. ACM Policy on Authorship (updated 16 September 2025),
   https://www.acm.org/publications/policies/new-acm-policy-on-authorship, and its FAQ,
   https://www.acm.org/publications/policies/frequently-asked-questions
3. SPLASH 2026 Artifact Evaluation call,
   https://2026.splashcon.org/track/splash-2026-artifact-evaluation, and the proof-artifact
   guidelines it links, https://proofartifacts.github.io/guidelines/. The OOPSLA 2027 artifact
   call is not out yet.
4. Lean Language Reference:
   - "Validating a Lean Proof", https://lean-lang.org/doc/reference/latest/ValidatingProofs/
     (axioms, `decide +kernel`, `implemented_by`, leanchecker, comparator, nanoda);
   - "Natural Numbers", https://lean-lang.org/doc/reference/latest/Basic-Types/Natural-Numbers/
     (GMP in the kernel).
5. Lean FAQ, https://lean-lang.org/faq/ (the TCB of native code).
6. X. Leroy. Formal verification of a realistic compiler. CACM 52(7), 2009.
   https://xavierleroy.org/publi/compcert-CACM.pdf
7. R. M. McConnell, K. Mehlhorn, S. Näher, P. Schweitzer. Certifying algorithms. Computer
   Science Review 5(2):119–161, 2011. https://doi.org/10.1016/j.cosrev.2010.09.009
8. K. Chatterjee, T. Quatmann, M. Schäffeler, M. Weininger, T. Winkler, D. Zilken. Fixed point
   certificates for reachability and expected rewards in MDPs. TACAS 2025.
   https://arxiv.org/abs/2501.11467
9. C. Hensel, S. Junges, J.-P. Katoen, T. Quatmann, M. Volk. The probabilistic model checker
   Storm. STTT 24:589–610, 2022. https://doi.org/10.1007/s10009-021-00633-z. Storm usage
   documentation: https://www.stormchecker.org/documentation/usage/running-storm.html
10. Z. Paraskevopoulou. Machine-generated, machine-checked proofs for a verified compiler
    (experience report). PACMPL 10(ICFP), 2026. https://doi.org/10.1145/3828700
11. Ammanamanchi, Bhat, Biderman. Faults in our formal benchmarking. ICML 2026.
    https://arxiv.org/abs/2606.29493
12. Liquid Tensor Experiment, README. https://github.com/leanprover-community/lean-liquid
13. A. Bagnall, G. Stewart, A. Banerjee. Formally verified samplers from probabilistic
    programs with loops and conditioning (Zar). PACMPL 7(PLDI), 2023.
    https://doi.org/10.1145/3591220
14. Verified foundations for differential privacy (SampCert). PACMPL 9(PLDI), 2025.
    https://doi.org/10.1145/3729294
15. C. Baier, J. Klein, L. Leuschner, D. Parker, S. Wunderlich. Ensuring the reliability of
    your model checker: interval iteration for Markov decision processes. CAV 2017.
    https://doi.org/10.1007/978-3-319-63387-9_8. See also A. Hartmanns, Correct probabilistic
    model checking with floating-point arithmetic, TACAS 2022,
    https://doi.org/10.1007/978-3-030-99527-0_3
16. R. Degenne. Markov kernels in Mathlib's probability library.
    https://arxiv.org/abs/2510.04070
17. A. B. Owen. Monte Carlo theory, methods and examples, Ch. 8 (§8.6 common random numbers,
    §8.7 conditioning). https://artowen.su.domains/mc/Ch-var-basic.pdf
18. F. Funke, S. Jantsch, C. Baier. Farkas certificates and minimal witnesses for
    probabilistic reachability constraints. TACAS 2020.
    https://doi.org/10.1007/978-3-030-45190-5_18
19. J. Hölzl. Markov chains and Markov decision processes in Isabelle/HOL. JAR 59:345–387,
    2017. https://doi.org/10.1007/s10817-016-9401-5
20. R. Affeldt, C. Cohen, A. Saito. Semantics of probabilistic programs using s-finite kernels
    in Coq. CPP 2023. https://doi.org/10.1145/3573105.3575691
21. Tuma, Dao, Waters, Hicks, Hopper. VCVio. IACR ePrint 2026/899.
    https://eprint.iacr.org/2026/899
