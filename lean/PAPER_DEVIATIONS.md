# Differences between the paper and the Lean development

A plain-language summary of where the paper in `tex/` and the Lean development in `lean/`
differ. The detailed list, with the Lean names for every paper notation, is the
section [Deviations from the paper](README.md#deviations-from-the-paper) of the README.

This comparison was recorded for the revisions named in the README. Later paper and
implementation changes require rechecking these findings. Open decisions are retained below;
accepted differences follow. [Open issues](../OPEN_ISSUES.md) lists the remaining paper work.

## Open

### 1. The Gaussian random walk in the introduction returns a list

- **Paper:** the introduction presents determinization on a Gaussian random walk whose result
  is the whole path, a list of (time, position) pairs.
- **Lean:** every soundness theorem is about programs that return a single real number. The
  tool infers and determinizes this program, but none of the guarantees apply to it as written.
- **Options:** make the example return one number, for example the final position, as the
  benchmark `benchmarks/exact/gauss_randwalk_finalPos.det` does; or add a sentence saying that
  the guarantees apply to real-valued results.

### 2. The appendix section on the trace-indexed kernels is missing

- **Paper:** Section 5 says the appendix gives the definitions of the kernels K_{n,s} and K_e,
  the lemma relating them, and the derivation of tracewise soundness. That appendix section is
  a to-do.
- **Lean:** all of it exists, and can serve as the basis for the section:
  - K_{n,s} is `compactFiber`.
  - K_e is `normalizedOutputGivenTrace`: the replay summed over all depths, replaced by a point
    mass at 0 where that sum does not have total mass 1.
  - The compatibility lemma is `outputGivenTrace_eq_outputGivenTraceAt`.
 
Either supply the promised appendix derivation or replace the promise with precise Lean
references.

### 3. The examples use syntax the grammar lacks

- **Paper:** the grammar has `λ`, `rec`, pairs and `case`. The examples use `fun`,
  `rec f x =>`, tuples, `match … with`, `flip(p)`, subtraction and `observe`.
- **Lean:** the unverified front end translates these:
  - `flip(p)` becomes `0 < bernoulli(p)`;
  - `a - b` becomes `a + -b`;
  - `observe(c)` becomes `if c then () else reject`.
- **Further front-end differences:**
  - It also accepts `<=`, and moves a constant right factor of a multiplication to the left.
  - It reads `discrete(p0, …, pn)` with all n + 1 probabilities and drops the last, while the
    paper's `discrete` takes only the first n.
- **Options:** one sentence in Section 2 saying that the examples use standard syntactic sugar,
  or extend the grammar.

Decide whether mathematical syntax and concrete program syntax should coincide, or explain
their relationship explicitly.

### 4. The recorded benchmark run has no checked certificates

- **Paper:** Section 7 says that the exact values come with certificates.
- **Repository:** `tools/bench.py` skips certificate checking unless `--check-certificates` is
  given. The recorded run `results/finite-after-det.json` lists every certificate as
  "generated, unchecked", and the `dreckon` export hit the 180-second timeout.
- **Options:** rerun with checking enabled and a longer timeout, or soften the sentence.

### 5. The review version links to an outdated snapshot

- **Paper:** the anonymous review build links to the apndx.org snapshot `sca95063a7c` instead of
  GitHub.
- **Repository:** that snapshot predates verified inference and uses old theorem names, so the
  review build's Lean links are wrong until it is regenerated from the current development.

Choose between an updated anonymous website and an anonymous supplementary ZIP containing
Lean and the simulator. For a ZIP, Lean references could name files and lines in an appendix.

## Accepted

### 6. The type inference algorithm is described differently

- **Paper:** Section 3 first checks type shapes, then solves the subtyping constraints by
  expanding type variables one constructor at a time. It sets aside constraints between two
  unknown variables, and propagates E forwards and G backwards.
- **Lean:** the implementation unifies the shapes of both sides of every constraint. It attaches
  a fresh E/G variable at every real-number position, splits the constraints into simple ones,
  then sets every variable forced below a G to G and all others to E.
- **Why it matters little:** both produce the same result, the program with as many E sites as
  possible. The Lean theorem verifies only that result, not the steps.
- **Two smaller points:**
  - An explicit annotation in Lean is the mode itself, not a fresh variable constrained to
    equal it.
  - The appendix's constraint rules cover mean sites (M), but Lean's inference never sees mean
    sites, because its input is a program before determinization.

### 7. Table 1's determinization times do not match the recorded run

- **Paper:** Table 1 gives per-benchmark determinization times.
- **Repository:** the committed run `results/exact-determinization-only.json` differs from the
  table by up to 16% (`swap`: 0.169 ms in the table, 0.146 ms recorded). The table was produced
  by a run that is not committed.
- **Options:** take the numbers from the committed file, or rerun
  `tools/bench.py --exact --det-only` (a minute or two) and commit that run.

The timing variation is accepted; reproductions may differ. Numerical results and certificate
status still need reproducible records.

### 8. Expectations of unnormalized measures

The paper defines expectation for probability distributions. Lean defines it for any measure
on the reals and applies it directly to the output law, which may have total mass below 1. When
the return probability is positive, "the expectation given return is well-defined" means the
same in both.

### 9. The expectation formula along traces

- **Paper:** Section 4 writes E_ret[e] = (1/q_e) ∫ E[e | τ] η_e(dτ): the expectation given
  return is the average over traces of the expected result given each trace.
- **Lean:** `returned_expectation_along_traces` proves it for every program, with no conditions.
  The expectation given a trace has a positive and a negative part, and Lean integrates the two
  parts over traces separately before subtracting them.
- **Gap:** getting from Lean's form to the paper's single integral takes one linearity step,
  which is valid when one of the two parts is finite. The paper does not name the identity.

### 10. The output marginal

Section 4 says, without proof, that the joint trace-and-output measure has the ordinary output
measure of Section 2 as its output marginal. Lean proves it (`trace_erasure`).

### 11. Tracewise soundness, formulation

The paper says that the determinized program returns a point mass at E[e | τ], an expectation in
the extended reals. Lean uses the ordinary (Bochner) integral. Both theorems also state that
this integral is finite, and then the two coincide.

### 12. Variance, formulation

A finite second moment is Mathlib's `MemLp id 2`, and the variance is Mathlib's `variance`. The
Lean variance decomposition also proves that the average variance within a trace is finite,
which the paper does not say.

### 13. Lean proves more than the paper states

- `return_or_diverge` for programs whose result has type `real^G`; the paper states it only at
  `real^E`.
- Expectation preservation for finite expectations.
- Preservation of the expectation given acceptance, which is what justifies `observe`.
- The determinized program's output law equals the law of the source's conditional mean given
  the trace.
- Every finite reward model has finite first and second moments
  (`finite_reward_integrability`). Section 6 implies it by calling the certified numbers the
  exact moments.

### 14. How the semantics is set up

- **No evaluation contexts:** Lean has none. Its step function goes into the leftmost
  unevaluated operand and wraps the result back up.
- **Mean sites:** the paper treats evaluating a mean site as a deterministic step. Lean treats
  it as a "sampling" step from a point mass at the mean.
- **Calls outside a distribution's domain:** the paper's semantics gets stuck. Lean takes a
  "sampling" step from the zero measure. Like a stuck state, this produces no output, and such
  a program is not domain-safe.
- **Domain safety:** Lean phrases it as "every sampling step's measure has total mass 1".
- The output law, the step count and the trace are the same either way.

### 15. One measure per distribution

The paper has a separate domain condition and mean for each distribution. Lean has one measure
per distribution:

- at a sampling site, the distribution itself;
- at a mean site, a point mass at the mean;
- outside the domain, zero.

Domains and means agree with the paper's table.

### 16. Trace entries for `discrete`

The paper's trace entry records the distribution and the sampled value. Lean's entry for
`discrete` also records how many probabilities were supplied. Otherwise the measurable structure
on traces is the same.

### 17. Subtyping

The paper lists reflexivity and transitivity as typing rules. Lean has reflexivity only at base
types as a rule, and proves general reflexivity and transitivity as lemmas.

### 18. Distribution typing

Each distribution rule of the paper's typing figure ("mode M, or a mode equal to the result
type") is two Lean rules: one for sampling sites, one for mean sites.

### 19. Proof internals of Section 5 and the appendix

- Samples are numbered from the newest backwards (de Bruijn style), not u₁, …, u_k in order.
- Parameters in which the mean is not affine, such as a Gaussian's variance, are kept as plain
  numbers rather than as constant affine expressions.
- The symbolic step is a function returning one of a few actions, not a relation given by
  inference rules.
- The actual and expected interpretations run over detailed traces with one entry per step,
  from which the paper's trace of G samples is read off. The appendix's commuting lemmas
  therefore hold up to that extra entry.
- The appendix's per-depth agreement theorems are replaced by a single identification with
  Mathlib's conditional distribution.
- The affine-means equation takes a bound on the distributions' first absolute moments as a
  premise, and Lean proves that bound separately.

### 20. What Section 6 does not mention

- The pretty printer that displays the programs is unverified too.
- The specification reviewed against Sections 2–4 does not include the finite-model and
  reward-model definitions in which the exact results are stated.

### 21. Exact results

- The paper gives the additive reward models, the tool's default on which Section 7 relies,
  one sentence.
- A certificate is checked by Lean's kernel only when its `.lean` file is compiled; the tool
  writes it but does not compile it by default.
- The command-line output shows only the first moment. The return probability, the second
  moment, and the conditional mean and variance are in the `.result.json` and `.storm.json`
  files.

### 22. Hand-simplified example output

The right-hand programs in the introduction's four figures are simplified by hand, as their
headings say. The tool prints mean sites such as `mean_gauss(…)`, names variables by position
(`x0`, `f1`) and removes nothing.
