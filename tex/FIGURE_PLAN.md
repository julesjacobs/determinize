# Paper drafting checklist

Use the mathematics and examples to explain the idea. Keep assumptions that
affect the results; leave routine formal details to Lean.

- [x] Language grammar.
- [x] Type grammar.
- [x] Primitive laws, domains, and means.
- [x] Output semantics.
- [x] Determinization.
- [x] Selected typing rules and subtyping.
- [x] Domain validity and global guarantees.
- [ ] Joint trace semantics and conditional trace soundness statement.
- [ ] Variance decomposition.
- [ ] Derive the main theorem from trace soundness.
- [ ] Example programs and moment calculations.
- [x] Clear section bodies, retaining the outline.
- [ ] Type inference.
- [ ] Write the general symbolic invariants and induction.
- [ ] Author review of the mathematical definitions and statements.
- [ ] Introduction, implementation, evaluation, and related work.

The previous draft remains in `archive/`. The new document follows
[the paper structure](../PAPER_STRUCTURE.md); the outline above records drafting
status, not a claim that the paper's proofs are complete.

## Soundness presentation plan

Lead with expectation preservation, then variance reduction. Explain the
probability conventions and supporting guarantees afterwards. Keep domain safety
as an assumption and mass balance as a consequence: the latter checks that the
semantics loses no probability through some additional mechanism.

- [x] Check the exported Lean statements and their assumptions.
- [x] Prove return-or-diverge mass balance, including rejection, in Lean.
- [x] Prove its equivalence with domain safety under real typing.
- [x] Rename section 3 to "Type System and Soundness Theorem".
- [x] State expectation preservation first, followed by variance non-increase.
- [x] Explain finite, infinite, and undefined expectations with positive and negative parts.
- [x] Give an undefined-expectation example that also motivates the trace theorem.
- [x] Explain domain safety and state mass balance and output-mass preservation.
- [ ] Add a Lean bridge lemma for normalized variance.
- [ ] Author review of the prose and normalization convention.

### 1. Lead with the guarantee

After the typing rules, explain in one sentence that replacing E samples by
means preserves the expected output and reduces variance. Briefly gloss domain
safety as valid arguments for executed operations, almost surely. The detailed
probability discussion comes after the statements.

**Theorem (Expectation preservation).** Suppose
$\vdash e : \mathsf{real}^{\mathsf E}$ and $e$ is domain-safe. If
$\int r\,\mu_e(dr)$ is defined, then $\int r\,\mu_{\operatorname{Determinize}(e)}(dr)$ is defined and

$$
\int r\,\mu_{\operatorname{Determinize}(e)}(dr)=\int r\,\mu_e(dr).
$$

"Defined" allows $+\infty$ and $-\infty$, but excludes $\infty-\infty$.
This is the existing `extendedExpectationThm` in
[`Spec/Main.lean`](../lean/Determinize/Spec/Main.lean). Its finite-expectation
specialization is `mainThm`, which also explicitly concludes target
integrability and domain safety. State that finite source expectation stays
finite; do not make the reader decode this from the extended-real equality.

Add a short forward pointer: the refined theorem in the Traces section does not
require the global expectation to exist.

### 2. Follow with variance reduction

Use ordinary variance for the output distribution conditioned on returning
successfully. When the output mass is positive, write
$\widehat\mu_e=\mu_e/\mu_e(\mathbb R)$; apply the same convention to $\operatorname{Determinize}(e)$.
The theorem's output-mass guarantee ensures that the target normalization exists.
Explain why the normalization matters in the later mass discussion.

**Corollary (Variance non-increase).** Under the same typing and domain-safety
assumptions, if the source has positive output mass and a finite second moment,
then the target also has positive output mass and a finite second moment, and

$$
\operatorname{Var}(\widehat\mu_{\operatorname{Determinize}(e)})
\le \operatorname{Var}(\widehat\mu_e).
$$

The expectations of these normalized laws also agree whenever the source
expectation is defined. Use the same normalization convention consistently when
speaking about moments of successful runs; the first theorem's displayed
integrals are explicitly over the unnormalized measures from the semantics.

Lean's `varianceThm` proves second-moment non-increase and a variance inequality
for the unnormalized measures. Its `variance` centers at the unnormalized first
moment, which is not the conditional mean when the output mass is below one.
Do not import that convention into the paper. The normalized statement follows
from equal output mass $q>0$, equal first moments, and

$$
\operatorname{Var}(\widehat\mu_e)
=\frac1q\int r^2\,\mu_e(dr)
-\left(\frac1q\int r\,\mu_e(dr)\right)^2.
$$

Add the short Lean bridge before describing this formulation as directly
checked. A finite second moment implies a finite absolute first moment because
the output measure is finite. No separate first-moment assumption is needed in
the variance corollary.

### 3. Explain what an expectation being defined means

Start from nonnegative integrals: these are always defined, with $+\infty$
allowed. Then introduce

$$
r^+=\max(r,0),\qquad r^-=\max(-r,0),\qquad
A=\int r^+\,\mu_e(dr),\quad B=\int r^-\,\mu_e(dr).
$$

The signed expectation is $A-B$.

- If both $A$ and $B$ are finite, the expectation is finite. Equivalently,
  $\int |r|\,\mu_e(dr)=A+B<\infty$.
- If exactly one is infinite, the expectation is defined as $+\infty$ or $-\infty$.
- If both are infinite, the expectation is undefined.

Do not say that a finite expectation requires the expectation of the absolute
value merely to be "defined": the nonnegative absolute-value integral is always
defined; it must be finite. This avoids a circular explanation. Nor should the
paper identify undefined expectation with nontermination or invalid operations.

### 4. Use one example to motivate the qualification and the trace theorem

Proposed example (all operations are valid almost surely):

```text
let u = uniform[G](0, 1) in
let s = 2 * bernoulli[G](1/2) - 1 in
s / u + uniform[E](-1, 1)
```

The program returns almost surely. Nevertheless, both positive and negative
parts of its expectation are infinite: the random sign multiplies the
nonintegrable reciprocal, and the bounded E noise does not change either tail.
For contrast, $1/u$ alone has the defined extended expectation $+\infty$.

Determinization removes the centered E noise and returns $s/u$. For almost every
fixed G trace, $u$ and $s$ are fixed, the source's conditional mean is the finite
number $s/u$, and the target returns exactly that number. This is meaningful even
though neither program has a global expectation. Use this to motivate the
forward pointer, without introducing trace kernels or restating the entire
refined theorem here.

The zero-divisor event $u=0$ has probability zero. The example therefore also
illustrates why domain safety is almost-sure safety, not validity on every
possible sample.

### 5. Explain domain safety and account for probability

Domain safety requires valid distribution parameters and nonzero divisors along
executed paths, almost surely at every finite depth. It concerns prefixes of
nonterminating executions too. It allows rejection and divergence. Typing alone
does not enforce these numerical conditions. Keep the recursive `DomainSafeAt`
definition out of the paper presentation.

Then give the supporting results, with no moment assumptions:

- A real-typed, domain-safe program has
  $\mu_e(\mathbb R)+\Pr(e\text{ diverges})=1$, counting rejection as divergence.
- Determinization preserves domain safety and output mass:
  $\mu_{\operatorname{Determinize}(e)}(\mathbb R)=\mu_e(\mathbb R)$.

Together these explain both the missing output mass and why conditioning on
successful return preserves the comparison. For output mass one the normalized
and original laws coincide. For mass zero there is no conditional output law,
although both unnormalized output measures and their integrals are zero.

Lean sources: `returnOrDivergeThm` and `outputMassThm` in `Spec/Main.lean`, and
`domainSafe_determinize` in `Proof/Semantics/Ordinary.lean`. The converse in
`Proof/Semantics/Termination.lean` is useful for the formal development, but need
not be another paper theorem or replace the direct domain-safety assumption.

### Traces and proof sections

In the Traces section, state `conditionalLawThm`: equal laws of terminating G
traces and, almost everywhere, the target conditional output law is a point mass
at the finite source conditional mean. No global integrability assumption is
needed. Follow it with the variance decomposition from `Spec/Traces/Main.lean`,
using the same normalization convention as the earlier variance corollary.
In the Proof section, derive the global results from this refined theorem.

Keep the general nonnegative-convex-function inequality out of the main statement
unless a later example or argument uses it. Do not add a fixed step bound or an
almost-sure termination assumption.
