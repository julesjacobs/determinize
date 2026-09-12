# Independent Pro focused response (rendered text)

1. The half-Dirac example: correct arithmetic, insufficient justification for the proposed API

For

let _ = observe(flip(0.5)) in 3

the successful-output law is μ=
2
1
	​

δ
3
	​

. The supplied finite-model tests already contain proofs of this law for the corresponding return/reject fork and of its expected reward 3/2. I inspected the relevant specifications, trace bridges, corollaries, CLI/result contracts, and documentation; I have not run Lean or compiled a normalization patch. 

determinize-pro-cli-tests

The reviewer’s three variance calculations are correct:

Interpretation	First moment	Variance
Current unnormalized law, using variance id μ	3/2	9/8
Law conditioned on successful return, δ
3
	​

	3	0
Probability law obtained by assigning rejected runs output zero	3/2	9/4

In particular, the current raw variance is

∫(x−∫xdμ)
2
dμ=
2
1
	​

(3−
2
3
	​

)
2
=
8
9
	​

.

This agrees with the general formula actually proved in Proof/Corollaries.lean:variance_id_eq_moments. Raw expectation agrees with zero-filled expectation, but raw variance does not agree with zero-filled variance. 

determinize-pro-architecture-so…

However, this example contains no E draw to eliminate: flip lowers to a G Bernoulli comparison, and observe to a conditional with rejection. Determinization leaves the program unchanged. Consequently, it illustrates competing statistical conventions—not a weakness in the variance-reduction theorem. 

determinize-pro-architecture-so… +1

My overall verdict is revise: keep the existing theorems, correct the documentation, defer the full normalized-law API, and drop the recommendation to promote it as the ordinary interface on the evidence presented.

2. The existing theorems already contain the required mathematics

Let μ
s
	​

,μ
t
	​

 be source and target output laws, with common mass m>0, common first moment a, and second moments b
s
	​

,b
t
	​

. Write 
μ
ˉ
	​

i
	​

=μ
i
	​

/m.

The existing mass, expectation, and variance statements give precisely the ingredients

m
s
	​

=m
t
	​

,a
s
	​

=a
t
	​

,b
t
	​

≤b
s
	​

,

together with transfer of the required integrability or finite-second-moment property. Notably, varianceThm exposes the second-moment inequality, not just the unusually centered raw variance inequality. Its documentation already explains the normalized consequence. 

determinize-pro-architecture-so… +1

For the two variance conventions,

V
raw
	​

(μ
i
	​

)=b
i
	​

+(m−2)a
2
,Var(
μ
ˉ
	​

i
	​

)=
m
b
i
	​

	​

−
m
2
a
2
	​

.

Therefore

Var(
μ
ˉ
	​

s
	​

)−Var(
μ
ˉ
	​

t
	​

)=
m
b
s
	​

−b
t
	​

	​

=
m
V
raw
	​

(μ
s
	​

)−V
raw
	​

(μ
t
	​

)
	​

≥0.
	​


Thus the normalized variance comparison requires no new determinization argument. The inspected raw decomposition proof explicitly obtains equal first moments and equal masses, expands both variances, and closes by algebra. 

determinize-pro-architecture-so…

The same reasoning applies to the trace decomposition. With τ the successful trace law and K(u) the source replay law,

Var(
μ
ˉ
	​

s
	​

)=Var(
μ
ˉ
	​

t
	​

)+
m
1
	​

∫Var(K(u))dτ(u).

The last term is the average replay variance under τ/m. This is already a mathematical consequence of the current raw identity, exactly as the trace specification explains. 

determinize-pro-architecture-so…

An important qualification is that an individual raw variance divided by mass is not generally the normalized variance. In the example, (9/8)/(1/2)=9/4, not zero. It is the difference of the source and target raw variances that scales correctly because the common correction cancels. The existing comments about cancellation are correct.

For expectation, the redundancy is stronger: conditionalExpectationThm already states the desired quotient equality, and Theorems.conditionalExpectationPreservation already exports it. The underlying proof is literally a rewrite using preservation of the first moment and mass. 

determinize-pro-architecture-so… +1

What is missing is a packaged endpoint whose conclusion mentions variance or expectation of a scaled measure—not the mathematical preservation result. Such an endpoint can improve convenience, but it is not a strengthening of the intended theorem.

3. Which conditioning and rejection criticisms survive?
“It is only a quotient identity”: accurate syntax, potentially misleading assessment

The reviewer correctly observes that the statement is totalized, including zero mass, and does not construct a conditional-expectation random variable. But for positive finite mass,

∫xd
μ
ˉ
	​

=
m
∫xdμ
	​

.

The existing theorem therefore already expresses preservation of the conditional mean. Constructing a conditional-expectation random variable on an execution probability space is not required for this law-level claim.

I would retain conditionalExpectationPreservation, rather than rename it to acceptanceMeanQuotient_preserved. The present name is justified with an explicit positive-mass interpretation. Renaming adds migration cost without changing the guarantee.

At zero mass, the quotient’s zero is a convention, not an expectation conditioned on a positive-probability event. The proposed acceptedOutputLaw does not remove this issue: its total definition likewise does not produce a probability law at zero mass. Positivity must still qualify the interpretation. The current specification deliberately records the quotient convention. 

determinize-pro-architecture-so…

The rejection-sampling caveat is correct and deserves a documentation repair

Positive successful-return probability does not imply almost-sure completion of sequential “retry after rejection.”

Replace the rejecting branch of the fair-coin example by an infinite loop. Its successful-output law remains 
2
1
	​

δ
3
	​

. But an ideal retry-after-rejection procedure now has probability 1/2 of becoming stuck forever in its first diverging attempt; there is no rejection on which to retry. By contrast, with an actual rejecting branch, independent retries eventually succeed almost surely.

This is precisely why the sentence about rejection sampling in conditionalExpectationThm should be narrowed. The theorem establishes equality of normalized output means; it does not establish a sampler’s correctness or termination. A normalized-law wrapper would not establish those either. 

determinize-pro-architecture-so…

The README really does contain the unqualified sentence “No conditioning theorem is claimed,” despite describing the conditional-mean theorem earlier. The reviewer correctly identified that inconsistency. Replace it with the distinction between the proved law-level result and the unverified executable procedure. 

determinize-pro-architecture-so… +1

The observable-scope criticism is correct, but normalization does not address it

The mathematical semantics records successful real returns. Rejection and divergence both contribute no output, and rejected executions contribute no successful trace. Model.Matches states safety and equality of these output laws—not equality of rejection probabilities or execution times. 

determinize-pro-architecture-so… +1

Accordingly, “the same traces are rejected” is not directly a statement of the exported successful-trace theorem. It may describe an operational invariant, but the present public trace objects do not record rejected traces. The minimal correction is to describe the preserved successful trace marginal and successful-return mass. This does not require a rejection-sensitive semantics.

There is also a distinct conditioning issue at the replay level. outputGivenTrace forces supplied G values; its conditional-law interpretation requires the theorem’s hypotheses and almost-everywhere qualification. The proof already identifies the source replay with a probability kernel almost everywhere. Acceptance-normalizing the final output marginal neither supplies nor replaces that reasoning. 

determinize-pro-architecture-so… +1

4. The CLI does not justify making normalization the default

The reviewer’s strongest usability argument is that normalized variance matches the CLI’s “among returned values” statistics. That is true: Main.summarize divides by the number of returned numeric values, excluding rejected and failed runs. But the same CLI also has a different, intentional exact-result contract: --result certifies the unnormalized expected terminal reward. For this example, 3/2, not 3, is the correct exact answer. The README explicitly documents this distinction. 

determinize-pro-cli-tests(1)

 

determinize-pro-architecture-so…

Promoting one convention because it matches the empirical display overlooks the other public client. Better labeling is sufficient; the interfaces need not share one statistic.

Moreover, adding normalized mathematical theorems would not validate the finite-fuel Float statistics. Your local reproduction of a nonconstant, finite-valued sample whose displayed variance becomes zero through overflow is an actual erroneous output, not a missing theorem wrapper. That repair has a stronger immediate claim on effort than normalization APIs. I am relying on your recorded execution, not claiming to have reproduced it myself. 

pro-architecture-review

5. Minimal recommendation and costs

Keep the raw expectation, mass, second-moment, variance, trace-decomposition, and quotient theorems. They express the intended transformation and already support both statistical interpretations.

Revise the documentation now, using wording such as:

Output laws record successful real returns. Their total mass excludes rejection and divergence. Under the theorem’s hypotheses, determinization preserves this mass and the raw first moment. When the common mass is positive, conditional means agree and conditional variance does not increase. At zero mass, the quotient theorem uses the convention zero; this is not conditioning on a positive-probability event. No correctness or termination guarantee for the numerical or retrying sampler is established.

This is a documentation-only change with no theorem migration. The half-Dirac calculation is enough to explain the variance distinction.

Defer the full normalized-law suite and drop its priority-1/default-interface status. The real benefit would be easier application of probability-measure lemmas and less client-side scaling algebra. Nothing in this example establishes that this benefit warrants another program-indexed family of definitions and results.

Should a concrete client need a formal normalized endpoint, the minimal alternative is a generic finite-measure bridge:

Var((μ(R))
−1
⋅μ)=
m
∫x
2
dμ
	​

−(
m
∫xdμ
	​

)
2
,m>0,

under a finite-second-moment assumption. Prove it once—or reuse an equivalent result from the pinned Mathlib—and obtain the program comparison from the existing statements. I have not checked whether that exact bridge already exists in the unbundled dependency. Its cost is scaling and integrability transport, not another soundness proof. A probability-normalization fact should similarly be generic, without unnecessary typing or second-moment hypotheses.

The recommendation survives as a small interpretation repair and an optional convenience corollary. It does not survive as a required improvement to the determinization theorem or as justification for expanding and re-centering the ordinary public API.

Sources
ChatGPT can make mistakes. Check important info.

6
Pro