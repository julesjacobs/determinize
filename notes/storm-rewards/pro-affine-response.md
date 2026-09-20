# GPT-6 Pro: source-grounded affine design

Source: https://chatgpt.com/c/6aaeec02-85e4-83ea-be66-940d8463a00c
Received 2026-09-19, after 12m53s. Substantive answer summarized below.
Pro read the supplied 67 KB source bundle. Proposed theorems are design proposals,
not proved results. Independent experiments are in experiment.py.

## Recommendation

Use a finite affine-kernel model extracted by a checked symbolic CEK machine.
Weighted equations are the canonical backend interface; ordinary rewards are the
additive instance. Define semantics for finite-dimensional affine maps, implementing
the scalar instance first. Affinity is relative to unresolved output or symbolic
parameters, independent of E/G affinity relative to random draws.

An edge `(p,t,a,b)` maps the child's successful output law by `x -> a*x+b` and
multiplies its measure by p. Mapping the zero measure still gives zero; at a=0,
the result is `h(t)*dirac(b)`, not `dirac(b)`. Finite-horizon laws have an increasing
supremum. The correspondence theorem should preserve the entire output law and
domain safety without an integrability premise.

Retain parallel affine outcomes. Moment aggregation needs separate sums p, pa,
pb, pa², pab, pb²; average coefficients do not preserve these quantities.

## Extraction

Use symbolic values `c+sum u_i*z_i` with canonical parameter templates, and
interpret outer output contexts as affine edge annotations. Permit addition,
negation, multiplication by a hole-independent value, and division by a checked
nonzero hole-independent denominator. Repeated use `r+r` is affine; `r*r` is not.
Guards, sampling parameters and retained recursive control arguments must be
hole-independent unless a separately checked invariant establishes their behavior.

Computed/random left operands are evaluated normally in source order. Once they
deliver a value, factoring the resulting frame needs no syntactic constant rule.
Their computations still need finite control/outcomes or certified summaries.
Preserve coefficients jointly with successor state and shared samples.

Pending effectful right operands cannot be treated as known offsets. Initially
retain them; later a continuation kernel could summarize execution after successful
return. Compressing an internal affine stack segment does not permit moving it
past a result-inspecting control continuation. The frame for `c / hole` is not
affine, despite the frame for `hole / c` being affine when c is valid.

Project environments only with exact dependency evidence, including values captured
in closures and saved frames. A candidate is an exact parameterized state family,
not a probabilistic overapproximation.

## Accumulators

For finite control q and symbolic registers z, edges update `z'=A*z+d`, with
probabilities and control independent of z. Interpret future return as random
affine function `z -> u^T*z+c`. Its coefficient vector transforms backward by

```
(u,c) = [[A^T,0],[d^T,1]] * (u_next,c_next).
```

An outer return transformer `alpha*x+beta` scales that vector and adds beta to
its constant coordinate. Thus accumulator summaries use the same vector affine
kernel semantics. Initial numeric output applies the coefficient vector to the
initial registers. Mixed second moments are necessary in the vector case.

Initially allow one unresolved recursive successor per affine edge. Two distinct
recursive computations are different from two uses of one result. Even independent
f()+g() has `h=h_f*h_g` and `m=m_f*h_g+h_f*m_g`; mutually unknown recursive summaries
can produce nonlinear equations. Keep such pending work or report unsupported.

## Proof/certificate layers

1. Graph dimensions, probability normalization, affine-map dimensions.
2. Symbolic CEK replay with source/subject alignment, exhaustive joint outcomes,
   valid parameter substitution, domain safety and evaluation-order preservation.
   Cyclic replay must represent actual source progress; administrative elimination
   needs a well-founded progress bound, preventing empty circular justifications.
3. Separate certificates for probabilities, integrable first moment, and integrable
   second moment. Current finite-terminal support gives integrability automatically;
   this no longer holds. Lean's Bochner integral alone is insufficient evidence:
   it is defined as zero when the integrand is nonintegrable.

Keep the existing backend intact while adding the affine backend. Reuse rational
elimination and sparse exact checking, but weighted systems need a new uniqueness
argument: the old unweighted maximum principle does not tolerate amplification.

## Integrability

After cutting states unable to return successfully and eliminating terminal values,
first and second moments satisfy `m=K1*m+c1(h)` and `v=K2*v+c2(h,m)`.
Use pathwise majorant `A1(s,t)=sum p*abs(a)`, not `abs(sum p*a)`.
Positive rational witnesses `A1*w1<w1` and `K2*w2<w2` certify the respective
contractions on transient unknowns. Bound finite-horizon absolute moments and
pass to the output-law limit. These are sufficient, not necessary.

Counterexamples/refinements:

- Factor 3/2 in geometric recursion: mean 2, infinite second moment.
- Factor 3: algebraic mean -1 is not an expectation.
- Recursive multipliers +3 and -3 with probability 1/4 each, base1 with probability
  1/2: aggregated K1=0 but absolute first moment diverges. Signed cancellation in
  the matrix is not integrability evidence.
- `if flip(.5) then 1 else 3*f()-2`: every successful output is 1, although the
  contraction criteria fail. Add support/bounded-output invariants, potentially
  exposed by recentering `x=c_s+y`, offset `b'=a*c_t+b-c_s`.
- Certificates must be query-local: `0*f_with_multiplier_3()` has finite zero
  output even though the child's first moment diverges. Its child return mass
  remains necessary. Do not populate unused child moments with false zero values
  and then claim an all-state theorem.

## Storm

With nonnegative K and positive w satisfying K*w<w, use Q=D^-1*K*D, reward D^-1*c,
and kill leftover row mass at a zero sink. Recover x=D*z. For signed K, double
sign states using positive/negative coefficient parts bounded by the pathwise
majorant; splitting only the RHS is insufficient. These sign components are solver
decomposition values, not necessarily positive/negative parts of terminal output.

Check restored values against original affine equations. Termination statistics
come from the original probability controller, never from the scaled solver chain.
Constructing w can itself require a solve, so scaling is backend reuse rather
than a claim of lower total computation cost.

## Proposed implementation/evaluation sequence

Start with scalar affine semantics and hand-written graph certificates, including
zero/support invariants. Add checked generic symbolic CEK extraction and environment
projection. Then add affine parameter templates for accumulator programs. Connect
internal rational solving, scaled Storm, and portable per-state proof tables.

Suggested regressions include geometric increments; computed fresh offsets 1/3
(mean2, second13); factors 3/2,3,-1; rejection/divergence gating; zero multipliers;
fixed output under expanding maps; and correlated scale/offset pairs. Keep negative
cases for control depending on symbolic registers and branching recursive work.

Close `coin_flip_unif.det` with `f () (fun u => 0)`. Close pack with `run 0` and a
numeric projection. Packing can retain `(nPacked,nHeavy-nLight)` with imbalance
-1,0,1: 28 transient macro-states plus a terminal. Determinized increments are
1,23/20,17/20. Pro proposed:

- mean weight = 118513705/10077696;
- second moment = 227517128669/1612431360.

Both values were independently verified against an explicitly constructed macro
model using exact Storm 1.14.0 in experiment.py. This is not a checked source-to-model
extraction. The source noisy program's second moment need not match.
