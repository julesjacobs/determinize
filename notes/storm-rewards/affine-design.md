# Algebraic residualization into affine equations

Historical broader design. The user selected the narrower
[addition-only implementation plan](addition-plan.md) for the next build.

Recommended design after source inspection, GPT-6 Pro consultation, and exact
Storm experiments. This supersedes the constant-addition extraction proposal in
`design.md` as the conceptual design. Proposed Lean theorems remain unimplemented.

The core idea is a graph of simplified residual computations, with affine uses of
their returned values recorded on edges. Define a finite-dimensional affine-kernel
semantics, implement scalar return contexts first, and extend to affine accumulator
parameters through coefficient-vector summaries. Weighted equations are the backend
interface; ordinary transition rewards are the additive special case.

See [Pro’s substantive review](pro-affine-response.md) and
[the exact Storm experiments](experiment.py).

## Semantic abstraction

An affine continuation maps a numeric result `x` to `a*x+b`. Keep its coefficients
outside the enumerated control state. Composition is

```
(a,b) after (c,d) = (a*c, a*d+b).
```

A control edge carries a joint outcome `(probability p, target t, a, b)` meaning
that successful output along this edge is `a * output(t) + b`. Neither coefficient
needs to be a literal. Evaluate coefficient expressions in source order; preserve
their joint distribution with the successor control state. Affinity is with
respect to the unresolved result, not constancy in program syntax.

This abstraction is applicable when the erased result/coefficients do not affect
future control, sampling parameters, domain checks, or rejection. It does not
make all functional recursion finite. Retain genuine control continuations,
especially pending independent recursive work. If these or the relevant data
are unbounded, report incomplete extraction rather than identify unequal states.

Existing E/G typing concerns affinity in E samples. It is useful evidence but
is not itself a proof of affinity in a recursive call result, finite control,
or integrability. The extraction analysis needs its own explicit contract.

## Moment equations

For successful return mass `h`, first moment `m`, second moment `v`, and terminal
residual `c`, terminal values are `(1,c,c²)`. Rejection and regions with no path to
successful return have `(0,0,0)`. Other states satisfy

```
h(s) = sum p * h(t)
m(s) = sum p * (a*m(t) + b*h(t))
v(s) = sum p * (a*a*v(t) + 2*a*b*m(t) + b*b*h(t)).
```

For a fixed outer continuation `(a,b)`, the initial reported values become
`m'=a*m+b*h` and `v'=a²*v+2ab*m+b²*h`; return mass is unchanged.
For additive transitions `a=1`, these are ordinary reward equations after return
probability gating. General multipliers yield weighted linear equations;
`p*a` is not necessarily a probability.

A moment-only representation can aggregate per source/destination:
`sum p`, `sum p*a`, `sum p*b`, `sum p*a²`, `sum p*a*b`, `sum p*b²`.
These are joint weighted moments, not products of marginal averages. To specify
and preserve the entire output law, retain the joint outcome kernel instead.
Continuous coefficients require exact joint-moment integration or a justified
determinization step; attaching a reward does not automatically provide either.

## Integrability is a separate obligation

For `f() = if flip(1/2) then 1 else a*f()`, execution terminates almost surely.
For nonnegative `a`, its first moment is finite exactly when `a<2`; its second
moment is finite exactly when `a²<2`:

```
m = (1/2) / (1-a/2)
v = (1/2) / (1-a²/2)
```

Thus `a=3/2` gives first moment 2 and infinite second moment. At `a=2` the first
moment diverges. At `a=3`, blindly solving the first equation gives `-1`, which
cannot be the expectation of this nonnegative program. Signed factors can also
produce a rational algebraic solution when the absolute first moment diverges.
Certificate acceptance must establish integrability before interpreting equations
as expectations. Mean-only queries must not require a finite second moment.

A conservative finite rational certificate on the productive transient states is
`w>0` and `K_abs*w < w`, where

```
K_abs(s,t) = sum_outcomes_to_t p*abs(a).
```

For the second moment use `K_2(s,t)=sum p*a²` and a corresponding bound, together
with the lower-moment evidence required by the second-moment equation. Boundary
terms, offsets, and their absolute moments must be finite. This condition bounds
absolute contributions, preventing cancellation from hiding divergence. It is
sufficient, not complete: a program with identically zero output may be integrable
even if a coarse absolute-coefficient kernel expands.

If checking fails, say “integrability not certified”; failure alone is not proof
of infinite expectation. Divergence requires a separate sound argument.

## Optional reduction to Storm

For a nonnegative weighted kernel `K` and positive rational `w` satisfying
`K*w < w`, set

```
Q(s,t) = K(s,t)*w(t)/w(s)
```

and send missing row mass to a zero-valued sink. If `m=K*m+d`, then `u=m/w`
satisfies `u=Q*u+d/w`. Storm can solve this absorbing DTMC and we recover `m=w*u`.
A checker verifies the scaling and original weighted equations with exact rationals.
The scaled DTMC is a numerical encoding of moment equations; it is not the
program's operational control chain. Do not read termination probabilities from it.

For signed factors, preserve each coefficient sign (or aggregate the first-moment
kernel before a justified split), use a two-copy sign construction bounded by
`K_abs`, and split signed right-hand sides. This is a separate implementation
choice from the semantic extraction. For a small prototype, solving checked
weighted rational equations directly is simpler; a Storm comparison can use the
scaled construction once implemented and independently checked.

Independently checked with Storm 1.14.0: the weighted kernel
`[[0,2],[1/10,0]]` admits `w=(15/4,11/8)`. With right-hand side `(1,0)`, the scaled
DTMC gives original values `(5/4,1/8)`. See `experiment.py`.

## Next implementation experiment

Use one general affine continuation representation from the start. Demonstrate:

1. Additive geometric recursion (mean 1, second moment 3).
2. A computed/random additive coefficient independent of future control, while
   preserving correlation whenever it influences the same edge's target.
3. An outer multiplier and a recursive multiplier (including factor 3/2).
4. Rejection after an affine contribution, to check return-mass factors.
5. An integrability failure, so an algebraic solution cannot become a certificate.

Only then widen control abstraction or add accumulator-parameter elimination.

## Common treatment of return contexts and accumulator parameters

For a broader implementation, use finite control locations with a finite vector
of symbolic numeric slots. Each edge carries an affine update `z' = A*z+b`;
terminal output is an affine function of the slots. Allocate slots at loop/call
boundaries and retain transfer matrices on edges rather than specializing state
keys by the accumulated coefficients. Guards, probabilities, and validity checks
must be determined by retained finite control; otherwise the slots cannot be
erased by this abstraction.

Accumulator parameters fit directly. A scalar return context also fits: use
slots `(alpha,beta)` for `x -> alpha*x+beta`. Encountering an inner context
`x -> a*x+b` updates them by

```
alpha' = a*alpha
beta'  = b*alpha + beta.
```

These updates are affine in the slots even when `a,b` are computed concrete
values on the current edge. A terminal residual `c` observes `alpha*c+beta`.
This provides a shared architecture rather than unrelated rewrites for additions,
scalings, and loop counters. A first implementation can use the scalar continuation
kernel as the simplest instance of this architecture.

Backward expected output is an affine function of the slots. Its coefficients
satisfy weighted linear equations over finite control. For the second moment,
lift to degree-at-most-two monomials; cross terms and coefficient/successor
correlations remain necessary. Finiteness of the basis does not prove the moments
exist, so the integrability certificate is still a separate obligation.

This does not license affine updates across arbitrary recursive calls: calls with
pending control work still need summaries or a retained stack. General recursive
function summaries can introduce nonlinear return-probability equations, so the
initial finite-control fragment must be explicit.

## Persistent random coefficients

A computed coefficient must be summarized conditionally on all persistent data
that affect future coefficients. Consider sampling `a ~ Uniform(0,1)` once, then
running `f() = if flip(1/2) then 1 else a*f()`. Its expectation is

```
E[1/(2-a)] = log(2).
```

Replacing `a` by its mean on each recursive edge would instead give `2/3`.
Repeated use of the same random coefficient creates dependence across iterations.
In fact the irrational exact answer cannot arise from a finite rational linear
system with a unique solution. A finite-support persistent coefficient can be
retained as finite control; a continuous one needs a parameterized/integrating
backend or an explicit unsupported result. Fresh coefficients can be integrated
locally only when their joint distribution with the next state is available.

The current frontend correctly marks the shared uniform coefficient G; a direct
export attempt fails on the residual stochastic uniform. By contrast the fresh
additive `uniform(0,2)+f()` coefficient is E and becomes its mean before extraction.

## User-facing formulation: algebraic residualization

Construct a graph of simplified residual programs. For `3 + 2*f x`, use a node
for the residual computation `f x` and an edge annotation `y -> 3 + 2*y`.
Composition of affine annotations replaces growth of algebraic expression
contexts; node keys omit the factored algebra. Evaluate coefficient expressions
in source order and retain all control-relevant dependencies.

This is a semantic factorization, not ordinary equality of the output laws of
`f x` and `3+2*f x`. Its equation for first moments is `m=2*m_next+3*h_next`,
so failure to return still discards the 3. The graph has an operational control
projection and a separate affine equation interpretation. Ordinary Markov rewards
are the special case in which every multiplier equals one.

Algebraic simplification must retain call-by-value evaluation effects. Factoring
`0*f()` into a constant payoff zero still executes `f()` and preserves its return
mass, rejection, divergence, and possible domain error. Similarly, replacing
`let x=f() in x-x` by immediate return 0 would be unsound; it may simplify the
payoff after retaining the computation of `f()`. Two occurrences `f()-f()` are
two evaluations, not a shared symbolic variable. The control graph records the
evaluations; algebraic annotations describe their numeric use.


## Refinements from Pro's source-grounded review

Separate three obligations: affine graph well-formedness; symbolic CEK replay
preserving the complete output law and domain safety; and query-specific moment
integrability plus weighted-equation correctness. Replay itself must not assume
integrability. Macro-step cycles must correspond to source progress rather than
empty circular abstraction.

Use a general finite-dimensional affine kernel for the specification. Accumulator
functions return coefficient vectors `(u,c)` representing `z -> u^T z+c`; a forward
register update `z'=Az+d` acts backward on those vectors by
`[[A^T,0],[d^T,1]]`. This unifies accumulator parameters with return contexts.
Vector second moments require cross moments, not just coordinate variances.

Moment certificates must follow the requested observable's dependencies. For
`0*f()` only the child's return mass is needed, even if its own moments diverge.
Do not require all-state moment finiteness or invent zero values for unavailable
moments. Add support invariants as a second certificate family: the recursion
`if flip(.5) then 1 else 3*f()-2` always returns 1, although the absolute-coefficient
contraction test fails. Failed contraction means uncertified, not divergent.

Use `sum p*abs(a)` as the pathwise first-moment majorant, not `abs(sum p*a)`.
Otherwise random signs can hide infinite absolute expectation. Plain Bochner
integral equalities also do not establish integrability in Lean.

Recommended sequence: (1) affine semantics and hand-written graph certificates;
(2) generic symbolic CEK evaluator and checked residualization; (3) parameter
templates for accumulator loops; (4) weighted rational solving and the scaled
Storm backend, all bound to the selected source/subject. Preserve the old exporter.

An independently checked substantial evaluation target is determinized packing:
28 transient macro-states plus a terminal, with expected weight
`118513705/10077696` (about 11.76), and second moment
`227517128669/1612431360`. The experiment constructs this model explicitly; source
extraction and its Lean proof are still required. The signed scalar Storm reduction
was also checked for multiplier -3/2, giving first moment 2/7.
