# RQ2: Statistical efficiency of determinization

Status: proposed general protocol. The completed first pilot, using a published
tree-ring HMM, is recorded in [HMM_RESULTS.md](HMM_RESULTS.md). Numbers below
are design choices, not results; the executed pilot's exact scope and deviations
are documented in that report.

## Claim and scope

Question: how much does automatic determinization reduce the variance of an
estimator of the same expectation, under a fixed sampling method and budget?

Use a factorial comparison: benchmark instance × sampler × transformation
(off/on) × budget × independent replicate. Compare off/on within each sampler
first. Comparisons between samplers answer a separate question.

Expectation preservation alone does not establish variance reduction. For a
benchmark where the transformed output is a conditional expectation of the
original output, the law of total variance gives a precise prediction. Do not
claim that characterization for arbitrary transformed programs without proof.

## First benchmark: HMM with a noisy reward

Let Z_1,...,Z_T be a finite-state Markov chain with known initial distribution
and transition matrix. Initially use two states, encoded with Boolean branches
if convenient for the source language. Optionally condition on a fixed sequence
of discrete observations y_1,...,y_T with known emission probabilities.

At each step generate R_t = a[Z_t] + eta_t, where eta_t are independent
N(0, sigma^2) variables independent of states and observations. Return
F = (R_1 + ... + R_T) / T. Rewards must not affect later transitions or evidence.
Express division by constant T as multiplication by its reciprocal if useful.
The OCaml Gaussian sampler's second argument is variance, not standard deviation.

The intended transformed output is G = sum_t a[Z_t] / T. Verify that the
compiler actually produces this behavior; do not substitute handwritten G as
the automatic-transformation result. Handwritten G is a separate reference.

Without evidence, the target is E[F]. With fixed evidence, it is E[F | y].
Writing all variances conditional on the same y when applicable:

    E[F] = E[G]
    Var(F) = Var(G) + sigma^2 / T
    VRF = 1 + sigma^2 / (T Var(G))

These identities follow from conditional independence and G = E[F | Z]. For
independent trajectories and equal trajectory counts they also give the ratio
of sample-mean variances. Increasing reward noise can make the ratio arbitrarily
large: this is a mechanism study, not evidence by itself of broad effectiveness.

Compute the exact mean using state marginals (forward-backward with evidence).
Compute Var(G) with a dynamic program carrying reward first and second moments,
or two-time state marginals; single-time marginals alone omit temporal covariance.
Validate the reference using enumeration of short state sequences.

Start without observations, then add short discrete observation sequences with
rejection sampling. This keeps the first experiment close to the existing
interpreter's Boolean observe semantics. Long conditioned sequences require a
more appropriate inference backend. Do not implement continuous observations as
equality tests against sampled continuous values.

## Instances and controls

Suggested initial mechanism grid, fixed before collecting confirmatory results:

- T in {10, 50, 200}.
- Symmetric two-state chains, stationary initial distribution, state means
  a = {0, 1}, and self-transition probability in {0.5, 0.9, 0.99}.
- Reward standard deviation sigma in {0, 0.5, 2, 8}; pass sigma squared to gauss.
- A separate constant-state-mean case where G is exactly constant.
- A state-occupancy query and a nonlinear reward query such as a threshold event,
  for which the compiler may retain most or all randomness.

This is 36 main configurations plus controls. It tests noise, temporal
dependence, and sequence length without selecting parameters to hit a desired
ratio. Later expand to more states and asymmetric chains.

For conditioned experiments, define a smaller grid separately, varying emission
informativeness and evidence length. Generate and freeze multiple evidence
sequences per configuration. Hold each sequence fixed across all replicates and
methods. Report instance-specific results before aggregating; do not mix
between-dataset variation with Monte Carlo estimator variance.

Include at least two other program families before making broad claims about the
language transformation (for example hierarchical mixtures and stochastic loop
rewards). Include cases where eliminated draws contribute little variance.

## Methods

| Method | Role |
| --- | --- |
| Original program + independent forward simulation | Primary unconditioned baseline |
| Automatically transformed program + same sampler | Primary treatment |
| Handwritten conditional-mean program + same sampler | Checks how much available simplification automation captures |
| Exact HMM dynamic programming | Ground truth and runtime reference |
| Original/transformed + rejection sampling | Initial conditioned experiment |
| Original/transformed + importance sampling | Later weighted-inference extension |
| Original/transformed + particle filtering | Later sequential-inference extension |
| Original/transformed + MCMC | Optional if supported and scientifically useful |

For each sampler pair, match proposals, particle counts, resampling rules,
initialization, and tuning policy wherever meaningful. Any tuning must use
separate pilot data and an equal tuning budget. Report differences explicitly.
Do not interpret a change of sampler as a pure transformation effect.

For this small finite-state HMM, exact inference may beat every Monte Carlo
method. Report that openly: the benchmark tests automatic elimination of reward
noise, not superiority to specialized exact HMM inference. Analytic
marginalization is established in Rao-Blackwellized inference; the compiler's
automation is the relevant contribution.

## Estimands and statistical protocol

For each fixed instance, method m, and budget N, obtain R independent complete
estimates mu_hat[m,r,N]. Propose R = 200 initially, with a separate pilot to assess
whether uncertainty is acceptable. Freeze the confirmatory replicate count.
Use geometrically spaced budgets, initially N = 2^k for k = 4,...,16, extending
or reducing the grid based on the pilot and then freezing it.

Define the primary variance reduction factor at equal N:

    V_hat[m,N] = sum_r (mu_hat[m,r,N] - mean_r mu_hat[m,r,N])^2 / (R-1)
    VRF(N) = V_hat[base,N] / V_hat[det,N]

This is variance across complete estimator replicates, not variance across a
single running-mean trace. For independent unweighted draws, the single-output
variance divided by N is an additional consistency check.

Using the exact target mu_star, also report:

    bias_hat(m,N) = mean_r mu_hat[m,r,N] - mu_star
    RMSE_hat(m,N) = sqrt(mean_r (mu_hat[m,r,N] - mu_star)^2)

Define population samples-to-accuracy by:

    N_m(epsilon) = min { N : E[(mu_hat[m,N] - mu_star)^2] <= epsilon^2 }
    S_N(epsilon) = N_base(epsilon) / N_det(epsilon)

Estimate crossings on the frozen budget grid using replicate RMSE. Report
bracketing budgets; label any fitted/interpolated crossing as an estimate, state
its fitting rule, and validate it with fresh runs near the estimated crossing.
Predeclare absolute tolerances in query units, e.g. {0.1, 0.05, 0.02} for average
rewards with state means {0,1}. Avoid relative error when the mean is near zero.
If a method does not reach tolerance, report a bound or "not reached".
Never use the first lucky crossing of a single trajectory's absolute error.

For unbiased IID sample means N_m(epsilon) = max(1, ceil(Var(F_m)/epsilon^2)),
so sample-count speedup approaches VRF as budgets increase. These are related
measurements, not independent evidence. For weighted or correlated estimators,
measure the full procedure's RMSE; do not assume this IID identity.

Use 95% bootstrap intervals by resampling independent replicate IDs, retaining
all budgets from each selected replicate. If methods are coupled, resample their
replicate pairs together. Recompute ratios and accuracy crossings in each
bootstrap draw; retain non-crossings rather than silently excluding them.
Do not present unstable crossing estimates as precise speedups.

Equal seeds alone do not couple retained random choices after draws are removed.
Use independent streams unless an explicit site-addressed coupling is provided.
Within each method replicates must remain independent. Coupling methods can
improve precision of contrasts, but the variance of paired differences is not
the primary VRF numerator or denominator.

If transformed variance is zero by analytic argument and implementation check,
report "zero variance" (or an infinite theoretical VRF), with absolute error and
runtime. An empirically constant finite sample alone does not prove zero
variance. Do not cap such cases at an arbitrary impressive finite number.

## Budgets and timing

- IID: N is the number of complete independent trajectories, not time steps or
  scalar random draws.
- Rejection: report accepted trajectories and total attempts. For the primary
  statistical comparison use a fixed accepted count; also evaluate fixed work
  budgets. Zero-acceptance runs at fixed work are failures, not missing values
  to discard. Include rejection costs in timing.
- Importance sampling: N is proposal count; report weight degeneracy and RMSE
  including finite-sample bias for self-normalized estimates.
- Particle filtering: report particle count and horizon; replicate entire
  filters. Dependent particles are not independent estimator replicates.
- MCMC: report iterations, warmup, convergence diagnostics and query-specific
  effective sample size; replicate complete chains/runs. Marginal draw variance
  alone ignores autocorrelation.

Report wall-clock time-to-accuracy in addition to sample-count speedup. Record
compilation/transformation, initialization, sampling, and total time separately.
Show both cold total cost and amortized repeated-query cost when relevant. Match
runtime/backend, hardware, thread count and timing boundaries; exclude plotting
and raw-output serialization from sampling time. Counterbalance execution order.
Count remaining distribution calls as a mechanism diagnostic, not as N.

## Paper presentation

Suggested order: (1) question and estimators; (2) benchmarks and methods;
(3) replicate/budget protocol; (4) variance reduction; (5) accuracy and runtime;
(6) ablations and limitations.

Main table: instance/query, sampler, N, baseline estimator variance, transformed
estimator variance, VRF with 95% interval, bias/RMSE, samples-to-accuracy speedup,
and time-to-accuracy speedup. Split statistical and runtime tables if too wide.

Main figures:

1. VRF by instance with intervals, including weak and no-benefit controls.
2. RMSE versus trajectory budget on log-log axes with declared tolerances.
3. RMSE versus elapsed time with identical targets.
4. Mechanism plot: observed VRF versus the analytic reward-noise prediction.

Keep detailed sweeps in the appendix. Report all prespecified cases; show the
range and number of regressions/no-benefit cases. Any aggregate must define its
weighting and treatment of zero variance. A large maximum is not a suite summary.

## Repository integration and implementation order

The current determinize_main.ml runs only 100 attempts per program and reports
means. It is a demonstration, not a variance experiment. tools/convergence.py
has useful repeated-run/plotting scaffolding, but references
sample_trace_main.exe, which is absent from the inspected OCaml sources and dune
stanzas. Restore or replace that interface before relying on it. Its optional
pooled Monte Carlo reference should be replaced by exact HMM truth here.

Proposed layout once implementation begins:

    experiments/
      README.md
      configs/       # frozen grids, seeds, budgets, tolerances
      benchmarks/    # generators and source .det programs
      reference/     # exact HMM means and second moments
      runner/        # actual original/transformed execution
      analysis/      # replicate statistics, intervals, figures
      results/       # raw replicates and summaries, with run metadata

Implement in this order: short HMM reference and enumeration check; source
generator and transformation inspection; replicated IID runner; variance/RMSE
analysis and plots; timing; controls; conditioned inference extensions.

Record commit and dirty diff, source/transformed program hashes, parameters,
evidence ID, method, seed, budget, replicate ID, estimate, accepted/attempted
counts, failures, elapsed times, software versions and hardware. Preserve raw
replicate data and the exact configuration used for every paper figure.

## Methodological references

- [Rabiner, A Tutorial on Hidden Markov Models (1989)](https://www.cs.ubc.ca/~murphyk/Bayes/rabiner.pdf): exact HMM inference.
- [Doucet et al., Rao-Blackwellised Particle Filtering for Dynamic Bayesian Networks](https://arxiv.org/abs/1301.3853): sampling some variables while analytically marginalizing others.
- [Stan Reference Manual: Effective sample size](https://mc-stan.org/docs/2_31/reference-manual/effective-sample-size.html): autocorrelation and estimator precision for MCMC.
