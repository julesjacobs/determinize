# Verified exact computation

The implementation now has one verified path from a resolved core program to a
finite Markov chain and two routes to a proved numerical result: the verified
rational solver, or exact Storm vectors checked in Lean. Neither route assumes
that the original program terminates.

## Completed

- [x] State normalized expectation, variance non-increase, and trace variance
  decomposition in the public specification, with proofs matching those statements.
- [x] Prove Gaussian elimination returns a solution of the original equations.
- [x] Separate finite graph data from exploration.
- [x] Verify graph construction: state lookup, hash collisions, insertion,
  successor coverage, sparse row aggregation, and completed-model correspondence.
- [x] Return candidate data with `ReplayValid` evidence; use it directly on the
  internal path. Retain independent replay for external graph data.
- [x] Preserve explicit failures and limits; incomplete exploration yields no model.
- [x] Compute a closed divergent region and prove cutting it off preserves the
  complete output law.
- [x] Construct positive descending paths to the remaining boundary and prove
  uniqueness of the value equations from those paths.
- [x] Use the same query theorem for return mass and first and second moments.
- [x] Derive conditional mean and variance, returning no conditional quantity at
  zero return probability.
- [x] Define rejection and divergence probabilities separately for finite models,
  and prove finite-depth and limiting mass balance.
- [x] Certify rejection values and derive divergence from that mass-balance theorem.
- [x] Compose output statistics with the selected paper program's output-law theorem.
- [x] Obtain all-state exact vectors from Storm without calling our solver.
- [x] Handle signed rewards, rejection, divergence, the synthetic sink, and exact
  rational decoding; validate all vectors against the original Lean model.
- [x] Export standalone kernel-checked Storm theorems and optional solver comparisons.
- [x] Test malformed dimensions, nonfinite values, wrong equations, wrong boundary
  data, wrong ranks, zero return mass, and real Storm examples.
- [x] Kill the entire subprocess group on a Storm-adapter timeout.
- [x] Measure the verified builder and replace expensive horizon checks with paths.
- [x] Update the architecture figure, exact-computation subsection, CLI documentation,
  and finite-model contract.

## Design decisions

The rank argument proves uniqueness directly. A separate numerical absorption
bound is unnecessary for the moment route. It permits loops: only one positive
edge per transient state must decrease the rank. The closed divergent region is
an internal zero-output boundary and is never reported as rejection.

The older absorbing-model solver/checker remains available. The CLI uses the
more general moment route. Multiple right-hand sides currently use separate
elimination runs; their proofs share boundary analysis and equation semantics.
There is no measured justification yet for a more complicated matrix factorization
API or a verified SCC algorithm.

The builder and solver carry proofs directly. Portable artifacts reconstruct
proofs by checking saved graph and value data. A trial of kernel reconstruction
through the current explorer did not reduce through its representation-based
hashing, so it is not a replacement for portable replay.

## Portable checking and measurements

- [x] Prove indexed successor witnesses and sparse row checks imply the original
  graph contract, including successor coverage and duplicate-state rejection.
- [x] Restrict full structural comparisons to equal-fingerprint pairs. Check the
  fingerprints independently; collisions cannot hide duplicate states.
- [x] Check moment and rejection equations over sparse edges, reusing their proofs.
- [x] Generate one kernel proof per state and combine them through a checked proof
  table. Use balanced lookup for fingerprint keys.
- [x] Require standard-axiom reports before marking a Storm result kernel-checked.
- [x] Add tampered-witness tests and a real Storm regression above the dense solver's
  256-state limit.

Development measurements, not paper benchmarks:

- On 107/287/557-state countdown programs, verified graph construction took
  35/145/449 ms; the former explorer plus replay took 70/359/1261 ms.
- A 40-state mixed-return/divergence certificate took 11.21 seconds with a
  numerical absorption horizon, 6.85 seconds with descending paths, and 5.14 seconds
  with sparse checking and per-state proofs.
- A complete 269-state Storm certificate took 88.98 seconds to kernel-check;
  export and Storm took 0.12 and 0.13 seconds. The same size previously exceeded
  five minutes. The certificate used only the standard Lean axioms.

Portable kernel checking remains the main scaling cost. Fingerprint comparisons
still examine state pairs, although most avoid structural comparison. Shared
elimination and further scaling work are deferred until profiling justifies them.

## Boundaries retained

The theorem names the resolved core program. Parsing, desugaring, and floating-point
sampling remain unverified. Exact exploration handles primitive means and finite
Bernoulli/discrete samples. It requires finite reachable machine state space and
successful exploration within resource limits. No discretization-error theorem
is claimed. Transferring a target certificate back to the source retains source
safety and integrability premises.

The construction and solver APIs prove successful results correct; separate
algorithmic completeness theorems are not claimed. No general PCTL checker or
formalization of Storm is part of this work.

## Closeout

- [x] Warning-free Lean build and standard-axiom checks.
- [x] Exact-result suite, including 14 real Storm cases and portable kernel checks.
- [x] Complete regression suite: 123 corpus cases, statistical tests, runner/export/
  result/workflow tests, and independent kernel certificates.
- [x] Final review and reviewable local commits. Nothing pushed.
