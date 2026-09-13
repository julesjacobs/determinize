# Certified finite computation

The reviewed definitions are in `Determinize/Spec/FiniteModel/`. Graph construction,
rational solving, and external certificate checking share those definitions.
Implementation details and correctness proofs live in `Proof/FiniteModel/`.

## Model and observables

A `Model` has a nonempty finite state space, initial state, rational nonnegative
transition rows summing to one, and states classified as transient, returned with
a rational value, or rejected. Evaluation stops at either terminal kind.
`outputWithin n s` records the output law reached within `n` transitions;
`outputMeasure` is its increasing supremum. Rejection and divergence contribute
no output. Finitely many terminal values ensure finite output moments even when
the chain can diverge.

`OutputStatistics.Matches` states exact return mass and first and second output
moments. Conditional mean and variance divide by return mass; the executable
accessors return `none` at mass zero.

`Termination.lean` defines rejection probability by marking rejected states as
returned and discarding successful returns. Divergence probability is the infimum
of finite-depth survival probabilities. `massBalanceThm` states that return,
rejection, and divergence probabilities sum to one. `TerminationStatistics.Matches`
relates the three reported rationals to these probabilities. This distinction is
for the finite model: the core paper semantics counts rejection among executions
that do not return.

## Exact execution boundary

The exact machine uses rational arithmetic and structural state equality.
Bernoulli and discrete samples have finite successors. Primitive means are
rational on rational arguments. Residual stochastic continuous and Poisson calls
are unsupported, including when nested inside a mean site's operands.
Arity and domains are checked; division by zero fails. Encountered nonnumeric
final values and unsupported operations fail explicitly.

Functions, recursion, pairs, sums, and lists may occur internally. Structural
exploration can identify finite cycles but does not abstract infinite state
spaces. Continuous distributions require an explicit finite approximation
before exploration unless determinization eliminates them. No discretization
error theorem is claimed.

## Graph construction and correspondence

`Finite.explore source subject limits` returns either a complete candidate with
`Candidate.ReplayValid source subject`, an incomplete result, or a failure.
The builder maintains correspondence between its state array and lookup map,
completed rows, and successor coverage. It proves row weights by aggregating all
outcomes for each destination. Hash collisions preserve correctness. The normal
export path consumes this proof directly and does not replay the graph.

The candidate records graph data. The independently supplied source and subject
select the program: either the resolved core source or its Lean determinization.
`replay_matches` proves `Model.Matches`, which states paper domain safety and
equality of complete unbounded output laws. No typing or termination premise is
needed for this finite-model correspondence.

External graph replay validates:

- Initial-state alignment, source scope, dimensions, and distinct stored states.
- Every actual exact-machine step and terminal classification.
- Every positive-probability successor and its exact transition weight.
- Distinct valid edge indices, positive weights, normalized rows, and absorbing
  terminal rows.

Portable replay supplies successor indices and state fingerprints. Lean checks
the indices against actual successors and checks each fingerprint; equal-key
pairs still undergo structural comparison. Sparse row checks imply the original
full-matrix contract, including zero weights at absent destinations.

Unique extra valid states are allowed. All certificate obligations cover every
stored state, including unreachable states.

## Why replay implies paper correspondence

The internal proof modules establish the following chain:

- `Substitution`, `Reification`, and `Initial` interpret closures and continuation
  stacks as paper expressions and prove initial program alignment. Missing
  environment entries map to rejection only in this total internal representation;
  replay safety excludes reachable failed lookups.
- `Reduction`, `Administrative`, and `Contexts` connect ordinary operations and
  contexts to paper reduction. Dropping a stack on rejection preserves zero output
  and safety even though paper rejection retains its context.
- `Sampling` and `MeasureLaws` identify full primitive measures, finite continuation
  sums, and almost-everywhere safety on positive-weight outcomes. Measurability
  comes from the existing paper kernel construction.
- `Invariants` proves that reachable sampling frames have the required arity.
- `Progress` bounds consecutive bookkeeping transitions by a decreasing natural
  number. Bookkeeping includes value construction, operand sequencing, and
  rejection. Sampling and application transitions are excluded, allowing recursion
  to diverge through paper reductions.
- `Local`, `Transition`, and `Continuation` classify every successful machine step.
- `Execution` proves safety, both finite-horizon comparisons, and unbounded
  output-law equality. It accounts for differing numbers of machine and paper steps.
- `Graph` regroups exact successor weights using coverage and unique states,
  identifying finite and unbounded graph output with machine output.
- `Soundness.replay_matches` composes these results without additional typing,
  termination, absorption, or integrability premises.

## Boundary analysis and value equations

Terminal reachability computes a closed region D from which no terminal state
is reachable. `cut_outputMeasure` proves that replacing D by zero-output terminal
states preserves the entire output law. For each remaining transient state, a
chosen positive edge of strictly smaller natural rank witnesses a route to the
boundary. Other edges may loop or increase rank.

`paths_unique` proves uniqueness of the terminal-value equations: a maximum
absolute difference between two solutions propagates along the chosen edges and
reaches a zero boundary. This avoids a numerical absorption bound. The analyzer
and path constructor return proofs on success; neither claims a separate
completeness theorem about its implementation.

`MomentCertificate` contains D, ranks, chosen successors, and rational value
vectors for return mass and first and second moments. `checkStatistics` checks
closedness, descending edges, and all equations against the original model.
`momentCertificate_sound` relates them to its output law. Terminal relabelling
and `query_sound` share the proof across observables.

`TerminationCertificate` adds a rejection vector, using the same D and paths.
The rejection query is formed before cutting off D, so a divergent state cannot
be counted as rejection. `terminationCertificate_sound` proves rejection
probability and derives divergence from the mass-balance theorem.

The verified internal route uses `solveValues` and the proved Gaussian elimination
algorithm, retaining equation proofs without checking the solutions afterward.
`solveStatistics` and `solveTermination` share boundary evidence. Separate right-hand
sides currently use separate elimination runs. The older horizon-based
`solveCertified`/`checkResult` API remains available for absorbing models; the CLI
uses the general moment route.

## Portable files and Storm

`--export PREFIX` writes raw `.candidate.lean`, standalone `.replay.lean`, and
Storm transition, label, and positive/negative reward files. `--result PREFIX`
adds `.result.json` and `.result.lean`. Portable Lean files use `decide +kernel`
and export axiom reports. They validate saved data rather than rerun exploration
and elimination. Each state has a separate kernel proof, assembled through a
checked proof table. Moment and rejection equations sum over sparse edges; their
proofs imply the full matrix equations. These checks can cost more than native
execution.

The Storm encoding redirects each terminal to a synthetic zero-reward sink,
paying its output reward once. The adapter adds D to the reward-until-target set;
original terminals remain outside that target so their reward is paid. It asks
Storm for every state's exact rational value, removes the synthetic sink only
after checking its dimension and zero value, and writes `.storm.lean`.
Positive and negative parts are queried separately for signed first moments.
Return, rejection, and second moments use nonnegative reward vectors.

The checker binds Storm's vectors to the original Lean graph and its requested
source/subject. Incorrect transitions, state ordering, rewards, boundary analysis,
or rational conversion cannot establish a false equation certificate. The adapter
never invokes our solver by default; `--compare` enables a differential check.
The result report marks `kernel_checked` only after Lean accepts the certificate
and reports that its output-statistics, termination-probability, and conditional-
variance theorems use only `propext`, `Classical.choice`, and `Quot.sound`.

## Limits and remaining boundaries

Exploration limits state count, edge count, and serialized size per state. Size
is checked after construction and is not a memory bound. An incomplete graph
never becomes a certificate. Computational failures preserve prior exports;
filesystem write failures can leave partial output files. Dense solving defaults
to 256 states; Storm uses the exploration limit instead. Subprocess timeouts
bound adapter stages.

Parsing and desugaring are unverified. The theorem names the resolved core
program rather than the original source bytes. Floating-point sampling is not
proved to implement the real-valued semantics. A determinized-program certificate
does not discharge the source safety and integrability premises needed to
transfer its moments back to the source. No approximation theorem connects a
user-chosen discretization to a continuous program.
