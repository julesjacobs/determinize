# Finite models and certified expected terminal rewards

The reviewed definitions are in `Determinize/Statement/FiniteModel/`. The exact
explorer and model checker are implemented. `Proof/FiniteModel/Soundness.lean`
proves that every accepted graph represents the selected paper program's complete
output law and that the program does not get stuck. Result checking and Storm
invocation remain unfinished: valid linear equations and absorption bounds have
not yet been connected to the infinite-horizon expectation.

## Model and quantity

`Model` contains a nonempty finite state space, an initial state, exact rational
transition probabilities, and a `StateKind` for each state:

- `transient`: evaluation continues.
- `returned r`: evaluation returned rational reward `r`.
- `rejected`: an observation failed.

Rows are nonnegative and sum to one. Terminal states have absorbing self-loops.
Returned rewards are paid once. `outputWithin n s` records the unnormalized real
output law reached within `n` transitions from `s`; a returned initial state
contributes at depth zero. `outputMeasure` is its increasing supremum.
`rewardWithin` computes the finite-horizon expectation in rational arithmetic,
with a proof connecting it to the real integral.

Rewards may be negative. Rejection and divergence contribute no output mass.
The answer is not conditioned on returning. `HasExpectedReward` explicitly
requires integrability. Infinite-horizon integrability and certified answers
remain obligations of the result checker.

## Exact execution boundary

The explorer uses rational arithmetic, including division by zero returning zero
as in the core semantics. It does not use the Float runtime or round values for
state equality. `supportedDraw` records the sampling policy:

| Evaluated call | Exact execution |
| --- | --- |
| Stochastic Bernoulli | Outcomes 0 and 1 |
| Stochastic discrete with checked rational weights | Numeric indices |
| Mean site with rational parameters | Rational mean formula |
| Other stochastic primitive | Unsupported |

Arity and parameter domains are checked at each call. Operands evaluate before
the draw, so a residual continuous stochastic operand remains unsupported even
inside a mean site. Stochastic Poisson and degenerate continuous draws remain
unsupported. Unreachable unsupported expressions do not require export failure.

Functions, recursion, pairs, sums, and lists may occur internally. Encountered
nonnumeric final values, invalid operations, invalid parameters, and unsupported
draws produce explicit failures. Resource exhaustion produces an incomplete
result rather than a model certificate.

## Checked model and exported evidence

`Checking.checkModel source subject candidate` returns a `CheckedModel` containing
a rational model and a proof of `Model.Matches (subject.program source)`.
`checkModelReplay` exposes the underlying `Candidate.ReplayValid` evidence.
`checkModelCertificate` also checks equality with a separately supplied model;
`checkModelCertificate_sound` proves the existing `ModelCheckerSound` contract.

The source and `Subject.source`/`Subject.determinized` selection are supplied
independently of the candidate. Determinized selection uses the existing Lean
transform. A certificate for the determinized program does not automatically
certify the source's safety or integrability.

Replay validation checks:

- Dimensions, the actual initial state, source scope (including unused bodies),
  and uniqueness of stored machine states.
- Every stored state's actual `Finite.step` result, evidence tag, and terminal kind.
- Coverage of every positive-probability successor; zero-weight successors may be absent.
- Exact transition weights, valid distinct edge indices, and positive sparse weights.
- Nonnegative normalized matrix rows and absorbing terminal rows.

Equality is kernel-reducible for nested closures and environments. The checker
uses neither the explorer's hash table nor its Boolean equality implementation.
Unique additional valid states are allowed. Duplicate states are rejected even
when unreachable. Result absorption certificates still quantify over all states.

Each export includes `.candidate.lean` with unverified data and `.replay.lean`
with an independent `decide +kernel` proof of replay, the constructed model, and
`modelMatches`. This last theorem states safety and equality of unbounded paper
output measures. Axiom checks use only Lean's standard axioms. Tests include
closures, value constructors, finite distributions with zero weights,
probabilistic recursion, and pure divergence. Tampering with the initial state
invalidates the certificate.

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

## Exploration and files

`Finite/Explore.lean` performs breadth-first exploration, aggregates equal
successors, and omits zero-weight edges. Equality is structural and resolves hash
collisions. Complete exploration produces candidate data; the checker validates
it before `Finite/Export.lean` writes files.

State count, edge count, and serialized per-state size have independent limits.
The size check happens after construction and is not a hard memory bound. On
failure, existing output files are preserved and the exit status indicates the
failure. Infinite recursion can produce a finite cyclic graph, while growing
arguments, environments, or stacks can exhaust limits. Complete exploration does
not imply absorption or finite expected execution time.

## Result certificates

The result contract is specified; result checking and Storm integration follow in the next change.
