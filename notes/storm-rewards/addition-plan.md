# Addition-only reward extraction: implementation plan

Status: implemented. See `README.md` and `lean/finite-model-contract.md` for the shipped scope.

Implementation choices superseding the proposals below:
- Extract only the maximal outer suffix of evaluated numeric-left addition frames.
  Pure right-hand-side folding is deferred. A zero-addition guard preserves errors.
- Retain outcome lists, including same-target/different-reward edges.
- Check absolute first/second moment upper bounds to establish integrability;
  no general geometric-tail existence theorem is required by the certificate.
- Hash-based exploration checks the complete local replay predicate before success.
- Native and Storm certificates prove full source-law correspondence, safety,
  integrability, moments and conditional variance. Termination probabilities are
  certified for the controller; first-hit rejection correspondence is separate.

Prepared from GPT-6 Pro's source-grounded plan and checked against this repository.
Pro received 300 first-party files (1.58 MB), including the complete Lean development,
proofs, tests, runner/Storm code, examples, and pinned build configuration.
Conversation: https://chatgpt.com/c/6aaeec02-85e4-83ea-be66-940d8463a00c
Source manifest: [addition-context-manifest.txt](addition-context-manifest.txt).
The earlier general affine design is out of scope for this build.

## Outcome and scope

Build a separate additive reward model with checked normalization layered over the
existing `Determinize.Finite.step`. Preserve the old finite model and default export
path. Introduce one opt-in `--additive` selection for export/result/Storm commands.

The first end-to-end milestone is:

```
let f = rec f u => if flip(0.5) then 0 else 1 + f u in f ()
```

Exploration completes below 100 states and produces independently kernel-checked
source correspondence, return/rejection/divergence probabilities `(1,0,0)`, first
moment `1`, second moment `3`, and conditional variance `2`.

This build factors translations `x -> x+r`, where r can be an already evaluated
computed or random number. It does not extract multiplication, negation, duplicated
results such as `x+x`, affine register matrices, or recursive function summaries.
Concrete arithmetic remains available when computing an increment. Keep original
evaluation order, dependencies, errors, rejection, and divergence.

Accumulator elimination, original packing-source extraction, and marginalization
of a random coin bias are separate follow-ons. They are not prerequisites.

All names under `RewardModel` and `Finite.Reward` below are proposed. Existing
module paths are relative to `lean/Determinize/` unless another root is shown.
Use `jujacobs/` for any implementation branch, for example `jujacobs/additive-rewards`.

## 1. Small specification extension

Add `Spec/RewardModel/Model.lean` with:

- A finite state space, initial state, and existing `Spec.FiniteModel.StateKind`.
- Finite outgoing edge lists with `target`, rational `probability`, and rational
  `reward`; nonnegative probabilities and normalized rows.
- `Model.control`, an ordinary `Spec.FiniteModel.Model` obtained by summing
  probabilities by target, preserving original terminal classifications.
- `shift r μ := μ.map (fun x : ℝ => x+r)`.
- Successful-output laws `outputWithin`, `outputAt`, and `outputMeasure`.
- `Model.Matches program := DomainSafe program ∧ outputMeasure = bigStepMeasure program`.

At a transient state:

```
outputWithin (n+1) s = sum_edges p * shift r (outputWithin n target)
```

Transient horizon-zero output is zero. Returned states retain their terminal Dirac
measure at every horizon; rejected states have zero output. The unbounded law is
the increasing supremum.

`shift r 0 = 0`: rewards translate successful returned values, so a rejected or
divergent suffix discards every pending addition. This is the specification,
not a correction applied to the final numerical answer.

Preserve separate `(target,reward)` outcomes. Only identical pairs may be merged by
adding their probabilities. Averaging rewards by target loses the output law and
second moments.

Add `Spec/RewardModel/Results.lean` for complete exported result propositions,
including explicit integrability of both `id` and `fun x => x^2`. Reuse
`Spec.FiniteModel.OutputStatistics`, but its `Matches` predicate alone does not
assert integrability. Keep representations, replay data, algorithms and auxiliary
lemmas outside `Spec/`; do not move unrelated existing declarations.

## 2. Normalization over the existing CEK machine

### Evaluated operands

Proposed function:

```
Finite.Reward.normalize : State -> Rat × State
```

The stack is ordered innermost first, as `stackExpr` in
`Proof/FiniteModel/Reification.lean` shows. Inspect the maximal *outer suffix*, at
the end of the stack, consisting of:

```
.right .add (.number c)
```

These operands have already been evaluated. For a nonempty recognized suffix,
sum its constants to r, replace it by one existing frame
`.right .add (.number 0)`, and return `(r, canonicalState)`. Leave an empty suffix
unchanged; rejection stays rejection.

Retain the `+ 0` frame even when the removed constants sum to zero. It preserves
the numeric operand check through the existing `binary` operation, including raw
ill-typed CEK inputs. It is not a new machine instruction or trusted type assertion.

The normalizer must be idempotent:

```
normalize s = (r,t) -> normalize t = (0,t)
```

Store only t in the exploration key. Attach r to the transition into t. Do not
accumulate r inside the state. Every other environment, saved frame, and closure
payload remains structurally represented; no liveness-based equality is required.

For `e + f()`, ordinary CEK evaluation computes e before entering f. Sampling,
parameter checks, calls and rejection in e are preserved. Once e returns c, the
same rule applies regardless of whether e was a literal, arithmetic, a finite
random choice, or a supported helper call. No source-literal classification of e
is needed. If c also affects future control, its retained environment occurrence
remains part of the state.

### Pure right operands: a small later commit within this build

Extend recognized suffix frames to `.left .add rhs savedEnvironment` only when a
bounded checked execution proves that rhs returns a numeric value deterministically
without sampling or rejection. Use the existing `Finite.step`:

1. Start from `.eval rhs savedEnvironment []`.
2. Permit only actual probability-one singleton transitions.
3. Initially exclude all `.sample` evidence, including mean sites.
4. Require a numeric return and retain/check its finite replay length and result.
5. On failure, rejection, sampling or fuel exhaustion, return *not foldable*.
   Do not diagnose an execution error or change the residual computation.

Use a fixed fuel budget per frame so repeated normalization cannot make additional
rewrites just because an earlier scan consumed a different shared budget. Bind the
budget/normalizer configuration consistently in construction and portable replay.
The fuel bounds optimization effort, not program semantics.

This supports `f()+c`, saved numeric variables, arithmetic, projections and
concretely terminating pure helpers. It does not execute an effectful RHS early.
In particular, `loop_forever()+(1/0)` must remain a valid zero-output divergent
program; `(1/0)+loop_forever()` fails before the call.

### Boundaries

| Context | Treatment |
|---|---|
| `c+x`, with c already evaluated | Extract translation |
| `x+rhs`, with checked pure numeric rhs | Extract after pure-RHS extension |
| `x+effectful_rhs()` | Retain pending work |
| `-x`, `c*x`, `x/c`, comparisons | Retain; no scaling extraction |
| `let x=f() in x+x` | Retain; not a translation |
| `let x=f() in x+c` | Retain initially; no let-body analysis required |
| `let _=f() in 0` | Retain the consumer; internal additions are not final output |
| Two unresolved recursive operands | Retain pending work; no summaries |

A non-additive outer frame blocks extraction of additions inside it. Some such
programs remain infinite-state; incompleteness is preferable to an unsound quotient.
`Symbolic.AffineExpr` concerns prior E samples and is not reused as an abstraction
of recursive results.

## 3. Step wrapper, replay and progress

Proposed `Finite.Reward.step` takes exactly one ordinary `Finite.step` and normalizes
its successors. Returned/rejected/failure outcomes remain unchanged. Each ordinary
successor `(p,u)` becomes `(p,normalize(u).state,normalize(u).reward)`.

Never export standalone normalization transitions. Each graph edge contains one
actual machine transition. Canonical states are checked as canonical; replay
validates both normalized destination and reward, not only the state index.

Reuse the indexed replay pattern: supplied successor indices, checked fingerprints,
structural equality on equal-key states, and one kernel proof per row. Adapt the
outcome aggregation key to `(target,reward)`. Reuse `Builder.Table` for State keys,
but add reward-aware row construction and its proofs. Native construction remains
proof-carrying; portable replay uses saved data, not the hash-based explorer.

### Concrete simulation relation

Use a proof-internal relation with external rational offset d and two phases.

**Active:** original and normalized configurations have the same active
expression/value, environment and retained inner stack. They differ in outer
translation suffixes. Both suffixes are empty or both demand a numeric result.
The nonempty canonical suffix is `[addRight 0]`; the original suffix's translation
differs by d. For pure RHS frames, retain their checked deterministic evaluations.

**Finishing:** normalized execution has delivered numeric b; original execution
finishes a finite translation suffix, returning b+d. Track remaining frames and
pure-RHS replay lengths.

Show matching probabilistic choices and weights, preservation of bindings and
rejection, and reflection of reachable original failures into reachable normalized
failures. The numeric guard prevents erasing an invalid numeric use. Rejection
discards the suffix and has zero output.

For one normalization `s -> (r,t)`, prove finite-delay comparisons both ways for
`machineOutput`, with a finite bound from the removed suffix and pure RHS witnesses:

```
machineOutput n s <= shift r (machineOutput (n+B) t)
shift r (machineOutput n t) <= machineOutput (n+B) s
```

For the full graph prove cofinal horizon comparisons, taking maxima over finitely
many branch horizons using `weightedOutput_uniform_bound`, then equality of the
increasing limits. Equality of recursive equations alone is not a correspondence
proof.

Progress is explicit:

- Stack normalization terminates structurally; RHS replay is bounded.
- Normalization does not increase `bookkeepingRank`.
- Retained bookkeeping uses existing `bookkeeping_decreases`.
- Original-only finishing decreases remaining suffix/replay length.
- Application and sampling remain genuine execution steps.

Accepted finite replay supplies original reachable-state failure freedom. Then reuse
`Proof.FiniteModel.stepMeaning`, `program_reachable_shape`, `execution_safe`,
`execution_output_eq` and `initial_reification`. The existing execution theorems do
not require the original CEK state space to be finite.

The proposed final extraction theorem is:

```
candidate.ReplayValid source subject ->
  (candidate.toModel ...).Matches (subject.program source)
```

It adds no typing, termination, absorption or integrability premise.

## 4. Integrability and moment certificates

### Reuse the existing divergent-region cut

Run the existing boundary analysis on `Model.control`. Its `ClosedDivergence`
region D is transient and closed, with neither return nor rejection reachable.
It need not be the largest no-success region; the existing cut is sufficient.
Do not add a second boundary mask just for reward moments.

Prove the reward-model counterparts of `dead_outputWithin`, `cut_outputWithin`
and `cut_outputMeasure`: induction uses `shift r 0=0`, including edges entering D.
Compute rejection statistics using original labels, not the cut's rejected labels.

### New integrability proof

From closedness and `Paths.Valid` on the stopped controller, choose a positive
uniform path-probability lower bound epsilon and a positive path-length bound L.
Show survival for kL steps is at most `(1-epsilon)^k`.

Let R bound absolute edge rewards and B bound absolute terminal values. Successful
output at exact return depth n has absolute value at most `B+n*R`.

Avoid a new infinite-path probability development. Define proof-internal
exact-return layers nu_n, where only depth zero contains immediate terminals and
positive depths propagate shifted child layers through transient states. Prove:

- `outputAt s = sum_n nu_n(s)`;
- for n>=1, layer mass is bounded by stopped-controller survival at n-1;
- nu_n is supported on `|x| <= B+n*R`.

Group depths in blocks of length L. The square integral is bounded by

```
B^2 + sum_k L * (B+(k+1)*L*R)^2 * (1-epsilon)^k < infinity.
```

Derive actual integrability of id and its square. This works with positive original
divergence probability because unsuccessful paths contribute no output. Do not copy
the old arbitrary-function integrability claim based on finite terminal support.

First establish this from closedness/path evidence. For the unconditional theorem
for every finite reward model, construct the required region and shortest paths
mathematically. Do not assume completeness of `analyze`, whose current contract is
only correctness on success.

### Equations and uniqueness

After integrability, derive for transient states:

```
h(s) = sum p * h(t)
m(s) = sum p * (m(t) + r*h(t))
v(s) = sum p * (v(t) + 2*r*m(t) + r*r*h(t))
```

Returned b has `(h,m,v)=(1,b,b²)`; rejected/cut states have `(0,0,0)`.
Prove uniqueness in order h, m, v. At each stage, previously established vectors
cancel, leaving the homogeneous equation `d=P*d` with zero boundary. Apply existing
`Proof.FiniteModel.paths_unique` unchanged. No spectral or general weighted-system
machinery is needed.

Certificates carry `dead/rank/next` plus rational h, m, v and rejection vectors.
Check sparse equations directly against reward edges. Export integrability alongside
`OutputStatistics.Matches`; derive `MemLp id 2` and use `Proof.normalized_variance`.
The source/subject binding stays unchanged. A determinized second moment describes
the determinized program, not automatically its source.

Proof order:

```
shift laws and finite output laws
 -> mass projection to control
 -> cut preservation
 -> geometric absorption bound
 -> exact-return-layer integrability
 -> unbounded moment equations
 -> sequential uniqueness
 -> checked statistics and conditional variance
```

Extraction correspondence is a separate chain, combined with this only at the final
source-bound result theorem.

## 5. Work packages and exit criteria

| Package | Proposed files/work | Exit criterion |
|---|---|---|
| A. Model and moments | `Spec/RewardModel/{Model,Results}.lean`; `Proof/RewardModel/{Measure,Integrability,Statistics}.lean`; `Finite/Reward/Solve.lean`; `Checking/RewardStatistics.lean`; `lean/Tests/RewardModel.lean` | Hand-built geometric, signed, rejection and divergence models have checked moments and integrability. Use `Proof.LinearAlgebra.solve` on I-P with successive right-hand sides. |
| B. Evaluated-operand normalization | `Finite/Reward/Normalize.lean`; `Proof/RewardModel/Normalization.lean`; `lean/Tests/RewardNormalization.lean` | Numeric guard, idempotence, finite-delay comparisons and failure reflection proved for evaluated additive suffixes. |
| C. First end-to-end result | `Finite/Reward/{Graph,Explore,Export}.lean`; `Proof/RewardModel/{Build,Replay,Execution}.lean`; `Checking/RewardModel.lean`; `lean/Tests/RewardReplay.lean`; `lean/Main.lean` | Depends on A/B. Opt-in `--additive`; geometric completes below 100 states and produces portable source-bound h=1,m=1,v=3 certificates. |
| D. Pure RHS folding | Extend normalization, proofs and tests with bounded deterministic numeric replay | `f()+c` and proved pure RHSs become finite; pending failing/effectful/nonterminating RHSs retain their semantics. |
| E. Storm and portable checking | `tools/storm.py`, reward export, sparse replay/results, `tests/test_additive.py` | Independent exact Storm vectors are checked against the original candidate; signed second RHS and tampering tests pass. |
| F. Integration and contract | Root `lean/Determinize.lean`, `lean/Determinize/Theorems.lean`, `lean/Tests/Main.lean`, `lean/test.sh`, `check.sh`, `tests/cases.toml`, `lean/finite-model-contract.md`, `lean/EXACT_COMPUTATION_PLAN.md` | New modules/tests and axiom reports are registered; old default-exporter regressions pass; limits and deferred work documented. |

B can be developed before A's analysis is complete; C requires both. D is not required
for C's first milestone. E requires C and the moment checks; integration changes
needed to build/test each package accompany it rather than waiting until the end.
Use coherent commits, not a prerequisite framework refactor or a duplicate CEK
interpreter. Exact new module splits can be adjusted while preserving these boundaries.

## 6. Storm and portable format

Keep state rewards as numerical equation right-hand sides. Direct transition reward
queries or edge-staging states are unnecessary. The certified graph still preserves
actual additive outcomes and their full-law meaning.

Export the aggregated probability controller `.tra`, terminal labels/output data,
a versioned additive-edge sidecar `(source,target,p,r)`, and reward-aware replay and
result Lean files. Bind additive mode explicitly: do not infer it from a stale
sidecar. Forward `--additive` consistently through the wrapper's export, worker and
`--compare` paths so source/subject/model selection cannot drift.

Use the current stopped-controller infrastructure, including D in `done`, and the
existing synthetic sink that lets original terminals pay once.

1. Query return and rejection probabilities, paying 1 at the respective original
   terminal labels.
2. First-moment state RHS is terminal b, or `sum p*r*h(t)` for transient states
   outside D, and zero on rejection, D and synthetic done.
3. Query positive and negative parts of that RHS separately and subtract.
4. Second-moment RHS is terminal b², or `sum p*(2*r*m(t)+r²*h(t))` at transient
   states outside D. Split its signs too: it may be negative.

For example, reward 1 followed by terminal -2 has second RHS -3 then terminal
payment 4, for total second moment 1. Squaring increments would be wrong.
Positive/negative solver components do not claim to be the positive/negative
parts of terminal output.

Check restored all-state vectors against original sparse reward equations, not just
against preprocessing. Verify normalization offsets, destinations, original step
coverage, row probabilities, pair-key aggregation, source/subject, state ordering,
boundary/path witnesses and every moment equation. Verify synthetic-sink zero values
before removing that coordinate. Reject malformed/nonfinite rational inputs and
extend axiom reports to include integrability and the source-bound result theorem.

## 7. Tests and evaluation fixtures

| Fixture | Return mass | First moment | Second moment |
|---|---:|---:|---:|
| Geometric base0, continuing `1+f()` | 1 | 1 | 3 |
| Same with base -1 | 1 | 0 | 2 |
| Base0, continuing `(-1)+f()` | 1 | -1 | 3 |
| Base0, continuing `(1+2*bernoulli[G](.5))+f()` | 1 | 2 | 13 |
| Continue .5 with reward1, return .25 with0, reject .25 | 1/2 | 1/2 | 3/2 |
| Same, replacing rejection with divergence | 1/2 | 1/2 | 3/2 |
| `f()=1+f()` | 0 | 0 | 0 |
| Reward1 then terminal -2 | 1 | -1 | 1 |
| Equiprobable reward1 then return0, or reward3 then reject | 1/2 | 1/2 | 1/2 |

Add a distinct same-target/different-reward graph fixture for edge aggregation.
Force finite random increment draws to G where their randomness is part of the test.
The determinized `uniform(0,2)+f()` fixture should give h=1,m=1,v=3; its source export
remains unsupported continuous sampling, and source analytical v is 10/3.

Boundary tests must cover:

- Divergence before invalid RHS versus invalid LHS before divergence.
- Returning call followed by invalid or rejecting RHS.
- Ignored finite result returning zero; ignored geometric result may remain incomplete.
- Duplicated recursive result, negation/scaling barriers and two unresolved calls.
- Ill-typed raw additions including zero-sum extracted offsets.
- Saved environment shadowing and shared random values.
- Both backends agreeing on ordinary finite programs.

Tamper separately with rewards, probability/reward associations, parallel edge
coverage, pure-RHS replay, errors relabeled as rejection, source/subject, cut/rank,
and moment vectors. These tests must fail certification, not merely change output.

Use temporary programs in `tests/test_additive.py` for most integration cases.
Register persistent `.det` fixtures in `tests/cases.toml`. Add the new Python module
to `lean/test.sh`, which currently lists test filenames explicitly. No explanatory
fixture comments that just restate expected output.

## 8. Separate accumulator follow-on

A tail-recursive `loop a = if flip(.5) then a else loop(a+1)` remains infinite-state
in the first build because a is stored in the environment.

A later translation-equivariance pass can use one distinguished numeric output
accumulator, direct fully applied tail calls, updates a+r with r independent of a,
and terminal output a+c. Keep a out of guards, distribution parameters, divisors,
scaling and escaping closures. Preserve evaluation order and require finite retained
control. Prove `law(loop(control,a)) = shift a (law(residualLoop(control)))`.

Original `pack.det` also needs a closed numeric query, propagation of that projection,
and removal of the irrelevant but unbounded count computation. Do not silently
claim stack normalization handles it. Bounded packing counters may stay concrete;
the optimized 28-state quotient is not required. The verified hand-built packing
moment fixtures are 118513705/10077696 and 227517128669/1612431360.

`coin_flip_unif.det` requires separate marginalization: its uniform bias is G because
it controls `flip(p)`. I independently verified that closing it as
`f () (fun u => 0)` still fails exact export on stochastic uniform. This corrects any
interpretation of the earlier benchmark tally as current compiler support.

## 9. Validation commands after implementation

Keep the existing Lean/Mathlib v4.33.1 pins. The flag and fixture below are proposed,
not currently implemented. Run build/test commands sequentially in this checkout.
Fetch the Mathlib cache only if it is missing.

```sh
(cd lean && lake build --wfail)

./run.sh --check --additive --result /tmp/additive-geometric \
  --subject source --max-states 100 tests/statistical/additive-geometric.det
(cd lean && lake env lean /tmp/additive-geometric.result.lean)

STORM_PYTHON=/tmp/determinize-reward-design-venv/bin/python \
  ./run.sh --storm tests/statistical/additive-geometric.det \
  --prefix /tmp/additive-geometric-storm --subject source \
  --additive --max-states 100 --compare

STORM_PYTHON=/tmp/determinize-reward-design-venv/bin/python ./test.sh --all
STORM_PYTHON=/tmp/determinize-reward-design-venv/bin/python ./check.sh lean det
```

The temporary Storm environment exists in this investigation and contains the pinned
stormpy 1.14.0; on another machine install `tools/storm-requirements.txt` in a fresh
venv. Full `./check.sh --all` additionally checks unrelated simulator/paper surfaces
and is appropriate at repository-wide closeout if required, rather than after every
small reward-model change. Exported theorem axiom checks must admit only the existing
standard axioms, with no sorry, compiler-trust shortcuts or new axioms.

Completion requires finite extraction, full-law/domain-safety correspondence, proved
successful-output moment integrability, exact statistics and independent Storm
certificates, while preserving old exporter behavior. This document is a plan;
none of the proposed Lean results has been implemented or kernel-checked yet.
