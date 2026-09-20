# GPT-6 Pro: addition-only implementation plan response

Conversation: https://chatgpt.com/c/6aaeec02-85e4-83ea-be66-940d8463a00c
Model verified as 6 Pro. Response completed after 16m12s on 2026-09-19.
Input: the 300-file complete first-party context bundle listed in
addition-context-manifest.txt, plus the prior geometric/affine discussion and an
explicit instruction narrowing this build to addition-only extraction.

This records the substantive answer, rather than a verbatim UI transcript. The
full assessed implementation plan is [addition-plan.md](addition-plan.md).
All new RewardModel/Finite.Reward declarations are proposals, not existing APIs.

## Pro's recommendation

> Build a separate additive reward model, with checked normalization layered over
> the existing Finite.step. Ship continuation factoring first. Put accumulator
> elimination—and therefore extraction of the original packing benchmark—in a
> separate follow-on.

The old finite model's finite-terminal-support proof establishes integrability for
every observable and cannot be reused for unbounded additive output. Keep that
model/exporter intact. Add successful-output reward semantics by translating child
measures on edges; shift of the zero measure is zero. Preserve parallel outcomes
with distinct (target,reward) pairs. Use the ordinary probability controller for
termination and boundary analysis.

## Normalization

Scan the maximal outer suffix of the existing stack, initially recognizing only
`.right .add (.number c)`. Sum its constants, move that sum onto the edge, and
replace the nonempty suffix by one `.right .add (.number 0)` frame. Retain this
numeric-result check even when constants cancel to zero. Empty suffixes and
rejection remain unchanged. Normalization must be idempotent. Accumulated rewards
never enter the state key.

This handles computed and random left operands after ordinary evaluation has
returned their numeric values, without literal-pattern matching. Keep all remaining
environments/closure payloads structurally and retain any non-additive consumer.
`x+x` is scaling, not translation; let-body recognition is not needed initially.
Do not repurpose the symbolic E-sample affine evaluator.

In a later small commit, recognize `.left .add rhs savedEnvironment` using bounded
concrete replay of Finite.step from rhs with empty stack. Accept only deterministic
probability-one steps and numeric return, initially excluding all sampling evidence.
Use a fixed per-frame budget. Failure/exhaustion means not foldable, never an early
program error. Thus loop_forever()+(1/0) remains domain-safe divergence, whereas
(1/0)+loop_forever() fails before recursion.

## Correspondence

Each normalized step wraps one actual Finite.step and normalizes its successors.
No normalization-only graph edges. Replay checks both destinations and rewards.
Reuse structural lookup/fingerprints and per-row indexed kernel checks, grouping by
(target,reward).

A proof-only relation has an external offset and active/finishing phases. Active
configurations share current expression/value, environment and retained inner
stack; their outer numeric-demanding suffixes differ by the offset. In finishing,
the normalized machine has a numeric answer and the original executes its finite
remaining suffix/replay. Preserve probability choices, rejection and bindings;
reflect original reachable failures into normalized ones.

Prove finite-delay comparisons of machine-output approximants in both directions,
then cofinal graph/original horizons and equality of unbounded laws. Reuse
weightedOutput_uniform_bound. Normalization scans a finite stack and does not
increase bookkeepingRank; retained bookkeeping decreases that rank; finishing
decreases its remaining witness length. Calls/sampling remain genuine steps.
Then reuse existing stepMeaning, execution_safe, execution_output_eq and
initial_reification. The source-bound Matches theorem has no new termination,
typing, absorption or integrability premise.

## Integrability and equations

Reuse the current closed divergent region D: states from which neither return nor
rejection is reachable. A larger no-success cut is unnecessary. Reward-model cut
preservation follows because shifted zero laws remain zero.

After cutting D, finite descending positive paths give a uniform positive chance
of absorption in L steps. Survival has a geometric block bound. Bounded local
rewards and terminal values bound successful output at depth n by B+nR.

Define exact-return layers instead of developing an infinite-path probability
space. Bound each layer's mass by survival and its support by B+nR. A convergent
polynomial-times-geometric series proves actual first/square integrability. Prove
this first from boundary/path witnesses; for an unconditional theorem construct
mathematical shortest-path witnesses, without assuming analyze is complete.

Derive h, m, v equations with r*h(target) and second-moment cross terms. Establish
uniqueness sequentially by applying existing paths_unique unchanged three times.
Use existing Proof.LinearAlgebra.solve and normalized_variance; explicitly export
integrability as well as OutputStatistics.Matches. Preserve source/subject binding.

## Work packages

A. Reward semantics, shifts, mass projection, cut, geometric-tail integrability,
   moments, equation checking and native solving on handwritten graphs.
B. Evaluated-operand normalization with guard, idempotence and correspondence.
C. Proof-carrying exploration, indexed replay, portable export and opt-in --additive;
   first end-to-end geometric certificate below 100 states, h=1,m=1,v=3.
D. Pure RHS folding through bounded concrete replay.
E. Storm successive state-reward queries and independent certificate checking.
F. Build/test/axiom registration and documentation, with old defaults preserved.

New specification surface mainly Spec/RewardModel/{Model,Results}.lean. New proof
modules for Measure, Integrability, Statistics, Normalization, Build, Replay and
Execution. Executable modules in Finite/Reward and checking facades in Checking.
The detailed files, dependency order and exit criteria are in addition-plan.md.

## Storm and tests

Preserve certified additive edges in a versioned sidecar; aggregate only transition
probabilities in .tra. Explicitly bind additive mode rather than detect a stale
sidecar. Use the existing stopped controller and state-reward mechanism to query
h/rejection, then first RHS sum p*r*h, then second RHS sum p*(2r*m+r²*h), with
appropriate terminal payments. Split both first and second RHS signs. Check the
original coupled equations, not merely Storm preprocessing. Original rejected
labels remain distinct from the cut. Keep synthetic-sink dimension/zero checks.

Pro supplied exact fixtures for geometric, signed base/increments, random increments,
rejection/divergence, negative second RHS and reward/target correlation; plus
same-target reward outcomes, shadowing, numeric errors, ignored results, delayed
failing RHS, scaling barriers and malformed certificates. Preserve all old finite
regressions. Add test_additive.py explicitly to lean/test.sh and register persistent
.det files in cases.toml.

## Follow-ons and correction

Tail-recursive accumulator translation-equivariance is a separate pass, restricted
to one output-only numeric accumulator updated by independent increments. Packing
also needs a closed numeric projection and elimination of an irrelevant unbounded
count. Keep bounded packing counters concrete; no optimal control quotient required.

coin_flip_unif.det is not an initial additive benchmark: closing the function does
not eliminate its G uniform bias, which controls flip. Separate marginalization is
required. Use direct flip(.5), or the fresh additive uniform-cost determinization
fixture, for this build.

## Independent assessment

- Verified referenced existing declarations and module paths: finiteLaw_sound,
  initial_reification, normalized_variance, massBalance, Builder.Table,
  execution_safe/output_eq, Paths.Valid/paths_unique, boundary machinery,
  indexed replay, sparse equation checks and rational solver.
- Existing theorem namespaces are Proof.FiniteModel (Execution and Boundary are
  filenames, not nested namespaces). The assessed plan uses actual namespaces.
- Confirmed Lean and Mathlib v4.33.1 pins, explicit Python-test registration and
  source/subject forwarding. The proposed --additive flag does not yet exist.
- Independently closed coin_flip_unif as f () (fun u => 0) and ran the current
  exporter: uniform remains G, and export fails at state18 on stochastic uniform.
- Adjusted illustrative commands to the installed investigation Storm environment,
  and made cache fetching conditional. No implementation/test suite was run as if
  the proposed feature already existed.
- Added explicit consistent normalizer-budget binding and additive-mode forwarding
  through export/worker/compare paths as integration requirements.

No follow-up was necessary: the plan resolves the main proof strategy, preserves
the requested scope, and provides a concrete first end-to-end milestone.
