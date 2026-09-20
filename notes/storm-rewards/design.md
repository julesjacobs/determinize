# Additive special case and evaluation examples

Supporting derivation for [the general affine design](affine-design.md). The
constant-addition extraction below is an initial special case, not the proposed
architecture. No production implementation changes.

## Recommendation

Add a separate additive-reward execution mode. Retain finite control and data that
influence future execution; remove an additive output accumulator from state.
Support additive return continuations first, then accumulator parameters.
Do not attach rewards to arbitrary syntactic additions.

For `f () = if flip(p) then 0 else 1 + f ()`, the macro model is
`loop --(1-p, reward 1)--> loop` and `loop --(p, reward 0)--> done`.
At `p = 1/2`, the first moment is 1, second moment 3, and variance 2.
The direct CEK explorer creates a different state for every pending addition.
Its structural equality cannot identify this finite reward representation.

## Preserve successful terminal output

The existing semantics is a subprobability distribution of returned values.
Rejection and divergence contribute no output mass. Define the reward model's
output law by summing rewards only along paths that successfully return,
including a terminal residual value. A rejected or infinite path has no output.
This preserves the contract while allowing infinitely many possible output values.

For finite outcome rows `(probability p, target t, additive reward r)`, define:

- `h(s)`: probability of successful return;
- `m(s)`: unnormalized first output moment;
- `v(s)`: unnormalized second output moment.

At a successful terminal with residual value `b`, set `(h,m,v) = (1,b,b²)`;
at rejection or a state with no path to a successful terminal, set all three to 0.
For other states:

```
h(s) = sum p * h(t)
m(s) = sum p * (m(t) + r * h(t))
v(s) = sum p * (v(t) + 2*r*m(t) + r*r*h(t))
```

The `h(t)` factor discards rewards on paths that later fail to return. For example,
with probability 1/2 continue and add 1, probability 1/4 return 0, and probability
1/4 reject: `h=1/2`, `m=1/2`, `v=3/2`. Ordinary accumulated rewards give mean 1.
An unconditional recursive loop that keeps adding 1 has zero terminal output mass,
but infinite ordinary accumulated reward.

After cutting the region with no path to a successful return, the remaining
finite chain reaches the boundary almost surely, with a geometric tail bound.
Finite rational outcome rows imply bounded increments. This gives finite absolute
moments of every fixed polynomial degree for successful output, even when the
original program has a positive divergence probability. Proving this replaces the
current proof based on finitely many terminal values. It is a statement about
finite additive reward models, not arbitrary recursive programs.

## Storm encoding

Storm 1.14.0 accepts exact transition reward matrices. For the almost-surely
successful fragment, ordinary transition rewards suffice for the first moment.
For the general contract, sequential state-reward queries are convenient:

1. Solve successful return probabilities `h`.
2. Solve the first-moment equation with state reward `sum p*r*h(t)`.
3. Solve the second-moment equation with state reward
   `sum p*(r*r*h(t) + 2*r*m(t))`.

Pay terminal `b` or `b²` once before entering a synthetic sink, as today. For
these queries, make rejection and the no-success region zero-valued targets.
Signed right-hand sides can be split into positive and negative parts and their
solutions subtracted, after establishing absorption in the cut chain. The cross
term can be signed even though the second moment itself is nonnegative.

Keep distinct reward outcomes that share a destination. Merging only their
probabilities and replacing rewards by a conditional mean loses second moments
and the output law. If only two moments are required, aggregate per destination
`P=sum p`, `A=sum p*r`, `B=sum p*r²`; then the equations use `P`, `A`, and `B`.
Keeping outcome lists is preferable for the initial full-output-law specification.

Return/rejection/divergence statistics must still describe the original control
chain. Cutting a region for a moment query does not turn divergence into rejection.

## Extraction boundary

Start with a numeric function whose result reaches the program output through
additive return continuations. In the CEK machine, `.right .add (.number c)` stores
`c + result`. Erase a contiguous outer suffix consisting solely of these frames,
recording its sum as emitted reward, and keep the remaining control configuration.
Record an emitted increment only once. Do not put the running total in the state
key. Keep any continuation that inspects, duplicates, discards, or nonlinearly
transforms the result. Numeric-result safety must be part of the correctness proof.

This handles `c + recursive_call` directly. `recursive_call + c` needs an additional
justified normalization of a pure constant operand; arbitrary operand reordering
would change call-by-value effects and rejection. General affine scaling, multiple
recursive calls with pending work, and higher-order recursion need further design.

A second pass can remove an accumulator parameter `a` when the loop updates it
only as `a + delta(control, fresh draws)` and returns it additively. It must not
influence guards, sampling parameters, other live parameters, or rejection.
The extracted control transition and increment must preserve their joint law.
Remove irrelevant parameters and unused closure environment entries as needed;
otherwise structural exploration can retain unbounded values indirectly.

Unbounded iteration count is fine; unbounded control state is not. A bounded
integer random walk with an additive step counter works. The one-sided unbounded
walk in `examples/loops/random_walk.det` still requires infinitely many positions.

## Repository changes and proof obligations

`Finite/Machine.lean` uses full environments and continuation stacks as state.
`Finite/Graph.lean` edges contain only probability and target; `Explore.lean`
hashes the full structural state. `Finite/Export.lean` exports terminal state
rewards, and `tools/storm.py` squares terminal outputs to obtain second moments.
None currently supports additive reward extraction.

Introduce a reward-model specification with successful-path output semantics,
a separate extraction/replay contract, and certificates for the equations above.
Do not present an abstract reward graph as an ordinary `Candidate.ReplayValid`:
its erased continuations do not satisfy the current exact-step replay contract.
Prove a simulation relating original configurations to reduced configurations and
an external additive offset. On successful return, original output equals offset
plus reduced output; rejection maps to zero measure. Account for administrative
stuttering and preservation of divergence and domain safety.

Keep the existing finite-terminal model as a zero-edge-reward special case or
separate backend until the new correspondence theorem is complete. Reuse sparse
rational equation checks and uniqueness-by-boundary-path arguments where possible.
New semantic definitions belong in `Spec/`; their proofs and implementation
internals belong outside the human-review specification boundary.

## Evaluation examples

| Example | Finite retained state | Additive output | Purpose |
|---|---|---|---|
| Geometric recursion | one loop state | failures | smallest continuation example; mean `(1-p)/p` |
| Negative binomial | successes `0..k` | failures | scalable finite control; mean `k*(1-p)/p` |
| Consecutive successes | current streak `0..k` | trials | resets and unbounded runtime; mean `(1-p^k)/((1-p)*p^k)` for `0<p<1` |
| Bounded gambler's walk | position `0..N` | steps or downward moves | unbounded runtime with bounded control; fair-walk step mean `i*(N-i)` |
| Geometric random costs | one loop state after determinization | fresh `uniform(0,2)` on each failure | continuous source becomes finite reward model; first moment 1 at `p=1/2` |
| Packing | bounded packing counters | accumulated weight or attempts | existing `pack.det`, after closing the function and projecting a numeric query |

The geometric random-cost example has source second moment `10/3` and determinized
second moment `3`. Determinization preserves the expected output under its theorem
premises; it does not in general preserve the second moment. Report which subject
each statistic describes.

`coin_flip_unif.det` currently returns a function. Close it with a base continuation
returning zero before evaluation. Its fresh random bias additionally needs valid
determinization/marginalization; reward extraction alone does not eliminate it.
The benchmark tally's “with reward encoding” entries describe proposed capability,
not current exporter support.

Recommended first experiment: implement the restricted additive-return extraction
and check that the geometric program produces a finite graph with first moment 1
and second moment 3. Then add the rejection variant, parallel reward outcomes,
signed increments, and a divergent control component before widening the fragment.

## Independently checked so far

- Current built CLI, `--subject source --max-states 100`: geometric example stops
  with 101 discovered states and no export.
- `experiment.py`, using the pinned `stormpy==1.14.0`: exact transition reward query
  gives geometric mean 1; staged moment query gives second moment 3.
- Same script: rejection example gives return mass 1/2, naive reward 1, correct
  first moment 1/2, and second moment 3/2.
- Same script: two outcomes with rewards 0 and 2 to one successor have first moment
  1 and second moment 2; replacing their rewards by mean 1 gives the wrong square.

These checks establish the small model calculations, not a Lean correctness theorem
for the proposed extraction.
