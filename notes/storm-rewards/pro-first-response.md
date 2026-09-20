# GPT-6 Pro: initial geometric-case analysis

Source: https://chatgpt.com/c/6aaeec02-85e4-83ea-be66-940d8463a00c
Received 2026-09-19, after 7m17s. Substantive answer summarized below; this is
Pro's proposed analysis, with independent checks recorded separately in design.md.

- The geometric recursion admits two states with transition reward 1 on the
  continuing edge of probability 1/2 and reward 0 on the exit. Mean 1, second
  moment 3, variance 2. A state-reward encoding preserving successful path totals
  can introduce a separate tick state.
- Replacing each outgoing transition reward by its probability-weighted state
  average preserves the mean but not the output law. Here reward 1/2 per call
  gives accumulated value `(N+1)/2`, whose second moment is 3/2 rather than 3.
- The fundamental identity is `M(c+e)=M(e)+c*H(e)`, where H is return mass.
  `if flip(.5) then 0 else 1+reject` has zero first output moment, but emitting
  the 1 eagerly gives ordinary expected reward 1/2. Multiplying by overall
  return probability does not fix the lost correlation.
- `f()=1+f()` has no successful output and therefore zero terminal-output
  expectation, although its ordinary accumulated reward is infinite.
- Reachability reward queries conventionally return infinity if the target is not
  reached almost surely. The current adapter must cut/target divergent regions.
  Independent source inspection confirms that it already does this for the
  existing terminal-only encoding; this is not a newly discovered existing bug.
- An extraction is output-directed: emitting global rewards for `let _=f() in 0`
  would change its mean. Retained state must determine future control,
  probabilities, arguments, rejection, and any remaining computations.
- Splitting positive/negative increments can calculate a signed mean when both
  accumulated parts are integrable, but does not preserve positive/negative
  parts of the final output. With base -1, terminal output is `N-1`; expected
  terminal positive/negative parts are each 1/2, whereas increment parts are 1.
- Squaring increments does not calculate the square of accumulated output.
  Preserve reward/successor correlation and solve a second moment equation with
  cross terms. For general success-gated additive output:
  `h_s=sum p*h_t`, `m_s=sum p*(r*h_t+m_t)`,
  `v_s=sum p*(r²*h_t+2r*m_t+v_t)`.
- A proof can relate a concrete configuration with pending additions to a reduced
  control state plus an external offset. Successful output is offset plus reduced
  output; rejection and nontermination contribute no output. Ordinary equality
  of states/output laws does not justify simply dropping stack frames from keys.
- Pro initially recommended an almost-sure-successful additive fragment and then
  generalizing to rejection. The user requested a principled general affine design;
  the follow-up explicitly challenges this narrow initial architecture.

Follow-up submitted with the 67 KB source bundle: general affine continuation
kernels, nonconstant coefficients, integrability, weighted equations/Storm scaling,
certificate architecture, and evaluation examples. Generation visibly started.
