# Exact benchmark tally

This table tracks whether each benchmark can be determinized, either fully or partially, and whether it can be represented as a finite-state model for Storm. This list is intended to grow as more benchmark programs are evaluated.

| Program | Determinize? | Storm? | Reason | Citation |
| --- | --- | --- | --- | --- |
| `2dwalk.det` | Yes, partially | No | Step length can become `1`, but direction remains random. `distance` never changes, so the walk is nonterminating and positions are unbounded. | [k-induction] |
| `bayesian.det` | No | No | Every draw influences later control flow. The literal `while true` and decreasing, unbounded `n` provide no terminal result. | [k-induction] |
| `ber.det` | No | Yes | The Bernoulli result controls progress, but `x` stays within a finite range for fixed `n`. | [k-induction] |
| `c4B_t303.det` | No | Yes | The categorical result controls loop state, but the integer coordinates decrease within a finite reachable range. | [k-induction] |
| `condand.det` | No | Yes | The choice controls which guard variable decreases; both variables remain bounded. | [k-induction] |
| `fcall.det` | Yes, fully | No | The update can become `x + 0.5`, but the loop never terminates and `x` is unbounded. | [k-induction] |
| `geo.det` | No | Yes, with reward encoding | The choice controls termination. Encode each failure as reward `1` instead of storing the unbounded `x`. | [k-induction] |
| `grid.det` | No | Yes | The choice controls the state, but `a` and `b` are explicitly bounded by `0..10`. | [k-induction] |
| `hyper.det` | No | Yes | The draw controls progress, but `x` is bounded for fixed `n`; zero increments become self-loops. | [k-induction] |
| `k-geo.det` | No | Yes, with reward encoding | The choice controls progress. The unbounded failure count can be represented as accumulated reward rather than state. | [k-induction] |
| `linear01.det` | No | Yes | The choice affects the loop guard, but `x` only decreases and has finitely many reachable values. | [k-induction] |
| `prdspeed.det` | No | Yes | Choices affect progress, but `x` and `y` remain bounded for fixed `n` and `m`. | [k-induction] |
| `prdwalk.det` | No | Yes | Both draws affect `x`, but only finitely many values are reachable before crossing fixed `n`. | [k-induction] |
| `rabin.det` | No | Yes, trivially | As written, `phase = 0` makes the initial guard `phase == 1` false, so the loop is unreachable. | [k-induction] |
| `race.det` | No | No | Both draws influence termination, and `h-t` has an unbounded reachable range. | [k-induction] |
| `rdwalk.det` | No | No | The choice affects termination, and repeated `-1` steps make `x` unbounded below. | [k-induction] |
| `retransmission.det` | No | No | The Bernoulli controls updates; the loop never exits and its counters are unbounded. | [k-induction] |
| `sprdwalk.det` | No | Yes | Same structure as `ber`: finite bounded progress with possible self-loops. | [k-induction] |
| `unif_gen.det` | No | Yes | The coin affects control flow, but the algorithm maintains bounded `v` and `c` for fixed finite bounds. | [k-induction] |
| `zero-conf.det` | No | Yes in principle | All variables are bounded, but `curprobe` allows roughly \(10^8\) values, making explicit Storm exploration likely impractical. | [k-induction] |
| `biasdir1_0.det` | No | Yes | Both choices affect the loop state and termination, but `x` and `y` remain Boolean. | [EXIST] |
| `bin01_0.det` | Yes, fully | Yes | Each choice can become the expected update `x + 0.5 * y`; the fixed `n` iterations give a finite deterministic target. | [EXIST] |
| `detm1_0.det` | Yes, trivially | Yes | The program is already deterministic, and `x` increases only until it exceeds `10`. | [EXIST] |
| `duel1_0.det` | No | Yes, after removing irrelevant state | The choices control termination. The unbounded `n` counter is irrelevant to the stated post `t`; removing it leaves finite Boolean state. | [EXIST] |
| `fair1_0.det` | No | Yes | The choices control termination and `count`, but continuing iterations leave all state unchanged and a terminating iteration increases `count` by at most two. | [EXIST] |
| `gambler01_0.det` | No | Yes, with reward encoding | The choice controls the bounded gambler position. Encode downward moves as reward `1` instead of storing the unbounded counter `z`. | [EXIST] |
| `geo01_0.det` | No | Yes, with reward encoding | The choice controls termination. Encode each continuing iteration as reward `1` instead of storing the unbounded `z`. | [EXIST] |
| `geoar01_0.det` | No | Yes, with reward encoding | The choice controls termination. Repeated additions to `x` can be represented as transition rewards, leaving finite control state for concrete inputs. | [EXIST] |
| `linexp1_0.det` | No | Yes | The sampled bits control conditional updates, but the loop has a fixed finite `n` horizon and all variables have finite reachable ranges. | [EXIST] |
| `mart1_0.det` | No | Yes, with abstraction and reward encoding | The choice controls termination. For the stated post `rounds`, retain only whether `b` is positive and count iterations as rewards. | [EXIST] |
| `prinsys1_0.det` | No | Yes | The choices control termination and the result, but `x` remains in the finite set `{0, 1, 2}`. | [EXIST] |
| `revbin1_0.det` | No | Yes, with reward encoding | The choice controls progress of bounded `x`; encode each iteration as reward `1` instead of storing unbounded `z`. | [EXIST] |
| `sum01_0.det` | Yes, fully | Yes | Each choice can become the expected update `x + 0.5 * n`; `n` then decreases deterministically to zero. | [EXIST] |
| `add-uniform-with-counter-large-bounded.det` | Yes, partially | Yes, with reward encoding | Split `cur` at `0.5`: the branch remains random, while the conditional values can become their means `0.25` and `0.75`. The fixed counter bound gives finite control state, and `score(0)` can be represented as rejection. | [GuBPI] |
| `add-uniform-with-counter-large-unbounded.det` | Yes, partially | Yes, with reward encoding | Split `cur` at `0.5`: the branch remains random, while the conditional values can become their means `0.25` and `0.75`. Only `y = 0, 1, 2` is reachable, and repeated failures become finite-state self-loops. | [GuBPI] |
| `beauquier-etal-3-Q1.det` | No | Yes, with abstraction | The threshold draws control the Boolean process state. Replacing them with Bernoulli choices and tracking only whether an iteration occurred removes the unbounded `count` while preserving the stated query. | [GuBPI] |
| `cav-example-5-simplified.det` | Yes, fully | Yes, trivially | The subtraction and addition of the same `bet` cancel, so the sample is irrelevant. The result is a one-state nonterminating loop; Storm can represent it, but there is no terminating return value. | [GuBPI] |
| `cav-example-5.det` | No | No | The continuous `bet` affects future money, termination, and the returned iteration count. Money has infinitely many reachable values, so thresholding the other draws does not yield a finite-state model. | [GuBPI] |
| `cav-example-7.det` | No | Yes, with reward encoding | The threshold draw controls progress and cannot be replaced by its mean. It is exactly a state-dependent Bernoulli choice over finite `x = 0..5`; iterations can be transition rewards. | [GuBPI] |
| `example-book-simple-Q1.det` | No | No | The target, initial value, and noise affect stopping and the final count query. Although the horizon is bounded, their continuously many reachable values prevent a finite-state Storm model. | [GuBPI] |
| `example-cart-Q1.det` | No | No | Continuous steering and position samples affect later guards and the final count query. The bounded iteration count does not eliminate the continuously many reachable states. | [GuBPI] |
| `example-ckd-epi-simple-Q1.det` | Yes, partially | No | The unused `age` and `ageErr` samples can be removed, but the remaining continuous samples affect branches and the final predicate. Those continuously many values prevent a finite-state model. | [GuBPI] |
| `example-fig6-Q1.det` | No | No | The continuous increment controls loop termination, while the threshold draw controls the returned counter. The unbounded continuous `x` state prevents finite-state Storm translation. | [GuBPI] |
| `example-fig7-Q1.det` | No | Yes, with abstraction | The threshold draw controls the number of doublings. Rewrite it as a Bernoulli choice, discard unused `lnX`, and merge all `x > 1000` values to preserve the stated Boolean query in finite state. | [GuBPI] |
| `example4-Q1.det` | No | No | Both continuous inputs influence branch decisions and the final predicate. Their continuum of possible values remains even though the program has no loop. | [GuBPI] |
| `example5-Q1.det` | No | No | All three continuous inputs influence the conditional update and final predicate, leaving a continuum of states that Storm cannot enumerate exactly. | [GuBPI] |
| `growing-walk.det`* | No | No | The stopping draw controls recursion, and each continuous step affects both the result and its soft likelihood. Soft scoring and the unbounded continuous accumulated value prevent a finite-state Storm model. | [GuBPI] |
| `herman-3-Q1.det` | No | Yes, with abstraction | The threshold draws control the finite three-bit protocol state. Rewrite them as Bernoulli choices and retain only whether `count` is zero to preserve the stated query. | [GuBPI] |
| `param-estimation-recursive.det`* | No | No | The continuous prior, steps, and direction choices affect control, likelihood, or the returned posterior value. Soft scoring and continuous unbounded walk values preclude a finite-state model. | [GuBPI] |
| `pedestrian.det`* | No | No | The continuous start, steps, and directions affect the walk likelihood, and the start is returned. The soft score and continuously many positions prevent finite-state Storm translation. | [GuBPI] |
| `random-box-walk.det` | No | No | Each sampled `s` determines direction, step size, future termination, and the output. The bounded interval still contains infinitely many reachable positions. | [GuBPI] |
| `tug-of-war-Q1.det` | No | Yes | Every threshold event affects the final comparison, so the choices must remain random. Rewriting the uniform guards as finite Bernoulli choices yields a finite acyclic model. | [GuBPI] |
| `binaryGmm.det` | No | No | Both continuous means affect every soft likelihood, and `mu1` is returned. Neither draw can be replaced by its mean, and the model retains continuously many states. | [GuBPI] |
| `coinBias.det` | No | No | The continuous `bias` determines each random branch, every failed match is rejected, and `bias` is returned. The resulting conditioned model has continuously many bias values. | [GuBPI] |
| `max.det` | No | No | Both Gaussian draws determine the comparison and the returned maximum, so neither can be replaced by its mean. Their continuous values prevent finite-state exploration. | [GuBPI] |
| `nealsFunnel.det` | Yes, fully | Yes | Every sampled `y` has conditional mean `0`, including returned `y0`; the unused draws can also be removed. The resulting program deterministically returns `0`. | [GuBPI] |
| `smallLikelihood.det` | No | No | The continuous `mu` affects every soft likelihood and is returned. It must remain random, leaving continuously many states. | [GuBPI] |
| `pd-beta-v1.program` | No | No | `prob(0.5)` updates `p_pos`, determining exit time, `p_dis`, terminal likelihood, and the return. Both state variables have infinitely many reachable values. | [Wang et al.] |
| `pd-beta-v2.program` | No | No | The file is identical to `pd-beta-v1.program`; its coin controls exit, likelihood, and return, with infinitely many reachable states. | [Wang et al.] |
| `pd-beta-v3.program` | No | No | The file is identical to `pd-beta-v1.program`; its coin controls exit, likelihood, and return, with infinitely many reachable states. | [Wang et al.] |
| `pd-beta-v4.program` | No | No | The file is identical to `pd-beta-v1.program`; its coin controls exit, likelihood, and return, with infinitely many reachable states. | [Wang et al.] |
| `pd-v1.program` | No | No | `prob(0.5)` controls the exit time, accumulated `p_dis`, Beta likelihood, and returned `p_pos`; both state variables are unbounded. | [Wang et al.] |
| `pd.program` | No | No | `prob(0.5)` controls the exit time, accumulated `p_dis`, normal likelihood, and returned `p_pos`; both state variables are unbounded. | [Wang et al.] |
| `pdld.program` | No | No | `prob(0.5)` controls exit, `p_dis`, the normal likelihood, and returned `p_pos`. Its larger variance does not bound the state space. | [Wang et al.] |
| `pdmb-v3.program` | Yes, trivially | No | No sampling occurs: `r_0`–`r_4` are free real inputs. Some inputs cause divergence or unbounded state, so this is not a closed finite model. | [Wang et al.] |
| `pdmb-v4.program` | Yes, trivially | No | No sampling occurs: `r_0`–`r_5` are free real inputs. The loop or `p_dis` may be unbounded, so this is not a closed finite model. | [Wang et al.] |
| `pdmb-v5.program` | No | No | Region-specific coins update `p_pos`, thereby controlling the guard, `p_dis`, terminal score, and return. Position and distance are unbounded. | [Wang et al.] |
| `phylogenetic.det` | Yes, partially | No | `birth ~ Uniform(0,0.01)` only changes unused `amount`, so it becomes `0.005`. Continuous `lambda` controls scored branching and the return; `wait` controls exit. | [Wang et al.] |
| `2d_robot.prog` | No | No | Direction and `Uniform(1,3)` steps update `x-y`, which is both loop guard and cost. This difference has continuous, unbounded reachability. | [Chatterjee et al.] |
| `example_1.prog` | Yes, partially | No | The fair branch has expected tick `0` and expected update `y`, so it disappears. `r ~ Uniform(-1,0.5)` still controls the unbounded `x` loop. | [Chatterjee et al.] |
| `example_3.prog` | Yes, partially | No | Within the nondeterministic branch, the fair choice becomes tick `0` and unchanged `y`. Nondeterminism remains, while continuous `r` controls the unbounded loop. | [Chatterjee et al.] |
| `goods_discount.prog` | No | No | `r ~ Uniform(1,2)` updates guard variable `d`, fixing the iteration count and all costs. Bounded `n` does not remove the continuum of `d` values. | [Chatterjee et al.] |
| `pollutant_disposal.prog` | Yes, partially | No | The `0.6` choice is removable because both branches differ only by names `x`/`y`. Their continuous samples still determine cost and the unbounded update of `n`. | [Chatterjee et al.] |
| `convoy.det` | No | No | `Uniform(-2,2)` changes `a1`, then `v1`, `x1`, and guard `x1-x2`; it cannot become its mean. `while true` leaves unbounded continuous state and no return. | [Chakarov et al.] |
| `dreckon.det` | Yes, fully | Yes | Over the fixed `N = 500` horizon, step sizes become `1.5`, sensor noises become `0`, and symmetric directions average to displacement `(0,0)`. The target is finite and deterministic. | [Chakarov et al.] |
| `invpend.det` | Yes, fully | Yes | The bounded loop is affine: initial means are `-4`, `2.5`, `3`, and `0`, and both disturbance means are `0`. This gives one finite deterministic trajectory. | [Chakarov et al.] |
| `pack.det` | Yes, partially | Yes, with reward encoding | Weight noises become `0.1` or `0.05`; object type stays random because it controls bounded counters and exit. Attempts, count, and weight become rewards. | [Chakarov et al.] |
| `roulette.det` | No | No | `rand(5,10)` and every flip update guard variable `money`; `i` counts an unbounded number of rounds. Reachable money is continuous and unbounded. | [Chakarov et al.] |
| `track.det` | Yes, partially | No | The guard is always true because `tgtVal-curVal <= 5 || tgtVal-curVal >= -5`; initial `curVal` then cancels. Noise still controls truncation, and `count` is unbounded. | [Chakarov et al.] |

[k-induction]: https://github.com/probing-lab/polar/tree/master/benchmarks/k_induction
[EXIST]: https://github.com/moves-rwth/cegispro2/tree/main/cegispro2/benchmarks/TACAS23_EXIST
[GuBPI]: https://github.com/gubpi-tool/gubpi/tree/main/benchmarks/Recursive
[Wang et al.]: https://arxiv.org/pdf/2307.13160
[Chatterjee et al.]: https://research-explorer.ista.ac.at/download/17162/17182/2024_ProcACMProgLanguage_Chatterjee.pdf
[Chakarov et al.]: https://plv.colorado.edu/papers/martingales-cav13.pdf

* For the asterics: after some slight modifications:
    *  growing-walk.det: No/No → Yes, partially/No
    * param-estimation-recursive.det: No/No → Yes, fully/Yes
    * pedestrian.det: No/No → Yes, fully/Yes
