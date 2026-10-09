// A port of Lean's `Finite/Statistics.lean` with `Finite/Solve.lean`'s `solveValues`: the exact
// return mass, first and second moments of a finite model's output, its rejection probability, and
// what follows from them, as `Finite.writeResult` reports them in `.result.json`.
import type { Model } from "./model.ts";
import type { Rational } from "./rational.ts";
import { div, fraction, isZero, mul, one, sub, zero } from "./rational.ts";
import type { Boundary } from "./solve.ts";
import {
  analyze,
  cut,
  defaultSolveStates,
  eliminate,
  findPaths,
  finish,
  stateLimitMessage,
} from "./solve.ts";

/** The exact statistics of a finite model, as `.result.json` reports them. */
export interface ExactResult {
  states: number;
  /** The probability of returning: `returnProbability`. */
  returnMass: Rational;
  firstMoment: Rational;
  secondMoment: Rational;
  /** The mean given that the program returns, `returnedExpectation`; null if it never returns. */
  conditionalMean: Rational | null;
  /** The variance given that the program returns, `returnedVariance`; null if it never returns. */
  conditionalVariance: Rational | null;
  rejectionProbability: Rational;
  divergenceProbability: Rational;
  /** The largest rank of the boundary analysis, over the states: of the fewest steps from a
   * state to a terminal one, the most. */
  rankBound: number;
}

/** The statistics, or Lean's message why they couldn't be solved. */
export type Solved = { ok: true; result: ExactResult } | { ok: false; message: string };

/** The result from a model's moments, as `OutputStatistics` and `TerminationStatistics` give
 * them. */
export function exactResult(
  states: number,
  moments: { mass: Rational; first: Rational; second: Rational; rejection: Rational },
  boundary: Boundary,
): ExactResult {
  const { mass, first, second, rejection } = moments;
  const mean = isZero(mass) ? null : div(first, mass);
  return {
    states,
    returnMass: mass,
    firstMoment: first,
    secondMoment: second,
    conditionalMean: mean,
    conditionalVariance: mean === null ? null : sub(div(second, mass), mul(mean, mean)),
    rejectionProbability: rejection,
    divergenceProbability: sub(sub(one, mass), rejection),
    rankBound: boundary.rank.reduce((a, b) => Math.max(a, b), 0),
  };
}

/** As Lean's `solveStatistics` and `solveTermination`: the moments from the value equations of the
 * model with its divergent region cut off. The four equations share their matrix, so one
 * elimination solves them; it yields as `eliminate` does. */
export function* statisticsSteps(
  model: Model,
  maxStates = defaultSolveStates,
): Generator<void, Solved> {
  if (model.size > maxStates)
    return { ok: false, message: stateLimitMessage(model.size, maxStates) };
  const boundary = analyze(model);
  if (typeof boundary === "string") return { ok: false, message: boundary };
  const stopped = cut(model, boundary.dead);
  const paths = findPaths(stopped, boundary.rank);
  if (typeof paths === "string") return { ok: false, message: paths };
  const returned = (f: (reward: Rational) => Rational) =>
    stopped.kind.map((k) => (k.kind === "returned" ? f(k.reward) : zero));
  // `rejectionQuery`: a rejected state that isn't dead pays 1.
  const rejected = stopped.kind.map((k, state) =>
    k.kind === "rejected" && !boundary.dead[state] ? one : zero,
  );
  const solution = yield* eliminate(stopped, [
    returned(() => one),
    returned((r) => r),
    returned((r) => mul(r, r)),
    rejected,
  ]);
  if (!solution)
    return { ok: false, message: "singular value equations; no absorption certificate" };
  const [mass, first, second, rejection] = solution.map((values) => values[0]);
  return {
    ok: true,
    result: exactResult(model.size, { mass, first, second, rejection }, boundary),
  };
}

/** `statisticsSteps`, run to their end. */
export function solveStatistics(model: Model, maxStates = defaultSolveStates): Solved {
  return finish(statisticsSteps(model, maxStates));
}

/** The fields of `.result.json` that come from the statistics, as Lean writes them. */
export function resultFields(result: ExactResult) {
  const optional = (q: Rational | null) => (q === null ? null : fraction(q));
  return {
    answer: fraction(result.firstMoment),
    return_mass: fraction(result.returnMass),
    rejection_probability: fraction(result.rejectionProbability),
    divergence_probability: fraction(result.divergenceProbability),
    second_moment: fraction(result.secondMoment),
    conditional_mean: optional(result.conditionalMean),
    conditional_variance: optional(result.conditionalVariance),
    states: result.states,
    rank_bound: result.rankBound,
  };
}
