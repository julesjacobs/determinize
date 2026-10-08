import { formatNumber } from "./format.ts";

/**
 * Why a run failed, in the terms of the theorems' premises: an operation outside its domain
 * (division by zero or an invalid distribution parameter, as `DomainSafe` in Spec/Semantics.lean
 * names them for typed programs); a limit of the runtime, such as its fuel, which says nothing
 * about the program; a floating-point result that isn't finite, which the real-valued semantics
 * doesn't have; or an ill-typed operation, which a checked program doesn't reach.
 */
export type FailureKind = "domain" | "limit" | "float" | "type";

/** The kind of a failure, from the message of Lean's runtime. */
export function failureKind(message: string): FailureKind {
  if (
    message === "division by zero" ||
    message === "discrete probabilities sum to more than one" ||
    / requires /.test(message)
  ) {
    return "domain";
  }
  if (message === "step limit reached" || /\blimit\b/.test(message)) return "limit";
  if (message.startsWith("nonfinite ")) return "float";
  return "type";
}

/** A run that failed on an operation outside its domain: its index and Lean's message. */
export interface DomainFailure {
  run: number;
  message: string;
}

/**
 * What Lean's CLI reports about the runs of one program, as `summarize` in lean/Main.lean computes
 * it: the counts of rejected and failed runs, the first failure, the first returned value, and
 * Welford's running mean and sum of squared deviations of the returned numbers, in run order;
 * and the first runs that failed on a domain error, which the CLI doesn't single out.
 */
export interface Summary {
  runs: number;
  rejected: number;
  failed: number;
  /** The failed runs that say nothing about the program: those that a limit of the runtime
   * stopped, and those that failed in floating point, on a non-finite number or at an inexact
   * parameter or divisor of exactly 0. */
  stopped: number;
  /** The failed runs that failed at an inexact parameter or divisor of exactly 0. */
  zeroFailed: number;
  firstFailure: string | null;
  /** The first domain failure, other than one at an inexact parameter or divisor of exactly 0. */
  firstDomainFailure: DomainFailure | null;
  /** The first domain failure at an inexact parameter or divisor of exactly 0 only, which a
   * floating-point underflow can reach where the real-valued semantics doesn't. */
  firstZeroFailure: DomainFailure | null;
  firstValue: string | null;
  /** How many runs returned a number. */
  count: number;
  mean: number;
  m2: number;
}

export const noRuns: Summary = {
  runs: 0,
  rejected: 0,
  failed: 0,
  stopped: 0,
  zeroFailed: 0,
  firstFailure: null,
  firstDomainFailure: null,
  firstZeroFailure: null,
  firstValue: null,
  count: 0,
  mean: 0,
  m2: 0,
};

/** The values that runs drew at a G site, in run order: NaN where a run didn't draw there
 * exactly once. */
export interface SiteDraws {
  /** The site's index in Lean's `Expr.sites`. */
  site: number;
  values: Float64Array<ArrayBuffer>;
}

/** Runs in run order: the number each returned, or NaN if it returned none, and the values of
 * their draws at the program's continuous G sites. */
export interface Runs {
  values: Float64Array<ArrayBuffer>;
  rejected: number;
  failed: number;
  stopped: number;
  zeroFailed: number;
  firstFailure: string | null;
  /** The first domain failures of either kind, at their indices among these runs. */
  firstDomainFailure: DomainFailure | null;
  firstZeroFailure: DomainFailure | null;
  firstValue: string | null;
  draws: SiteDraws[];
}

/** `summary` followed by `runs`. */
export function addRuns(summary: Summary, runs: Runs): Summary {
  let { count, mean, m2 } = summary;
  const after = (failure: DomainFailure | null) =>
    failure && { run: summary.runs + failure.run, message: failure.message };
  for (const x of runs.values) {
    if (Number.isNaN(x)) continue;
    count += 1;
    const delta = x - mean;
    mean = mean + delta / count;
    m2 = m2 + delta * (x - mean);
  }
  return {
    runs: summary.runs + runs.values.length,
    rejected: summary.rejected + runs.rejected,
    failed: summary.failed + runs.failed,
    stopped: summary.stopped + runs.stopped,
    zeroFailed: summary.zeroFailed + runs.zeroFailed,
    firstFailure: summary.firstFailure ?? runs.firstFailure,
    firstDomainFailure: summary.firstDomainFailure ?? after(runs.firstDomainFailure),
    firstZeroFailure: summary.firstZeroFailure ?? after(runs.firstZeroFailure),
    firstValue: summary.firstValue ?? runs.firstValue,
    count,
    mean,
    m2,
  };
}

/** How a run of one program ended: it returned a value, the number it returned if any, or it
 * was rejected by an observation, or it failed, at an inexact parameter or divisor of exactly 0
 * only where `inexactZero` says so. */
export type RunOutcome =
  | { kind: "returned"; number: number | null; display: string }
  | { kind: "rejected" }
  | { kind: "failed"; message: string; inexactZero?: boolean };

/** Outcomes of runs, in run order, as `Runs`. */
export function runsOf(outcomes: RunOutcome[], draws: SiteDraws[] = []): Runs {
  const runs: Runs = {
    values: new Float64Array(outcomes.length),
    rejected: 0,
    failed: 0,
    stopped: 0,
    zeroFailed: 0,
    firstFailure: null,
    firstDomainFailure: null,
    firstZeroFailure: null,
    firstValue: null,
    draws,
  };
  for (const [i, outcome] of outcomes.entries()) {
    runs.values[i] = outcome.kind === "returned" && outcome.number !== null ? outcome.number : NaN;
    if (outcome.kind === "rejected") runs.rejected += 1;
    if (outcome.kind === "failed") {
      runs.failed += 1;
      runs.firstFailure ??= outcome.message;
      const kind = failureKind(outcome.message);
      const failure = { run: i, message: outcome.message };
      const zero = kind === "domain" && outcome.inexactZero === true;
      if (zero) runs.firstZeroFailure ??= failure;
      else if (kind === "domain") runs.firstDomainFailure ??= failure;
      if (zero) runs.zeroFailed += 1;
      if (kind === "limit" || kind === "float" || zero) runs.stopped += 1;
    }
    if (outcome.kind === "returned") runs.firstValue ??= outcome.display;
  }
  return runs;
}

export interface Stats {
  /** The number of runs that returned a number. */
  n: number;
  mean: number;
  /** The population variance, with n in the denominator and at least 0, as Lean's CLI computes
   * it. */
  variance: number;
  /** The standard error of the mean, from the sample variance with n - 1 in the denominator. */
  standardError: number;
}

export function statsOf(summary: Summary): Stats {
  const n = summary.count;
  return {
    n,
    mean: n > 0 ? summary.mean : NaN,
    variance: n > 0 ? Math.max(0, summary.m2 / n) : NaN,
    standardError: n > 1 ? Math.sqrt(summary.m2 / (n * (n - 1))) : NaN,
  };
}

/** The variance-reduction factor, as the paper's evaluation calls the source's variance over the
 * determinized program's, and what it means for the number of runs. */
export function varianceRatio(originalStats: Stats, determinizedStats: Stats) {
  const originalVariance = originalStats.variance;
  const determinizedVariance = determinizedStats.variance;
  if (originalStats.n < 2 || determinizedStats.n < 2) {
    return {
      value: NaN,
      explanation: "Run each program at least twice to estimate the variance reduction.",
    };
  }
  if (!Number.isFinite(originalVariance) || !Number.isFinite(determinizedVariance)) {
    return {
      value: NaN,
      explanation:
        "A variance is unavailable (floating-point overflow), so the variance reduction is too.",
    };
  }
  if (originalVariance === 0 && determinizedVariance === 0) {
    return {
      value: NaN,
      explanation:
        "Neither program's returned numbers vary here, so there is no variance to reduce.",
    };
  }
  if (determinizedVariance === 0) {
    return {
      value: Infinity,
      explanation:
        "The determinized program's returned numbers don't vary here: the variance-reduction factor is infinite, and one run gives its mean.",
    };
  }
  if (originalVariance === 0) {
    return {
      value: 0,
      explanation:
        "The source's returned numbers don't vary here, so determinization shows no variance reduction.",
    };
  }
  const ratio = originalVariance / determinizedVariance;
  return {
    value: ratio,
    explanation: `The source's variance is ${formatNumber(ratio)} times the determinized program's, so for the same accuracy of the mean the determinized program needs about ${formatNumber(1 / ratio)} times as many runs.`,
  };
}
