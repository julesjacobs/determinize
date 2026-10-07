import { formatNumber } from "./format.ts";

/**
 * What Lean's CLI reports about the runs of one program, as `summarize` in lean/Main.lean computes
 * it: the counts of rejected and failed runs, the first failure, the first returned value, and
 * Welford's running mean and sum of squared deviations of the returned numbers, in run order.
 */
export interface Summary {
  runs: number;
  rejected: number;
  failed: number;
  firstFailure: string | null;
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
  firstFailure: null,
  firstValue: null,
  count: 0,
  mean: 0,
  m2: 0,
};

/** Runs in run order: the number each returned, or NaN if it returned none. */
export interface Runs {
  values: Float64Array<ArrayBuffer>;
  rejected: number;
  failed: number;
  firstFailure: string | null;
  firstValue: string | null;
}

/** `summary` followed by `runs`. */
export function addRuns(summary: Summary, runs: Runs): Summary {
  let { count, mean, m2 } = summary;
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
    firstFailure: summary.firstFailure ?? runs.firstFailure,
    firstValue: summary.firstValue ?? runs.firstValue,
    count,
    mean,
    m2,
  };
}

export interface Stats {
  /** The number of runs that returned a number. */
  n: number;
  mean: number;
  /** The population variance, with n in the denominator, as Lean's CLI computes it. */
  variance: number;
  /** The standard error of the mean, from the sample variance with n - 1 in the denominator. */
  standardError: number;
}

export function statsOf(summary: Summary): Stats {
  const n = summary.count;
  return {
    n,
    mean: n > 0 ? summary.mean : NaN,
    variance: n > 0 ? summary.m2 / n : NaN,
    standardError: n > 1 ? Math.sqrt(summary.m2 / (n * (n - 1))) : NaN,
  };
}

/** The source's variance over the determinized program's, and what it means for sample sizes. */
export function varianceRatio(originalStats: Stats, determinizedStats: Stats) {
  const originalVariance = originalStats.variance;
  const determinizedVariance = determinizedStats.variance;
  if (
    originalStats.n < 2 ||
    determinizedStats.n < 2 ||
    !Number.isFinite(originalVariance) ||
    !Number.isFinite(determinizedVariance)
  ) {
    return {
      value: NaN,
      explanation: "Run at least two samples to estimate variance and sample savings.",
    };
  }
  if (originalVariance === 0 && determinizedVariance === 0) {
    return {
      value: NaN,
      explanation:
        "Both estimators have zero observed variance, so there is no sample reduction to estimate.",
    };
  }
  if (determinizedVariance === 0) {
    return {
      value: Infinity,
      explanation:
        "The determinized estimator has zero observed variance, so it needs only one sample here; the sample reduction is effectively unbounded.",
    };
  }
  if (originalVariance === 0) {
    return {
      value: 0,
      explanation:
        "The original estimator has zero observed variance here, so determinization shows no sample reduction on this run.",
    };
  }
  const ratio = originalVariance / determinizedVariance;
  return {
    value: ratio,
    explanation: `For the same mean accuracy, the determinized program needs about ${formatNumber(1 / ratio)}x as many samples, i.e. about ${formatNumber(ratio)}x fewer samples.`,
  };
}
