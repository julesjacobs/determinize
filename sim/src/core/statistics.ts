import { formatNumber } from "./format.ts";

export interface Stats {
  n: number;
  mean: number;
  /** The sample variance, with n - 1 in the denominator. */
  variance: number;
  /** The standard error of the mean. */
  standardError: number;
}

export function sampleStats(values: number[]): Stats {
  const n = values.length;
  const mean = values.reduce((sum, value) => sum + value, 0) / n;
  const variance =
    n < 2 ? NaN : values.reduce((sum, value) => sum + (value - mean) ** 2, 0) / (n - 1);
  return {
    n,
    mean,
    variance,
    standardError: Number.isFinite(variance) ? Math.sqrt(variance / n) : NaN,
  };
}

/** The source's variance over the determinized program's, and what it means for sample sizes. */
export function varianceRatio(originalStats: Stats, determinizedStats: Stats) {
  const originalVariance = originalStats.variance;
  const determinizedVariance = determinizedStats.variance;
  if (!Number.isFinite(originalVariance) || !Number.isFinite(determinizedVariance)) {
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
