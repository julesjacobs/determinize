// The distributions of the step table's machines. A draw and the mean of concrete parameters are
// those of Lean's runtime (`sampling.ts`); the mean of parameters that depend on symbolic E draws
// is an affine form, checked against the domain where its parameters are known.
import type { DistributionKind, Expr, MeanKind } from "../compiler/ast.ts";
import type { Affine } from "./affine.ts";
import {
  affineAdd,
  affineDiv,
  affineMul,
  affineScale,
  affineToNumber,
  constantError,
  isConcreteAffine,
} from "./affine.ts";
import type { Op } from "./sampling.ts";
import { SamplingError, SplitMix64, sample } from "./sampling.ts";

/** A sampled parameter: a number, a Const or SymFloat node, or a concrete affine form. */
export type SampleArg =
  | number
  | (Expr & { constant?: undefined })
  | (Affine & { kind?: undefined });

export const floatDistributions = new Set([
  "Uniform",
  "Gauss",
  "Exponential",
  "Gamma",
  "Beta",
  "Bernoulli",
  "Poisson",
  "Discrete",
  "DiscreteList",
]);

/** A distribution's parameters outside its domain, with the message of Lean's runtime. */
export class DistributionDomainError extends Error {
  declare kind: DistributionKind;
  declare reason: string;

  constructor(kind: DistributionKind, message: string) {
    super(message);
    this.name = "DistributionDomainError";
    this.kind = kind;
    this.reason = message;
  }
}

export function isDistributionDomainError(error: unknown): error is DistributionDomainError {
  return error instanceof DistributionDomainError;
}

/**
 * The op of Lean's runtime that draws a site of `kind` with parameters `values`, and its
 * arguments, as Lean's elaborator compiles the site: `flip(p)` draws `bernoulli(p)`, and
 * `discrete(p0, …, pn)` draws from the list of all but the last weight, which takes the
 * remainder.
 */
function leanDraw(kind: DistributionKind, values: number[]): { op: Op; args: number[] } {
  switch (kind) {
    case "Uniform":
      return { op: "uniform", args: values };
    case "Gauss":
      return { op: "gaussian", args: values };
    case "Exponential":
      return { op: "exponential", args: values };
    case "Gamma":
      return { op: "gamma", args: values };
    case "Beta":
      return { op: "beta", args: values };
    case "Flip":
    case "Bernoulli":
      return { op: "bernoulli", args: values };
    case "Poisson":
      return { op: "poisson", args: values };
    case "Discrete":
      return { op: "discrete", args: values.slice(0, -1) };
    case "DiscreteList":
      return { op: "discrete", args: values };
  }
}

/** The mean draws nothing; Lean runs it on the E stream without advancing it. */
const meanStream = new SplitMix64(0n);

function leanSample(kind: DistributionKind, values: number[], mean: boolean, rng: SplitMix64) {
  const { op, args } = leanDraw(kind, values);
  try {
    return sample(op, mean, args, rng);
  } catch (error) {
    if (error instanceof SamplingError) throw new DistributionDomainError(kind, error.message);
    throw error;
  }
}

/** The distributions whose draws vary continuously with their parameters. */
const continuousDistributions = new Set<DistributionKind>([
  "Uniform",
  "Gauss",
  "Exponential",
  "Gamma",
  "Beta",
]);

/** The bound on the error of a parameter, as `Affine` and `Const` carry it. */
function argumentError(arg: SampleArg): number {
  if (typeof arg === "number") return 0;
  if (arg?.kind === "Const") return arg.error ?? 0;
  if (arg?.kind === "SymFloat") return constantError(arg.affine);
  if (arg?.constant != null) return constantError(arg);
  return 0;
}

/**
 * The first-order bound on how far `f(values)`, which is `result`, is from `f` of the exact
 * values, each of which is within its bound in `errors`: the larger change of `f` at the two ends
 * of each bound, summed.
 */
function propagatedError(
  values: number[],
  errors: number[],
  f: (values: number[]) => number,
  result: number,
) {
  let total = 0;
  errors.forEach((error, index) => {
    if (!(error > 0)) return;
    let largest = 0;
    for (const moved of [values[index] - error, values[index] + error]) {
      try {
        const change = Math.abs(f(values.with(index, moved)) - result);
        if (Number.isFinite(change)) largest = Math.max(largest, change);
      } catch (error) {
        // Outside the domain, the exact parameters can't be either.
        if (!isDistributionDomainError(error)) throw error;
      }
    }
    total += largest;
  });
  return total;
}

/**
 * The first-order bound on how far a draw is from the draw that the exact parameters give, from
 * the bounds on its parameters' errors: how far a draw from `before`, the stream's state before
 * the draw, moves when each parameter moves within its bound. A discrete draw changes only where
 * a parameter crosses a threshold, which no bound covers, so its bound is 0.
 */
export function drawError(
  kind: DistributionKind,
  args: SampleArg[],
  before: SplitMix64,
  value: number | boolean,
): number {
  if (!continuousDistributions.has(kind) || typeof value !== "number") return 0;
  return propagatedError(
    args.map(numberArg),
    args.map(argumentError),
    (values) => leanSample(kind, values, false, before.clone()),
    value,
  );
}

/** Whether a draw's bound is 0 without computing it, as for exact parameters. */
export function hasDrawError(kind: DistributionKind, args: SampleArg[]) {
  return continuousDistributions.has(kind) && args.some((arg) => argumentError(arg) > 0);
}

/** A draw from `rng`: a number, a Boolean for `flip`, and an index for `discrete`. */
export function sampleDistribution(kind: MeanKind, args: SampleArg[], rng: SplitMix64): number;
export function sampleDistribution(
  kind: DistributionKind,
  args: SampleArg[],
  rng: SplitMix64,
): number | boolean;
export function sampleDistribution(
  kind: DistributionKind,
  args: SampleArg[],
  rng: SplitMix64,
): number | boolean {
  const value = leanSample(kind, args.map(numberArg), false, rng);
  return kind === "Flip" ? 0 < value : value;
}

/** The mean of a distribution: Lean's mean of concrete parameters, and otherwise an affine form
 * in the symbols the parameters depend on; either with the bound on its error. */
export function meanDistribution(kind: MeanKind, args: Affine[]): Affine {
  if (args.every(isConcreteAffine)) {
    const values = args.map(affineToNumber);
    const mean = leanSample(kind, values, true, meanStream);
    const error = propagatedError(
      values,
      args.map(constantError),
      (moved) => leanSample(kind, moved, true, meanStream),
      mean,
    );
    return error > 0
      ? { constant: mean, terms: {}, errors: { constant: error, terms: {} } }
      : { constant: mean, terms: {} };
  }
  validateSymbolicMean(kind, args);
  switch (kind) {
    case "Uniform":
      return affineScale(affineAdd(args[0], args[1]), 0.5);
    case "Gauss":
      return args[0];
    case "Exponential":
      return affineDiv({ constant: 1, terms: {} }, args[0]);
    case "Gamma":
      return affineDiv(args[0], args[1]);
    case "Beta":
      return affineDiv(args[0], affineAdd(args[0], args[1]));
    case "Bernoulli":
    case "Poisson":
      return args[0];
    case "Discrete":
    case "DiscreteList": {
      // n + Σ (i - n) p_i: outcome n takes the remainder 1 - Σ p_i.
      const probabilities = kind === "Discrete" ? args.slice(0, -1) : args;
      const n = probabilities.length;
      return probabilities.reduce<Affine>(
        (acc, probability, index) =>
          affineAdd(acc, affineMul(probability, { constant: index - n, terms: {} })),
        { constant: n, terms: {} },
      );
    }
  }
}

function numberArg(arg: SampleArg): number {
  if (typeof arg === "number") return arg;
  if (arg?.kind === "Const") return arg.value;
  if (arg?.kind === "SymFloat") return affineToNumber(arg.affine);
  if (arg?.constant != null) {
    if (!isConcreteAffine(arg)) throw new Error("expected concrete distribution argument");
    return arg.constant;
  }
  throw new Error(`expected numeric argument, got ${JSON.stringify(arg)}`);
}

/**
 * Lean's domain checks of `sample`, as far as they apply to the parameters that are concrete;
 * a parameter that depends on symbols passes. Lean checks the same conditions once the symbols
 * have values.
 */
function validateSymbolicMean(kind: MeanKind, args: Affine[]) {
  const finite = (arg: Affine) =>
    Number.isFinite(arg.constant) && Object.values(arg.terms).every(Number.isFinite);
  if (!args.every(finite)) {
    throw new DistributionDomainError(kind, "nonfinite distribution parameter");
  }
  const value = (index: number) => (isConcreteAffine(args[index]) ? args[index].constant : null);
  const fails = (index: number, outside: (x: number) => boolean) => {
    const x = value(index);
    return x !== null && outside(x);
  };
  switch (kind) {
    case "Gauss":
      if (fails(1, (v) => v < 0)) {
        throw new DistributionDomainError(kind, "gaussian requires variance ≥ 0");
      }
      return;
    case "Bernoulli":
      if (fails(0, (p) => p < 0 || p > 1)) {
        throw new DistributionDomainError(kind, "bernoulli requires probability in [0,1]");
      }
      return;
    case "Poisson":
      if (fails(0, (rate) => rate < 0)) {
        throw new DistributionDomainError(kind, "poisson requires rate ≥ 0");
      }
      return;
    case "Exponential":
      if (fails(0, (rate) => rate <= 0)) {
        throw new DistributionDomainError(kind, "exponential requires rate > 0");
      }
      return;
    case "Gamma":
      if (fails(0, (x) => x <= 0) || fails(1, (x) => x <= 0)) {
        throw new DistributionDomainError(kind, "gamma requires positive shape and rate");
      }
      return;
    case "Beta":
      if (fails(0, (x) => x <= 0) || fails(1, (x) => x <= 0)) {
        throw new DistributionDomainError(kind, "beta requires positive parameters");
      }
      return;
    case "Discrete":
    case "DiscreteList":
      if (args.some((_, i) => fails(i, (p) => p < 0))) {
        throw new DistributionDomainError(kind, "discrete requires nonnegative probabilities");
      }
      return;
    case "Uniform":
      return;
  }
}

export function distributionName(kind: string) {
  if (kind === "DiscreteList") return "discrete_list";
  return kind === "Gauss" ? "gauss" : kind.toLowerCase();
}
