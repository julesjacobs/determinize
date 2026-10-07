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
  evalAffine,
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
 * in the symbols the parameters depend on. */
export function meanDistribution(kind: MeanKind, args: Affine[]): Affine {
  if (args.every(isConcreteAffine)) {
    return { constant: leanSample(kind, args.map(affineToNumber), true, meanStream), terms: {} };
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

export function instantiateArgs(args: Affine[], env: Map<string, number>): number[] {
  return args.map((arg) => evalAffine(arg, env));
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
