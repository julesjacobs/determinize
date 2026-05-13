import { affineAdd, affineDiv, affineMul, affineScale, affineToNumber, evalAffine, isConcreteAffine } from "./affine.js";

export const floatDistributions = new Set(["Uniform", "Gauss", "Exponential", "Gamma", "Beta", "Bernoulli", "Poisson", "Discrete"]);
const MIN_POSITIVE_SAMPLE = Number.MIN_VALUE;
const PROBABILITY_EPS = 1e-9;
const ARITIES = {
  Uniform: 2,
  Gauss: 2,
  Exponential: 1,
  Gamma: 2,
  Beta: 2,
  Flip: 1,
  Bernoulli: 1,
  Poisson: 1,
};

export class DistributionDomainError extends Error {
  constructor(kind, message) {
    super(`domain error in ${distributionName(kind)}: ${message}`);
    this.name = "DistributionDomainError";
    this.kind = kind;
    this.reason = message;
  }
}

export function isDistributionDomainError(error) {
  return error instanceof DistributionDomainError;
}

export function sampleDistribution(kind, args, rng) {
  const domain = validateSampleDomain(kind, args);
  switch (kind) {
    case "Uniform": {
      const [lo, hi] = domain;
      return lo + rng.next() * (hi - lo);
    }
    case "Gauss": {
      const [mean, variance] = domain;
      const u1 = rng.positive();
      const u2 = rng.next();
      return mean + Math.sqrt(variance) * Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
    }
    case "Exponential": {
      const [rate] = domain;
      return -Math.log(rng.positive()) / rate;
    }
    case "Gamma":
      return gammaSample(domain[0], domain[1], rng);
    case "Beta": {
      const x = gammaSample(domain[0], 1, rng);
      const y = gammaSample(domain[1], 1, rng);
      return x / (x + y);
    }
    case "Flip": {
      const [p] = domain;
      return rng.next() < p;
    }
    case "Bernoulli":
      return rng.next() < domain[0] ? 1 : 0;
    case "Poisson":
      return poissonSample(domain[0], rng);
    case "Discrete": {
      const probabilities = domain;
      const total = probabilities.reduce((a, b) => a + b, 0);
      const r = rng.next() * total;
      let acc = 0;
      for (let i = 0; i < probabilities.length; i++) {
        acc += probabilities[i];
        if (r <= acc) return i;
      }
      return probabilities.length - 1;
    }
    default:
      throw new Error(`unknown distribution ${kind}`);
  }
}

export function meanDistribution(kind, args) {
  validateMeanDomain(kind, args);
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
      return args.reduce((acc, probability, index) => affineAdd(acc, affineMul(probability, { constant: index, terms: {} })), { constant: 0, terms: {} });
    default:
      throw new Error(`no symbolic mean for ${kind}`);
  }
}

export function instantiateArgs(args, env) {
  return args.map((arg) => evalAffine(arg, env));
}

export function meanArgs(args, env) {
  return args.map((arg) => ({ constant: evalAffine(arg, env), terms: {} }));
}

function numberArg(arg) {
  if (typeof arg === "number") return arg;
  if (arg?.kind === "Const") return arg.value;
  if (arg?.kind === "SymFloat") return affineToNumber(arg.affine);
  if (arg?.constant != null) {
    if (!isConcreteAffine(arg)) throw new Error("expected concrete distribution argument");
    return arg.constant;
  }
  throw new Error(`expected numeric argument, got ${JSON.stringify(arg)}`);
}

function validateSampleDomain(kind, args) {
  const values = args.map(numberArg);
  validateConcreteDomain(kind, values);
  return values;
}

function validateMeanDomain(kind, args) {
  for (const arg of args) validateFiniteAffine(kind, arg);
  const values = args.map((arg) => (isConcreteAffine(arg) ? affineToNumber(arg) : null));
  validateConcreteDomain(kind, values, { skipSymbolic: true });
}

function validateFiniteAffine(kind, arg) {
  if (!Number.isFinite(arg.constant)) throw new DistributionDomainError(kind, "parameters must be finite");
  for (const coeff of Object.values(arg.terms)) {
    if (!Number.isFinite(coeff)) throw new DistributionDomainError(kind, "parameters must be finite");
  }
}

function validateConcreteDomain(kind, values, options = {}) {
  const skipSymbolic = Boolean(options.skipSymbolic);
  validateArity(kind, values.length);
  const concrete = values.filter((value) => value !== null);
  for (const value of concrete) {
    if (!Number.isFinite(value)) throw new DistributionDomainError(kind, "parameters must be finite");
  }

  const arg = (index) => values[index];
  const check = (index, predicate, message) => {
    const value = arg(index);
    if (value === null && skipSymbolic) return;
    if (!predicate(value)) throw new DistributionDomainError(kind, message);
  };

  switch (kind) {
    case "Uniform":
      if (!(skipSymbolic && (arg(0) === null || arg(1) === null)) && arg(0) > arg(1)) {
        throw new DistributionDomainError(kind, "lower bound must be <= upper bound");
      }
      break;
    case "Gauss":
      check(1, (value) => value >= 0, "variance must be >= 0");
      break;
    case "Exponential":
      check(0, (value) => value > 0, "rate must be > 0");
      break;
    case "Gamma":
      check(0, (value) => value > 0, "shape must be > 0");
      check(1, (value) => value > 0, "rate must be > 0");
      break;
    case "Beta":
      check(0, (value) => value > 0, "alpha must be > 0");
      check(1, (value) => value > 0, "beta must be > 0");
      break;
    case "Flip":
    case "Bernoulli":
      check(0, (value) => value >= 0 && value <= 1, "probability must be in [0, 1]");
      break;
    case "Poisson":
      check(0, (value) => value >= 0, "lambda must be >= 0");
      break;
    case "Discrete": {
      if (values.length === 0) throw new DistributionDomainError(kind, "at least one probability is required");
      for (const [index, value] of values.entries()) {
        if (value === null && skipSymbolic) continue;
        if (value < 0 || value > 1) throw new DistributionDomainError(kind, `probability ${index} must be in [0, 1]`);
      }
      if (!values.includes(null)) {
        const total = values.reduce((sum, value) => sum + value, 0);
        if (Math.abs(total - 1) > PROBABILITY_EPS) throw new DistributionDomainError(kind, "probabilities must sum to 1");
      }
      break;
    }
    default:
      throw new Error(`unknown distribution ${kind}`);
  }
}

function validateArity(kind, actual) {
  if (kind === "Discrete") return;
  const expected = ARITIES[kind];
  if (expected === undefined) throw new Error(`unknown distribution ${kind}`);
  if (actual !== expected) {
    const noun = expected === 1 ? "parameter" : "parameters";
    throw new DistributionDomainError(kind, `expected ${expected} ${noun}, got ${actual}`);
  }
}

function distributionName(kind) {
  return kind === "Gauss" ? "gauss" : kind.toLowerCase();
}

function gammaSample(alpha, beta, rng) {
  const scale = 1 / beta;
  if (alpha < 1) return positiveSample(gammaSample(alpha + 1, beta, rng) * rng.positive() ** (1 / alpha));
  const d = alpha - 1 / 3;
  const c = 1 / Math.sqrt(9 * d);
  for (;;) {
    const x = stdNormal(rng);
    const v = 1 + c * x;
    if (v <= 0) continue;
    const v3 = v * v * v;
    const u = rng.positive();
    if (u < 1 - 0.0331 * x ** 4) return positiveSample(scale * d * v3);
    if (Math.log(u) < 0.5 * x * x + d * (1 - v3 + Math.log(v3))) return positiveSample(scale * d * v3);
  }
}

function positiveSample(value) {
  return Number.isFinite(value) && value > 0 ? value : MIN_POSITIVE_SAMPLE;
}

function stdNormal(rng) {
  return Math.sqrt(-2 * Math.log(rng.positive())) * Math.cos(2 * Math.PI * rng.next());
}

function poissonSample(lambda, rng) {
  if (lambda === 0) return 0;
  const l = Math.exp(-lambda);
  let k = 0;
  let p = 1;
  do {
    k += 1;
    p *= rng.positive();
  } while (p > l);
  return k - 1;
}
