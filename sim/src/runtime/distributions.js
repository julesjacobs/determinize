import { affineAdd, affineDiv, affineMul, affineScale, affineToNumber, evalAffine, isConcreteAffine } from "./affine.js";

export const floatDistributions = new Set(["Uniform", "Gauss", "Exponential", "Gamma", "Beta", "Bernoulli", "Poisson", "Discrete"]);
const MIN_POSITIVE_SAMPLE = Number.MIN_VALUE;

export function sampleDistribution(kind, args, rng) {
  switch (kind) {
    case "Uniform": {
      const a = numberArg(args[0]);
      const b = numberArg(args[1]);
      const lo = Math.min(a, b);
      const hi = Math.max(a, b);
      return lo + rng.next() * (hi - lo);
    }
    case "Gauss": {
      const mean = numberArg(args[0]);
      const variance = numberArg(args[1]);
      const u1 = rng.positive();
      const u2 = rng.next();
      return mean + Math.sqrt(variance) * Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
    }
    case "Exponential": {
      const rate = numberArg(args[0]);
      if (rate <= 0) throw new Error("exponential: rate must be > 0");
      return -Math.log(rng.positive()) / rate;
    }
    case "Gamma":
      return gammaSample(numberArg(args[0]), numberArg(args[1]), rng);
    case "Beta": {
      const x = gammaSample(numberArg(args[0]), 1, rng);
      const y = gammaSample(numberArg(args[1]), 1, rng);
      return x / (x + y);
    }
    case "Flip": {
      const p = numberArg(args[0]);
      if (p < 0 || p > 1) throw new Error("flip: p not in [0,1]");
      return rng.next() < p;
    }
    case "Bernoulli":
      return rng.next() < numberArg(args[0]) ? 1 : 0;
    case "Poisson":
      return poissonSample(numberArg(args[0]), rng);
    case "Discrete": {
      const probabilities = args.map(numberArg);
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

function gammaSample(alpha, beta, rng) {
  if (alpha <= 0 || beta <= 0) throw new Error("gamma: parameters must be > 0");
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
  if (lambda < 0) throw new Error("poisson: lambda must be >= 0");
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
