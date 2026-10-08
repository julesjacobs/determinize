// A port of Lean's `Runtime/Sampling.lean`: the SplitMix64 generator, the samplers and means of
// the primitives, and their domain checks with Lean's messages. Each function performs the same
// floating-point operations in the same order as Lean's, so a run draws the numbers that Lean's
// runtime draws, up to the last bit of `Math.log`, `Math.exp`, `Math.cos` and `Math.pow`.

/** A primitive, as Lean's `Op`; a discrete distribution's arity is its argument count. */
export type Op =
  | "uniform"
  | "gaussian"
  | "discrete"
  | "bernoulli"
  | "poisson"
  | "exponential"
  | "gamma"
  | "beta";

/** The ops whose draws are integers, which `--sample-sites` counts as discrete. */
export const discreteOps: ReadonlySet<Op> = new Set(["discrete", "bernoulli", "poisson"]);

/** A failure of a sampler, with the message of Lean's `RandomM` error, and whether a parameter of
 * exactly 0, the others valid, fails the check: where that 0 is inexact, a floating-point
 * underflow can reach it although the real-valued parameter is positive. */
export class SamplingError extends Error {
  declare atZero: boolean;

  constructor(message: string, atZero = false) {
    super(message);
    this.name = "SamplingError";
    this.atZero = atZero;
  }
}

/** 2⁶⁴ − 1, the largest UInt64. */
const uint64Mask = (1n << 64n) - 1n;

/** A UInt64, as an unsigned 64-bit BigInt. */
export function uint64(value: bigint): bigint {
  return value & uint64Mask;
}

/** The high 32 bits of the 64-bit product of two UInt32s. */
function mulHigh(a: number, b: number) {
  const a0 = a & 0xffff;
  const a1 = a >>> 16;
  const b0 = b & 0xffff;
  const b1 = b >>> 16;
  const p01 = a0 * b1;
  const p10 = a1 * b0;
  const middle = ((a0 * b0) >>> 16) + (p01 & 0xffff) + (p10 & 0xffff);
  return (a1 * b1 + (p01 >>> 16) + (p10 >>> 16) + (middle >>> 16)) >>> 0;
}

/**
 * The state of Lean's `RandomM`, a UInt64, held as two UInt32 halves. `uniform01` advances it as
 * Lean's does; `clone` copies it, so that a stream can be replayed.
 */
export class SplitMix64 {
  declare high: number;
  declare low: number;

  constructor(seed: bigint) {
    const state = uint64(seed);
    this.high = Number(state >> 32n);
    this.low = Number(state & 0xffffffffn);
  }

  clone(): SplitMix64 {
    const copy = new SplitMix64(0n);
    copy.high = this.high;
    copy.low = this.low;
    return copy;
  }

  /** The state as a UInt64, which Lean's `EvalState` calls the stream's seed. */
  get seed(): bigint {
    return (BigInt(this.high) << 32n) | BigInt(this.low);
  }

  /** Lean's `uniform01`: a double in (0, 1) from the next SplitMix64 output. */
  uniform01(): number {
    // next := state + 0x9e3779b97f4a7c15
    const low = this.low + 0x7f4a7c15;
    this.low = low >>> 0;
    this.high = (this.high + 0x9e3779b9 + (low > 0xffffffff ? 1 : 0)) >>> 0;
    let high = this.high;
    let lowWord = this.low;
    // z := (next ^^^ (next >>> 30)) * 0xbf58476d1ce4e5b9
    [high, lowWord] = [high ^ (high >>> 30), lowWord ^ ((lowWord >>> 30) | (high << 2))];
    [high, lowWord] = multiply(high >>> 0, lowWord >>> 0, 0xbf58476d, 0x1ce4e5b9);
    // z := (z ^^^ (z >>> 27)) * 0x94d049bb133111eb
    [high, lowWord] = [high ^ (high >>> 27), lowWord ^ ((lowWord >>> 27) | (high << 5))];
    [high, lowWord] = multiply(high >>> 0, lowWord >>> 0, 0x94d049bb, 0x133111eb);
    // z := z ^^^ (z >>> 31)
    [high, lowWord] = [high ^ (high >>> 31), lowWord ^ ((lowWord >>> 31) | (high << 1))];
    high >>>= 0;
    lowWord >>>= 0;
    // ((z >>> 12).toFloat + 0.5) / 2⁵²; z >>> 12 has 52 bits, so the sum is exact.
    const shifted = (high >>> 12) * 4294967296 + (high & 0xfff) * 1048576 + (lowWord >>> 12);
    return (shifted + 0.5) / 4503599627370496.0;
  }
}

/** The product of two UInt64s modulo 2⁶⁴, each as high and low UInt32 halves. */
function multiply(aHigh: number, aLow: number, bHigh: number, bLow: number): [number, number] {
  const low = Math.imul(aLow, bLow) >>> 0;
  const high = (mulHigh(aLow, bLow) + Math.imul(aHigh, bLow) + Math.imul(aLow, bHigh)) >>> 0;
  return [high, low];
}

function normal(rng: SplitMix64) {
  const u = rng.uniform01();
  const v = rng.uniform01();
  return Math.sqrt(-2.0 * Math.log(u)) * Math.cos(6.283185307179586 * v);
}

function gammaLarge(a: number, rng: SplitMix64) {
  const d = a - 1.0 / 3.0;
  const c = 1.0 / Math.sqrt(9.0 * d);
  for (let i = 0; i < 100000; i++) {
    const x = normal(rng);
    let v = 1.0 + c * x;
    if (v > 0) {
      v = v * v * v;
      const u = rng.uniform01();
      if (
        u < 1.0 - 0.0331 * x * x * x * x ||
        Math.log(u) < 0.5 * x * x + d * (1.0 - v + Math.log(v))
      ) {
        return d * v;
      }
    }
  }
  throw new SamplingError("gamma sampler exceeded its rejection limit");
}

function gammaDraw(a: number, rng: SplitMix64) {
  if (a < 1.0) {
    const x = gammaLarge(a + 1.0, rng);
    const u = rng.uniform01();
    return x * u ** (1.0 / a);
  }
  return gammaLarge(a, rng);
}

/** Lean's `min` and `max` on floats, which compare with `≤`. */
function min(a: number, b: number) {
  return a <= b ? a : b;
}

function max(a: number, b: number) {
  return a <= b ? b : a;
}

function poissonDraw(rate: number, rng: SplitMix64) {
  // Independent Poisson pieces avoid exp(-rate) underflow.
  if (rate > 1000000.0) {
    throw new SamplingError("Poisson rate exceeds numerical runtime limit (1000000)");
  }
  let remaining = rate;
  let result = 0.0;
  while (remaining > 0) {
    const part = min(remaining, 20.0);
    remaining = remaining - part;
    const threshold = Math.exp(-part);
    let product = 1.0;
    let count = 0;
    while (product > threshold) {
      if (count >= 100000) throw new SamplingError("Poisson sampler exceeded its iteration limit");
      count += 1;
      product = product * rng.uniform01();
    }
    // Lean subtracts natural numbers, so a chunk whose threshold rounds to 1 adds 0.
    result = result + Math.max(0, count - 1);
  }
  return result;
}

const finite = (x: number) => Number.isFinite(x);

/**
 * Lean's `sample`: a draw of `op` with parameters `args`, or with `mean` its mean, which draws
 * nothing. A failure throws a `SamplingError` with Lean's message.
 */
export function sample(op: Op, mean: boolean, args: readonly number[], rng: SplitMix64): number {
  if (!args.every(finite)) throw new SamplingError("nonfinite distribution parameter");
  const result = draw(op, mean, args, rng);
  if (!finite(result)) throw new SamplingError("nonfinite numerical sampling result");
  return result;
}

function draw(op: Op, mean: boolean, args: readonly number[], rng: SplitMix64): number {
  const arity =
    op === "discrete" ? args.length : ["bernoulli", "poisson", "exponential"].includes(op) ? 1 : 2;
  if (args.length !== arity) throw new SamplingError("invalid primitive arity");
  const [a, b] = args;
  switch (op) {
    case "uniform":
      if (a > b) throw new SamplingError("uniform requires lower ≤ upper");
      if (mean) return a / 2.0 + b / 2.0;
      if (a === b) return a;
      {
        const u = rng.uniform01();
        return a * (1.0 - u) + b * u;
      }
    case "gaussian":
      if (b < 0) throw new SamplingError("gaussian requires variance ≥ 0");
      if (mean || b === 0) return a;
      return a + Math.sqrt(b) * normal(rng);
    case "discrete": {
      const n = args.length;
      if (!args.every((p) => p >= 0)) {
        throw new SamplingError("discrete requires nonnegative probabilities");
      }
      const total = args.reduce((sum, p) => sum + p, 0.0);
      // Allow accumulation rounding at the domain boundary in this numerical backend.
      const tolerance = 8.0 * 2.220446049250313e-16 * (n + 1);
      if (total > 1.0 + tolerance) {
        throw new SamplingError("discrete probabilities sum to more than one");
      }
      if (mean) {
        const supplied = args.reduce((sum, p, i) => sum + i * p, 0.0);
        return supplied + n * max(0.0, 1.0 - total);
      }
      const u = rng.uniform01();
      let cumulative = 0.0;
      for (const [i, p] of args.entries()) {
        cumulative = cumulative + p;
        if (u < cumulative) return i;
      }
      return n;
    }
    case "bernoulli":
      if (a < 0 || a > 1) throw new SamplingError("bernoulli requires probability in [0,1]");
      if (mean) return a;
      return rng.uniform01() < a ? 1 : 0;
    case "poisson":
      if (a < 0) throw new SamplingError("poisson requires rate ≥ 0");
      return mean ? a : poissonDraw(a, rng);
    case "exponential":
      if (a <= 0) throw new SamplingError("exponential requires rate > 0", a === 0);
      if (mean) return 1.0 / a;
      return -Math.log(rng.uniform01()) / a;
    case "gamma":
      if (a <= 0 || b <= 0) {
        throw new SamplingError("gamma requires positive shape and rate", a >= 0 && b >= 0);
      }
      if (mean) return a / b;
      return gammaDraw(a, rng) / b;
    case "beta": {
      if (a <= 0 || b <= 0) {
        throw new SamplingError("beta requires positive parameters", a >= 0 && b >= 0);
      }
      if (mean) return a / (a + b);
      const x = gammaDraw(a, rng);
      const y = gammaDraw(b, rng);
      return x / (x + y);
    }
  }
}
