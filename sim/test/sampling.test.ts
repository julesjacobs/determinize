// The port of Lean's Runtime/Sampling.lean: the SplitMix64 generator against a BigInt reference
// of Lean's `uniform01`, and the samplers where Lean's differ from textbook ones.
import assert from "node:assert/strict";
import test from "node:test";
import { SplitMix64, sample } from "../src/core/runtime/sampling.ts";

const mask = (1n << 64n) - 1n;

/** Lean's `uniform01` over UInt64s: the next state and the double drawn. */
function reference(state: bigint) {
  const next = (state + 0x9e3779b97f4a7c15n) & mask;
  let z = ((next ^ (next >> 30n)) * 0xbf58476d1ce4e5b9n) & mask;
  z = ((z ^ (z >> 27n)) * 0x94d049bb133111ebn) & mask;
  z ^= z >> 31n;
  return { next, z, value: (Number(z >> 12n) + 0.5) / 4503599627370496 };
}

test("SplitMix64 draws what Lean's uniform01 draws", () => {
  // The published first output of SplitMix64 from state 0.
  assert.equal(reference(0n).z, 0xe220a8397b1dcdafn);
  const seeds = [0n, 1n, 2n, 20260911n, 0x517cc1b727220a95n, 1n << 63n, mask, mask - 1n];
  let lcg = 12345n;
  for (let i = 0; i < 24; i++) {
    lcg = (lcg * 6364136223846793005n + 1442695040888963407n) & mask;
    seeds.push(lcg);
  }
  for (const seed of seeds) {
    const rng = new SplitMix64(seed);
    let state = seed;
    for (let i = 0; i < 500; i++) {
      const expected = reference(state);
      assert.equal(rng.uniform01(), expected.value, `seed ${seed}, draw ${i}`);
      state = expected.next;
      assert.equal(rng.seed, state);
    }
  }
});

test("SplitMix64 seeds wrap around 2⁶⁴, and a clone replays the stream", () => {
  assert.equal(new SplitMix64(1n << 64n).seed, 0n);
  assert.equal(new SplitMix64(-1n).seed, mask);
  const rng = new SplitMix64(7n);
  rng.uniform01();
  const copy = rng.clone();
  assert.deepEqual([rng.uniform01(), rng.uniform01()], [copy.uniform01(), copy.uniform01()]);
});

/** The value `sample` returns and whether it advanced the stream. */
function drawn(op: Parameters<typeof sample>[0], args: number[], seed = 3n, mean = false) {
  const rng = new SplitMix64(seed);
  const value = sample(op, mean, args, rng);
  return { value, advanced: rng.seed !== seed };
}

test("degenerate draws and means draw nothing", () => {
  assert.deepEqual(drawn("gaussian", [2, 0]), { value: 2, advanced: false });
  assert.deepEqual(drawn("uniform", [2, 2]), { value: 2, advanced: false });
  assert.deepEqual(drawn("uniform", [1, 2], 3n, true), { value: 1.5, advanced: false });
  assert.deepEqual(drawn("poisson", [0]), { value: 0, advanced: false });
  assert.deepEqual(drawn("uniform", [1, 2]).advanced, true);
});

test("discrete draws compare one uniform with cumulative thresholds, without normalizing", () => {
  for (let seed = 0n; seed < 64n; seed++) {
    const u = new SplitMix64(seed).uniform01();
    const expected = u < 0.25 ? 0 : 1;
    assert.equal(drawn("discrete", [0.25], seed).value, expected, `seed ${seed}`);
    const remainder = u < 0.2 ? 0 : u < 0.5 ? 1 : 2;
    assert.equal(drawn("discrete", [0.2, 0.3], seed).value, remainder, `seed ${seed}`);
  }
  assert.equal(drawn("discrete", [0.2, 0.3], 0n, true).value, 0 * 0.2 + 1 * 0.3 + 2 * 0.5);
  assert.equal(drawn("discrete", [], 0n, true).value, 0);
  // The sum may exceed one by the rounding of n + 1 additions, 8ε(n + 1).
  assert.equal(drawn("discrete", [0.6, 0.4 + 4e-15], 0n, true).advanced, false);
  assert.throws(() => drawn("discrete", [0.6, 0.41]), {
    message: "discrete probabilities sum to more than one",
  });
  assert.throws(() => drawn("discrete", [0.5, -0.1]), {
    message: "discrete requires nonnegative probabilities",
  });
});

test("samplers fail with Lean's messages", () => {
  const cases: [Parameters<typeof sample>[0], number[], string][] = [
    ["uniform", [0, Number.NaN], "nonfinite distribution parameter"],
    ["poisson", [2e6], "Poisson rate exceeds numerical runtime limit (1000000)"],
    ["exponential", [0], "exponential requires rate > 0"],
    ["gamma", [1, -1], "gamma requires positive shape and rate"],
    ["beta", [1], "invalid primitive arity"],
    ["bernoulli", [1.5], "bernoulli requires probability in [0,1]"],
  ];
  for (const [op, args, message] of cases) {
    assert.throws(() => drawn(op, args), { message }, `${op}(${args})`);
  }
});

test("gamma draws are not clamped; a nonfinite draw fails", () => {
  // A shape of 1e-300 draws u^(1e300), which underflows to zero, as in Lean.
  let zeros = 0;
  for (let seed = 0n; seed < 20n; seed++) {
    if (drawn("gamma", [1e-300, 1], seed).value === 0) zeros += 1;
  }
  assert.ok(zeros > 0);
  assert.throws(() => drawn("exponential", [1e-320]), {
    message: "nonfinite numerical sampling result",
  });
});

test("a Poisson rate whose threshold rounds to one draws zero", () => {
  assert.deepEqual(drawn("poisson", [1e-17]), { value: 0, advanced: false });
});
