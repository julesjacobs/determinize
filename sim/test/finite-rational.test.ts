// The rationals of the finite models: a fraction beyond a double's range still converts to about
// the double nearest its value, for the decimals that the exact values show.
import assert from "node:assert/strict";
import test from "node:test";
import { rational, toNumber } from "../src/core/finite/rational.ts";

test("a fraction beyond a double's range converts to the double nearest its value", () => {
  const power = (base: bigint, exponent: number) => base ** BigInt(exponent);
  const cases: [bigint, bigint, number][] = [
    [power(2n, 1200), power(3n, 700), 1200 * Math.LN2 - 700 * Math.log(3)],
    [power(3n, 700), power(2n, 1200), 700 * Math.log(3) - 1200 * Math.LN2],
    [power(2n, 700), power(3n, 700), 700 * (Math.LN2 - Math.log(3))],
    [-power(2n, 1200), power(3n, 700), 1200 * Math.LN2 - 700 * Math.log(3)],
  ];
  for (const [num, den, log] of cases) {
    const value = toNumber(rational(num, den));
    const expected = Math.sign(Number(num)) * Math.exp(log);
    assert.ok(Math.abs(value / expected - 1) < 1e-9, `${value} for ${expected}`);
  }
  assert.equal(toNumber(rational(1n, 3n)), 1 / 3);
});
