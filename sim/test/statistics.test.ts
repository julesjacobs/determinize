// The statistics the page shows are those that `summarize` in lean/Main.lean prints.
import assert from "node:assert/strict";
import test from "node:test";
import { noRuns, statsOf, varianceRatio } from "../src/core/statistics.ts";

test("the variance is at least 0, as summarize clamps it", () => {
  const stats = statsOf({ ...noRuns, runs: 2, count: 2, mean: 1, m2: -1e-18 });
  assert.equal(stats.variance, 0);
});

test("a variance that overflows leaves the variance reduction unavailable", () => {
  const overflowed = statsOf({ ...noRuns, runs: 2, count: 2, mean: 0, m2: Infinity });
  const finite = statsOf({ ...noRuns, runs: 2, count: 2, mean: 0, m2: 1 });
  const ratio = varianceRatio(overflowed, finite);
  assert.ok(Number.isNaN(ratio.value));
  assert.match(ratio.explanation, /floating-point overflow/);
});
