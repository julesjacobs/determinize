// The distributions band's axes and scales: they come out finite and round for any outputs,
// including a range of one ulp and numbers near the largest float, and the 2.6× rule cuts bars
// only when both programs have some.
import assert from "node:assert/strict";
import test from "node:test";
import { cutHeight, niceAxis, outputAxis, ticks, widened } from "../src/ui/charts.ts";

test("an axis around one ulp of a large number has a few round ticks", () => {
  const lo = 100000000;
  // The next float after 1e8, one ulp (1.49e-8) above it.
  const hi = 100000000 + 2 ** -26;
  const [from, to] = widened(lo, hi);
  const axis = niceAxis(from, to, 3);
  assert.ok(axis.ticks.length > 1 && axis.ticks.length < 20);
  assert.ok(axis.ticks.every(Number.isFinite));
  // Without widening, a step below half an ulp still ends.
  assert.ok(ticks(lo, hi, 3).values.length <= 16);
});

test("outputs near the largest float give no axis rather than a broken one", () => {
  assert.equal(outputAxis([-1e308, 1e308, -1e308], []), null);
  const axis = outputAxis([1, 2, 3], [2]);
  assert.ok(axis && axis.bins > 0 && Number.isFinite(axis.binWidth));
});

test("a bar is cut only above 2.6 times the other program's highest, when both have bars", () => {
  assert.equal(cutHeight([10, 20], [0, 0]), 21);
  assert.equal(cutHeight([0, 2], [0, 0]), 3);
  assert.equal(cutHeight([100, 5], [10, 1]), 28);
  assert.equal(cutHeight([20, 5], [10, 1]), 21);
});
