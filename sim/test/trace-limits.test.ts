// The step table stops a run that would take too long to show: after `maxSymbolicSteps` steps, or
// once its states' sizes add up to `maxShownSize`.
import assert from "node:assert/strict";
import test from "node:test";
import { runCoupling } from "../src/core/trace.ts";

/** A recursion `n` calls deep, whose state grows with each call. */
const deep = (n: number) =>
  `let u = uniform(0, 1) in (rec f n => if n < 1 then u else 1 + f (n - 1)) ${n}`;

test("a deep recursion stops once its states are too large to show", () => {
  // Without the bound on the states' size, this run exhausts the memory of Node's default heap.
  const trace = runCoupling(deep(2000), 1);
  assert.equal(trace.stopped, "size");
  assert.ok(trace.frames.length > 0);
  assert.equal(trace.finalOriginal, undefined);
});

test("a shallow recursion ends", () => {
  const trace = runCoupling(deep(50), 1);
  assert.equal(trace.stopped, null);
  assert.equal(trace.ok, true);
});

test("a run that doesn't end stops after the step limit", () => {
  assert.equal(runCoupling("(rec f x => f x) ()", 1).stopped, "steps");
});

test("a long sum of draws stops once its affine forms are too large to show", () => {
  // Each state holds the sum of the draws so far, so the steps cost more and more.
  const source = `let n = 4000 in
    (rec loop i => fun x => if i <= n then loop (i + 1) (x + uniform(0, 1)) else x) 1 0`;
  assert.equal(runCoupling(source, 1).stopped, "size");
});

test("a run that leaves an operation's domain early is not stopped", () => {
  const source = "let x = uniform[E](-10, 30) in let y = gamma[E](x, 1) in y * 2";
  const trace = runCoupling(source, 2);
  assert.ok(trace.frames.at(-1)?.domainFailure);
  assert.equal(trace.stopped, null);
});
