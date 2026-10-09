// The gallery's reasons are what the simulator's check finds for each example: no premise failing
// for those it groups first, in floating point or not; for a domain failure, a run that fails with
// its message; and for a failure in floating point only, a run that fails at exactly 0.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import { examples } from "../src/core/examples.ts";
import { runIndex, runnerOf } from "../src/core/sampler.ts";
import { addRuns, noRuns, runsOf } from "../src/core/statistics.ts";
import { reasonOf } from "../src/ui/gallery.ts";
import { premiseVerdict } from "../src/ui/verdict.ts";

/** The summary of `count` runs of `source` from seed 1. */
function summary(source: string, count: number) {
  const runner = runnerOf(analyze(source));
  assert.ok(runner, source);
  const outcomes = Array.from({ length: count }, (_, i) => runIndex(runner, 1, i).source);
  return addRuns(noRuns, runsOf(outcomes));
}

for (const example of examples) {
  test(`${example.title}: the gallery's reason is what the check finds`, () => {
    const reason = reasonOf(example);
    const analysis = analyze(example.source);
    if (example.fails) {
      assert.equal(reason?.chip, "a run fails");
      const runs = summary(example.source, 1000);
      assert.equal(runs.firstDomainFailure?.message, example.fails.message);
      assert.equal(runs.firstZeroFailure, null);
    } else if (example.floatFailure) {
      assert.equal(reason?.chip, "underflow");
      const runs = summary(example.source, 1000);
      assert.equal(runs.firstDomainFailure, null);
      assert.equal(runs.firstZeroFailure?.message, example.floatFailure.message);
    } else if (reason === null) {
      assert.ok(analysis.ok && analysis.type === "float[E]");
      const runs = summary(example.source, 1000);
      assert.equal(runs.firstDomainFailure, null);
      assert.equal(runs.firstZeroFailure, null);
    } else {
      assert.ok(!analysis.ok || analysis.type !== "float[E]");
    }
  });
}

test("the gallery has examples that the check finds failing no premise, and some that fail one", () => {
  const reasons = examples.map(reasonOf);
  assert.ok(reasons.some((reason) => reason === null));
  assert.ok(new Set(reasons.map((reason) => reason?.group).filter(Boolean)).size >= 2);
});

test("a program that Lean rejects outright, and one of type float[G], have their own reasons", () => {
  const example = (source: string) => ({
    id: "",
    title: "",
    explanation: "",
    fromPaper: false,
    source,
  });
  assert.deepEqual(reasonOf(example("let x =")), {
    group: "Lean rejects the program",
    chip: "rejected",
    description: "Type float[E]: Lean rejects the program at parsing, so it doesn't run.",
  });
  assert.equal(reasonOf(example("uniform[G](0, 1)")), null);
});

test("the gallery's Gaussian random walk returns its final position, which the theorems cover", () => {
  const walk = examples.find((example) => example.title === "Gaussian random walk");
  assert.ok(walk);
  const analysis = analyze(walk.source);
  assert.ok(analysis.ok && analysis.type === "float[E]");
  assert.equal(reasonOf(walk), null);
  // The paper's walk, which returns the whole path, stays as it is.
  const paper = analyze(
    readFileSync(new URL("../../examples/paper/gauss-random-walk.det", import.meta.url), "utf8"),
  );
  assert.ok(paper.ok && paper.type === "[(float[E] * float[E])]");
});

test("the Gaussian bound fails domain safety, in about one run in six", () => {
  const bound = examples.find((example) => example.title === "Gaussian bound");
  assert.ok(bound);
  const runs = summary(bound.source, 10000);
  // 1 625 of 10 000 runs from seed 1 draw a bound below 0, against Pr(x < 0) ≈ 0.159.
  assert.equal(runs.failed, 1625);
  assert.equal(runs.firstZeroFailure, null);
  const verdict = premiseVerdict(analyze(bound.source), runs, 1);
  assert.deepEqual(verdict.safe, {
    status: "fails",
    text: "run 6 failed: uniform requires lower ≤ upper (seed 7)",
  });
});
