// The gallery's reasons are what the simulator's check finds for each example: no premise failing
// for those it groups first, in floating point or not, and for a failure in floating point only,
// a run that fails at exactly 0 with its message.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { test } from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import { examples } from "../src/core/examples.ts";
import { runIndex, runnerOf } from "../src/core/sampler.ts";
import { addRuns, noRuns, runsOf } from "../src/core/statistics.ts";
import { reasonOf } from "../src/ui/gallery.ts";

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
    if (example.floatFailure) {
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
