// The G draws that "Run N" records by site: the value of each continuous G site in each run, where
// the run drew there exactly once, in the order of Lean's G trace.
import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import { runIndex, runnerOf, siteDraws } from "../src/core/sampler.ts";

const program = `
let x = uniform[G](0, 1) in
let n = poisson[G](1) in
if x < 0.5 then x else x + uniform[G](0, 1)`;

test("runs record their draws at each continuous G site", () => {
  const runner = runnerOf(analyze(program));
  assert.ok(runner);
  // Sites in Lean's order: x's uniform, n's Poisson (discrete), the second uniform.
  assert.deepEqual(runner.gSites, [0, 2]);
  const runs = Array.from({ length: 40 }, (_, i) => runIndex(runner, 7, i));
  const [first, second] = siteDraws(
    runner.gSites,
    runs.map((run) => run.traces.source),
  );
  for (const [i, run] of runs.entries()) {
    const [x, , y] = run.traces.source;
    assert.deepEqual(run.traces.source, run.traces.determinized);
    assert.equal(first.values[i], x.value);
    assert.equal(x.op, "uniform");
    if (x.value < 0.5) assert.ok(Number.isNaN(second.values[i]));
    else assert.equal(second.values[i], y.value);
  }
  assert.ok([...second.values].some(Number.isNaN));
});
