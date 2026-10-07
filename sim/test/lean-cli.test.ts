// The port of Lean's evaluator against the Lean CLI: for every case of the corpus manifest,
// test/fixtures/lean-cli.json records what the CLI prints (scripts/lean-fixtures.ts). The runs
// that "Run N" adds must give the CLI's statistics of `--seed S --samples N` at its printed
// precision, and the simulator must reject what Lean rejects.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { parse } from "smol-toml";
import { analyze } from "../src/core/compiler/analyze.ts";
import { displayFloat } from "../src/core/runtime/eval.ts";
import type { Runner } from "../src/core/sampler.ts";
import { runIndex, runnerOf } from "../src/core/sampler.ts";
import type { RunOutcome, Summary } from "../src/core/statistics.ts";
import { addRuns, noRuns, runsOf } from "../src/core/statistics.ts";
import type { CliFixture, CliSummary } from "./lean-cli.ts";

const root = new URL("../../", import.meta.url);
const fixture: CliFixture = JSON.parse(
  readFileSync(new URL("fixtures/lean-cli.json", import.meta.url), "utf8"),
);

/** The summaries of runs 0 to `samples` - 1 from `seed`, as the store adds them up. */
function summarize(runner: Runner, seed: number, samples: number) {
  const outcomes = { source: [] as RunOutcome[], determinized: [] as RunOutcome[] };
  for (let i = 0; i < samples; i++) {
    const run = runIndex(runner, seed, i);
    outcomes.source.push(run.source);
    outcomes.determinized.push(run.determinized);
  }
  return {
    source: addRuns(noRuns, runsOf(outcomes.source)),
    determinized: addRuns(noRuns, runsOf(outcomes.determinized)),
  };
}

/** `summary` as `summarize` in lean/Main.lean prints it. */
function printed(summary: Summary): CliSummary {
  const text = (x: number) =>
    Number.isFinite(x) ? displayFloat(x) : "unavailable (floating-point overflow)";
  const result: CliSummary = {
    returned: summary.runs - summary.failed - summary.rejected,
    samples: summary.runs,
    rejected: summary.rejected,
  };
  if (summary.count > 0) {
    const variance = summary.m2 / summary.count;
    result.mean = text(summary.mean);
    result.variance = Number.isFinite(variance)
      ? displayFloat(Math.max(0, variance))
      : text(variance);
  } else if (summary.firstValue !== null) {
    result.firstValue = summary.firstValue;
  }
  if (summary.failed > 0 && summary.firstFailure !== null) {
    result.firstFailure = summary.firstFailure;
  }
  return result;
}

for (const item of fixture.accepted) {
  test(`Lean CLI: ${item.file}`, () => {
    const analysis = analyze(readFileSync(new URL(item.file, root), "utf8"));
    assert.ok(analysis.ok);
    assert.equal(analysis.type, item.checked);
    const runner = runnerOf(analysis);
    assert.ok(runner);
    for (const { seed, samples, source, determinized } of item.runs) {
      // The UI's seeds are safe integers; the CLI's UInt64 seeds near 2⁶⁴ are the same runs as
      // their negative two's complements.
      const at = Number(BigInt.asIntN(64, BigInt(seed)));
      const summaries = summarize(runner, at, samples);
      assert.deepEqual(printed(summaries.source), source, `source, --seed ${seed}`);
      assert.deepEqual(
        printed(summaries.determinized),
        determinized,
        `determinized, --seed ${seed}`,
      );
    }
  });
}

for (const { file, message } of fixture.rejected) {
  test(`Lean CLI rejects: ${file}`, () => {
    const analysis = analyze(readFileSync(new URL(file, root), "utf8"));
    assert.equal(analysis.ok, false, `Lean: ${message}`);
  });
}

test("the fixture records every case of the manifest", () => {
  const manifest = parse(readFileSync(new URL("tests/cases.toml", root), "utf8"));
  const cases = (manifest.case as { file: string }[]).map((entry) => entry.file).sort();
  const recorded = [...fixture.accepted, ...fixture.rejected].map((entry) => entry.file).sort();
  assert.deepEqual(recorded, cases, "run scripts/lean-fixtures.ts");
});

test("Lean's floats print as C's %f, rounding half to even", () => {
  assert.equal(displayFloat(0.0078125), "0.007812");
  assert.equal(displayFloat(0.5078125), "0.507812");
  assert.equal(displayFloat(1.5e-6), "0.000002");
  assert.equal(displayFloat(-1e-9), "-0.000000");
  assert.equal(displayFloat(-0), "-0.000000");
  assert.equal(displayFloat(0.1 + 0.2), "0.300000");
  assert.equal(displayFloat(1e21), "1000000000000000000000.000000");
  assert.equal(displayFloat(2 ** 70 + 2 ** 20), "1180591620717412352000.000000");
  assert.equal(displayFloat(2 ** 60), "1152921504606846976.000000");
});
