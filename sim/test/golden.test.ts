// Golden tests of what the simulator shows for every example of the gallery: the step table's
// frames at seeds 1–3, and what Lean's CLI would report about a 200-run batch at seeds 1–200,
// which the sampler runs as the worker does. A frame is recorded
// in part as text and in full by the SHA-256 of a canonical serialization. When a change of the
// simulator's semantics is intended, regenerate the snapshots with
//
//   node --import ./test/det-loader.ts --test --test-update-snapshots test/golden.test.ts
import { createHash } from "node:crypto";
import test from "node:test";
import { prettyExpr } from "../src/core/compiler/pretty.ts";
import { examples } from "../src/core/examples.ts";
import type { CoupledTrace, Frame } from "../src/core/runtime/semantics.ts";
import { createSampler } from "../src/core/sampler.ts";
import type { Summary } from "../src/core/statistics.ts";
import { addRuns, noRuns } from "../src/core/statistics.ts";
import type { RunOutcome } from "../src/core/trace.ts";
import { frameOk, hasDomainError, outcomesOf, runCoupling } from "../src/core/trace.ts";

const traceSeeds = [1, 2, 3];
const batchSeeds = Array.from({ length: 200 }, (_, index) => index + 1);

/** `value` as JSON with sorted keys, nonfinite numbers as strings and undefined fields left out. */
function canonical(value: unknown): string {
  if (typeof value === "number") return Number.isFinite(value) ? String(value) : `"${value}"`;
  if (value === null || typeof value !== "object") return JSON.stringify(value) ?? "null";
  if (Array.isArray(value)) return `[${value.map(canonical).join(",")}]`;
  const entries = Object.entries(value)
    .filter(([, field]) => field !== undefined)
    .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  return `{${entries.map(([key, field]) => `${JSON.stringify(key)}:${canonical(field)}`).join(",")}}`;
}

/** The label of a frame's check in the step table. */
function checkLabel(frame: Frame) {
  if (!frameOk(frame)) return "FAIL";
  return hasDomainError(frame) ? "ERR" : "OK";
}

/** The labels of the frames' checks, each run of equal labels as `label ×count`. */
function checkLabels(frames: Frame[]) {
  const runs: { label: string; count: number }[] = [];
  for (const label of frames.map(checkLabel)) {
    const last = runs.at(-1);
    if (last?.label === label) last.count += 1;
    else runs.push({ label, count: 1 });
  }
  return runs.map(({ label, count }) => (count === 1 ? label : `${label} ×${count}`)).join(", ");
}

/** The batch at `seeds`: the sampler's slices, run one after the other. */
function runBatch(source: string, seeds: number[]) {
  const batch = { source: noRuns, determinized: noRuns };
  const tasks: (() => void)[] = [];
  const sampler = createSampler({
    post(response) {
      if (response.type !== "batch") return;
      batch.source = addRuns(batch.source, response.source);
      batch.determinized = addRuns(batch.determinized, response.determinized);
    },
    defer: (task) => tasks.push(task),
    now: () => performance.now(),
  });
  sampler.handle({ type: "run", generation: 1, source, seeds: Float64Array.from(seeds) });
  for (let task = tasks.shift(); task; task = tasks.shift()) task();
  return batch;
}

function describeOutcome(outcome: RunOutcome) {
  if (outcome.kind === "returned") return outcome.display;
  return outcome.kind === "rejected" ? "rejected" : `failed: ${outcome.message}`;
}

function describeTrace(trace: CoupledTrace) {
  const final = trace.frames.at(-1);
  const outcomes = outcomesOf(trace);
  const status = trace.counterexample ? "counterexample" : trace.ok ? "checked" : "failed";
  return [
    `seed ${trace.seed}: ${status}, ${trace.frames.length} frames`,
    `  checks: ${checkLabels(trace.frames)}`,
    `  final source: ${final ? prettyExpr(final.original) : "none"}`,
    `  final determinized: ${final ? prettyExpr(final.determinized) : "none"}`,
    `  outcome: ${describeOutcome(outcomes.source)} | ${describeOutcome(outcomes.determinized)}`,
    `  frames: sha256 ${createHash("sha256").update(canonical(trace.frames)).digest("hex")}`,
  ].join("\n");
}

function describeSummary(name: string, summary: Summary) {
  const { runs, rejected, failed, firstFailure, count, mean, m2 } = summary;
  const failures = failed > 0 ? ` (first: ${firstFailure})` : "";
  return [
    `  ${name}: ${runs} runs, ${rejected} rejected, ${failed} failed${failures}`,
    `    returned numbers: n ${count}, mean ${count > 0 ? mean : NaN}, population variance ${count > 0 ? m2 / count : NaN}`,
  ].join("\n");
}

for (const example of examples) {
  test(example.id, (t) => {
    const traces = traceSeeds.map((seed) => describeTrace(runCoupling(example.source, seed)));
    const batch = runBatch(example.source, batchSeeds);
    const text = [
      ...traces,
      "batch at seeds 1-200:",
      describeSummary("source", batch.source),
      describeSummary("determinized", batch.determinized),
    ].join("\n");
    t.assert.snapshot(text, { serializers: [String] });
  });
}
