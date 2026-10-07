// Golden tests of what the simulator shows for every example of the gallery: the step table's
// frames at seeds 1–3, and the statistics of a 200-run batch at seeds 1–200. A frame is recorded
// in part as text and in full by the SHA-256 of a canonical serialization. When a change of the
// simulator's semantics is intended, regenerate the snapshots with
//
//   node --import ./test/det-loader.ts --test --test-update-snapshots test/golden.test.ts
import { createHash } from "node:crypto";
import test from "node:test";
import { prettyExpr } from "../src/core/compiler/pretty.ts";
import { examples } from "../src/core/examples.ts";
import type { CoupledTrace, Frame } from "../src/core/runtime/semantics.ts";
import { sampleStats } from "../src/core/statistics.ts";
import { frameOk, hasDomainError, runBatch, runCoupling, sampleOf } from "../src/core/trace.ts";

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

function describeTrace(trace: CoupledTrace) {
  const final = trace.frames.at(-1);
  const sample = sampleOf(trace);
  const status = trace.counterexample ? "counterexample" : trace.ok ? "checked" : "failed";
  return [
    `seed ${trace.seed}: ${status}, ${trace.frames.length} frames`,
    `  checks: ${checkLabels(trace.frames)}`,
    `  final source: ${final ? prettyExpr(final.original) : "none"}`,
    `  final determinized: ${final ? prettyExpr(final.determinized) : "none"}`,
    `  sample: ${sample ? `${sample.original} | ${sample.determinized}` : "none"}`,
    `  frames: sha256 ${createHash("sha256").update(canonical(trace.frames)).digest("hex")}`,
  ].join("\n");
}

function describeStats(name: string, values: number[]) {
  const { n, mean, variance, standardError } = sampleStats(values);
  return `  ${name}: n ${n}, mean ${mean}, variance ${variance}, standard error ${standardError}`;
}

for (const example of examples) {
  test(example.id, (t) => {
    const traces = traceSeeds.map((seed) => describeTrace(runCoupling(example.source, seed)));
    const batch = runBatch(example.source, batchSeeds);
    const text = [
      ...traces,
      `batch at seeds 1-200: ${batch.runs} runs`,
      describeStats("source", batch.original),
      describeStats("determinized", batch.determinized),
    ].join("\n");
    t.assert.snapshot(text, { serializers: [String] });
  });
}
