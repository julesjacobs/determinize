// Golden tests of what the simulator shows for every example of the gallery: the step table's
// frames at seeds 1–3, and the statistics of a 200-run batch at seeds 1–200. A frame is recorded
// in part as text and in full by the SHA-256 of a canonical serialization. When a change of the
// simulator's semantics is intended, regenerate the snapshots with
//
//   node --import ./test/det-loader.ts --test --test-update-snapshots test/golden.test.ts
import { createHash } from "node:crypto";
import test from "node:test";
import type { Expr } from "../src/compiler/ast.ts";
import { prettyExpr } from "../src/compiler/pretty.ts";
import { examples } from "../src/examples.ts";
import type { CoupledTrace, Frame } from "../src/runtime/semantics.ts";
import { runCoupledTrace } from "../src/runtime/semantics.ts";

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

function frameOk(frame: Frame) {
  return (
    frame.originalOk &&
    frame.determinizedOk &&
    frame.symbolicOk !== false &&
    frame.consistencyOk !== false
  );
}

function hasDomainError(frame: Frame) {
  return [frame.original, frame.symbolic, frame.determinized].some(
    (expr) => expr?.kind === "DomainError",
  );
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

function numericValue(expr: Expr | undefined) {
  return expr?.kind === "Const" ? expr.value : undefined;
}

/** The pair of results that a run adds to the distributions, if both are finite numbers. */
function sampleOf(trace: CoupledTrace) {
  const final = trace.frames.at(-1);
  const original = numericValue(final?.original) ?? numericValue(trace.finalOriginal);
  const determinized = numericValue(final?.determinized) ?? numericValue(trace.finalDeterminized);
  if (!Number.isFinite(original) || !Number.isFinite(determinized)) return null;
  return { original: original as number, determinized: determinized as number };
}

function sampleStats(values: number[]) {
  const n = values.length;
  const mean = values.reduce((sum, value) => sum + value, 0) / n;
  const variance =
    n < 2 ? NaN : values.reduce((sum, value) => sum + (value - mean) ** 2, 0) / (n - 1);
  return {
    n,
    mean,
    variance,
    standardError: Number.isFinite(variance) ? Math.sqrt(variance / n) : NaN,
  };
}

/** The samples of runs at `seeds`, stopping at the first run that throws, as "Run 200" does. */
function runBatch(source: string, seeds: number[]) {
  const original: number[] = [];
  const determinized: number[] = [];
  let runs = 0;
  for (const seed of seeds) {
    let trace: CoupledTrace;
    try {
      trace = runCoupledTrace(source, seed, 1000, 200);
    } catch {
      break;
    }
    runs += 1;
    const sample = sampleOf(trace);
    if (sample) {
      original.push(sample.original);
      determinized.push(sample.determinized);
    }
  }
  return { runs, original, determinized };
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
    const traces = traceSeeds.map((seed) => describeTrace(runCoupledTrace(example.source, seed)));
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
