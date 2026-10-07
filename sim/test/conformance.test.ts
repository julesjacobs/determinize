// Runs every case of the corpus manifest tests/cases.toml through the simulator's front end and
// compares what Lean's front end decides: acceptance, the rejecting stage, the checked type and
// the mode of every sample site. The execution and statistical cases also run through the port of
// Lean's evaluator, with the expectations, seeds, fuel and tolerances that lean/Tests/Corpus.lean
// applies.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { parse } from "smol-toml";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { Program } from "../src/core/compiler/core.ts";
import type { Node } from "../src/core/runtime/eval.ts";
import { display, prepare, run } from "../src/core/runtime/eval.ts";

/** What the manifest expects of the front end for one program, and of its runs. */
interface Case {
  file: string;
  outcome: "accept" | "reject";
  stage?: string;
  expectedType?: string;
  affinities?: string[];
  /** The expectations of the source and the determinized program, Lean's `Observation`. */
  source?: Record<string, unknown>;
  target?: Record<string, unknown>;
  samples: number;
  seed: number;
  fuel: number;
}

/** Cases where the simulator differs from Lean, with the reason. */
const todo: Record<string, string> = {};

const root = new URL("../../", import.meta.url);

function optionalString(table: Record<string, unknown>, key: string): string | undefined {
  const value = table[key];
  if (value === undefined) return undefined;
  if (typeof value !== "string") throw new Error(`${key} must be a string`);
  return value;
}

function readCases(): Case[] {
  const manifest = parse(readFileSync(new URL("tests/cases.toml", root), "utf8"));
  const cases = manifest.case;
  if (!Array.isArray(cases)) throw new Error("the manifest has no cases");
  return cases.map((entry): Case => {
    if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
      throw new Error("a case must be a table");
    }
    const table = entry as Record<string, unknown>;
    const file = optionalString(table, "file");
    const outcome = optionalString(table, "outcome");
    if (!file || (outcome !== "accept" && outcome !== "reject")) {
      throw new Error(`invalid case ${JSON.stringify(table)}`);
    }
    const affinities = table.affinities;
    if (
      affinities !== undefined &&
      !(Array.isArray(affinities) && affinities.every((mode) => mode === "E" || mode === "G"))
    ) {
      throw new Error(`${file}: affinities must be a list of E and G`);
    }
    const observation = (key: string) => {
      const value = table[key];
      if (value === undefined) return undefined;
      if (typeof value !== "object" || value === null) throw new Error(`${file}: invalid ${key}`);
      return value as Record<string, unknown>;
    };
    const natural = (key: string, fallback: number) => {
      const value = table[key] ?? fallback;
      if (typeof value !== "number" || !Number.isSafeInteger(value)) {
        throw new Error(`${file}: invalid ${key}`);
      }
      return value;
    };
    return {
      file,
      outcome,
      stage: optionalString(table, "stage"),
      expectedType: optionalString(table, "expected_type"),
      affinities,
      source: observation("source"),
      target: observation("target"),
      // Lean's defaults in lean/Tests/Corpus.lean.
      samples: natural("samples", 20000),
      seed: natural("seed", 20260911),
      fuel: natural("fuel", 100000),
    };
  });
}

/** How the simulator's front end differs from the manifest on this case, or null. */
function difference(item: Case): string | null {
  const result = analyze(readFileSync(new URL(item.file, root), "utf8"));
  if (item.outcome === "reject") {
    if (result.ok) return `Lean rejects at ${item.stage}; the simulator accepts`;
    if (result.stage !== item.stage) {
      const message = result.diagnostics.map((diagnostic) => diagnostic.message).join("; ");
      return `Lean rejects at ${item.stage}; the simulator at ${result.stage ?? "an internal error"}: ${message}`;
    }
    return null;
  }
  if (!result.ok) {
    const message = result.diagnostics.map((diagnostic) => diagnostic.message).join("; ");
    return `Lean accepts; the simulator rejects at ${result.stage ?? "an internal error"}: ${message}`;
  }
  if (item.expectedType !== undefined && result.type !== item.expectedType) {
    return `Lean infers ${item.expectedType}; the simulator ${result.type}`;
  }
  if (item.affinities !== undefined && result.affinities.join() !== item.affinities.join()) {
    return `Lean infers modes [${item.affinities.join(", ")}]; the simulator [${result.affinities.join(", ")}]`;
  }
  return null;
}

/** A number of an observation, or of its moments. */
function number(table: Record<string, unknown>, key: string): number | undefined {
  const value = table[key];
  if (value === undefined) return undefined;
  if (typeof value !== "number") throw new Error(`${key} must be a number`);
  return value;
}

/** How a run of `program` differs from what `observation` expects, as Lean's `observation`
 * checks it, or null. */
function observationDifference(
  program: Node,
  item: Case,
  observation: Record<string, unknown>,
): string | null {
  const keys = ["number", "value", "error", "draws"];
  if (!keys.some((key) => key in observation)) return null;
  const outcome = run(program, BigInt(item.seed), { fuel: item.fuel });
  // Lean's `Runtime.run` turns a rejection into a failure.
  const failure =
    outcome.kind === "failed"
      ? outcome.message
      : outcome.kind === "rejected"
        ? "observation rejected"
        : null;
  const error = observation.error;
  if (typeof error === "string") {
    if (failure === null) return `expected runtime error '${error}', execution succeeded`;
    return failure.includes(error) ? null : `expected an error with '${error}', got '${failure}'`;
  }
  if (outcome.kind !== "returned") return `expected a value, got '${failure}'`;
  const expected = number(observation, "number");
  if (expected !== undefined) {
    const tolerance = number(observation, "tolerance") ?? 1e-10;
    const value = outcome.value;
    if (value.tag !== "number") return `expected numeric result, got ${display(value)}`;
    if (!(Math.abs(value.value - expected) <= tolerance)) {
      return `expected ${expected} ± ${tolerance}, got ${value.value}`;
    }
  }
  if (typeof observation.value === "string" && display(outcome.value) !== observation.value) {
    return `expected '${observation.value}', got '${display(outcome.value)}'`;
  }
  const draws = number(observation, "draws");
  if (draws !== undefined && outcome.draws !== draws) {
    return `expected ${draws} draws, got ${outcome.draws}`;
  }
  return null;
}

/** How the moments of `program`'s runs differ from what `observation` expects, as Lean's
 * `statistical` checks them: run i at seed + i · 0x9e3779b97f4a7c15, and the sample variance. */
function momentsDifference(
  program: Node,
  item: Case,
  observation: Record<string, unknown>,
): string | null {
  const moments = observation.moments as Record<string, unknown> | undefined;
  if (!moments) return null;
  const lower = number(moments, "lower");
  const upper = number(moments, "upper");
  let mean = 0;
  let m2 = 0;
  for (let i = 0; i < item.samples; i++) {
    const seed = BigInt.asUintN(64, BigInt(item.seed) + BigInt(i) * 0x9e3779b97f4a7c15n);
    const outcome = run(program, seed, { fuel: item.fuel });
    if (outcome.kind !== "returned" || outcome.value.tag !== "number") {
      return `run ${i}: expected a number`;
    }
    const x = outcome.value.value;
    if (!Number.isFinite(x)) return `nonfinite sample at index ${i}`;
    if (lower !== undefined && x < lower) return `sample ${x} below support ${lower}`;
    if (upper !== undefined && x > upper) return `sample ${x} above support ${upper}`;
    if (moments.integer === true && x !== Math.floor(x)) return `noninteger sample ${x}`;
    const delta = x - mean;
    mean = mean + delta / (i + 1);
    m2 = m2 + delta * (x - mean);
  }
  const variance = m2 / (item.samples - 1);
  for (const [label, actual] of [
    ["mean", mean],
    ["variance", variance],
  ] as const) {
    const expected = number(moments, label) ?? NaN;
    const tolerance = number(moments, `${label}_tolerance`) ?? NaN;
    if (!(Math.abs(actual - expected) <= tolerance)) {
      return `${label}: expected ${expected} ± ${tolerance}, got ${actual}`;
    }
  }
  return null;
}

const cases = readCases();

for (const item of cases) {
  if (item.outcome !== "accept" || (!item.source && !item.target)) continue;
  test(`conformance of runs: ${item.file}`, () => {
    const analysis = analyze(readFileSync(new URL(item.file, root), "utf8"));
    assert.ok(analysis.ok);
    const programs: [string, Program, Record<string, unknown> | undefined][] = [
      ["source", analysis.program.source, item.source],
      ["target", analysis.program.determinized, item.target],
    ];
    for (const [label, program, observation] of programs) {
      if (!observation) continue;
      const node = prepare(program);
      assert.equal(observationDifference(node, item, observation), null, label);
      assert.equal(momentsDifference(node, item, observation), null, label);
    }
  });
}

for (const item of cases) {
  const found = difference(item);
  const reason = todo[item.file];
  if (reason === undefined) {
    test(`conformance: ${item.file}`, () => assert.equal(found, null));
  } else if (found === null) {
    test(`conformance: ${item.file}`, () => assert.fail("conforms now; remove it from todo"));
  } else {
    test(`conformance: ${item.file}`, { todo: reason }, () => assert.fail(found));
  }
}

test("conformance: every todo entry is a manifest case", () => {
  const files = new Set(cases.map((item) => item.file));
  assert.deepEqual(
    Object.keys(todo).filter((file) => !files.has(file)),
    [],
  );
});
