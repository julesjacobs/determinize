// Runs every case of the corpus manifest tests/cases.toml through the simulator's front end and
// compares what Lean's front end decides: acceptance, the rejecting stage, the checked type and
// the mode of every sample site.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { parse } from "smol-toml";
import { analyze } from "../src/compiler/analyze.ts";

/** What the manifest expects of the front end for one program. */
interface Case {
  file: string;
  outcome: "accept" | "reject";
  stage?: string;
  expectedType?: string;
  affinities?: string[];
}

/** Cases where the simulator differs from Lean, with the reason. */
const todo: Record<string, string> = {
  // Elaboration
  "tests/typing/accept/gauss.det":
    "the checker types the parsed `e * literal`, not the elaborated `literal * e`",
  "tests/typing/accept/mult.det":
    "the checker types the parsed `e * literal`, not the elaborated `literal * e`",
  // Inference
  "tests/typing/accept/subtyping.det":
    "an annotated site fixes the mode of its context instead of being a subtype of it",
  "tests/typing/accept/mixed-list.det":
    "an annotated site fixes the mode of its context instead of being a subtype of it",
  "tests/typing/accept/mixed-pair.det":
    "an annotated site fixes the mode of its context instead of being a subtype of it",
  "tests/typing/accept/mixed-sum.det":
    "an annotated site fixes the mode of its context instead of being a subtype of it",
  "tests/statistical/mixed.det":
    "an annotated site fixes the mode of its context instead of being a subtype of it",
  "tests/execution/bernoulli-general-operand.det":
    "a Bernoulli operand has the site's mode instead of a subtype of it",
  "tests/statistical/bernoulli-mixed.det":
    "a Bernoulli operand has the site's mode instead of a subtype of it",
  "examples/paper/gauss-random-walk.det":
    "a result type equals the first type checked against it, not the greatest one",
  "examples/loops/bounded-iteration.det":
    "a result type equals the first type checked against it, not the greatest one",
  "examples/loops/geometric-random-increment.det":
    "a result type equals the first type checked against it, not the greatest one",
  "examples/while.det":
    "a result type equals the first type checked against it, not the greatest one",
  "tests/execution/flip-prob.det":
    "a result type equals the first type checked against it, not the greatest one",
  "tests/typing/accept/identity.det": "unconstrained type variables are not read back as unit",
  "tests/execution/divergence.det": "unconstrained type variables are not read back as unit",
  "tests/typing/reject/infinite-type.det": "no occurs check, so unification overflows the stack",
};

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
    return {
      file,
      outcome,
      stage: optionalString(table, "stage"),
      expectedType: optionalString(table, "expected_type"),
      affinities,
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

const cases = readCases();

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
