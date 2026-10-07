// The step table's small-step machine and the port of Lean's evaluator take their draws from the
// same streams in the same order: for every accepted case of the corpus manifest at seeds 1–20,
// both end the same way, with the same failure or the same value, bit for bit. A program that
// doesn't end runs out of both machines' steps.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { parse } from "smol-toml";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { Expr } from "../src/core/compiler/ast.ts";
import { sites } from "../src/core/compiler/core.ts";
import type { Value } from "../src/core/runtime/eval.ts";
import { prepare, run } from "../src/core/runtime/eval.ts";
import { isValue } from "../src/core/runtime/semantics.ts";
import { outcomesOf, runCoupling } from "../src/core/trace.ts";

const root = new URL("../../", import.meta.url);
const seeds = Array.from({ length: 20 }, (_, i) => i + 1);

/** Whether the step table's value `expr` is the evaluator's `value`. */
function same(expr: Expr, value: Value): boolean {
  switch (expr.kind) {
    case "Const":
      return value.tag === "number" && Object.is(expr.value + 0, value.value + 0);
    case "Bool":
      return value.tag === "bool" && expr.value === value.value;
    case "Unit":
      return value.tag === "unit";
    case "Nil":
      return value.tag === "nil";
    case "Pair":
      return value.tag === "pair" && same(expr.left, value.a) && same(expr.right, value.b);
    case "Inl":
    case "Inr":
      return (
        (value.tag === "inl" || value.tag === "inr") &&
        value.tag === expr.kind.toLowerCase() &&
        same(expr.expr, value.value)
      );
    case "Cons":
      return value.tag === "cons" && same(expr.head, value.head) && same(expr.tail, value.tail);
    case "Lam":
      return value.tag === "closure";
    case "Rec":
      return value.tag === "recursive";
    default:
      return false;
  }
}

/** The step table's final state of one program: the last frame's, or a run of its own after a
 * failed check. */
function final(expr: Expr | undefined, fallback: Expr | undefined) {
  return expr && isValue(expr) ? expr : fallback;
}

const manifest = parse(readFileSync(new URL("tests/cases.toml", root), "utf8"));
const files = (manifest.case as { file: string; outcome: string }[])
  .filter((entry) => entry.outcome === "accept")
  .map((entry) => entry.file);

for (const file of files) {
  test(`the step table and the evaluator agree: ${file}`, () => {
    const source = readFileSync(new URL(file, root), "utf8");
    const analysis = analyze(source);
    assert.ok(analysis.ok);
    const programs = {
      source: prepare(analysis.program.source),
      determinized: prepare(analysis.program.determinized),
    };
    // A program without sample sites runs alike at every seed.
    const random = sites(analysis.program.source).length > 0;
    for (const seed of random ? seeds : seeds.slice(0, 1)) {
      const trace = runCoupling(source, seed);
      const outcomes = outcomesOf(trace);
      const last = trace.frames.at(-1);
      const steps = {
        source: final(last?.original, trace.finalOriginal),
        determinized: final(last?.determinized, trace.finalDeterminized),
      };
      for (const which of ["source", "determinized"] as const) {
        const outcome = run(programs[which], BigInt(seed));
        const where = `${which} at seed ${seed}`;
        assert.equal(outcomes[which].kind, outcome.kind, where);
        if (outcome.kind === "failed") assert.deepEqual(outcomes[which], outcome, where);
        const expr = steps[which];
        if (outcome.kind === "returned") assert.ok(expr && same(expr, outcome.value), where);
      }
    }
  });
}
