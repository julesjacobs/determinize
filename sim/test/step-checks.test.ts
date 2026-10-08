// The step table's checks raise no false alarm: for every accepted case of the corpus manifest at
// seeds 1–50, every frame's checks pass. A program that isn't domain-safe at a seed ends in a
// domain failure, which is not a failed check.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { parse } from "smol-toml";
import { analyze } from "../src/core/compiler/analyze.ts";
import { node } from "../src/core/compiler/ast.ts";
import { sites } from "../src/core/compiler/core.ts";
import { exprEqual } from "../src/core/runtime/semantics.ts";
import { frameOk, runCoupling } from "../src/core/trace.ts";

const root = new URL("../../", import.meta.url);
const manifest = parse(readFileSync(new URL("tests/cases.toml", root), "utf8"));
const files = (manifest.case as { file: string; outcome: string }[])
  .filter((entry) => entry.outcome === "accept")
  .map((entry) => entry.file);

for (const file of files) {
  test(`the step checks pass: ${file}`, () => {
    const source = readFileSync(new URL(file, root), "utf8");
    const analysis = analyze(source);
    // A program without sample sites runs alike at every seed.
    const random = analysis.ok && sites(analysis.program.source).length > 0;
    for (let seed = 1; seed <= (random ? 50 : 1); seed++) {
      const failed = runCoupling(source, seed).frames.find((frame) => !frameOk(frame));
      assert.equal(failed, undefined, `seed ${seed}, step ${failed?.step}`);
    }
  });
}

// Large numbers round at their own scale. Each machine bounds the rounding error of every number
// it computes, also where large numbers cancel, a product distributes over a sum, or a draw's or a
// mean's parameters were rounded, and the checks allow for the two bounds.
const largeNumbers = [
  "let x = uniform(0, 1) in (1e10 + x) + 1e10",
  "let y = uniform(0, 1) in (y + 1e12) + (y + 0.1)",
  "let x = uniform(0, 1) in ((1e10 + x) - 1e10) * 1e6",
  "let a = gauss(1e10, 1) in let b = gauss(-1e10, 1) in (a + b) * 1e6",
  "let x = uniform(0, 1) in (x + 1e10) / 3",
  "let x = uniform(0, 1) in gauss((1e10 + x) + 1e10, 1)",
  "let x = uniform(0, 1e10) in let y = uniform(x, (x + 1e10) + 1e10) in y + 1e10",
  "let x = uniform(0, 1) in let y = uniform[G](0, 1) in ((1e10 + x) + y) + 1e10",
];

for (const source of largeNumbers) {
  test(`the step checks pass on large numbers: ${source}`, () => {
    for (let seed = 1; seed <= 50; seed++) {
      const failed = runCoupling(source, seed).frames.find((frame) => !frameOk(frame));
      assert.equal(failed, undefined, `seed ${seed}, step ${failed?.step}`);
    }
  });
}

test("two numbers are equal only within the bounds on their errors", () => {
  const number = (value: number, error?: number) =>
    node("Const", error ? { value, error } : { value }, 0, 0);
  const ulp = 2 ** -18; // of 2e10
  assert.ok(exprEqual(number(2e10, 2e-6), number(2e10 + ulp, 2e-6)));
  assert.ok(!exprEqual(number(2e10, 2e-6), number(2e10 + 4 * ulp, 2e-6)));
  assert.ok(!exprEqual(number(2e10), number(2e10 + ulp)));
});

test("a run that leaves an operation's domain ends in a domain failure", () => {
  const source = readFileSync(
    new URL("examples/paper/nested-uniform-parameters.det", root),
    "utf8",
  );
  const trace = runCoupling(source, 1);
  const last = trace.frames.at(-1);
  assert.equal(trace.ok, true);
  assert.equal(
    last?.domainFailure,
    "Source failed: uniform requires lower ≤ upper; " +
      "Determinized failed: uniform requires lower ≤ upper",
  );
});
