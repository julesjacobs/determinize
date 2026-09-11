import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { checkEquivalences, runCoupledTrace } from "../src/runtime/semantics.js";

for (const name of ["nested", "coupling-branch", "coupling-mixed"]) {
  test(`retained coupling example: ${name}`, () => {
    const source = readFileSync(new URL(`../../tests/statistical/${name}.det`, import.meta.url), "utf8");
    for (const seed of [1, 2, 7, 31]) {
      const result = checkEquivalences(source, seed);
      assert.equal(result.sampledEquivalent, true, `sampled projection, seed ${seed}`);
      assert.equal(result.meanEquivalent, true, `mean projection, seed ${seed}`);
      const trace = runCoupledTrace(source, seed);
      assert.ok(trace.frames.length > 1);
      assert.equal(trace.ok, true, `trace, seed ${seed}`);
      assert.notEqual(trace.finalOriginal, undefined);
      assert.notEqual(trace.finalDeterminized, undefined);
    }
  });
}
