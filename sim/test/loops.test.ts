// Every program of examples/loops/ that Lean accepts ends at seeds 1–20 in both machines: the step
// table reaches a value within its limits, and the evaluator behind "Run N" returns or rejects
// within Lean's fuel.
import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import { prepare, run } from "../src/core/runtime/eval.ts";
import { isValue } from "../src/core/runtime/semantics.ts";
import { runCoupling } from "../src/core/trace.ts";

const loops = new URL("../../examples/loops/", import.meta.url);
const seeds = Array.from({ length: 20 }, (_, i) => i + 1);

for (const file of readdirSync(loops)
  .filter((name) => name.endsWith(".det"))
  .sort()) {
  const source = readFileSync(new URL(file, loops), "utf8");
  const analysis = analyze(source);
  if (!analysis.ok) continue;
  test(`examples/loops/${file} ends in both machines at seeds 1–20`, () => {
    const programs = [prepare(analysis.program.source), prepare(analysis.program.determinized)];
    for (const seed of seeds) {
      const last = runCoupling(source, seed).frames.at(-1);
      assert.ok(last && isValue(last.symbolic), `the step table ends at seed ${seed}`);
      for (const program of programs) {
        const outcome = run(program, BigInt(seed));
        assert.notDeepEqual(
          outcome,
          { kind: "failed", message: "step limit reached" },
          `seed ${seed}`,
        );
      }
    }
  });
}
