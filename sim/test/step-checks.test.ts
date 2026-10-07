// The step table's checks raise no false alarm: for every accepted case of the corpus manifest at
// seeds 1–50, every frame's checks pass. A program that isn't domain-safe at a seed ends in a
// domain failure, which is not a failed check.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { parse } from "smol-toml";
import { analyze } from "../src/core/compiler/analyze.ts";
import { sites } from "../src/core/compiler/core.ts";
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
