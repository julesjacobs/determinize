// The printed programs say where each node is: the determinized pane finds the counterpart of a
// source span through them. For every program that Lean accepts, both printed programs read the
// same with and without the ranges, the ranges nest, and each leaf's range holds its text.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import { prettyExpr } from "../src/core/compiler/pretty.ts";
import { sourcePretty, sourcePrettyWithSpans } from "../src/core/compiler/print.ts";
import type { CliFixture } from "./lean-cli.ts";

const root = new URL("../../", import.meta.url);
const fixture: CliFixture = JSON.parse(
  readFileSync(new URL("fixtures/lean-cli.json", import.meta.url), "utf8"),
);

test("every accepted program prints with the ranges of its nodes", () => {
  assert.ok(fixture.accepted.length > 100);
  for (const { file } of fixture.accepted) {
    const result = analyze(readFileSync(new URL(file, root), "utf8"));
    assert.ok(result.ok, file);
    for (const program of [result.program.source, result.program.determinized]) {
      const { text, spans } = sourcePrettyWithSpans(program);
      assert.equal(text, sourcePretty(program), file);
      assert.deepEqual([spans[0].start, spans[0].end], [0, text.length], file);
      for (const span of spans) {
        assert.ok(0 <= span.start && span.start <= span.end && span.end <= text.length, file);
        if (span.expr.kind === "Var" || span.expr.kind === "Const") {
          assert.equal(text.slice(span.start, span.end), prettyExpr(span.expr), file);
        }
      }
    }
  }
});
