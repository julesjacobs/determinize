import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import { lintDiagnostics, normalizeDiagnostics } from "../src/ui/editor.ts";

const cases = [
  {
    name: "empty program",
    source: "",
    message: "expected expression before end of input",
    from: 0,
    to: 0,
    highlight: "",
  },
  {
    name: "let binding missing value",
    source: "let x =",
    message: "expected expression before end of input",
    from: 6,
    to: 7,
    highlight: "=",
  },
  {
    name: "let binding missing in",
    source: "let x = 1",
    message: "expected `in` before end of input",
    from: 8,
    to: 9,
    highlight: "1",
  },
  {
    name: "let binding missing name",
    source: "let = 1 in 2",
    message: "expected identifier, found `=`",
    from: 4,
    to: 5,
    highlight: "=",
  },
  {
    name: "distribution missing close paren",
    source: "uniform(0, 1",
    message: "expected `)` before end of input",
    from: 11,
    to: 12,
    highlight: "1",
  },
  {
    name: "invalid distribution mode",
    source: "uniform[Q](0, 1)",
    message: "expected distribution mode `E` or `G`",
    from: 8,
    to: 9,
    highlight: "Q",
  },
  {
    name: "unexpected character",
    source: "uniform(0, @)",
    message: "unexpected character `@`",
    from: 11,
    to: 12,
    highlight: "@",
  },
  {
    name: "unterminated comment",
    source: "(* hello",
    message: "unterminated comment; expected `*)`",
    from: 0,
    to: 8,
    highlight: "(* hello",
  },
  {
    name: "unbound variable",
    source: "x + 1",
    message: "unbound variable `x`",
    from: 0,
    to: 1,
    highlight: "x",
  },
  {
    name: "if condition has wrong type",
    source: "if 0 then 1 else 2",
    message: "type mismatch: expected bool, found float",
    from: 3,
    to: 4,
    highlight: "0",
  },
  {
    name: "if branch has wrong type",
    source: "if true then 1 else false",
    message: "type mismatch: expected float, found bool",
    from: 20,
    to: 25,
    highlight: "false",
  },
  {
    name: "projection argument is not a pair",
    source: "fst 1",
    message: "type mismatch: expected unknown * unknown, found float",
    from: 4,
    to: 5,
    highlight: "1",
  },
  {
    name: "incompatible explicit modes",
    source: "uniform[E](0, 1) * uniform[E](0, 1)",
    message: "mode mismatch: an [E] value is used where a G value is required",
    from: 0,
    to: 16,
    highlight: "uniform[E](0, 1)",
  },
  {
    name: "flip probability cannot be E",
    source: "flip(uniform[E](0, 1))",
    message: "mode mismatch: an [E] value is used where a G value is required",
    from: 5,
    to: 21,
    highlight: "uniform[E](0, 1)",
  },
  {
    name: "discrete weights that do not sum to 1",
    source: "discrete(0.2, 1.7)",
    message: "discrete weights must sum to 1, not 1.9",
    from: 0,
    to: 18,
    highlight: "discrete(0.2, 1.7)",
  },
];

for (const item of cases) {
  test(`diagnostic: ${item.name}`, () => {
    const result = analyze(item.source);
    assert.equal(result.ok, false);
    assert.equal(result.diagnostics.length, 1);
    assert.equal(result.diagnostics[0].message, item.message);

    const diagnostic = normalizeDiagnostics(result, item.source)[0];
    assert.deepEqual(
      {
        from: diagnostic.from,
        to: diagnostic.to,
        message: diagnostic.message,
        highlight: item.source.slice(diagnostic.from, diagnostic.to),
      },
      {
        from: item.from,
        to: item.to,
        message: item.message,
        highlight: item.highlight,
      },
    );
  });
}

test("the editor shows Lean's rejections as errors from the stage that rejects", () => {
  const source = "uniform[E](0, 1) * uniform[E](0, 1)";
  const [diagnostic] = lintDiagnostics(analyze(source), source);
  assert.deepEqual(
    { severity: diagnostic.severity, source: diagnostic.source },
    { severity: "error", source: "Lean's front end, inference" },
  );
  assert.deepEqual(lintDiagnostics(analyze("1 + 2"), "1 + 2"), []);
});
