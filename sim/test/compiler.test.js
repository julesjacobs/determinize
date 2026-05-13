import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/compiler/analyze.js";
import { parse } from "../src/compiler/parser.js";
import { prettyExpr } from "../src/compiler/pretty.js";

test("parser preserves arithmetic precedence", () => {
  const ast = parse("uniform(0, 1) * 2 + 3");
  assert.equal(ast.kind, "Add");
  assert.equal(ast.left.kind, "Mul");
  assert.equal(ast.left.left.kind, "Uniform");
});

test("parser accepts comments and explicit distribution modes", () => {
  const ast = parse("(* sample symbolically *) uniform[E](0, 1)");
  assert.equal(ast.kind, "Uniform");
  assert.equal(ast.mode, "E");
  assert.equal(prettyExpr(ast), "uniform[E](0, 1)");
});

test("pretty printer keeps short let chains compact and aligned", () => {
  const ast = parse("let x = uniform[E](0, 1) in let y = uniform[G](0, 1) in x + y");
  assert.equal(
    prettyExpr(ast),
    "let x = uniform[E](0, 1) in\nlet y = uniform[G](0, 1) in\nx + y",
  );
});

test("pretty printer keeps short conditionals on one line", () => {
  const ast = parse("if x < 0.5 then x + y else x - y");
  assert.equal(prettyExpr(ast), "if x < 0.5 then x + y else x - y");
});

test("uniform determinizes to its mean by default", () => {
  const result = analyze("let a = uniform(0, 1) in\na + 2");
  assert.equal(result.ok, true);
  assert.match(result.pretty.determinized, /mean_uniform\(0, 1\)/);
});

test("multiplication keeps one sample symbolic and samples the other operand", () => {
  const result = analyze("uniform(0, 1) * uniform(1, 2)");
  assert.equal(result.ok, true);
  assert.match(result.pretty.elaboratedDefaulted, /uniform\[E\]/);
  assert.match(result.pretty.elaboratedDefaulted, /uniform\[G\]/);
  assert.match(result.pretty.determinized, /mean_uniform\(0, 1\) \* uniform\(1, 2\)/);
});

test("multiplication is asymmetric, so users commute to keep the symbolic operand on the left", () => {
  const result = analyze("uniform[E](1, 2) * uniform[G](0, 1)");
  assert.equal(result.ok, true);
  assert.match(result.pretty.determinized, /mean_uniform\(1, 2\) \* uniform\(0, 1\)/);
});

test("nonlinear variable use forces operand G but not result G", () => {
  const result = analyze("let x = uniform(0, 1) in\nx * x");
  assert.equal(result.ok, true);
  assert.match(result.pretty.elaboratedDefaulted, /let x : float\[G\]/);
  assert.match(result.pretty.elaboratedDefaulted, /\*.*: float\[E\]/s);
  assert.equal(result.pretty.determinized, "let x = uniform(0, 1) in\nx * x");
});

test("gamma dependency determinizes by expectation mode", () => {
  const result = analyze("let x = gamma(1, 2) in\nlet y = gamma(x, 8) in\ny + 1");
  assert.equal(result.ok, true);
  assert.match(result.pretty.determinized, /mean_gamma\(1, 2\)/);
  assert.match(result.pretty.determinized, /mean_gamma\(x, 8\)/);
});

test("syntax errors report diagnostics", () => {
  const result = analyze("let =");
  assert.equal(result.ok, false);
  assert.match(result.diagnostics[0].message, /expected identifier/);
});
