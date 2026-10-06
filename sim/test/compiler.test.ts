import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/compiler/analyze.ts";
import { CompileError } from "../src/compiler/errors.ts";
import { parse } from "../src/compiler/parser.ts";
import { prettyExpr } from "../src/compiler/pretty.ts";
import { rational } from "../src/compiler/rational.ts";

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

test("parser accepts what Lean's parser accepts", () => {
  for (const source of [
    "(* outer (* inner *) *) 1.25e-2",
    "(*) comment *) 0",
    "(* outer (*) inner *) *) 0",
    "discrete[E](*)",
    "discrete[E]( * )",
    "discrete[E](*\n)",
    "discrete[G] (* )",
    "discrete (* comment *) [E] (* )",
    "discrete[E] (* comment *) (0,1)",
    "discrete(0.25, x, *)",
    "discrete_list(0.5 :: [])",
    "discrete()",
    "let x' = gaussian(0, 1) in x'",
    "match [] with | [] => 0 | x :: xs => x",
    "1 + if true then 1 else 2",
    "2 * let x = 1 in x",
    "(fun f => f 1) lambda x => x",
    "let fun = 1 in 2",
    "fst inl 1",
    "1. + 1e10000 + 1E-10000",
    "flip[E](0.5)",
  ]) {
    assert.doesNotThrow(() => parse(source), source);
  }
});

test("parser rejects what Lean's parser rejects", () => {
  for (const source of [
    "uniform[Q](0,1)",
    "1 garbage )",
    "(* unfinished",
    "(* outer (* inner *) 0",
    "uniform(0)",
    "poisson(1,2)",
    "observe[E](true)",
    "discrete_list(*)",
    "uniform(0, *)",
    "1e10001",
    "2e",
    "2else",
    ".5",
    "f fun x => x",
    "1 > 0",
    "match [] with x :: xs => x | [] => 0",
  ]) {
    assert.throws(() => parse(source), CompileError, source);
  }
});

test("number literals are exact rationals", () => {
  const ast = parse("1.25e-2");
  assert.equal(ast.kind, "Const");
  assert.deepEqual(ast.exact, rational(1n, 80n));
  assert.equal(ast.value, 0.0125);
});

test("pretty printer keeps short let chains compact and aligned", () => {
  const ast = parse("let x = uniform[E](0, 1) in let y = uniform[G](0, 1) in x + y");
  assert.equal(prettyExpr(ast), "let x = uniform[E](0, 1) in\nlet y = uniform[G](0, 1) in\nx + y");
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
  assert.match(result.pretty.determinized, /uniform\(0, 1\) \* mean_uniform\(1, 2\)/);
});

test("multiplication is asymmetric, so users commute to keep the symbolic operand on the right", () => {
  const result = analyze("uniform[G](0, 1) * uniform[E](1, 2)");
  assert.equal(result.ok, true);
  assert.match(result.pretty.determinized, /uniform\(0, 1\) \* mean_uniform\(1, 2\)/);
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
