import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { Input } from "../src/core/compiler/core.ts";
import { children } from "../src/core/compiler/core.ts";
import { elaborate } from "../src/core/compiler/elaborate.ts";
import { CompileError } from "../src/core/compiler/errors.ts";
import { parse } from "../src/core/compiler/parser.ts";
import { prettyExpr } from "../src/core/compiler/pretty.ts";
import { rational } from "../src/core/compiler/rational.ts";

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

/** An elaborated program in the constructor syntax of Lean's tests. */
function constructors(e: Input): string {
  const head =
    e.kind === "bvar"
      ? `.bvar ${e.index}`
      : e.kind === "bool"
        ? `.bool ${e.value}`
        : e.kind === "real"
          ? `.real ${e.value.num}/${e.value.den}`
          : "site" in e
            ? `.${e.kind} ${e.site ?? "none"}`
            : `.${e.kind}`;
  const parts = [head, ...children(e).map(constructors)];
  return parts.length === 1 && !head.includes(" ") ? head : `(${parts.join(" ")})`;
}

test("elaboration resolves and desugars as Lean's elaborator", () => {
  const cases: [string, string][] = [
    ["(* outer (* inner *) *) 1.25e-2", "(.real 1/80)"],
    ["rec f x => f x", "(.fix (.app (.bvar 1) (.bvar 0)))"],
    [
      "fun x => uniform[G](0,1) <= (fun y => x + y) (bernoulli(0.5))",
      "(.lam (.letE (.uniform G (.real 0/1) (.real 1/1)) (.letE (.app (.lam (.add (.bvar 2) (.bvar 0))) (.bernoulli none (.real 1/2))) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))))",
    ],
    ["discrete[E](0.25,0.25,0.5)", "(.discrete E (.cons (.real 1/4) (.cons (.real 1/4) .nil)))"],
    ["discrete[E] (* comment *) (0,1)", "(.discrete E (.cons (.real 0/1) .nil))"],
    ["discrete(*)", "(.discrete none .nil)"],
    ["discrete_list(0.5 :: [])", "(.discrete none (.cons (.real 1/2) .nil))"],
    ["fun x => x - 1", "(.lam (.add (.bvar 0) (.neg (.real 1/1))))"],
    ["fun x => x * 2", "(.lam (.mul (.real 2/1) (.bvar 0)))"],
    ["fun x => 2 * x", "(.lam (.mul (.real 2/1) (.bvar 0)))"],
    ["observe(true)", "(.ite (.bool true) .unit .reject)"],
    ["flip(0.5)", "(.lt (.real 0/1) (.bernoulli G (.real 1/2)))"],
  ];
  for (const [source, expected] of cases) {
    assert.equal(constructors(elaborate(parse(source))), expected, source);
  }
});

test("elaboration rejects what Lean's elaborator rejects", () => {
  for (const source of [
    "missing",
    "fun x => y",
    "flip[E](0.5)",
    "discrete()",
    "discrete(0, 0)",
    "discrete(0.2, 0.3)",
    "discrete(1, -1)",
    "let w = 1 in discrete(w, 1)",
    "discrete(missing, 1)",
  ]) {
    assert.throws(() => elaborate(parse(source)), CompileError, source);
  }
});

// The cases of Lean's lean/Tests/Inference.lean and lean/Tests/Completions.lean.
test("inference chooses Lean's greatest modes", () => {
  const cases: [string, string[]][] = [
    ["uniform(0,1) + gauss(2,1)", ["E", "E"]],
    ["let x = uniform(0,1) in if x < 0.5 then x else 0", ["G"]],
    ["uniform[G](1,2) * uniform[E](0,1)", ["G", "E"]],
    ["uniform[E](0,1) / uniform[G](1,2)", ["E", "G"]],
    ["if true then uniform(0,1) else uniform(0,1) * uniform(0,1)", ["E", "G", "E"]],
  ];
  for (const [source, affinities] of cases) {
    const result = analyze(source);
    assert.ok(result.ok, source);
    assert.deepEqual(result.affinities, affinities, source);
  }
});

test("inference accepts and rejects as Lean's inference", () => {
  for (const source of [
    "let x = uniform[E](0,1) in x*x",
    "uniform[E](0,1) < 0.5",
    "uniform[G](0,uniform[E](0,1))",
    "fun x => x x",
    "true + 1",
    "missing",
    "flip(uniform[E](0,1))",
    "discrete(1,2,3)",
    "discrete(0.2,0.3)",
    "discrete(-0.5,1.5)",
    "fun x => let f = fun y => x :: y in f x",
    "fun x => let f = fun y => x :: y :: [] in f (x :: [])",
    "fun f => let g = fun x => f x in g f",
    "fun x => let f = fun y => x :: y :: [] in let a = f true in f 0",
  ]) {
    assert.equal(analyze(source).ok, false, source);
  }
  for (const source of [
    "fun x => x",
    "[]",
    "inl 1",
    "(1,true)",
    "let x = uniform[G](0,1) in x + uniform[E](0,1)",
    "flip(0.5)",
    "bernoulli(0.5)",
    "discrete(0.25,0.25,0.5)",
    "observe(true)",
  ]) {
    assert.equal(analyze(source).ok, true, source);
  }
});

test("explicit modes are subtypes, not equalities", () => {
  for (const source of [
    "uniform[G](0,1) :: uniform[E](0,1) :: []",
    "uniform[E](0,1) :: uniform[G](0,1) :: []",
    "if true then (uniform[G](0,1), true) else (uniform[E](0,1), false)",
    "if true then inl uniform[G](0,1) else inl uniform[E](0,1)",
    "let f = if true then (fun x => uniform[G](0,1)) else (fun x => uniform[E](0,1)) in f 0",
    "let f = if true then (rec f x => if x < 0 then f (x+1) else uniform[G](0,1)) else (fun x => uniform[E](0,1)) in f 0",
  ]) {
    const result = analyze(source);
    assert.ok(result.ok, source);
    assert.ok(result.affinities.includes("G") && result.affinities.includes("E"), source);
  }
  for (const sample of ["uniform(0,1)", "uniform[E](0,1)"]) {
    for (const calls of [`f x + f (${sample})`, `f (${sample}) + f x`]) {
      const source = `let use = fun f => fun x => ${calls} + x*x in use (fun z => z) (uniform[G](0,1))`;
      const result = analyze(source);
      assert.ok(result.ok, source);
      assert.equal(result.type, "float[E]", source);
      assert.deepEqual(result.affinities, ["E", "G"], source);
    }
  }
});

test("unconstrained types read back as unit", () => {
  const result = analyze("fun x => x");
  assert.ok(result.ok);
  assert.equal(result.type, "(unit -> unit)");
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
  assert.deepEqual(result.affinities, ["G", "E"]);
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
  assert.deepEqual(result.affinities, ["G"]);
  assert.equal(result.type, "float[E]");
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
