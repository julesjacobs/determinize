import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { Expr, ExprOf, MeanKind } from "../src/core/compiler/ast.ts";
import { prettyExpr } from "../src/core/compiler/pretty.ts";
import { examples } from "../src/core/examples.ts";
import { affineConst, affineScale, affineVar } from "../src/core/runtime/affine.ts";
import { meanDistribution, sampleDistribution } from "../src/core/runtime/distributions.ts";
import { makeStreams } from "../src/core/runtime/rng.ts";
import type { CoupledTrace, Frame } from "../src/core/runtime/semantics.ts";
import {
  checkEquivalences,
  exprEqual,
  prepareRuntime,
  projectMean,
  projectSample,
  runCoupledTrace,
  runOrdinary,
  runSymbolic,
  stepOrdinary,
} from "../src/core/runtime/semantics.ts";
import { runsOf } from "../src/core/statistics.ts";
import { outcomesOf, runCoupling } from "../src/core/trace.ts";

/** The last frame of a trace; every trace has one. */
function last(trace: CoupledTrace): Frame {
  const frame = trace.frames.at(-1);
  assert.ok(frame);
  return frame;
}

/** expr, asserted to be of the given kind. */
function expectKind<K extends Expr["kind"]>(expr: Expr | undefined, kind: K): ExprOf<K> {
  assert.equal(expr?.kind, kind);
  return expr as ExprOf<K>;
}

test("symbolic semantics stores E samples in sigma", () => {
  const { expr } = prepareRuntime("let u = uniform[E](0, 1) in\nu + 1");
  const result = runSymbolic(expr, makeStreams(7));
  assert.equal(result.sigma.length, 1);
  assert.equal(result.sigma[0].name, "v1");
  assert.equal(result.sigma[0].kind, "Uniform");
  assert.equal(prettyExpr(result.value), "1 + v1");
});

test("symbolic arithmetic on E samples is affine", () => {
  const { expr } = prepareRuntime(
    "let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\n2 * u + y - 1",
  );
  const result = runSymbolic(expr, makeStreams(11));
  assert.equal(result.sigma.length, 2);
  assert.equal(prettyExpr(result.value), "-1 + 2*v1 + v2");
});

test("G samples are sampled during symbolic stepping", () => {
  const { expr } = prepareRuntime(
    "let u = uniform[E](0, 1) in\nlet g = uniform[G](0, 2) in\ng + u",
  );
  const result = runSymbolic(expr, makeStreams(13));
  assert.equal(result.sigma.length, 1);
  assert.equal(result.value.kind, "SymFloat");
  assert.match(prettyExpr(result.value), /v1/);
});

test("sampled projection equals ordinary expression semantics with split streams", () => {
  const source =
    "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\n2 * g + 1";
  for (const seed of [1, 2, 3, 99]) {
    const result = checkEquivalences(source, seed);
    assert.equal(result.sampledEquivalent, true, `seed ${seed}`);
  }
});

test("mean projection equals determinized semantics under shared G randomness", () => {
  const source =
    "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\n2 * g + 1";
  for (const seed of [4, 5, 6, 100]) {
    const result = checkEquivalences(source, seed);
    assert.equal(result.meanEquivalent, true, `seed ${seed}`);
  }
});

test("projections produce concrete values", () => {
  const source = "let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\nu + y";
  const { expr } = prepareRuntime(source);
  const streams = makeStreams(21);
  const symbolic = runSymbolic(expr, streams);
  const sampled = projectSample(symbolic, streams.rngE);
  const mean = projectMean(symbolic);
  assert.equal(sampled.kind, "Const");
  assert.equal(mean.kind, "Const");
});

test("ordinary and determinized traces both terminate", () => {
  const source = "let x = gamma[E](1, 2) in\nlet y = gamma[G](1, 8) in\ny + x";
  const { expr, determinized } = prepareRuntime(source);
  const streams = makeStreams(31);
  assert.equal(runOrdinary(expr, streams).value.kind, "Const");
  assert.equal(runOrdinary(determinized, streams).value.kind, "Const");
});

test("determinized mean forms reduce in one primitive step", () => {
  const { determinized } = prepareRuntime("let u = uniform[E](0, 1) in\nu + 1");
  assert.equal(prettyExpr(determinized), "let u = mean_uniform(0, 1) in\nu + 1");
  const streams = makeStreams(33);
  const afterLetValueStep = stepOrdinary({
    expr: determinized,
    rngE: streams.rngE,
    rngG: streams.rngG,
  });
  assert.equal(prettyExpr(afterLetValueStep.expr), "let u = 0.5 in\nu + 1");
});

test("distribution means check the same domains as sampling", () => {
  const source = "let x = gauss[E](0, -1) in\nx + 1";
  const { expr, determinized } = prepareRuntime(source);
  assert.equal(prettyExpr(determinized), "let x = mean_gauss(0, -1) in\nx + 1");

  const ordinary = runOrdinary(expr, makeStreams(34));
  const mean = runOrdinary(determinized, makeStreams(34));
  assert.equal(ordinary.value.kind, "DomainError");
  assert.equal(mean.value.kind, "DomainError");
  assert.equal(ordinary.value.message, mean.value.message);
  assert.equal(ordinary.value.message, "gaussian requires variance ≥ 0");
});

test("coupled trace treats shared distribution domain errors as checked terminal outcomes", () => {
  const trace = runCoupledTrace("let x = gamma[E](-1, 2) in\nx + 1", 35);
  assert.equal(trace.ok, true);
  assert.equal(last(trace).original.kind, "DomainError");
  assert.equal(last(trace).symbolic.kind, "DomainError");
  assert.equal(last(trace).determinized.kind, "DomainError");
  assert.equal(trace.finalOriginal?.kind, "DomainError");
  assert.equal(trace.finalDeterminized?.kind, "DomainError");
  assert.equal(
    expectKind(last(trace).original, "DomainError").message,
    "gamma requires positive shape and rate",
  );
});

test("a run that fails where another doesn't ends in a domain failure", () => {
  // At seed 2, the E stream's first draw is 0.113, so x is about -5.5.
  const originalErrors = runCoupledTrace("let x = uniform[E](-10, 30) in\ngamma[E](x, 1)", 2);
  assert.equal(originalErrors.ok, true);
  assert.equal(last(originalErrors).original.kind, "DomainError");
  assert.notEqual(last(originalErrors).determinized.kind, "DomainError");
  assert.equal(
    last(originalErrors).domainFailure,
    "Original failed: gamma requires positive shape and rate",
  );

  const determinizedErrors = runCoupledTrace("let x = uniform[E](-10, 30) in\nuniform[E](x, 1)", 2);
  assert.equal(determinizedErrors.ok, true);
  assert.notEqual(last(determinizedErrors).original.kind, "DomainError");
  assert.equal(last(determinizedErrors).determinized.kind, "DomainError");
  assert.equal(
    last(determinizedErrors).domainFailure,
    "Determinized failed: uniform requires lower ≤ upper",
  );
});

test("distribution domain checks cover bernoulli probability and discrete totals", () => {
  const bernoulli = checkEquivalences("let x = bernoulli[E](1.5) in\nx", 36);
  assert.equal(bernoulli.sampledEquivalent, true);
  assert.equal(bernoulli.meanEquivalent, true);
  assert.equal(bernoulli.ordinary.value.kind, "DomainError");
  assert.equal(bernoulli.ordinary.value.message, "bernoulli requires probability in [0,1]");

  const discrete = runCoupledTrace("let x = discrete[E](0.6, 0.6, *) in\nx", 37);
  assert.equal(discrete.ok, true);
  assert.equal(last(discrete).symbolic.kind, "DomainError");
  assert.equal(
    expectKind(last(discrete).symbolic, "DomainError").message,
    "discrete probabilities sum to more than one",
  );
});

test("primitive distribution samples and means reject the same invalid concrete domains", () => {
  const cases: [MeanKind, number[], string][] = [
    ["Uniform", [2, 1], "uniform requires lower ≤ upper"],
    ["Gauss", [0, -1], "gaussian requires variance ≥ 0"],
    ["Exponential", [0], "exponential requires rate > 0"],
    ["Gamma", [0, 2], "gamma requires positive shape and rate"],
    ["Gamma", [1, 0], "gamma requires positive shape and rate"],
    ["Beta", [0, 2], "beta requires positive parameters"],
    ["Beta", [1, 0], "beta requires positive parameters"],
    ["Bernoulli", [1.5], "bernoulli requires probability in [0,1]"],
    ["Poisson", [-1], "poisson requires rate ≥ 0"],
  ];

  for (const [kind, args, text] of cases) {
    const message = { message: text };
    assert.throws(
      () => sampleDistribution(kind, args, makeStreams(50).rngG),
      message,
      `${kind} sample`,
    );
    assert.throws(() => meanDistribution(kind, args.map(affineConst)), message, `${kind} mean`);
  }
});

test("primitive distribution checks reject non-finite parameters and wrong arity", () => {
  assert.throws(() => sampleDistribution("Beta", [1], makeStreams(51).rngG), {
    message: "invalid primitive arity",
  });
  assert.throws(() => meanDistribution("Beta", [affineConst(1), affineConst(2), affineConst(3)]), {
    message: "invalid primitive arity",
  });
  assert.throws(() => sampleDistribution("Uniform", [0, Infinity], makeStreams(52).rngG), {
    message: "nonfinite distribution parameter",
  });
  assert.throws(
    () => meanDistribution("Uniform", [affineScale(affineVar("v"), Infinity), affineConst(1)]),
    { message: "nonfinite distribution parameter" },
  );
});

test("observe failure rejects the trace rather than throwing", () => {
  const source = "let _ = observe(false) in\n1";
  const { expr, determinized } = prepareRuntime(source);
  const streams = makeStreams(37);
  assert.equal(runOrdinary(expr, streams).value.kind, "Reject");
  assert.equal(runOrdinary(determinized, streams).value.kind, "Reject");
});

test("coupled trace checks sampled and mean projections at every symbolic step", () => {
  const source =
    "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\n2 * g + 1";
  for (const seed of [1, 17, 2026]) {
    const trace = runCoupledTrace(source, seed);
    assert.equal(trace.ok, true, `seed ${seed}`);
    assert.ok(trace.frames.length > 4);
    assert.equal(
      trace.frames.every((frame) => frame.originalOk && frame.determinizedOk),
      true,
    );
  }
});

test("coupled trace records sampled symbolic values for hover correspondence", () => {
  const trace = runCoupledTrace("let u = uniform(0, 1) in\nu + 1", 2026);
  const frame = trace.frames.find((candidate) => candidate.sampleBySymbol.v1 !== undefined);
  assert.ok(frame);
  assert.ok(frame.originalTarget);
  assert.equal(typeof frame.sampleBySymbol.v1, "number");
  assert.match(
    prettyExpr(frame.originalTarget),
    new RegExp(String(frame.sampleBySymbol.v1).replaceAll(".", "\\.")),
  );
});

test("coupled trace treats shared observe rejection as a checked terminal outcome", () => {
  const source = "let _ = observe(false) in\n1";
  const trace = runCoupledTrace(source, 41);
  assert.equal(trace.ok, true);
  assert.equal(last(trace).original.kind, "Reject");
  assert.equal(last(trace).symbolic.kind, "Reject");
  assert.equal(last(trace).determinized.kind, "Reject");
  assert.equal(trace.finalOriginal?.kind, "Reject");
  assert.equal(trace.finalDeterminized?.kind, "Reject");
});

test("coupled trace handles affine symbolic residuals at every step", () => {
  const source = "let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\n2 * u + y - 1";
  const trace = runCoupledTrace(source, 42);
  assert.equal(trace.ok, true);
  assert.match(prettyExpr(last(trace).symbolic), /v1/);
});

test("a mode conflict runs as a counterexample that exposes the bad E/G dependency", () => {
  const source =
    "let x = uniform[E](0, 1) in\nlet y = uniform[G](0, 1) in\nif x < 0.5 then x + y else x - y";
  const trace = runCoupledTrace(source, 42, 20, 20);
  assert.equal(trace.counterexample, true);
  assert.equal(trace.ok, false);
  assert.equal(last(trace).symbolicOk, false);
  assert.match(last(trace).symbolicError ?? "", /concrete affine value/);
  assert.equal(trace.finalOriginal?.kind, "Const");
  assert.equal(trace.finalDeterminized?.kind, "Const");
  assert.notEqual(
    expectKind(trace.finalOriginal, "Const").value,
    expectKind(trace.finalDeterminized, "Const").value,
  );
});

test("only mode conflicts run as counterexamples, with exactly the [E] draws at their means", () => {
  // The paper's signal example with both draws marked E: replacing them returns 1/4.
  const signal = "let x = uniform[E](0, 1) in\nlet y = gaussian[E](x, 1) in\nx * y";
  const analysis = analyze(signal);
  assert.equal(analysis.ok, false);
  assert.ok(!analysis.ok && analysis.stage === "inference" && analysis.counterexample);
  const prepared = prepareRuntime(signal);
  assert.equal(prepared.counterexample, true);
  for (const seed of [1, 2, 3]) {
    const value = runOrdinary(prepared.determinized, makeStreams(seed)).value;
    assert.equal(expectKind(value, "Const").value, 0.25);
  }
  // Unannotated sites stay random in the counterexample.
  const mixed = prepareRuntime("let x = uniform[E](0, 1) in\nlet y = uniform(0, 1) in\nx * y");
  assert.equal(
    prettyExpr(mixed.determinized),
    "let x = mean_uniform(0, 1) in\nlet y = uniform[G](0, 1) in\nx * y",
  );
  for (const source of ["true+1", "missing", "fun x => x x", "flip[E](0.5)", "1 +"]) {
    const result = analyze(source);
    assert.ok(!result.ok && !result.counterexample, source);
    assert.throws(() => prepareRuntime(source), source);
  }
});

test("recursive gamma coupling does not fail from floating-point underflow", () => {
  const source =
    "let f = rec f n =>\n  if n <= 0 then 1 else gamma(f (n - 1), uniform(1, 2))\nin\nf 4";
  for (const seed of [1, 2, 17, 42, 2026]) {
    const trace = runCoupledTrace(source, seed, 1000, 400);
    assert.equal(trace.ok, true, `seed ${seed}`);
    assert.equal(last(trace).symbolic.kind, "SymFloat");
  }
});

test("bundled examples analyze and run as intended", () => {
  const counterexamples = ["simulator/noisy-product-all-e", "simulator/bad-e-branching"];
  for (const example of examples) {
    const result = analyze(example.source);
    const counterexample = counterexamples.includes(example.id);
    assert.equal(result.ok, !counterexample, example.id);
    const trace = runCoupledTrace(example.source, 2026, 1000, 400);
    assert.equal(trace.counterexample, counterexample, example.id);
    assert.equal(trace.ok, !counterexample, example.id);
    assert.ok(trace.frames.length > 0, example.id);
  }
});

test("recursive parameter shadows the recursive function name", () => {
  for (const source of [
    "let f = rec x x => x in f 3",
    "let f = rec x x => (fun y => x) 2 in f 3",
    "let f = rec f x => if x <= 0 then 3 else f (x - 1) in f 2",
  ]) {
    const { expr } = prepareRuntime(source);
    assert.equal(expectKind(runOrdinary(expr, makeStreams(1)).value, "Const").value, 3);
  }
});

test("affine arithmetic preserves small literals and coefficients", () => {
  const { expr } = prepareRuntime("1e13 * (1e-13 * 1)");
  assert.equal(expectKind(runOrdinary(expr, makeStreams(1)).value, "Const").value, 1);
  const symbolic = runSymbolic(
    prepareRuntime("1e13 * (1e-13 * uniform[E](0, 1))").expr,
    makeStreams(1),
  );
  assert.equal(expectKind(symbolic.value, "SymFloat").affine.terms.v1, 1);
  assert.equal(affineConst(1e-13).constant, 1e-13);
  assert.equal(affineScale(affineVar("v"), 1e-13).terms.v, 1e-13);
  const tiny = runSymbolic(prepareRuntime("1e-13 * uniform[E](0, 1)").expr, makeStreams(1));
  assert.equal(prettyExpr(tiny.value), "1e-13*v1");
});

test("Poisson sampling preserves mean and variance above the underflow threshold", () => {
  const rng = makeStreams(1).rngG;
  for (const rate of [0, 1, 1000]) {
    let sum = 0;
    let squares = 0;
    const samples = 10000;
    for (let i = 0; i < samples; i++) {
      const value = sampleDistribution("Poisson", [rate], rng);
      assert.ok(Number.isInteger(value) && value >= 0);
      sum += value;
      squares += (value - rate) ** 2;
    }
    assert.ok(Math.abs(sum / samples - rate) <= 6 * Math.sqrt(rate / samples));
    assert.ok(Math.abs(squares / samples - rate) <= 0.1 * rate);
  }
});

test("division by zero and nonfinite results fail as in Lean's runtime", () => {
  const cases = [
    ["1 / 0", "division by zero"],
    ["let zero = uniform[G](0, 0) in 1 / zero", "division by zero"],
    ["1e300 * 1e300", "nonfinite arithmetic result"],
    ["1e400", "nonfinite arithmetic result"],
    ["if true then 1 else 1e400", null],
  ] as const;
  for (const [source, message] of cases) {
    const trace = runCoupling(source, 1);
    const expected = message
      ? { kind: "failed", message }
      : { kind: "returned", number: 1, display: "1" };
    assert.deepEqual(outcomesOf(trace), { source: expected, determinized: expected }, source);
    assert.equal(trace.ok, true, source);
  }
});

test("rejected and failed runs are counted apart from returned ones", () => {
  const runs = runsOf([
    { kind: "returned", number: 2, display: "2" },
    { kind: "rejected" },
    { kind: "failed", message: "division by zero" },
    { kind: "returned", number: null, display: "()" },
    { kind: "failed", message: "step limit reached" },
  ]);
  assert.deepEqual([...runs.values], [2, NaN, NaN, NaN, NaN]);
  assert.deepEqual(
    { ...runs, values: undefined },
    {
      values: undefined,
      rejected: 1,
      failed: 2,
      firstFailure: "division by zero",
      firstValue: "2",
    },
  );
});

test("numbers combine with the operations of Lean's runtime", () => {
  // 5 · (1/3) is 1.6666666666666665, one ulp below 5 / 3.
  const expected = { kind: "returned", number: 5 / 3, display: String(5 / 3) };
  assert.deepEqual(outcomesOf(runCoupling("5 / 3", 1)).source, expected);
});

test("a literal factor runs first, as Lean's elaborator puts it on the left", () => {
  const failed = { kind: "failed", message: "nonfinite arithmetic result" };
  assert.deepEqual(outcomesOf(runCoupling("(1 / 0) * 1e400", 1)).source, failed);
  assert.equal(
    prettyExpr(prepareRuntime("let x = uniform(0, 1) in x * 3").expr).slice(-5),
    "3 * x",
  );
});

test("expressions compare numbers up to the tolerance wherever they occur", () => {
  const expr = (text: string) => prepareRuntime(text).expr;
  // A projected affine form sums its constant first: (2.5 + v2) + v3 for v2 + (2.5 + v3).
  const program = "(fun acc => 0.5 + acc) 4.779884548700825";
  assert.ok(exprEqual(expr(program), expr("(fun acc => 0.5 + acc) 4.779884548700826")));
  assert.ok(!exprEqual(expr(program), expr("(fun acc => 0.5 + acc) 4.7798")));
  assert.ok(!exprEqual(expr(program), expr("(fun x => 0.5 + x) 4.779884548700825")));
  assert.ok(!exprEqual(expr("uniform[G](0, 1)"), expr("uniform[E](0, 1)")));
});
