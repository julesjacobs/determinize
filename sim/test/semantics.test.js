import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/compiler/analyze.js";
import { prettyExpr } from "../src/compiler/pretty.js";
import { examples } from "../src/examples.js";
import { affineConst, affineScale, affineVar } from "../src/runtime/affine.js";
import { meanDistribution, sampleDistribution } from "../src/runtime/distributions.js";
import { checkEquivalences, prepareRuntime, projectMean, projectSample, runCoupledTrace, runOrdinary, runSymbolic, stepOrdinary } from "../src/runtime/semantics.js";
import { makeStreams } from "../src/runtime/rng.js";

test("symbolic semantics stores E samples in sigma", () => {
  const { expr } = prepareRuntime("let u = uniform[E](0, 1) in\nu + 1");
  const result = runSymbolic(expr, makeStreams(7));
  assert.equal(result.sigma.length, 1);
  assert.equal(result.sigma[0].name, "v1");
  assert.equal(result.sigma[0].kind, "Uniform");
  assert.equal(prettyExpr(result.value), "1 + v1");
});

test("symbolic arithmetic on E samples is affine", () => {
  const { expr } = prepareRuntime("let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\nu * 2 + y - 1");
  const result = runSymbolic(expr, makeStreams(11));
  assert.equal(result.sigma.length, 2);
  assert.equal(prettyExpr(result.value), "-1 + 2*v1 + v2");
});

test("G samples are sampled during symbolic stepping", () => {
  const { expr } = prepareRuntime("let u = uniform[E](0, 1) in\nlet g = uniform[G](0, 2) in\ng + u");
  const result = runSymbolic(expr, makeStreams(13));
  assert.equal(result.sigma.length, 1);
  assert.equal(result.value.kind, "SymFloat");
  assert.match(prettyExpr(result.value), /v1/);
});

test("sampled projection equals ordinary expression semantics with split streams", () => {
  const source = "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\ng * 2 + 1";
  for (const seed of [1, 2, 3, 99]) {
    const result = checkEquivalences(source, seed);
    assert.equal(result.sampledEquivalent, true, `seed ${seed}`);
  }
});

test("mean projection equals determinized semantics under shared G randomness", () => {
  const source = "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\ng * 2 + 1";
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
  const afterLetValueStep = stepOrdinary({ expr: determinized, rngE: streams.rngE, rngG: streams.rngG });
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
  assert.match(ordinary.value.message, /variance must be >= 0/);
});

test("coupled trace treats shared distribution domain errors as checked terminal outcomes", () => {
  const trace = runCoupledTrace("let x = gamma[E](-1, 2) in\nx + 1", 35);
  assert.equal(trace.ok, true);
  assert.equal(trace.frames.at(-1).original.kind, "DomainError");
  assert.equal(trace.frames.at(-1).symbolic.kind, "DomainError");
  assert.equal(trace.frames.at(-1).determinized.kind, "DomainError");
  assert.equal(trace.finalOriginal.kind, "DomainError");
  assert.equal(trace.finalDeterminized.kind, "DomainError");
  assert.match(trace.frames.at(-1).original.message, /shape must be > 0/);
});

test("coupled trace fails when only one side reaches a distribution domain error", () => {
  const originalErrors = runCoupledTrace("let x = uniform[E](-10, 30) in\ngamma[E](x, 1)", 1);
  assert.equal(originalErrors.ok, false);
  assert.equal(originalErrors.frames.at(-1).original.kind, "DomainError");
  assert.notEqual(originalErrors.frames.at(-1).determinized.kind, "DomainError");
  assert.equal(originalErrors.frames.at(-1).consistencyOk, false);
  assert.match(originalErrors.frames.at(-1).consistencyError, /terminal effect mismatch/);

  const determinizedErrors = runCoupledTrace("let x = uniform[E](-10, 30) in\nuniform[E](x, 1)", 1);
  assert.equal(determinizedErrors.ok, false);
  assert.notEqual(determinizedErrors.frames.at(-1).original.kind, "DomainError");
  assert.equal(determinizedErrors.frames.at(-1).determinized.kind, "DomainError");
  assert.equal(determinizedErrors.frames.at(-1).consistencyOk, false);
  assert.match(determinizedErrors.frames.at(-1).consistencyError, /terminal effect mismatch/);
});

test("distribution domain checks cover bernoulli probability and discrete totals", () => {
  const bernoulli = checkEquivalences("let x = bernoulli[E](1.5) in\nx", 36);
  assert.equal(bernoulli.sampledEquivalent, true);
  assert.equal(bernoulli.meanEquivalent, true);
  assert.equal(bernoulli.ordinary.value.kind, "DomainError");
  assert.match(bernoulli.ordinary.value.message, /probability must be in \[0, 1\]/);

  const discrete = runCoupledTrace("let x = discrete[E](0.2, 0.2) in\nx", 37);
  assert.equal(discrete.ok, true);
  assert.equal(discrete.frames.at(-1).symbolic.kind, "DomainError");
  assert.match(discrete.frames.at(-1).symbolic.message, /probabilities must sum to 1/);
});

test("primitive distribution samples and means reject the same invalid concrete domains", () => {
  const cases = [
    ["Uniform", [2, 1], /lower bound must be <= upper bound/],
    ["Gauss", [0, -1], /variance must be >= 0/],
    ["Exponential", [0], /rate must be > 0/],
    ["Gamma", [0, 2], /shape must be > 0/],
    ["Gamma", [1, 0], /rate must be > 0/],
    ["Beta", [0, 2], /alpha must be > 0/],
    ["Beta", [1, 0], /beta must be > 0/],
    ["Bernoulli", [1.5], /probability must be in \[0, 1\]/],
    ["Poisson", [-1], /lambda must be >= 0/],
  ];

  for (const [kind, args, message] of cases) {
    assert.throws(() => sampleDistribution(kind, args, makeStreams(50).rngG), message, `${kind} sample`);
    assert.throws(() => meanDistribution(kind, args.map(affineConst)), message, `${kind} mean`);
  }
});

test("primitive distribution checks reject non-finite parameters and wrong arity", () => {
  assert.throws(
    () => sampleDistribution("Beta", [1], makeStreams(51).rngG),
    /domain error in beta: expected 2 parameters, got 1/,
  );
  assert.throws(
    () => meanDistribution("Beta", [affineConst(1), affineConst(2), affineConst(3)]),
    /domain error in beta: expected 2 parameters, got 3/,
  );
  assert.throws(
    () => sampleDistribution("Uniform", [0, Infinity], makeStreams(52).rngG),
    /domain error in uniform: parameters must be finite/,
  );
  assert.throws(
    () => meanDistribution("Uniform", [affineScale(affineVar("v"), Infinity), affineConst(1)]),
    /domain error in uniform: parameters must be finite/,
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
  const source = "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\ng * 2 + 1";
  for (const seed of [1, 17, 2026]) {
    const trace = runCoupledTrace(source, seed);
    assert.equal(trace.ok, true, `seed ${seed}`);
    assert.ok(trace.frames.length > 4);
    assert.equal(trace.frames.every((frame) => frame.originalOk && frame.determinizedOk), true);
  }
});

test("coupled trace records sampled symbolic values for hover correspondence", () => {
  const trace = runCoupledTrace("let u = uniform(0, 1) in\nu + 1", 2026);
  const frame = trace.frames.find((candidate) => candidate.sampleBySymbol.v1 !== undefined);
  assert.ok(frame);
  assert.equal(typeof frame.sampleBySymbol.v1, "number");
  assert.match(prettyExpr(frame.originalTarget), new RegExp(String(frame.sampleBySymbol.v1).replaceAll(".", "\\.")));
});

test("coupled trace treats shared observe rejection as a checked terminal outcome", () => {
  const source = "let _ = observe(false) in\n1";
  const trace = runCoupledTrace(source, 41);
  assert.equal(trace.ok, true);
  assert.equal(trace.frames.at(-1).original.kind, "Reject");
  assert.equal(trace.frames.at(-1).symbolic.kind, "Reject");
  assert.equal(trace.frames.at(-1).determinized.kind, "Reject");
  assert.equal(trace.finalOriginal.kind, "Reject");
  assert.equal(trace.finalDeterminized.kind, "Reject");
});

test("coupled trace handles affine symbolic residuals at every step", () => {
  const source = "let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\nu * 2 + y - 1";
  const trace = runCoupledTrace(source, 42);
  assert.equal(trace.ok, true);
  assert.match(prettyExpr(trace.frames.at(-1).symbolic), /v1/);
});

test("unchecked coupled trace exposes bad E/G dependencies", () => {
  const source = "let x = uniform[E](0, 1) in\nlet y = uniform[G](0, 1) in\nif x < 0.5 then x + y else x - y";
  const trace = runCoupledTrace(source, 42, 20, 20, { allowIllTyped: true });
  assert.equal(trace.unchecked, true);
  assert.equal(trace.ok, false);
  assert.equal(trace.frames.at(-1).symbolicOk, false);
  assert.match(trace.frames.at(-1).symbolicError, /concrete affine value/);
  assert.equal(trace.finalOriginal.kind, "Const");
  assert.equal(trace.finalDeterminized.kind, "Const");
  assert.notEqual(trace.finalOriginal.value, trace.finalDeterminized.value);
});

test("recursive gamma coupling does not fail from floating-point underflow", () => {
  const source = "let f = rec f n =>\n  if n <= 0 then 1 else gamma(f (n - 1), uniform(1, 2))\nin\nf 4";
  for (const seed of [1, 2, 17, 42, 2026]) {
    const trace = runCoupledTrace(source, seed, 1000, 400);
    assert.equal(trace.ok, true, `seed ${seed}`);
    assert.equal(trace.frames.at(-1).symbolic.kind, "SymFloat");
  }
});

test("bundled examples analyze and run as intended", () => {
  for (const example of examples) {
    const result = analyze(example.source);
    const intentionallyBad = example.name === "Bad E-branching";
    assert.equal(result.ok, !intentionallyBad, example.name);
    const trace = runCoupledTrace(example.source, 2026, 1000, 400, { allowIllTyped: intentionallyBad });
    assert.equal(trace.ok, !intentionallyBad, example.name);
    assert.ok(trace.frames.length > 0, example.name);
  }
});
