import assert from "node:assert/strict";
import test from "node:test";
import { analyze } from "../src/compiler/analyze.js";
import { prettyExpr } from "../src/compiler/pretty.js";
import { examples } from "../src/examples.js";
import { checkEquivalences, prepareRuntime, projectMean, projectSample, runCoupledTrace, runOrdinary, runSymbolic } from "../src/runtime/semantics.js";
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
  const { expr } = prepareRuntime("let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\n2 * u + y - 1");
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
  const source = "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\n2 * g + 1";
  for (const seed of [1, 2, 3, 99]) {
    const result = checkEquivalences(source, seed);
    assert.equal(result.sampledEquivalent, true, `seed ${seed}`);
  }
});

test("mean projection equals determinized semantics under shared G randomness", () => {
  const source = "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\n2 * g + 1";
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

test("observe failure rejects the trace rather than throwing", () => {
  const source = "let _ = observe(false) in\n1";
  const { expr, determinized } = prepareRuntime(source);
  const streams = makeStreams(37);
  assert.equal(runOrdinary(expr, streams).value.kind, "Reject");
  assert.equal(runOrdinary(determinized, streams).value.kind, "Reject");
});

test("coupled trace checks sampled and mean projections at every symbolic step", () => {
  const source = "let u = uniform[E](0, 1) in\nlet b = beta[G](3, 2) in\nlet g = gamma[E](u, b) in\n2 * g + 1";
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
  const source = "let u = uniform[E](0, 1) in\nlet y = uniform[E](u, 2) in\n2 * u + y - 1";
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
