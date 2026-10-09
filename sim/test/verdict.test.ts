// The simulator's check of the theorems' premises: how a failed run is classified, where the first
// domain failures are, and what the check says for typical programs.
import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { test } from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import { runIndex, runnerOf } from "../src/core/sampler.ts";
import type { FailureKind, Summary } from "../src/core/statistics.ts";
import { addRuns, failureKind, noRuns, runsOf } from "../src/core/statistics.ts";
import { thin } from "../src/ui/charts.ts";
import { compareMeans, premiseVerdict } from "../src/ui/verdict.ts";

/** The kind of each failure of Lean's runtime that the port reproduces. Division by zero and
 * invalid distribution parameters are the failures that `DomainSafe` rules out for typed programs
 * (Spec/Semantics.lean); the runtime's own limits, non-finite floats and ill-typed operations are
 * not. */
const kinds: Record<string, FailureKind> = {
  "division by zero": "domain",
  "uniform requires lower ≤ upper": "domain",
  "gaussian requires variance ≥ 0": "domain",
  "discrete requires nonnegative probabilities": "domain",
  "discrete probabilities sum to more than one": "domain",
  "bernoulli requires probability in [0,1]": "domain",
  "poisson requires rate ≥ 0": "domain",
  "exponential requires rate > 0": "domain",
  "gamma requires positive shape and rate": "domain",
  "beta requires positive parameters": "domain",
  "step limit reached": "limit",
  "gamma sampler exceeded its rejection limit": "limit",
  "Poisson rate exceeds numerical runtime limit (1000000)": "limit",
  "Poisson sampler exceeded its iteration limit": "limit",
  "nonfinite arithmetic result": "float",
  "nonfinite distribution parameter": "float",
  "nonfinite numerical sampling result": "float",
  "expected a numeric value": "type",
  "expected a list of probabilities": "type",
  "unbound runtime variable": "type",
  "application of nonfunction": "type",
  "sum match of nonsum": "type",
  "list match of nonlist": "type",
  "expected a Boolean value": "type",
  "invalid primitive arity": "type",
};

test("every failure message of the runtime has the kind that the premises give it", () => {
  const runtime = new URL("../src/core/runtime/", import.meta.url);
  const messages = readdirSync(runtime)
    .filter((file) => file.endsWith(".ts"))
    .flatMap((file) => [
      ...readFileSync(new URL(file, runtime), "utf8").matchAll(
        /new (?:Failure|SamplingError)\("([^"]+)"[,)]/g,
      ),
    ])
    .map((match) => match[1]);
  assert.ok(messages.length > 20);
  for (const message of messages) {
    assert.ok(message in kinds, `classify "${message}" in this test`);
    assert.equal(failureKind(message), kinds[message], message);
  }
  assert.equal(failureKind("fst of nonpair"), "type");
});

test("the first domain failure is found across batches, and a step limit isn't one", () => {
  const first = runsOf([
    { kind: "returned", number: 1, display: "1" },
    { kind: "failed", message: "step limit reached" },
  ]);
  const second = runsOf([
    { kind: "failed", message: "nonfinite arithmetic result" },
    { kind: "failed", message: "gamma requires positive shape and rate" },
    { kind: "failed", message: "division by zero" },
  ]);
  assert.equal(first.firstDomainFailure, null);
  const summary = addRuns(addRuns(noRuns, first), second);
  assert.deepEqual(summary.firstDomainFailure, {
    run: 3,
    message: "gamma requires positive shape and rate",
  });
  assert.equal(summary.failed, 4);
});

function summary(runs: number, fields: Partial<Summary> = {}): Summary {
  return { ...noRuns, runs, ...fields };
}

/** The verdict on `count` runs of `source` from seed 1, as the sampler runs them. */
function checked(source: string, count = 10) {
  const analysis = analyze(source);
  const runner = runnerOf(analysis);
  assert.ok(runner, source);
  const outcomes = Array.from({ length: count }, (_, i) => runIndex(runner, 1, i).source);
  return premiseVerdict(analysis, addRuns(noRuns, runsOf(outcomes)), 1);
}

test("the noisy product: its type holds, no run fails, every run returns, integrability unchecked", () => {
  const verdict = premiseVerdict(
    analyze("let x = uniform(0, 1) in\nlet y = gaussian[E](x, 1) in\nx * y"),
    summary(10000),
    1,
  );
  assert.deepEqual(verdict, {
    type: { status: "holds", text: "holds" },
    safe: { status: "holds", text: `no domain failure in ${thin(10000)} runs` },
    returns: { status: "holds", text: `${thin(10000)} of ${thin(10000)} runs returned` },
    moments: { status: "unchecked", text: "not checked" },
    counterexample: false,
    failing: false,
  });
});

test("a run that fails on a domain error is the witness that domain safety fails", () => {
  const verdict = premiseVerdict(
    analyze("1 / uniform(0, 1)"),
    summary(10, { failed: 1, firstDomainFailure: { run: 4, message: "division by zero" } }),
    1,
  );
  assert.deepEqual(verdict?.safe, {
    status: "fails",
    text: "run 4 failed: division by zero (seed 5)",
  });
  assert.equal(verdict?.failing, true);
});

test("a literal or exact 0 fails domain safety; only an inexact 0, maybe an underflow, is open", () => {
  // Lean's corpus has the first, third, fourth and fifth (tests/execution).
  for (const [source, message] of [
    ["1 / 0", "division by zero"],
    ["0 / 0", "division by zero"],
    ["exponential(0)", "exponential requires rate > 0"],
    ["gamma(1, 0)", "gamma requires positive shape and rate"],
    ["beta(0, 1)", "beta requires positive parameters"],
    ["1 / (1 - 1)", "division by zero"],
    // An exact 0 times, or divided by, a continuous draw is an exact 0.
    ["let x = uniform(0, 1) in 1 / (0 * x)", "division by zero"],
    ["let x = uniform(0, 1) in 1 / (0 / x)", "division by zero"],
    ["let x = uniform(0, 1) in exponential(x * 0)", "exponential requires rate > 0"],
    ["gamma[G](-1, 1)", "gamma requires positive shape and rate"],
    ["gamma[G](0, -1)", "gamma requires positive shape and rate"],
  ]) {
    const verdict = checked(source);
    const text = `run 0 failed: ${message} (seed 1)`;
    assert.deepEqual(verdict.safe, { status: "fails", text }, source);
    assert.equal(verdict.failing, true, source);
  }
  const recursiveGamma =
    "let f = rec f n =>\n  if n <= 0 then 1 else gamma(f (n - 1), uniform(1, 2))\nin\nf 4";
  for (const [source, run, message] of [
    ["let x = uniform(0, 1) in 1 / (x - x)", 0, "division by zero"],
    ["1 / (1e-200 * 1e-200)", 0, "division by zero"],
    [recursiveGamma, 9, "gamma requires positive shape and rate"],
  ] as const) {
    const verdict = checked(source, 20);
    const text = `run ${run} failed at exactly 0, in floating point: ${message} (seed ${run + 1}), which may be an underflow the real-valued semantics doesn't reach`;
    assert.deepEqual(verdict.safe, { status: "open", text }, source);
    assert.equal(verdict.failing, false, source);
  }
  // Runs that failed at an inexact 0 say nothing about whether they return either.
  assert.equal(checked("let x = uniform(0, 1) in 1 / (x - x)").returns.status, "open");
});

test("a failure at an inexact 0 is kept apart from one that fails domain safety, which wins", () => {
  const message = "gamma requires positive shape and rate";
  const runs = addRuns(
    noRuns,
    runsOf([
      { kind: "failed", message, inexactZero: true },
      { kind: "failed", message },
    ]),
  );
  assert.deepEqual(runs.firstZeroFailure, { run: 0, message });
  assert.deepEqual(runs.firstDomainFailure, { run: 1, message });
  assert.equal(runs.stopped, 1);
  assert.equal(runs.zeroFailed, 1);
  const verdict = premiseVerdict(analyze("gamma[G](uniform(0, 1), 1)"), runs, 1);
  assert.deepEqual(verdict.safe, { status: "fails", text: `run 1 failed: ${message} (seed 2)` });
});

test("an output of type float[G] is typed at float[E] too", () => {
  const verdict = premiseVerdict(analyze("uniform[G](0, 1)"), summary(10), 1);
  assert.deepEqual(verdict.type, {
    status: "holds",
    text: "holds, as float[G] is a subtype of float[E]",
  });
  assert.equal(verdict.failing, false);
});

test("an output that isn't float[E], a counterexample, a rejected program and no returned run", () => {
  const bool = premiseVerdict(analyze("uniform(0, 1) < 0.5"), summary(1), 1);
  assert.deepEqual(bool?.type, { status: "fails", text: "the output has type bool" });
  const counter = premiseVerdict(
    analyze("let x = uniform[E](0, 1) in\nlet y = gaussian[E](x, 1) in\nx * y"),
    summary(1),
    1,
  );
  assert.equal(counter?.counterexample, true);
  assert.deepEqual(counter?.type, {
    status: "fails",
    text: "Lean rejects the written modes",
    detail: "inconsistent E/G constraints",
  });
  const rejected = premiseVerdict(analyze("let x ="), summary(0), 1);
  assert.deepEqual(rejected.type, {
    status: "fails",
    text: "Lean rejects the program at parsing",
    detail: "expected expression before end of input",
  });
  assert.deepEqual(rejected.safe, { status: "unchecked", text: "not run" });
  assert.equal(rejected.failing, true);
  const never = premiseVerdict(
    analyze("let _ = observe(false) in uniform(0, 1)"),
    summary(100, { rejected: 100 }),
    1,
  );
  assert.deepEqual(never?.returns, {
    status: "fails",
    text: "no run returned in 100 runs, so it likely fails",
  });
  assert.equal(never?.failing, true);
});

test("runs that the runtime's limits stopped leave the return probability open", () => {
  const deep = analyze("let f = rec f n => if n <= 0 then uniform(0, 1) else f (n - 1) in f 30000");
  const verdict = premiseVerdict(
    deep,
    summary(1000, { failed: 1000, stopped: 1000, firstFailure: "step limit reached" }),
    1,
  );
  assert.deepEqual(verdict.returns, {
    status: "open",
    text: `no run returned in ${thin(1000)} runs; ${thin(1000)} stopped at the runtime's limits or in floating point, which says nothing either way`,
  });
  assert.equal(verdict.failing, false);
  // Runs that failed otherwise, or that observe rejected, likely never return.
  const rejected = premiseVerdict(deep, summary(10, { rejected: 9, failed: 1, stopped: 1 }), 1);
  assert.equal(rejected.returns.status, "open");
  const never = premiseVerdict(deep, summary(10, { rejected: 10 }), 1);
  assert.equal(never.returns.status, "fails");
});

test("failed runs that aren't domain failures leave no domain failure found", () => {
  const verdict = premiseVerdict(
    analyze("uniform(0, 1)"),
    summary(4856, { failed: 55, stopped: 55, firstFailure: "step limit reached" }),
    1,
  );
  assert.deepEqual(verdict.safe, {
    status: "holds",
    text: `no domain failure in ${thin(4856)} runs`,
  });
  assert.doesNotMatch(verdict.safe.text, /no failing run/);
});

test("a program that the simulator fails on is left unchecked, not failing", () => {
  const verdict = premiseVerdict({ ok: false, stage: null, diagnostics: [] }, summary(0), 1);
  assert.deepEqual(verdict.type, {
    status: "unchecked",
    text: "not checked, as the simulator fails on the program",
  });
  assert.deepEqual(verdict.safe, { status: "unchecked", text: "not run" });
  assert.equal(verdict.failing, false);
});

test("means differ only where their 95 % intervals are disjoint, and need a number each", () => {
  const format = (value: number) => value.toFixed(4);
  const near = { n: 1000, mean: 0.5002, standardError: 0.01 };
  const far = { n: 1000, mean: 0.25, standardError: 0.01 };
  const half = { n: 1000, mean: 0.5, standardError: 0.01 };
  assert.equal(compareMeans(near, half, format), "Means: 0.5002 and 0.5000.");
  assert.equal(compareMeans(far, half, format), "Means differ: 0.2500 and 0.5000.");
  assert.equal(compareMeans({ n: 0, mean: 0, standardError: NaN }, half, format), null);
  // One returned number has no interval, so nothing says the means differ.
  assert.equal(
    compareMeans({ n: 1, mean: 3, standardError: NaN }, half, format),
    "Means: 3.0000 and 0.5000.",
  );
});
