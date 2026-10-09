// The sizes of states that Lean's exploration limits: the port of `reprStr` prints as Lean 4.34.1
// prints, and its cheap bound is never below the printed length. The fixture's runs at
// `--max-state-bytes` check the sizes of whole explorations against the CLI.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { Program } from "../src/core/compiler/core.ts";
import { rational } from "../src/core/compiler/rational.ts";
import { explore } from "../src/core/finite/explore.ts";
import type { State } from "../src/core/finite/machine.ts";
import { Failure, Machine } from "../src/core/finite/machine.ts";
import { formats, pretty, StateSizes } from "../src/core/finite/repr.ts";

const root = new URL("../../", import.meta.url);
const at = { from: 0, to: 0, source: null };

test("states print as Lean's reprStr prints them", () => {
  const machine = new Machine();
  const value = machine.cell(machine.number(rational(-3n)), machine.nil);
  assert.equal(
    pretty(formats.value(value, 0)),
    "Determinize.Finite.Value.cons (Determinize.Finite.Value.number -3) (Determinize.Finite.Value.nil)",
  );
  const site: Program = { ...at, kind: "discrete", site: "mean", a: { ...at, kind: "nil" } };
  const draw = machine.makeFrame({
    tag: "draw",
    action: "mean",
    op: { discrete: 2 },
    pending: [],
    env: null,
    args: [rational(1n, 2n)],
    at: site,
  });
  assert.equal(
    pretty(formats.frame(draw, 0)),
    [
      "Determinize.Finite.Frame.draw",
      "  (Determinize.Spec.Paper.DistributionAction.mean, Determinize.Spec.Paper.Op.discrete 2)",
      "  []",
      "  []",
      "  [(1 : Rat)/2]",
    ].join("\n"),
  );
  const discrete = machine.makeFrame({ tag: "discrete", action: "E", at: site });
  assert.equal(
    pretty(formats.frame(discrete, 0)),
    "Determinize.Finite.Frame.discrete (Determinize.Spec.Paper.DistributionAction.sample (Determinize.Spec.Paper.Affinity.E))",
  );
  const half = machine.code({ ...at, kind: "real", value: rational(-1n, 2n) });
  const negate = machine.makeFrame({ tag: "unary", op: "neg" });
  const state = machine.evalState(half, null, machine.push(negate, null));
  assert.equal(
    pretty(formats.state(state)),
    [
      "Determinize.Finite.State.eval",
      "  (Determinize.Spec.Paper.Expr.real (-1 : Rat)/2)",
      "  []",
      "  [Determinize.Finite.Frame.unary (Determinize.Finite.Unary.neg)]",
    ].join("\n"),
  );
  assert.equal(
    pretty(formats.state(machine.deliver(machine.unit, null))),
    "Determinize.Finite.State.deliver (Determinize.Finite.Value.unit) []",
  );
});

test("the bound of a state's size is never below its printed length", () => {
  for (const file of ["examples/paper/dungeon.det", "examples/simulator/random-list-sum.det"]) {
    const analysis = analyze(readFileSync(new URL(file, root), "utf8"));
    if (!analysis.ok) throw new Error(file);
    const machine = new Machine();
    const sizes = new StateSizes();
    const states: State[] = [machine.initial(analysis.program.determinized)];
    const seen = new Set([states[0].id]);
    for (let i = 0; i < states.length && states.length < 300; i++) {
      let step: ReturnType<Machine["step"]>;
      try {
        step = machine.step(states[i]);
      } catch (error) {
        if (error instanceof Failure) continue;
        throw error;
      }
      if (step.kind !== "next") continue;
      for (const [, next] of step.successors) {
        if (seen.has(next.id)) continue;
        seen.add(next.id);
        states.push(next);
      }
    }
    for (const state of states) {
      const length = pretty(formats.state(state)).length;
      assert.ok(sizes.bound(state) >= length, `${file}: a state of ${length} bytes`);
      assert.equal(sizes.exceeds(state, length), false);
      assert.equal(sizes.exceeds(state, length - 1), true);
    }
  }
});

test("an initial state too large ends exploration before it starts", () => {
  const analysis = analyze("let f = rec f n => if n < 1 then 0 else 1 + f (n - 1) in f 3");
  if (!analysis.ok) throw new Error("the program doesn't check");
  const { determinized, source } = analysis.program;
  const result = explore(determinized, source, {
    maxStates: 10000,
    maxEdges: 100000,
    maxStateBytes: 100,
  });
  assert.deepEqual(result, {
    kind: "incomplete",
    limit: "stateBytes",
    discovered: 0,
    expanded: 0,
    edges: 0,
  });
});
