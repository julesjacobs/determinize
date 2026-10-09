// The exact solver stops at deadlines: a dense model's elimination, which takes long, yields after
// every row, so that the thread it runs on stays responsive, and gives the result that the solve
// in one go gives.
import assert from "node:assert/strict";
import test from "node:test";
import type { Model } from "../src/core/finite/model.ts";
import { rational } from "../src/core/finite/rational.ts";
import { solveStatistics, statisticsSteps } from "../src/core/finite/statistics.ts";

/** 255 transient states, each going to the next and to a scattered one with probability 1/2,
 * and a returning state at the end: elimination fills its rows in. */
function dense(): Model {
  const size = 256;
  const half = rational(1n, 2n);
  return {
    size,
    kind: Array.from({ length: size }, (_, state) =>
      state === size - 1
        ? { kind: "returned" as const, reward: rational(BigInt(state)) }
        : { kind: "transient" as const },
    ),
    rows: Array.from({ length: size }, (_, state) => {
      if (state === size - 1) return [{ target: state, probability: rational(1n) }];
      const targets = [state + 1, (state * 37 + 11) % (size - 1)];
      if (targets[0] === targets[1]) return [{ target: targets[0], probability: rational(1n) }];
      return targets.sort((a, b) => a - b).map((target) => ({ target, probability: half }));
    }),
  };
}

test("a dense model's solve stops at every deadline and gives the solve's result", () => {
  const model = dense();
  const steps = statisticsSteps(model);
  const slices: number[] = [];
  let yields = 0;
  for (;;) {
    const start = performance.now();
    let next = steps.next();
    while (!next.done && performance.now() - start < 50) {
      yields++;
      next = steps.next();
    }
    slices.push(performance.now() - start);
    if (next.done) {
      assert.deepEqual(next.value, solveStatistics(model));
      break;
    }
  }
  assert.ok(yields >= model.size, `${yields} yields`);
  const longest = Math.max(...slices);
  assert.ok(longest < 200, `slices of up to ${Math.round(longest)} ms`);
});
