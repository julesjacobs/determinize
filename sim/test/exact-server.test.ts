// The exploration that the exact worker runs: both programs in slices, with progress after each,
// replaced by a newer request; and the messages it sends, which pass their validators after
// structured cloning.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import type { ExactState } from "../src/core/exact.ts";
import { createExactServer } from "../src/core/exact.ts";
import type { ExactResponse } from "../src/core/protocol.ts";
import { isExactRequest, isExactResponse } from "../src/core/protocol.ts";
import { createExactClient } from "../src/ui/exact-client.ts";

const root = new URL("../../", import.meta.url);
const read = (file: string) => readFileSync(new URL(file, root), "utf8");

/** A server whose clock advances 1 ms at each reading, so that a slice takes 50 steps, and whose
 * deferred tasks run when `drain` says. */
function server() {
  const posted: ExactResponse[] = [];
  const tasks: (() => void)[] = [];
  let clock = 0;
  const explorer = createExactServer({
    post: (response) => posted.push(structuredClone(response)),
    defer: (task) => tasks.push(task),
    now: () => clock++,
  });
  const drain = (limit = Number.POSITIVE_INFINITY) => {
    for (let ran = 0; tasks.length > 0 && ran < limit; ran++) tasks.shift()?.();
  };
  return { explorer, posted, drain };
}

test("both programs of the noisy iteration explore to Lean's outcomes", () => {
  const { explorer, posted, drain } = server();
  const source = read("examples/paper/noisy-iteration.det");
  explorer.handle({ type: "exact", generation: 1, source, additive: false });
  drain();
  const last = posted.at(-1);
  assert.ok(last?.done);
  assert.equal(last.source.kind, "draw");
  if (last.source.kind !== "draw") return;
  assert.deepEqual([last.source.distribution, last.source.state], ["gaussian", 34]);
  assert.deepEqual(
    last.source.sites.map(({ from, to }) => source.slice(from, to)),
    ["gaussian(0, 1)"],
  );
  const determinized: ExactState = {
    kind: "finite",
    states: 68,
    returnProbability: { fraction: "1", value: 1 },
    rejectionProbability: { fraction: "0", value: 0 },
    mean: { fraction: "1/3", value: 1 / 3 },
    variance: { fraction: "2/9", value: 2 / 9 },
  };
  assert.deepEqual(last.determinized, determinized);
  for (const response of posted) assert.equal(isExactResponse(response), true);
});

test("a long exploration reports its progress after each slice", () => {
  const { explorer, posted, drain } = server();
  explorer.handle({
    type: "exact",
    generation: 1,
    source: read("examples/paper/dungeon.det"),
    additive: false,
  });
  drain();
  assert.ok(posted.length > 10, `${posted.length} responses`);
  const found = posted
    .map((response) => response.source)
    .filter((state) => state.kind === "exploring")
    .map((state) => (state.kind === "exploring" ? state.discovered : 0));
  assert.ok(found.length > 2);
  assert.deepEqual(
    found,
    [...found].sort((a, b) => a - b),
  );
  const last = posted.at(-1);
  assert.ok(last?.done);
  for (const state of [last.source, last.determinized]) {
    assert.equal(state.kind, "limit");
    assert.equal(state.kind === "limit" && state.limit, "states");
  }
  assert.equal(posted.filter((response) => response.done).length, 1);
});

test("a newer request replaces an exploration in progress", () => {
  const { explorer, posted, drain } = server();
  explorer.handle({
    type: "exact",
    generation: 1,
    source: read("examples/paper/dungeon.det"),
    additive: false,
  });
  drain(3);
  assert.ok(posted.length > 0 && posted.every((response) => response.generation === 1));
  const before = posted.length;
  explorer.handle({ type: "exact", generation: 2, source: "1 + 2", additive: true });
  drain();
  const after = posted.slice(before);
  assert.ok(after.length > 0);
  assert.ok(after.every((response) => response.generation === 2));
  assert.ok(after.at(-1)?.done);
});

test("a program that Lean rejects isn't explored, and the reply says so at once", () => {
  const { explorer, posted, drain } = server();
  explorer.handle({ type: "exact", generation: 1, source: "true + 1", additive: false });
  assert.equal(posted.length, 1);
  drain();
  assert.equal(posted.length, 1);
  assert.deepEqual(
    [posted[0].done, posted[0].source.kind, posted[0].determinized.kind],
    [true, "error", "error"],
  );
});

test("an exact request needs a source, a mode and a generation", () => {
  const request = { type: "exact", generation: 1, source: "1", additive: false };
  assert.equal(isExactRequest(request), true);
  assert.equal(isExactRequest({ ...request, additive: "no" }), false);
  assert.equal(isExactRequest({ ...request, source: undefined }), false);
  assert.equal(isExactRequest({ ...request, type: "run" }), false);
});

test("an exact response's states are checked by their kind", () => {
  const response = {
    type: "exact",
    generation: 1,
    source: { kind: "exploring", discovered: 3 },
    determinized: { kind: "limit", limit: "edges", discovered: 9, message: "…" },
    done: false,
  };
  assert.equal(isExactResponse(response), true);
  assert.equal(
    isExactResponse({ ...response, determinized: { ...response.determinized, limit: "time" } }),
    false,
  );
  assert.equal(isExactResponse({ ...response, source: { kind: "exploring" } }), false);
  assert.equal(
    isExactResponse({
      ...response,
      source: { kind: "finite", states: 2, returnProbability: { fraction: "1" } },
    }),
    false,
  );
});

test("a failure names every site that the failing state may stand for", () => {
  const { explorer, posted, drain } = server();
  // The two uniform draws have one structure, so their frames are one, as in Lean; only the one on
  // line 3 runs.
  const twins =
    "let f = fun u => uniform[G](0, 1) in\nlet b = flip(0.5) in\nif b then 0 else uniform[G](0, 1)";
  explorer.handle({ type: "exact", generation: 1, source: twins, additive: false });
  drain();
  const draw = posted.at(-1)?.source;
  assert.ok(draw?.kind === "draw");
  const lines = (text: string, sites: { from: number }[]) =>
    sites.map(({ from }) => text.slice(0, from).split("\n").length);
  assert.deepEqual(lines(twins, draw.sites), [1, 3]);
  // A discrete draw's frame keeps only its mode, so it stands for every discrete site of that mode.
  const discrete = "let a = discrete[G](1/2, *) in\nlet b = discrete[G](1, 1/2, *) in\na + b";
  explorer.handle({ type: "exact", generation: 2, source: discrete, additive: false });
  drain();
  const fails = posted.at(-1)?.source;
  assert.ok(fails?.kind === "fails");
  assert.equal(fails.detail, "negative discrete weight");
  assert.deepEqual(lines(discrete, fails.sites), [1, 2]);
});

test("a program too deep for the call stack is explored, to Lean's limit on a state's size", () => {
  const { explorer, posted, drain } = server();
  const deep = `let x = bernoulli[G](0.5) in ${"let y = x in ".repeat(2000)}x`;
  explorer.handle({ type: "exact", generation: 1, source: deep, additive: false });
  drain();
  const last = posted.at(-1);
  assert.ok(last?.done);
  for (const state of [last.source, last.determinized]) {
    assert.deepEqual(state.kind === "limit" && [state.limit, state.discovered], ["stateBytes", 0]);
  }
});

test("the page's own exploration, after a worker failed, gives way to a fresh worker's", async () => {
  const started: FakeWorker[] = [];
  class FakeWorker {
    posted: unknown[] = [];
    listeners = new Map<string, ((event: unknown) => void)[]>();
    constructor() {
      started.push(this);
    }
    postMessage(message: unknown) {
      this.posted.push(message);
    }
    addEventListener(type: string, listener: (event: unknown) => void) {
      this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
    }
    terminate() {}
    fail() {
      for (const listener of this.listeners.get("error") ?? []) listener({});
    }
  }
  const global = globalThis as Record<string, unknown>;
  global.Worker = FakeWorker;
  global.EXACT_WORKER_URL = "exact-worker.js";
  const received: ExactResponse[] = [];
  const send = createExactClient((response) => received.push(response));
  // Squaring 2/3 twenty times makes ever larger rationals: an exploration of seconds.
  const squares = "(rec f n => fun x => if n < 1 then x else f (n - 1) (x * x)) 20 (2 / 3)";
  send({ type: "exact", generation: 1, source: squares, additive: false });
  started[0].fail();
  // The page explores it on its own thread, in slices.
  const until = async (done: () => boolean) => {
    for (let waited = 0; !done() && waited < 5000; waited += 10) {
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
  };
  await until(() => received.some((response) => response.generation === 1));
  assert.ok(received.some((response) => response.generation === 1 && !response.done));
  send({ type: "exact", generation: 2, source: "1 + 2", additive: false });
  assert.equal(started.length, 2);
  assert.deepEqual(started[1].posted, [
    { type: "exact", generation: 2, source: "1 + 2", additive: false },
  ]);
  const before = received.length;
  await new Promise((resolve) => setTimeout(resolve, 300));
  assert.deepEqual(received.slice(before), []);
  delete global.Worker;
  delete global.EXACT_WORKER_URL;
});
