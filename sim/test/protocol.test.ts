// The messages between the page and its workers: the validators accept what the workers send and
// reject anything else, and the step table's run arrives in pages that stay small.
import assert from "node:assert/strict";
import test from "node:test";
import { isTraceRequest, isTraceResponse } from "../src/core/protocol.ts";
import { stateSize } from "../src/core/runtime/semantics.ts";
import { runCoupling } from "../src/core/trace.ts";
import {
  createTraceServer,
  maxPageFrames,
  maxPageSize,
  pageIndexOf,
  pageStarts,
} from "../src/core/trace-pages.ts";

/** A recursion `n` calls deep, whose state grows with each call. */
const deep = (n: number) =>
  `let u = uniform(0, 1) in (rec f n => if n < 1 then u else 1 + f (n - 1)) ${n}`;

test("a step table request needs a source, a seed and a generation, a page request a page", () => {
  assert.equal(isTraceRequest({ type: "trace", generation: 1, source: "1", seed: 7 }), true);
  assert.equal(isTraceRequest({ type: "trace", generation: 1, source: "1", seed: 0.5 }), false);
  assert.equal(isTraceRequest({ type: "trace", generation: 1, seed: 7 }), false);
  assert.equal(isTraceRequest({ type: "trace-page", generation: 1, page: 2 }), true);
  assert.equal(isTraceRequest({ type: "trace-page", generation: 1 }), false);
  assert.equal(isTraceRequest({ type: "run", generation: 1, source: "1", seed: 7 }), false);
  assert.equal(isTraceRequest(null), false);
});

test("the step table's responses pass their validator after structured cloning", () => {
  const server = createTraceServer();
  const first = structuredClone(
    server.handle({ type: "trace", generation: 2, source: deep(300), seed: 1 }),
  );
  assert.equal(isTraceResponse(first), true);
  assert.equal(first?.type, "trace");
  const page = structuredClone(server.handle({ type: "trace-page", generation: 2, page: 1 }));
  assert.equal(isTraceResponse(page), true);
  assert.equal(page?.type === "trace-page" && page.page.index, 1);
  const failed = server.handle({ type: "trace", generation: 3, source: "let x =", seed: 1 });
  assert.equal(failed?.type, "trace-failed");
  assert.equal(isTraceResponse(failed), true);
  // The run of an older request is gone.
  assert.equal(server.handle({ type: "trace-page", generation: 2, page: 1 }), null);
  if (first?.type !== "trace") return;
  assert.equal(isTraceResponse({ ...first, overview: { ...first.overview, ok: "yes" } }), false);
  assert.equal(
    isTraceResponse({ ...first, page: { ...first.page, frames: [{ step: 0 }] } }),
    false,
  );
  assert.equal(isTraceResponse({ type: "trace-failed", generation: 2 }), false);
  assert.equal(
    isTraceResponse({ type: "trace", page: first.page, overview: first.overview }),
    false,
  );
});

test("a page holds at most its number of frames and its size of states", () => {
  for (const source of [deep(2000), "(rec f x => f x) ()"]) {
    const { frames } = runCoupling(source, 1);
    const starts = pageStarts(frames);
    assert.equal(starts[0], 0);
    for (const [index, first] of starts.entries()) {
      const page = frames.slice(first, starts[index + 1] ?? frames.length);
      const size = page.reduce(
        (sum, frame) =>
          sum +
          stateSize(frame.original) +
          stateSize(frame.symbolic) +
          stateSize(frame.determinized),
        0,
      );
      assert.ok(page.length >= 1 && page.length <= maxPageFrames);
      assert.ok(page.length === 1 || size <= maxPageSize, `page ${index} holds ${size}`);
      assert.equal(pageIndexOf(starts, first + page.length - 1), index);
    }
  }
});

test("structured cloning keeps the frames' shared subexpressions shared", () => {
  const trace = runCoupling("let x = uniform(0, 1) in let y = x + 1 in y * y", 5);
  /** For each pair of the frames' states, whether they are the same object. */
  const sharing = (frames: typeof trace.frames) => {
    const states = frames.flatMap((frame) => [frame.original, frame.symbolic, frame.determinized]);
    return states.flatMap((a, i) => states.slice(i + 1).map((b) => a === b));
  };
  const before = sharing(trace.frames);
  assert.ok(before.some(Boolean));
  assert.deepEqual(sharing(structuredClone(trace).frames), before);
});

test("a run whose single frame exceeds a page's size ends with the size bound's status", () => {
  // Each let doubles the state, so the frames grow past a page's size within a few steps.
  const names = "abcdefghijklmnopqrst".split("");
  const lets = names.slice(1).map((name, i) => `let ${name} = (${names[i]}, ${names[i]}) in`);
  const source = `let a = uniform(0, 1) in ${lets.join(" ")} t`;
  const server = createTraceServer();
  const response = server.handle({ type: "trace", generation: 1, source, seed: 1 });
  assert.equal(response?.type, "trace");
  if (response?.type !== "trace") return;
  assert.equal(response.overview.stopped, "size");
  for (let index = 0; index < response.overview.pageStarts.length; index++) {
    const page = server.handle({ type: "trace-page", generation: 1, page: index });
    assert.equal(page?.type, "trace-page");
    if (page?.type !== "trace-page") continue;
    const size = page.page.frames.reduce(
      (sum, frame) =>
        sum + stateSize(frame.original) + stateSize(frame.symbolic) + stateSize(frame.determinized),
      0,
    );
    assert.ok(size <= maxPageSize, `page ${index} holds ${size}`);
  }
  assert.equal(server.handle({ type: "trace-page", generation: 1, page: 999 }), null);
});
