import assert from "node:assert/strict";
import test from "node:test";
import { examples } from "../src/core/examples.ts";
import type { SharedState } from "../src/core/share.ts";
import { decodeShare, encodeShare, maxSharedBytes } from "../src/core/share.ts";

/** The fragment `#v1=` with `json` compressed as a link carries it. */
async function fragmentOf(json: string) {
  const stream = new Blob([json]).stream().pipeThrough(new CompressionStream("deflate-raw"));
  const bytes = new Uint8Array(await new Response(stream).arrayBuffer());
  return `#v1=${Buffer.from(bytes).toString("base64url")}`;
}

test("a link restores every example's program, seed and id", async () => {
  for (const [index, example] of examples.entries()) {
    const state: SharedState = {
      source: example.source,
      seed: 4294967295 - index,
      example: example.id,
    };
    const hash = await encodeShare(state);
    assert.match(hash, /^#v1=[A-Za-z0-9_-]+$/);
    assert.deepEqual(await decodeShare(hash), { kind: "state", state });
  }
});

test("the encoding is deflate-raw and unpadded base64url of the JSON", async () => {
  const state = { source: "let x = uniform(0, 1) in x ≥ 0.5", seed: 7, example: "paper/x" };
  assert.equal(await encodeShare(state), await fragmentOf(JSON.stringify(state)));
});

test("a link's state may have up to 64 KiB of JSON", async () => {
  const empty = JSON.stringify({ source: "", seed: 1, example: "e" }).length;
  const fits = { source: "x".repeat(maxSharedBytes - empty), seed: 1, example: "e" };
  assert.equal((await decodeShare(await encodeShare(fits))).kind, "state");
  const over = { ...fits, source: `${fits.source}x` };
  assert.deepEqual(await decodeShare(await encodeShare(over)), {
    kind: "error",
    message: "This link's program is larger than 64 KiB.",
  });
});

test("other fragments hold no state or can't be read", async () => {
  assert.deepEqual(await decodeShare(""), { kind: "none" });
  assert.deepEqual(await decodeShare("#debug"), { kind: "none" });
  const unreadable = { kind: "error", message: "This link could not be read." };
  for (const json of ["[1]", '{"source":"x","seed":1.5,"example":"e"}', '{"source":1}', "{"]) {
    assert.deepEqual(await decodeShare(await fragmentOf(json)), unreadable, json);
  }
  assert.deepEqual(await decodeShare("#v1=not-deflate"), unreadable);
  assert.deepEqual(await decodeShare("#v1=%%%"), unreadable);
});
