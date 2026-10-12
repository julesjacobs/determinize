// The layout of chains by Graphviz's dot: every chain of a complete model of
// test/fixtures/lean-exact.json that the worker sends, in both modes, laid out as the chain worker
// lays it out, has its nodes apart and its labels inside the drawing; the text of the nodes and
// edges is as the page shows it; and the chain worker's messages are checked.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { instance } from "@viz-js/viz";
import type { ChainLayout, Point } from "../src/core/chain-layout.ts";
import {
  advance,
  chainDots,
  chainText,
  labelSize,
  layoutOf,
  lineHeight,
  linesText,
  narrowest,
  nodeSize,
  sourceLines,
} from "../src/core/chain-layout.ts";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { ChainData } from "../src/core/exact.ts";
import { chainData } from "../src/core/exact.ts";
import { explore, subjectProgram } from "../src/core/finite/explore.ts";
import { exploreAdditive } from "../src/core/finite/reward.ts";
import { isChainLayoutRequest, isChainLayoutResponse } from "../src/core/protocol.ts";
import type { ExactFixture } from "./lean-exact.ts";

const root = new URL("../../", import.meta.url);
const fixture: ExactFixture = JSON.parse(
  readFileSync(new URL("fixtures/lean-exact.json", import.meta.url), "utf8"),
);
const viz = await instance();

type Drawn = Extract<ChainData, { kind: "chain" }>;

function chainOf(source: string, subject: "source" | "determinized", additive: boolean) {
  const analysis = analyze(source);
  if (!analysis.ok) throw new Error("the simulator rejects a case Lean accepts");
  const program = subjectProgram(analysis.program, subject);
  const explored = additive
    ? exploreAdditive(program, analysis.program.source)
    : explore(program, analysis.program.source);
  return explored.kind === "complete" ? chainData(explored) : null;
}

/** The chain laid out as the chain worker lays it out: the narrowest of its dot sources. */
function layout(chain: Drawn, source: string, additive: boolean) {
  const text = chainText(chain, source, additive);
  const layouts = chainDots(text, chain.edges).map((dot) =>
    layoutOf(viz.renderJSON(dot, { yInvert: true }), chain.nodes.length, chain.edges.length),
  );
  return { text, layouts, layout: narrowest(layouts) };
}

/** The boxes of the drawing's parts: each node, and each edge label as the page sets it, centred
 * on its position. */
function boxes(drawing: ChainLayout, labels: (string | null)[]) {
  const nodes = drawing.nodes.map(({ x, y, width, height }) => ({
    left: x - width / 2,
    right: x + width / 2,
    top: y - height / 2,
    bottom: y + height / 2,
  }));
  const edges = [drawing.entry, ...drawing.edges].flatMap((edge, i) => {
    const text = labels[i];
    if (!edge.label || !text) return [];
    const [x, y] = edge.label;
    const half = (text.length * advance) / 2;
    return [
      { left: x - half, right: x + half, top: y - lineHeight / 2, bottom: y + lineHeight / 2 },
    ];
  });
  return { nodes, edges };
}

const drawn = new Map<string, { chain: Drawn; source: string; additive: boolean }>();
for (const entry of fixture.cases) {
  const source = readFileSync(new URL(entry.file, root), "utf8");
  for (const additive of [false, true]) {
    for (const subject of ["source", "determinized"] as const) {
      const outcome = entry[additive ? "additive" : "plain"][subject];
      const complete =
        "result" in outcome || outcome.message.startsWith("exact solver state limit exceeded");
      if (!complete) continue;
      const chain = chainOf(source, subject, additive);
      assert.ok(chain, `${entry.file} explores to a complete model`);
      if (chain.kind !== "chain" || chain.nodes.length === 1) continue;
      drawn.set(`${entry.file}, ${subject}${additive ? ", additive" : ""}`, {
        chain,
        source,
        additive,
      });
    }
  }
}

test("the fixture has chains of more than one node to lay out", () => {
  assert.ok(drawn.size >= 40, `${drawn.size} chains`);
});

for (const [name, { chain, source, additive }] of drawn) {
  test(`${name}: dot keeps the nodes apart and the labels in the drawing`, () => {
    const { text, layout: drawing } = layout(chain, source, additive);
    const { nodes, edges } = boxes(drawing, [text.entry, ...text.edges]);
    for (const [i, a] of nodes.entries()) {
      for (const b of nodes.slice(i + 1)) {
        const apart =
          a.right <= b.left || b.right <= a.left || a.bottom <= b.top || b.bottom <= a.top;
        assert.ok(apart, `nodes overlap: ${JSON.stringify([a, b])}`);
      }
    }
    for (const box of [...nodes, ...edges]) {
      const inside =
        box.left >= -0.5 &&
        box.top >= -0.5 &&
        box.right <= drawing.width + 0.5 &&
        box.bottom <= drawing.height + 0.5;
      assert.ok(inside, `${JSON.stringify(box)} outside ${drawing.width} × ${drawing.height}`);
    }
    for (const [i, node] of drawing.nodes.entries()) {
      const { width, height } = nodeSize(text.nodes[i]);
      assert.ok(Math.abs(node.width - width) < 0.01 && Math.abs(node.height - height) < 0.01);
    }
  });
}

test("the noisy iteration's determinized chain reads as the program does", () => {
  const source = readFileSync(new URL("examples/paper/noisy-iteration.det", root), "utf8");
  const chain = chainOf(source, "determinized", false);
  assert.ok(chain?.kind === "chain");
  assert.deepEqual(chainText(chain, source, false), {
    sums: false,
    nodes: [
      { kind: "branch", mode: "G", number: "1", lines: ["flip[G]", "line 2"] },
      { kind: "branch", mode: "G", number: "2", lines: ["flip[G]", "line 2"] },
      { kind: "returned", mode: null, number: "3", lines: ["0"] },
      { kind: "returned", mode: null, number: "4", lines: ["1"] },
    ],
    edges: ["1/2", "1/2", "1/2", "1/2"],
    entry: null,
  });
});

test("in the additive mode, returns and the entry say what they add, where something is added", () => {
  const source = "3 + (if flip(0.25) then 0 - 2 else 5)";
  const chain = chainOf(source, "source", true);
  assert.ok(chain?.kind === "chain");
  const text = chainText(chain, source, true);
  assert.equal(text.entry, "+3");
  assert.deepEqual(
    text.nodes.map((node) => node.lines),
    [["flip[G]", "line 1"], ["+5"], ["−2"]],
  );
  assert.deepEqual(text.edges, ["3/4", "1/4"]);
  assert.deepEqual(chainText(chain, source, false).nodes[2].lines, ["−2"]);
  // Without a reward on the way, a return holds the whole output, as in the plain mode.
  const plain = "if flip(0.25) then 0 - 2 else 5";
  const unsummed = chainOf(plain, "source", true);
  assert.ok(unsummed?.kind === "chain");
  assert.deepEqual(chainText(unsummed, plain, true), chainText(unsummed, plain, false));
  // dot measures an ASCII minus, whose advance is the same.
  const negative = { ...text, edges: ["3/4, −1", "1/4"], entry: "−3" };
  for (const dot of chainDots(negative, chain.edges)) {
    assert.match(dot, /label=" -3 "/);
    assert.match(dot, /label=" 3\/4, -1 "/);
    assert.doesNotMatch(dot, /−/);
  }
});

test("a node's lines name every site it may stand for", () => {
  const source = "a\nb\nc\nd\ne";
  const at = (line: number) => ({ from: source.indexOf("abcde"[line - 1]), to: 0 });
  const lines = (...numbers: number[]) => linesText(sourceLines(numbers.map(at), source));
  assert.equal(lines(2), "line 2");
  assert.equal(lines(3, 2), "lines 2 or 3");
  assert.equal(lines(1, 2, 5, 2), "lines 1, 2 or 5");
  assert.equal(linesText([]), "");
});

test("the first draw's outcomes read from left to right where that makes the drawing no wider", () => {
  const source =
    "let express = flip(0.3) in\nlet delay = discrete(0.25, 0.5, *) in\nif express then delay else 4 * delay";
  for (const subject of ["source", "determinized"] as const) {
    const chain = chainOf(source, subject, true);
    assert.ok(chain?.kind === "chain");
    const { layouts, layout: chosen } = layout(chain, source, true);
    assert.ok(chosen.width <= Math.min(...layouts.map(({ width }) => width)) + 0.5);
    // The coin's arms: node 2 (its first outcome) left of node 3, as the edges go.
    assert.ok(chosen.nodes[1].x < chosen.nodes[2].x, subject);
  }
});

test("dot measures every edge label at the code face's advance", () => {
  for (const { chain, source, additive } of drawn.values()) {
    const text = chainText(chain, source, additive);
    const [dot] = chainDots(text, chain.edges);
    const json = viz.renderJSON(dot, { yInvert: true }) as { edges?: unknown[] };
    for (const edge of json.edges ?? []) {
      const { label, _ldraw_: ops } = edge as { label?: string; _ldraw_?: unknown[] };
      if (!label) continue;
      const drawn = (ops ?? []).find((op) => (op as { op?: string }).op === "T") as
        | { width: number }
        | undefined;
      assert.ok(drawn, label);
      assert.ok(Math.abs(drawn.width - label.length * advance) < 0.05, `${label}: ${drawn.width}`);
    }
  }
});

test("the chain worker's messages pass their validators, and malformed ones don't", () => {
  const [{ chain, source, additive }] = drawn.values();
  const { layout: laid } = layout(chain, source, additive);
  const request = { type: "chain-layout", id: 3, dots: ["digraph {}"], nodes: 2, edges: 1 };
  assert.equal(isChainLayoutRequest(request), true);
  assert.equal(isChainLayoutRequest({ ...request, nodes: "2" }), false);
  assert.equal(isChainLayoutRequest({ ...request, dots: [] }), false);
  const response = structuredClone({ type: "chain-layout", id: 3, layout: laid });
  assert.equal(isChainLayoutResponse(response), true);
  assert.equal(isChainLayoutResponse({ type: "chain-layout-failed", id: 3, message: "…" }), true);
  const edge = { ...laid.entry, curve: laid.entry.curve.slice(0, 2) };
  assert.equal(isChainLayoutResponse({ ...response, layout: { ...laid, entry: edge } }), false);
  const node = { ...laid.nodes[0], x: Number.NaN };
  assert.equal(isChainLayoutResponse({ ...response, layout: { ...laid, nodes: [node] } }), false);
  assert.equal(isChainLayoutResponse({ ...response, id: "3" }), false);
});

test("a layout that misses a node is refused", () => {
  const json = viz.renderJSON('digraph { start [shape=point]; n0; start -> n0 [id="entry"]; }', {
    yInvert: true,
  });
  assert.throws(() => layoutOf(json, 2, 0), /left out a node/);
  assert.equal(layoutOf(json, 1, 0).nodes.length, 1);
});

/** Points along a laid-out edge's cubic Bézier curve. */
function along(curve: Point[], steps = 32): Point[] {
  const points: Point[] = [];
  for (let i = 0; i + 3 < curve.length; i += 3) {
    const [p0, p1, p2, p3] = curve.slice(i, i + 4);
    for (let k = 0; k <= steps; k++) {
      const t = k / steps;
      const u = 1 - t;
      const at = (j: 0 | 1) =>
        u * u * u * p0[j] + 3 * u * u * t * p1[j] + 3 * u * t * t * p2[j] + t * t * t * p3[j];
      points.push([at(0), at(1)]);
    }
  }
  return points;
}

test("every edge label keeps 3 px from the other edges", () => {
  const close: string[] = [];
  for (const [name, { chain, source, additive }] of drawn) {
    const { text, layout: drawing } = layout(chain, source, additive);
    const edges = [drawing.entry, ...drawing.edges];
    const labels = [text.entry, ...text.edges];
    for (const [i, edge] of edges.entries()) {
      const label = labels[i];
      if (!edge.label || !label) continue;
      const [x, y] = edge.label;
      // The label as the page draws it: its advance across, a line of the font down.
      const [halfWidth, halfHeight] = [(label.length * advance) / 2, (labelSize * 1.25) / 2];
      for (const [j, other] of edges.entries()) {
        if (j === i) continue;
        for (const [px, py] of along(other.curve)) {
          const dx = Math.max(0, Math.abs(px - x) - halfWidth);
          const dy = Math.max(0, Math.abs(py - y) - halfHeight);
          const gap = Math.hypot(dx, dy);
          if (gap < 3) {
            close.push(`${name}: "${label}" ${gap.toFixed(1)} px from edge ${j}`);
            break;
          }
        }
      }
    }
  }
  assert.deepEqual(close, []);
});

test("a node that stands for several sites names their lines", () => {
  const source = readFileSync(new URL("examples/loops/geometric-rejection.det", root), "utf8");
  for (const subject of ["source", "determinized"] as const) {
    const chain = chainOf(source, subject, true);
    assert.ok(chain?.kind === "chain");
    const branches = chainText(chain, source, true).nodes.filter((node) => node.kind === "branch");
    assert.equal(branches.length, 4);
    // Both flips have one structure, so each state of one stands for the other's line too.
    for (const node of branches) assert.deepEqual(node.lines, ["flip[G]", "lines 2 or 3"], subject);
  }
});
