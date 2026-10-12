// The drawing of a contracted chain: the text of its nodes and edges, the dot source that
// Graphviz lays out, and the boxes, curves and label positions that it returns, in pixels with y
// downwards. Labels are set in the code face, whose advance is 8 px at 13 px; dot measures the
// edge labels in Courier at 13.333 pt, whose advance is the same, and the nodes have fixed sizes
// computed from their text.
import type { ChainData, Range } from "./exact.ts";

/** The labels' size, in px. */
export const labelSize = 13;
/** The code face's advance at `labelSize`, in px (0.615 em). */
export const advance = 8;
/** A node's number's size, in px, and its advance. */
export const numberSize = 12;
export const numberAdvance = 7.4;
/** The height of a line of a node's label. */
export const lineHeight = 16;
/** A node's padding inside its outline, across and down. */
export const padding = { x: 8, y: 5 };
/** The gap between a node's number and its text, which a rule divides. */
export const numberGap = 11;

/** A node's text: the number that the transition table shows too, and its lines. */
export interface NodeText {
  kind: "branch" | "returned" | "rejected" | "diverges";
  /** The draw's mode, at a branch. */
  mode: "E" | "G" | null;
  number: string;
  lines: string[];
}

export interface ChainText {
  /** Whether the chain adds rewards on its way: in the additive mode, where an edge or the entry
   * adds one that isn't 0. Only then are returns written as what they add. */
  sums: boolean;
  nodes: NodeText[];
  /** Each edge's label: its probability, and in the additive mode its reward where it isn't 0. */
  edges: string[];
  /** The entry's label: its reward, in the additive mode where it isn't 0. */
  entry: string | null;
}

/** A fraction with U+2212 for its sign. */
function signed(fraction: string) {
  return fraction.replace(/^-/, "−");
}

/** A reward as an addend: `+5`, `−1/2`. */
export function addend(fraction: string) {
  return fraction.startsWith("-") ? signed(fraction) : `+${fraction}`;
}

/** The lines of `sites` in `source`. */
export function sourceLines(sites: Range[], source: string) {
  return sites.map(({ from }) => source.slice(0, from).split("\n").length);
}

/** Line numbers as a phrase: "line 2", "lines 2 or 3", "lines 1, 2 or 5". */
export function linesText(numbers: number[]) {
  const lines = [...new Set(numbers)].sort((a, b) => a - b);
  const last = lines.pop();
  if (last === undefined) return "";
  return lines.length > 0 ? `lines ${lines.join(", ")} or ${last}` : `line ${last}`;
}

/** The text of `chain`'s nodes and edges, whose sites are ranges of `source`. A draw is named as
 * the source names it at its first site, with its mode, and its lines are `lines` of its sites,
 * by default the source's. Where the additive mode's chain adds rewards on its way, a return node
 * and the entry say what they add, as the edges do; elsewhere a return node holds the whole
 * output. */
export function chainText(
  chain: Extract<ChainData, { kind: "chain" }>,
  source: string,
  additive: boolean,
  lines: (sites: Range[]) => number[] = (sites) => sourceLines(sites, source),
): ChainText {
  const sums =
    additive && (chain.entry !== "0" || chain.edges.some(({ reward }) => reward !== "0"));
  const nodes = chain.nodes.map((node, i): NodeText => {
    const number = String(i + 1);
    switch (node.kind) {
      case "branch": {
        const [first] = node.sites;
        const written = first && /^[A-Za-z_]+/.exec(source.slice(first.from, first.to))?.[0];
        const name = `${written || node.distribution}[${node.mode}]`;
        return {
          kind: "branch",
          mode: node.mode,
          number,
          lines: [name, linesText(lines(node.sites))],
        };
      }
      case "returned":
        return {
          kind: "returned",
          mode: null,
          number,
          lines: [sums ? addend(node.value) : signed(node.value)],
        };
      default:
        return { kind: node.kind, mode: null, number, lines: [node.kind] };
    }
  });
  const edges = chain.edges.map(({ probability, reward }) =>
    sums && reward !== "0" ? `${probability}, ${addend(reward)}` : probability,
  );
  const entry = sums && chain.entry !== "0" ? addend(chain.entry) : null;
  return { sums, nodes, edges, entry };
}

/** A node's box, from its text. */
export function nodeSize(text: NodeText) {
  const longest = Math.max(...text.lines.map((line) => line.length));
  const width = 2 * padding.x + text.number.length * numberAdvance + numberGap + longest * advance;
  return { width: Math.ceil(width), height: text.lines.length * lineHeight + 2 * padding.y };
}

/** A label for dot, with an ASCII minus, which has the advance of U+2212, and with a space on
 * either side, so that dot keeps other edges a character's width from the label the page draws. */
function quoted(text: string) {
  return JSON.stringify(text ? ` ${text.replaceAll("−", "-")} ` : "");
}

/** The dot sources of a chain of `edges` between nodes 0… with `text`, top to bottom: first with
 * the initial node's successors in the order of its edges, so that the first draw's outcomes read
 * from left to right in both programs' drawings, then as dot orders them, which is at times
 * narrower. */
export function chainDots(text: ChainText, edges: { from: number; to: number }[]) {
  return [chainDot(text, edges, true), chainDot(text, edges, false)];
}

function chainDot(text: ChainText, edges: { from: number; to: number }[], ordered: boolean) {
  const inches = (px: number) => (px / 72).toFixed(4);
  const lines = [
    "digraph {",
    "rankdir=TB; nodesep=0.3; ranksep=0.32; margin=0;",
    'node [shape=box, fixedsize=true, label=""];',
    'edge [fontname="Courier", fontsize=13.333, arrowsize=0.6];',
    "start [shape=point, width=0.08];",
  ];
  for (const [i, node] of text.nodes.entries()) {
    const { width, height } = nodeSize(node);
    const order = ordered && i === 0 ? "ordering=out, " : "";
    lines.push(`n${i} [${order}width=${inches(width)}, height=${inches(height)}];`);
  }
  lines.push(`start -> n0 [id="entry", label=${quoted(text.entry ?? "")}];`);
  for (const [i, { from, to }] of edges.entries()) {
    lines.push(`n${from} -> n${to} [id="e${i}", label=${quoted(text.edges[i])}];`);
  }
  lines.push("}");
  return lines.join("\n");
}

/** A point, in px. */
export type Point = [number, number];

/** A laid-out edge: its cubic Bézier curve, from its first point through each next three, its
 * arrowhead, and its label's centre. */
export interface EdgeLayout {
  curve: Point[];
  arrow: Point[];
  label: Point | null;
}

/** A chain as dot lays it out, in px with y downwards: each node's centre and size, the entry's
 * start, the entry's edge, and each edge in the chain's order. */
export interface ChainLayout {
  width: number;
  height: number;
  nodes: { x: number; y: number; width: number; height: number }[];
  start: Point;
  entry: EdgeLayout;
  edges: EdgeLayout[];
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

function point(text: unknown): Point {
  if (typeof text !== "string") throw new Error("dot gave no position");
  const [x, y] = text.split(",").map(Number);
  if (!Number.isFinite(x) || !Number.isFinite(y)) throw new Error(`dot gave the position ${text}`);
  return [x, y];
}

function points(value: unknown): Point[] {
  if (!Array.isArray(value)) return [];
  return value.map((pair: unknown) => {
    if (!Array.isArray(pair) || pair.length !== 2 || !pair.every(Number.isFinite)) {
      throw new Error("dot gave a malformed point");
    }
    return [pair[0], pair[1]];
  });
}

/** The points of the first drawing operation of `kinds` in `ops`, as Graphviz's xdot gives
 * them. */
function drawn(ops: unknown, kinds: string): Point[] {
  if (!Array.isArray(ops)) return [];
  const op = ops.find((entry: unknown) => isRecord(entry) && kinds.includes(String(entry.op)));
  return isRecord(op) ? points(op.points) : [];
}

/** The layout of a chain of `nodes` and `edges` from Graphviz's JSON output, rendered with
 * `yInvert`, whose nodes are named `n<i>` and whose edges have the ids `entry` and `e<i>`. */
export function layoutOf(json: unknown, nodes: number, edges: number): ChainLayout {
  if (!isRecord(json) || !Array.isArray(json.objects)) throw new Error("dot gave no objects");
  const [, height, width] = String(json.bb).split(",").map(Number);
  const boxes: ChainLayout["nodes"] = new Array(nodes);
  let start: Point | null = null;
  for (const object of json.objects as unknown[]) {
    if (!isRecord(object)) continue;
    const [x, y] = point(object.pos);
    if (object.name === "start") {
      start = [x, y];
      continue;
    }
    const index = /^n(\d+)$/.exec(String(object.name));
    if (!index) continue;
    boxes[Number(index[1])] = {
      x,
      y,
      width: Number(object.width) * 72,
      height: Number(object.height) * 72,
    };
  }
  const laid = new Map<string, EdgeLayout>();
  for (const edge of Array.isArray(json.edges) ? (json.edges as unknown[]) : []) {
    if (!isRecord(edge)) continue;
    laid.set(String(edge.id), {
      curve: drawn(edge._draw_, "b"),
      arrow: drawn(edge._hdraw_, "pP"),
      label: typeof edge.lp === "string" && edge.label !== "" ? point(edge.lp) : null,
    });
  }
  const entry = laid.get("entry");
  const all = Array.from({ length: edges }, (_, i) => laid.get(`e${i}`));
  // Array.from fills the holes of nodes that dot left out, which `some` would skip.
  if (!start || !entry || Array.from(boxes).some((box) => !box)) {
    throw new Error("dot left out a node");
  }
  if (all.some((edge) => !edge || edge.curve.length < 4)) throw new Error("dot left out an edge");
  if (!Number.isFinite(width) || !Number.isFinite(height)) throw new Error("dot gave no size");
  return { width, height, nodes: boxes, start, entry, edges: all as EdgeLayout[] };
}

/** Of the layouts of one chain, in the order of `chainDots`, the narrowest, the first of equals. */
export function narrowest(layouts: ChainLayout[]) {
  return layouts.reduce((best, layout) => (layout.width < best.width - 0.5 ? layout : best));
}
