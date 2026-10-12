// The figure of both programs' Markov chains, below the distributions: each program's chain with
// its probability-1 steps contracted, laid out by Graphviz in the chain worker and drawn here in
// the tokens' colours, the source in outline and the determinized program solid; or, where there's
// nothing to draw, a sentence: a single node, a chain larger than the figure draws, or why the
// program has no model. A table of each chain's transitions is the figure's equivalent and its
// keyboard layer. Hovering a node, or a row, highlights it and the sites of its draws in both
// program panes. A button in the exact values' header shows and hides the figure, wherever a
// program has a chain; the figure opens only on that action, so that Graphviz's bundle loads only
// then, and shows at once what needs no layout. A new program's drawing replaces the last once it
// is laid out. Where a drawing is wider than its column, the columns stack. What the drawings are,
// the "i" beside the title says, on hover or on a click.
import type { ReadonlySignal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import type { ChainLayout, ChainText, EdgeLayout, NodeText, Point } from "../core/chain-layout.ts";
import {
  addend,
  chainDots,
  chainText,
  lineHeight,
  numberAdvance,
  numberGap,
  padding,
  sourceLines,
} from "../core/chain-layout.ts";
import type { PrintedSpan } from "../core/compiler/pretty.ts";
import type { ChainData, ExactState, Range } from "../core/exact.ts";
import { chainCap } from "../core/exact.ts";
import { add, fraction, rational } from "../core/finite/rational.ts";
import type { LayoutClient } from "./chain-client.ts";
import { minus, thin } from "./charts.ts";
import { reasonText } from "./exact-view.ts";
import { escapeHtml } from "./html.ts";
import { printedRange } from "./linking.ts";
import type { ExactValues, Store } from "./store.ts";

type Subject = "source" | "determinized";
type Drawn = Extract<ChainData, { kind: "chain" }>;

const subjects: Subject[] = ["source", "determinized"];
const names: Record<Subject, string> = { source: "Source", determinized: "Determinized" };
const programs: Record<Subject, string> = {
  source: "the source",
  determinized: "the determinized program",
};

/** The space around a drawing, for its outlines and its labels' halos; the drawing's region lets
 * it into the gutter. */
const margin = 4;

export interface ChainElements {
  figure: HTMLElement;
  /** The button that shows and hides the figure. */
  toggle: HTMLButtonElement;
  /** The "i" button, and the popover that it shows. */
  info: HTMLButtonElement;
  about: HTMLElement;
  key: HTMLElement;
  columns: Record<Subject, { count: HTMLElement; body: HTMLElement; table: HTMLElement }>;
}

/** The determinized pane: the program it shows, its printed text, and where in the text each
 * node of the program is. */
export interface PrintedProgram {
  source: string;
  text: string;
  spans: PrintedSpan[];
}

/** What a column shows: a drawing, or a sentence. */
type Column =
  | { kind: "drawing"; states: number; chain: Drawn; text: ChainText; dots: string[] }
  | { kind: "sentence"; states: number | null; sentence: string };

function parse(text: string) {
  const [num, den = "1"] = text.split("/");
  return rational(BigInt(num), BigInt(den));
}

/** What a chain of a single node that doesn't branch says. */
function single(subject: Subject, chain: Drawn) {
  const [node] = chain.nodes;
  const lead = subject === "source" ? "It makes no random choice" : "No random choice remains";
  if (node.kind === "returned") {
    const value = fraction(add(parse(chain.entry), parse(node.value)));
    return `${lead}: it returns ${minus(value)} with probability 1.`;
  }
  if (node.kind === "rejected") return `${lead}: it is rejected with probability 1.`;
  return `${lead}: it diverges with probability 1.`;
}

/** The lines of the determinized pane where `sites` of the source are printed, if it shows the
 * program of `source` and prints every one of them. */
function printedLines(printed: PrintedProgram, source: string) {
  return (sites: Range[]) => {
    if (printed.source !== source) return null;
    const lines: number[] = [];
    for (const { from, to } of sites) {
      const range = printedRange(printed.spans, { from, to, headTo: to });
      if (!range) return null;
      lines.push(printed.text.slice(0, range.from).split("\n").length);
    }
    return lines;
  };
}

/** Whether a program has a chain: its model is complete. */
function hasChain(state: ExactState) {
  return state.kind === "finite" || state.kind === "too many";
}

/** What a program's column shows, from its exploration. The determinized program's lines are
 * those of its pane. */
function columnOf(
  subject: Subject,
  state: ExactState,
  exact: ExactValues,
  printed: PrintedProgram,
): Column {
  if (state.kind === "exploring" || state.kind === "solving") {
    return { kind: "sentence", states: null, sentence: "Exploring the model…" };
  }
  if (state.kind !== "finite" && state.kind !== "too many") {
    const reason = reasonText(state, exact.source) ?? "";
    return { kind: "sentence", states: null, sentence: reason };
  }
  const { chain, states } = state;
  if (chain.kind === "too large") {
    return {
      kind: "sentence",
      states,
      sentence: `Its states contract to ${thin(chain.nodes)} nodes and ${thin(chain.edges)} transitions, more than this figure draws (${chainCap.nodes} nodes and ${chainCap.edges} transitions).`,
    };
  }
  // A single branch, which draws forever, is drawn with its loop.
  if (chain.nodes.length === 1 && chain.nodes[0].kind !== "branch") {
    return { kind: "sentence", states, sentence: single(subject, chain) };
  }
  const inPane = printedLines(printed, exact.source);
  const lines = (sites: Range[]) =>
    (subject === "determinized" && inPane(sites)) || sourceLines(sites, exact.source);
  const text = chainText(chain, exact.source, exact.additive, lines);
  return { kind: "drawing", states, chain, text, dots: chainDots(text, chain.edges) };
}

const f1 = (value: number) => String(Math.round(value * 10) / 10);

/** A cubic Bézier path through `points`, shifted by the margin. */
function path(points: Point[]) {
  const at = ([x, y]: Point) => `${f1(x + margin)},${f1(y + margin)}`;
  let d = `M${at(points[0])}`;
  for (let i = 1; i + 2 < points.length; i += 3) {
    d += ` C${at(points[i])} ${at(points[i + 1])} ${at(points[i + 2])}`;
  }
  return d;
}

function edgeMarkup(edge: EdgeLayout, label: string | null, index: string) {
  const arrow = edge.arrow.map(([x, y]) => `${f1(x + margin)},${f1(y + margin)}`).join(" ");
  const drawn = `<path class="edge" d="${path(edge.curve)}"/>${arrow ? `<polygon class="arrow" points="${arrow}"/>` : ""}`;
  const text =
    label && edge.label
      ? `<text class="edge-label" data-edge="${index}" x="${f1(edge.label[0] + margin)}" y="${f1(edge.label[1] + margin)}">${escapeHtml(label)}</text>`
      : "";
  return { drawn, text };
}

/** The outline of a node of `kind` in the box at `left`, `top`. */
function shape(kind: NodeText["kind"], left: number, top: number, width: number, height: number) {
  const box = (inset: number, radius: number) =>
    `<rect class="shape" x="${f1(left + inset)}" y="${f1(top + inset)}" width="${f1(width - 2 * inset)}" height="${f1(height - 2 * inset)}" rx="${radius}"/>`;
  if (kind === "returned") return box(0, 3) + box(3, 1.5).replace('"shape"', '"shape inner"');
  if (kind === "diverges") return box(0, height / 2);
  if (kind === "rejected") {
    const k = 7;
    const [r, b] = [left + width, top + height];
    const points = [
      [left + k, top],
      [r - k, top],
      [r, top + k],
      [r, b - k],
      [r - k, b],
      [left + k, b],
      [left, b - k],
      [left, top + k],
    ];
    return `<polygon class="shape" points="${points.map(([x, y]) => `${f1(x)},${f1(y)}`).join(" ")}"/>`;
  }
  return box(0, 3);
}

function nodeMarkup(text: NodeText, index: number, layout: ChainLayout["nodes"][number]) {
  const left = layout.x - layout.width / 2 + margin;
  const top = layout.y - layout.height / 2 + margin;
  const { width, height } = layout;
  const ruleX = left + padding.x + text.number.length * numberAdvance + numberGap / 2;
  const textLeft = ruleX + numberGap / 2;
  const centre = (textLeft + left + width - padding.x) / 2;
  const line = (k: number) => top + padding.y + lineHeight * (k + 0.5);
  const mode = text.mode ? ` ${text.mode.toLowerCase()}` : "";
  // The number sits on the ground, apart from the node's content, also on the solid fill.
  const digits = text.number.length * numberAdvance;
  const chip = `<rect class="number-chip" x="${f1(left + padding.x - 3)}" y="${f1(line(0) - 8)}" width="${f1(digits + 6)}" height="16" rx="2"/>`;
  const lines = text.lines
    .map(
      (content, k) =>
        `<text class="${k === 0 ? `label${mode}` : "where"}" x="${f1(centre)}" y="${f1(line(k))}">${escapeHtml(content)}</text>`,
    )
    .join("");
  return `<g class="node ${text.kind}${mode}" data-node="${index}">${shape(text.kind, left, top, width, height)}${chip}<text class="number" x="${f1(left + padding.x)}" y="${f1(line(0))}">${text.number}</text><line class="number-rule" x1="${f1(ruleX)}" x2="${f1(ruleX)}" y1="${f1(top + 4)}" y2="${f1(top + height - 4)}"/>${lines}</g>`;
}

/** A node as the table and the description name it. */
function nodeName(text: NodeText, sums: boolean) {
  const [first, second] = text.lines;
  if (text.kind === "branch") return `${first}, ${second}`;
  if (text.kind === "returned") {
    return sums ? `adds ${first.replace(/^\+/, "")} and returns` : `returns ${first}`;
  }
  return first;
}

/** A node as the table names it, its draw in the code face. */
function nodeCell(text: NodeText, sums: boolean) {
  if (text.kind !== "branch") return escapeHtml(nodeName(text, sums));
  const [draw, where] = text.lines;
  // "line 2" and "or 3" don't break before their numbers.
  return `<code>${escapeHtml(draw)}</code>, ${escapeHtml(where.replace(/ (?=\d)/g, "\u00a0"))}`;
}

/** The SVG of a laid-out chain, with its title and description. */
function svgMarkup(
  subject: Subject,
  column: Extract<Column, { kind: "drawing" }>,
  layout: ChainLayout,
) {
  const { text, chain, states } = column;
  const width = Math.ceil(layout.width + 2 * margin);
  const height = Math.ceil(layout.height + 2 * margin);
  const entry = edgeMarkup(layout.entry, text.entry, "start");
  const edges = layout.edges.map((edge, i) => edgeMarkup(edge, text.edges[i], String(i)));
  const nodes = text.nodes.map((node, i) => nodeMarkup(node, i, layout.nodes[i]));
  const [sx, sy] = layout.start;
  const id = `chain-svg-${subject}`;
  const description = `${chain.nodes.length} nodes and ${chain.edges.length} transitions, contracted from ${thin(states)} machine states. Runs start at node 1: ${nodeName(text.nodes[0], text.sums)}. “Show the chains as tables” lists every transition.`;
  return `<svg class="chain-svg ${subject === "source" ? "source" : "det"}" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}" role="img" aria-labelledby="${id}-title ${id}-desc"><title id="${id}-title">Markov chain of ${programs[subject]}</title><desc id="${id}-desc">${escapeHtml(description)}</desc>${entry.drawn}${edges.map((edge) => edge.drawn).join("")}<circle class="start" cx="${f1(sx + margin)}" cy="${f1(sy + margin)}" r="3"/>${nodes.join("")}${entry.text}${edges.map((edge) => edge.text).join("")}</svg>`;
}

/** The transitions of a chain as a table: from the start and from each node. */
function tableMarkup(subject: Subject, column: Extract<Column, { kind: "drawing" }>) {
  const { text, chain } = column;
  const { sums } = text;
  const node = (i: number) =>
    `<span class="node-number">${i + 1}</span> ${nodeCell(text.nodes[i], sums)}`;
  const reward = (value: string) =>
    sums ? `<td class="num">${escapeHtml(addend(value))}</td>` : "";
  const rows = [
    `<tr data-to="0" tabindex="0"><th scope="row">start</th><td>${node(0)}</td><td class="num">1</td>${reward(chain.entry)}</tr>`,
    ...chain.edges.map(
      (edge, i) =>
        `<tr data-from="${edge.from}" data-to="${edge.to}" data-edge="${i}" tabindex="-1"><th scope="row">${node(edge.from)}</th><td>${node(edge.to)}</td><td class="num">${escapeHtml(edge.probability)}</td>${reward(edge.reward)}</tr>`,
    ),
  ];
  return `<table class="chain-table"><caption>${names[subject]}: transitions</caption><thead><tr><th scope="col">From</th><th scope="col">To</th><th scope="col" class="num">Probability</th>${sums ? '<th scope="col" class="num">Adds</th>' : ""}</tr></thead><tbody>${rows.join("")}</tbody></table>`;
}

/** The key's entries for the kinds of node that the drawings show, and how to read the rewards
 * where they add any. */
function keyMarkup(kinds: Set<NodeText["kind"]>, sums: boolean) {
  const glyph = (kind: NodeText["kind"]) =>
    `<svg class="key-glyph ${kind}" width="30" height="18" viewBox="0 0 30 18" aria-hidden="true">${shape(kind, 1, 1, 28, 16)}</svg>`;
  const entries: [NodeText["kind"], string][] = [
    ["branch", "a draw, with its mode and line"],
    ["returned", sums ? "a return, with what it adds" : "a return, with its value"],
    ["rejected", "rejected by an observation"],
    ["diverges", "diverges: a loop that never returns"],
  ];
  const shapes = entries
    .filter(([kind]) => kinds.has(kind))
    .map(([kind, words]) => `<li>${glyph(kind)}<span>${words}</span></li>`)
    .join("");
  const plus = sums
    ? '<li><code class="key-plus">+1</code><span>adds 1 to the output: a run returns the sum of the + labels on its path</span></li>'
    : "";
  return shapes + plus;
}

export function mountChainView(
  elements: ChainElements,
  store: Pick<Store, "chainsOpen" | "chainSites" | "followLinked" | "analysis" | "checkedSource">,
  shown: ReadonlySignal<ExactValues | null>,
  printed: () => PrintedProgram,
  layoutChain: LayoutClient,
) {
  const { figure, toggle, info, about, key, columns } = elements;
  /** Layouts by their dot sources, the latest few. */
  const layouts = new Map<string, Promise<ChainLayout>>();
  const kept = 8;
  /** The plan of the columns as last laid out, and whether they show anything. */
  let written = "";
  let painted = false;
  let generation = 0;
  /** The program of the drawings, and the columns as drawn, for the linked highlighting. */
  let drawnSource = "";
  let drawn: Partial<Record<Subject, Extract<Column, { kind: "drawing" }>>> = {};

  function layoutOf(dots: string[], chain: Drawn) {
    const key = dots.join("\n");
    let layout = layouts.get(key);
    if (!layout) {
      layout = layoutChain({ dots, nodes: chain.nodes.length, edges: chain.edges.length });
      layouts.set(key, layout);
      // A failed layout is tried again with the next program.
      layout.catch(() => layouts.delete(key));
      for (const old of [...layouts.keys()].slice(0, -kept)) layouts.delete(old);
    }
    return layout;
  }

  /** Writes the columns of `planned` with the layouts of their drawings: laid out, still to come,
   * or failed. */
  function write(
    exact: ExactValues,
    planned: Record<Subject, Column>,
    laid: (ChainLayout | "pending" | Error | null)[],
  ) {
    const kinds = new Set<NodeText["kind"]>();
    let sums = false;
    drawn = {};
    drawnSource = exact.source;
    for (const [i, subject] of subjects.entries()) {
      const column = planned[subject];
      const { count, body, table } = columns[subject];
      const layout = laid[i];
      const states = column.states === null ? "" : `${thin(column.states)} machine states`;
      if (
        column.kind === "sentence" ||
        !layout ||
        layout === "pending" ||
        layout instanceof Error
      ) {
        const sentence =
          column.kind === "sentence"
            ? column.sentence
            : layout === "pending"
              ? "Laying out the chain…"
              : "The layout of this chain failed.";
        // A failure keeps Graphviz's message for whoever looks.
        const title = layout instanceof Error ? ` title="${escapeHtml(layout.message)}"` : "";
        count.textContent = states;
        body.innerHTML = `<p class="chain-note"${title}>${escapeHtml(sentence)}</p>`;
        table.innerHTML = "";
        continue;
      }
      drawn[subject] = column;
      for (const node of column.text.nodes) kinds.add(node.kind);
      sums ||= column.text.sums;
      count.textContent = `${states}, drawn as ${column.chain.nodes.length}`;
      body.innerHTML = `<div class="chain-scroll">${svgMarkup(subject, column, layout)}</div>`;
      table.innerHTML = tableMarkup(subject, column);
    }
    about.innerHTML = aboutMarkup(exact.additive, sums, kinds.size > 0);
    key.innerHTML = keyMarkup(kinds, sums);
    painted = true;
    key.hidden = kinds.size === 0;
    figure.querySelector<HTMLElement>(".chain-tables")?.toggleAttribute("hidden", kinds.size === 0);
    fit();
    showStarts();
    highlight(null, null);
    if (reveal) {
      reveal = false;
      // Where the figure starts below the middle of the window, it scrolls into view at once.
      if (figure.getBoundingClientRect().top > window.innerHeight / 2) {
        figure.scrollIntoView({ block: "start", behavior: "instant" });
      }
    }
  }

  /** Stacks the columns where a drawing is wider than its column beside the other, and lets a
   * drawing's region take the focus, as a named region, where it still scrolls. */
  function fit() {
    const grid = figure.querySelector<HTMLElement>(".chain-columns");
    if (!grid) return;
    const gap = Number.parseFloat(getComputedStyle(grid).columnGap) || 0;
    const half = (figure.clientWidth - gap) / 2;
    const widths = [...figure.querySelectorAll<SVGSVGElement>(".chain-svg")].map(
      (svg) => Number(svg.getAttribute("width")) - 2 * margin,
    );
    figure.classList.toggle(
      "stacked",
      widths.some((width) => width > half),
    );
    for (const region of figure.querySelectorAll<HTMLElement>(".chain-scroll")) {
      const subject = subjectOf(region);
      if (subject && region.scrollWidth > region.clientWidth) {
        region.tabIndex = 0;
        region.setAttribute("role", "region");
        region.setAttribute("aria-label", `Chain of ${programs[subject]}`);
      } else {
        for (const name of ["tabindex", "role", "aria-label"]) region.removeAttribute(name);
      }
    }
  }

  /** Scrolls each drawing that is wider than its region to where runs start. */
  function showStarts() {
    for (const region of figure.querySelectorAll<HTMLElement>(".chain-scroll")) {
      const start = region.querySelector(".start");
      if (!start || region.scrollWidth <= region.clientWidth) continue;
      const x = Number(start.getAttribute("cx"));
      region.scrollLeft = x - region.clientWidth / 2;
    }
  }
  new ResizeObserver(fit).observe(figure);

  mountInfo(info, about);

  /** Marks the figure as busy, `aria-busy="true"`, or not. */
  function busy(on: boolean) {
    if (on) figure.setAttribute("aria-busy", "true");
    else figure.removeAttribute("aria-busy");
  }

  /** Forgets the drawings, so that the figure shows the current program's when it shows again. */
  function forget() {
    generation += 1;
    written = "";
    painted = false;
    drawn = {};
    busy(false);
    for (const subject of subjects) {
      const { count, body, table } = columns[subject];
      for (const part of [count, body, table]) part.replaceChildren();
    }
    store.chainSites.value = null;
  }

  /** Whether a click opened the figure, which then comes into view once it is drawn. */
  let reveal = false;
  toggle.addEventListener("click", () => {
    store.chainsOpen.value = !store.chainsOpen.peek();
    reveal = store.chainsOpen.peek();
  });

  effect(() => {
    const open = store.chainsOpen.value;
    const exact = shown.value;
    const result = store.analysis.value;
    const checked = store.checkedSource.value;
    // There is something to show where one of the programs that the exact rows show has a chain;
    // an open figure keeps the last program's while a new one is explored.
    const runnable = result.ok && result.type.startsWith("float");
    const states = exact ? [exact.programs.source, exact.programs.determinized] : [];
    const exploring = states.some(
      (state) => state.kind === "exploring" || state.kind === "solving",
    );
    const drawing = states.some(hasChain) || (painted && (exploring || exact?.source !== checked));
    const available = !!exact && runnable && drawing;
    toggle.classList.toggle("absent", !available);
    toggle.setAttribute("aria-expanded", String(open && available));
    toggle.textContent = open ? "Hide the Markov chains" : "Show the Markov chains";
    figure.hidden = !open || !available;
    if (figure.hidden || !exact) {
      forget();
      return;
    }
    // Until the new program's first runs come in, the exact rows and the figure show the last.
    const stale = exact.source !== checked;
    const pane = printed();
    const planned = {
      source: columnOf("source", exact.programs.source, exact, pane),
      determinized: columnOf("determinized", exact.programs.determinized, exact, pane),
    };
    const plan = JSON.stringify([exact.source, exact.additive, planned]);
    if (stale && painted) {
      busy(true);
      return;
    }
    if (plan === written) return;
    generation += 1;
    const mine = generation;
    // While a new program is explored, the figure keeps what it shows.
    if (!exact.done && painted) {
      written = "";
      busy(true);
      return;
    }
    written = plan;
    const pending = subjects.map((subject) => {
      const column = planned[subject];
      return column.kind === "drawing" ? layoutOf(column.dots, column.chain) : null;
    });
    const waiting = pending.some((layout) => layout !== null);
    busy(stale || !exact.done || waiting);
    // A first opening shows at once what needs no layout, and says where a drawing is to come;
    // later, the previous drawing stays until the new one is laid out.
    if (!painted && waiting)
      write(
        exact,
        planned,
        pending.map((layout) => layout && "pending"),
      );
    void Promise.all(
      pending.map((layout) => layout?.catch((error: unknown) => new Error(String(error))) ?? null),
    ).then((laid) => {
      if (mine !== generation) return;
      write(exact, planned, laid);
      // Only the checked program's finished values end the wait.
      busy(!exact.done || exact.source !== store.checkedSource.peek());
    });
  });

  /** Highlights the nodes `nodes` of `subject`'s drawing, and the sites of their draws in the
   * panes. */
  function highlight(subject: Subject | null, nodes: number[] | null) {
    for (const node of figure.querySelectorAll(".node.linked")) node.classList.remove("linked");
    const column = subject && drawn[subject];
    if (!subject || !column || !nodes) {
      store.chainSites.value = null;
      return;
    }
    const svg = figure.querySelector(`#chain-svg-${subject}-title`)?.parentElement;
    const sites: Range[] = [];
    for (const index of nodes) {
      svg?.querySelector(`.node[data-node="${index}"]`)?.classList.add("linked");
      const node = column.chain.nodes[index];
      if (node?.kind === "branch") sites.push(...node.sites);
    }
    store.chainSites.value = { source: drawnSource, sites };
    // The panes scroll to the sites, where there are any.
    if (sites.length > 0) store.followLinked.value += 1;
  }

  function subjectOf(element: Element): Subject | null {
    const holder = element.closest("[data-subject]");
    const subject = holder?.getAttribute("data-subject");
    return subject === "source" || subject === "determinized" ? subject : null;
  }

  /** The nodes of a table's row: those of its transition, or the first for the start. */
  function rowNodes(row: HTMLElement) {
    const nodes = [row.dataset.from, row.dataset.to].filter((value) => value !== undefined);
    return [...new Set(nodes.map(Number))];
  }

  /** Highlights what `item` stands for: a node of a drawing, or a table's row. */
  function show(item: Element | null) {
    if (!item) highlight(null, null);
    else if (item instanceof HTMLElement) highlight(subjectOf(item), rowNodes(item));
    else highlight(subjectOf(item), [Number(item.getAttribute("data-node"))]);
  }

  /** The item under the pointer, or else the row with the focus, highlighted. */
  let hovered: Element | null = null;
  function showFocused() {
    show(hovered ?? figure.querySelector(".chain-table tbody tr:focus"));
  }

  figure.addEventListener("pointerover", (event) => {
    const target = event.target instanceof Element ? event.target : null;
    const item = target?.closest(".node[data-node], .chain-table tbody tr") ?? null;
    if (item === hovered) return;
    hovered = item;
    showFocused();
  });
  figure.addEventListener("pointerleave", () => {
    hovered = null;
    showFocused();
  });

  // A table's rows are one tab stop; the arrow keys move between them.
  figure.addEventListener("keydown", (event) => {
    const row = event.target instanceof HTMLElement ? event.target.closest("tbody tr") : null;
    if (!(row instanceof HTMLElement) || event.altKey || event.ctrlKey || event.metaKey) return;
    const rows = [...(row.parentElement?.children ?? [])] as HTMLElement[];
    const at = rows.indexOf(row);
    const moves: Record<string, number> = {
      ArrowDown: at + 1,
      ArrowUp: at - 1,
      Home: 0,
      End: rows.length - 1,
    };
    if (!(event.key in moves)) return;
    event.preventDefault();
    const next = rows[Math.max(0, Math.min(rows.length - 1, moves[event.key]))];
    for (const other of rows) other.tabIndex = other === next ? 0 : -1;
    next.focus();
  });
  figure.addEventListener("focusin", (event) => {
    const row = event.target instanceof HTMLElement ? event.target.closest("tbody tr") : null;
    if (row) show(row);
  });
  figure.addEventListener("focusout", (event) => {
    const next = event.relatedTarget instanceof Element ? event.relatedTarget : null;
    if (!next?.closest(".chain-table tbody tr")) show(hovered);
  });
}

/** The "i" button's popover: shown while the pointer is over the button or the popover, and
 * kept open by a click, until another click, Escape or a click elsewhere closes it. It sits below
 * the button, within the page's width. */
function mountInfo(info: HTMLButtonElement, about: HTMLElement) {
  let pinned = false;
  let leaving: ReturnType<typeof setTimeout> | undefined;
  const open = () => about.matches(":popover-open");
  function place() {
    const box = info.getBoundingClientRect();
    const width = Math.min(about.offsetWidth, document.documentElement.clientWidth - 32);
    const left = Math.max(
      16,
      Math.min(box.left, document.documentElement.clientWidth - 16 - width),
    );
    about.style.left = `${left + window.scrollX}px`;
    about.style.top = `${box.bottom + 6 + window.scrollY}px`;
  }
  function show() {
    clearTimeout(leaving);
    if (!open()) about.showPopover();
    place();
  }
  function leave() {
    clearTimeout(leaving);
    if (!pinned) leaving = setTimeout(() => open() && !pinned && about.hidePopover(), 300);
  }
  info.addEventListener("click", () => {
    if (open() && pinned) {
      about.hidePopover();
      return;
    }
    pinned = true;
    show();
  });
  for (const element of [info, about]) {
    element.addEventListener("pointerenter", (event) => {
      if (event.pointerType === "mouse") show();
    });
    element.addEventListener("pointerleave", leave);
  }
  about.addEventListener("toggle", () => {
    info.setAttribute("aria-expanded", String(open()));
    if (!open()) pinned = false;
  });
  document.addEventListener("keydown", (event) => {
    if (event.key !== "Escape" || !open()) return;
    // The focus doesn't stay behind in a closed popover.
    if (about.contains(document.activeElement)) info.focus();
    about.hidePopover();
  });
  document.addEventListener("pointerdown", (event) => {
    const target = event.target instanceof Node ? event.target : null;
    if (open() && !info.contains(target) && !about.contains(target)) about.hidePopover();
  });
  window.addEventListener("resize", () => open() && place());
}

/** What the drawings are: Lean's models with their probability-1 steps contracted, how their nodes
 * are numbered, and where they add rewards on the way, how to read those. */
function aboutMarkup(additive: boolean, sums: boolean, drawings: boolean) {
  const model = additive
    ? '<a href="https://julesjacobs.com/determinize/docs/Determinize/Spec/RewardModel/Model.html#Determinize.Spec.RewardModel.Model" title="RewardModel, in the Lean development"><code>RewardModel</code></a>'
    : '<a href="https://julesjacobs.com/determinize/docs/Determinize/Spec/FiniteModel/Model.html#Determinize.Spec.FiniteModel.Model" title="Model, in the Lean development"><code>Model</code></a>';
  const sum = sums
    ? " Each + label adds to the output: a run returns the sum of the + labels on its path."
    : "";
  const numbers = drawings ? " The tables of transitions number the nodes as the drawings do." : "";
  return `Each program's finite model (Lean's ${model}), with each step of probability 1 contracted, so that a node is where a run draws, returns, is rejected or diverges. A source branches at its E and its G draws; its determinized program only at the G draws, as the E draws are replaced by their means.${numbers}${sum}`;
}
