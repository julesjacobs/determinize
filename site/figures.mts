// Draws the landing page's two figures of the signal example as inline SVG and writes them into
// site/index.html between their <!-- figure:NAME --> markers:
//
//   node site/figures.mts
//
// The runs come from the simulator's port of Lean's evaluator: run i of both programs at seed
// 1 + i, as `./run.sh --seed 1 --samples 10000` runs them, so the histograms are of that command's
// runs. Both programs share the G draw x of each run.
import { readFileSync, writeFileSync } from "node:fs";
import { analyze } from "../sim/src/core/compiler/analyze.ts";
import type { Node } from "../sim/src/core/runtime/eval.ts";
import { prepare, run, siteNodes } from "../sim/src/core/runtime/eval.ts";

const page = new URL("index.html", import.meta.url);
const program = readFileSync(
  new URL("../examples/paper/noisy-product.det", import.meta.url),
  "utf8",
);
const histogramRuns = 10000;
const plotRuns = 320;
const firstSeed = 1;

const analysis = analyze(program);
if (!analysis.ok) throw new Error("Lean's front end rejects the signal example");
const source = prepare(analysis.program.source);
const determinized = prepare(analysis.program.determinized);
const gSite = siteNodes(source).find((site) => site.action === "G");
if (!gSite) throw new Error("the program has no G draw");

/** The output of run `seed` and the value of its draw at the first G site. */
function runAt(node: Node, seed: number) {
  let x: number | undefined;
  const outcome = run(node, BigInt(seed), {
    onGDraw: (draw) => {
      if (draw.site === gSite?.index && x === undefined) x = draw.value;
    },
  });
  if (outcome.kind !== "returned" || outcome.value.tag !== "number" || x === undefined) {
    throw new Error(`seed ${seed}: the run returned no number or drew no G value`);
  }
  return { value: outcome.value.value, x };
}

const runs = Array.from({ length: histogramRuns }, (_, i) => {
  const ran = runAt(source, firstSeed + i);
  const det = runAt(determinized, firstSeed + i);
  if (ran.x !== det.x) throw new Error(`seed ${firstSeed + i}: the G draws differ`);
  return { x: det.x, source: ran.value, det: det.value };
});

interface Histogram {
  lo: number;
  hi: number;
  counts: number[];
  /** Values outside [lo, hi). */
  outside: number;
}

function histogram(values: number[], lo: number, hi: number, bins: number): Histogram {
  const counts = new Array<number>(bins).fill(0);
  let outside = 0;
  for (const v of values) {
    const i = Math.floor(((v - lo) / (hi - lo)) * bins);
    if (i >= 0 && i < bins) counts[i]++;
    else outside++;
  }
  return { lo, hi, counts, outside };
}

const f2 = (v: number) => v.toFixed(2);
const minus = (v: number | string) => String(v).replace("-", "−");
const thin = (n: number) => n.toLocaleString("en-US").replaceAll(",", " ");

/**
 * The output distributions: two histograms on one x axis and one y scale, the source above
 * (outlined) and the determinized program below (filled), with a dashed line at their common
 * mean.
 */
function stacked(o: {
  source: Histogram;
  det: Histogram;
  notes: [string, string];
  mean: { value: number; label: string };
  width: number;
  rowHeight: number;
  id: string;
  title: string;
}) {
  const { width: W, rowHeight: rowH } = o;
  const gap = 40;
  const padX = 8;
  const padT = 34;
  const axisH = 28;
  const H = padT + rowH * 2 + gap + axisH;
  const { lo, hi } = o.source;
  const pw = W - 2 * padX;
  const bw = pw / o.source.counts.length;
  const x = (v: number) => padX + ((v - lo) / (hi - lo)) * pw;
  const yMax = Math.ceil(Math.max(...o.source.counts, ...o.det.counts) * 1.04);
  const parts = [
    `<svg class="chart stacked" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-labelledby="${o.id}-t">`,
    `<title id="${o.id}-t">${o.title}</title>`,
  ];
  const rows = [
    { hist: o.source, kind: "source", top: padT, label: "Source", note: o.notes[0] },
    { hist: o.det, kind: "det", top: padT + rowH + gap, label: "Determinized", note: o.notes[1] },
  ];
  for (const r of rows) {
    const base = r.top + rowH;
    const y = (c: number) => base - (c / yMax) * rowH;
    parts.push(
      `<text class="row-label row-${r.kind}" x="${padX}" y="${r.top - 8}">${r.label}<tspan class="row-note" dx="8">${r.note}</tspan></text>`,
    );
    if (r.kind === "source") {
      let d = `M${f2(padX)} ${base}`;
      r.hist.counts.forEach((c, i) => {
        d += ` L${f2(padX + i * bw)} ${f2(y(c))} L${f2(padX + (i + 1) * bw)} ${f2(y(c))}`;
      });
      parts.push(`<path class="outline" d="${d} L${f2(padX + pw)} ${base}"/>`);
    } else {
      r.hist.counts.forEach((c, i) => {
        if (c === 0) return;
        parts.push(
          `<rect class="filled" x="${f2(padX + i * bw + 0.5)}" y="${f2(y(c))}" width="${f2(Math.max(bw - 1, 0.8))}" height="${f2(base - y(c))}"/>`,
        );
      });
    }
    parts.push(`<line class="axis" x1="${padX}" x2="${W - padX}" y1="${base}" y2="${base}"/>`);
  }
  const mx = x(o.mean.value);
  parts.push(
    `<line class="mean-line" x1="${f2(mx)}" x2="${f2(mx)}" y1="${padT - 2}" y2="${padT + rowH * 2 + gap}"/>`,
    `<text class="mean-label" x="${f2(mx + 5)}" y="${padT + 10}">${o.mean.label}</text>`,
  );
  const axisY = padT + rowH * 2 + gap;
  for (let t = Math.ceil(lo); t <= hi; t++) {
    parts.push(
      `<line class="tick" x1="${f2(x(t))}" x2="${f2(x(t))}" y1="${axisY}" y2="${axisY + 4}"/>`,
      `<text class="tick-label" x="${f2(x(t))}" y="${axisY + 18}" text-anchor="middle">${minus(t)}</text>`,
    );
  }
  parts.push("</svg>");
  return parts.join("");
}

interface Band {
  x0: number;
  x1: number;
  label: string;
  /** The curve's point in the band, ringed. */
  point: [number, number];
}

/**
 * Each run's output against its G draw: the source's runs as open circles, the determinized
 * program's as filled points, with the curve they lie on, a shaded band of the G draw, and a
 * dashed line at the common mean.
 */
function conditional(o: {
  pairs: { x: number; source: number; det: number }[];
  yRange: [number, number];
  curve: [number, number][];
  curveLabel: string;
  band: Band;
  mean: { value: number; label: string };
  width: number;
  height: number;
  id: string;
  title: string;
}) {
  const { width: W, height: H } = o;
  const padL = 34;
  const padR = 12;
  const padT = 24;
  const padB = 40;
  const pw = W - padL - padR;
  const ph = H - padT - padB;
  const [ylo, yhi] = o.yRange;
  const X = (v: number) => padL + v * pw;
  const Y = (v: number) => padT + ph - ((v - ylo) / (yhi - ylo)) * ph;
  const inBand = (x: number) => x >= o.band.x0 && x <= o.band.x1;
  const shown = (v: number) => v >= ylo && v <= yhi;
  const parts = [
    `<svg class="chart conditional" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-labelledby="${o.id}-t">`,
    `<title id="${o.id}-t">${o.title}</title>`,
  ];
  const bx0 = X(o.band.x0);
  const bx1 = X(o.band.x1);
  parts.push(
    `<rect class="band" x="${f2(bx0)}" y="${padT}" width="${f2(bx1 - bx0)}" height="${ph}"/>`,
    `<text class="band-label halo" x="${f2((bx0 + bx1) / 2)}" y="${padT - 8}" text-anchor="middle">${o.band.label}</text>`,
  );
  for (let t = Math.ceil(ylo); t <= yhi; t++) {
    parts.push(
      `<line class="${t === 0 ? "zero" : "grid"}" x1="${padL}" x2="${padL + pw}" y1="${f2(Y(t))}" y2="${f2(Y(t))}"/>`,
      `<text class="tick-label" x="${padL - 6}" y="${f2(Y(t) + 4)}" text-anchor="end">${minus(t)}</text>`,
    );
  }
  parts.push(
    `<line class="axis-given" x1="${padL}" x2="${padL + pw}" y1="${padT + ph}" y2="${padT + ph}"/>`,
  );
  for (const t of [0, 0.5, 1]) {
    parts.push(
      `<line class="tick-given" x1="${f2(X(t))}" x2="${f2(X(t))}" y1="${padT + ph}" y2="${padT + ph + 4}"/>`,
      `<text class="tick-label given" x="${f2(X(t))}" y="${padT + ph + 17}" text-anchor="middle">${t}</text>`,
    );
  }
  parts.push(
    `<text class="axis-title given" x="${padL + pw}" y="${H - 4}" text-anchor="end">x, the G draw</text>`,
    `<text class="axis-title" x="${padL - 30}" y="${padT - 10}">output</text>`,
    `<line class="mean-line" x1="${padL}" x2="${padL + pw}" y1="${f2(Y(o.mean.value))}" y2="${f2(Y(o.mean.value))}"/>`,
  );
  for (const p of o.pairs.filter((p) => shown(p.source))) {
    parts.push(
      `<circle class="pt-source${inBand(p.x) ? " in-band" : ""}" cx="${f2(X(p.x))}" cy="${f2(Y(p.source))}" r="2.6"/>`,
    );
  }
  const d = o.curve.map(([cx, cy], i) => `${i ? "L" : "M"}${f2(X(cx))} ${f2(Y(cy))}`).join(" ");
  parts.push(`<path class="curve" d="${d}"/>`);
  for (const p of o.pairs.filter((p) => shown(p.det))) {
    parts.push(`<circle class="pt-det" cx="${f2(X(p.x))}" cy="${f2(Y(p.det))}" r="2.4"/>`);
  }
  const [px, py] = o.band.point;
  const [lx, ly] = o.curve[o.curve.length - 1];
  parts.push(
    `<circle class="band-point" cx="${f2(X(px))}" cy="${f2(Y(py))}" r="6"/>`,
    `<text class="curve-label halo" x="${f2(X(lx) - 6)}" y="${f2(Y(ly) - 12)}" text-anchor="end">${o.curveLabel}</text>`,
    `<text class="mean-label halo" x="${padL + 4}" y="${f2(Y(o.mean.value) - 6)}">${o.mean.label}</text>`,
    "</svg>",
  );
  return parts.join("");
}

const sourceHist = histogram(
  runs.map((r) => r.source),
  -2,
  3,
  50,
);
const detHist = histogram(
  runs.map((r) => r.det),
  -2,
  3,
  50,
);
const outputs = {
  source: sourceHist,
  det: detHist,
  notes: ["variance 19/45", "variance 4/45"] as [string, string],
  mean: { value: 1 / 3, label: "expected output 1/3" },
};
const outside = sourceHist.outside + detHist.outside;
const outputsTitle = `Histograms of ${thin(histogramRuns)} runs of each program on one axis: the source spreads from about −2 to 3, the determinized program stays between 0 and 1; both have expected output 1/3.${outside ? ` ${outside} runs of the source lie outside the axis.` : ""}`;

const pairs = runs.slice(0, plotRuns);
const band: Band = { x0: 0.65, x1: 0.75, label: "x near 0.7", point: [0.7, 0.49] };
const why = {
  pairs,
  yRange: [-2, 3] as [number, number],
  curve: Array.from({ length: 41 }, (_, i): [number, number] => [i / 40, (i / 40) ** 2]),
  curveLabel: "determinized: x²",
  band,
  mean: { value: 1 / 3, label: "1/3, the expected output of both" },
};
const hidden = pairs.filter((p) => p.source < why.yRange[0] || p.source > why.yRange[1]).length;
const whyTitle = `Output of ${plotRuns} runs of each program against x. The source's runs scatter between −2 and 3; the determinized program's runs lie on the curve x squared. A shaded band marks the runs with x near 0.7.${hidden ? ` ${hidden} runs of the source lie outside the plot.` : ""}`;

const inBand = pairs.filter((p) => p.x >= band.x0 && p.x <= band.x1).map((p) => p.source);
const tenth = (v: number) => minus((Math.round(v * 10) / 10).toFixed(1));

const figures: Record<string, string> = {
  outputs:
    stacked({ ...outputs, width: 640, rowHeight: 104, id: "ow", title: outputsTitle }).replace(
      'class="chart stacked"',
      'class="chart stacked wide"',
    ) +
    stacked({
      ...outputs,
      width: 358,
      rowHeight: 84,
      id: "on",
      title: `Histograms of ${thin(histogramRuns)} runs of each program on one axis; both have expected output 1/3.`,
    }).replace('class="chart stacked"', 'class="chart stacked narrow"'),
  why:
    conditional({ ...why, width: 640, height: 380, id: "ww", title: whyTitle }).replace(
      'class="chart conditional"',
      'class="chart conditional wide"',
    ) +
    conditional({
      ...why,
      width: 358,
      height: 320,
      id: "wn",
      title: `Output of ${plotRuns} runs of each program against x.`,
    }).replace('class="chart conditional"', 'class="chart conditional narrow"'),
  slice: `${tenth(Math.min(...inBand))} to ${tenth(Math.max(...inBand))}`,
};

let html = readFileSync(page, "utf8");
for (const [name, content] of Object.entries(figures)) {
  const marked = new RegExp(`(<!-- figure:${name} -->)[^]*?(<!-- /figure:${name} -->)`);
  if (!marked.test(html)) throw new Error(`index.html has no markers for the figure ${name}`);
  const inline = name === "slice";
  html = html.replace(marked, (_, open, close) =>
    inline ? open + content + close : `${open}\n${content}\n${close}`,
  );
}
writeFileSync(page, html);
console.log(
  `Drew ${histogramRuns} runs and ${plotRuns} pairs (${inBand.length} in the band) into site/index.html.`,
);
