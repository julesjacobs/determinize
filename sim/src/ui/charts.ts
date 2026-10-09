// The distributions band's charts, hand-rolled: each program's output distribution as a histogram
// on the axis and scale that both share, and each run's output against the value of one G draw.
// The histograms and the plot's axes are SVG markup; the plot's runs are drawn on a canvas,
// because thousands of SVG circles are slow. Colours are the tokens' roles: the source outlined in
// --muted, the determinized program filled with --change, the G draw's axis in --given.

/** A number on an axis or in a label, with a true minus sign. */
export function minus(text: string | number) {
  return String(text).replace(/^-/, "−");
}

/** A count with its thousands separated by a narrow space, as `10 000`. */
export function thin(count: number) {
  return count.toLocaleString("en-US").replaceAll(",", " ");
}

const f2 = (value: number) => value.toFixed(2);

/** Whether `lo`–`hi` is too narrow for round ticks: within a billionth of its size, so that
 * adding a step may not change a value. */
export function tooNarrow(lo: number, hi: number) {
  return !(hi - lo > 1e-9 * Math.max(1, Math.abs(lo), Math.abs(hi)));
}

/** `lo`–`hi`, widened around its middle when it is too narrow for round ticks. */
export function widened(lo: number, hi: number): [number, number] {
  if (!tooNarrow(lo, hi)) return [lo, hi];
  const pad = Math.max(1, Math.abs(lo) * 0.5);
  return [lo - pad, hi + pad];
}

/** Ticks at a round step, from `lo` to `hi`, about `count` of them. */
export function ticks(lo: number, hi: number, count: number) {
  const raw = (hi - lo) / Math.max(1, count);
  const power = 10 ** Math.floor(Math.log10(raw));
  const step = [1, 2, 5, 10].map((m) => m * power).find((candidate) => candidate >= raw) ?? raw;
  const first = Math.ceil(lo / step - 1e-9);
  const values: number[] = [];
  // By index, so that a step too small to change a value can't loop forever.
  for (let i = 0; (first + i) * step <= hi + step * 1e-9 && i <= 4 * count + 4; i++) {
    values.push(Number(((first + i) * step).toPrecision(12)));
  }
  return { step, values };
}

/** The round number (1, 2 or 5 times a power of ten) nearest to `value`, by ratio. */
function roundNear(value: number) {
  const power = 10 ** Math.floor(Math.log10(value));
  const candidates = [1, 2, 5, 10].map((m) => m * power);
  return candidates.reduce((best, candidate) =>
    Math.abs(Math.log(candidate / value)) < Math.abs(Math.log(best / value)) ? candidate : best,
  );
}

/** An axis around `lo`–`hi` with round ends and about `count` steps between ticks. */
export function niceAxis(lo: number, hi: number, count: number) {
  const { step } = ticks(lo, hi, count);
  const axisLo = Math.floor(lo / step + 1e-9) * step;
  const axisHi = Math.ceil(hi / step - 1e-9) * step;
  const values: number[] = [];
  for (let i = 0; axisLo + i * step <= axisHi + step * 1e-9; i++) {
    values.push(Number((axisLo + i * step).toPrecision(12)));
  }
  return { lo: axisLo, hi: axisHi, step, ticks: values };
}

/** Counts of values in equal bins from `lo` to `hi`, and how many fell outside. */
export interface Histogram {
  lo: number;
  hi: number;
  counts: number[];
  below: number;
  above: number;
}

export function histogram(values: Iterable<number>, lo: number, hi: number, bins: number) {
  const result: Histogram = { lo, hi, counts: new Array<number>(bins).fill(0), below: 0, above: 0 };
  const width = (hi - lo) / bins;
  for (const value of values) {
    if (Number.isNaN(value)) continue;
    if (value < lo) result.below += 1;
    else if (value > hi) result.above += 1;
    else result.counts[Math.min(bins - 1, Math.floor((value - lo) / width))] += 1;
  }
  return result;
}

/**
 * The shared axis of both programs' outputs: round ends around the middle 99.8 % of their returned
 * numbers, and bins of a round width, about 50 of them.
 */
export function outputAxis(source: readonly number[], determinized: readonly number[]) {
  const numbers = Float64Array.from([...source, ...determinized].filter(Number.isFinite)).sort();
  if (numbers.length === 0) return null;
  const at = (q: number) => numbers[Math.min(numbers.length - 1, Math.floor(q * numbers.length))];
  const [lo, hi] = widened(at(0.001), at(0.999));
  // A span that a float can't hold has no axis of equal bins.
  if (!Number.isFinite(hi - lo)) return null;
  const axis = niceAxis(lo, hi, 6);
  if (!Number.isFinite(axis.hi - axis.lo)) return null;
  const binWidth = roundNear((axis.hi - axis.lo) / 50);
  const bins = Math.max(1, Math.round((axis.hi - axis.lo) / binWidth));
  return { lo: axis.lo, hi: axis.lo + bins * binWidth, bins, binWidth, ticks: axis.ticks };
}

/**
 * The height of the histograms' shared scale: their highest bar, unless that is more than 2.6
 * times the other program's highest, which only applies when both programs have bars. A bar above
 * the scale is cut.
 */
export function cutHeight(source: readonly number[], determinized: readonly number[]) {
  const peaks = [Math.max(0, ...source), Math.max(0, ...determinized)];
  const highest = Math.max(1, ...peaks);
  const lowest = Math.min(...peaks);
  return Math.ceil((lowest > 0 ? Math.min(highest, 2.6 * lowest) : highest) * 1.04);
}

export interface HistogramOptions {
  hist: Histogram;
  /** The source, drawn as a step outline, or the determinized program, as filled bars. */
  kind: "source" | "det";
  label: string;
  note: string;
  /** The program's mean, where it has one, and its label. */
  mean: number | null;
  meanLabel: string;
  ticks: number[];
  /** The height of the scale that both programs' histograms share; a bar above it is cut. */
  yMax: number;
  width: number;
  /** Taller, for histograms side by side. */
  tall?: boolean;
  id: string;
  title: string;
}

/** About how wide a chart's label of 12.5 px is: its characters at 0.56 em, 0.6 em in bold. */
function labelWidth(text: string, bold = false) {
  return text.length * 12.5 * (bold ? 0.6 : 0.56);
}

/**
 * One program's output distribution: a histogram on the axis and the scale that both programs'
 * histograms share, so that they compare at a glance side by side, with a dashed line at its
 * mean. A bar above the scale is cut and labelled with its count.
 */
export function histogramChart(o: HistogramOptions) {
  const W = o.width;
  const rowH = o.tall ? Math.round(Math.min(200, Math.max(110, W * 0.45))) : W < 360 ? 84 : 110;
  const padX = 8;
  const padT = 34;
  const axisH = 28;
  const H = padT + rowH + axisH;
  const { lo, hi } = o.hist;
  const pw = W - 2 * padX;
  const bw = pw / o.hist.counts.length;
  const x = (value: number) => padX + ((value - lo) / (hi - lo)) * pw;
  const base = padT + rowH;
  const y = (count: number) => base - (Math.min(count, o.yMax) / o.yMax) * rowH;
  const parts = [
    `<svg class="chart histogram" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-labelledby="${o.id}-t">`,
    `<title id="${o.id}-t">${o.title}</title>`,
    `<text class="row-label row-${o.kind}" x="${padX}" y="${padT - 14}">${o.label}<tspan class="row-note" dx="8">${o.note}</tspan></text>`,
  ];
  if (o.kind === "source") {
    let d = `M${f2(padX)} ${base}`;
    o.hist.counts.forEach((count, i) => {
      d += ` L${f2(padX + i * bw)} ${f2(y(count))} L${f2(padX + (i + 1) * bw)} ${f2(y(count))}`;
    });
    parts.push(`<path class="outline" d="${d} L${f2(padX + pw)} ${base}"/>`);
  } else {
    o.hist.counts.forEach((count, i) => {
      if (count === 0) return;
      parts.push(
        `<rect class="filled" x="${f2(padX + i * bw + 0.5)}" y="${f2(y(count))}" width="${f2(Math.max(bw - 1, 0.8))}" height="${f2(base - y(count))}"/>`,
      );
    });
  }
  // The mean's label sits in the top row, beside its line on a side where no bar reaches that
  // high; each cut bar's count sits centred on its bar, below its break, in the first row where it
  // overlaps no other count, and a row lower where it crosses the mean's line.
  const labels: string[] = [];
  const tall = o.hist.counts
    .map((count, i) => ({ from: padX + i * bw, to: padX + (i + 1) * bw, top: y(count) }))
    .filter((bar) => bar.top < padT + 12);
  parts.push(`<line class="axis" x1="${padX}" x2="${W - padX}" y1="${base}" y2="${base}"/>`);
  if (o.mean !== null && Number.isFinite(o.mean) && o.mean >= lo && o.mean <= hi) {
    const mx = x(o.mean);
    const text = `mean ${minus(o.meanLabel)}`;
    const width = labelWidth(text, true);
    const sides = [mx + 5, mx - 5 - width].filter((from) => from >= 0 && from + width <= W);
    const clear = (from: number) =>
      tall.every((bar) => bar.to + 2 < from || from + width + 2 < bar.from);
    const from = sides.find(clear) ?? sides[0] ?? Math.max(0, Math.min(mx + 5, W - width));
    parts.push(
      `<line class="mean-line mean-${o.kind}" x1="${f2(mx)}" x2="${f2(mx)}" y1="${padT}" y2="${base}"/>`,
    );
    labels.push(`<text class="mean-label halo" x="${f2(from)}" y="${padT + 10}">${text}</text>`);
  }
  const counts: { from: number; to: number; row: number }[] = [];
  const meanX =
    o.mean !== null && Number.isFinite(o.mean) && o.mean >= lo && o.mean <= hi ? x(o.mean) : null;
  const tallest = Math.max(...o.hist.counts);
  o.hist.counts.forEach((count, i) => {
    if (count <= o.yMax) return;
    const x0 = padX + i * bw;
    // The tallest count says what it counts; the caption says it for the others.
    const text = count === tallest ? `${thin(count)} runs` : thin(count);
    const width = labelWidth(text);
    const from = Math.max(0, Math.min(x0 + bw / 2 - width / 2, W - width));
    // A count across the mean's line starts a row lower, clear of the mean's label.
    const across = meanX !== null && from - 3 < meanX && meanX < from + width + 3;
    let row = across ? 1 : 0;
    while (
      counts.some(
        (label) => label.row === row && from < label.to + 4 && label.from < from + width + 4,
      )
    ) {
      row++;
    }
    counts.push({ from, to: from + width, row });
    parts.push(
      `<path class="break" d="M${f2(x0 - 1)} ${padT + 7} L${f2(x0 + bw + 1)} ${padT + 3} M${f2(x0 - 1)} ${padT + 11} L${f2(x0 + bw + 1)} ${padT + 7}"/>`,
    );
    labels.push(
      `<text class="clip-label halo" x="${f2(from)}" y="${padT + 27 + 17 * row}">${text}</text>`,
    );
  });
  // Over every mark, so that a later bar's break doesn't cross an earlier label.
  parts.push(...labels);
  for (const tick of o.ticks) {
    const tx = x(tick);
    const anchor = tx < padX + 10 ? "start" : tx > W - padX - 10 ? "end" : "middle";
    parts.push(
      `<line class="tick" x1="${f2(tx)}" x2="${f2(tx)}" y1="${base}" y2="${base + 4}"/>`,
      `<text class="tick-label" x="${f2(tx)}" y="${base + 18}" text-anchor="${anchor}">${minus(tick)}</text>`,
    );
  }
  parts.push("</svg>");
  return parts.join("");
}

/** The frame of the conditional-mean plot: its size, ranges and the mapping into it. */
export interface PlotFrame {
  width: number;
  height: number;
  padL: number;
  padR: number;
  padT: number;
  padB: number;
  xRange: [number, number];
  yRange: [number, number];
}

export function plotX(frame: PlotFrame, value: number) {
  const [lo, hi] = frame.xRange;
  return frame.padL + ((value - lo) / (hi - lo)) * (frame.width - frame.padL - frame.padR);
}

export function plotY(frame: PlotFrame, value: number) {
  const [lo, hi] = frame.yRange;
  const ph = frame.height - frame.padT - frame.padB;
  return frame.padT + ph - ((value - lo) / (hi - lo)) * ph;
}

/** The axes and labels of the conditional-mean plot, as SVG drawn over its canvas. */
export function plotAxes(
  frame: PlotFrame,
  o: {
    xTicks: number[];
    yTicks: number[];
    xLabel: string;
    yLabel: string;
    /** A dashed line at the mean that both programs share, and its label. */
    mean: { value: number; label: string } | null;
    id: string;
    title: string;
  },
) {
  const { width: W, height: H, padL, padR, padT, padB } = frame;
  const right = W - padR;
  const bottom = H - padB;
  const parts = [
    `<svg class="chart conditional" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-labelledby="${o.id}-t">`,
    `<title id="${o.id}-t">${o.title}</title>`,
  ];
  for (const tick of o.yTicks) {
    const ty = plotY(frame, tick);
    parts.push(
      `<line class="${tick === 0 ? "zero" : "grid"}" x1="${padL}" x2="${right}" y1="${f2(ty)}" y2="${f2(ty)}"/>`,
      `<text class="tick-label" x="${padL - 6}" y="${f2(ty + 4)}" text-anchor="end">${minus(tick)}</text>`,
    );
  }
  parts.push(`<line class="axis-given" x1="${padL}" x2="${right}" y1="${bottom}" y2="${bottom}"/>`);
  for (const tick of o.xTicks) {
    const tx = plotX(frame, tick);
    parts.push(
      `<line class="tick-given" x1="${f2(tx)}" x2="${f2(tx)}" y1="${bottom}" y2="${bottom + 4}"/>`,
      `<text class="tick-label given" x="${f2(tx)}" y="${bottom + 17}" text-anchor="middle">${minus(tick)}</text>`,
    );
  }
  parts.push(
    `<text class="axis-title given" x="${right}" y="${H - 4}" text-anchor="end">${o.xLabel}</text>`,
    `<text class="axis-title" x="4" y="${padT - 10}">${o.yLabel}</text>`,
  );
  if (o.mean && o.mean.value >= frame.yRange[0] && o.mean.value <= frame.yRange[1]) {
    const my = plotY(frame, o.mean.value);
    parts.push(
      `<line class="mean-line" x1="${padL}" x2="${right}" y1="${f2(my)}" y2="${f2(my)}"/>`,
      `<text class="mean-label halo" x="${padL + 4}" y="${f2(my - 6)}">${o.mean.label}</text>`,
    );
  }
  parts.push("</svg>");
  return parts.join("");
}

/** The colours of the tokens as the canvas needs them, resolved for the current theme. */
export function tokenColours(element: Element) {
  const probe = document.createElement("span");
  probe.hidden = true;
  element.append(probe);
  const colour = (token: string) => {
    probe.style.color = `var(${token})`;
    return getComputedStyle(probe).color;
  };
  const colours = {
    muted: colour("--muted"),
    change: colour("--change"),
    ink: colour("--ink"),
    ground: colour("--ground"),
  };
  probe.remove();
  return colours;
}

export interface PlotPoint {
  run: number;
  x: number;
  source: number;
  determinized: number;
}

/** Draws the runs on the plot's canvas: the source's as open circles, the determinized program's
 * as filled points on top, and the pair of run `highlight` outlined. */
export function drawCloud(
  canvas: HTMLCanvasElement,
  frame: PlotFrame,
  points: PlotPoint[],
  highlight: PlotPoint | null,
) {
  const ratio = window.devicePixelRatio || 1;
  canvas.width = Math.round(frame.width * ratio);
  canvas.height = Math.round(frame.height * ratio);
  const context = canvas.getContext("2d");
  if (!context) return;
  context.setTransform(ratio, 0, 0, ratio, 0, 0);
  context.clearRect(0, 0, frame.width, frame.height);
  const colours = tokenColours(canvas);
  const inside = (y: number) => y >= frame.yRange[0] && y <= frame.yRange[1];
  // Thousands of runs get smaller, fainter circles, so that the cloud keeps its shape.
  const many = points.length > 1000;
  context.lineWidth = 1;
  context.strokeStyle = colours.muted;
  context.globalAlpha = Math.min(1, Math.sqrt(1000 / points.length));
  for (const point of points) {
    if (!inside(point.source)) continue;
    context.beginPath();
    context.arc(
      plotX(frame, point.x),
      plotY(frame, point.source),
      many ? 1.8 : 2.6,
      0,
      2 * Math.PI,
    );
    context.stroke();
  }
  context.globalAlpha = 1;
  context.fillStyle = colours.change;
  for (const point of points) {
    if (!inside(point.determinized)) continue;
    context.beginPath();
    context.arc(
      plotX(frame, point.x),
      plotY(frame, point.determinized),
      many ? 1.6 : 2.4,
      0,
      2 * Math.PI,
    );
    context.fill();
  }
  if (!highlight) return;
  context.strokeStyle = colours.ink;
  context.lineWidth = 2;
  for (const y of [highlight.source, highlight.determinized]) {
    if (!inside(y)) continue;
    context.beginPath();
    context.arc(plotX(frame, highlight.x), plotY(frame, y), 5.5, 0, 2 * Math.PI);
    context.stroke();
  }
}
