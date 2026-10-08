// The distributions band: both programs run many times, from each edit and each seed on, and what
// they return is compared, the runs so far as they arrive. The output
// distributions come first, a histogram of each program under its column of the step table, on
// one axis and one scale, with their means and variances; where a G draw qualifies, a switch shows
// each run's output against that draw instead, where the
// determinized runs lie on the curve of the source's mean given the draw. Beside the chart: the
// statistics as Lean's CLI computes them, each linked to the Lean definition it estimates, the
// variance-reduction factor and the sample sites that determinization leaves. Every number is an
// estimate of this unverified simulator, and the command that has Lean's CLI report the same runs
// is shown.
import { computed, effect } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import type { Expr } from "../core/compiler/ast.ts";
import { sites } from "../core/compiler/core.ts";
import { examples } from "../core/examples.ts";
import type { Stats, Summary } from "../core/statistics.ts";
import { varianceRatio } from "../core/statistics.ts";
import type { PlotFrame, PlotPoint } from "./charts.ts";
import {
  cutHeight,
  drawCloud,
  histogram,
  histogramChart,
  minus,
  niceAxis,
  outputAxis,
  plotAxes,
  plotX,
  plotY,
  thin,
  widened,
} from "./charts.ts";
import { escapeHtml } from "./html.ts";
import { describeSites } from "./lean-view.ts";
import type { ProgramRuns, Samples, Store } from "./store.ts";
import { eligibleSites, samplingBudgetMs } from "./store.ts";

/** The most runs that the conditional-mean plot draws: more would take the page's thread too long
 * at every batch. */
const plottedRuns = 10000;

/** A statistic as the simulator shows it: four decimals, or three digits with an exponent. */
export function formatStat(value: number) {
  if (Number.isNaN(value)) return "–";
  if (value === 0) return "0";
  if (!Number.isFinite(value)) return value > 0 ? "∞" : "−∞";
  const size = Math.abs(value);
  const text = size >= 1e-3 && size < 1e5 ? value.toFixed(4) : value.toExponential(2);
  return minus(text);
}

/** A mean or a variance of `n` returned numbers, as Lean's CLI prints one that isn't finite. */
function cliStat(value: number, n: number) {
  if (n === 0) return "–";
  return Number.isFinite(value) ? formatStat(value) : "unavailable (floating-point overflow)";
}

/** The command with which Lean's CLI reports these runs' statistics, and the file it reads. */
function command(samples: Samples, file: string | null) {
  // Lean's seeds are UInt64s; run i is at the seed plus i, wrapping around.
  const seed = BigInt.asUintN(64, BigInt(samples.seed));
  const runs = samples.original.summary.runs;
  const line = `./run.sh --seed ${seed} --samples ${runs} ${file ?? "program.det"}`;
  // It breaks only at its spaces, so that no flag is split.
  const words = line
    .split(" ")
    .map((word) => `<span class="word">${escapeHtml(word)}</span>`)
    .join(" ");
  return ` Reproduce them with <code class="cmd">${words}</code>${file ? "." : ", with the program saved as program.det."}`;
}

function returnedText(summary: Summary) {
  const returned = summary.runs - summary.rejected - summary.failed;
  const notes = [
    ...(summary.rejected > 0 ? [`${thin(summary.rejected)} rejected by observe`] : []),
    ...(summary.failed > 0 ? [`${thin(summary.failed)} failed`] : []),
  ];
  return `${thin(returned)} of ${thin(summary.runs)}${notes.map((note) => `<br><span class="sub">${note}</span>`).join("")}`;
}

/** The variable that a `let` binds to the expression with range `from`–`to`, if any. */
function boundName(expr: Expr, from: number, to: number): string | null {
  if (expr.kind === "Let" && expr.value.from === from && expr.value.to === to) return expr.name;
  for (const value of Object.values(expr)) {
    for (const child of Array.isArray(value) ? value : [value]) {
      if (child && typeof child === "object" && "kind" in child && "from" in child) {
        const found = boundName(child as Expr, from, to);
        if (found) return found;
      }
    }
  }
  return null;
}

/** How the band names a G site: its variable and its line. */
function siteLabel(analysis: Analysis, site: number, source: string) {
  if (!analysis.ok) return { name: null, line: null };
  const node = sites(analysis.program.source)[site];
  if (!node) return { name: null, line: null };
  return {
    name: boundName(analysis.ast, node.from, node.to),
    line: source.slice(0, node.from).split("\n").length,
  };
}

export function mountDistributionView(
  band: HTMLElement,
  store: Pick<
    Store,
    | "samples"
    | "stats"
    | "analysis"
    | "checkedSource"
    | "running"
    | "samplesReady"
    | "paused"
    | "exampleId"
    | "sampleCount"
    | "resample"
    | "resume"
    | "stop"
    | "view"
    | "plotSite"
    | "pickRun"
    | "shownRun"
  >,
) {
  const $ = <E extends Element>(id: string) => band.querySelector(`#${id}`) as E;
  const runs = $<HTMLSelectElement>("runs");
  const resample = $<HTMLButtonElement>("resample");
  const action = $<HTMLButtonElement>("sampling-action");
  const progress = $<HTMLElement>("progress");
  const viewSwitch = $<HTMLElement>("view-switch");
  const sitePicker = $<HTMLElement>("site-picker");
  const plotSite = $<HTMLSelectElement>("plot-site");
  const empty = $<HTMLElement>("dist-empty");
  const grid = $<HTMLElement>("dist-grid");
  const histSource = $<HTMLElement>("hist-source");
  const histDet = $<HTMLElement>("hist-det");
  const chartBody = $<HTMLElement>("chart-body");
  const caption = $<HTMLElement>("chart-caption");
  const listOut = $<HTMLElement>("list-out");
  const asTable = $<HTMLDetailsElement>("as-table");

  /** Whether the programs can run: Lean accepts the source, or rejects it only for its modes. */
  const runnable = computed(() => {
    const result = store.analysis.value;
    return result.ok || !!result.counterexample;
  });
  const counterexample = computed(() => {
    const result = store.analysis.value;
    return !result.ok && !!result.counterexample;
  });

  // The controls: the number of runs, Resample, and the progress of sampling with Stop, or where it
  // stopped with Continue.
  effect(() => {
    const count = store.sampleCount.value;
    if (![...runs.options].some((option) => option.value === String(count))) {
      runs.add(new Option(thin(count), String(count)));
    }
    runs.value = String(count);
  });
  runs.addEventListener("change", () => {
    store.sampleCount.value = Number(runs.value);
  });
  resample.addEventListener("click", () => store.resample());
  action.addEventListener("click", () => (store.running.peek() ? store.stop() : store.resume()));
  effect(() => {
    resample.disabled = !runnable.value;
    const running = store.running.value;
    const paused = store.paused.value;
    const done = store.samples.value.original.summary.runs;
    if (running) band.setAttribute("aria-busy", "true");
    else band.removeAttribute("aria-busy");
    progress.classList.toggle("idle", !running && !paused);
    action.textContent = running ? "Stop" : "Continue";
    const target = running?.target ?? paused?.target ?? done;
    const runs = `${thin(done)} of ${thin(target)} runs`;
    $<HTMLElement>("progress-text").textContent = running
      ? `Sampling: ${runs}`
      : paused?.why === "budget"
        ? `Stopped after ${samplingBudgetMs / 1000} s at ${runs}.`
        : paused
          ? `Stopped at ${runs}.`
          : runnable.value
            ? `${thin(done)} run${done === 1 ? "" : "s"} of each program.`
            : "No runs.";
    const bar = $<HTMLProgressElement>("progress-bar");
    bar.max = Math.max(target, 1);
    bar.value = done;
  });

  // The honesty sentence and the command that reproduces the numbers.
  effect(() => {
    const samples = store.samples.value;
    const example = examples.find((entry) => entry.id === store.exampleId.value);
    const file = example?.source === samples.source ? `examples/${example.id}.det` : null;
    const reproduce = $<HTMLElement>("reproduce");
    // The earlier command stays until the first batch of new runs has come in.
    if (runnable.value && !store.samplesReady.value) return;
    if (!runnable.value || samples.original.summary.runs < 2) reproduce.innerHTML = "";
    else if (counterexample.value) {
      reproduce.textContent =
        " Lean rejects this program, so its command-line tool runs no counterexample.";
    } else reproduce.innerHTML = command(samples, file);
  });

  // The switch between the charts, where a G draw qualifies.
  for (const radio of viewSwitch.querySelectorAll<HTMLInputElement>("input")) {
    radio.addEventListener("change", () => {
      if (radio.checked) store.view.value = radio.value === "against" ? "against" : "outputs";
    });
  }
  plotSite.addEventListener("change", () => {
    store.plotSite.value = Number(plotSite.value);
  });
  // A counterexample's runs don't lie on the source's conditional means, so it has no plot.
  const eligible = computed(() => {
    const samples = store.samples.value;
    const result = store.analysis.value;
    const numeric = result.ok && result.type.startsWith("float");
    return samples.original.summary.runs > 1 && numeric ? eligibleSites(samples.original) : [];
  });
  const site = computed(() => {
    const chosen = store.plotSite.value;
    const all = eligible.value;
    return chosen !== null && all.includes(chosen) ? chosen : (all[0] ?? null);
  });
  effect(() => {
    const all = eligible.value;
    const chosen = site.value;
    const result = store.analysis.value;
    const source = store.checkedSource.value;
    // The switch keeps its place until the first batch of new runs has come in.
    if (runnable.value && !store.samplesReady.value) return;
    viewSwitch.hidden = chosen === null;
    for (const radio of viewSwitch.querySelectorAll<HTMLInputElement>("input")) {
      radio.checked = radio.value === (chosen === null ? "outputs" : store.view.value);
    }
    const label = (index: number) => {
      const { name, line } = siteLabel(result, index, source);
      return { name, line };
    };
    const current = chosen === null ? null : label(chosen);
    $<HTMLElement>("against-label").textContent = current?.name
      ? `Against ${current.name}, the G draw`
      : "Against a G draw";
    sitePicker.hidden = chosen === null || all.length < 2 || store.view.value !== "against";
    plotSite.replaceChildren(
      ...all.map((index) => {
        const { name, line } = label(index);
        return new Option(
          `${name ?? "a draw"}${line ? ` on line ${line}` : ""}`,
          String(index),
          false,
          index === chosen,
        );
      }),
    );
  });

  // What the band shows, redrawn at most once a frame while batches arrive.
  let frame = 0;
  const redraw = () => {
    cancelAnimationFrame(frame);
    frame = requestAnimationFrame(render);
  };
  effect(() => {
    store.samples.value;
    store.analysis.value;
    store.running.value;
    store.samplesReady.value;
    store.view.value;
    site.value;
    redraw();
  });
  // A chart is redrawn when its width changes, not when the band grows taller.
  const widths = new Map<Element, number>();
  asTable.addEventListener("toggle", redraw);
  const resized = new ResizeObserver((entries) => {
    let changed = false;
    for (const { target } of entries) {
      if (widths.get(target) === target.clientWidth) continue;
      widths.set(target, target.clientWidth);
      changed = true;
    }
    if (changed) redraw();
  });
  for (const slot of [histSource, chartBody]) resized.observe(slot);
  // The canvas takes the theme's colours when it is drawn.
  new MutationObserver(redraw).observe(document.documentElement, {
    attributes: true,
    attributeFilter: ["data-theme"],
  });
  window.matchMedia("(prefers-color-scheme: dark)").addEventListener("change", redraw);

  function render() {
    const samples = store.samples.peek();
    const result = store.analysis.peek();
    const runsSoFar = samples.original.summary.runs;
    // Until the first batch of new runs has come in, the band keeps the earlier ones, so that
    // nothing below it moves in the meantime, whether or not the batch was cancelled.
    const ready = store.samplesReady.peek();
    if (runnable.peek() && !ready && !(grid.hidden && listOut.hidden)) return;
    const right = counterexample.peek() ? "Counterexample" : "Determinized";
    $<HTMLElement>("honesty").hidden = !runnable.peek() || !ready || runsSoFar < 2;
    if (!runnable.peek() || !ready || runsSoFar < 2) {
      grid.hidden = true;
      listOut.hidden = true;
      asTable.hidden = true;
      empty.hidden = false;
      empty.textContent =
        store.checkedSource.peek().trim() === ""
          ? "Write a program to compare its runs with the determinized program's."
          : runnable.peek()
            ? "Sampling both programs…"
            : result.ok || result.stage !== null
              ? "Nothing to run: Lean rejects the program."
              : "Nothing to run: the simulator failed on this program.";
      return;
    }
    empty.hidden = true;
    // A program of type float has a histogram, even where no run returned; a counterexample
    // has no checked type, so its runs decide.
    const numbers = (runs: ProgramRuns) => runs.values.some((value) => !Number.isNaN(value));
    const numeric = result.ok
      ? result.type.startsWith("float")
      : numbers(samples.original) || numbers(samples.determinized);
    if (!numeric) {
      grid.hidden = true;
      asTable.hidden = true;
      listOut.hidden = false;
      renderList(samples, result, right);
      return;
    }
    listOut.hidden = true;
    grid.hidden = false;
    asTable.hidden = false;
    const stats = store.stats.peek();
    renderStats(samples, stats, result, right);
    const chosen = site.peek();
    const against = chosen !== null && store.view.peek() === "against";
    grid.classList.toggle("against", against);
    histSource.hidden = histDet.hidden = against;
    chartBody.hidden = !against;
    if (against)
      renderPlot(samples, stats, chosen, Math.min(900, chartBody.clientWidth || 640), result);
    else renderOutputs(samples, stats, right);
    // The widths drawn at, so that showing a chart doesn't draw it again.
    for (const slot of [histSource, chartBody]) widths.set(slot, slot.clientWidth);
  }

  function renderStats(
    samples: Samples,
    stats: { original: Stats; determinized: Stats },
    result: Analysis,
    right: string,
  ) {
    $<HTMLElement>("stats-right").textContent = right;
    const programs = [
      ["source", stats.original, samples.original.summary],
      ["det", stats.determinized, samples.determinized.summary],
    ] as const;
    for (const [key, stat, summary] of programs) {
      $<HTMLElement>(`mean-${key}`).textContent = cliStat(stat.mean, stat.n);
      $<HTMLElement>(`ci-${key}`).textContent =
        stat.n > 1 && Number.isFinite(stat.standardError)
          ? formatStat(1.96 * stat.standardError)
          : "–";
      $<HTMLElement>(`variance-${key}`).textContent = cliStat(stat.variance, stat.n);
      $<HTMLElement>(`returned-${key}`).innerHTML = returnedText(summary);
    }
    // The share of runs that return estimates returnProbability only for a float program.
    const floats = result.ok && result.type.startsWith("float");
    $<HTMLElement>("returned-link").hidden = !floats;
    $<HTMLElement>("returned-plain").hidden = floats;
    const failures = [
      ["the source", samples.original.summary.firstFailure],
      [
        right === "Counterexample" ? "the counterexample" : "the determinized program",
        samples.determinized.summary.firstFailure,
      ],
    ].filter((entry): entry is [string, string] => entry[1] !== null);
    const failure = $<HTMLElement>("first-failure");
    failure.hidden = failures.length === 0;
    failure.innerHTML =
      failures
        .map(
          ([where, message]) =>
            `<p><strong>First failure</strong> in ${where}: ${escapeHtml(message)}</p>`,
        )
        .join("") +
      "<p>The statistics are over the runs that returned. The theorems say nothing about failed runs: a run that leaves an operation's domain shows that the program is not domain-safe, and one that reaches the step limit might still have returned.</p>";
    const factor = $<HTMLElement>("factor");
    if (counterexample.peek()) {
      renderSites(result);
      const [a, b] = [stats.original.mean, stats.determinized.mean].map(formatStat);
      factor.textContent = a === b ? `Means: ${a} and ${b}.` : `Means differ: ${a} and ${b}.`;
      return;
    }
    renderSites(result);
    const ratio = varianceRatio(stats.original, stats.determinized);
    factor.innerHTML = Number.isFinite(ratio.value)
      ? `Variance-reduction factor <strong>${escapeHtml(ratio.value.toFixed(2))}</strong>`
      : ratio.value === Infinity
        ? `Variance-reduction factor <strong>∞</strong><span class="sub factor-note">${escapeHtml(ratio.explanation)}</span>`
        : `<span class="factor-note">${escapeHtml(ratio.explanation)}</span>`;
  }

  /** The sample sites of the source and of what determinization makes of it. */
  function renderSites(result: Analysis) {
    const programs = result.ok ? result.program : result.counterexample?.program;
    $<HTMLElement>("sites").hidden = !programs;
    if (!programs) return;
    const target = counterexample.peek() ? "in the counterexample" : "determinized";
    $<HTMLElement>("sites-text").innerHTML =
      `${escapeHtml(describeSites(programs.source))}<span class="vh"> in the source,</span> <span aria-hidden="true">→</span> ${escapeHtml(describeSites(programs.determinized))}<span class="vh"> ${target}</span>`;
  }

  function renderList(samples: Samples, result: Analysis, right: string) {
    $<HTMLElement>("list-right").textContent = right;
    const type = result.ok ? `type <code>${escapeHtml(result.type)}</code>` : "no number";
    $<HTMLElement>("list-lead").innerHTML =
      `The output has ${type}, not a number, so there is no histogram. The first value that each program returned:`;
    for (const [key, summary] of [
      ["source", samples.original.summary],
      ["det", samples.determinized.summary],
    ] as const) {
      const returned = summary.runs - summary.rejected - summary.failed;
      const first = summary.firstValue
        ? `<code class="block">${escapeHtml(summary.firstValue)}</code>`
        : "No run returned a value.";
      $<HTMLElement>(`list-${key}`).innerHTML =
        `${first}<span class="sub">${thin(returned)} of ${thin(summary.runs)} runs returned.</span>`;
    }
  }

  function renderOutputs(
    samples: Samples,
    stats: { original: Stats; determinized: Stats },
    right: string,
  ) {
    const axis = outputAxis(samples.original.values, samples.determinized.values);
    if (!axis) {
      const any = [...samples.original.values, ...samples.determinized.values].some(
        Number.isFinite,
      );
      histSource.innerHTML = any
        ? '<p class="band-empty">The returned numbers span more than a float can hold, so they have no histogram.</p>'
        : '<p class="band-empty">No run returned a number.</p>';
      histDet.innerHTML = "";
      caption.textContent = "";
      $<HTMLElement>("bins-table").innerHTML = "";
      return;
    }
    const source = histogram(samples.original.values, axis.lo, axis.hi, axis.bins);
    const determinized = histogram(samples.determinized.values, axis.lo, axis.hi, axis.bins);
    const yMax = cutHeight(source.counts, determinized.counts);
    const variance = (stat: Stats) => `variance ${cliStat(stat.variance, stat.n)}`;
    const sideBySide = histSource.offsetTop === histDet.offsetTop;
    const programs = [
      [histSource, source, "source", "Source", stats.original],
      [histDet, determinized, "det", right, stats.determinized],
    ] as const;
    for (const [slot, hist, kind, label, stat] of programs) {
      slot.innerHTML = histogramChart({
        hist,
        kind,
        label,
        note: variance(stat),
        mean: stat.n > 0 ? stat.mean : null,
        meanLabel: formatStat(stat.mean),
        ticks: axis.ticks,
        yMax,
        width: Math.min(900, slot.clientWidth || 320),
        tall: sideBySide,
        id: `outputs-${kind}`,
        title: `Histogram of the values that the ${kind === "source" ? "source" : `${right.toLowerCase()} program`} returned, on the axis and scale of both programs' histograms.`,
      });
    }
    const cut = [...source.counts, ...determinized.counts].some((count) => count > yMax);
    const outside = (hist: typeof source, name: string) => {
      const count = hist.below + hist.above;
      if (count === 0) return "";
      return count === 1
        ? ` 1 run of the ${name} lies outside the axis.`
        : ` ${thin(count)} runs of the ${name} lie outside the axis.`;
    };
    caption.textContent =
      `Runs per bin of width ${formatStat(axis.binWidth).replace(/0+$/, "").replace(/\.$/, "")}, on one axis and one scale; the dashed lines mark the means.` +
      (cut ? " A cut bar is labelled with its count." : "") +
      outside(source, "source") +
      outside(determinized, right === "Counterexample" ? "counterexample" : "determinized program");
    renderBinsTable(axis, source, determinized, right);
  }

  function renderBinsTable(
    axis: { lo: number; binWidth: number },
    source: { counts: number[] },
    determinized: { counts: number[] },
    right: string,
  ) {
    if (!asTable.open) return;
    const rows = source.counts
      .map((count, i) => {
        const from = axis.lo + i * axis.binWidth;
        return `<tr><th scope="row">${formatStat(from)} to ${formatStat(from + axis.binWidth)}</th><td>${thin(count)}</td><td>${thin(determinized.counts[i])}</td></tr>`;
      })
      .join("");
    $<HTMLElement>("bins-table").innerHTML =
      `<table class="stats bins"><thead><tr><th scope="col">Returned value</th><th scope="col">Source</th><th scope="col">${right}</th></tr></thead><tbody>${rows}</tbody></table>`;
  }

  /** The plotted runs, for hovering and clicking. */
  let plotted: { frame: PlotFrame; points: PlotPoint[]; canvas: HTMLCanvasElement } | null = null;

  function renderPlot(
    samples: Samples,
    stats: { original: Stats; determinized: Stats },
    chosen: number,
    width: number,
    result: Analysis,
  ) {
    const draws = samples.original.draws.get(chosen) ?? [];
    const points: PlotPoint[] = [];
    for (let run = 0; run < draws.length && points.length < plottedRuns; run++) {
      const source = samples.original.values[run];
      const determinized = samples.determinized.values[run];
      if (Number.isNaN(source) && Number.isNaN(determinized)) continue;
      points.push({ run, x: draws[run], source, determinized });
    }
    const xs = points.map((point) => point.x);
    const outputs = points.flatMap((point) => [point.source, point.determinized]);
    const yAxis = outputAxis(outputs, []);
    if (!yAxis || xs.length === 0) return;
    const [lo, hi] = widened(Math.min(...xs), Math.max(...xs));
    const xAxis = niceAxis(lo, hi, 3);
    const xRange: [number, number] = [xAxis.lo, xAxis.hi];
    const height = Math.round(Math.min(380, Math.max(280, width * 0.5)));
    const plotFrame: PlotFrame = {
      width,
      height,
      padL: 34,
      padR: 12,
      padT: 24,
      padB: 40,
      xRange,
      yRange: [yAxis.lo, yAxis.hi],
    };
    const { name, line } = siteLabel(result, chosen, store.checkedSource.peek());
    const xLabel = `${name ?? "the G draw"}${name ? ", the G draw" : ""}${line ? ` on line ${line}` : ""}`;
    chartBody.innerHTML = `<div class="plot">${plotAxes(plotFrame, {
      xTicks: xAxis.ticks,
      yTicks: yAxis.ticks,
      xLabel: escapeHtml(xLabel),
      yLabel: "output",
      mean:
        stats.determinized.n > 0 && Number.isFinite(stats.determinized.mean)
          ? {
              value: stats.determinized.mean,
              label: `mean ${formatStat(stats.determinized.mean)} (source ${formatStat(stats.original.mean)})`,
            }
          : null,
      id: "against",
      title: `Each run's output against ${escapeHtml(name ?? "the G draw")}: open circles for the source, filled points for the determinized program.`,
    })}<canvas aria-hidden="true"></canvas></div><p class="plot-readout" id="plot-readout"></p>`;
    const canvas = chartBody.querySelector("canvas") as HTMLCanvasElement;
    canvas.style.width = `${width}px`;
    canvas.style.height = `${height}px`;
    drawCloud(canvas, plotFrame, points, null);
    plotted = { frame: plotFrame, points, canvas };
    const shown =
      points.length < samples.original.summary.runs
        ? `${thin(points.length)} of ${thin(samples.original.summary.runs)}`
        : thin(points.length);
    caption.innerHTML = `<span class="key key-source" aria-hidden="true"></span>Source runs <span class="key key-det" aria-hidden="true"></span>Determinized runs, ${shown} shown. For each value of ${escapeHtml(name ?? "the draw")}, the determinized program returns the source's average output at that value, so its runs lie on one curve; the spread of the source's runs around it is the variance that determinization removes. The dashed line marks the mean. Click a run to step through it.`;
    const axis = outputAxis(samples.original.values, samples.determinized.values);
    if (axis) {
      renderBinsTable(
        axis,
        histogram(samples.original.values, axis.lo, axis.hi, axis.bins),
        histogram(samples.determinized.values, axis.lo, axis.hi, axis.bins),
        "Determinized",
      );
    }
  }

  function nearest(event: MouseEvent) {
    if (!plotted) return null;
    const box = plotted.canvas.getBoundingClientRect();
    const x = event.clientX - box.left;
    const y = event.clientY - box.top;
    let best: PlotPoint | null = null;
    let distance = 64;
    for (const point of plotted.points) {
      const px = plotX(plotted.frame, point.x);
      for (const value of [point.source, point.determinized]) {
        const d = (px - x) ** 2 + (plotY(plotted.frame, value) - y) ** 2;
        if (d < distance) {
          distance = d;
          best = point;
        }
      }
    }
    return best;
  }
  chartBody.addEventListener("pointermove", (event) => {
    if (!plotted || !(event.target instanceof HTMLCanvasElement)) return;
    const point = nearest(event);
    drawCloud(plotted.canvas, plotted.frame, plotted.points, point);
    plotted.canvas.style.cursor = point ? "pointer" : "";
    const readout = chartBody.querySelector("#plot-readout");
    if (readout) {
      readout.textContent = point
        ? `Run ${point.run}: draw ${formatStat(point.x)}; source ${formatStat(point.source)}, determinized ${formatStat(point.determinized)}.`
        : "";
    }
  });
  chartBody.addEventListener("pointerleave", () => {
    if (plotted) drawCloud(plotted.canvas, plotted.frame, plotted.points, null);
  });
  chartBody.addEventListener("click", (event) => {
    if (!(event.target instanceof HTMLCanvasElement)) return;
    const point = nearest(event);
    if (point) store.pickRun(point.run);
  });
}
