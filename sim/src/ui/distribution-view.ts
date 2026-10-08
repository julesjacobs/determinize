// The distributions band: run both programs many times and compare what they return. The output
// distributions come first, as histograms on one axis with their means and variances; where a G
// draw qualifies, a switch shows each run's output against that draw instead, where the
// determinized runs lie on the curve of the source's mean given the draw. Beside the chart: the
// statistics as Lean's CLI computes them, each linked to the Lean definition it estimates, the
// variance-reduction factor, and the theorems' premises. Every number is an estimate of this
// unverified simulator, and the command that has Lean's CLI report the same runs is shown.
import type { ReadonlySignal } from "@preact/signals-core";
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
  minus,
  niceAxis,
  outputAxis,
  plotAxes,
  plotX,
  plotY,
  stacked,
  thin,
  widened,
} from "./charts.ts";
import { escapeHtml } from "./html.ts";
import type { ProgramRuns, Samples, Store } from "./store.ts";
import { eligibleSites } from "./store.ts";

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
  return ` Reproduce them with <code class="cmd">${escapeHtml(line)}</code>${file ? "." : ", with the program saved as program.det."}`;
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
  /** Whether the band asks for a guess instead of showing its empty state. */
  predicting: ReadonlySignal<boolean>,
  store: Pick<
    Store,
    | "samples"
    | "stats"
    | "analysis"
    | "checkedSource"
    | "running"
    | "exampleId"
    | "sampleCount"
    | "runBoth"
    | "stop"
    | "view"
    | "plotSite"
    | "pickRun"
    | "shownRun"
  >,
) {
  const $ = <E extends Element>(id: string) => band.querySelector(`#${id}`) as E;
  const runs = $<HTMLSelectElement>("runs");
  const runBoth = $<HTMLButtonElement>("run-both");
  const progress = $<HTMLElement>("progress");
  const viewSwitch = $<HTMLElement>("view-switch");
  const sitePicker = $<HTMLElement>("site-picker");
  const plotSite = $<HTMLSelectElement>("plot-site");
  const empty = $<HTMLElement>("dist-empty");
  const grid = $<HTMLElement>("dist-grid");
  const chartBody = $<HTMLElement>("chart-body");
  const caption = $<HTMLElement>("chart-caption");
  const premises = $<HTMLElement>("premises");
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

  // The controls: the number of runs, Run both, and the progress of a batch with Stop.
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
  runBoth.addEventListener("click", () => store.runBoth());
  $<HTMLButtonElement>("stop").addEventListener("click", () => store.stop());
  effect(() => {
    runBoth.disabled = !runnable.value;
    const running = store.running.value;
    const done = store.samples.value.original.summary.runs;
    if (running) band.setAttribute("aria-busy", "true");
    else band.removeAttribute("aria-busy");
    progress.classList.toggle("idle", !running);
    if (!running) return;
    $<HTMLElement>("progress-text").textContent =
      `Sampling: ${thin(done)} of ${thin(running.end)} runs`;
    const bar = $<HTMLProgressElement>("progress-bar");
    bar.max = running.end;
    bar.value = done;
  });

  // The honesty sentence and the command that reproduces the numbers.
  effect(() => {
    const samples = store.samples.value;
    const example = examples.find((entry) => entry.id === store.exampleId.value);
    const file = example?.source === samples.source ? `examples/${example.id}.det` : null;
    const reproduce = $<HTMLElement>("reproduce");
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
    store.view.value;
    site.value;
    predicting.value;
    redraw();
  });
  // A chart is redrawn when its width changes, not when the band grows taller.
  let width = 0;
  asTable.addEventListener("toggle", redraw);
  new ResizeObserver(() => {
    if (chartBody.clientWidth === width) return;
    width = chartBody.clientWidth;
    redraw();
  }).observe(chartBody);
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
    const right = counterexample.peek() ? "Counterexample" : "Determinized";
    $<HTMLElement>("honesty").hidden = !runnable.peek() || runsSoFar < 2;
    renderPremises(result);
    if (!runnable.peek() || runsSoFar < 2) {
      grid.hidden = true;
      empty.after(premises);
      listOut.hidden = true;
      asTable.hidden = true;
      empty.hidden = predicting.peek();
      empty.textContent =
        store.checkedSource.peek().trim() === ""
          ? "Write a program or pick an example, then run both programs to compare their outputs."
          : runnable.peek()
            ? "Run both programs to compare their outputs."
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
      listOut.after(premises);
      renderList(samples, result, right);
      return;
    }
    listOut.hidden = true;
    grid.hidden = false;
    asTable.hidden = false;
    $<HTMLElement>("dist-grid").querySelector(".dist-side")?.append(premises);
    const stats = store.stats.peek();
    renderStats(samples, stats, result, right);
    const width = Math.min(900, chartBody.clientWidth || 640);
    const chosen = site.peek();
    if (chosen !== null && store.view.peek() === "against") {
      renderPlot(samples, stats, chosen, width, result);
    } else {
      renderOutputs(samples, stats, width, right);
    }
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
      const [a, b] = [stats.original.mean, stats.determinized.mean].map(formatStat);
      factor.textContent = a === b ? `Means: ${a} and ${b}.` : `Means differ: ${a} and ${b}.`;
      return;
    }
    const ratio = varianceRatio(stats.original, stats.determinized);
    factor.innerHTML = Number.isFinite(ratio.value)
      ? `Variance-reduction factor <strong>${escapeHtml(ratio.value.toFixed(2))}</strong>`
      : ratio.value === Infinity
        ? `Variance-reduction factor <strong>∞</strong><span class="sub factor-note">${escapeHtml(ratio.explanation)}</span>`
        : `<span class="factor-note">${escapeHtml(ratio.explanation)}</span>`;
  }

  function renderPremises(result: Analysis) {
    premises.hidden = !result.ok;
    if (!result.ok) return;
    const float = result.type === "float[E]";
    $<HTMLElement>("premises-float").hidden = !float;
    $<HTMLElement>("premises-other").hidden = float;
    $<HTMLElement>("other-type").textContent = result.type;
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
    width: number,
    right: string,
  ) {
    const axis = outputAxis(samples.original.values, samples.determinized.values);
    if (!axis) {
      const any = [...samples.original.values, ...samples.determinized.values].some(
        Number.isFinite,
      );
      chartBody.innerHTML = any
        ? '<p class="band-empty">The returned numbers span more than a float can hold, so they have no histogram.</p>'
        : '<p class="band-empty">No run returned a number.</p>';
      caption.textContent = "";
      $<HTMLElement>("bins-table").innerHTML = "";
      return;
    }
    const source = histogram(samples.original.values, axis.lo, axis.hi, axis.bins);
    const determinized = histogram(samples.determinized.values, axis.lo, axis.hi, axis.bins);
    const variance = (stat: Stats) => `variance ${cliStat(stat.variance, stat.n)}`;
    chartBody.innerHTML = stacked({
      source,
      determinized,
      labels: ["Source", right],
      notes: [variance(stats.original), variance(stats.determinized)],
      means: [
        stats.original.n > 0 ? stats.original.mean : null,
        stats.determinized.n > 0 ? stats.determinized.mean : null,
      ],
      meanLabels: [formatStat(stats.original.mean), formatStat(stats.determinized.mean)],
      ticks: axis.ticks,
      width,
      id: "outputs",
      title: `Histograms of the returned values, the source above and the ${right.toLowerCase()} program below, on one axis.`,
    });
    const yMax = cutHeight(source.counts, determinized.counts);
    const cut = [...source.counts, ...determinized.counts].some((count) => count > yMax);
    const outside = (hist: typeof source, name: string) => {
      const count = hist.below + hist.above;
      if (count === 0) return "";
      return count === 1
        ? ` 1 run of the ${name} lies outside the axis.`
        : ` ${thin(count)} runs of the ${name} lie outside the axis.`;
    };
    caption.textContent =
      `Runs per bin of width ${formatStat(axis.binWidth).replace(/0+$/, "").replace(/\.$/, "")}, on the same axes; the dashed lines mark the means.` +
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
