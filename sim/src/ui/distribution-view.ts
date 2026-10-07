// The distributions of the numbers that the runs of the source and of the determinized program
// returned so far: histogram, empirical CDF, mean, population variance and standard error, and
// the variance ratio.
import { effect } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import { formatNumber } from "../core/format.ts";
import type { Stats } from "../core/statistics.ts";
import { varianceRatio } from "../core/statistics.ts";
import { counterexampleLabel, escapeHtml } from "./html.ts";
import type { Samples, Store } from "./store.ts";

export interface DistributionViewElements {
  /** The panel, busy while a batch runs. */
  panel: HTMLElement;
  view: HTMLElement;
  status: HTMLElement;
}

export function mountDistributionView(
  elements: DistributionViewElements,
  store: Pick<Store, "samples" | "stats" | "analysis" | "trace" | "running">,
) {
  effect(() => {
    if (store.running.value) elements.panel.setAttribute("aria-busy", "true");
    else elements.panel.removeAttribute("aria-busy");
  });
  effect(() => {
    if (store.trace.value.kind === "unavailable") {
      elements.view.innerHTML = "";
      elements.status.textContent = "not numeric";
      return;
    }
    renderDistributions(elements, store.samples.value, store.stats.value, store.analysis.value);
  });
}

function renderDistributions(
  elements: DistributionViewElements,
  samples: Samples,
  stats: { original: Stats; determinized: Stats },
  analysis: Analysis,
) {
  const runs = samples.original.summary.runs;
  elements.status.textContent = `${runs} run${runs === 1 ? "" : "s"}`;
  const all = [samples.original.values, samples.determinized.values];
  if (all.every((values) => values.length === 0)) {
    elements.view.innerHTML = `<p class="distribution-empty">Numeric final results will appear here.</p>`;
    return;
  }
  let min = Infinity;
  let max = -Infinity;
  for (const values of all) {
    for (const x of values) {
      if (x < min) min = x;
      if (x > max) max = x;
    }
  }
  const pad = Math.max((max - min) * 0.08, 1e-6);
  const domain = [min - pad, max + pad];
  const originalStats = stats.original;
  const determinizedStats = stats.determinized;
  const counterexample = !analysis.ok && analysis.counterexample;
  elements.view.innerHTML = `
    ${counterexample ? `<p class="counterexample-label">${counterexampleLabel}</p>` : ""}
    ${distributionCard("Original", samples.original.values, originalStats, domain, "original")}
    ${comparisonCard(originalStats, determinizedStats)}
    ${distributionCard("Determinized", samples.determinized.values, determinizedStats, domain, "determinized")}
  `;
}

function comparisonCard(originalStats: Stats, determinizedStats: Stats) {
  const ratio = varianceRatio(originalStats, determinizedStats);
  return `
    <div class="symbolic-distribution-note">
      <div class="variance-ratio-card">
        <span>Variance ratio</span>
        ${metricValue(ratio.value, "x")}
        <p>${ratio.explanation}</p>
      </div>
    </div>
  `;
}

function metricBlock(label: string, value: number, caption: string, suffix = "") {
  return `
    <div class="metric-block">
      <span>${label}</span>
      ${metricValue(value, suffix)}
      <small>${caption}</small>
    </div>
  `;
}

function metricValue(value: number, suffix = "") {
  return `<strong class="metric-value">${escapeHtml(formatNumber(value))}${suffix}</strong>`;
}

function distributionCard(
  title: string,
  values: readonly number[],
  stats: Stats,
  domain: number[],
  tone: string,
) {
  if (values.length === 0) {
    return `
    <article class="dist-card ${tone}">
      <div class="dist-title"><span>${title}</span></div>
      <p class="distribution-empty">No run returned a number.</p>
    </article>
  `;
  }
  const width = 520;
  const height = 230;
  const margin = { top: 16, right: 16, bottom: 26, left: 34 };
  const pdfBand = { top: 20, bottom: 94 };
  const cdfBand = { top: 126, bottom: 200 };
  const x = (value: number) =>
    margin.left +
    ((value - domain[0]) / (domain[1] - domain[0])) * (width - margin.left - margin.right);
  const yPdf = (density: number, maxDensity: number) =>
    pdfBand.bottom - (density / maxDensity) * (pdfBand.bottom - pdfBand.top);
  const yCdf = (probability: number) =>
    cdfBand.top + (1 - probability) * (cdfBand.bottom - cdfBand.top);
  const sorted = [...values].sort((a, b) => a - b);
  const cdfPath = ecdfPath(sorted, domain, x, yCdf);
  const cdfArea = `${cdfPath} L ${x(domain[1]).toFixed(2)} ${yCdf(0).toFixed(2)} L ${x(domain[0]).toFixed(2)} ${yCdf(0).toFixed(2)} Z`;
  const bins = histogram(values, domain, 100);
  const maxDensity = Math.max(1e-12, ...bins.map((bin) => bin.density));
  const pdfBars = bins
    .map((bin) => {
      const left = x(bin.left);
      const right = x(bin.right);
      const top = yPdf(bin.density, maxDensity);
      return `<rect class="dist-bin" x="${left.toFixed(2)}" y="${top.toFixed(2)}" width="${Math.max(0.5, right - left).toFixed(2)}" height="${(pdfBand.bottom - top).toFixed(2)}"></rect>`;
    })
    .join("");
  const rugValues = values.slice(-180);
  const rugs = rugValues
    .map((value, index) => {
      const jitter = (index % 4) * 1.6;
      const rx = x(value);
      return `<line class="dist-rug" x1="${rx.toFixed(2)}" y1="${(cdfBand.bottom + 7 + jitter).toFixed(2)}" x2="${rx.toFixed(2)}" y2="${(cdfBand.bottom + 13 + jitter).toFixed(2)}"></line>`;
    })
    .join("");
  const meanX = x(stats.mean);
  return `
    <article class="dist-card ${tone}">
      <div class="dist-title">
        <span>${title}</span>
        <span class="metric-pair">mean ${metricValue(stats.mean)}</span>
      </div>
      <div class="dist-metrics">
        ${metricBlock("Variance", stats.variance, "population variance, as Lean's CLI computes it")}
        ${metricBlock("Std. error", stats.standardError, "mean uncertainty")}
      </div>
      <svg viewBox="0 0 ${width} ${height}" role="img" aria-label="${title} empirical PDF and CDF">
        <text class="dist-section-label" x="${margin.left}" y="12">PDF estimate - 100 bins</text>
        <line class="dist-grid" x1="${margin.left}" y1="${pdfBand.top}" x2="${width - margin.right}" y2="${pdfBand.top}"></line>
        <line class="dist-axis" x1="${margin.left}" y1="${pdfBand.bottom}" x2="${width - margin.right}" y2="${pdfBand.bottom}"></line>
        ${pdfBars}

        <text class="dist-section-label" x="${margin.left}" y="${cdfBand.top - 8}">Empirical CDF</text>
        <line class="dist-grid" x1="${margin.left}" y1="${yCdf(1)}" x2="${width - margin.right}" y2="${yCdf(1)}"></line>
        <line class="dist-grid" x1="${margin.left}" y1="${yCdf(0.5)}" x2="${width - margin.right}" y2="${yCdf(0.5)}"></line>
        <line class="dist-axis" x1="${margin.left}" y1="${cdfBand.bottom}" x2="${width - margin.right}" y2="${cdfBand.bottom}"></line>
        <path class="dist-area cdf-area" d="${cdfArea}"></path>
        <path class="dist-curve cdf-curve" d="${cdfPath}"></path>
        <line class="dist-mean" x1="${meanX.toFixed(2)}" y1="${pdfBand.top}" x2="${meanX.toFixed(2)}" y2="${cdfBand.bottom}"></line>
        ${rugs}
        <text class="dist-label" x="${margin.left}" y="${height - 6}">${formatNumber(domain[0])}</text>
        <text class="dist-label end" x="${width - margin.right}" y="${height - 6}">${formatNumber(domain[1])}</text>
        <text class="dist-label y" x="${margin.left - 7}" y="${yCdf(1) + 4}">1</text>
        <text class="dist-label y" x="${margin.left - 7}" y="${yCdf(0) + 4}">0</text>
      </svg>
    </article>
  `;
}

function histogram(values: readonly number[], domain: number[], count: number) {
  const width = domain[1] - domain[0];
  const binWidth = width / count;
  const bins = Array.from({ length: count }, (_, index) => ({
    left: domain[0] + index * binWidth,
    right: domain[0] + (index + 1) * binWidth,
    count: 0,
    density: 0,
  }));
  for (const value of values) {
    const rawIndex = Math.floor((value - domain[0]) / binWidth);
    const index = Math.max(0, Math.min(count - 1, rawIndex));
    bins[index].count += 1;
  }
  for (const bin of bins) bin.density = bin.count / (values.length * binWidth);
  return bins;
}

function ecdfPath(
  sorted: number[],
  domain: number[],
  x: (value: number) => number,
  y: (probability: number) => number,
) {
  if (sorted.length === 0) return "";
  const n = sorted.length;
  const parts = [`M ${x(domain[0]).toFixed(2)} ${y(0).toFixed(2)}`];
  for (let i = 0; i < sorted.length; i++) {
    const valueX = x(sorted[i]).toFixed(2);
    parts.push(`L ${valueX} ${y(i / n).toFixed(2)}`);
    parts.push(`L ${valueX} ${y((i + 1) / n).toFixed(2)}`);
  }
  parts.push(`L ${x(domain[1]).toFixed(2)} ${y(1).toFixed(2)}`);
  return parts.join(" ");
}
