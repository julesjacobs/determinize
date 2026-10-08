// The steps band: one run of the source and of the determinized program, step by step, beside the
// G draws they share. A transport bar (a scrubber between buttons to the first and the last step)
// and the arrow keys move the current step. The rows scroll in a region of their own below the
// bar, which stays in place while the region follows the current step. Rows show the source, the
// G draw, the symbolic state of the paper's proof (σ above its program) and the determinized
// program, with notes on each step's draws and means; a long run arrives from its worker a page at
// a time.
import { computed, effect } from "@preact/signals-core";
import type { Expr } from "../core/compiler/ast.ts";
import { formatNumber } from "../core/format.ts";
import type { Affine } from "../core/runtime/affine.ts";
import { prettyAffine } from "../core/runtime/affine.ts";
import { distributionName } from "../core/runtime/distributions.ts";
import type { GDraw } from "../core/runtime/eval.ts";
import type { Binding, Frame } from "../core/runtime/semantics.ts";
import { isValue } from "../core/runtime/semantics.ts";
import {
  domainErrorMessage,
  frameOk,
  hasDomainError,
  maxSymbolicSteps,
  sigmaMeans,
} from "../core/trace.ts";
import type { TraceOverview, TracePage } from "../core/trace-pages.ts";
import { pageIndexOf } from "../core/trace-pages.ts";
import { escapeHtml } from "./html.ts";
import type { Reduced, StepFacts } from "./steps.ts";
import { stepFacts } from "./steps.ts";
import type { Store } from "./store.ts";
import { renderHighlightedText, renderTraceExpr } from "./trace-expr.ts";

export interface TraceViewElements {
  band: HTMLElement;
  /** The step controls above the rows. */
  transport: HTMLElement;
  first: HTMLButtonElement;
  last: HTMLButtonElement;
  scrubber: HTMLInputElement;
  stepOf: HTMLOutputElement;
  status: HTMLElement;
  /** Goes back to run 0 from a run picked in the conditional-mean plot. */
  showFirst: HTMLButtonElement;
  table: HTMLElement;
  /** The G trace of the run that the table shows. */
  gTrace: HTMLElement;
}

export function mountTraceView(
  elements: TraceViewElements,
  store: Pick<
    Store,
    | "trace"
    | "samples"
    | "showPage"
    | "currentStep"
    | "hoveredStep"
    | "hoveredSite"
    | "linked"
    | "shownRun"
    | "shownTraces"
    | "pickRun"
    | "hoveredRange"
    | "followLinked"
  >,
) {
  const run = computed(() => {
    const state = store.trace.value;
    return state.kind === "run" ? state : null;
  });
  /** What each row of the page shows besides its states. */
  const facts = computed(() => {
    const page = run.value?.page;
    if (!page) return new Map<number, StepFacts>();
    return new Map(
      page.frames.map((frame, index) => [
        frame.step,
        stepFacts(index === 0 ? page.previous : page.frames[index - 1], frame),
      ]),
    );
  });

  effect(() => {
    elements.gTrace.textContent = describeTraces(store.shownTraces.value);
  });

  // The table and the status: busy while the worker computes; the earlier table stays meanwhile.
  effect(() => {
    const state = store.trace.value;
    const busy = state.kind === "computing";
    if (busy) elements.band.setAttribute("aria-busy", "true");
    else elements.band.removeAttribute("aria-busy");
    if (busy) {
      elements.status.textContent = "Computing the steps of this run…";
      return;
    }
    elements.band.classList.toggle(
      "no-run",
      state.kind === "not run" || state.kind === "unavailable",
    );
    if (state.kind !== "run") {
      elements.table.replaceChildren();
      elements.status.textContent =
        state.kind === "not run"
          ? "Steps appear once Lean accepts the program."
          : `The simulator couldn't compute the steps: ${state.message}`;
      return;
    }
    elements.status.textContent = describeRun(state.overview, store.shownRun.peek());
  });
  effect(() => {
    elements.showFirst.hidden = store.shownRun.value === 0;
  });
  elements.showFirst.addEventListener("click", () => store.pickRun(0));

  effect(() => {
    const current = run.value;
    if (!current) return;
    renderRows(elements.table, current.overview, current.page, facts.value);
    markCurrent();
    // A new page, of this run or a new one, brings the current row into view.
    follow(true);
    markMore();
  });

  // The controls follow the current step; the page that holds it is fetched when needed.
  effect(() => {
    const current = run.value;
    const step = store.currentStep.value;
    const last = current ? current.overview.frameCount - 1 : 0;
    elements.scrubber.max = String(last);
    elements.scrubber.value = String(step);
    elements.scrubber.disabled = !current;
    elements.stepOf.textContent = current ? `Step ${step} of ${last}` : "No steps";
    elements.first.disabled = !current || step === 0;
    elements.last.disabled = !current || step === last;
    if (!current) return;
    const { page, overview } = current;
    if (step < page.first || step >= page.first + page.frames.length) {
      store.showPage(pageIndexOf(overview.pageStarts, step));
    }
    markCurrent();
  });

  /** The part of the source under the pointer in either pane: the smallest span that a step on
   * the page reduces around it, or else the sample site there. */
  const hovered = computed(() => {
    const range = store.hoveredRange.value;
    if (range) {
      let smallest: Reduced | null = null;
      for (const fact of facts.value.values()) {
        const reduced = fact.reduced;
        if (!reduced || reduced.from > range.from || range.to > reduced.to) continue;
        if (!smallest || reduced.to - reduced.from < smallest.to - smallest.from)
          smallest = reduced;
      }
      if (smallest) return smallest;
    }
    const site = store.hoveredSite.value;
    return site && { from: site.from, to: site.to, headTo: site.to };
  });

  // Both panes highlight what the hovered or current step reduces, or the hovered span.
  effect(() => {
    const span = hovered.value;
    if (span) {
      store.linked.value = span;
      return;
    }
    const step = store.hoveredStep.value ?? store.currentStep.value;
    // Step 0 has reduced nothing yet; it shows what the first step reduces.
    store.linked.value = facts.value.get(Math.max(step, 1))?.reduced ?? null;
  });
  // The rows that reduce the hovered span.
  effect(() => {
    const span = hovered.value;
    for (const row of elements.table.querySelectorAll<HTMLElement>(".step")) {
      const reduced = facts.peek().get(Number(row.dataset.step))?.reduced;
      row.classList.toggle(
        "linked",
        !!span && !!reduced && reduced.from === span.from && reduced.to === span.to,
      );
    }
  });

  function markCurrent() {
    const step = store.currentStep.peek();
    for (const row of elements.table.querySelectorAll<HTMLElement>(".step[aria-current]")) {
      row.removeAttribute("aria-current");
      // A row that is no longer current hides again what its expanders showed.
      for (const button of row.querySelectorAll<HTMLButtonElement>(
        '.expander[aria-expanded="true"]',
      )) {
        toggle(button);
      }
    }
    elements.table
      .querySelector<HTMLElement>(`.step[data-step="${step}"]`)
      ?.setAttribute("aria-current", "step");
    markMore();
  }

  /**
   * Scrolls the rows' region, and only it, so that the current row is fully visible below the
   * sticky headings, now and again once its page renders: at once while the scrubber is dragged,
   * smoothly for a single move unless the reader prefers reduced motion.
   */
  function follow(instant: boolean) {
    const region = elements.table;
    const row = region.querySelector<HTMLElement>(`.step[data-step="${store.currentStep.peek()}"]`);
    if (!row) return;
    const head = region.querySelector<HTMLElement>(".step-head");
    const covered = head?.offsetParent ? head.offsetHeight : 0;
    const margin = 8;
    const top = row.offsetTop;
    const bottom = top + row.offsetHeight;
    const viewTop = region.scrollTop + covered;
    const viewBottom = region.scrollTop + region.clientHeight;
    if (top >= viewTop && bottom <= viewBottom) return;
    const target =
      top < viewTop || row.offsetHeight > region.clientHeight - covered
        ? top - covered - margin
        : bottom - region.clientHeight + margin;
    const reduced = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    region.scrollTo({
      top: Math.max(0, target),
      behavior: instant || reduced ? "instant" : "smooth",
    });
  }

  /** Moves to `step`, within the run, and keeps its row in view in the region. */
  function moveTo(step: number, instant = false) {
    const current = run.peek();
    if (!current) return;
    const last = current.overview.frameCount - 1;
    store.currentStep.value = Math.max(0, Math.min(last, step));
    store.followLinked.value += 1;
    follow(instant);
  }

  // The rows' region scrolls, so it takes the focus: the keyboard then scrolls it and moves the
  // current step with the arrow keys.
  elements.table.tabIndex = 0;
  new ResizeObserver(() => {
    elements.band.style.setProperty("--transport-height", `${elements.transport.offsetHeight}px`);
  }).observe(elements.transport);
  // The rule below the region follows its scrolling and its size.
  new ResizeObserver(markMore).observe(elements.table);
  elements.table.addEventListener("scroll", markMore, { passive: true });

  /** Marks the region while more rows follow below its view. */
  function markMore() {
    const region = elements.table;
    region.classList.toggle(
      "more-below",
      region.scrollTop + region.clientHeight < region.scrollHeight - 1,
    );
  }

  elements.first.addEventListener("click", () => moveTo(0));
  elements.last.addEventListener("click", () => moveTo(Number.POSITIVE_INFINITY));
  elements.scrubber.addEventListener("input", () => moveTo(Number(elements.scrubber.value), true));

  // The keys move the step from the region and from its expanders, whose row may stop being
  // current; the focus then returns to the region.
  elements.table.addEventListener("keydown", (event) => {
    const target = event.target;
    const expander = target instanceof HTMLElement && target.matches(".expander");
    if (target !== elements.table && !expander) return;
    const step = store.currentStep.peek();
    const moves: Record<string, number> = {
      ArrowDown: step + 1,
      ArrowUp: step - 1,
      PageDown: step + 10,
      PageUp: step - 10,
      Home: 0,
      End: Number.POSITIVE_INFINITY,
    };
    if (!(event.key in moves)) return;
    event.preventDefault();
    if (expander) elements.table.focus({ preventScroll: true });
    moveTo(moves[event.key]);
  });
  elements.table.addEventListener("click", (event) => {
    const target = event.target instanceof Element ? event.target : null;
    const button = target?.closest<HTMLButtonElement>(".expander");
    if (button) {
      toggle(button);
      return;
    }
    const row = target?.closest<HTMLElement>(".step[data-step]");
    if (row && store.trace.peek().kind === "run") moveTo(Number(row.dataset.step));
  });
  elements.table.addEventListener("pointerover", (event) => {
    const target = event.target instanceof Element ? event.target : null;
    const row = target?.closest<HTMLElement>(".step[data-step]");
    const step = row ? Number(row.dataset.step) : null;
    const moved = step !== null && step !== store.hoveredStep.peek();
    store.hoveredStep.value = step;
    if (moved) store.followLinked.value += 1;
    const corr = target?.closest<HTMLElement>(".corr-item");
    showCorrespondence(row, corr?.dataset.corr ?? null);
  });
  elements.table.addEventListener("pointerleave", () => {
    store.hoveredStep.value = null;
    showCorrespondence(null, null);
  });

  /** Marks, within `row`, the symbol `symbol` of σ and the values that correspond to it. */
  function showCorrespondence(row: HTMLElement | null | undefined, symbol: string | null) {
    for (const item of elements.table.querySelectorAll(".corr-active")) {
      item.classList.remove("corr-active");
    }
    if (!row || !symbol) return;
    for (const item of row.querySelectorAll<HTMLElement>(".corr-item")) {
      if (item.dataset.corr === symbol) item.classList.add("corr-active");
    }
  }
}

/** Shows or hides, in place, what an expander's count stands for. */
function toggle(button: HTMLButtonElement) {
  const expanded = button.getAttribute("aria-expanded") !== "true";
  button.setAttribute("aria-expanded", String(expanded));
  button.textContent = (expanded ? button.dataset.fewer : button.dataset.more) ?? "";
  const fold = button.closest(".sigma, .affine");
  fold?.classList.toggle("expanded", expanded);
  fold?.querySelector(".affine-rest")?.toggleAttribute("hidden", !expanded);
}

/** The status of a run as a whole, run `index` of the runs at the seed plus `index`: what is
 * unusual about it, and nothing for run 0 when every step check passed. */
function describeRun(run: TraceOverview, index: number) {
  const steps = run.frameCount - 1;
  const count = `${steps} step${steps === 1 ? "" : "s"}`;
  const at = index === 0 ? `Seed ${run.seed}:` : `Run ${index} of the runs, at seed ${run.seed}:`;
  if (run.domainFailure) {
    return `${at} at step ${steps} a run left an operation's domain (${run.domainFailure}). Typing doesn't establish domain safety, so no theorem relates the runs from there on.`;
  }
  if (run.stopped === "steps") {
    return `${at} the table stops after ${maxSymbolicSteps} steps; Lean's fuel may still let the run return.`;
  }
  if (run.stopped === "size") {
    return `${at} the table stops after ${count}; its states grew too large to show.`;
  }
  // What the counterexample is, the note with the determinized program says.
  if (run.counterexample) return index === 0 ? "" : `${at} ${count} of the counterexample.`;
  if (!run.ok) return `${at} ${count}; a step check failed.`;
  if (run.domainError)
    return `${at} all three runs reached the same domain error at step ${steps}.`;
  return index === 0 ? "" : `${at} ${count}, and every step check passed.`;
}

/** The G draws shown of a trace; the rest are counted. */
const shownDraws = 12;

/** A trace as Lean's `List (Op × ℝ)`, with its first draws. */
function describeTrace(trace: GDraw[]) {
  const shown = trace
    .slice(0, shownDraws)
    .map(({ op, value }) => `(${op}, ${formatNumber(value)})`);
  const more = trace.length > shownDraws ? `, … ${trace.length - shownDraws} more` : "";
  return `[${shown.join(", ")}${more}]`;
}

/** A run's G traces: one when the programs drew the same, as determinization keeps G draws. */
function describeTraces(traces: { source: GDraw[]; determinized: GDraw[] } | null) {
  if (!traces) return "none";
  const { source, determinized } = traces;
  const same =
    source.length === determinized.length &&
    source.every(
      (draw, i) => draw.op === determinized[i].op && draw.value === determinized[i].value,
    );
  if (same) return `${describeTrace(source)}, in both programs`;
  return `${describeTrace(source)} in the source, ${describeTrace(determinized)} in the determinized program`;
}

function cell(kind: string, label: string, content: string) {
  return `<div class="cell cell-${kind}"><span class="cell-label">${label}</span>${content}</div>`;
}

function note(text: string, className: string) {
  return `<span class="step-note ${className}">${text}</span>`;
}

/** The lines of a state that rows other than the current one show. */
const shownLines = 3;

/** `html` split at its line breaks, each line with the elements that are open across it closed
 * at its end and opened again on the next line. */
export function htmlLines(html: string): string[] {
  const lines: string[] = [];
  const open: string[] = [];
  let line = "";
  for (const part of html.split(/(<[^>]+>|\n)/)) {
    if (part === "\n") {
      lines.push(line + open.map(() => "</span>").join(""));
      line = open.join("");
    } else {
      if (part.startsWith("</")) open.pop();
      else if (part.startsWith("<") && !part.endsWith("/>")) open.push(part);
      line += part;
    }
  }
  lines.push(line);
  return lines;
}

/** A state as lines that keep their indentation when they wrap; a long one ends in "…" in every
 * row but the current one. */
function state(html: string) {
  const lines = htmlLines(html).map((line) => {
    const indent = /^ */.exec(line.replace(/^(<[^>]+>)+/, ""))?.[0].length ?? 0;
    const text = line.replace(/^((?:<[^>]+>)*) +/, "$1");
    return `<span class="sl" style="--indent: ${indent}">${text || "&nbsp;"}</span>`;
  });
  const long = lines.length > shownLines;
  return `<code class="state${long ? " long" : ""}">${lines.join("")}${long ? '<span class="more">…<span class="vh"> more lines not shown</span></span>' : ""}</code>`;
}

/** The latest bindings of σ that the current row shows, and that the other rows show; the
 * earlier ones are counted, so that a run with many E draws keeps rows of a readable height, and
 * the current row's count is a button that shows them all. */
const currentBindings = 12;
const shownBindings = 3;

/** A button that shows, in place, what a count stands for, and hides it again. */
function expander(more: string, fewer: string) {
  return `<button type="button" class="expander" aria-expanded="false" data-more="${more}" data-fewer="${fewer}">${more}</button>`;
}

/** σ, its bindings since the step before marked, as a block above the symbolic program. */
function sigmaBlock(lines: string[], added: number) {
  const label = '<span class="sigma-label">σ</span>';
  if (lines.length === 0)
    return `<span class="sigma">${label}<span class="sigma-lines">empty</span></span>`;
  const items = lines.map((line, index) => {
    const age = lines.length - index;
    const classes = [
      "sigma-line",
      ...(age <= added ? ["sigma-new"] : []),
      ...(age > shownBindings ? ["sigma-old"] : []),
      ...(age > currentBindings ? ["sigma-older"] : []),
    ];
    return `<span class="${classes.join(" ")}">${line}</span>`;
  });
  const count = (n: number) => `… ${n} earlier binding${n === 1 ? "" : "s"}`;
  const elsewhere = lines.length - shownBindings;
  const current = lines.length - currentBindings;
  const more =
    (elsewhere > 0
      ? `<span class="sigma-more">${count(elsewhere)}<span class="vh"> not shown</span></span>`
      : "") + (current > 0 ? expander(count(current), "Fewer bindings") : "");
  return `<span class="sigma">${label}<span class="sigma-lines">${more}${items.join("")}</span></span>`;
}

function renderRows(
  table: HTMLElement,
  run: TraceOverview,
  page: TracePage,
  facts: Map<number, StepFacts>,
) {
  const right = run.counterexample ? "Counterexample" : "Determinized";
  const head = `<div class="step-head" aria-hidden="true"><span></span><span>Source</span><span>G draw</span><span>Symbolic state</span><span>${right}</span></div>`;
  const rows = page.frames.map((frame) => {
    const fact = facts.get(frame.step);
    const sigma = sigmaView(frame.sigma);
    const source = renderTraceExpr(frame.original, {
      valueBySymbol: frame.sampleBySymbol,
      valueLabel: "sampled value for",
      short: true,
    });
    const determinized = renderTraceExpr(frame.determinized, {
      valueBySymbol: sigma.meanBySymbol,
      valueLabel: "mean substituted for",
      short: true,
    });
    const eDraw = fact?.eDraw;
    const gDraw = fact?.gDraw;
    const mean = fact?.mean;
    const sourceNote = eDraw
      ? note(
          `E draw, source only: ${eDraw.name ? `${escapeHtml(eDraw.name)} = ` : ""}${renderTraceExpr(eDraw.value, { short: true })}`,
          "n-e",
        )
      : "";
    const meanNote = mean
      ? note(
          `${renderHighlightedText(mean.call, { short: true })} = ${renderTraceExpr(mean.value, { short: true })}`,
          "n-mean",
        )
      : "";
    const draw = gDraw
      ? `<span class="draw"><code>${gDraw.name ? `${escapeHtml(gDraw.name)} ← ` : ""}${renderTraceExpr(gDraw.value, { short: true })}</code><span class="draw-d">${renderHighlightedText(gDraw.distribution, { short: true })}</span></span>`
      : "";
    const before =
      frame.step === page.first ? page.previous : page.frames[frame.step - page.first - 1];
    const added = sigma.lines.length - (before?.sigma.length ?? 0);
    const result =
      frame.symbolic.kind === "SymFloat" && isValue(frame.symbolic)
        ? note(
            `mean ${escapeHtml(formatNumber(affineMean(frame.symbolic.affine, sigma.meanBySymbol)))}`,
            "n-sym",
          )
        : "";
    const symbolicCell = cell(
      "sym",
      "Symbolic state",
      sigmaBlock(sigma.lines, added) +
        state(renderTraceExpr(frame.symbolic, { short: true })) +
        result,
    );
    return `<li class="step${gDraw ? " has-draw" : ""}" data-step="${frame.step}">
      <span class="step-n">${frame.step}</span>
      ${cell("source", "Source", state(source) + sourceNote)}
      <div class="cell cell-draw">${draw ? `<span class="cell-label">G draw</span>${draw}` : ""}</div>
      ${symbolicCell}
      ${cell("det", right, state(determinized) + meanNote)}
      ${run.counterexample ? "" : checkNote(frame)}
    </li>`;
  });
  const last = page.frames.at(-1);
  const end = page.first + page.frames.length === run.frameCount;
  const final = (expr: Expr | undefined, shown: Expr | undefined) =>
    expr && shown && !isValue(shown) ? renderTraceExpr(expr, { short: true }) : null;
  const finals = [
    final(run.finalOriginal, last?.original),
    final(run.finalDeterminized, last?.determinized),
  ];
  if (end && finals.some((final) => final !== null)) {
    const value = (html: string | null) => (html === null ? "" : state(html));
    rows.push(`<li class="step step-result">
      <span class="step-n">→</span>
      ${cell("source", "Source", note("Beyond the table, the run returns", "n-result") + value(finals[0]))}
      <div class="cell cell-draw"></div>
      <div class="cell cell-sym"></div>
      ${cell("det", right, note("Beyond the table, the run returns", "n-result") + value(finals[1]))}
    </li>`);
  }
  table.innerHTML = `${head}<ol class="step-list">${rows.join("")}</ol>${pageNote(page, run)}`;
}

/** What went wrong at a frame whose checks failed or that reached a domain error. */
function checkNote(frame: Frame) {
  if (frame.domainFailure) {
    return `<p class="alert step-alert"><strong>A run left an operation's domain here:</strong> ${escapeHtml(frame.domainFailure)}</p>`;
  }
  if (frameOk(frame)) {
    return hasDomainError(frame)
      ? `<p class="step-alert-note">All three runs reached the same domain error: ${escapeHtml(domainErrorMessage(frame))}</p>`
      : "";
  }
  const failures = [
    frame.originalOk
      ? ""
      : `the source: ${frame.originalError ?? "it doesn't reach the symbolic state"}`,
    frame.determinizedOk
      ? ""
      : `the determinized program: ${frame.determinizedError ?? "it doesn't reach the symbolic state"}`,
    frame.consistencyOk === false ? `the terminal effects: ${frame.consistencyError}` : "",
    frame.symbolicOk === false ? `the symbolic step: ${frame.symbolicError}` : "",
  ].filter(Boolean);
  return `<p class="alert step-alert"><strong>A step check failed</strong> for ${escapeHtml(failures.join("; "))}.</p>`;
}

/** Which steps the page holds, if the run has more than one page. */
function pageNote(page: TracePage, run: TraceOverview) {
  if (run.pageStarts.length < 2) return "";
  const to = page.first + page.frames.length - 1;
  return `<p class="page-note">Steps ${page.first} to ${to} of ${run.frameCount - 1} are shown; the step controls reach the others.</p>`;
}

/** The mean of an affine form, given the means of its symbols. */
function affineMean(affine: Affine, means: Record<string, number>) {
  let mean = affine.constant;
  for (const [name, coefficient] of Object.entries(affine.terms))
    mean += coefficient * (means[name] ?? Number.NaN);
  return mean;
}

/** σ, the E draws of the symbolic state so far, each with its mean. */
function sigmaView(sigma: Binding[]) {
  const meanBySymbol: Record<string, number> = {};
  const lines = sigmaMeans(sigma).map(({ binding, mean, error }) => {
    meanBySymbol[binding.name] = mean;
    const args = binding.args
      .map((arg) => renderHighlightedText(prettyAffine(arg), { short: true }))
      .join(", ");
    const name = escapeHtml(binding.name);
    const value = error
      ? `<span class="sigma-error" title="${escapeHtml(error)}">no mean: a domain error</span>`
      : `mean <span class="corr-item" data-corr="${name}" title="mean substituted for ${name}">${escapeHtml(formatNumber(mean))}</span>`;
    return `<span class="corr-item tok-sym" data-corr="${name}">${name}</span> ~ ${distributionName(binding.kind)}(${args}), ${value}`;
  });
  return { lines, meanBySymbol };
}
