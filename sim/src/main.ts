import { defaultKeymap, history, historyKeymap } from "@codemirror/commands";
import { bracketMatching, indentOnInput } from "@codemirror/language";
import { EditorState, Transaction } from "@codemirror/state";
import type { ViewUpdate } from "@codemirror/view";
import {
  drawSelection,
  dropCursor,
  EditorView,
  highlightActiveLine,
  highlightActiveLineGutter,
  hoverTooltip,
  keymap,
  lineNumbers,
} from "@codemirror/view";
import type { Analysis } from "./core/compiler/analyze.ts";
import { analyze } from "./core/compiler/analyze.ts";
import type { Expr } from "./core/compiler/ast.ts";
import { prettyExpr } from "./core/compiler/pretty.ts";
import { examples } from "./core/examples.ts";
import { formatNumber } from "./core/format.ts";
import { prettyAffine } from "./core/runtime/affine.ts";
import { distributionName } from "./core/runtime/distributions.ts";
import type { Binding, CoupledTrace, Frame } from "./core/runtime/semantics.ts";
import type { Stats } from "./core/statistics.ts";
import { sampleStats, varianceRatio } from "./core/statistics.ts";
import {
  domainErrorMessage,
  frameOk,
  hasDomainError,
  runBatch,
  runCoupling,
  sampleOf,
  sigmaMeans,
} from "./core/trace.ts";
import type { EditorDiagnostic } from "./diagnostics.ts";
import {
  diagnosticHover,
  diagnosticsState,
  normalizeDiagnostics,
  setDiagnostics,
} from "./diagnostics.ts";
import { detHighlighting, detLanguage } from "./language.ts";
import { hoveredTypeHintState, modeHints, setTypeHints, typeHintState } from "./modeHints.ts";
import type { TraceOptions } from "./traceRender.ts";
import { changedPath, renderHighlightedText, renderTraceExpr } from "./traceRender.ts";

/** The label of the run of a program that Lean rejects only for a mode conflict. */
const counterexampleLabel =
  "Lean rejects this program; this is what replacing its [E] draws anyway does";

const editorHost = document.querySelector("#editor") as HTMLElement;
const exampleSelect = document.querySelector("#example-select") as HTMLSelectElement;
const statusEl = document.querySelector("#status") as HTMLElement;
const typeHintsToggle = document.querySelector("#type-hints-toggle") as HTMLInputElement;
const debugToggle = document.querySelector("#debug-toggle") as HTMLInputElement;
const debugToggleControl = document.querySelector(".debug-toggle") as HTMLElement;
const editorDiagnostics = document.querySelector("#editor-diagnostics") as HTMLElement;
const debugPanel = document.querySelector("#debug-panel") as HTMLElement;
const debugLogEl = document.querySelector("#debug-log") as HTMLElement;
const debugCopyButton = document.querySelector("#debug-copy") as HTMLButtonElement;
const debugClearButton = document.querySelector("#debug-clear") as HTMLButtonElement;
const panels = {
  coupling: document.querySelector("#coupling-trace") as HTMLElement,
  couplingStatus: document.querySelector("#coupling-status") as HTMLElement,
  distribution: document.querySelector("#distribution-view") as HTMLElement,
  distributionStatus: document.querySelector("#distribution-status") as HTMLElement,
};
const rerunButton = document.querySelector("#rerun-coupling") as HTMLButtonElement;
const manyButton = document.querySelector("#many-coupling") as HTMLButtonElement;
typeHintsToggle.checked = false;

const ANALYZE_IDLE_MS = 500;

const checkPopoverPortal = document.createElement("div");
checkPopoverPortal.className = "floating-check-popover";
checkPopoverPortal.setAttribute("role", "tooltip");
document.body.append(checkPopoverPortal);

let latest: Analysis | null = null;
let debounce: number | null = null;
let couplingSeed = 2026;
let sampleSource = "";
let lastSampleKey = "";
let activeCheck: Element | null = null;
let hideCheckPopoverTimer: number | null = null;
let activeCorrespondence: string | null = null;
let debugEnabled = false;
let debugSeq = 0;
const debugLog: Record<string, unknown>[] = [];
const samples: { original: number[]; determinized: number[] } = {
  original: [],
  determinized: [],
};

updateDebugVisibility();
window.addEventListener("hashchange", updateDebugVisibility);

function typeHover() {
  return hoverTooltip((_view, pos) => {
    if (!latest?.ok) return null;
    const span = latest.spans.find((candidate) => candidate.from <= pos && pos <= candidate.to);
    if (!span) return null;
    return {
      pos: span.from,
      end: span.to,
      above: true,
      create() {
        const dom = document.createElement("div");
        dom.className = "type-tooltip";
        dom.textContent = span.text;
        return { dom };
      },
    };
  });
}

function updateDebugVisibility() {
  const params = new URLSearchParams(window.location.search);
  const hash = window.location.hash.toLowerCase();
  const visible = params.has("debug") || hash === "#debug" || hash.includes("debug");
  debugToggleControl.hidden = !visible;
  debugToggleControl.style.display = visible ? "" : "none";
  if (!visible && debugEnabled) {
    debugEnabled = false;
    debugToggle.checked = false;
    debugPanel.hidden = true;
  }
}

for (const [index, example] of examples.entries()) {
  const option = document.createElement("option");
  option.value = String(index);
  option.textContent = example.title;
  option.title = example.explanation;
  exampleSelect.append(option);
}
exampleSelect.title = examples[0].explanation;

const editor = new EditorView({
  parent: editorHost,
  state: EditorState.create({
    doc: examples[0].source,
    extensions: [
      lineNumbers(),
      highlightActiveLineGutter(),
      history(),
      drawSelection(),
      dropCursor(),
      indentOnInput(),
      bracketMatching(),
      highlightActiveLine(),
      detLanguage,
      detHighlighting,
      typeHintState,
      hoveredTypeHintState,
      diagnosticsState,
      modeHints,
      typeHover(),
      diagnosticHover(),
      keymap.of([...defaultKeymap, ...historyKeymap]),
      EditorView.lineWrapping,
      EditorView.updateListener.of((update) => {
        if (update.docChanged || update.selectionSet) logEditorUpdate(update);
        if (update.docChanged) scheduleAnalyze();
      }),
    ],
  }),
});

exampleSelect.addEventListener("change", () => {
  const example = examples[Number(exampleSelect.value)];
  const source = example.source;
  exampleSelect.title = example.explanation;
  logDebug("example-change", { example: example.id });
  editor.dispatch({ changes: { from: 0, to: editor.state.doc.length, insert: source } });
  runAnalyze();
});

rerunButton.addEventListener("click", () => {
  couplingSeed = Math.floor(1 + Math.random() * 0xffffffff);
  runAnalyze();
});

manyButton.addEventListener("click", () => {
  const source = editor.state.doc.toString();
  if (source !== sampleSource) resetSamples(source);
  const seeds = Array.from({ length: 200 }, () => Math.floor(1 + Math.random() * 0xffffffff));
  const batch = runBatch(source, seeds);
  samples.original.push(...batch.original);
  samples.determinized.push(...batch.determinized);
  if (batch.last) {
    couplingSeed = batch.last.seed;
    if (sampleOf(batch.last)) lastSampleKey = `${source}:${batch.last.seed}`;
    renderCoupling(batch.last);
  }
  renderDistributions();
});

typeHintsToggle.addEventListener("change", () => {
  logDebug("type-hints-toggle", { checked: typeHintsToggle.checked });
  editor.dispatch({ effects: setTypeHints.of(typeHintsToggle.checked) });
});

debugToggle.addEventListener("change", () => {
  debugEnabled = debugToggle.checked;
  debugPanel.hidden = !debugEnabled;
  if (debugEnabled) {
    logDebug("debug-enabled", collectEditorDebugState("toggle"));
  } else {
    renderDebugLog();
  }
});

debugCopyButton.addEventListener("click", async () => {
  const text = debugLog.map((entry) => JSON.stringify(entry)).join("\n");
  try {
    await navigator.clipboard.writeText(text);
    debugCopyButton.textContent = "Copied";
    setTimeout(() => {
      debugCopyButton.textContent = "Copy";
    }, 900);
  } catch {
    debugLogEl.textContent = text;
  }
});

debugClearButton.addEventListener("click", () => {
  debugLog.length = 0;
  debugSeq = 0;
  logDebug("debug-cleared", collectEditorDebugState("clear"));
});

panels.coupling.addEventListener("pointerover", (event) => {
  const corr =
    event.target instanceof Element ? event.target.closest<HTMLElement>(".corr-item") : null;
  if (corr) showCorrespondence(corr);
  const check = event.target instanceof Element ? event.target.closest(".step-check") : null;
  if (check) showCheckPopover(check);
});

panels.coupling.addEventListener("pointerout", (event) => {
  const corr =
    event.target instanceof Element ? event.target.closest<HTMLElement>(".corr-item") : null;
  if (corr) {
    const next =
      event.relatedTarget instanceof Element
        ? event.relatedTarget.closest<HTMLElement>(".corr-item")
        : null;
    if (!next || next.dataset.corr !== corr.dataset.corr) hideCorrespondence();
  }
  const check = event.target instanceof Element ? event.target.closest(".step-check") : null;
  if (!check) return;
  const next = event.relatedTarget instanceof Element ? event.relatedTarget : null;
  if (next && (check.contains(next) || checkPopoverPortal.contains(next))) return;
  scheduleHideCheckPopover();
});

panels.coupling.addEventListener("focusin", (event) => {
  const corr =
    event.target instanceof Element ? event.target.closest<HTMLElement>(".corr-item") : null;
  if (corr) showCorrespondence(corr);
  const check = event.target instanceof Element ? event.target.closest(".step-check") : null;
  if (check) showCheckPopover(check);
});

panels.coupling.addEventListener("focusout", (event) => {
  const corr = event.target instanceof Element ? event.target.closest(".corr-item") : null;
  if (corr) hideCorrespondence();
  const next = event.relatedTarget instanceof Element ? event.relatedTarget : null;
  if (next && (panels.coupling.contains(next) || checkPopoverPortal.contains(next))) return;
  scheduleHideCheckPopover();
});

checkPopoverPortal.addEventListener("pointerenter", cancelHideCheckPopover);
checkPopoverPortal.addEventListener("pointerleave", scheduleHideCheckPopover);
window.addEventListener(
  "scroll",
  () => {
    if (activeCheck) positionCheckPopover(activeCheck);
  },
  true,
);
window.addEventListener("resize", () => {
  if (activeCheck) positionCheckPopover(activeCheck);
});
document.addEventListener("keydown", (event) => {
  if (event.key === "Escape") hideCheckPopover();
});

function scheduleAnalyze() {
  clearTimeout(debounce as number);
  logDebug("schedule-analyze", { idleMs: ANALYZE_IDLE_MS, ...collectEditorDebugState("schedule") });
  debounce = setTimeout(runAnalyze, ANALYZE_IDLE_MS);
}

function runAnalyze() {
  const source = editor.state.doc.toString();
  logDebug("run-analyze-start", collectEditorDebugState("before-analyze"));
  latest = analyze(source);
  const diagnostics = normalizeDiagnostics(latest, source);
  editor.dispatch({ effects: setDiagnostics.of(diagnostics) });
  logDebug("run-analyze-result", {
    ok: latest.ok,
    diagnostics: diagnostics.map((diagnostic) => ({
      from: diagnostic.from,
      to: diagnostic.to,
      message: diagnostic.message,
    })),
    rawDiagnostics: latest.ok ? [] : latest.diagnostics,
    ...collectEditorDebugState("after-diagnostics-dispatch"),
  });
  renderResult(latest);
}

function renderResult(result: Analysis) {
  logDebug("render-result", { ok: result.ok, ...collectEditorDebugState("render-result") });
  if (result.ok) {
    setEditorStatus("ok", "Parsed and checked", "✓");
    editorDiagnostics.textContent = "No diagnostics.";
    editorDiagnostics.className = "editor-diagnostics ok";
    renderSemantics(editor.state.doc.toString());
    return;
  }

  setEditorStatus("error", "Diagnostics", "!");
  editorDiagnostics.textContent = result.diagnostics.map((diag) => diag.message).join("\n");
  editorDiagnostics.className = "editor-diagnostics error";
  if (result.counterexample) {
    renderSemantics(editor.state.doc.toString());
    return;
  }
  // Lean rejects the program for a reason other than a mode conflict: there is nothing to run.
  resetSamples(editor.state.doc.toString());
  panels.coupling.innerHTML = "";
  panels.couplingStatus.textContent = "Not run";
  panels.couplingStatus.className = "status error";
  renderDistributions();
}

function setEditorStatus(kind: string, label: string, glyph: string) {
  statusEl.textContent = glyph;
  statusEl.title = label;
  statusEl.setAttribute("aria-label", label);
  statusEl.className = `status editor-status ${kind}`;
}

function logEditorUpdate(update: ViewUpdate) {
  logDebug("editor-update", {
    docChanged: update.docChanged,
    selectionSet: update.selectionSet,
    transactions: update.transactions.map((transaction) => ({
      docChanged: transaction.docChanged,
      selection: transaction.selection
        ? transaction.selection.ranges.map((range) => ({
            from: range.from,
            to: range.to,
            anchor: range.anchor,
            head: range.head,
            empty: range.empty,
          }))
        : [],
      userEvent: transaction.annotation(Transaction.userEvent) ?? null,
      effects: transaction.effects.length,
    })),
    ...collectEditorDebugState("update"),
  });
}

function logDebug(event: string, data: Record<string, unknown> = {}) {
  if (!debugEnabled && event !== "debug-enabled") return;
  debugLog.push({
    seq: ++debugSeq,
    timeMs: Math.round(performance.now()),
    event,
    ...data,
  });
  if (debugLog.length > 400) debugLog.splice(0, debugLog.length - 400);
  renderDebugLog();
}

function renderDebugLog() {
  if (!debugLogEl) return;
  debugLogEl.textContent = debugLog.map((entry) => JSON.stringify(entry)).join("\n");
  debugLogEl.scrollTop = debugLogEl.scrollHeight;
}

function collectEditorDebugState(label: string) {
  try {
    const doc = editor.state.doc.toString();
    const selection = editor.state.selection.ranges.map((range) => ({
      from: range.from,
      to: range.to,
      anchor: range.anchor,
      head: range.head,
      empty: range.empty,
    }));
    const main = editor.state.selection.main;
    const headLine = editor.state.doc.lineAt(main.head);
    const diagnostics = readDiagnosticsForDebug();
    const root = editorHost.closest(".editor-pane") ?? document;
    return {
      label,
      doc: {
        length: doc.length,
        lines: editor.state.doc.lines,
        text: capDebugText(doc, 2000),
      },
      selection,
      activeLine: {
        number: headLine.number,
        from: headLine.from,
        to: headLine.to,
        text: headLine.text,
      },
      example: examples[Number(exampleSelect.value)]?.id ?? null,
      typeHintsEnabled: typeHintsToggle.checked,
      status: statusEl.getAttribute("aria-label"),
      diagnostics: diagnostics.map((diagnostic) => ({
        from: diagnostic.from,
        to: diagnostic.to,
        message: diagnostic.message,
      })),
      dom: {
        activeElement: describeElement(document.activeElement),
        cursorCount: root.querySelectorAll(".cm-cursor").length,
        cursorStyles: Array.from(
          root.querySelectorAll(".cm-cursor"),
          (el) => el.getAttribute("style") ?? "",
        ),
        modeHints: Array.from(root.querySelectorAll(".mode-hint"), describeHint),
        typeHints: Array.from(root.querySelectorAll(".type-hint"), describeHint),
        diagnosticSquiggles: root.querySelectorAll(".diagnostic-squiggle").length,
        diagnosticPoints: root.querySelectorAll(".diagnostic-point").length,
        contentText: capDebugText(
          editorHost.querySelector<HTMLElement>(".cm-content")?.innerText ?? "",
          2000,
        ),
      },
    };
  } catch (error) {
    return {
      label,
      collectError: error instanceof Error ? error.message : String(error),
    };
  }
}

function readDiagnosticsForDebug(): EditorDiagnostic[] {
  try {
    return editor.state.field(diagnosticsState);
  } catch {
    return [];
  }
}

function describeHint(element: Element) {
  const rect = element.getBoundingClientRect();
  return {
    text: element.textContent,
    className: element.className,
    left: Math.round(rect.left),
    top: Math.round(rect.top),
    width: Math.round(rect.width),
    height: Math.round(rect.height),
  };
}

function describeElement(element: Element | null) {
  if (!(element instanceof Element)) return null;
  return {
    tag: element.tagName.toLowerCase(),
    id: element.id || null,
    className: typeof element.className === "string" ? element.className : null,
    text: capDebugText(element.textContent ?? "", 120),
  };
}

function capDebugText(text: string, maxLength: number) {
  if (text.length <= maxLength) return text;
  return `${text.slice(0, maxLength)}...<truncated ${text.length - maxLength} chars>`;
}

function renderSemantics(source: string) {
  try {
    logDebug("render-semantics-start", { sourceLength: source.length });
    if (source !== sampleSource) resetSamples(source);
    const coupled = runCoupling(source, couplingSeed);
    addSampleFromCoupling(coupled, source);
    renderCoupling(coupled);
    renderDistributions();
    logDebug("render-semantics-ok", {
      frames: coupled.frames.length,
      ok: coupled.ok,
      ...collectEditorDebugState("render-semantics-ok"),
    });
  } catch (error) {
    panels.coupling.innerHTML = "";
    panels.couplingStatus.textContent = "Trace unavailable";
    panels.couplingStatus.className = "status error";
    panels.distribution.innerHTML = "";
    panels.distributionStatus.textContent = "not numeric";
    logDebug("render-semantics-error", {
      message: error instanceof Error ? error.message : String(error),
      ...collectEditorDebugState("render-semantics-error"),
    });
  }
}

function renderCoupling(coupled: CoupledTrace) {
  hideCheckPopover();
  const terminalDomainError = coupled.frames.some(hasDomainError);
  if (coupled.counterexample) {
    panels.couplingStatus.textContent = `seed ${coupled.seed} - counterexample`;
    panels.couplingStatus.className = "status warning";
  } else {
    panels.couplingStatus.textContent = `seed ${coupled.seed} - ${coupled.ok ? (terminalDomainError ? "checked domain error" : "checked") : "failed"}`;
    panels.couplingStatus.className = `status ${coupled.ok ? (terminalDomainError ? "warning" : "ok") : "error"}`;
  }
  panels.coupling.innerHTML =
    (coupled.counterexample ? `<p class="counterexample-label">${counterexampleLabel}</p>` : "") +
    `
    <div class="coupling-table-head">
      <span></span>
      <span>Original</span>
      <span>Symbolic</span>
      <span>Determinized</span>
    </div>
    <div class="coupling-table-body">
  ` +
    coupled.frames
      .map((frame, index, frames) => {
        const previous = index > 0 ? frames[index - 1] : null;
        const sigma = sigmaView(frame.sigma);
        const sigmaLines = Math.max(1, Math.min(4, sigma.lineCount));
        const ok = frameOk(frame);
        const domainError = hasDomainError(frame);
        return `
        <section class="coupling-row ${ok ? "" : "failed"} ${ok && domainError ? "domain-error-row" : ""}" style="--sigma-lines: ${sigmaLines}">
          <div class="step-rail">
            <span>${frame.step}</span>
            ${stepCheck(frame, coupled)}
          </div>
          ${couplingCell(frame.original, "", "original", { focusPath: changedPath(previous?.original, frame.original), valueBySymbol: frame.sampleBySymbol, valueLabel: "sampled value for" })}
          ${couplingCell(frame.symbolic, sigma.html, "symbolic", { focusPath: changedPath(previous?.symbolic, frame.symbolic) })}
          ${couplingCell(frame.determinized, "", "determinized", { focusPath: changedPath(previous?.determinized, frame.determinized), valueBySymbol: sigma.meanBySymbol, valueLabel: "mean substituted for" })}
        </section>
      `;
      })
      .join("") +
    "</div>";
}

function stepCheck(frame: Frame, coupled: CoupledTrace) {
  const ok = frameOk(frame);
  const domainError = hasDomainError(frame);
  const label = domainError && ok ? "ERR" : ok ? "OK" : "FAIL";
  const aria =
    domainError && ok
      ? "Coupling checks reached a shared domain error"
      : ok
        ? "Coupling checks passed"
        : "Coupling check failed";
  return `
    <span class="step-check ${ok ? (domainError ? "domain" : "ok") : "fail"}" tabindex="0" aria-label="${aria}">
      ${label}
      <span class="check-popover-source">
        ${checkPopoverContent(frame, coupled, ok, domainError)}
      </span>
    </span>
  `;
}

function checkPopoverContent(
  frame: Frame,
  coupled: CoupledTrace,
  ok: boolean,
  domainError: boolean,
) {
  const originalTarget = frame.originalTarget ? prettyExpr(frame.originalTarget) : "not available";
  const determinizedTarget = frame.determinizedTarget
    ? prettyExpr(frame.determinizedTarget)
    : "not available";
  return `
    <strong>${domainError && ok ? "All traces reached the same domain error at this symbolic step." : ok ? "Coupling checks passed at this symbolic step." : "Coupling check failed at this symbolic step."}</strong>
    ${domainError ? `<span class="domain-error-note">${escapeHtml(domainErrorMessage(frame))}</span>` : ""}
    <span>The source trace must match the symbolic state after sampling stored E-bindings with the same E-randomness.</span>
    <code>${escapeHtml(originalTarget)}</code>
    <span>The determinized trace must match the symbolic state after replacing stored E-bindings by their means.</span>
    <code>${escapeHtml(determinizedTarget)}</code>
    <span>Source sync: ${frame.originalOk ? `${frame.originalMicroSteps} step${frame.originalMicroSteps === 1 ? "" : "s"}` : `failed${frame.originalError ? `: ${escapeHtml(frame.originalError)}` : ""}`}</span>
    <span>Determinized sync: ${frame.determinizedOk ? `${frame.determinizedMicroSteps} step${frame.determinizedMicroSteps === 1 ? "" : "s"}` : `failed${frame.determinizedError ? `: ${escapeHtml(frame.determinizedError)}` : ""}`}</span>
    ${frame.consistencyOk === false ? `<span>Terminal consistency: failed: ${escapeHtml(frame.consistencyError)}</span>` : ""}
    ${frame.symbolicOk === false ? `<span>Symbolic next step failed: ${escapeHtml(frame.symbolicError)}</span>` : ""}
    ${coupled.counterexample ? `<em>${counterexampleLabel}</em>` : ""}
  `;
}

function couplingCell(expr: Expr, meta: string, tone: string, traceOptions: TraceOptions = {}) {
  return `
    <article class="coupling-cell ${tone}">
      <div class="sigma-strip ${meta ? "" : "blank"}">${meta || "&nbsp;"}</div>
      <pre class="code-view">${renderTraceExpr(expr, traceOptions)}</pre>
    </article>
  `;
}

function showCheckPopover(check: Element) {
  const source = check.querySelector(".check-popover-source");
  if (!source) return;
  cancelHideCheckPopover();
  activeCheck = check;
  checkPopoverPortal.innerHTML = source.innerHTML;
  checkPopoverPortal.classList.add("visible");
  check.classList.add("popover-open");
  positionCheckPopover(check);
}

function positionCheckPopover(check: Element) {
  const anchor = check.getBoundingClientRect();
  const popover = checkPopoverPortal.getBoundingClientRect();
  const margin = 10;
  const preferredLeft = anchor.right + 10;
  const left =
    preferredLeft + popover.width <= window.innerWidth - margin
      ? preferredLeft
      : Math.max(margin, anchor.left - popover.width - 10);
  const centeredTop = anchor.top + anchor.height / 2 - popover.height / 2;
  const top = clamp(centeredTop, margin, window.innerHeight - popover.height - margin);
  checkPopoverPortal.style.left = `${left}px`;
  checkPopoverPortal.style.top = `${top}px`;
}

function scheduleHideCheckPopover() {
  clearTimeout(hideCheckPopoverTimer as number);
  hideCheckPopoverTimer = setTimeout(hideCheckPopover, 120);
}

function cancelHideCheckPopover() {
  clearTimeout(hideCheckPopoverTimer as number);
}

function hideCheckPopover() {
  clearTimeout(hideCheckPopoverTimer as number);
  if (activeCheck) activeCheck.classList.remove("popover-open");
  activeCheck = null;
  checkPopoverPortal.classList.remove("visible");
}

function showCorrespondence(anchor: HTMLElement) {
  const symbol = anchor.dataset.corr;
  if (!symbol) return;
  const scope = anchor.closest(".coupling-row") ?? panels.coupling;
  const key = `${symbol}:${rowIndex(scope)}`;
  if (activeCorrespondence === key) return;
  hideCorrespondence();
  activeCorrespondence = key;
  for (const item of scope.querySelectorAll(`[data-corr="${cssEscape(symbol)}"]`)) {
    item.classList.add("corr-active");
  }
}

function hideCorrespondence() {
  if (!activeCorrespondence) return;
  for (const item of panels.coupling.querySelectorAll(".corr-active"))
    item.classList.remove("corr-active");
  activeCorrespondence = null;
}

function rowIndex(scope: Element) {
  return scope instanceof HTMLElement
    ? String(Array.prototype.indexOf.call(scope.parentElement?.children ?? [], scope))
    : "all";
}

function sigmaView(sigma: Binding[]) {
  if (sigma.length === 0) return { html: "", lineCount: 0, meanBySymbol: {} };
  const meanBySymbol: Record<string, number> = {};
  const lines = sigmaMeans(sigma).map(({ binding, mean, error }) => {
    meanBySymbol[binding.name] = mean;
    const args = binding.args.map((arg) => renderHighlightedText(prettyAffine(arg))).join(", ");
    return `<span class="sigma-binding corr-item" data-corr="${escapeHtml(binding.name)}" tabindex="0"><span class="sigma-definition"><span class="tok-sym">${escapeHtml(binding.name)}</span> ~ <span class="tok-dist">${distributionName(binding.kind)}</span>(${args})</span><span class="sigma-mean">E[<span class="tok-sym">${escapeHtml(binding.name)}</span>] = ${meanMarkup(binding.name, mean, error)}</span></span>`;
  });
  return { html: lines.join("\n"), lineCount: lines.length, meanBySymbol };
}

function meanMarkup(symbol: string, mean: number, error: string | null = null) {
  if (error) {
    return `<span class="sigma-mean-error" title="${escapeHtml(error)}">domain error</span>`;
  }
  const value = formatNumber(mean);
  return `<span class="corr-item sigma-mean-value" data-corr="${escapeHtml(symbol)}" title="mean substituted for ${escapeHtml(symbol)}">${escapeHtml(value)}</span>`;
}

function resetSamples(source: string) {
  sampleSource = source;
  lastSampleKey = "";
  samples.original = [];
  samples.determinized = [];
}

function addSampleFromCoupling(coupled: CoupledTrace, source: string) {
  const key = `${source}:${coupled.seed}`;
  if (key === lastSampleKey) return;
  const sample = sampleOf(coupled);
  if (sample) {
    samples.original.push(sample.original);
    samples.determinized.push(sample.determinized);
    lastSampleKey = key;
  }
}

function renderDistributions() {
  const count = Math.min(samples.original.length, samples.determinized.length);
  panels.distributionStatus.textContent = `${count} sample${count === 1 ? "" : "s"}`;
  if (count === 0) {
    panels.distribution.innerHTML = `<p class="distribution-empty">Numeric final results will appear here.</p>`;
    return;
  }
  const all = [...samples.original, ...samples.determinized];
  const min = Math.min(...all);
  const max = Math.max(...all);
  const pad = Math.max((max - min) * 0.08, 1e-6);
  const domain = [min - pad, max + pad];
  const originalStats = sampleStats(samples.original);
  const determinizedStats = sampleStats(samples.determinized);
  const counterexample = latest && !latest.ok && latest.counterexample;
  panels.distribution.innerHTML = `
    ${counterexample ? `<p class="counterexample-label">${counterexampleLabel}</p>` : ""}
    ${distributionCard("Original", samples.original, originalStats, domain, "original")}
    ${comparisonCard(originalStats, determinizedStats)}
    ${distributionCard("Determinized", samples.determinized, determinizedStats, domain, "determinized")}
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
  values: number[],
  stats: Stats,
  domain: number[],
  tone: string,
) {
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
        ${metricBlock("Variance", stats.variance, "sample variance")}
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

function histogram(values: number[], domain: number[], count: number) {
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

function clamp(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value));
}

function cssEscape(value: string) {
  if (window.CSS?.escape) return window.CSS.escape(value);
  return String(value).replace(/["\\]/g, "\\$&");
}

function escapeHtml(text: string | undefined) {
  return String(text)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}

runAnalyze();
