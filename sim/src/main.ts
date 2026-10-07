import { StateEffect, Transaction } from "@codemirror/state";
import type { ViewUpdate } from "@codemirror/view";
import { EditorView } from "@codemirror/view";
import { batch, effect } from "@preact/signals-core";
import type { Analysis } from "./core/compiler/analyze.ts";
import { examples } from "./core/examples.ts";
import type { Decoded } from "./core/share.ts";
import { decodeShare } from "./core/share.ts";
import { mountDistributionView } from "./ui/distribution-view.ts";
import type { EditorDiagnostic } from "./ui/editor.ts";
import { createEditor, diagnosticsField, replaceDoc } from "./ui/editor.ts";
import { createSampling } from "./ui/sampling.ts";
import type { Store } from "./ui/store.ts";
import { createStore } from "./ui/store.ts";
import { mountTraceView } from "./ui/trace-view.ts";
import { bindUrl } from "./ui/url.ts";

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
const rerunButton = document.querySelector("#rerun-coupling") as HTMLButtonElement;
const manyButton = document.querySelector("#many-coupling") as HTMLButtonElement;

let editor: EditorView;
let debugEnabled = false;
let debugSeq = 0;
const debugLog: Record<string, unknown>[] = [];

updateDebugVisibility();
window.addEventListener("hashchange", updateDebugVisibility);

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

decodeShare(window.location.hash).then(start);

/** A seed for a run, drawn as the simulator always has. */
function randomSeed() {
  return Math.floor(1 + Math.random() * 0xffffffff);
}

/** The page, from the state in its link or else the first example. */
function start(decoded: Decoded): Store {
  const sampling = createSampling((response) => store.receive(response));
  const initial =
    decoded.kind === "state"
      ? decoded.state
      : { source: examples[0].source, seed: 2026, example: examples[0].id };
  const store = createStore(
    { source: initial.source, seed: initial.seed, exampleId: initial.example },
    (request) => sampling.send(request),
  );
  mountTraceView(
    {
      table: document.querySelector("#coupling-trace") as HTMLElement,
      status: document.querySelector("#coupling-status") as HTMLElement,
    },
    store,
  );
  mountDistributionView(
    {
      panel: document.querySelector(".distribution-panel") as HTMLElement,
      view: document.querySelector("#distribution-view") as HTMLElement,
      status: document.querySelector("#distribution-status") as HTMLElement,
    },
    store,
  );
  editor = createEditor(editorHost, store.source.peek(), {
    analysis: store.analysis,
    commits: store.commits,
    typeHints: store.typeHints,
    hoveredSpan: store.hoveredSpan,
    onChange: (doc) => {
      store.source.value = doc;
    },
  });
  typeHintsToggle.checked = store.typeHints.peek();
  editor.dispatch({
    effects: StateEffect.appendConfig.of(
      EditorView.updateListener.of((update) => {
        if (update.docChanged || update.selectionSet) logEditorUpdate(update);
      }),
    ),
  });

  exampleSelect.addEventListener("change", () => {
    const example = examples[Number(exampleSelect.value)];
    logDebug("example-change", { example: example.id });
    batch(() => {
      store.exampleId.value = example.id;
      replaceDoc(editor, example.source);
      store.commitSource();
    });
  });

  rerunButton.addEventListener("click", () => {
    store.runAt(randomSeed());
  });

  manyButton.addEventListener("click", () => {
    store.runMany(Array.from({ length: store.sampleCount.peek() }, randomSeed));
  });

  typeHintsToggle.addEventListener("change", () => {
    logDebug("type-hints-toggle", { checked: typeHintsToggle.checked });
    store.typeHints.value = typeHintsToggle.checked;
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

  effect(() => {
    manyButton.textContent = `Run ${store.sampleCount.value}`;
  });
  effect(() => {
    const index = examples.findIndex((example) => example.id === store.exampleId.value);
    if (index < 0) return;
    exampleSelect.value = String(index);
    exampleSelect.title = examples[index].explanation;
  });
  effect(() => {
    renderResult(store.analysis.value);
  });
  bindUrl(store, (next) => restore(store, next));
  if (decoded.kind === "error")
    showNotice(store, `${decoded.message} The simulator opened its first example.`);
  return store;
}

/** Opens the state of a link that the page navigated to. */
function restore(store: Store, decoded: Decoded) {
  if (decoded.kind === "error") showNotice(store, decoded.message);
  if (decoded.kind !== "state") return;
  const { source, seed, example } = decoded.state;
  batch(() => {
    store.exampleId.value = example;
    replaceDoc(editor, source);
    store.runAt(seed);
  });
}

/** Shows `message` above the diagnostics until the program changes. */
function showNotice(store: Store, message: string) {
  const notice = document.createElement("p");
  notice.className = "editor-diagnostics error";
  notice.setAttribute("role", "alert");
  notice.textContent = message;
  editorDiagnostics.before(notice);
  const shown = store.source.peek();
  const stop = store.source.subscribe((text) => {
    if (text === shown) return;
    notice.remove();
    stop();
  });
}

function renderResult(result: Analysis) {
  logDebug("analysis", {
    ok: result.ok,
    diagnostics: result.ok ? [] : result.diagnostics,
    ...collectEditorDebugState("analysis"),
  });
  if (result.ok) {
    setEditorStatus("ok", "Parsed and checked", "✓");
    editorDiagnostics.textContent = "No diagnostics.";
    editorDiagnostics.className = "editor-diagnostics ok";
    return;
  }
  setEditorStatus("error", "Diagnostics", "!");
  editorDiagnostics.textContent = result.diagnostics.map((diag) => diag.message).join("\n");
  editorDiagnostics.className = "editor-diagnostics error";
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
    return editor.state.field(diagnosticsField);
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
