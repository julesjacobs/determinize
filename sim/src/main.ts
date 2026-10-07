// The simulator's page: the store, the editor, the views and the controls, opened in the state
// of the page's link or else at the first example.
import type { EditorView } from "@codemirror/view";
import { batch, effect } from "@preact/signals-core";
import type { Analysis } from "./core/compiler/analyze.ts";
import { examples } from "./core/examples.ts";
import type { Decoded } from "./core/share.ts";
import { decodeShare } from "./core/share.ts";
import { mountDistributionView } from "./ui/distribution-view.ts";
import { createEditor, replaceDoc } from "./ui/editor.ts";
import { mountLeanView } from "./ui/lean-view.ts";
import { createSampling } from "./ui/sampling.ts";
import type { Store } from "./ui/store.ts";
import { createStore } from "./ui/store.ts";
import { mountTraceView } from "./ui/trace-view.ts";
import { bindUrl } from "./ui/url.ts";

const editorHost = document.querySelector("#editor") as HTMLElement;
const exampleSelect = document.querySelector("#example-select") as HTMLSelectElement;
const statusEl = document.querySelector("#status") as HTMLElement;
const typeHintsToggle = document.querySelector("#type-hints-toggle") as HTMLInputElement;
const editorDiagnostics = document.querySelector("#editor-diagnostics") as HTMLElement;
const rerunButton = document.querySelector("#rerun-coupling") as HTMLButtonElement;
const manyButton = document.querySelector("#many-coupling") as HTMLButtonElement;

for (const [index, example] of examples.entries()) {
  const option = document.createElement("option");
  option.value = String(index);
  option.textContent = example.title;
  option.title = example.explanation;
  exampleSelect.append(option);
}

/** The page's store, once the page has opened; scripts and browser tests reach it as
 * `DeterminizeSim.ready`. */
export const ready = decodeShare(window.location.hash).then(start);

/** A random seed for a run, from 1 to 2³² − 1. */
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
      gTrace: document.querySelector("#g-trace") as HTMLElement,
    },
    store,
  );
  mountLeanView(
    {
      section: document.querySelector("#lean-output") as HTMLElement,
      counterexample: document.querySelector("#lean-counterexample") as HTMLElement,
      summary: document.querySelector("#lean-summary") as HTMLElement,
      type: document.querySelector("#checked-type") as HTMLElement,
      sites: document.querySelector("#sample-sites") as HTMLElement,
      typePremise: document.querySelector("#premise-type") as HTMLElement,
      safetyPremise: document.querySelector("#premise-safety") as HTMLElement,
      annotated: document.querySelector("#annotated-program") as HTMLElement,
      determinized: document.querySelector("#determinized-program") as HTMLElement,
    },
    store.analysis,
  );
  mountDistributionView(
    {
      panel: document.querySelector(".distribution-panel") as HTMLElement,
      view: document.querySelector("#distribution-view") as HTMLElement,
      status: document.querySelector("#distribution-status") as HTMLElement,
    },
    store,
  );
  const editor = createEditor(editorHost, store.source.peek(), {
    analysis: store.analysis,
    commits: store.commits,
    typeHints: store.typeHints,
    hoveredSpan: store.hoveredSpan,
    onChange: (doc) => {
      store.source.value = doc;
    },
  });
  typeHintsToggle.checked = store.typeHints.peek();

  exampleSelect.addEventListener("change", () => {
    const example = examples[Number(exampleSelect.value)];
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
    store.runMany(store.sampleCount.peek());
  });
  typeHintsToggle.addEventListener("change", () => {
    store.typeHints.value = typeHintsToggle.checked;
  });

  effect(() => {
    manyButton.textContent = `Run ${store.sampleCount.value}`;
  });
  // A link may name an example that the gallery doesn't have; then none is selected.
  effect(() => {
    const index = examples.findIndex((example) => example.id === store.exampleId.value);
    exampleSelect.selectedIndex = index;
    exampleSelect.title = examples[index]?.explanation ?? "";
  });
  effect(() => {
    renderResult(store.analysis.value);
  });
  bindUrl(store, (next) => restore(store, editor, next));
  if (decoded.kind === "error") {
    showNotice(store, `${decoded.message} The simulator opened its first example.`);
  }
  return store;
}

/** Opens the state of a link that the page navigated to. */
function restore(store: Store, editor: EditorView, decoded: Decoded) {
  if (decoded.kind === "error") showNotice(store, decoded.message);
  if (decoded.kind !== "state") return;
  dismissNotice();
  const { source, seed, example } = decoded.state;
  batch(() => {
    store.exampleId.value = example;
    replaceDoc(editor, source);
    store.runAt(seed);
  });
}

/** Removes the notice that `showNotice` shows, if any. */
let dismissNotice = () => {};

/** Shows `message` above the diagnostics, in place of an earlier notice, until the program
 * changes. */
function showNotice(store: Store, message: string) {
  dismissNotice();
  const notice = document.createElement("p");
  notice.className = "editor-diagnostics error";
  notice.setAttribute("role", "alert");
  notice.textContent = message;
  editorDiagnostics.before(notice);
  const shown = store.source.peek();
  const stop = store.source.subscribe((text) => {
    if (text !== shown) dismissNotice();
  });
  dismissNotice = () => {
    notice.remove();
    stop();
    dismissNotice = () => {};
  };
}

function renderResult(result: Analysis) {
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
