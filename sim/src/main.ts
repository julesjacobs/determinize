// The simulator's page: the store, the editor, the views and the controls, opened in the state
// of the page's link or else at the first example.
import type { EditorView } from "@codemirror/view";
import { batch, effect } from "@preact/signals-core";
import { examples } from "./core/examples.ts";
import type { Decoded } from "./core/share.ts";
import { decodeShare } from "./core/share.ts";
import { mountDistributionView } from "./ui/distribution-view.ts";
import { createEditor, replaceDoc } from "./ui/editor.ts";
import { mountLeanView } from "./ui/lean-view.ts";
import { mountProgramView } from "./ui/program-view.ts";
import { createSampling } from "./ui/sampling.ts";
import type { Store } from "./ui/store.ts";
import { createStore } from "./ui/store.ts";
import { mountTraceView } from "./ui/trace-view.ts";
import { bindUrl } from "./ui/url.ts";

const editorHost = document.querySelector("#editor") as HTMLElement;
const exampleSelect = document.querySelector("#example-select") as HTMLSelectElement;
const notices = document.querySelector("#notices") as HTMLElement;
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
      alert: document.querySelector("#source-alert") as HTMLElement,
      checked: document.querySelector("#checked") as HTMLElement,
      type: document.querySelector("#checked-type") as HTMLElement,
      sourceSites: document.querySelector("#source-sites") as HTMLElement,
      determinizedSites: document.querySelector("#determinized-sites") as HTMLElement,
      annotated: document.querySelector("#annotated") as HTMLDetailsElement,
      annotatedProgram: document.querySelector("#annotated-program") as HTMLElement,
      note: document.querySelector("#lean-note") as HTMLElement,
    },
    store.analysis,
    store.checkedSource,
  );
  mountProgramView(
    {
      pane: document.querySelector("#determinized-pane") as HTMLElement,
      title: document.querySelector("#determinized-title") as HTMLElement,
      label: document.querySelector("#counterexample-label") as HTMLElement,
      editor: document.querySelector("#determinized-editor") as HTMLElement,
      empty: document.querySelector("#determinized-empty") as HTMLElement,
    },
    store.analysis,
    store.checkedSource,
  );
  // The theorems' premises that typing leaves open; a counterexample shows none.
  effect(() => {
    const result = store.analysis.value;
    (document.querySelector("#premise-safety") as HTMLElement).hidden = !result.ok;
    (document.querySelector("#premise-type") as HTMLElement).hidden =
      !result.ok || result.type === "float[E]";
  });
  mountDistributionView(
    {
      panel: document.querySelector(".distribution-panel") as HTMLElement,
      view: document.querySelector("#distribution-view") as HTMLElement,
      status: document.querySelector("#distribution-status") as HTMLElement,
    },
    store,
  );
  const editor = createEditor(editorHost, store.source.peek(), {
    onChange: (doc) => {
      store.source.value = doc;
    },
  });

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

  effect(() => {
    manyButton.textContent = `Run ${store.sampleCount.value}`;
  });
  // A link may name an example that the gallery doesn't have; then none is selected.
  effect(() => {
    const index = examples.findIndex((example) => example.id === store.exampleId.value);
    exampleSelect.selectedIndex = index;
    exampleSelect.title = examples[index]?.explanation ?? "";
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

/** Shows `message` under the source editor, in place of an earlier notice, until the program
 * changes. */
function showNotice(store: Store, message: string) {
  dismissNotice();
  const notice = document.createElement("p");
  notice.className = "alert";
  notice.setAttribute("role", "alert");
  notice.textContent = message;
  notices.append(notice);
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
