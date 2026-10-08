// The simulator's page: the store, the editor, the views and the controls, opened in the state
// of the page's link or else at the first example.
import type { EditorView } from "@codemirror/view";
import { batch, effect } from "@preact/signals-core";
import { examples } from "./core/examples.ts";
import type { Decoded } from "./core/share.ts";
import { decodeShare } from "./core/share.ts";
import { mountDistributionView } from "./ui/distribution-view.ts";
import { createEditor, replaceDoc } from "./ui/editor.ts";
import { mountGallery } from "./ui/gallery.ts";
import { mountHeader } from "./ui/header.ts";
import { mountIntro } from "./ui/intro.ts";
import { mountLeanView } from "./ui/lean-view.ts";
import { printedRange, revealLines, setLinked } from "./ui/linking.ts";
import { mountProgramView } from "./ui/program-view.ts";
import { createSampling } from "./ui/sampling.ts";
import type { Store } from "./ui/store.ts";
import { createStore, randomSeed } from "./ui/store.ts";
import { mountTheme, storedTheme } from "./ui/theme.ts";
import { createTraceClient } from "./ui/trace-client.ts";
import { mountTraceView } from "./ui/trace-view.ts";
import { bindUrl, sharedSource } from "./ui/url.ts";
import { mountVerdict } from "./ui/verdict.ts";

const editorHost = document.querySelector("#editor") as HTMLElement;
const notices = document.querySelector("#notices") as HTMLElement;
const seedInput = document.querySelector("#seed") as HTMLInputElement;
const newSeedButton = document.querySelector("#new-seed") as HTMLButtonElement;

/** The page's store, once the page has opened; scripts and browser tests reach it as
 * `DeterminizeSim.ready`. */
export const ready = decodeShare(window.location.hash).then(start);

/** The page, from the state in its link or else the first example. */
function start(decoded: Decoded): Store {
  const sampling = createSampling((response) => store.receive(response));
  const traces = createTraceClient((response) => store.receiveTrace(response));
  const initial =
    decoded.kind === "state"
      ? { ...decoded.state, source: sharedSource(decoded.state) }
      : { source: examples[0].source, seed: 1, example: examples[0].id };
  const store = createStore(
    {
      source: initial.source,
      seed: initial.seed,
      exampleId: initial.example,
      view: initial.view,
      theme: storedTheme(),
      // On the page's own thread, a smaller first batch keeps the first distributions quick.
      firstBatch: sampling.inThread ? 200 : 1000,
    },
    (request) => sampling.send(request),
    (request) => traces.request(request),
  );
  const $ = <E extends Element>(selector: string) => document.querySelector(selector) as E;
  mountTraceView(
    {
      band: $("#steps"),
      transport: $("#transport"),
      first: $("#step-first"),
      last: $("#step-last"),
      scrubber: $("#scrubber"),
      stepOf: $("#step-of"),
      status: $("#steps-status"),
      showFirst: $("#show-first-run"),
      table: $("#step-table"),
      gTrace: $("#g-trace"),
    },
    store,
  );
  mountLeanView(
    document.querySelector("#source-alert") as HTMLElement,
    store.analysis,
    store.checkedSource,
  );
  const determinized = mountProgramView(
    {
      pane: document.querySelector("#determinized-pane") as HTMLElement,
      title: document.querySelector("#determinized-title") as HTMLElement,
      editor: document.querySelector("#determinized-editor") as HTMLElement,
      empty: document.querySelector("#determinized-empty") as HTMLElement,
    },
    store.analysis,
    store.checkedSource,
    (range, site) => {
      batch(() => {
        store.hoveredRange.value = range;
        store.hoveredSite.value = site;
      });
      if (range || site) store.followLinked.value += 1;
    },
  );
  mountDistributionView($("#distributions"), store);
  mountVerdict(
    {
      pane: $("#determinized-pane"),
      verdict: $("#verdict"),
      lead: $("#verdict-lead"),
      type: $("#premise-type"),
      safe: $("#premise-safe"),
      returns: $("#premise-returns"),
      moments: $("#premise-moments"),
    },
    store,
  );
  mountGallery(
    {
      button: $("#example-button"),
      title: $("#example-title"),
      dialog: $("#gallery"),
      list: $("#gallery-list"),
      close: $("#gallery-close"),
    },
    store,
  );
  mountHeader(
    {
      more: $("#more-button"),
      tools: $("#sim-tools"),
      copy: $("#copy-link"),
      copyStatus: $("#copy-status"),
      glossaryButton: $("#glossary-button"),
      glossary: $("#glossary"),
    },
    store,
  );
  mountIntro($("#intro"), $("#hide-intro"));
  mountTheme($("#theme"), store.theme);
  const editor = createEditor(editorHost, store.source.peek(), {
    onChange: (doc) => {
      // Hovered positions are of the checked text, which the edit makes stale.
      batch(() => {
        store.source.value = doc;
        store.hoveredRange.value = null;
        store.hoveredSite.value = null;
      });
    },
    onHover: (position, site) => {
      // Positions are of the checked text, which an edit makes stale until it is checked again.
      const checked = editor.state.doc.toString() === store.checkedSource.peek();
      batch(() => {
        store.hoveredRange.value =
          checked && position !== null ? { from: position, to: position } : null;
        store.hoveredSite.value = checked ? site : null;
      });
      if (checked && (position !== null || site)) store.followLinked.value += 1;
    },
  });
  // Both panes highlight the lines of what the hovered or current step reduces, or of the hovered
  // site; the source pane only while it shows the checked text. They scroll to those lines when the
  // reader moves the step or hovers, not when a new run or an edit changes them.
  let followed = store.followLinked.peek();
  effect(() => {
    const linked = store.linked.value;
    const checked = store.checkedSource.value;
    const inSource = linked && editor.state.doc.toString() === checked;
    const sourceRange = inSource ? { from: linked.from, to: linked.headTo } : null;
    const detRange = linked && printedRange(determinized.spans(), linked);
    editor.dispatch({ effects: setLinked.of(sourceRange) });
    determinized.view.dispatch({ effects: setLinked.of(detRange) });
    const request = store.followLinked.value;
    // A request waits for the lines of a page still on its way.
    if (request === followed || (!sourceRange && !detRange)) return;
    followed = request;
    revealLines(editor, sourceRange);
    revealLines(determinized.view, detRange);
  });
  // A new run drops a request that it leaves unanswered.
  effect(() => {
    if (store.trace.value.kind !== "run") followed = store.followLinked.peek();
  });

  // An empty editor says what to write.
  const hint = $<HTMLElement>("#source-hint");
  effect(() => {
    hint.hidden = store.source.value.trim() !== "";
  });

  effect(() => {
    seedInput.value = String(store.seed.value);
  });
  seedInput.addEventListener("change", () => {
    const seed = Number(seedInput.value.trim());
    if (Number.isSafeInteger(seed) && seed >= 0) store.runAt(seed);
    else seedInput.value = String(store.seed.peek());
  });
  newSeedButton.addEventListener("click", () => {
    store.runAt(randomSeed());
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
  const { source, seed, example, view } = decoded.state;
  batch(() => {
    store.exampleId.value = example;
    store.view.value = view ?? "outputs";
    replaceDoc(editor, sharedSource({ source, example }));
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
