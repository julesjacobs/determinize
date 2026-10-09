// The determinized pane: the determinized program as Lean prints it, with the source's names, in
// a read-only editor, or the counterexample of a program that Lean rejects only for a mode
// conflict, or why there is no program.
import { lintGutter } from "@codemirror/lint";
import { EditorState, RangeSetBuilder, StateField } from "@codemirror/state";
import type { DecorationSet } from "@codemirror/view";
import { Decoration, EditorView, lineNumbers } from "@codemirror/view";
import type { ReadonlySignal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import type { PrintedSpan } from "../core/compiler/pretty.ts";
import { sourcePrettyWithSpans } from "../core/compiler/print.ts";
import { editorTheme, hangingIndent } from "./editor-theme.ts";
import { detHighlighting, detLanguage } from "./language.ts";
import { linkedLines, printedNodeAt, printedSiteAt } from "./linking.ts";

export interface ProgramViewElements {
  pane: HTMLElement;
  title: HTMLElement;
  editor: HTMLElement;
  /** Why there is no program. */
  empty: HTMLElement;
}

/** The means that replaced E draws, and the modes of the draws that stay. */
const marks = StateField.define<DecorationSet>({
  create: buildMarks,
  update: (value, tr) => (tr.docChanged ? buildMarks(tr.state) : value),
  provide: (field) => EditorView.decorations.from(field),
});

function buildMarks(state: EditorState): DecorationSet {
  const builder = new RangeSetBuilder<Decoration>();
  for (const match of state.doc.toString().matchAll(/\bmean_[a-z_]+|\[([EG])\]/g)) {
    const className = match[1] ? `cm-mode-${match[1].toLowerCase()}` : "cm-mean";
    builder.add(match.index, match.index + match[0].length, Decoration.mark({ class: className }));
  }
  return builder.finish();
}

/** The program that the pane shows for `result`, with where each node is, or why it shows
 * none. */
export function shownProgram(result: Analysis) {
  if (result.ok) {
    return { ...sourcePrettyWithSpans(result.program.determinized), counterexample: false };
  }
  if (result.counterexample) {
    return {
      ...sourcePrettyWithSpans(result.counterexample.program.determinized),
      counterexample: true,
    };
  }
  return {
    text: null,
    spans: [],
    why:
      result.stage === null
        ? "No determinized program: the simulator failed on the source."
        : "No determinized program: Lean rejects the source.",
  };
}

export function mountProgramView(
  elements: ProgramViewElements,
  analysis: ReadonlySignal<Analysis>,
  source: ReadonlySignal<string>,
  /** Receives the source range of the smallest printed node under the pointer and the sample site
   * there, if any; both null when the pointer leaves the pane. */
  onHover: (
    range: { from: number; to: number } | null,
    site: { from: number; to: number } | null,
  ) => void,
) {
  /** Where each node of the shown program is. */
  let spans: PrintedSpan[] = [];
  let hovered: number | null = null;
  const hover = (event: MouseEvent, view: EditorView) => {
    const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
    if (pos === hovered) return false;
    hovered = pos;
    const node = pos === null ? null : printedNodeAt(spans, pos);
    const site = pos === null ? null : printedSiteAt(spans, pos);
    onHover(node && { from: node.from, to: node.to }, site && { from: site.from, to: site.to });
    return false;
  };
  const view = new EditorView({
    parent: elements.editor,
    state: EditorState.create({
      extensions: [
        lineNumbers(),
        // An empty diagnostics gutter, so that the code lines up with the source pane's.
        lintGutter(),
        EditorState.readOnly.of(true),
        detLanguage,
        detHighlighting,
        editorTheme,
        EditorView.lineWrapping,
        hangingIndent,
        marks,
        linkedLines,
        EditorView.domEventHandlers({
          mousemove: hover,
          mouseleave: () => {
            if (hovered !== null) onHover(null, null);
            hovered = null;
            return false;
          },
        }),
        EditorView.contentAttributes.of({
          "aria-label": "Determinized program",
          "aria-readonly": "true",
          tabindex: "0",
        }),
      ],
    }),
  });

  effect(() => {
    const result = analysis.value;
    const shown = shownProgram(result);
    const counterexample = shown.text !== null && shown.counterexample;
    elements.pane.classList.toggle("counter", counterexample);
    elements.title.textContent = counterexample ? "Replacing the [E] draws anyway" : "Determinized";
    elements.editor.hidden = shown.text === null;
    elements.empty.hidden = shown.text !== null;
    spans = shown.spans;
    if (shown.text === null) {
      elements.empty.textContent =
        source.value.trim() === ""
          ? "The determinized program appears here once Lean's front end accepts the source."
          : shown.why;
      return;
    }
    if (view.state.doc.toString() !== shown.text) {
      view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: shown.text } });
    }
  });
  return { view, spans: () => spans };
}
