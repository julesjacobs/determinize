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
import { sourcePretty } from "../core/compiler/print.ts";
import { editorTheme } from "./editor-theme.ts";
import { detHighlighting, detLanguage } from "./language.ts";

export interface ProgramViewElements {
  pane: HTMLElement;
  title: HTMLElement;
  /** The counterexample's label above its program. */
  label: HTMLElement;
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

/** The program that the pane shows for `result`, or why it shows none. */
export function shownProgram(result: Analysis) {
  if (result.ok) return { text: sourcePretty(result.program.determinized), counterexample: false };
  if (result.counterexample) {
    return { text: sourcePretty(result.counterexample.program.determinized), counterexample: true };
  }
  return {
    text: null,
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
) {
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
        marks,
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
    elements.label.hidden = !counterexample;
    elements.editor.hidden = shown.text === null;
    elements.empty.hidden = shown.text !== null;
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
  return view;
}
