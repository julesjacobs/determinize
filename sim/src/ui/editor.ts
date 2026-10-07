// The program editor: CodeMirror with the language, the [E]/[G] and type hints, the diagnostics
// and the hovers, bound to the store's signals. Tab is not bound, so it moves the focus on.
import { defaultKeymap, history, historyKeymap } from "@codemirror/commands";
import { bracketMatching, indentOnInput } from "@codemirror/language";
import type { EditorState, Extension } from "@codemirror/state";
import { Compartment, Facet, RangeSetBuilder, StateEffect, StateField } from "@codemirror/state";
import type { DecorationSet } from "@codemirror/view";
import {
  Decoration,
  drawSelection,
  dropCursor,
  EditorView,
  highlightActiveLine,
  highlightActiveLineGutter,
  hoverTooltip,
  keymap,
  lineNumbers,
  WidgetType,
} from "@codemirror/view";
import type { ReadonlySignal, Signal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import type { Analysis, SpanInfo } from "../core/compiler/analyze.ts";
import type { Mode } from "../core/compiler/ast.ts";
import { detHighlighting, detLanguage } from "./language.ts";
import type { Span } from "./store.ts";
import { analyzeSource } from "./store.ts";

/** A diagnostic with a range clamped to the document. */
export interface EditorDiagnostic {
  from: number;
  to: number;
  message: string;
}

/** What the editor reads from the store and reports to it. */
export interface EditorBindings {
  /** The analysis of the checked text, for the diagnostics and the type hovers. */
  analysis: ReadonlySignal<Analysis>;
  /** Changes whenever the text is analyzed; each time, the diagnostics are shown again. */
  commits: ReadonlySignal<number>;
  typeHints: ReadonlySignal<boolean>;
  hoveredSpan: Signal<Span | null>;
  onChange: (doc: string) => void;
}

const distributionNames = new Set([
  "uniform",
  "gauss",
  "gaussian",
  "exponential",
  "gamma",
  "beta",
  "bernoulli",
  "poisson",
  "discrete",
  "discrete_list",
]);

class TypeHintWidget extends WidgetType {
  declare type: string;
  declare from: number;
  declare to: number;

  constructor(type: string, from: number, to: number) {
    super();
    this.type = type;
    this.from = from;
    this.to = to;
  }

  eq(other: TypeHintWidget) {
    return other.type === this.type && other.from === this.from && other.to === this.to;
  }

  toDOM() {
    const span = document.createElement("span");
    span.className = "type-hint";
    span.textContent = `: ${this.type}`;
    span.title = this.type;
    span.tabIndex = 0;
    span.dataset.from = String(this.from);
    span.dataset.to = String(this.to);
    return span;
  }

  ignoreEvent() {
    return false;
  }
}

class ModeHintWidget extends WidgetType {
  declare mode: Mode;

  constructor(mode: Mode) {
    super();
    this.mode = mode;
  }

  eq(other: ModeHintWidget) {
    return other.mode === this.mode;
  }

  toDOM() {
    const span = document.createElement("span");
    span.className = `mode-hint ${this.mode === "G" ? "g-mode" : "e-mode"}`;
    span.textContent = `[${this.mode}]`;
    span.title = `${this.mode}-mode distribution`;
    return span;
  }

  ignoreEvent() {
    return true;
  }
}

class DiagnosticPointWidget extends WidgetType {
  declare message: string;

  constructor(message: string) {
    super();
    this.message = message;
  }

  eq(other: DiagnosticPointWidget) {
    return other.message === this.message;
  }

  toDOM() {
    const marker = document.createElement("span");
    marker.className = "diagnostic-point";
    marker.title = this.message;
    marker.setAttribute("aria-label", this.message);
    return marker;
  }
}

/** Whether the type hints show; a compartment switches it. */
const showTypeHints = Facet.define<boolean, boolean>({
  combine: (values) => values.some(Boolean),
});
const typeHintsCompartment = new Compartment();

/** The [E]/[G] hint after each unannotated distribution's name, and the type hints. */
const hints = StateField.define<DecorationSet>({
  create: buildHints,
  update(value, tr) {
    const toggled = tr.startState.facet(showTypeHints) !== tr.state.facet(showTypeHints);
    return tr.docChanged || tr.selection || toggled ? buildHints(tr.state) : value;
  },
  provide: (field) => EditorView.decorations.from(field),
});

function buildHints(state: EditorState): DecorationSet {
  const source = state.doc.toString();
  const result = analyzeSource(source);
  const builder = new RangeSetBuilder<Decoration>();
  if (!result.ok) return builder.finish();
  const decorations: { pos: number; decoration: Decoration }[] = [];

  for (const span of result.spans) {
    if (span.kind !== "distribution" || (span.mode !== "E" && span.mode !== "G")) continue;
    const pos = hintPosition(source, span);
    if (pos === null) continue;
    // The hint would sit under the cursor while typing the annotation.
    if (state.selection.ranges.some((range) => range.empty && Math.abs(range.head - pos) <= 1)) {
      continue;
    }
    decorations.push({
      pos,
      decoration: Decoration.widget({ widget: new ModeHintWidget(span.mode), side: 1 }),
    });
  }

  if (state.facet(showTypeHints)) {
    const seen = new Set<string>();
    for (const span of result.spans) {
      if (!span.type || span.from === span.to) continue;
      const key = `${span.from}:${span.to}:${span.type}`;
      if (seen.has(key)) continue;
      seen.add(key);
      decorations.push({
        pos: span.to,
        decoration: Decoration.widget({
          widget: new TypeHintWidget(span.type, span.from, span.to),
          side: 1,
        }),
      });
    }
  }

  decorations.sort((a, b) => a.pos - b.pos);
  for (const item of decorations) builder.add(item.pos, item.pos, item.decoration);
  return builder.finish();
}

/** The position after a distribution's name, unless it is annotated already. */
function hintPosition(source: string, span: SpanInfo) {
  const text = source.slice(span.from, span.to);
  const match = text.match(/^\s*([A-Za-z_][A-Za-z0-9_]*)/);
  if (!match || !distributionNames.has(match[1])) return null;
  const pos = span.from + match[0].length;
  const afterName = source.slice(pos, span.to).trimStart();
  if (afterName.startsWith("[E]") || afterName.startsWith("[G]")) return null;
  return pos;
}

const setHoveredSpan = StateEffect.define<Span | null>();

/** The source range of the type hint under the pointer or the focus, marked in the text. */
const hoveredSpanField = StateField.define<Span | null>({
  create: () => null,
  update(value, tr) {
    let next = value;
    if (next && tr.docChanged) {
      next = { from: tr.changes.mapPos(next.from), to: tr.changes.mapPos(next.to) };
    }
    for (const effect of tr.effects) {
      if (effect.is(setHoveredSpan)) next = effect.value;
    }
    return next && next.from < next.to ? next : null;
  },
  provide: (field) =>
    EditorView.decorations.from(field, (span) =>
      span
        ? Decoration.set([Decoration.mark({ class: "type-hint-target" }).range(span.from, span.to)])
        : Decoration.none,
    ),
});

/** The source range that a type hint stands for, if `event` is on one. */
function typeHintTarget(event: Event): Span | null {
  const target =
    event.target instanceof Element ? event.target.closest<HTMLElement>(".type-hint") : null;
  if (!target) return null;
  const from = Number(target.dataset.from);
  const to = Number(target.dataset.to);
  if (!Number.isFinite(from) || !Number.isFinite(to) || from >= to) return null;
  return { from, to };
}

export const setDiagnostics = StateEffect.define<EditorDiagnostic[]>();

/** The diagnostics of the last analysis, until the text changes. */
export const diagnosticsField = StateField.define<EditorDiagnostic[]>({
  create: () => [],
  update(value, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setDiagnostics)) return effect.value;
    }
    return tr.docChanged ? [] : value;
  },
  provide: (field) =>
    EditorView.decorations.from(field, (diagnostics) => {
      const builder = new RangeSetBuilder<Decoration>();
      const sorted = [...diagnostics].sort((a, b) => a.from - b.from || a.to - b.to);
      for (const diagnostic of sorted) {
        builder.add(
          diagnostic.from,
          diagnostic.to,
          diagnostic.from === diagnostic.to
            ? Decoration.widget({ widget: new DiagnosticPointWidget(diagnostic.message), side: 1 })
            : Decoration.mark({ class: "diagnostic-squiggle" }),
        );
      }
      return builder.finish();
    }),
});

/** The analysis's diagnostics, with ranges clamped to a document of length `doc`. */
export function normalizeDiagnostics(result: Analysis, doc: string | number): EditorDiagnostic[] {
  if (result.ok) return [];
  const docLength = typeof doc === "string" ? doc.length : doc;
  return result.diagnostics.map((diagnostic) => {
    const from = clamp(diagnostic.from ?? 0, 0, docLength);
    if (from === docLength && docLength > 0) {
      return { from: docLength - 1, to: docLength, message: diagnostic.message };
    }
    const rawTo = diagnostic.to ?? Math.min(docLength, from + 1);
    const to = docLength === 0 ? 0 : Math.max(from + 1, clamp(rawTo, 0, docLength));
    return { from, to: Math.min(to, docLength), message: diagnostic.message };
  });
}

function clamp(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value));
}

/** A tooltip above `from`–`to` with the class `className` and the text `text`. */
function tooltip(from: number, to: number, className: string, text: string) {
  return {
    pos: from,
    end: to,
    above: true,
    create() {
      const dom = document.createElement("div");
      dom.className = className;
      dom.textContent = text;
      return { dom };
    },
  };
}

function hovers(analysis: ReadonlySignal<Analysis>): Extension {
  return [
    hoverTooltip((_view, pos) => {
      const result = analysis.value;
      if (!result.ok) return null;
      const span = result.spans.find((candidate) => candidate.from <= pos && pos <= candidate.to);
      return span ? tooltip(span.from, span.to, "type-tooltip", span.text) : null;
    }),
    hoverTooltip((view, pos) => {
      const diagnostic = view.state
        .field(diagnosticsField)
        .find((item) => item.from <= pos && pos <= item.to);
      return diagnostic
        ? tooltip(diagnostic.from, diagnostic.to, "diagnostic-tooltip", diagnostic.message)
        : null;
    }),
  ];
}

export function createEditor(parent: HTMLElement, doc: string, bindings: EditorBindings) {
  const showHint = (event: Event) => {
    const span = typeHintTarget(event);
    if (span) bindings.hoveredSpan.value = span;
    return false;
  };
  const hideHint = (event: Event) => {
    if (typeHintTarget(event)) bindings.hoveredSpan.value = null;
    return false;
  };
  const view = new EditorView({
    parent,
    doc,
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
      typeHintsCompartment.of(showTypeHints.of(bindings.typeHints.peek())),
      // The marks precede the hints, so that a mark is split around a hint, not wrapped around it.
      hoveredSpanField,
      diagnosticsField,
      hints,
      hovers(bindings.analysis),
      EditorView.domEventHandlers({
        mouseover: showHint,
        click: showHint,
        focusin: showHint,
        mouseout: hideHint,
        focusout: hideHint,
      }),
      keymap.of([...defaultKeymap, ...historyKeymap]),
      EditorView.lineWrapping,
      EditorView.contentAttributes.of({ "aria-label": "Program" }),
      EditorView.updateListener.of((update) => {
        if (update.docChanged) bindings.onChange(update.state.doc.toString());
      }),
    ],
  });

  effect(() => {
    bindings.commits.value;
    const result = bindings.analysis.value;
    view.dispatch({
      effects: setDiagnostics.of(normalizeDiagnostics(result, view.state.doc.length)),
    });
  });
  effect(() => {
    const show = bindings.typeHints.value;
    view.dispatch({ effects: typeHintsCompartment.reconfigure(showTypeHints.of(show)) });
  });
  effect(() => {
    view.dispatch({ effects: setHoveredSpan.of(bindings.hoveredSpan.value) });
  });
  return view;
}

/** Replaces the editor's whole text. */
export function replaceDoc(view: EditorView, text: string) {
  view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text } });
}
