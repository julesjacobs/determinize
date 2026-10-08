// The source editor: CodeMirror with the language, the modes of the sample sites (written ones as
// marks, inferred ones as hints where the annotation would be written), Lean's diagnostics through
// @codemirror/lint, and hovers that give each site's mode with its reason and each expression's
// type. Tab is not bound, so it moves the focus on.
import { defaultKeymap, history, historyKeymap } from "@codemirror/commands";
import { bracketMatching, indentOnInput } from "@codemirror/language";
import type { Diagnostic } from "@codemirror/lint";
import { forceLinting, linter, lintGutter } from "@codemirror/lint";
import type { EditorState, Extension } from "@codemirror/state";
import { RangeSetBuilder, StateField } from "@codemirror/state";
import type { DecorationSet } from "@codemirror/view";
import {
  Decoration,
  drawSelection,
  dropCursor,
  EditorView,
  hoverTooltip,
  keymap,
  lineNumbers,
  placeholder,
  WidgetType,
} from "@codemirror/view";
import type { Analysis, SpanInfo } from "../core/compiler/analyze.ts";
import type { Mode } from "../core/compiler/ast.ts";
import { editorTheme } from "./editor-theme.ts";
import { detHighlighting, detLanguage } from "./language.ts";
import { linkedLines } from "./linking.ts";
import { analysisDelayMs, analyzeSource } from "./store.ts";

/** A diagnostic with a range clamped to the document. */
export interface EditorDiagnostic {
  from: number;
  to: number;
  message: string;
}

/** What the editor reports to the store. */
export interface EditorBindings {
  onChange: (doc: string) => void;
  /** The position under the pointer and the sample site there, if any; both null when the
   * pointer leaves the editor. */
  onHover: (position: number | null, site: { from: number; to: number } | null) => void;
}

const distributionNames = new Set([
  "uniform",
  "gauss",
  "gaussian",
  "exponential",
  "gamma",
  "beta",
  "flip",
  "bernoulli",
  "poisson",
  "discrete",
  "discrete_list",
]);

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
    span.className = `cm-mode-hint cm-mode-${this.mode.toLowerCase()}`;
    span.textContent = `[${this.mode}]`;
    return span;
  }

  ignoreEvent() {
    return false;
  }
}

/** Where a site's mode is: written after the distribution's name, or to be shown there. */
function modePosition(source: string, span: SpanInfo) {
  const text = source.slice(span.from, span.to);
  const match = text.match(/^\s*([A-Za-z_][A-Za-z0-9_]*)/);
  if (!match || !distributionNames.has(match[1])) return null;
  const pos = span.from + match[0].length;
  const written = /^\s*\[([EG])\]/.exec(source.slice(pos, span.to));
  return written ? { pos: pos + written[0].indexOf("["), written: true } : { pos, written: false };
}

/** The sites of an accepted program, or of a counterexample's source, with their modes. */
function sitesOf(result: Analysis) {
  return result.ok
    ? result.spans.filter(
        (span): span is SpanInfo & { mode: Mode } =>
          span.kind === "distribution" && span.mode !== undefined,
      )
    : [];
}

/** The marks on written modes and the hints of inferred ones. Written modes are marked even in a
 * program that Lean rejects. */
const modeMarks = StateField.define<DecorationSet>({
  create: buildModeMarks,
  update(value, tr) {
    return tr.docChanged || tr.selection ? buildModeMarks(tr.state) : value;
  },
  provide: (field) => EditorView.decorations.from(field),
});

function buildModeMarks(state: EditorState): DecorationSet {
  const source = state.doc.toString();
  const result = analyzeSource(source);
  const decorations: { pos: number; decoration: Decoration; to?: number }[] = [];
  const writtenAt = new Set<number>();
  for (const match of source.matchAll(/\[([EG])\]/g)) {
    const pos = match.index;
    writtenAt.add(pos);
    const mode = match[1].toLowerCase();
    decorations.push({
      pos,
      to: pos + 3,
      decoration: Decoration.mark({ class: `cm-mode-${mode}` }),
    });
  }
  for (const span of sitesOf(result)) {
    const at = modePosition(source, span);
    if (!at || at.written || writtenAt.has(at.pos)) continue;
    // The hint would sit under the cursor while typing the annotation.
    if (state.selection.ranges.some((range) => range.empty && Math.abs(range.head - at.pos) <= 1)) {
      continue;
    }
    decorations.push({
      pos: at.pos,
      decoration: Decoration.widget({ widget: new ModeHintWidget(span.mode), side: 1 }),
    });
  }
  decorations.sort((a, b) => a.pos - b.pos);
  const builder = new RangeSetBuilder<Decoration>();
  for (const item of decorations) builder.add(item.pos, item.to ?? item.pos, item.decoration);
  return builder.finish();
}

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

/** The diagnostics that the editor shows for `result`: errors, from Lean's stage that rejects the
 * program, or from the simulator where it failed itself. */
export function lintDiagnostics(result: Analysis, doc: string | number): Diagnostic[] {
  // An empty editor waits for a program; it has nothing wrong to point at.
  if (result.ok || (typeof doc === "string" && doc.trim() === "")) return [];
  const source = result.stage ? `Lean's front end, ${result.stage}` : "the simulator";
  return normalizeDiagnostics(result, doc).map((diagnostic) => ({
    ...diagnostic,
    severity: "error",
    source,
  }));
}

function clamp(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value));
}

/** Why inference gave a site its mode, in the paper's terms. */
export function modeReason(mode: Mode, written: boolean, flip: boolean) {
  if (written)
    return mode === "E" ? "E, as written: replaced by its mean." : "G, as written: kept random.";
  if (flip) return "G: kept random. A flip returns a Boolean, which has no mean.";
  return mode === "E"
    ? "E: replaced by its mean. The output depends on this draw affinely."
    : "G: kept random. The output depends on this draw non-affinely.";
}

function hoverDom(lines: { text: string; className: string }[]) {
  const dom = document.createElement("div");
  dom.className = "cm-hover";
  for (const { text, className } of lines) {
    const line = document.createElement("span");
    line.className = className;
    line.textContent = text;
    dom.append(line);
  }
  return dom;
}

function hovers(): Extension {
  // The first hover that names a mode says what Lean calls it.
  let named = false;
  return hoverTooltip((view, pos) => {
    const source = view.state.doc.toString();
    const result = analyzeSource(source);
    if (!result.ok) return null;
    const span = result.spans.find((candidate) => candidate.from <= pos && pos <= candidate.to);
    if (!span) return null;
    const lines: { text: string; className: string }[] = [];
    if (span.kind === "distribution" && span.mode) {
      const at = modePosition(source, span);
      const flip = /^\s*flip\b/.test(source.slice(span.from, span.to));
      let reason = modeReason(span.mode, at?.written ?? false, flip);
      if (!named) {
        reason += " (Mode: affinity in the Lean development.)";
        named = true;
      }
      lines.push({ text: reason, className: "cm-hover-reason" });
    }
    lines.push({ text: `Type: ${span.type}`, className: "cm-hover-type" });
    return {
      pos: span.from,
      end: span.to,
      above: true,
      create: () => ({ dom: hoverDom(lines) }),
    };
  });
}

/** The smallest sample site at `pos`, if any. */
function siteAt(source: string, pos: number) {
  const result = analyzeSource(source);
  if (!result.ok) return null;
  return sitesOf(result).find((span) => span.from <= pos && pos <= span.to) ?? null;
}

/** The editor; the diagnostics of the text it opens with, or that `replaceDoc` puts in, show at
 * once, those of typing after a pause. */
export function createEditor(parent: HTMLElement, doc: string, bindings: EditorBindings) {
  let hovered: number | null = null;
  const hover = (event: MouseEvent, view: EditorView) => {
    const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
    if (pos === hovered) return false;
    hovered = pos;
    const site = pos === null ? null : siteAt(view.state.doc.toString(), pos);
    bindings.onHover(pos, site && { from: site.from, to: site.to });
    return false;
  };
  const view = new EditorView({
    parent,
    doc,
    extensions: [
      lineNumbers(),
      lintGutter(),
      history(),
      drawSelection(),
      dropCursor(),
      indentOnInput(),
      bracketMatching(),
      detLanguage,
      detHighlighting,
      editorTheme,
      modeMarks,
      linkedLines,
      EditorView.domEventHandlers({
        mousemove: hover,
        mouseleave: () => {
          if (hovered !== null) bindings.onHover(null, null);
          hovered = null;
          return false;
        },
      }),
      linter(
        (view) => {
          const doc = view.state.doc.toString();
          return lintDiagnostics(analyzeSource(doc), doc);
        },
        { delay: analysisDelayMs },
      ),
      hovers(),
      placeholder("Write a program, or pick an example."),
      keymap.of([...defaultKeymap, ...historyKeymap]),
      // The content is in the tab order already; the attribute says so to checkers that look for
      // focusable content in a region that scrolls.
      EditorView.contentAttributes.of({ "aria-label": "Source program", tabindex: "0" }),
      EditorView.updateListener.of((update) => {
        if (update.docChanged) bindings.onChange(update.state.doc.toString());
      }),
    ],
  });
  forceLinting(view);
  return view;
}

/** Replaces the editor's whole text. */
export function replaceDoc(view: EditorView, text: string) {
  view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text } });
  forceLinting(view);
}
