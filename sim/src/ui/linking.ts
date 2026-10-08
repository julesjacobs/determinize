// Linked highlighting between the step table and the two program panes: the lines of the part of
// the source that the current or hovered step reduces, in the source and at its counterpart in the
// determinized program, and the rows that reduce a hovered sample site. The determinized pane
// shows a printed program whose nodes keep the spans of the source they stand for, so a source
// span is found there through the printer's ranges.
import type { EditorState, Extension, RangeSet, RangeValue } from "@codemirror/state";
import { RangeSetBuilder, StateEffect, StateField } from "@codemirror/state";
import { Decoration, EditorView, GutterMarker, gutterLineClass } from "@codemirror/view";
import type { Expr } from "../core/compiler/ast.ts";
import type { PrintedSpan } from "../core/compiler/pretty.ts";
import type { Reduced } from "./steps.ts";

/** A range of an editor's text. */
export interface TextRange {
  from: number;
  to: number;
}

export const setLinked = StateEffect.define<TextRange | null>();

/** The first and the last line of `range`, if it fits the document. */
function linesOf(state: EditorState, range: TextRange | null) {
  if (!range || range.to > state.doc.length) return null;
  return {
    first: state.doc.lineAt(range.from).number,
    last: state.doc.lineAt(Math.max(range.from, range.to - 1)).number,
  };
}

/** A field of `mark` on the start of each line of the linked range. */
function linkedField<T extends RangeValue>(mark: T) {
  return StateField.define<RangeSet<T>>({
    create: () => new RangeSetBuilder<T>().finish(),
    update(value, tr) {
      let next = value.map(tr.changes);
      for (const effect of tr.effects) {
        if (!effect.is(setLinked)) continue;
        const lines = linesOf(tr.state, effect.value);
        const builder = new RangeSetBuilder<T>();
        for (let line = lines?.first ?? 1; lines && line <= lines.last; line++) {
          const from = tr.state.doc.line(line).from;
          builder.add(from, from, mark);
        }
        next = builder.finish();
      }
      return next;
    },
  });
}

class LinkedGutterMarker extends GutterMarker {
  elementClass = "cm-linked";
}

/**
 * Scrolls the editor, and only it, so that the lines of `range` are fully visible, if they aren't
 * (`EditorView.scrollIntoView` would scroll the window too). An editor that has the focus or the
 * pointer stays put, so that the text doesn't move away from the reader's caret or hover.
 */
export function revealLines(view: EditorView, range: TextRange | null) {
  const lines = linesOf(view.state, range);
  if (!lines || view.hasFocus || view.dom.matches(":hover")) return;
  const from = view.state.doc.line(lines.first).from;
  const to = view.state.doc.line(lines.last).to;
  view.requestMeasure({
    read(view) {
      const scroller = view.scrollDOM;
      // Where the first line starts in the scroller's content.
      const start = view.documentTop - scroller.getBoundingClientRect().top + scroller.scrollTop;
      const top = start + view.lineBlockAt(from).top;
      const bottom = start + view.lineBlockAt(to).bottom;
      const shownTop = scroller.scrollTop;
      const shownBottom = shownTop + scroller.clientHeight;
      if (top >= shownTop && bottom <= shownBottom) return null;
      const margin = view.defaultLineHeight;
      return top < shownTop || bottom - top > scroller.clientHeight
        ? top - margin
        : bottom - scroller.clientHeight + margin;
    },
    write(target, view) {
      if (target !== null) view.scrollDOM.scrollTop = Math.max(0, target);
    },
  });
}

const lineField = linkedField<Decoration>(Decoration.line({ class: "cm-linked" }));
const gutterField = linkedField<GutterMarker>(new LinkedGutterMarker());

/** The linked range's lines, tinted along with their line numbers; the tint follows the text
 * through edits. */
export const linkedLines: Extension = [
  lineField,
  EditorView.decorations.from(lineField),
  gutterField,
  gutterLineClass.from(gutterField),
];

const sites = new Set([
  "Uniform",
  "Gauss",
  "Exponential",
  "Gamma",
  "Beta",
  "Flip",
  "Bernoulli",
  "Poisson",
  "Discrete",
  "DiscreteList",
  "Mean",
]);

/** Whether a node of a printed program is a sample site or the mean that replaced one. */
export function isSite(expr: Expr) {
  return sites.has(expr.kind);
}

/** The printed nodes that stand for the source range `from`–`to`, outermost first. */
function printedFor(spans: PrintedSpan[], from: number, to: number) {
  return spans.filter((span) => span.expr.from === from && span.expr.to === to);
}

/** Where the determinized pane shows the reduced part of the source: from the start of the node
 * that stands for it to the end of its head; or else the smallest node around it. */
export function printedRange(spans: PrintedSpan[], reduced: Reduced): TextRange | null {
  const [node] = printedFor(spans, reduced.from, reduced.to);
  if (node) {
    if (reduced.headTo === reduced.to) return { from: node.start, to: node.end };
    const head = spans.find(
      (span) =>
        span.expr.to === reduced.headTo &&
        span.expr.from > reduced.from &&
        span.start >= node.start &&
        span.end <= node.end,
    );
    return { from: node.start, to: head ? head.end : node.end };
  }
  let around: PrintedSpan | null = null;
  for (const span of spans) {
    if (span.expr.from <= reduced.from && reduced.to <= span.expr.to) around = span;
  }
  return around ? { from: around.start, to: around.end } : null;
}

/** The smallest node of the printed program under position `pos`. */
export function printedNodeAt(spans: PrintedSpan[], pos: number): Expr | null {
  let found: PrintedSpan | null = null;
  for (const span of spans) {
    if (
      span.start <= pos &&
      pos <= span.end &&
      (!found || span.end - span.start <= found.end - found.start)
    ) {
      found = span;
    }
  }
  return found?.expr ?? null;
}

/** The sample site, or the mean that replaced one, under position `pos` of the printed program. */
export function printedSiteAt(spans: PrintedSpan[], pos: number): Expr | null {
  let found: Expr | null = null;
  for (const span of spans) {
    if (span.start <= pos && pos <= span.end && isSite(span.expr)) found = span.expr;
  }
  return found;
}
