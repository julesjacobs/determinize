import { RangeSetBuilder, StateEffect, StateField } from "@codemirror/state";
import { Decoration, EditorView, hoverTooltip, WidgetType } from "@codemirror/view";
import type { Analysis } from "./core/compiler/analyze.ts";

/** A diagnostic with a range clamped to the document. */
export interface EditorDiagnostic {
  from: number;
  to: number;
  message: string;
}

export const setDiagnostics = StateEffect.define<EditorDiagnostic[]>();

export const diagnosticsState = StateField.define<EditorDiagnostic[]>({
  create() {
    return [];
  },
  update(value, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setDiagnostics)) return effect.value;
    }
    if (tr.docChanged) return [];
    return value;
  },
  provide(field) {
    return EditorView.decorations.from(field, (diagnostics) => {
      const builder = new RangeSetBuilder<Decoration>();
      const sorted = [...diagnostics].sort((a, b) => a.from - b.from || a.to - b.to);
      for (const diagnostic of sorted) {
        if (diagnostic.from === diagnostic.to) {
          builder.add(
            diagnostic.from,
            diagnostic.to,
            Decoration.widget({
              widget: new DiagnosticPointWidget(diagnostic.message),
              side: 1,
            }),
          );
        } else {
          builder.add(
            diagnostic.from,
            diagnostic.to,
            Decoration.mark({
              class: "diagnostic-squiggle",
            }),
          );
        }
      }
      return builder.finish();
    });
  },
});

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

export function diagnosticHover() {
  return hoverTooltip((view, pos) => {
    const diagnostics = view.state.field(diagnosticsState);
    const diagnostic = diagnostics.find((item) => item.from <= pos && pos <= item.to);
    if (!diagnostic) return null;
    return {
      pos: diagnostic.from,
      end: diagnostic.to,
      above: true,
      create() {
        const dom = document.createElement("div");
        dom.className = "diagnostic-tooltip";
        dom.textContent = diagnostic.message;
        return { dom };
      },
    };
  });
}

export function normalizeDiagnostics(result: Analysis, doc: string | number): EditorDiagnostic[] {
  if (result.ok) return [];
  const docLength = typeof doc === "string" ? doc.length : doc;
  return result.diagnostics.map((diagnostic) => {
    const from = clamp(diagnostic.from ?? 0, 0, docLength);
    if (from === docLength && docLength > 0) {
      return {
        from: docLength - 1,
        to: docLength,
        message: diagnostic.message,
      };
    }
    const rawTo = diagnostic.to ?? Math.min(docLength, from + 1);
    const to = docLength === 0 ? 0 : Math.max(from + 1, clamp(rawTo, 0, docLength));
    return {
      from,
      to: Math.min(to, docLength),
      message: diagnostic.message,
    };
  });
}

function clamp(value: number, min: number, max: number) {
  return Math.max(min, Math.min(max, value));
}
