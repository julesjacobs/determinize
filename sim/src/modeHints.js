import { RangeSetBuilder, StateEffect, StateField } from "@codemirror/state";
import { Decoration, EditorView, ViewPlugin, WidgetType } from "@codemirror/view";
import { analyze } from "./compiler/analyze.js";

const distributionNames = new Set([
  "uniform",
  "gauss",
  "exponential",
  "gamma",
  "beta",
  "bernoulli",
  "poisson",
  "discrete",
]);

class TypeHintWidget extends WidgetType {
  constructor(type, from, to) {
    super();
    this.type = type;
    this.from = from;
    this.to = to;
  }

  eq(other) {
    return other.type === this.type && other.from === this.from && other.to === this.to;
  }

  toDOM(view) {
    const span = document.createElement("span");
    span.className = "type-hint";
    span.textContent = `: ${this.type}`;
    span.title = this.type;
    span.tabIndex = 0;
    span.dataset.from = String(this.from);
    span.dataset.to = String(this.to);
    const show = () => view?.dispatch?.({ effects: setHoveredTypeHint.of({ from: this.from, to: this.to }) });
    const hide = () => view?.dispatch?.({ effects: setHoveredTypeHint.of(null) });
    span.addEventListener("mouseenter", show);
    span.addEventListener("mouseover", show);
    span.addEventListener("pointerenter", show);
    span.addEventListener("click", show);
    span.addEventListener("focus", show);
    span.addEventListener("mouseleave", hide);
    span.addEventListener("pointerleave", hide);
    span.addEventListener("blur", hide);
    return span;
  }

  ignoreEvent() {
    return false;
  }
}

class ModeHintWidget extends WidgetType {
  constructor(mode) {
    super();
    this.mode = mode;
  }

  eq(other) {
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

export const setTypeHints = StateEffect.define();

const setHoveredTypeHint = StateEffect.define();

export const typeHintState = StateField.define({
  create() {
    return false;
  },
  update(value, tr) {
    for (const effect of tr.effects) {
      if (effect.is(setTypeHints)) return effect.value;
    }
    return value;
  },
});

export const hoveredTypeHintState = StateField.define({
  create() {
    return null;
  },
  update(value, tr) {
    let next = value;
    if (next && tr.docChanged) {
      next = {
        from: tr.changes.mapPos(next.from),
        to: tr.changes.mapPos(next.to),
      };
    }
    for (const effect of tr.effects) {
      if (effect.is(setHoveredTypeHint)) next = effect.value;
    }
    return next && next.from < next.to ? next : null;
  },
  provide(field) {
    return EditorView.decorations.from(field, (hovered) => {
      if (!hovered) return Decoration.none;
      return Decoration.set([
        Decoration.mark({ class: "type-hint-target" }).range(hovered.from, hovered.to),
      ]);
    });
  },
});

export const modeHints = ViewPlugin.fromClass(
  class {
    constructor(view) {
      this.decorations = buildModeHints(view);
    }

    update(update) {
      const typeHintChanged = update.transactions.some((tr) => tr.effects.some((effect) => effect.is(setTypeHints)));
      if (update.docChanged || update.selectionSet || update.viewportChanged || typeHintChanged) {
        this.decorations = buildModeHints(update.view);
      }
    }
  },
  {
    decorations: (plugin) => plugin.decorations,
    eventHandlers: {
      mouseover(event, view) {
        const hint = typeHintTarget(event);
        if (!hint) return false;
        view.dispatch({ effects: setHoveredTypeHint.of(hint) });
        return false;
      },
      mouseout(event, view) {
        if (!typeHintTarget(event)) return false;
        view.dispatch({ effects: setHoveredTypeHint.of(null) });
        return false;
      },
      focusin(event, view) {
        const hint = typeHintTarget(event);
        if (!hint) return false;
        view.dispatch({ effects: setHoveredTypeHint.of(hint) });
        return false;
      },
      focusout(event, view) {
        if (!typeHintTarget(event)) return false;
        view.dispatch({ effects: setHoveredTypeHint.of(null) });
        return false;
      },
    },
  },
);

function typeHintTarget(event) {
  const target = event.target instanceof Element ? event.target.closest(".type-hint") : null;
  if (!target) return null;
  const from = Number(target.dataset.from);
  const to = Number(target.dataset.to);
  if (!Number.isFinite(from) || !Number.isFinite(to) || from >= to) return null;
  return { from, to };
}

function buildModeHints(view) {
  const source = view.state.doc.toString();
  const result = analyze(source);
  const builder = new RangeSetBuilder();
  if (!result.ok) return builder.finish();
  const showTypeHints = view.state.field(typeHintState);
  const decorations = [];

  for (const span of result.spans) {
    if (span.kind !== "distribution" || (span.mode !== "E" && span.mode !== "G")) continue;
    const hint = hintPosition(source, span);
    if (!hint) continue;
    if (cursorAtHintPosition(view, hint.pos)) continue;
    decorations.push({
      pos: hint.pos,
      decoration: Decoration.widget({
        widget: new ModeHintWidget(span.mode),
        side: 1,
      }),
    });
  }

  if (showTypeHints) {
    const seen = new Set();
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

function cursorAtHintPosition(view, pos) {
  return view.state.selection.ranges.some((range) => range.empty && Math.abs(range.head - pos) <= 1);
}

function hintPosition(source, span) {
  const text = source.slice(span.from, span.to);
  const match = text.match(/^\s*([A-Za-z_][A-Za-z0-9_]*)/);
  if (!match) return null;
  const name = match[1];
  if (!distributionNames.has(name)) return null;
  const pos = span.from + match[0].length;
  const afterName = source.slice(pos, span.to).trimStart();
  if (afterName.startsWith("[E]") || afterName.startsWith("[G]")) return null;
  return { pos };
}
