// The look of both program panes, from the site's tokens: code on the editor fill, keywords
// SemiBold and no other syntax colour, E marks and mean calls in the change colour, G marks in the
// given colour, and Lean's diagnostics in the alert colour. CodeMirror's own styles are not in a
// cascade layer, so the panes are styled here rather than in styles.css.
import { EditorView } from "@codemirror/view";

export const editorTheme = EditorView.theme({
  "&": {
    backgroundColor: "var(--code-fill)",
    color: "var(--ink)",
    borderRadius: "3px",
    fontSize: "var(--size-code)",
    // The panes' shared height (styles.css); the text scrolls inside.
    height: "var(--pane-height)",
  },
  "&.cm-focused": { outline: "2px solid var(--muted)", outlineOffset: "2px" },
  ".cm-scroller": {
    overflow: "auto",
    fontFamily: "var(--font-code)",
    lineHeight: "var(--leading-code)",
  },
  ".cm-content": { padding: "var(--space-3) 0", caretColor: "var(--ink)" },
  ".cm-line": { padding: "0 var(--space-4) 0 var(--space-1)" },
  ".cm-gutters": { backgroundColor: "transparent", color: "var(--muted)", border: "none" },
  ".cm-lineNumbers .cm-gutterElement": { minWidth: "2.4em", padding: "0 0.5em 0 0.6em" },
  ".cm-cursor, .cm-dropCursor": { borderLeftColor: "var(--ink)" },
  "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection":
    { backgroundColor: "color-mix(in oklab, var(--given) 24%, transparent)" },
  ".cm-placeholder": { color: "var(--muted)" },
  // The lines of the span that the step table's current or hovered step reduces.
  ".cm-linked": { backgroundColor: "var(--highlight)" },
  // Modes: E SemiBold with a solid underline, G Regular with a dotted one. Inferred modes are
  // hints where the annotation would be written, smaller and not selectable.
  ".cm-mode-e": {
    color: "var(--change)",
    fontWeight: "600",
    textDecoration: "underline solid 1px",
    textUnderlineOffset: "0.2em",
  },
  ".cm-mode-g": {
    color: "var(--given)",
    textDecoration: "underline dotted 1.5px",
    textUnderlineOffset: "0.2em",
  },
  ".cm-mode-hint": { fontSize: "0.86em", userSelect: "none" },
  ".cm-mean": { color: "var(--change)", fontWeight: "600" },
  ".cm-tooltip": {
    maxWidth: "min(28rem, 90vw)",
    border: "1px solid var(--rule)",
    borderRadius: "3px",
    backgroundColor: "var(--ground)",
    color: "var(--ink)",
    fontFamily: "var(--font-text)",
    fontSize: "var(--size-small)",
    lineHeight: "var(--leading-ui)",
  },
  ".cm-tooltip .cm-hover": { display: "block", padding: "var(--space-2) var(--space-3)" },
  ".cm-hover-type": { display: "block", color: "var(--muted)" },
  ".cm-lintRange-error": {
    backgroundImage: "none",
    textDecoration: "underline wavy var(--alert) 1px",
    textUnderlineOffset: "0.25em",
  },
  ".cm-lintPoint:after": { borderBottomColor: "var(--alert)" },
  ".cm-gutter-lint": { width: "0.5em" },
  ".cm-gutter-lint .cm-gutterElement": { padding: "0" },
  ".cm-lint-marker": { width: "3px", height: "calc(var(--leading-code) * 1em)" },
  ".cm-lint-marker-error": { content: "normal", backgroundColor: "var(--alert)" },
  ".cm-diagnostic-error": { borderLeft: "3px solid var(--alert)" },
  ".cm-diagnostic": { padding: "var(--space-2) var(--space-3)" },
});
