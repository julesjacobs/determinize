// Markup and text that the views share.

/** The label of the run of a program that Lean rejects only for a mode conflict. */
export const counterexampleLabel =
  "Lean rejects this program; this is what replacing its [E] draws anyway does";

export function escapeHtml(text: string | undefined) {
  return String(text)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}
