// The step table's printing of states: an affine form of many terms is shortened to its first and
// last terms, with the whole form as its title and the rest behind a button.
import assert from "node:assert/strict";
import { test } from "node:test";
import type { Expr } from "../src/core/compiler/ast.ts";
import { renderTraceExpr } from "../src/ui/trace-expr.ts";

function sum(count: number): Expr {
  const terms = Object.fromEntries(Array.from({ length: count }, (_, i) => [`v${i + 1}`, 1]));
  return { kind: "SymFloat", affine: { constant: 3, terms }, from: 0, to: 0 };
}

const text = (html: string) => html.replace(/<[^>]+>/g, "").replace(/\s+/g, " ");

/** `html` without the element that opens with `opening`, nested elements and all. */
function without(html: string, opening: string) {
  const start = html.indexOf(opening);
  assert.ok(start >= 0, opening);
  const tag = /^<(\w+)/.exec(opening)?.[1] ?? "";
  let depth = 0;
  for (const match of html.slice(start).matchAll(new RegExp(`<(/?)${tag}\\b[^>]*>`, "g"))) {
    depth += match[1] ? -1 : 1;
    if (depth === 0)
      return html.slice(0, start) + html.slice(start + match.index + match[0].length);
  }
  throw new Error(`${opening} isn't closed`);
}
const whole = (count: number) =>
  ["3", ...Array.from({ length: count }, (_, i) => `v${i + 1}`)].join(" + ");

test("an affine form of a few terms prints whole", () => {
  assert.equal(text(renderTraceExpr(sum(3))), whole(3));
});

test("an affine form of many terms shows its first and last terms and counts the rest", () => {
  const html = without(renderTraceExpr(sum(20)), "<button");
  const rest = '<span class="affine-rest" hidden>';
  const count = '<span class="tok-more terms-short"';
  assert.equal(text(without(html, rest)), "3 + v1 + … 18 more terms … not shown + v20");
  assert.ok(html.includes(`title="${whole(20)}"`));
  // Shown, the rest completes the form in place.
  assert.equal(text(without(html, count)), whole(20));
});
