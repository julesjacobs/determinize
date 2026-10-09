// The step table's printing of states: an affine form of many terms is shortened to its first and
// last terms, with the whole form as its title and the rest behind a button; a number links to a
// symbol only where it stands for the symbol.
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

const number = (value: number): Expr => ({ kind: "Const", value, from: 0, to: 0 });
const lone = (symbol: string, coefficient = 1): Expr => ({
  kind: "SymFloat",
  affine: { constant: 0, terms: { [symbol]: coefficient } },
  from: 0,
  to: 0,
});
const product = (left: Expr, right: Expr): Expr => ({ kind: "Mul", left, right, from: 0, to: 0 });
const linked = (html: string) => [...html.matchAll(/data-corr="(\w+)" title="([^"]*)"/g)];

test("only a number that stands where the symbolic state holds a symbol links to it", () => {
  // The noisy product's determinized program after its E draw: x's G draw and v1's mean are equal.
  const html = renderTraceExpr(product(number(0.5666), number(0.5666)), {
    counterpart: product(number(0.5666), lone("v1")),
    valueLabel: "mean substituted for",
  });
  assert.deepEqual(
    linked(html).map((match) => [match[1], match[2]]),
    [["v1", "mean substituted for v1"]],
  );
  assert.ok(html.indexOf("data-corr") > html.indexOf("tok-op"));
});

test("a number computed from a symbol doesn't link to it", () => {
  assert.deepEqual(linked(renderTraceExpr(number(0), { counterpart: lone("v1", 200) })), []);
  assert.deepEqual(linked(renderTraceExpr(number(0), { counterpart: number(0) })), []);
});

test("a mean call's argument links to the symbol of the draw it stands for", () => {
  const draw: Expr = { kind: "Gauss", mode: "E", args: [lone("v1"), number(1)], from: 0, to: 0 };
  const mean: Expr = {
    kind: "Mean",
    distribution: "Gauss",
    args: [number(0.5), number(1)],
    from: 0,
    to: 0,
  };
  assert.deepEqual(
    linked(renderTraceExpr(mean, { counterpart: draw })).map((match) => match[1]),
    ["v1"],
  );
});
