// What each row of the step table shows besides the three states: the part of the source that the
// step reduces, the G draw that both programs share, the E draw that only the source makes, and
// the mean that replaces it in the determinized program. DOM-free, from two consecutive frames.
import type { Expr } from "../core/compiler/ast.ts";
import { prettyExpr } from "../core/compiler/pretty.ts";
import type { Frame } from "../core/runtime/semantics.ts";
import type { Path } from "./trace-expr.ts";
import { changedPath } from "./trace-expr.ts";

/** A range of the source that a step reduces: a node, and the end of its head (`let x = v`,
 * `if c`, `match e`), which is what a reader looks for. */
export interface Reduced {
  from: number;
  to: number;
  headTo: number;
}

export interface Draw {
  /** The variable that the draw binds, if a `let` binds it. */
  name: string | null;
  value: Expr;
  /** The distribution with its parameters, as `uniform(0, 1)`. */
  distribution: string;
}

export interface StepFacts {
  step: number;
  reduced: Reduced | null;
  /** The subexpression of each state that the step produced. */
  focus: { original: Path | null; symbolic: Path | null; determinized: Path | null };
  gDraw: Draw | null;
  eDraw: Draw | null;
  /** The mean call that the determinized program evaluated, and its value. */
  mean: { call: string; value: Expr } | null;
}

const distributions = new Set([
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
]);

/** The subexpression of `expr` at `path`. */
export function nodeAt(expr: Expr, path: Path): Expr | null {
  let node: unknown = expr;
  for (const key of path) {
    if (typeof node !== "object" || node === null) return null;
    node = (node as Record<string | number, unknown>)[key];
  }
  return typeof node === "object" && node !== null && "kind" in node ? (node as Expr) : null;
}

/** The variable that a `let` binds to the subexpression at `path`, if the subexpression is the
 * let's value. */
function boundName(expr: Expr, path: Path) {
  if (path.at(-1) !== "value") return null;
  const parent = nodeAt(expr, path.slice(0, -1));
  return parent?.kind === "Let" ? parent.name : null;
}

function headEnd(node: Expr) {
  switch (node.kind) {
    case "Let":
      return node.value.to;
    case "If":
      return node.cond.to;
    case "Case":
    case "MatchList":
      return node.scrutinee.to;
    default:
      return node.to;
  }
}

function drawAt(previous: Expr, current: Expr, path: Path | null, mode: "E" | "G"): Draw | null {
  if (!path) return null;
  const site = nodeAt(previous, path);
  const value = nodeAt(current, path);
  if (!site || !value || !distributions.has(site.kind) || !("mode" in site)) return null;
  if ((site.mode ?? "G") !== mode) return null;
  return {
    name: boundName(previous, path),
    value,
    distribution: prettyExpr(site).replace(/\[[EG]\]/, ""),
  };
}

export function stepFacts(previous: Frame | null, frame: Frame): StepFacts {
  const focus = {
    original: changedPath(previous?.original, frame.original),
    symbolic: changedPath(previous?.symbolic, frame.symbolic),
    determinized: changedPath(previous?.determinized, frame.determinized),
  };
  if (!previous) {
    return { step: frame.step, reduced: null, focus, gDraw: null, eDraw: null, mean: null };
  }
  const node = focus.original ? nodeAt(previous.original, focus.original) : null;
  const reduced = node ? { from: node.from, to: node.to, headTo: headEnd(node) } : null;
  const meanNode = focus.determinized ? nodeAt(previous.determinized, focus.determinized) : null;
  const meanValue = focus.determinized ? nodeAt(frame.determinized, focus.determinized) : null;
  return {
    step: frame.step,
    reduced,
    focus,
    gDraw: drawAt(previous.original, frame.original, focus.original, "G"),
    eDraw: drawAt(previous.original, frame.original, focus.original, "E"),
    mean:
      meanNode?.kind === "Mean" && meanValue
        ? { call: prettyExpr(meanNode), value: meanValue }
        : null,
  };
}
