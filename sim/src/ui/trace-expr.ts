import type { DistributionKind, Expr, ExprOf, MeanKind } from "../core/compiler/ast.ts";
import { listElements, prettyExpr } from "../core/compiler/pretty.ts";
import { formatNumber } from "../core/format.ts";
import { escapeHtml } from "./html.ts";

/** The fields leading from an expression to a subexpression, with indices into argument lists. */
export type Path = (string | number)[];

export interface TraceOptions {
  /** The subexpression that the last step produced. */
  focusPath?: Path | null;
  /** Values to link to the symbols they correspond to. */
  valueBySymbol?: Record<string, number>;
  valueLabel?: string;
  /** Numbers with about four digits, as the simulator shows them, each with its full value as
   * its title. */
  short?: boolean;
}

/** An expression as a record of its fields, for fields named at run time. */
type Fields = Readonly<Record<string, unknown>>;

/** The operands of an infix node: left and right, or head and tail of a Cons. */
interface Operands {
  kind: string;
  left?: Expr;
  right?: Expr;
  head?: Expr;
  tail?: Expr;
}

const infix: Record<string, [string, number]> = {
  Lt: ["<", 1],
  Leq: ["<=", 1],
  Cons: ["::", 2],
  Add: ["+", 3],
  Sub: ["-", 3],
  Mul: ["*", 4],
  Div: ["/", 4],
};

const distNames: Record<string, string> = {
  Uniform: "uniform",
  Gauss: "gauss",
  Exponential: "exponential",
  Gamma: "gamma",
  Beta: "beta",
  Flip: "flip",
  Bernoulli: "bernoulli",
  Poisson: "poisson",
  Discrete: "discrete",
  DiscreteList: "discrete_list",
};

export function renderTraceExpr(expr: Expr, options: TraceOptions = {}) {
  return renderExpr(expr, 0, options.focusPath ?? null, options);
}

/** The path to the subexpression of `after` that differs from `before`, found in one descent:
 * the node itself if its kind or its own fields changed, or else the first child that changed. */
export function changedPath(
  before: Expr | null | undefined,
  after: Expr | null | undefined,
): Path | null {
  if (!before || !after) return null;
  return diffPath(before, after);
}

function isExpr(value: unknown): value is Expr {
  return typeof value === "object" && value !== null && "kind" in value && "from" in value;
}

function diffPath(before: Expr, after: Expr): Path | null {
  if (before === after) return null;
  if (before.kind !== after.kind) return [];
  const a = before as Fields;
  const b = after as Fields;
  const children: string[] = [];
  for (const key of Object.keys(b)) {
    if (ignoredKeys.has(key)) continue;
    const x = a[key];
    const y = b[key];
    const exprs =
      (isExpr(x) && isExpr(y)) ||
      (Array.isArray(x) && Array.isArray(y) && x.length === y.length && y.every(isExpr));
    if (exprs) children.push(key);
    else if (!sameExpr(x, y)) return [];
  }
  for (const key of children) {
    const x = a[key];
    const y = b[key];
    if (isExpr(x) && isExpr(y)) {
      const path = diffPath(x, y);
      if (path) return [key, ...path];
    } else if (Array.isArray(x) && Array.isArray(y)) {
      for (const [index, item] of y.entries()) {
        const path = diffPath(x[index] as Expr, item as Expr);
        if (path) return [key, index, ...path];
      }
    }
  }
  return null;
}

function renderExpr(
  expr: Expr,
  prec = 0,
  focusPath: Path | null = null,
  options: TraceOptions = {},
): string {
  const focused = focusPath && focusPath.length === 0;
  const wrap = (html: string, level: number) => (prec > level ? `(${html})` : html);
  let html: string;

  switch (expr.kind) {
    case "Var":
      html = renderHighlightedText(expr.name, options);
      break;
    case "Const":
    case "Bool":
    case "Unit":
    case "Nil":
    case "Reject":
      html = renderHighlightedText(prettyExpr(expr), options);
      break;
    case "SymFloat":
      html = renderHighlightedText(prettyExpr(expr).replaceAll("*", " * "), options);
      break;
    case "DomainError":
      html = `<span class="trace-domain-error" title="${escapeHtml(expr.message)}">${renderHighlightedText(prettyExpr(expr), options)}</span>`;
      break;
    case "Let":
      html = renderLet(expr, focusPath, options);
      break;
    case "If":
      html = renderIf(expr, focusPath, options);
      break;
    case "Lam":
      html = wrap(
        `fun ${plain(expr.param)} =>\n${indent(renderExpr(expr.body, 0, childFocus(focusPath, "body"), options))}`,
        0,
      );
      break;
    case "Rec":
      html = wrap(
        `rec ${plain(expr.name)} ${plain(expr.param)} =>\n${indent(renderExpr(expr.body, 0, childFocus(focusPath, "body"), options))}`,
        0,
      );
      break;
    case "App":
      html = wrap(
        `${renderExpr(expr.fn, 5, childFocus(focusPath, "fn"), options)} ${renderExpr(expr.arg, 6, childFocus(focusPath, "arg"), options)}`,
        5,
      );
      break;
    case "Pair":
      html = `(${renderExpr(expr.left, 0, childFocus(focusPath, "left"), options)}, ${renderExpr(expr.right, 0, childFocus(focusPath, "right"), options)})`;
      break;
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
      html = `${keyword(expr.kind.toLowerCase())} ${renderExpr(expr.expr, 6, childFocus(focusPath, "expr"), options)}`;
      break;
    case "Neg":
      html = wrap(`-${renderExpr(expr.expr, 6, childFocus(focusPath, "expr"), options)}`, 6);
      break;
    case "Case":
      html = `${keyword("match")} ${renderExpr(expr.scrutinee, 0, childFocus(focusPath, "scrutinee"), options)} ${keyword("with")} ${keyword("inl")} ${plain(expr.leftName)} =>\n${indent(renderExpr(expr.left, 0, childFocus(focusPath, "left"), options))}\n| ${keyword("inr")} ${plain(expr.rightName)} =>\n${indent(renderExpr(expr.right, 0, childFocus(focusPath, "right"), options))}`;
      break;
    case "MatchList":
      html = `${keyword("match")} ${renderExpr(expr.scrutinee, 0, childFocus(focusPath, "scrutinee"), options)} ${keyword("with")} [] =>\n${indent(renderExpr(expr.nilBranch, 0, childFocus(focusPath, "nilBranch"), options))}\n| ${plain(expr.headName)} :: ${plain(expr.tailName)} =>\n${indent(renderExpr(expr.consBranch, 0, childFocus(focusPath, "consBranch"), options))}`;
      break;
    case "Observe":
      html = `${keyword("observe")}(${renderExpr(expr.cond, 0, childFocus(focusPath, "cond"), options)})`;
      break;
    case "Mean":
      html = renderMean(expr, focusPath, options);
      break;
    default:
      if (expr.kind in infix) {
        const [op, level] = infix[expr.kind];
        html = wrap(
          `${renderExpr(leftOf(expr), level, childFocus(focusPath, leftKey(expr)), options)} ${plain(op)} ${renderExpr(rightOf(expr), level + (expr.kind === "Cons" ? -1 : 1), childFocus(focusPath, rightKey(expr)), options)}`,
          level,
        );
        break;
      }
      if (expr.kind in distNames) {
        html = renderDistribution(expr as ExprOf<DistributionKind>, focusPath, options);
        break;
      }
      html = renderHighlightedText(prettyExpr(expr), options);
  }

  if (expr.kind === "SymFloat") html = valueSpan(html);
  if (focused) html = stepSpan(html);
  return html;
}

export function renderHighlightedText(code: string, options: TraceOptions = {}) {
  const escaped = escapeHtml(code);
  return escaped.replace(
    /\b(let|in|if|then|else|match|with|fun|rec|true|false|fst|snd|inl|inr|observe|domain_error)\b|\b(mean_(?:uniform|gauss|exponential|gamma|beta|bernoulli|poisson|discrete(?:_list)?))\b|\b(uniform|gauss(?:ian)?|exponential|gamma|beta|flip|bernoulli|poisson|discrete(?:_list)?)\b|(\[[EG]\])|\b(v\d+)\b|(-?\d+(?:\.\d+)?(?:e[+-]?\d+)?)/gi,
    (
      match: string,
      keywordMatch: string | undefined,
      mean: string | undefined,
      dist: string | undefined,
      mode: string | undefined,
      sym: string | undefined,
      number: string | undefined,
    ) => {
      if (keywordMatch) return `<span class="tok-keyword">${match}</span>`;
      if (mean) return `<span class="tok-mean">${match}</span>`;
      if (dist) return `<span class="tok-dist">${match}</span>`;
      if (mode) return `<span class="tok-mode tok-mode-${match[1].toLowerCase()}">${match}</span>`;
      if (sym) return corrSpan(match, "tok-sym", sym);
      if (number) return numberSpan(match, options);
      return match;
    },
  );
}

function renderLet(expr: ExprOf<"Let">, focusPath: Path | null, options: TraceOptions) {
  const value = renderExpr(expr.value, 0, childFocus(focusPath, "value"), options);
  const body = renderExpr(expr.body, 0, childFocus(focusPath, "body"), options);
  if (!prettyExpr(expr.value).includes("\n")) {
    return `${keyword("let")} ${plain(expr.name)} = ${value} ${keyword("in")}\n${body}`;
  }
  return `${keyword("let")} ${plain(expr.name)} =\n${indent(value)}\n${keyword("in")}\n${indent(body)}`;
}

function renderIf(expr: ExprOf<"If">, focusPath: Path | null, options: TraceOptions) {
  const cond = renderExpr(expr.cond, 0, childFocus(focusPath, "cond"), options);
  const thenBranch = renderExpr(expr.thenBranch, 0, childFocus(focusPath, "thenBranch"), options);
  const elseBranch = renderExpr(expr.elseBranch, 0, childFocus(focusPath, "elseBranch"), options);
  const plainText = prettyExpr(expr);
  if (!plainText.includes("\n")) {
    return `${keyword("if")} ${cond} ${keyword("then")} ${thenBranch} ${keyword("else")} ${elseBranch}`;
  }
  return `${keyword("if")} ${cond}\n${keyword("then")}\n${indent(thenBranch)}\n${keyword("else")}\n${indent(elseBranch)}`;
}

function renderDistribution(
  expr: ExprOf<DistributionKind>,
  focusPath: Path | null,
  options: TraceOptions,
) {
  const name = `<span class="tok-dist">${distNames[expr.kind]}</span>`;
  const mode = expr.mode
    ? `<span class="tok-mode tok-mode-${expr.mode.toLowerCase()}">[${plain(expr.mode)}]</span>`
    : "";
  if (expr.kind === "Discrete") {
    return `${name}${mode}(${expr.choices.map((choice) => renderHighlightedText(String(choice.probability), options)).join(", ")})`;
  }
  if (expr.kind === "DiscreteList") {
    const probabilities = childFocus(focusPath, "probabilities");
    const elements = expr.form === "remainder" ? listElements(expr.probabilities) : null;
    if (elements) {
      const discrete = '<span class="tok-dist">discrete</span>';
      const rendered = elements.map((element) => renderExpr(element, 0, null, options));
      return `${discrete}${mode}(${[...rendered, "*"].join(", ")})`;
    }
    return `${name}${mode}(${renderExpr(expr.probabilities, 0, probabilities, options)})`;
  }
  return `${name}${mode}(${expr.args.map((arg, index) => renderExpr(arg, 0, childFocus(focusPath, "args", index), options)).join(", ")})`;
}

function renderMean(expr: ExprOf<"Mean">, focusPath: Path | null, options: TraceOptions) {
  const name = distNames[expr.distribution] ?? expr.distribution.toLowerCase();
  const args = expr.args.map((arg, index) =>
    renderExpr(arg, 0, childFocus(focusPath, "args", index), options),
  );
  const formula = meanFormula(
    expr.distribution,
    expr.args.map((arg) => prettyExpr(arg)),
  );
  return `<span class="mean-form" title="one-step mean redex: ${escapeHtml(formula)}"><span class="tok-mean">mean_${plain(name)}</span>(${args.join(", ")})</span>`;
}

function meanFormula(distribution: MeanKind, args: string[]) {
  switch (distribution) {
    case "Uniform":
      return `(${args[0]} + ${args[1]}) * 0.5`;
    case "Gauss":
      return args[0];
    case "Exponential":
      return `1 / ${args[0]}`;
    case "Gamma":
      return `${args[0]} / ${args[1]}`;
    case "Beta":
      return `${args[0]} / (${args[0]} + ${args[1]})`;
    case "Bernoulli":
    case "Poisson":
      return args[0];
    case "Discrete":
      return args.map((probability, index) => `${index} * ${probability}`).join(" + ") || "0";
    case "DiscreteList":
      return `n + Σ (i - n) * p_i over the list ${args[0]} of length n`;
    default:
      return `mean(${args.join(", ")})`;
  }
}

function valueSpan(html: string) {
  return `<span class="trace-value symbolic-value" title="symbolic affine value">${html}</span>`;
}

function stepSpan(html: string) {
  return `<span class="trace-step" title="result of previous small-step">${html}</span>`;
}

function corrSpan(text: string, className: string, symbol: string) {
  const escaped = escapeHtml(text);
  return `<span class="corr-item ${className}" data-corr="${escapeHtml(symbol)}" title="corresponds to ${escapeHtml(symbol)}">${escaped}</span>`;
}

function numberSpan(text: string, options: TraceOptions) {
  const symbol = symbolForNumber(Number(text), options.valueBySymbol);
  const shown = options.short ? formatNumber(Number(text)) : text;
  const html =
    shown === text
      ? `<span class="tok-number">${text}</span>`
      : `<span class="tok-number" title="${text}">${escapeHtml(shown)}</span>`;
  const label = options.valueLabel ?? "corresponds to";
  return symbol
    ? `<span class="corr-item" data-corr="${escapeHtml(symbol)}" title="${escapeHtml(label)} ${escapeHtml(symbol)}">${html}</span>`
    : html;
}

function symbolForNumber(value: number, valueBySymbol: Record<string, number> | undefined) {
  if (!Number.isFinite(value) || !valueBySymbol) return null;
  for (const [symbol, target] of Object.entries(valueBySymbol)) {
    if (Number.isFinite(target) && Math.abs(value - target) <= 1e-9) return symbol;
  }
  return null;
}

/** Whether two states print alike: equal apart from their spans and rounding bounds. Unchanged
 * parts of consecutive states are mostly the same objects. */
function sameExpr(before: unknown, after: unknown): boolean {
  if (before === after) return true;
  if (typeof before !== "object" || typeof after !== "object" || !before || !after) return false;
  if (Array.isArray(before) || Array.isArray(after)) {
    return (
      Array.isArray(before) &&
      Array.isArray(after) &&
      before.length === after.length &&
      before.every((item, index) => sameExpr(item, after[index]))
    );
  }
  const keys = (value: object) =>
    Object.keys(value).filter(
      (key) => !ignoredKeys.has(key) && (value as Fields)[key] !== undefined,
    );
  const beforeKeys = keys(before);
  const afterKeys = keys(after);
  return (
    beforeKeys.length === afterKeys.length &&
    beforeKeys.every((key) => sameExpr((before as Fields)[key], (after as Fields)[key]))
  );
}

/** The fields that the printed form of a state doesn't show. */
const ignoredKeys = new Set(["from", "to", "error", "errors"]);

function childFocus(focusPath: Path | null, key: string, index: number | null = null) {
  if (!focusPath || focusPath.length === 0 || focusPath[0] !== key) return null;
  if (index === null) return focusPath.slice(1);
  return focusPath[1] === index ? focusPath.slice(2) : null;
}

function keyword(text: string) {
  return `<span class="tok-keyword">${escapeHtml(text)}</span>`;
}

function plain(text: string) {
  return escapeHtml(text);
}

function indent(html: string) {
  return html
    .split("\n")
    .map((line) => (line ? `  ${line}` : line))
    .join("\n");
}

function leftOf(expr: Operands) {
  return (expr.left ?? expr.head) as Expr;
}

function rightOf(expr: Operands) {
  return (expr.right ?? expr.tail) as Expr;
}

function leftKey(expr: Operands) {
  return "left" in expr ? "left" : "head";
}

function rightKey(expr: Operands) {
  return "right" in expr ? "right" : "tail";
}
