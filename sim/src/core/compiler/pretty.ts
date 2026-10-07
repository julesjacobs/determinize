import type { Affine } from "../runtime/affine.ts";
import type { DistributionKind, Expr, ExprOf } from "./ast.ts";

/** The operands of an infix node: left and right, or head and tail of a Cons. */
interface Operands<E> {
  kind: string;
  left?: E;
  right?: E;
  head?: E;
  tail?: E;
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
  DiscreteWeights: "discrete",
  DiscreteList: "discrete_list",
};

export function prettyExpr(expr: Expr, prec = 0): string {
  const wrap = (s: string, level: number) => (prec > level ? `(${s})` : s);
  switch (expr.kind) {
    case "Var":
      return expr.name;
    case "Const":
      return formatNumber(expr.value);
    case "SymFloat":
      return prettyAffine(expr.affine);
    case "Bool":
      return expr.value ? "true" : "false";
    case "Unit":
      return "()";
    case "Reject":
      return "reject";
    case "DomainError":
      return `domain_error(${domainErrorSummary(expr)})`;
    case "Mean":
      return prettyMean(expr);
    case "Nil":
      return "[]";
    case "Lam":
      return wrap(`fun ${expr.param} =>\n${indent(prettyExpr(expr.body))}`, 0);
    case "Rec":
      return wrap(`rec ${expr.name} ${expr.param} =>\n${indent(prettyExpr(expr.body))}`, 0);
    case "Let":
      return prettyLet(expr);
    case "If":
      return prettyIf(expr);
    case "App":
      return wrap(`${prettyExpr(expr.fn, 5)} ${prettyExpr(expr.arg, 6)}`, 5);
    case "Pair":
      return `(${prettyExpr(expr.left)}, ${prettyExpr(expr.right)})`;
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
      return `${expr.kind.toLowerCase()} ${prettyExpr(expr.expr, 6)}`;
    case "Neg":
      return wrap(`-${prettyExpr(expr.expr, 6)}`, 6);
    case "Case":
      return `match ${prettyExpr(expr.scrutinee)} with inl ${expr.leftName} =>\n${indent(prettyExpr(expr.left))}\n| inr ${expr.rightName} =>\n${indent(prettyExpr(expr.right))}`;
    case "MatchList":
      return `match ${prettyExpr(expr.scrutinee)} with [] =>\n${indent(prettyExpr(expr.nilBranch))}\n| ${expr.headName} :: ${expr.tailName} =>\n${indent(prettyExpr(expr.consBranch))}`;
    case "Observe":
      return `observe(${prettyExpr(expr.cond)})`;
    default:
      if (expr.kind in infix) {
        const [op, level] = infix[expr.kind];
        return wrap(
          `${prettyExpr(leftOf(expr), level)} ${op} ${prettyExpr(rightOf(expr), level + (expr.kind === "Cons" ? -1 : 1))}`,
          level,
        );
      }
      if (expr.kind in distNames)
        return prettyDistribution(expr as ExprOf<DistributionKind | "DiscreteWeights">);
      return `<${expr.kind}>`;
  }
}

function prettyDistribution(expr: ExprOf<DistributionKind | "DiscreteWeights">) {
  const name = distNames[expr.kind];
  const mode = expr.mode ? `[${expr.mode}]` : "";
  if (expr.kind === "Discrete")
    return `${name}${mode}(${expr.choices.map((c) => formatNumber(c.probability)).join(", ")})`;
  if (expr.kind === "DiscreteWeights")
    return `${name}${mode}(${expr.weights.map((weight) => prettyExpr(weight)).join(", ")})`;
  if (expr.kind === "DiscreteList") {
    const elements = expr.form === "remainder" ? listElements(expr.probabilities) : null;
    if (elements)
      return `discrete${mode}(${[...elements.map((e) => prettyExpr(e)), "*"].join(", ")})`;
    return `${name}${mode}(${prettyExpr(expr.probabilities)})`;
  }
  return `${name}${mode}(${expr.args.map((arg) => prettyExpr(arg)).join(", ")})`;
}

/** The elements of a list built from `::` and `[]`, or null. */
export function listElements(expr: Expr): Expr[] | null {
  const elements: Expr[] = [];
  let rest = expr;
  while (rest.kind === "Cons") {
    elements.push(rest.head);
    rest = rest.tail;
  }
  return rest.kind === "Nil" ? elements : null;
}

function prettyMean(expr: ExprOf<"Mean">) {
  const name = distNames[expr.distribution] ?? expr.distribution.toLowerCase();
  return `mean_${name}(${expr.args.map((arg) => prettyExpr(arg)).join(", ")})`;
}

function leftOf<E>(expr: Operands<E>): E {
  return (expr.left ?? expr.head) as E;
}

function rightOf<E>(expr: Operands<E>): E {
  return (expr.right ?? expr.tail) as E;
}

function prettyLet(expr: ExprOf<"Let">) {
  const value = prettyExpr(expr.value);
  const body = prettyExpr(expr.body);
  if (!hasLineBreak(value)) {
    return `let ${expr.name} = ${value} in\n${body}`;
  }
  return `let ${expr.name} =\n${indent(value)}\nin\n${indent(body)}`;
}

function prettyIf(expr: ExprOf<"If">) {
  const cond = prettyExpr(expr.cond);
  const thenBranch = prettyExpr(expr.thenBranch);
  const elseBranch = prettyExpr(expr.elseBranch);
  if (
    !hasLineBreak(cond) &&
    !hasLineBreak(thenBranch) &&
    !hasLineBreak(elseBranch) &&
    lineLength(`if ${cond} then ${thenBranch} else ${elseBranch}`) <= 80
  ) {
    return `if ${cond} then ${thenBranch} else ${elseBranch}`;
  }
  return `if ${cond}\nthen\n${indent(thenBranch)}\nelse\n${indent(elseBranch)}`;
}

function hasLineBreak(text: string) {
  return text.includes("\n");
}

function lineLength(text: string) {
  return Math.max(...text.split("\n").map((line) => line.length));
}

function indent(text: string) {
  return text
    .split("\n")
    .map((line) => (line ? `  ${line}` : line))
    .join("\n");
}

function formatNumber(value: number) {
  if (Object.is(value, -0)) return "0";
  return Number.isInteger(value) ? String(value) : String(value);
}

function domainErrorSummary(expr: ExprOf<"DomainError">) {
  const distribution = expr.distribution
    ? `${distNames[expr.distribution] ?? expr.distribution.toLowerCase()}: `
    : "";
  return `${distribution}${expr.reason ?? expr.message}`;
}

function prettyAffine(affine: Affine) {
  const terms: string[] = [];
  if ((affine.constant ?? 0) !== 0 || Object.keys(affine.terms ?? {}).length === 0) {
    terms.push(formatNumber(affine.constant ?? 0));
  }
  for (const [name, coeff] of Object.entries(affine.terms ?? {})) {
    if (coeff === 0) continue;
    if (coeff === 1) terms.push(name);
    else if (coeff === -1) terms.push(`-${name}`);
    else terms.push(`${formatNumber(coeff)}*${name}`);
  }
  return terms.join(" + ").replace(/\+ -/g, "- ");
}
