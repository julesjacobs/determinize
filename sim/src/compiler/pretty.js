import { formatType, zonk } from "./types.js";

const infix = {
  Lt: ["<", 1],
  Leq: ["<=", 1],
  Cons: ["::", 2],
  Add: ["+", 3],
  Sub: ["-", 3],
  Mul: ["*", 4],
  Div: ["/", 4],
};

const distNames = {
  Uniform: "uniform",
  Gauss: "gauss",
  Exponential: "exponential",
  Gamma: "gamma",
  Beta: "beta",
  Flip: "flip",
  Bernoulli: "bernoulli",
  Poisson: "poisson",
  Discrete: "discrete",
};

export function prettyExpr(expr, prec = 0) {
  const wrap = (s, level) => (prec > level ? `(${s})` : s);
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
        return wrap(`${prettyExpr(leftOf(expr), level)} ${op} ${prettyExpr(rightOf(expr), level + (expr.kind === "Cons" ? -1 : 1))}`, level);
      }
      if (expr.kind in distNames) return prettyDistribution(expr);
      return `<${expr.kind}>`;
  }
}

export function prettyTyped(te, prec = 0) {
  const typeText = formatType(te.typ);
  const withType = (body, level = 0) => (prec > level ? `(${body} : ${typeText})` : `${body} : ${typeText}`);
  switch (te.kind) {
    case "Var":
      return withType(te.name, 6);
    case "Const":
      return withType(formatNumber(te.value), 6);
    case "Bool":
      return withType(te.value ? "true" : "false", 6);
    case "Unit":
      return withType("()", 6);
    case "Nil":
      return withType("[]", 6);
    case "Let":
      return `let ${te.name} : ${formatType(te.value.typ)} =\n${indent(prettyTyped(te.value))}\nin\n${indent(prettyTyped(te.body))}\n: ${typeText}`;
    case "Lam":
      return withType(`fun ${te.param} =>\n${indent(prettyTyped(te.body))}`);
    case "Rec":
      return withType(`rec ${te.name} ${te.param} =>\n${indent(prettyTyped(te.body))}`);
    case "App":
      return withType(`${prettyTyped(te.fn, 5)} ${prettyTyped(te.arg, 6)}`, 5);
    case "Pair":
      return withType(`(${prettyTyped(te.left)}, ${prettyTyped(te.right)})`, 6);
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
      return withType(`${te.kind.toLowerCase()} ${prettyTyped(te.expr, 6)}`, 6);
    case "Neg":
      return withType(`-${prettyTyped(te.expr, 6)}`, 6);
    case "If":
      return `if ${prettyTyped(te.cond)}\nthen\n${indent(prettyTyped(te.thenBranch))}\nelse\n${indent(prettyTyped(te.elseBranch))}\n: ${typeText}`;
    case "Case":
      return `match ${prettyTyped(te.scrutinee)} with inl ${te.leftName} =>\n${indent(prettyTyped(te.left))}\n| inr ${te.rightName} =>\n${indent(prettyTyped(te.right))}\n: ${typeText}`;
    case "MatchList":
      return `match ${prettyTyped(te.scrutinee)} with [] =>\n${indent(prettyTyped(te.nilBranch))}\n| ${te.headName} :: ${te.tailName} =>\n${indent(prettyTyped(te.consBranch))}\n: ${typeText}`;
    case "Observe":
      return withType(`observe(${prettyTyped(te.cond)})`, 6);
    default:
      if (te.kind in infix) {
        const [op, level] = infix[te.kind];
        return withType(`${prettyTyped(leftOf(te), level)} ${op} ${prettyTyped(rightOf(te), level + 1)}`, level);
      }
      if (te.kind in distNames) return withType(prettyTypedDistribution(te), 6);
      return withType(`<${te.kind}>`);
  }
}

function prettyDistribution(expr) {
  const name = distNames[expr.kind];
  const mode = expr.mode ? `[${expr.mode}]` : "";
  if (expr.kind === "Discrete") return `${name}${mode}(${expr.choices.map((c) => formatNumber(c.probability)).join(", ")})`;
  return `${name}${mode}(${expr.args.map((arg) => prettyExpr(arg)).join(", ")})`;
}

function prettyMean(expr) {
  const name = distNames[expr.distribution] ?? expr.distribution.toLowerCase();
  return `mean_${name}(${expr.args.map((arg) => prettyExpr(arg)).join(", ")})`;
}

function prettyTypedDistribution(te) {
  const name = distNames[te.kind];
  const ty = zonk(te.typ);
  const derivedMode = ty.tag === "Float" ? ty.mode.mode : null;
  const mode = te.mode ?? derivedMode;
  const modeText = mode ? `[${mode}]` : "";
  if (te.kind === "Discrete") return `${name}${modeText}(${te.choices.map((c) => formatNumber(c.probability)).join(", ")})`;
  return `${name}${modeText}(${te.args.map((arg) => prettyTyped(arg)).join(", ")})`;
}

function leftOf(expr) {
  return expr.left ?? expr.head;
}

function rightOf(expr) {
  return expr.right ?? expr.tail;
}

function prettyLet(expr) {
  const value = prettyExpr(expr.value);
  const body = prettyExpr(expr.body);
  if (!hasLineBreak(value)) {
    return `let ${expr.name} = ${value} in\n${body}`;
  }
  return `let ${expr.name} =\n${indent(value)}\nin\n${indent(body)}`;
}

function prettyIf(expr) {
  const cond = prettyExpr(expr.cond);
  const thenBranch = prettyExpr(expr.thenBranch);
  const elseBranch = prettyExpr(expr.elseBranch);
  if (!hasLineBreak(cond) && !hasLineBreak(thenBranch) && !hasLineBreak(elseBranch) && lineLength(`if ${cond} then ${thenBranch} else ${elseBranch}`) <= 80) {
    return `if ${cond} then ${thenBranch} else ${elseBranch}`;
  }
  return `if ${cond}\nthen\n${indent(thenBranch)}\nelse\n${indent(elseBranch)}`;
}

function hasLineBreak(text) {
  return text.includes("\n");
}

function lineLength(text) {
  return Math.max(...text.split("\n").map((line) => line.length));
}

function indent(text) {
  return text.split("\n").map((line) => (line ? `  ${line}` : line)).join("\n");
}

function formatNumber(value) {
  if (Object.is(value, -0)) return "0";
  return Number.isInteger(value) ? String(value) : String(value);
}

function domainErrorSummary(expr) {
  const distribution = expr.distribution ? `${distNames[expr.distribution] ?? expr.distribution.toLowerCase()}: ` : "";
  return `${distribution}${expr.reason ?? expr.message}`;
}

function prettyAffine(affine) {
  const terms = [];
  if (Math.abs(affine.constant ?? 0) > 1e-12 || Object.keys(affine.terms ?? {}).length === 0) {
    terms.push(formatNumber(affine.constant ?? 0));
  }
  for (const [name, coeff] of Object.entries(affine.terms ?? {})) {
    if (Math.abs(coeff) <= 1e-12) continue;
    if (coeff === 1) terms.push(name);
    else if (coeff === -1) terms.push(`-${name}`);
    else terms.push(`${formatNumber(coeff)}*${name}`);
  }
  return terms.join(" + ").replace(/\+ -/g, "- ");
}
