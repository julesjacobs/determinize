import { prettyExpr } from "./compiler/pretty.js";

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

export function renderTraceExpr(expr, options = {}) {
  return renderExpr(expr, 0, options.focusPath ?? null, options);
}

export function changedPath(before, after) {
  if (!before || !after || sameExpr(before, after)) return null;
  if (before.kind !== after.kind) return [];
  const child = changedChildPath(before, after);
  return child ?? [];
}

function renderExpr(expr, prec = 0, focusPath = null, options = {}) {
  const focused = focusPath && focusPath.length === 0;
  const wrap = (html, level) => (prec > level ? `(${html})` : html);
  let html;

  switch (expr.kind) {
    case "Var":
      html = renderHighlightedText(expr.name, options);
      break;
    case "Const":
    case "Bool":
    case "Unit":
    case "Nil":
    case "Reject":
    case "SymFloat":
      html = renderHighlightedText(prettyExpr(expr), options);
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
      html = wrap(`fun ${plain(expr.param)} =>\n${indent(renderExpr(expr.body, 0, childFocus(focusPath, "body"), options))}`, 0);
      break;
    case "Rec":
      html = wrap(`rec ${plain(expr.name)} ${plain(expr.param)} =>\n${indent(renderExpr(expr.body, 0, childFocus(focusPath, "body"), options))}`, 0);
      break;
    case "App":
      html = wrap(`${renderExpr(expr.fn, 5, childFocus(focusPath, "fn"), options)} ${renderExpr(expr.arg, 6, childFocus(focusPath, "arg"), options)}`, 5);
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
        html = wrap(`${renderExpr(leftOf(expr), level, childFocus(focusPath, leftKey(expr)), options)} ${plain(op)} ${renderExpr(rightOf(expr), level + (expr.kind === "Cons" ? -1 : 1), childFocus(focusPath, rightKey(expr)), options)}`, level);
        break;
      }
      if (expr.kind in distNames) {
        html = renderDistribution(expr, focusPath, options);
        break;
      }
      html = renderHighlightedText(prettyExpr(expr), options);
  }

  if (expr.kind === "SymFloat") html = valueSpan(html);
  if (focused) html = stepSpan(html);
  return html;
}

export function renderHighlightedText(code, options = {}) {
  const escaped = escapeHtml(code);
  return escaped.replace(
    /\b(let|in|if|then|else|match|with|fun|rec|true|false|fst|snd|inl|inr|observe|domain_error)\b|\b(mean_(?:uniform|gauss|exponential|gamma|beta|bernoulli|poisson|discrete))\b|\b(uniform|gauss|exponential|gamma|beta|flip|bernoulli|poisson|discrete)\b|(\[[EG]\])|\b(v\d+)\b|(-?\d+(?:\.\d+)?(?:e[+-]?\d+)?)/gi,
    (match, keywordMatch, mean, dist, mode, sym, number) => {
      if (keywordMatch) return `<span class="tok-keyword">${match}</span>`;
      if (mean) return `<span class="tok-mean">${match}</span>`;
      if (dist) return `<span class="tok-dist">${match}</span>`;
      if (mode) return `<span class="tok-mode">${match}</span>`;
      if (sym) return corrSpan(match, "tok-sym", sym);
      if (number) return numberSpan(match, options);
      return match;
    },
  );
}

function renderLet(expr, focusPath, options) {
  const value = renderExpr(expr.value, 0, childFocus(focusPath, "value"), options);
  const body = renderExpr(expr.body, 0, childFocus(focusPath, "body"), options);
  if (!prettyExpr(expr.value).includes("\n")) {
    return `${keyword("let")} ${plain(expr.name)} = ${value} ${keyword("in")}\n${body}`;
  }
  return `${keyword("let")} ${plain(expr.name)} =\n${indent(value)}\n${keyword("in")}\n${indent(body)}`;
}

function renderIf(expr, focusPath, options) {
  const cond = renderExpr(expr.cond, 0, childFocus(focusPath, "cond"), options);
  const thenBranch = renderExpr(expr.thenBranch, 0, childFocus(focusPath, "thenBranch"), options);
  const elseBranch = renderExpr(expr.elseBranch, 0, childFocus(focusPath, "elseBranch"), options);
  const plainText = prettyExpr(expr);
  if (!plainText.includes("\n")) {
    return `${keyword("if")} ${cond} ${keyword("then")} ${thenBranch} ${keyword("else")} ${elseBranch}`;
  }
  return `${keyword("if")} ${cond}\n${keyword("then")}\n${indent(thenBranch)}\n${keyword("else")}\n${indent(elseBranch)}`;
}

function renderDistribution(expr, focusPath, options) {
  const name = `<span class="tok-dist">${distNames[expr.kind]}</span>`;
  const mode = expr.mode ? `<span class="tok-mode">[${plain(expr.mode)}]</span>` : "";
  if (expr.kind === "Discrete") {
    return `${name}${mode}(${expr.choices.map((choice) => renderHighlightedText(String(choice.probability), options)).join(", ")})`;
  }
  return `${name}${mode}(${expr.args.map((arg, index) => renderExpr(arg, 0, childFocus(focusPath, "args", index), options)).join(", ")})`;
}

function renderMean(expr, focusPath, options) {
  const name = distNames[expr.distribution] ?? expr.distribution.toLowerCase();
  const args = expr.args.map((arg, index) => renderExpr(arg, 0, childFocus(focusPath, "args", index), options));
  const formula = meanFormula(expr.distribution, expr.args.map((arg) => prettyExpr(arg)));
  return `<span class="mean-form" title="one-step mean redex: ${escapeHtml(formula)}"><span class="tok-mean">mean_${plain(name)}</span>(${args.join(", ")})</span>`;
}

function meanFormula(distribution, args) {
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
    default:
      return `mean(${args.join(", ")})`;
  }
}

function valueSpan(html) {
  return `<span class="trace-value symbolic-value" title="symbolic affine value">${html}</span>`;
}

function stepSpan(html) {
  return `<span class="trace-step" title="result of previous small-step">${html}</span>`;
}

function corrSpan(text, className, symbol) {
  const escaped = escapeHtml(text);
  return `<span class="corr-item ${className}" data-corr="${escapeHtml(symbol)}" title="corresponds to ${escapeHtml(symbol)}">${escaped}</span>`;
}

function numberSpan(text, options) {
  const symbol = symbolForNumber(Number(text), options.valueBySymbol);
  const html = `<span class="tok-number">${text}</span>`;
  const label = options.valueLabel ?? "corresponds to";
  return symbol ? `<span class="corr-item" data-corr="${escapeHtml(symbol)}" title="${escapeHtml(label)} ${escapeHtml(symbol)}">${html}</span>` : html;
}

function symbolForNumber(value, valueBySymbol) {
  if (!Number.isFinite(value) || !valueBySymbol) return null;
  for (const [symbol, target] of Object.entries(valueBySymbol)) {
    if (Number.isFinite(target) && Math.abs(value - target) <= 1e-9) return symbol;
  }
  return null;
}

function changedChildPath(before, after) {
  switch (after.kind) {
    case "Let":
      return changedFieldPath(before, after, "value") ?? changedFieldPath(before, after, "body");
    case "App":
      return changedFieldPath(before, after, "fn") ?? changedFieldPath(before, after, "arg");
    case "Pair":
      return changedFieldPath(before, after, "left") ?? changedFieldPath(before, after, "right");
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return changedFieldPath(before, after, "expr");
    case "Case":
      return changedFieldPath(before, after, "scrutinee") ?? changedFieldPath(before, after, "left") ?? changedFieldPath(before, after, "right");
    case "Cons":
      return changedFieldPath(before, after, "head") ?? changedFieldPath(before, after, "tail");
    case "MatchList":
      return changedFieldPath(before, after, "scrutinee") ?? changedFieldPath(before, after, "nilBranch") ?? changedFieldPath(before, after, "consBranch");
    case "If":
      return changedFieldPath(before, after, "cond") ?? changedFieldPath(before, after, "thenBranch") ?? changedFieldPath(before, after, "elseBranch");
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
    case "Lt":
    case "Leq":
      return changedFieldPath(before, after, "left") ?? changedFieldPath(before, after, "right");
    case "Observe":
      return changedFieldPath(before, after, "cond");
    case "Mean":
      return changedIndexedPath(before, after, "args");
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Flip":
    case "Bernoulli":
    case "Poisson":
      return changedIndexedPath(before, after, "args");
    case "Discrete":
      return null;
    case "DomainError":
      return null;
    default:
      return null;
  }
}

function changedFieldPath(before, after, key) {
  if (!(key in before) || !(key in after) || sameExpr(before[key], after[key])) return null;
  return [key, ...(changedPath(before[key], after[key]) ?? [])];
}

function changedIndexedPath(before, after, key) {
  if (!Array.isArray(before[key]) || !Array.isArray(after[key])) return null;
  const count = Math.min(before[key].length, after[key].length);
  for (let index = 0; index < count; index++) {
    if (!sameExpr(before[key][index], after[key][index])) {
      return [key, index, ...(changedPath(before[key][index], after[key][index]) ?? [])];
    }
  }
  return before[key].length === after[key].length ? null : [];
}

function sameExpr(before, after) {
  return prettyExpr(before) === prettyExpr(after);
}

function childFocus(focusPath, key, index = null) {
  if (!focusPath || focusPath.length === 0 || focusPath[0] !== key) return null;
  if (index === null) return focusPath.slice(1);
  return focusPath[1] === index ? focusPath.slice(2) : null;
}

function keyword(text) {
  return `<span class="tok-keyword">${escapeHtml(text)}</span>`;
}

function plain(text) {
  return escapeHtml(text);
}

function indent(html) {
  return html.split("\n").map((line) => (line ? `  ${line}` : line)).join("\n");
}

function leftOf(expr) {
  return expr.left ?? expr.head;
}

function rightOf(expr) {
  return expr.right ?? expr.tail;
}

function leftKey(expr) {
  return "left" in expr ? "left" : "head";
}

function rightKey(expr) {
  return "right" in expr ? "right" : "tail";
}

function escapeHtml(text) {
  return String(text)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#039;");
}
