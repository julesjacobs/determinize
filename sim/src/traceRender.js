import { prettyExpr } from "./compiler/pretty.js";
import { isValue } from "./runtime/semantics.js";

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
  return renderExpr(expr, 0, stepPath(expr), options);
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
    case "SymFloat":
      html = renderHighlightedText(prettyExpr(expr), options);
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
    /\b(let|in|if|then|else|match|with|fun|rec|true|false|fst|snd|inl|inr|observe)\b|\b(uniform|gauss|exponential|gamma|beta|flip|bernoulli|poisson|discrete)\b|(\[[EG]\])|\b(v\d+)\b|(-?\d+(?:\.\d+)?(?:e[+-]?\d+)?)/gi,
    (match, keywordMatch, dist, mode, sym, number) => {
      if (keywordMatch) return `<span class="tok-keyword">${match}</span>`;
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

function valueSpan(html) {
  return `<span class="trace-value symbolic-value" title="symbolic affine value">${html}</span>`;
}

function stepSpan(html) {
  return `<span class="trace-step" title="next small-step reduction">${html}</span>`;
}

function corrSpan(text, className, symbol) {
  const escaped = escapeHtml(text);
  return `<span class="corr-item ${className}" data-corr="${escapeHtml(symbol)}" title="corresponds to ${escapeHtml(symbol)}">${escaped}</span>`;
}

function numberSpan(text, options) {
  const symbol = options.highlightMeans ? meanSymbolForNumber(Number(text), options.meanBySymbol) : null;
  const html = `<span class="tok-number">${text}</span>`;
  return symbol ? `<span class="corr-item" data-corr="${escapeHtml(symbol)}" title="mean substituted for ${escapeHtml(symbol)}">${html}</span>` : html;
}

function meanSymbolForNumber(value, meanBySymbol) {
  if (!Number.isFinite(value) || !meanBySymbol) return null;
  for (const [symbol, mean] of Object.entries(meanBySymbol)) {
    if (Number.isFinite(mean) && Math.abs(value - mean) <= 1e-9) return symbol;
  }
  return null;
}

function stepPath(expr) {
  if (isValue(expr)) return null;
  switch (expr.kind) {
    case "Let":
      return isValue(expr.value) ? [] : prepend("value", stepPath(expr.value));
    case "App":
      if (!isValue(expr.fn)) return prepend("fn", stepPath(expr.fn));
      if (!isValue(expr.arg)) return prepend("arg", stepPath(expr.arg));
      return [];
    case "Pair":
      if (!isValue(expr.left)) return prepend("left", stepPath(expr.left));
      if (!isValue(expr.right)) return prepend("right", stepPath(expr.right));
      return null;
    case "Fst":
    case "Snd":
      return isValue(expr.expr) ? [] : prepend("expr", stepPath(expr.expr));
    case "Inl":
    case "Inr":
      return isValue(expr.expr) ? null : prepend("expr", stepPath(expr.expr));
    case "Case":
      return isValue(expr.scrutinee) ? [] : prepend("scrutinee", stepPath(expr.scrutinee));
    case "Cons":
      if (!isValue(expr.head)) return prepend("head", stepPath(expr.head));
      if (!isValue(expr.tail)) return prepend("tail", stepPath(expr.tail));
      return null;
    case "MatchList":
      return isValue(expr.scrutinee) ? [] : prepend("scrutinee", stepPath(expr.scrutinee));
    case "If":
      return isValue(expr.cond) ? [] : prepend("cond", stepPath(expr.cond));
    case "Neg":
      return isValue(expr.expr) ? [] : prepend("expr", stepPath(expr.expr));
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
    case "Lt":
    case "Leq":
      if (!isValue(expr.left)) return prepend("left", stepPath(expr.left));
      if (!isValue(expr.right)) return prepend("right", stepPath(expr.right));
      return [];
    case "Observe":
      return isValue(expr.cond) ? [] : prepend("cond", stepPath(expr.cond));
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Flip":
    case "Bernoulli":
    case "Poisson":
      for (let i = 0; i < expr.args.length; i++) {
        if (!isValue(expr.args[i])) return prepend("args", prepend(i, stepPath(expr.args[i])));
      }
      return [];
    case "Discrete":
      return [];
    default:
      return [];
  }
}

function prepend(part, rest) {
  return [part, ...(rest ?? [])];
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
