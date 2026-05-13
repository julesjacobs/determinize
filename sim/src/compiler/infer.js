import { CompileError } from "./errors.js";
import {
  TArrow,
  TBool,
  TFloat,
  TList,
  TMeta,
  TPair,
  TSum,
  TUnit,
  assertSubtype,
  defaultModesType,
  ensureFloat,
  formatType,
  freshFloat,
  freshMeta,
  freshModeMeta,
  resetTypeState,
  setMode,
  zonk,
} from "./types.js";

function typed(expr, typ, extra = {}) {
  return { kind: expr.kind, typ, from: expr.from, to: expr.to, ...extra };
}

function lookup(env, name, source) {
  if (!env.has(name)) throw new CompileError(`unbound variable \`${name}\``, source.from, source.to);
  return env.get(name);
}

function extend(env, entries) {
  const next = new Map(env);
  for (const [name, typ] of entries) next.set(name, typ);
  return next;
}

function forceAnnotatedMode(expr, typ) {
  if (!expr.mode) return;
  const floatTy = ensureFloat(typ, expr);
  setMode(floatTy.mode, expr.mode, expr);
}

function floatG() {
  const mode = freshModeMeta();
  setMode(mode, "G");
  return TFloat(mode);
}

export function inferProgram(expr) {
  resetTypeState();
  return infer(new Map(), expr, TMeta(freshMeta()));
}

export function infer(env, expr, expected) {
  switch (expr.kind) {
    case "Var": {
      const tyVar = lookup(env, expr.name, expr);
      assertSubtype(tyVar, expected, expr);
      return typed(expr, expected, { name: expr.name });
    }
    case "Lam": {
      const dom = TMeta(freshMeta());
      const cod = TMeta(freshMeta());
      const body = infer(extend(env, [[expr.param, dom]]), expr.body, cod);
      const lamTy = TArrow(dom, cod);
      assertSubtype(lamTy, expected, expr);
      return typed(expr, lamTy, { param: expr.param, body });
    }
    case "Rec": {
      const dom = TMeta(freshMeta());
      const cod = TMeta(freshMeta());
      const fnTy = TArrow(dom, cod);
      const body = infer(extend(env, [[expr.name, fnTy], [expr.param, dom]]), expr.body, cod);
      assertSubtype(body.typ, cod, expr.body);
      assertSubtype(fnTy, expected, expr);
      return typed(expr, fnTy, { name: expr.name, param: expr.param, body });
    }
    case "App": {
      const argTy = TMeta(freshMeta());
      const resTy = TMeta(freshMeta());
      const fnTy = TArrow(argTy, resTy);
      const fn = infer(env, expr.fn, fnTy);
      const arg = infer(env, expr.arg, argTy);
      assertSubtype(resTy, expected, expr);
      return typed(expr, resTy, { fn, arg });
    }
    case "Unit":
      assertSubtype(TUnit, expected, expr);
      return typed(expr, TUnit);
    case "Nil": {
      const elem = TMeta(freshMeta());
      const listTy = TList(elem);
      assertSubtype(listTy, expected, expr);
      return typed(expr, listTy);
    }
    case "Cons": {
      const elem = TMeta(freshMeta());
      const listTy = TList(elem);
      const head = infer(env, expr.head, elem);
      const tail = infer(env, expr.tail, listTy);
      assertSubtype(listTy, expected, expr);
      return typed(expr, listTy, { head, tail });
    }
    case "Pair": {
      const leftTy = TMeta(freshMeta());
      const rightTy = TMeta(freshMeta());
      const left = infer(env, expr.left, leftTy);
      const right = infer(env, expr.right, rightTy);
      const pairTy = TPair(left.typ, right.typ);
      assertSubtype(pairTy, expected, expr);
      return typed(expr, pairTy, { left, right });
    }
    case "Fst": {
      const a = TMeta(freshMeta());
      const b = TMeta(freshMeta());
      const exprTyped = infer(env, expr.expr, TPair(a, b));
      assertSubtype(a, expected, expr);
      return typed(expr, a, { expr: exprTyped });
    }
    case "Snd": {
      const a = TMeta(freshMeta());
      const b = TMeta(freshMeta());
      const exprTyped = infer(env, expr.expr, TPair(a, b));
      assertSubtype(b, expected, expr);
      return typed(expr, b, { expr: exprTyped });
    }
    case "Inl": {
      const leftTy = TMeta(freshMeta());
      const rightTy = TMeta(freshMeta());
      const value = infer(env, expr.expr, leftTy);
      const sumTy = TSum(value.typ, rightTy);
      assertSubtype(sumTy, expected, expr);
      return typed(expr, sumTy, { expr: value });
    }
    case "Inr": {
      const leftTy = TMeta(freshMeta());
      const rightTy = TMeta(freshMeta());
      const value = infer(env, expr.expr, rightTy);
      const sumTy = TSum(leftTy, value.typ);
      assertSubtype(sumTy, expected, expr);
      return typed(expr, sumTy, { expr: value });
    }
    case "Case": {
      const leftTy = TMeta(freshMeta());
      const rightTy = TMeta(freshMeta());
      const scrutinee = infer(env, expr.scrutinee, TSum(leftTy, rightTy));
      const left = infer(extend(env, [[expr.leftName, leftTy]]), expr.left, expected);
      const right = infer(extend(env, [[expr.rightName, rightTy]]), expr.right, expected);
      return typed(expr, expected, {
        scrutinee,
        leftName: expr.leftName,
        left,
        rightName: expr.rightName,
        right,
      });
    }
    case "MatchList": {
      const elemTy = TMeta(freshMeta());
      const listTy = TList(elemTy);
      const scrutinee = infer(env, expr.scrutinee, listTy);
      const nilBranch = infer(env, expr.nilBranch, expected);
      const consBranch = infer(extend(env, [[expr.headName, elemTy], [expr.tailName, listTy]]), expr.consBranch, expected);
      return typed(expr, expected, {
        scrutinee,
        nilBranch,
        headName: expr.headName,
        tailName: expr.tailName,
        consBranch,
      });
    }
    case "Bool":
      assertSubtype(TBool, expected, expr);
      return typed(expr, TBool, { value: expr.value });
    case "If": {
      const cond = infer(env, expr.cond, TBool);
      const thenBranch = infer(env, expr.thenBranch, expected);
      const elseBranch = infer(env, expr.elseBranch, expected);
      return typed(expr, expected, { cond, thenBranch, elseBranch });
    }
    case "Let": {
      const valueTy = TMeta(freshMeta());
      const value = infer(env, expr.value, valueTy);
      const body = infer(extend(env, [[expr.name, value.typ]]), expr.body, expected);
      return typed(expr, body.typ, { name: expr.name, value, body });
    }
    case "Const": {
      const ty = freshFloat();
      assertSubtype(ty, expected, expr);
      return typed(expr, ty, { value: expr.value });
    }
    case "Neg": {
      const ty = ensureFloat(expected, expr);
      const value = infer(env, expr.expr, ty);
      return typed(expr, ty, { expr: value });
    }
    case "Add":
    case "Sub": {
      const ty = ensureFloat(expected, expr);
      const left = infer(env, expr.left, ty);
      const right = infer(env, expr.right, ty);
      return typed(expr, ty, { left, right });
    }
    case "Mul": {
      const scaling = expr.left.kind === "Const" || expr.right.kind === "Const";
      if (scaling) {
        const ty = ensureFloat(expected, expr);
        return typed(expr, ty, { left: infer(env, expr.left, ty), right: infer(env, expr.right, ty) });
      }
      const gTy = floatG();
      const left = infer(env, expr.left, gTy);
      const right = infer(env, expr.right, gTy);
      const resTy = ensureFloat(expected, expr);
      return typed(expr, resTy, { left, right });
    }
    case "Div": {
      const scaling = expr.right.kind === "Const";
      const aTy = scaling ? expected : floatG();
      const bTy = scaling ? expected : aTy;
      const resTy = ensureFloat(expected, expr);
      const left = infer(env, expr.left, aTy);
      const right = infer(env, expr.right, bTy);
      return typed(expr, resTy, { left, right });
    }
    case "Lt":
    case "Leq": {
      const gTy = floatG();
      const left = infer(env, expr.left, gTy);
      const right = infer(env, expr.right, gTy);
      assertSubtype(TBool, expected, expr);
      return typed(expr, TBool, { left, right });
    }
    case "Uniform": {
      const ty = ensureFloat(expected, expr);
      forceAnnotatedMode(expr, ty);
      return typed(expr, ty, { mode: expr.mode, args: [infer(env, expr.args[0], ty), infer(env, expr.args[1], ty)] });
    }
    case "Gauss": {
      const meanTy = ensureFloat(expected, expr);
      forceAnnotatedMode(expr, meanTy);
      const args = [infer(env, expr.args[0], meanTy), infer(env, expr.args[1], floatG())];
      return typed(expr, meanTy, { mode: expr.mode, args });
    }
    case "Exponential": {
      const ty = ensureFloat(expected, expr);
      forceAnnotatedMode(expr, ty);
      return typed(expr, ty, { mode: expr.mode, args: [infer(env, expr.args[0], floatG())] });
    }
    case "Gamma": {
      const ty = ensureFloat(expected, expr);
      forceAnnotatedMode(expr, ty);
      return typed(expr, ty, { mode: expr.mode, args: [infer(env, expr.args[0], ty), infer(env, expr.args[1], floatG())] });
    }
    case "Beta": {
      const ty = ensureFloat(expected, expr);
      forceAnnotatedMode(expr, ty);
      const paramTy = floatG();
      return typed(expr, ty, { mode: expr.mode, args: [infer(env, expr.args[0], paramTy), infer(env, expr.args[1], paramTy)] });
    }
    case "Flip": {
      const p = infer(env, expr.args[0], freshFloat());
      assertSubtype(TBool, expected, expr);
      return typed(expr, TBool, { mode: expr.mode, args: [p] });
    }
    case "Bernoulli":
    case "Poisson": {
      const ty = ensureFloat(expected, expr);
      forceAnnotatedMode(expr, ty);
      return typed(expr, ty, { mode: expr.mode, args: [infer(env, expr.args[0], ty)] });
    }
    case "Discrete": {
      const ty = ensureFloat(expected, expr);
      forceAnnotatedMode(expr, ty);
      const choices = expr.choices.map((choice) => {
        if (choice.probability < 0 || choice.probability > 1) {
          throw new CompileError("discrete probability must be in [0, 1]", expr.from, expr.to);
        }
        return { probability: choice.probability, value: infer(env, choice.value, ty) };
      });
      return typed(expr, ty, { mode: expr.mode, choices });
    }
    case "Observe": {
      const cond = infer(env, expr.cond, TBool);
      assertSubtype(TUnit, expected, expr);
      return typed(expr, TUnit, { cond });
    }
    default:
      throw new CompileError(`unsupported expression kind ${expr.kind}`, expr.from, expr.to);
  }
}

export function defaultModes(typedExpr) {
  const go = (te) => {
    defaultModesType(te.typ);
    for (const child of typedChildren(te)) go(child);
  };
  go(typedExpr);
  return typedExpr;
}

export function typedChildren(te) {
  switch (te.kind) {
    case "Lam":
    case "Rec":
      return [te.body];
    case "App":
      return [te.fn, te.arg];
    case "Pair":
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
    case "Lt":
    case "Leq":
      return [te.left, te.right];
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return [te.expr];
    case "Cons":
      return [te.head, te.tail];
    case "Case":
      return [te.scrutinee, te.left, te.right];
    case "MatchList":
      return [te.scrutinee, te.nilBranch, te.consBranch];
    case "If":
      return [te.cond, te.thenBranch, te.elseBranch];
    case "Let":
      return [te.value, te.body];
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Flip":
    case "Bernoulli":
    case "Poisson":
      return te.args;
    case "Discrete":
      return te.choices.map((choice) => choice.value);
    case "Observe":
      return [te.cond];
    default:
      return [];
  }
}

export function collectSpans(te, spans = []) {
  spans.push({
    from: te.from,
    to: te.to,
    kind: ["Var"].includes(te.kind) ? "identifier" : isDistribution(te.kind) ? "distribution" : "expr",
    type: formatType(te.typ),
    mode: zonk(te.typ)?.tag === "Float" ? (zonk(te.typ).mode.mode ?? "?") : undefined,
    text: hoverText(te),
  });
  for (const child of typedChildren(te)) collectSpans(child, spans);
  return spans;
}

function hoverText(te) {
  const base = `${te.kind}: ${formatType(te.typ)}`;
  if (!isDistribution(te.kind)) return base;
  const mode = te.typ.tag === "Float" ? te.typ.mode.mode ?? "?" : "?";
  if (mode === "E") return `${base}\ndeterminizes to its expectation`;
  if (mode === "G") return `${base}\nsampled normally`;
  return base;
}

function isDistribution(kind) {
  return ["Uniform", "Gauss", "Exponential", "Gamma", "Beta", "Flip", "Bernoulli", "Poisson", "Discrete"].includes(kind);
}
