import { node } from "../compiler/ast.js";
import { determinize } from "../compiler/determinize.js";
import { defaultModes, inferProgram } from "../compiler/infer.js";
import { parse } from "../compiler/parser.js";
import { prettyExpr } from "../compiler/pretty.js";
import { zonk } from "../compiler/types.js";
import { affineAdd, affineConst, affineDiv, affineMul, affineNeg, affineSub, affineToNumber, affineVar, evalAffine, prettyAffine, symFloat, valueToAffine } from "./affine.js";
import { floatDistributions, instantiateArgs, meanDistribution, sampleDistribution } from "./distributions.js";
import { makeStreams } from "./rng.js";

export function prepareRuntime(source) {
  const ast = parse(source);
  const typed = inferProgram(ast);
  defaultModes(typed);
  return {
    expr: runtimeFromTyped(typed),
    determinized: runtimeFromAst(determinize(typed)),
    typed,
  };
}

export function prepareRuntimeUnchecked(source) {
  const ast = parse(source);
  const expr = runtimeFromAst(ast);
  return {
    expr,
    determinized: determinizeResidual(expr),
    typed: null,
    unchecked: true,
  };
}

export function runtimeFromTyped(te) {
  const distMode = () => {
    if (!floatDistributions.has(te.kind)) return "G";
    const ty = zonk(te.typ);
    return ty?.tag === "Float" ? ty.mode.mode ?? "E" : "G";
  };
  switch (te.kind) {
    case "Var":
      return n("Var", { name: te.name }, te);
    case "Lam":
      return n("Lam", { param: te.param, body: runtimeFromTyped(te.body) }, te);
    case "Rec":
      return n("Rec", { name: te.name, param: te.param, body: runtimeFromTyped(te.body) }, te);
    case "App":
      return n("App", { fn: runtimeFromTyped(te.fn), arg: runtimeFromTyped(te.arg) }, te);
    case "Unit":
    case "Nil":
      return n(te.kind, {}, te);
    case "Pair":
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
    case "Lt":
    case "Leq":
      return n(te.kind, { left: runtimeFromTyped(te.left), right: runtimeFromTyped(te.right) }, te);
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return n(te.kind, { expr: runtimeFromTyped(te.expr) }, te);
    case "Cons":
      return n("Cons", { head: runtimeFromTyped(te.head), tail: runtimeFromTyped(te.tail) }, te);
    case "Case":
      return n("Case", { scrutinee: runtimeFromTyped(te.scrutinee), leftName: te.leftName, left: runtimeFromTyped(te.left), rightName: te.rightName, right: runtimeFromTyped(te.right) }, te);
    case "MatchList":
      return n("MatchList", { scrutinee: runtimeFromTyped(te.scrutinee), nilBranch: runtimeFromTyped(te.nilBranch), headName: te.headName, tailName: te.tailName, consBranch: runtimeFromTyped(te.consBranch) }, te);
    case "Bool":
      return n("Bool", { value: te.value }, te);
    case "If":
      return n("If", { cond: runtimeFromTyped(te.cond), thenBranch: runtimeFromTyped(te.thenBranch), elseBranch: runtimeFromTyped(te.elseBranch) }, te);
    case "Let":
      return n("Let", { name: te.name, value: runtimeFromTyped(te.value), body: runtimeFromTyped(te.body) }, te);
    case "Const":
      return n("Const", { value: te.value }, te);
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Flip":
    case "Bernoulli":
    case "Poisson":
      return n(te.kind, { mode: distMode(), args: te.args.map(runtimeFromTyped) }, te);
    case "Discrete":
      return n("Discrete", { mode: distMode(), choices: te.choices.map((choice) => ({ probability: choice.probability, value: runtimeFromTyped(choice.value) })) }, te);
    case "Observe":
      return n("Observe", { cond: runtimeFromTyped(te.cond) }, te);
    default:
      throw new Error(`unsupported typed expression ${te.kind}`);
  }
}

export function runtimeFromAst(expr) {
  switch (expr.kind) {
    case "Var":
    case "Const":
    case "Bool":
    case "Unit":
    case "Nil":
    case "SymFloat":
      return clone(expr);
    case "Lam":
      return n("Lam", { param: expr.param, body: runtimeFromAst(expr.body) }, expr);
    case "Rec":
      return n("Rec", { name: expr.name, param: expr.param, body: runtimeFromAst(expr.body) }, expr);
    case "App":
      return n("App", { fn: runtimeFromAst(expr.fn), arg: runtimeFromAst(expr.arg) }, expr);
    case "Pair":
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
    case "Lt":
    case "Leq":
      return n(expr.kind, { left: runtimeFromAst(expr.left), right: runtimeFromAst(expr.right) }, expr);
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return n(expr.kind, { expr: runtimeFromAst(expr.expr) }, expr);
    case "Cons":
      return n("Cons", { head: runtimeFromAst(expr.head), tail: runtimeFromAst(expr.tail) }, expr);
    case "Case":
      return n("Case", { scrutinee: runtimeFromAst(expr.scrutinee), leftName: expr.leftName, left: runtimeFromAst(expr.left), rightName: expr.rightName, right: runtimeFromAst(expr.right) }, expr);
    case "MatchList":
      return n("MatchList", { scrutinee: runtimeFromAst(expr.scrutinee), nilBranch: runtimeFromAst(expr.nilBranch), headName: expr.headName, tailName: expr.tailName, consBranch: runtimeFromAst(expr.consBranch) }, expr);
    case "If":
      return n("If", { cond: runtimeFromAst(expr.cond), thenBranch: runtimeFromAst(expr.thenBranch), elseBranch: runtimeFromAst(expr.elseBranch) }, expr);
    case "Let":
      return n("Let", { name: expr.name, value: runtimeFromAst(expr.value), body: runtimeFromAst(expr.body) }, expr);
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Flip":
    case "Bernoulli":
    case "Poisson":
      return n(expr.kind, { mode: expr.mode ?? "G", args: expr.args.map(runtimeFromAst) }, expr);
    case "Discrete":
      return n("Discrete", { mode: expr.mode ?? "G", choices: expr.choices.map((choice) => ({ probability: choice.probability, value: runtimeFromAst(choice.value) })) }, expr);
    case "Observe":
      return n("Observe", { cond: runtimeFromAst(expr.cond) }, expr);
    default:
      throw new Error(`unsupported expression ${expr.kind}`);
  }
}

export function runOrdinary(expr, streams, maxSteps = 1000) {
  let state = { expr: clone(expr), rngE: streams.rngE.clone(), rngG: streams.rngG.clone() };
  const trace = [prettyExpr(state.expr)];
  for (let steps = 0; steps < maxSteps && !isValue(state.expr); steps++) {
    state = stepOrdinary(state);
    trace.push(prettyExpr(state.expr));
  }
  if (!isValue(state.expr)) throw new Error("ordinary semantics did not terminate");
  return { ...state, trace, value: state.expr };
}

export function runSymbolic(expr, streams, maxSteps = 1000) {
  let state = { expr: clone(expr), sigma: [], rngG: streams.rngG.clone(), nextSymbol: 1 };
  const trace = [prettySymbolicState(state)];
  for (let steps = 0; steps < maxSteps && !isValue(state.expr); steps++) {
    state = stepSymbolic(state);
    trace.push(prettySymbolicState(state));
  }
  if (!isValue(state.expr)) throw new Error("symbolic semantics did not terminate");
  return { ...state, trace, value: state.expr };
}

export function stepOrdinary(state) {
  const result = step(state.expr, { kind: "ordinary", rngE: state.rngE, rngG: state.rngG });
  return { ...state, expr: result.expr, rngE: result.rngE ?? state.rngE, rngG: result.rngG ?? state.rngG };
}

export function stepSymbolic(state) {
  const result = step(state.expr, { kind: "symbolic", sigma: state.sigma, rngG: state.rngG, nextSymbol: state.nextSymbol });
  return {
    ...state,
    expr: result.expr,
    sigma: result.sigma ?? state.sigma,
    rngG: result.rngG ?? state.rngG,
    nextSymbol: result.nextSymbol ?? state.nextSymbol,
  };
}

export function projectSample(symbolicState, rngE) {
  const env = symbolicSampleEnv(symbolicState, rngE);
  return concretize(symbolicState.expr, env);
}

function projectSampleWithEnv(symbolicState, rngE) {
  const env = symbolicSampleEnv(symbolicState, rngE);
  return {
    expr: concretize(symbolicState.expr, env),
    sampleBySymbol: Object.fromEntries(env),
  };
}

function symbolicSampleEnv(symbolicState, rngE) {
  const env = new Map();
  const rng = rngE.clone();
  for (const binding of symbolicState.sigma) {
    const args = instantiateArgs(binding.args, env);
    env.set(binding.name, sampleDistribution(binding.kind, args, rng));
  }
  return env;
}

export function projectMean(symbolicState) {
  const env = new Map();
  for (const binding of symbolicState.sigma) {
    const args = binding.args.map((arg) => affineConst(evalAffine(arg, env)));
    env.set(binding.name, affineToNumber(meanDistribution(binding.kind, args)));
  }
  return concretize(symbolicState.expr, env);
}

export function projectMeanDeterminized(symbolicState) {
  const env = symbolicMeanEnv(symbolicState);
  return determinizeResidual(concretize(symbolicState.expr, env));
}

export function checkEquivalences(source, seed = 1) {
  const prepared = prepareRuntime(source);
  const streams = makeStreams(seed);
  const ordinary = runOrdinary(prepared.expr, streams);
  const symbolic = runSymbolic(prepared.expr, streams);
  const sampledProjection = projectSample(symbolic, streams.rngE);
  const determinized = runOrdinary(prepared.determinized, streams);
  const meanProjection = projectMean(symbolic);
  return {
    ordinary,
    symbolic,
    sampledProjection,
    determinized,
    meanProjection,
    sampledEquivalent: valuesEqual(ordinary.value, sampledProjection),
    meanEquivalent: valuesEqual(determinized.value, meanProjection),
  };
}

export function runCoupledTrace(source, seed = 1, maxSymbolicSteps = 1000, maxSyncSteps = 200, options = {}) {
  const prepared = options.allowIllTyped ? prepareRuntimeUnchecked(source) : prepareRuntime(source);
  const streams = makeStreams(seed);
  let symbolic = { expr: clone(prepared.expr), sigma: [], rngG: streams.rngG.clone(), nextSymbol: 1 };
  let original = { expr: clone(prepared.expr), rngE: streams.rngE.clone(), rngG: streams.rngG.clone() };
  let determinizedState = { expr: clone(prepared.determinized), rngE: streams.rngE.clone(), rngG: streams.rngG.clone() };
  const frames = [];

  for (let stepIndex = 0; stepIndex <= maxSymbolicSteps; stepIndex++) {
    const originalProjection = safe(() => projectSampleWithEnv(symbolic, streams.rngE));
    const determinizedProjection = safe(() => projectMeanDeterminized(symbolic));
    const originalTarget = originalProjection.value?.expr;
    const determinizedTarget = determinizedProjection.value;
    const originalSync = originalProjection.ok
      ? advanceToTarget(original, originalTarget, maxSyncSteps)
      : failedAdvance(original, originalProjection.error);
    const determinizedSync = determinizedProjection.ok
      ? advanceToTarget(determinizedState, determinizedTarget, maxSyncSteps)
      : failedAdvance(determinizedState, determinizedProjection.error);
    original = originalSync.state;
    determinizedState = determinizedSync.state;

    const frame = {
      step: stepIndex,
      original: clone(original.expr),
      symbolic: clone(symbolic.expr),
      sigma: symbolic.sigma.map(cloneBinding),
      sampleBySymbol: originalProjection.value?.sampleBySymbol ?? {},
      determinized: clone(determinizedState.expr),
      originalTarget,
      determinizedTarget,
      originalOk: originalSync.ok,
      determinizedOk: determinizedSync.ok,
      originalMicroSteps: originalSync.steps,
      determinizedMicroSteps: determinizedSync.steps,
      originalError: originalSync.error,
      determinizedError: determinizedSync.error,
    };

    if (originalSync.ok && determinizedSync.ok && !isValue(symbolic.expr)) {
      const nextSymbolic = safe(() => stepSymbolic(symbolic));
      if (nextSymbolic.ok) {
        frames.push({ ...frame, symbolicOk: true });
        symbolic = nextSymbolic.value;
        continue;
      }
      frames.push({ ...frame, symbolicOk: false, symbolicError: nextSymbolic.error });
      break;
    }

    frames.push({ ...frame, symbolicOk: true });
    if (!originalSync.ok || !determinizedSync.ok || isValue(symbolic.expr)) break;
  }

  return {
    seed,
    frames,
    unchecked: prepared.unchecked ?? false,
    finalOriginal: safe(() => runOrdinary(prepared.expr, streams).value).value,
    finalDeterminized: safe(() => runOrdinary(prepared.determinized, streams).value).value,
    ok: frames.every((frame) => frame.originalOk && frame.determinizedOk && frame.symbolicOk !== false),
  };
}

function safe(fn) {
  try {
    return { ok: true, value: fn() };
  } catch (error) {
    return { ok: false, error: error?.message ?? String(error) };
  }
}

function failedAdvance(state, error) {
  return {
    ok: false,
    state,
    steps: 0,
    microTrace: [prettyExpr(state.expr)],
    error,
  };
}

function step(expr, ctx) {
  switch (expr.kind) {
    case "Let":
      if (!isValue(expr.value)) return stepChild(expr, "value", ctx);
      return out(subst(expr.body, expr.name, expr.value), ctx);
    case "App":
      if (!isValue(expr.fn)) return stepChild(expr, "fn", ctx);
      if (!isValue(expr.arg)) return stepChild(expr, "arg", ctx);
      if (expr.fn.kind === "Lam") return out(subst(expr.fn.body, expr.fn.param, expr.arg), ctx);
      if (expr.fn.kind === "Rec") {
        const body = subst(subst(expr.fn.body, expr.fn.name, expr.fn), expr.fn.param, expr.arg);
        return out(body, ctx);
      }
      throw new Error("application to non-function");
    case "Pair":
      if (!isValue(expr.left)) return stepChild(expr, "left", ctx);
      if (!isValue(expr.right)) return stepChild(expr, "right", ctx);
      break;
    case "Fst":
      if (!isValue(expr.expr)) return stepChild(expr, "expr", ctx);
      if (expr.expr.kind !== "Pair") throw new Error("fst on non-pair");
      return out(expr.expr.left, ctx);
    case "Snd":
      if (!isValue(expr.expr)) return stepChild(expr, "expr", ctx);
      if (expr.expr.kind !== "Pair") throw new Error("snd on non-pair");
      return out(expr.expr.right, ctx);
    case "Inl":
    case "Inr":
      if (!isValue(expr.expr)) return stepChild(expr, "expr", ctx);
      break;
    case "Case":
      if (!isValue(expr.scrutinee)) return stepChild(expr, "scrutinee", ctx);
      if (expr.scrutinee.kind === "Inl") return out(subst(expr.left, expr.leftName, expr.scrutinee.expr), ctx);
      if (expr.scrutinee.kind === "Inr") return out(subst(expr.right, expr.rightName, expr.scrutinee.expr), ctx);
      throw new Error("match on non-sum");
    case "Cons":
      if (!isValue(expr.head)) return stepChild(expr, "head", ctx);
      if (!isValue(expr.tail)) return stepChild(expr, "tail", ctx);
      break;
    case "MatchList":
      if (!isValue(expr.scrutinee)) return stepChild(expr, "scrutinee", ctx);
      if (expr.scrutinee.kind === "Nil") return out(expr.nilBranch, ctx);
      if (expr.scrutinee.kind === "Cons") return out(subst(subst(expr.consBranch, expr.headName, expr.scrutinee.head), expr.tailName, expr.scrutinee.tail), ctx);
      throw new Error("match on non-list");
    case "If":
      if (!isValue(expr.cond)) return stepChild(expr, "cond", ctx);
      if (expr.cond.kind !== "Bool") throw new Error("if condition is not boolean");
      return out(expr.cond.value ? expr.thenBranch : expr.elseBranch, ctx);
    case "Neg":
      if (!isValue(expr.expr)) return stepChild(expr, "expr", ctx);
      return out(floatResult(affineNeg(valueToAffine(expr.expr)), expr), ctx);
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
      if (!isValue(expr.left)) return stepChild(expr, "left", ctx);
      if (!isValue(expr.right)) return stepChild(expr, "right", ctx);
      return out(arithmetic(expr.kind, expr.left, expr.right, expr), ctx);
    case "Lt":
    case "Leq":
      if (!isValue(expr.left)) return stepChild(expr, "left", ctx);
      if (!isValue(expr.right)) return stepChild(expr, "right", ctx);
      return out(n("Bool", { value: expr.kind === "Lt" ? numberValue(expr.left) < numberValue(expr.right) : numberValue(expr.left) <= numberValue(expr.right) }, expr), ctx);
    case "Observe":
      if (!isValue(expr.cond)) return stepChild(expr, "cond", ctx);
      if (expr.cond.kind !== "Bool") throw new Error("observe: expected bool");
      if (!expr.cond.value) return out(n("Reject", {}, expr), ctx);
      return out(n("Unit", {}, expr), ctx);
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Flip":
    case "Bernoulli":
    case "Poisson":
      return stepDistribution(expr, ctx);
    case "Discrete":
      return stepDiscrete(expr, ctx);
  }
  throw new Error(`stuck expression ${expr.kind}`);
}

function stepDistribution(expr, ctx) {
  for (let i = 0; i < expr.args.length; i++) {
    if (!isValue(expr.args[i])) return stepIndexedChild(expr, "args", i, ctx);
  }
  if (ctx.kind === "symbolic" && expr.mode === "E" && floatDistributions.has(expr.kind)) {
    const name = `v${ctx.nextSymbol}`;
    const binding = { name, kind: expr.kind, args: expr.args.map(valueToAffine) };
    return out(symFloat(affineVar(name), expr.from, expr.to), { ...ctx, sigma: [...ctx.sigma, binding], nextSymbol: ctx.nextSymbol + 1 });
  }
  const streamName = expr.mode === "E" ? "rngE" : "rngG";
  const rng = ctx[streamName];
  const value = sampleDistribution(expr.kind, expr.args, rng);
  return out(typeof value === "boolean" ? n("Bool", { value }, expr) : n("Const", { value }, expr), { ...ctx, [streamName]: rng });
}

function stepDiscrete(expr, ctx) {
  if (ctx.kind === "symbolic" && expr.mode === "E") {
    const name = `v${ctx.nextSymbol}`;
    const binding = { name, kind: "Discrete", args: expr.choices.map((choice) => affineConst(choice.probability)) };
    return out(symFloat(affineVar(name), expr.from, expr.to), { ...ctx, sigma: [...ctx.sigma, binding], nextSymbol: ctx.nextSymbol + 1 });
  }
  const streamName = expr.mode === "E" ? "rngE" : "rngG";
  const rng = ctx[streamName];
  const index = sampleDistribution("Discrete", expr.choices.map((choice) => n("Const", { value: choice.probability }, expr)), rng);
  return out(expr.choices[index].value, { ...ctx, [streamName]: rng });
}

function advanceToTarget(state, target, maxSteps) {
  let current = state;
  let steps = 0;
  const microTrace = [prettyExpr(current.expr)];
  try {
    while (!exprEqual(current.expr, target) && steps < maxSteps && !isValue(current.expr)) {
      current = stepOrdinary(current);
      steps += 1;
      microTrace.push(prettyExpr(current.expr));
    }
  } catch (error) {
    return {
      ok: false,
      state: current,
      steps,
      microTrace,
      error: error?.message ?? String(error),
    };
  }
  return {
    ok: exprEqual(current.expr, target),
    state: current,
    steps,
    microTrace,
    error: exprEqual(current.expr, target) ? undefined : "ordinary trace did not reach the projected target",
  };
}

function symbolicMeanEnv(symbolicState) {
  const env = new Map();
  for (const binding of symbolicState.sigma) {
    const args = binding.args.map((arg) => affineConst(evalAffine(arg, env)));
    env.set(binding.name, affineToNumber(meanDistribution(binding.kind, args)));
  }
  return env;
}

function determinizeResidual(expr) {
  switch (expr.kind) {
    case "Uniform": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return n("Mul", { left: n("Add", { left: args[0], right: args[1] }, expr), right: n("Const", { value: 0.5 }, expr) }, expr);
      return n("Uniform", { mode: "G", args }, expr);
    }
    case "Gauss": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return args[0];
      return n("Gauss", { mode: "G", args }, expr);
    }
    case "Exponential": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return n("Div", { left: n("Const", { value: 1 }, expr), right: args[0] }, expr);
      return n("Exponential", { mode: "G", args }, expr);
    }
    case "Gamma": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return n("Div", { left: args[0], right: args[1] }, expr);
      return n("Gamma", { mode: "G", args }, expr);
    }
    case "Beta": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return n("Div", { left: args[0], right: n("Add", { left: args[0], right: args[1] }, expr) }, expr);
      return n("Beta", { mode: "G", args }, expr);
    }
    case "Bernoulli":
    case "Poisson": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return args[0];
      return n(expr.kind, { mode: "G", args }, expr);
    }
    case "Discrete": {
      const choices = expr.choices.map((choice) => ({ probability: choice.probability, value: determinizeResidual(choice.value) }));
      if (expr.mode === "E") return weightedChoiceSum(choices, expr);
      return n("Discrete", { mode: "G", choices }, expr);
    }
    case "Flip":
      return n("Flip", { mode: "G", args: expr.args.map(determinizeResidual) }, expr);
    default:
      return mapChildren(expr, determinizeResidual);
  }
}

function weightedChoiceSum(choices, source) {
  if (choices.length === 0) return n("Const", { value: 0 }, source);
  const [first, ...rest] = choices;
  const term = n("Mul", { left: n("Const", { value: first.probability }, source), right: first.value }, source);
  if (rest.length === 0) return term;
  return n("Add", { left: term, right: weightedChoiceSum(rest, source) }, source);
}

function arithmetic(kind, left, right, source) {
  const a = valueToAffine(left);
  const b = valueToAffine(right);
  if (kind === "Add") return floatResult(affineAdd(a, b), source);
  if (kind === "Sub") return floatResult(affineSub(a, b), source);
  if (kind === "Mul") return floatResult(affineMul(a, b), source);
  return floatResult(affineDiv(a, b), source);
}

function floatResult(affine, source) {
  if (Object.keys(affine.terms).length === 0) return n("Const", { value: affine.constant }, source);
  return symFloat(affine, source.from, source.to);
}

function stepChild(expr, key, ctx) {
  const result = step(expr[key], ctx);
  if (result.expr.kind === "Reject") return out(result.expr, { ...ctx, ...contextPatch(result) });
  return rebuild(expr, { [key]: result.expr }, ctx, result);
}

function stepIndexedChild(expr, key, index, ctx) {
  const result = step(expr[key][index], ctx);
  if (result.expr.kind === "Reject") return out(result.expr, { ...ctx, ...contextPatch(result) });
  const next = expr[key].slice();
  next[index] = result.expr;
  return rebuild(expr, { [key]: next }, ctx, result);
}

function rebuild(expr, patch, ctx, result) {
  return out(n(expr.kind, { ...copyProps(expr), ...patch }, expr), { ...ctx, ...contextPatch(result) });
}

function contextPatch(result) {
  const patch = {};
  for (const key of ["rngE", "rngG", "sigma", "nextSymbol"]) if (key in result) patch[key] = result[key];
  return patch;
}

function out(expr, ctx) {
  return { expr, ...contextPatch(ctx) };
}

function copyProps(expr) {
  const props = { ...expr };
  delete props.kind;
  delete props.from;
  delete props.to;
  return props;
}

function subst(expr, name, replacement) {
  switch (expr.kind) {
    case "Var":
      return expr.name === name ? clone(replacement) : clone(expr);
    case "Lam":
      return expr.param === name ? clone(expr) : n("Lam", { param: expr.param, body: subst(expr.body, name, replacement) }, expr);
    case "Rec":
      return expr.name === name || expr.param === name ? clone(expr) : n("Rec", { name: expr.name, param: expr.param, body: subst(expr.body, name, replacement) }, expr);
    case "Let":
      return n("Let", { name: expr.name, value: subst(expr.value, name, replacement), body: expr.name === name ? clone(expr.body) : subst(expr.body, name, replacement) }, expr);
    case "Case":
      return n("Case", { scrutinee: subst(expr.scrutinee, name, replacement), leftName: expr.leftName, left: expr.leftName === name ? clone(expr.left) : subst(expr.left, name, replacement), rightName: expr.rightName, right: expr.rightName === name ? clone(expr.right) : subst(expr.right, name, replacement) }, expr);
    case "MatchList":
      return n("MatchList", { scrutinee: subst(expr.scrutinee, name, replacement), nilBranch: subst(expr.nilBranch, name, replacement), headName: expr.headName, tailName: expr.tailName, consBranch: expr.headName === name || expr.tailName === name ? clone(expr.consBranch) : subst(expr.consBranch, name, replacement) }, expr);
    default:
      return mapChildren(expr, (child) => subst(child, name, replacement));
  }
}

function mapChildren(expr, f) {
  switch (expr.kind) {
    case "Lam":
      return n("Lam", { param: expr.param, body: f(expr.body) }, expr);
    case "Rec":
      return n("Rec", { name: expr.name, param: expr.param, body: f(expr.body) }, expr);
    case "Let":
      return n("Let", { name: expr.name, value: f(expr.value), body: f(expr.body) }, expr);
    case "App":
      return n("App", { fn: f(expr.fn), arg: f(expr.arg) }, expr);
    case "Pair":
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
    case "Lt":
    case "Leq":
      return n(expr.kind, { left: f(expr.left), right: f(expr.right) }, expr);
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return n(expr.kind, { expr: f(expr.expr) }, expr);
    case "Cons":
      return n("Cons", { head: f(expr.head), tail: f(expr.tail) }, expr);
    case "If":
      return n("If", { cond: f(expr.cond), thenBranch: f(expr.thenBranch), elseBranch: f(expr.elseBranch) }, expr);
    case "Case":
      return n("Case", { scrutinee: f(expr.scrutinee), leftName: expr.leftName, left: f(expr.left), rightName: expr.rightName, right: f(expr.right) }, expr);
    case "MatchList":
      return n("MatchList", { scrutinee: f(expr.scrutinee), nilBranch: f(expr.nilBranch), headName: expr.headName, tailName: expr.tailName, consBranch: f(expr.consBranch) }, expr);
    case "Observe":
      return n("Observe", { cond: f(expr.cond) }, expr);
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Flip":
    case "Bernoulli":
    case "Poisson":
      return n(expr.kind, { mode: expr.mode, args: expr.args.map(f) }, expr);
    case "Discrete":
      return n("Discrete", { mode: expr.mode, choices: expr.choices.map((choice) => ({ probability: choice.probability, value: f(choice.value) })) }, expr);
    default:
      return clone(expr);
  }
}

function concretize(expr, env) {
  switch (expr.kind) {
    case "SymFloat":
      return n("Const", { value: evalAffine(expr.affine, env) }, expr);
    default:
      return mapChildren(expr, (child) => concretize(child, env));
  }
}

export function isValue(expr) {
  return expr.kind === "Reject" || expr.kind === "Const" || expr.kind === "SymFloat" || expr.kind === "Bool" || expr.kind === "Unit" || expr.kind === "Lam" || expr.kind === "Rec" || expr.kind === "Nil" || (expr.kind === "Pair" && isValue(expr.left) && isValue(expr.right)) || (expr.kind === "Inl" && isValue(expr.expr)) || (expr.kind === "Inr" && isValue(expr.expr)) || (expr.kind === "Cons" && isValue(expr.head) && isValue(expr.tail));
}

function numberValue(expr) {
  return affineToNumber(valueToAffine(expr));
}

export function exprEqual(a, b, eps = 1e-9) {
  if (a.kind !== b.kind) return false;
  switch (a.kind) {
    case "Const":
      return Math.abs(a.value - b.value) <= eps;
    case "Bool":
      return a.value === b.value;
    case "Unit":
    case "Nil":
    case "Reject":
      return true;
    case "Pair":
      return exprEqual(a.left, b.left, eps) && exprEqual(a.right, b.right, eps);
    case "Inl":
    case "Inr":
      return exprEqual(a.expr, b.expr, eps);
    case "Cons":
      return exprEqual(a.head, b.head, eps) && exprEqual(a.tail, b.tail, eps);
    case "SymFloat":
      return prettyAffine(a.affine) === prettyAffine(b.affine);
    default:
      return prettyExpr(a) === prettyExpr(b);
  }
}

function valuesEqual(a, b, eps = 1e-9) {
  return exprEqual(a, b, eps);
}

export function prettySymbolicState(state) {
  const sigma = state.sigma.length === 0
    ? "empty"
    : state.sigma.map((binding) => `${binding.name} ~ ${binding.kind.toLowerCase()}(${binding.args.map(prettyAffine).join(", ")})`).join("; ");
  return `<${sigma} || ${prettyExpr(state.expr)}>`;
}

function clone(expr) {
  if (expr.kind === "SymFloat") return symFloat(expr.affine, expr.from, expr.to);
  return JSON.parse(JSON.stringify(expr));
}

function cloneBinding(binding) {
  return JSON.parse(JSON.stringify(binding));
}

function n(kind, props, source) {
  return node(kind, props, source.from ?? 0, source.to ?? source.from ?? 0);
}
