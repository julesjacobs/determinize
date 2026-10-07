import { analyze } from "../compiler/analyze.ts";
import type { Expr, ExprOf, MeanKind, ParamDistributionKind, Span } from "../compiler/ast.ts";
import { node } from "../compiler/ast.ts";
import { CompileError } from "../compiler/errors.ts";
import { prettyExpr } from "../compiler/pretty.ts";
import type { Affine } from "./affine.ts";
import {
  affineAdd,
  affineConst,
  affineDiv,
  affineMul,
  affineNeg,
  affineSub,
  affineToNumber,
  affineVar,
  evalAffine,
  isConcreteAffine,
  prettyAffine,
  symFloat,
  valueToAffine,
} from "./affine.ts";
import type { DistributionDomainError } from "./distributions.ts";
import {
  distributionName,
  floatDistributions,
  instantiateArgs,
  isDistributionDomainError,
  meanDistribution,
  sampleDistribution,
} from "./distributions.ts";
import type { Rng, Streams } from "./rng.ts";
import { makeStreams } from "./rng.ts";

/** A symbolic E draw, name ~ kind(args). */
export interface Binding {
  name: string;
  kind: MeanKind;
  args: Affine[];
}

export interface OrdinaryState {
  expr: Expr;
  rngE: Rng;
  rngG: Rng;
}

export interface SymbolicState {
  expr: Expr;
  sigma: Binding[];
  rngG: Rng;
  nextSymbol: number;
}

/** The part of a machine's state that a step reads and may change. */
type Context =
  | { kind: "ordinary"; rngE: Rng; rngG: Rng }
  | { kind: "symbolic"; rngE?: undefined; sigma: Binding[]; rngG: Rng; nextSymbol: number };

interface ContextPatch {
  rngE?: Rng;
  rngG?: Rng;
  sigma?: Binding[];
  nextSymbol?: number;
}

interface StepResult extends ContextPatch {
  expr: Expr;
}

export interface Prepared {
  expr: Expr;
  determinized: Expr;
  /** Lean rejects the program for a mode conflict; this is the counterexample. */
  counterexample: boolean;
}

/** An ordinary run synchronized with a target expression. */
interface Advance {
  ok: boolean;
  state: OrdinaryState;
  steps: number;
  microTrace: string[];
  error: string | undefined;
}

/** One symbolic step, with the source and determinized runs synchronized to it. */
export interface Frame {
  step: number;
  original: Expr;
  symbolic: Expr;
  sigma: Binding[];
  sampleBySymbol: Record<string, number>;
  determinized: Expr;
  originalTarget: Expr | undefined;
  determinizedTarget: Expr | undefined;
  originalOk: boolean;
  determinizedOk: boolean;
  originalMicroSteps: number;
  determinizedMicroSteps: number;
  originalError: string | undefined;
  determinizedError: string | undefined;
  consistencyOk: boolean;
  consistencyError: string | undefined;
  symbolicOk: boolean;
  symbolicError?: string;
}

export interface CoupledTrace {
  seed: number;
  frames: Frame[];
  counterexample: boolean;
  finalOriginal: Expr | undefined;
  finalDeterminized: Expr | undefined;
  ok: boolean;
}

type TerminalEffect = { kind: "Reject" } | { kind: "DomainError"; message: string };

interface LabelledEffect {
  label: string;
  effect: TerminalEffect;
}

type Safe<T> =
  | { ok: true; value: T; error?: undefined }
  | { ok: false; error: string; value?: undefined };

/**
 * The annotated and the determinized program that the front end produces. For a program that
 * Lean rejects only for a mode conflict, the counterexample: exactly the [E] draws replaced by
 * their means, every other draw random. Any other rejection throws.
 */
export function prepareRuntime(source: string): Prepared {
  const analysis = analyze(source);
  if (analysis.ok) {
    return {
      expr: runtimeFromAst(analysis.annotated),
      determinized: runtimeFromAst(analysis.determinized),
      counterexample: false,
    };
  }
  if (analysis.counterexample) {
    return {
      expr: runtimeFromAst(analysis.counterexample.annotated),
      determinized: runtimeFromAst(analysis.counterexample.determinized),
      counterexample: true,
    };
  }
  const [diagnostic] = analysis.diagnostics;
  throw new CompileError(diagnostic.message, diagnostic.from, diagnostic.to);
}

export function runtimeFromAst(expr: Expr): Expr {
  switch (expr.kind) {
    case "Const":
      return n("Const", { value: expr.value }, expr);
    case "Var":
    case "Bool":
    case "Unit":
    case "Nil":
    case "SymFloat":
    case "DomainError":
      return clone(expr);
    case "Mean":
      return n(
        "Mean",
        { distribution: expr.distribution, args: expr.args.map(runtimeFromAst) },
        expr,
      );
    case "Lam":
      return n("Lam", { param: expr.param, body: runtimeFromAst(expr.body) }, expr);
    case "Rec":
      return n(
        "Rec",
        { name: expr.name, param: expr.param, body: runtimeFromAst(expr.body) },
        expr,
      );
    case "App":
      return n("App", { fn: runtimeFromAst(expr.fn), arg: runtimeFromAst(expr.arg) }, expr);
    case "Mul": {
      // A literal factor goes to the left, as Lean's elaborator puts it, and runs first.
      const [left, right] =
        expr.right.kind === "Const" && expr.left.kind !== "Const"
          ? [expr.right, expr.left]
          : [expr.left, expr.right];
      return n("Mul", { left: runtimeFromAst(left), right: runtimeFromAst(right) }, expr);
    }
    case "Pair":
    case "Add":
    case "Sub":
    case "Div":
    case "Lt":
    case "Leq":
      return n(
        expr.kind,
        { left: runtimeFromAst(expr.left), right: runtimeFromAst(expr.right) },
        expr,
      );
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return n(expr.kind, { expr: runtimeFromAst(expr.expr) }, expr);
    case "Cons":
      return n("Cons", { head: runtimeFromAst(expr.head), tail: runtimeFromAst(expr.tail) }, expr);
    case "Case":
      return n(
        "Case",
        {
          scrutinee: runtimeFromAst(expr.scrutinee),
          leftName: expr.leftName,
          left: runtimeFromAst(expr.left),
          rightName: expr.rightName,
          right: runtimeFromAst(expr.right),
        },
        expr,
      );
    case "MatchList":
      return n(
        "MatchList",
        {
          scrutinee: runtimeFromAst(expr.scrutinee),
          nilBranch: runtimeFromAst(expr.nilBranch),
          headName: expr.headName,
          tailName: expr.tailName,
          consBranch: runtimeFromAst(expr.consBranch),
        },
        expr,
      );
    case "If":
      return n(
        "If",
        {
          cond: runtimeFromAst(expr.cond),
          thenBranch: runtimeFromAst(expr.thenBranch),
          elseBranch: runtimeFromAst(expr.elseBranch),
        },
        expr,
      );
    case "Let":
      return n(
        "Let",
        { name: expr.name, value: runtimeFromAst(expr.value), body: runtimeFromAst(expr.body) },
        expr,
      );
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
      return n(
        "Discrete",
        {
          mode: expr.mode ?? "G",
          choices: expr.choices.map((choice) => ({
            probability: choice.probability,
            value: runtimeFromAst(choice.value),
          })),
        },
        expr,
      );
    case "DiscreteWeights":
      return n(
        "Discrete",
        {
          mode: expr.mode ?? "G",
          choices: expr.weights.map((weight, index) => {
            if (weight.kind !== "Const") throw new Error("discrete expects literal weights");
            return { probability: weight.value, value: n("Const", { value: index }, expr) };
          }),
        },
        expr,
      );
    case "DiscreteList":
      return n(
        "DiscreteList",
        {
          mode: expr.mode ?? "G",
          probabilities: runtimeFromAst(expr.probabilities),
          form: expr.form,
        },
        expr,
      );
    case "Observe":
      return n("Observe", { cond: runtimeFromAst(expr.cond) }, expr);
    default:
      throw new Error(`unsupported expression ${expr.kind}`);
  }
}

export function runOrdinary(expr: Expr, streams: Streams, maxSteps = 1000) {
  let state = { expr: clone(expr), rngE: streams.rngE.clone(), rngG: streams.rngG.clone() };
  const trace = [prettyExpr(state.expr)];
  for (let steps = 0; steps < maxSteps && !isValue(state.expr); steps++) {
    state = stepOrdinary(state);
    trace.push(prettyExpr(state.expr));
  }
  if (!isValue(state.expr)) throw new Error("ordinary semantics did not terminate");
  return { ...state, trace, value: state.expr };
}

export function runSymbolic(expr: Expr, streams: Streams, maxSteps = 1000) {
  let state: SymbolicState = {
    expr: clone(expr),
    sigma: [],
    rngG: streams.rngG.clone(),
    nextSymbol: 1,
  };
  const trace = [prettySymbolicState(state)];
  for (let steps = 0; steps < maxSteps && !isValue(state.expr); steps++) {
    state = stepSymbolic(state);
    trace.push(prettySymbolicState(state));
  }
  if (!isValue(state.expr)) throw new Error("symbolic semantics did not terminate");
  return { ...state, trace, value: state.expr };
}

export function stepOrdinary(state: OrdinaryState): OrdinaryState {
  const result = step(state.expr, { kind: "ordinary", rngE: state.rngE, rngG: state.rngG });
  return {
    ...state,
    expr: result.expr,
    rngE: result.rngE ?? state.rngE,
    rngG: result.rngG ?? state.rngG,
  };
}

export function stepSymbolic(state: SymbolicState): SymbolicState {
  const result = step(state.expr, {
    kind: "symbolic",
    sigma: state.sigma,
    rngG: state.rngG,
    nextSymbol: state.nextSymbol,
  });
  return {
    ...state,
    expr: result.expr,
    sigma: result.sigma ?? state.sigma,
    rngG: result.rngG ?? state.rngG,
    nextSymbol: result.nextSymbol ?? state.nextSymbol,
  };
}

export function projectSample(symbolicState: SymbolicState, rngE: Rng): Expr {
  return projectSampleWithEnv(symbolicState, rngE).expr;
}

function projectSampleWithEnv(symbolicState: SymbolicState, rngE: Rng) {
  const env = new Map<string, number>();
  const rng = rngE.clone();
  const sampleBySymbol: Record<string, number> = {};
  for (const binding of symbolicState.sigma) {
    const args = instantiateArgs(binding.args, env);
    try {
      const value = sampleDistribution(binding.kind, args, rng);
      env.set(binding.name, value);
      sampleBySymbol[binding.name] = value;
    } catch (error) {
      if (!isDistributionDomainError(error)) throw error;
      return {
        expr: domainErrorExpr(error, symbolicState.expr),
        sampleBySymbol,
      };
    }
  }
  return {
    expr: concretize(symbolicState.expr, env),
    sampleBySymbol,
  };
}

export function projectMean(symbolicState: SymbolicState): Expr {
  const env = new Map<string, number>();
  for (const binding of symbolicState.sigma) {
    const args = binding.args.map((arg) => affineConst(evalAffine(arg, env)));
    try {
      env.set(binding.name, affineToNumber(meanDistribution(binding.kind, args)));
    } catch (error) {
      if (!isDistributionDomainError(error)) throw error;
      return domainErrorExpr(error, symbolicState.expr);
    }
  }
  return concretize(symbolicState.expr, env);
}

export function projectMeanDeterminized(symbolicState: SymbolicState): Expr {
  try {
    const env = symbolicMeanEnv(symbolicState);
    return determinizeResidual(concretize(symbolicState.expr, env));
  } catch (error) {
    if (!isDistributionDomainError(error)) throw error;
    return domainErrorExpr(error, symbolicState.expr);
  }
}

export function checkEquivalences(source: string, seed = 1) {
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

export function runCoupledTrace(
  source: string,
  seed = 1,
  maxSymbolicSteps = 1000,
  maxSyncSteps = 200,
): CoupledTrace {
  const prepared = prepareRuntime(source);
  const streams = makeStreams(seed);
  let symbolic: SymbolicState = {
    expr: clone(prepared.expr),
    sigma: [],
    rngG: streams.rngG.clone(),
    nextSymbol: 1,
  };
  let original = {
    expr: clone(prepared.expr),
    rngE: streams.rngE.clone(),
    rngG: streams.rngG.clone(),
  };
  let determinizedState = {
    expr: clone(prepared.determinized),
    rngE: streams.rngE.clone(),
    rngG: streams.rngG.clone(),
  };
  const frames: Frame[] = [];

  for (let stepIndex = 0; stepIndex <= maxSymbolicSteps; stepIndex++) {
    const originalProjection = safe(() => projectSampleWithEnv(symbolic, streams.rngE));
    const determinizedProjection = safe(() => projectMeanDeterminized(symbolic));
    const originalTarget = originalProjection.value?.expr;
    const determinizedTarget = determinizedProjection.value;
    const originalSync = originalProjection.ok
      ? advanceToTarget(original, originalTarget as Expr, maxSyncSteps)
      : failedAdvance(original, originalProjection.error);
    const determinizedSync = determinizedProjection.ok
      ? advanceToTarget(determinizedState, determinizedTarget as Expr, maxSyncSteps)
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
    const consistency = terminalEffectConsistency(frame);
    const checkedFrame = {
      ...frame,
      consistencyOk: consistency.ok,
      consistencyError: consistency.error,
    };

    if (!consistency.ok) {
      frames.push({ ...checkedFrame, symbolicOk: true });
      break;
    }

    if (originalSync.ok && determinizedSync.ok && !isValue(symbolic.expr)) {
      const nextSymbolic = safe(() => stepSymbolic(symbolic));
      if (nextSymbolic.ok) {
        frames.push({ ...checkedFrame, symbolicOk: true });
        symbolic = nextSymbolic.value;
        continue;
      }
      frames.push({ ...checkedFrame, symbolicOk: false, symbolicError: nextSymbolic.error });
      break;
    }

    frames.push({ ...checkedFrame, symbolicOk: true });
    if (!originalSync.ok || !determinizedSync.ok || isValue(symbolic.expr)) break;
  }

  return {
    seed,
    frames,
    counterexample: prepared.counterexample,
    finalOriginal: safe(() => runOrdinary(prepared.expr, streams).value).value,
    finalDeterminized: safe(() => runOrdinary(prepared.determinized, streams).value).value,
    ok: frames.every(frameChecksOk),
  };
}

function frameChecksOk(frame: Frame) {
  return (
    frame.originalOk &&
    frame.determinizedOk &&
    frame.symbolicOk !== false &&
    frame.consistencyOk !== false
  );
}

function terminalEffectConsistency(frame: Pick<Frame, "original" | "symbolic" | "determinized">): {
  ok: boolean;
  error?: string;
} {
  const effects = (
    [
      ["Original", frame.original],
      ["Symbolic", frame.symbolic],
      ["Determinized", frame.determinized],
    ] as const
  ).map(([label, expr]) => ({ label, effect: terminalEffect(expr) }));
  const active = effects.filter((item) => item.effect) as LabelledEffect[];
  if (active.length === 0) return { ok: true };
  if (active.length !== effects.length) {
    const errored = active.map((item) => item.label).join(", ");
    const succeeded = effects
      .filter((item) => !item.effect)
      .map((item) => item.label)
      .join(", ");
    return {
      ok: false,
      error: `terminal effect mismatch: ${errored} errored, but ${succeeded} did not`,
    };
  }
  const [first] = active;
  const mismatch = active.find((item) => !sameTerminalEffect(first.effect, item.effect));
  if (!mismatch) return { ok: true };
  return {
    ok: false,
    error: `terminal effect mismatch: ${first.label} reached ${prettyTerminalEffect(first.effect)}, but ${mismatch.label} reached ${prettyTerminalEffect(mismatch.effect)}`,
  };
}

function terminalEffect(expr: Expr | undefined): TerminalEffect | null {
  if (expr?.kind === "Reject") return { kind: "Reject" };
  if (expr?.kind === "DomainError") return { kind: "DomainError", message: expr.message };
  return null;
}

function sameTerminalEffect(a: TerminalEffect, b: TerminalEffect) {
  return a.kind === b.kind && (a.kind !== "DomainError" || a.message === (b as typeof a).message);
}

function prettyTerminalEffect(effect: TerminalEffect) {
  return effect.kind === "DomainError" ? effect.message : effect.kind.toLowerCase();
}

function safe<T>(fn: () => T): Safe<T> {
  try {
    return { ok: true, value: fn() };
  } catch (error) {
    return { ok: false, error: error instanceof Error ? error.message : String(error) };
  }
}

function failedAdvance(state: OrdinaryState, error: string): Advance {
  return {
    ok: false,
    state,
    steps: 0,
    microTrace: [prettyExpr(state.expr)],
    error,
  };
}

function step(expr: Expr, ctx: Context): StepResult {
  switch (expr.kind) {
    case "Const":
      // Only a nonfinite literal is not a value; Lean's runtime fails when it evaluates one.
      return out(failure("nonfinite arithmetic result", expr), ctx);
    case "Let":
      if (!isValue(expr.value)) return stepChild(expr, "value", ctx);
      return out(subst(expr.body, expr.name, expr.value), ctx);
    case "App":
      if (!isValue(expr.fn)) return stepChild(expr, "fn", ctx);
      if (!isValue(expr.arg)) return stepChild(expr, "arg", ctx);
      if (expr.fn.kind === "Lam") return out(subst(expr.fn.body, expr.fn.param, expr.arg), ctx);
      if (expr.fn.kind === "Rec") {
        const body = subst(subst(expr.fn.body, expr.fn.param, expr.arg), expr.fn.name, expr.fn);
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
      if (expr.scrutinee.kind === "Inl")
        return out(subst(expr.left, expr.leftName, expr.scrutinee.expr), ctx);
      if (expr.scrutinee.kind === "Inr")
        return out(subst(expr.right, expr.rightName, expr.scrutinee.expr), ctx);
      throw new Error("match on non-sum");
    case "Cons":
      if (!isValue(expr.head)) return stepChild(expr, "head", ctx);
      if (!isValue(expr.tail)) return stepChild(expr, "tail", ctx);
      break;
    case "MatchList":
      if (!isValue(expr.scrutinee)) return stepChild(expr, "scrutinee", ctx);
      if (expr.scrutinee.kind === "Nil") return out(expr.nilBranch, ctx);
      if (expr.scrutinee.kind === "Cons")
        return out(
          subst(
            subst(expr.consBranch, expr.headName, expr.scrutinee.head),
            expr.tailName,
            expr.scrutinee.tail,
          ),
          ctx,
        );
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
      return out(
        n(
          "Bool",
          {
            value:
              expr.kind === "Lt"
                ? numberValue(expr.left) < numberValue(expr.right)
                : numberValue(expr.left) <= numberValue(expr.right),
          },
          expr,
        ),
        ctx,
      );
    case "Observe":
      if (!isValue(expr.cond)) return stepChild(expr, "cond", ctx);
      if (expr.cond.kind !== "Bool") throw new Error("observe: expected bool");
      if (!expr.cond.value) return out(n("Reject", {}, expr), ctx);
      return out(n("Unit", {}, expr), ctx);
    case "Mean":
      return stepMean(expr, ctx);
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
    case "DiscreteList":
      return stepDiscreteList(expr, ctx);
  }
  throw new Error(`stuck expression ${expr.kind}`);
}

function stepMean(expr: ExprOf<"Mean">, ctx: Context): StepResult {
  for (let i = 0; i < expr.args.length; i++) {
    if (!isValue(expr.args[i])) return stepIndexedChild(expr, "args", i, ctx);
  }
  try {
    const args =
      expr.distribution === "DiscreteList"
        ? listValues(expr.args[0]).map(valueToAffine)
        : expr.args.map(valueToAffine);
    const mean = meanDistribution(expr.distribution, args);
    return out(floatResult(mean, expr), ctx);
  } catch (error) {
    if (!isDistributionDomainError(error)) throw error;
    return out(domainErrorExpr(error, expr), ctx);
  }
}

function stepDistribution(expr: ExprOf<ParamDistributionKind>, ctx: Context): StepResult {
  for (let i = 0; i < expr.args.length; i++) {
    if (!isValue(expr.args[i])) return stepIndexedChild(expr, "args", i, ctx);
  }
  if (ctx.kind === "symbolic" && expr.mode === "E" && floatDistributions.has(expr.kind)) {
    try {
      meanDistribution(expr.kind as MeanKind, expr.args.map(valueToAffine));
    } catch (error) {
      if (!isDistributionDomainError(error)) throw error;
      return out(domainErrorExpr(error, expr), ctx);
    }
  }
  if (ctx.kind === "symbolic" && expr.mode === "E" && floatDistributions.has(expr.kind)) {
    const name = `v${ctx.nextSymbol}`;
    const binding = { name, kind: expr.kind as MeanKind, args: expr.args.map(valueToAffine) };
    return out(symFloat(affineVar(name), expr.from, expr.to), {
      ...ctx,
      sigma: [...ctx.sigma, binding],
      nextSymbol: ctx.nextSymbol + 1,
    });
  }
  const streamName = expr.mode === "E" ? "rngE" : "rngG";
  const rng = ctx[streamName];
  if (!rng) {
    throw new Error(
      `the symbolic semantics keeps E draws as symbols, but ${expr.kind.toLowerCase()}[E] has no mean`,
    );
  }
  try {
    const value = sampleDistribution(expr.kind, expr.args, rng);
    return out(
      typeof value === "boolean" ? n("Bool", { value }, expr) : n("Const", { value }, expr),
      { ...ctx, [streamName]: rng },
    );
  } catch (error) {
    if (!isDistributionDomainError(error)) throw error;
    return out(domainErrorExpr(error, expr), { ...ctx, [streamName]: rng });
  }
}

function stepDiscrete(expr: ExprOf<"Discrete">, ctx: Context): StepResult {
  if (ctx.kind === "symbolic" && expr.mode === "E") {
    try {
      meanDistribution(
        "Discrete",
        expr.choices.map((choice) => affineConst(choice.probability)),
      );
    } catch (error) {
      if (!isDistributionDomainError(error)) throw error;
      return out(domainErrorExpr(error, expr), ctx);
    }
    const name = `v${ctx.nextSymbol}`;
    const binding: Binding = {
      name,
      kind: "Discrete",
      args: expr.choices.map((choice) => affineConst(choice.probability)),
    };
    return out(symFloat(affineVar(name), expr.from, expr.to), {
      ...ctx,
      sigma: [...ctx.sigma, binding],
      nextSymbol: ctx.nextSymbol + 1,
    });
  }
  const streamName = expr.mode === "E" ? "rngE" : "rngG";
  const rng = ctx[streamName] as Rng;
  try {
    const index = sampleDistribution(
      "Discrete",
      expr.choices.map((choice) => n("Const", { value: choice.probability }, expr)),
      rng,
    );
    return out(expr.choices[index].value, { ...ctx, [streamName]: rng });
  } catch (error) {
    if (!isDistributionDomainError(error)) throw error;
    return out(domainErrorExpr(error, expr), { ...ctx, [streamName]: rng });
  }
}

function stepDiscreteList(expr: ExprOf<"DiscreteList">, ctx: Context): StepResult {
  if (!isValue(expr.probabilities)) return stepChild(expr, "probabilities", ctx);
  const probabilities = listValues(expr.probabilities).map(valueToAffine);
  if (ctx.kind === "symbolic" && expr.mode === "E") {
    try {
      meanDistribution("DiscreteList", probabilities);
    } catch (error) {
      if (!isDistributionDomainError(error)) throw error;
      return out(domainErrorExpr(error, expr), ctx);
    }
    const name = `v${ctx.nextSymbol}`;
    const binding: Binding = { name, kind: "DiscreteList", args: probabilities };
    return out(symFloat(affineVar(name), expr.from, expr.to), {
      ...ctx,
      sigma: [...ctx.sigma, binding],
      nextSymbol: ctx.nextSymbol + 1,
    });
  }
  const streamName = expr.mode === "E" ? "rngE" : "rngG";
  const rng = ctx[streamName] as Rng;
  try {
    const index = sampleDistribution("DiscreteList", probabilities, rng);
    return out(n("Const", { value: index }, expr), { ...ctx, [streamName]: rng });
  } catch (error) {
    if (!isDistributionDomainError(error)) throw error;
    return out(domainErrorExpr(error, expr), { ...ctx, [streamName]: rng });
  }
}

/** The elements of a list value. */
function listValues(list: Expr): Expr[] {
  const elements: Expr[] = [];
  let rest = list;
  while (rest.kind === "Cons") {
    elements.push(rest.head);
    rest = rest.tail;
  }
  if (rest.kind !== "Nil") throw new Error("discrete probabilities are not a list");
  return elements;
}

function advanceToTarget(state: OrdinaryState, target: Expr, maxSteps: number): Advance {
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
      error: error instanceof Error ? error.message : String(error),
    };
  }
  return {
    ok: exprEqual(current.expr, target),
    state: current,
    steps,
    microTrace,
    error: exprEqual(current.expr, target)
      ? undefined
      : "ordinary trace did not reach the projected target",
  };
}

function symbolicMeanEnv(symbolicState: SymbolicState) {
  const env = new Map<string, number>();
  for (const binding of symbolicState.sigma) {
    const args = binding.args.map((arg) => affineConst(evalAffine(arg, env)));
    env.set(binding.name, affineToNumber(meanDistribution(binding.kind, args)));
  }
  return env;
}

function determinizeResidual(expr: Expr): Expr {
  switch (expr.kind) {
    case "Mean":
      return n(
        "Mean",
        { distribution: expr.distribution, args: expr.args.map(determinizeResidual) },
        expr,
      );
    case "Uniform": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return meanNode(expr.kind, args, expr);
      return n("Uniform", { mode: "G", args }, expr);
    }
    case "Gauss": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return meanNode(expr.kind, args, expr);
      return n("Gauss", { mode: "G", args }, expr);
    }
    case "Exponential": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return meanNode(expr.kind, args, expr);
      return n("Exponential", { mode: "G", args }, expr);
    }
    case "Gamma": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return meanNode(expr.kind, args, expr);
      return n("Gamma", { mode: "G", args }, expr);
    }
    case "Beta": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return meanNode(expr.kind, args, expr);
      return n("Beta", { mode: "G", args }, expr);
    }
    case "Bernoulli":
    case "Poisson": {
      const args = expr.args.map(determinizeResidual);
      if (expr.mode === "E") return meanNode(expr.kind, args, expr);
      return n(expr.kind, { mode: "G", args }, expr);
    }
    case "Discrete": {
      const choices = expr.choices.map((choice) => ({
        probability: choice.probability,
        value: determinizeResidual(choice.value),
      }));
      if (expr.mode === "E") {
        return meanNode(
          "Discrete",
          choices.map((choice) => n("Const", { value: choice.probability }, expr)),
          expr,
        );
      }
      return n("Discrete", { mode: "G", choices }, expr);
    }
    case "DiscreteList": {
      const probabilities = determinizeResidual(expr.probabilities);
      if (expr.mode === "E") return meanNode("DiscreteList", [probabilities], expr);
      return n("DiscreteList", { mode: "G", probabilities, form: expr.form }, expr);
    }
    case "Flip":
      return n("Flip", { mode: "G", args: expr.args.map(determinizeResidual) }, expr);
    default:
      return mapChildren(expr, determinizeResidual);
  }
}

function meanNode(distribution: MeanKind, args: Expr[], source: Span) {
  return n("Mean", { distribution, args }, source);
}

/**
 * The result of an arithmetic operation, or the failure of Lean's runtime: division by zero and
 * a nonfinite result fail, as in `Runtime/Eval.lean`.
 */
function arithmetic(kind: "Add" | "Sub" | "Mul" | "Div", left: Expr, right: Expr, source: Span) {
  const a = valueToAffine(left);
  const b = valueToAffine(right);
  if (kind === "Div" && isConcreteAffine(b) && b.constant === 0) {
    return failure("division by zero", source);
  }
  if (isConcreteAffine(a) && isConcreteAffine(b)) {
    // Numbers combine with the operation Lean's runtime performs; a - b is its a + -b.
    const x = a.constant;
    const y = b.constant;
    const value = kind === "Add" ? x + y : kind === "Sub" ? x + -y : kind === "Mul" ? x * y : x / y;
    if (!Number.isFinite(value)) return failure("nonfinite arithmetic result", source);
    return n("Const", { value }, source);
  }
  const result =
    kind === "Add"
      ? affineAdd(a, b)
      : kind === "Sub"
        ? affineSub(a, b)
        : kind === "Mul"
          ? affineMul(a, b)
          : affineDiv(a, b);
  if (!isFiniteAffine(result)) return failure("nonfinite arithmetic result", source);
  return floatResult(result, source);
}

function isFiniteAffine(affine: Affine) {
  return Number.isFinite(affine.constant) && Object.values(affine.terms).every(Number.isFinite);
}

/** A failure of an operation outside its domain, which ends the run as Lean's runtime does. */
function failure(message: string, source: Span): Expr {
  return n("DomainError", { message, distribution: null, reason: message }, source);
}

function floatResult(affine: Affine, source: Span): Expr {
  if (Object.keys(affine.terms).length === 0) return n("Const", { value: affine.constant }, source);
  return symFloat(affine, source.from, source.to);
}

function stepChild<K extends string>(expr: Expr & Record<K, Expr>, key: K, ctx: Context) {
  const result = step(expr[key], ctx);
  if (isTerminalError(result.expr)) return out(result.expr, { ...ctx, ...contextPatch(result) });
  return rebuild(expr, { [key]: result.expr }, ctx, result);
}

function stepIndexedChild<K extends string>(
  expr: Expr & Record<K, Expr[]>,
  key: K,
  index: number,
  ctx: Context,
) {
  const result = step(expr[key][index], ctx);
  if (isTerminalError(result.expr)) return out(result.expr, { ...ctx, ...contextPatch(result) });
  const next = expr[key].slice();
  next[index] = result.expr;
  return rebuild(expr, { [key]: next }, ctx, result);
}

function isTerminalError(expr: Expr) {
  return expr.kind === "Reject" || expr.kind === "DomainError";
}

function rebuild(expr: Expr, patch: Record<string, unknown>, ctx: Context, result: StepResult) {
  return out(n(expr.kind, { ...copyProps(expr), ...patch }, expr), {
    ...ctx,
    ...contextPatch(result),
  });
}

function contextPatch(result: ContextPatch): ContextPatch {
  const patch: Record<string, unknown> = {};
  for (const key of ["rngE", "rngG", "sigma", "nextSymbol"] as const)
    if (key in result) patch[key] = result[key];
  return patch as ContextPatch;
}

function out(expr: Expr, ctx: ContextPatch): StepResult {
  return { expr, ...contextPatch(ctx) };
}

function copyProps(expr: Expr) {
  const props: Record<string, unknown> = { ...expr };
  delete props.kind;
  delete props.from;
  delete props.to;
  return props;
}

function subst(expr: Expr, name: string, replacement: Expr): Expr {
  switch (expr.kind) {
    case "Var":
      return expr.name === name ? clone(replacement) : clone(expr);
    case "Lam":
      return expr.param === name
        ? clone(expr)
        : n("Lam", { param: expr.param, body: subst(expr.body, name, replacement) }, expr);
    case "Rec":
      return expr.name === name || expr.param === name
        ? clone(expr)
        : n(
            "Rec",
            { name: expr.name, param: expr.param, body: subst(expr.body, name, replacement) },
            expr,
          );
    case "Let":
      return n(
        "Let",
        {
          name: expr.name,
          value: subst(expr.value, name, replacement),
          body: expr.name === name ? clone(expr.body) : subst(expr.body, name, replacement),
        },
        expr,
      );
    case "Case":
      return n(
        "Case",
        {
          scrutinee: subst(expr.scrutinee, name, replacement),
          leftName: expr.leftName,
          left: expr.leftName === name ? clone(expr.left) : subst(expr.left, name, replacement),
          rightName: expr.rightName,
          right: expr.rightName === name ? clone(expr.right) : subst(expr.right, name, replacement),
        },
        expr,
      );
    case "MatchList":
      return n(
        "MatchList",
        {
          scrutinee: subst(expr.scrutinee, name, replacement),
          nilBranch: subst(expr.nilBranch, name, replacement),
          headName: expr.headName,
          tailName: expr.tailName,
          consBranch:
            expr.headName === name || expr.tailName === name
              ? clone(expr.consBranch)
              : subst(expr.consBranch, name, replacement),
        },
        expr,
      );
    default:
      return mapChildren(expr, (child) => subst(child, name, replacement));
  }
}

function mapChildren(expr: Expr, f: (child: Expr) => Expr): Expr {
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
      return n(
        "If",
        { cond: f(expr.cond), thenBranch: f(expr.thenBranch), elseBranch: f(expr.elseBranch) },
        expr,
      );
    case "Case":
      return n(
        "Case",
        {
          scrutinee: f(expr.scrutinee),
          leftName: expr.leftName,
          left: f(expr.left),
          rightName: expr.rightName,
          right: f(expr.right),
        },
        expr,
      );
    case "MatchList":
      return n(
        "MatchList",
        {
          scrutinee: f(expr.scrutinee),
          nilBranch: f(expr.nilBranch),
          headName: expr.headName,
          tailName: expr.tailName,
          consBranch: f(expr.consBranch),
        },
        expr,
      );
    case "Observe":
      return n("Observe", { cond: f(expr.cond) }, expr);
    case "Mean":
      return n("Mean", { distribution: expr.distribution, args: expr.args.map(f) }, expr);
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
      return n(
        "Discrete",
        {
          mode: expr.mode,
          choices: expr.choices.map((choice) => ({
            probability: choice.probability,
            value: f(choice.value),
          })),
        },
        expr,
      );
    case "DiscreteList":
      return n(
        "DiscreteList",
        { mode: expr.mode, probabilities: f(expr.probabilities), form: expr.form },
        expr,
      );
    default:
      return clone(expr);
  }
}

function concretize(expr: Expr, env: Map<string, number>): Expr {
  switch (expr.kind) {
    case "SymFloat":
      return n("Const", { value: evalAffine(expr.affine, env) }, expr);
    case "DomainError":
      return clone(expr);
    default:
      return mapChildren(expr, (child) => concretize(child, env));
  }
}

export function isValue(expr: Expr): boolean {
  return (
    expr.kind === "Reject" ||
    expr.kind === "DomainError" ||
    (expr.kind === "Const" && Number.isFinite(expr.value)) ||
    expr.kind === "SymFloat" ||
    expr.kind === "Bool" ||
    expr.kind === "Unit" ||
    expr.kind === "Lam" ||
    expr.kind === "Rec" ||
    expr.kind === "Nil" ||
    (expr.kind === "Pair" && isValue(expr.left) && isValue(expr.right)) ||
    (expr.kind === "Inl" && isValue(expr.expr)) ||
    (expr.kind === "Inr" && isValue(expr.expr)) ||
    (expr.kind === "Cons" && isValue(expr.head) && isValue(expr.tail))
  );
}

function numberValue(expr: Expr) {
  return affineToNumber(valueToAffine(expr));
}

export function exprEqual(a: Expr, b: Expr, eps = 1e-9): boolean {
  if (a.kind !== b.kind) return false;
  switch (a.kind) {
    case "Const":
      // Equal infinities, which nonfinite literals hold, are equal too.
      return a.value === (b as typeof a).value || Math.abs(a.value - (b as typeof a).value) <= eps;
    case "Bool":
      return a.value === (b as typeof a).value;
    case "Unit":
    case "Nil":
    case "Reject":
      return true;
    case "DomainError":
      return a.message === (b as typeof a).message;
    case "Pair":
      return (
        exprEqual(a.left, (b as typeof a).left, eps) &&
        exprEqual(a.right, (b as typeof a).right, eps)
      );
    case "Inl":
    case "Inr":
      return exprEqual(a.expr, (b as typeof a).expr, eps);
    case "Cons":
      return (
        exprEqual(a.head, (b as typeof a).head, eps) && exprEqual(a.tail, (b as typeof a).tail, eps)
      );
    case "SymFloat":
      return prettyAffine(a.affine) === prettyAffine((b as typeof a).affine);
    default:
      return prettyExpr(a) === prettyExpr(b);
  }
}

function valuesEqual(a: Expr, b: Expr, eps = 1e-9) {
  return exprEqual(a, b, eps);
}

function domainErrorExpr(error: DistributionDomainError, source: Span | undefined) {
  return n(
    "DomainError",
    {
      message: error.message,
      distribution: error.kind ?? null,
      reason: error.reason ?? error.message,
    },
    source ?? { from: 0, to: 0 },
  );
}

export function prettySymbolicState(state: { sigma: Binding[]; expr: Expr }) {
  const sigma =
    state.sigma.length === 0
      ? "empty"
      : state.sigma
          .map(
            (binding) =>
              `${binding.name} ~ ${distributionName(binding.kind)}(${binding.args.map(prettyAffine).join(", ")})`,
          )
          .join("; ");
  return `<${sigma} || ${prettyExpr(state.expr)}>`;
}

/** A deep copy of plain data, which keeps nonfinite numbers, -0 and BigInts. */
function deepCopy<T>(value: T): T {
  if (Array.isArray(value)) return value.map(deepCopy) as T;
  if (value === null || typeof value !== "object") return value;
  const copy: Record<string, unknown> = {};
  for (const [key, field] of Object.entries(value)) copy[key] = deepCopy(field);
  return copy as T;
}

function clone(expr: Expr): Expr {
  if (expr.kind === "SymFloat") return symFloat(expr.affine, expr.from, expr.to);
  return deepCopy(expr);
}

function cloneBinding(binding: Binding) {
  return deepCopy(binding);
}

function n<K extends Expr["kind"]>(
  kind: K,
  props: Omit<ExprOf<K>, "kind" | "from" | "to">,
  source: Span,
): ExprOf<K> {
  return node(kind, props, source.from ?? 0, source.to ?? source.from ?? 0);
}
