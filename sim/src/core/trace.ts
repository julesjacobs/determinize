import type { Expr } from "./compiler/ast.ts";
import { prettyExpr } from "./compiler/pretty.ts";
import { affineConst, affineToNumber, evalAffine } from "./runtime/affine.ts";
import { meanDistribution } from "./runtime/distributions.ts";
import type { Binding, CoupledTrace, Frame } from "./runtime/semantics.ts";
import { isValue, runCoupledTrace } from "./runtime/semantics.ts";
import type { Runs } from "./statistics.ts";

/** The step table's limits: symbolic steps, and steps of either program per symbolic step. */
export const maxSymbolicSteps = 1000;
export const maxSyncSteps = 200;

/**
 * The step table's run of a program at a seed. It throws for a program that Lean rejects for
 * another reason than a mode conflict.
 */
export function runCoupling(source: string, seed: number): CoupledTrace {
  return runCoupledTrace(source, seed, maxSymbolicSteps, maxSyncSteps);
}

/** Whether every check of a frame passed. */
export function frameOk(frame: Frame) {
  return (
    frame.originalOk &&
    frame.determinizedOk &&
    frame.symbolicOk !== false &&
    frame.consistencyOk !== false
  );
}

export function hasDomainError(frame: Frame) {
  return (
    frame.original?.kind === "DomainError" ||
    frame.symbolic?.kind === "DomainError" ||
    frame.determinized?.kind === "DomainError"
  );
}

/** The message of the first domain error among the source, symbolic and determinized states. */
export function domainErrorMessage(frame: Frame) {
  for (const expr of [frame.original, frame.symbolic, frame.determinized]) {
    if (expr?.kind === "DomainError") return expr.message;
  }
  return "domain error";
}

/** A draw of σ with its mean, or the error that computing the mean raised. */
export interface SigmaMean {
  binding: Binding;
  mean: number;
  error: string | null;
}

/** The mean of each draw of σ, in order; a draw's arguments refer to the earlier draws' means. */
export function sigmaMeans(sigma: Binding[]): SigmaMean[] {
  const env = new Map<string, number>();
  return sigma.map((binding) => {
    try {
      const meanArgs = binding.args.map((arg) => affineConst(evalAffine(arg, env)));
      const mean = affineToNumber(meanDistribution(binding.kind, meanArgs));
      env.set(binding.name, mean);
      return { binding, mean, error: null };
    } catch (error) {
      env.set(binding.name, NaN);
      return { binding, mean: NaN, error: error instanceof Error ? error.message : String(error) };
    }
  });
}

/** How a run of one program ended: it returned a value, the number it returned if any, or it
 * was rejected by an observation, or it failed. */
export type RunOutcome =
  | { kind: "returned"; number: number | null; display: string }
  | { kind: "rejected" }
  | { kind: "failed"; message: string };

function outcomeOf(expr: Expr | undefined): RunOutcome {
  if (!expr || !isValue(expr)) return { kind: "failed", message: "step limit reached" };
  if (expr.kind === "Reject") return { kind: "rejected" };
  if (expr.kind === "DomainError") return { kind: "failed", message: expr.message };
  return {
    kind: "returned",
    number: expr.kind === "Const" ? expr.value : null,
    display: prettyExpr(expr),
  };
}

/** How the run of the source and of the determinized program ended. */
export function outcomesOf(trace: CoupledTrace): { source: RunOutcome; determinized: RunOutcome } {
  const finalFrame = trace.frames.at(-1);
  const final = (expr: Expr | undefined, fallback: Expr | undefined) =>
    expr && isValue(expr) ? expr : fallback;
  return {
    source: outcomeOf(final(finalFrame?.original, trace.finalOriginal)),
    determinized: outcomeOf(final(finalFrame?.determinized, trace.finalDeterminized)),
  };
}

/** Outcomes of runs, in run order, as `Runs`. */
export function runsOf(outcomes: RunOutcome[]): Runs {
  const runs: Runs = {
    values: new Float64Array(outcomes.length),
    rejected: 0,
    failed: 0,
    firstFailure: null,
    firstValue: null,
  };
  for (const [i, outcome] of outcomes.entries()) {
    runs.values[i] = outcome.kind === "returned" && outcome.number !== null ? outcome.number : NaN;
    if (outcome.kind === "rejected") runs.rejected += 1;
    if (outcome.kind === "failed") {
      runs.failed += 1;
      runs.firstFailure ??= outcome.message;
    }
    if (outcome.kind === "returned") runs.firstValue ??= outcome.display;
  }
  return runs;
}
