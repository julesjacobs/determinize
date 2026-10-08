import type { Expr } from "./compiler/ast.ts";
import { prettyExpr } from "./compiler/pretty.ts";
import { affineConst, affineToNumber, evalAffine } from "./runtime/affine.ts";
import { meanDistribution } from "./runtime/distributions.ts";
import type { Binding, CoupledTrace, Frame } from "./runtime/semantics.ts";
import { isValue, runCoupledTrace } from "./runtime/semantics.ts";
import type { RunOutcome } from "./statistics.ts";

/**
 * The step table's limits. A run stops after `maxSymbolicSteps` symbolic steps, as Lean's
 * evaluator stops after its fuel of 100000 evaluated subexpressions. Every program of
 * examples/loops/ ends within it at seeds 1-20; the random walk takes up to 12994 steps there.
 */
export const maxSymbolicSteps = 20000;
/**
 * The page computes the step table itself, and each step costs as much as its state is large, so
 * a run also stops once its states' sizes (nodes and affine terms) add up to `maxShownSize`; a deep
 * recursion or a long sum of draws would otherwise hold the page for seconds and exhaust its
 * memory. At seeds 1-20, the random walk's states add up to 534883, the most of examples/, and the
 * Irwin-Hall sum's to 255191.
 */
export const maxShownSize = 800000;
/** The steps of either program towards each symbolic step; every program of the corpus needs at
 * most one. */
export const maxSyncSteps = 200;

/**
 * The step table's run of a program at a seed. It throws for a program that Lean rejects for
 * another reason than a mode conflict.
 */
export function runCoupling(source: string, seed: number): CoupledTrace {
  return runCoupledTrace(source, seed, maxSymbolicSteps, maxSyncSteps, maxShownSize);
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
