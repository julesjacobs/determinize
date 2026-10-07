import type { Expr } from "./compiler/ast.ts";
import { affineConst, affineToNumber, evalAffine } from "./runtime/affine.ts";
import { meanDistribution } from "./runtime/distributions.ts";
import type { Binding, CoupledTrace, Frame } from "./runtime/semantics.ts";
import { runCoupledTrace } from "./runtime/semantics.ts";

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

/** The results of a run's source and determinized program. */
export interface Sample {
  original: number;
  determinized: number;
}

function numericValue(expr: Expr | undefined) {
  return expr?.kind === "Const" ? expr.value : undefined;
}

/** The results that a run adds to the distributions, if both are finite numbers. */
export function sampleOf(trace: CoupledTrace): Sample | null {
  const finalFrame = trace.frames.at(-1);
  const original = numericValue(finalFrame?.original) ?? numericValue(trace.finalOriginal);
  const determinized =
    numericValue(finalFrame?.determinized) ?? numericValue(trace.finalDeterminized);
  if (original === undefined || determinized === undefined) return null;
  if (!Number.isFinite(original) || !Number.isFinite(determinized)) return null;
  return { original, determinized };
}

/** The runs of a batch: how many ran, their samples, and the last run. */
export interface Batch {
  runs: number;
  original: number[];
  determinized: number[];
  last: CoupledTrace | null;
}

/** The runs at `seeds`, up to the first that throws. */
export function runBatch(source: string, seeds: Iterable<number>): Batch {
  const batch: Batch = { runs: 0, original: [], determinized: [], last: null };
  for (const seed of seeds) {
    let trace: CoupledTrace;
    try {
      trace = runCoupling(source, seed);
    } catch {
      break;
    }
    batch.runs += 1;
    batch.last = trace;
    const sample = sampleOf(trace);
    if (sample) {
      batch.original.push(sample.original);
      batch.determinized.push(sample.determinized);
    }
  }
  return batch;
}
