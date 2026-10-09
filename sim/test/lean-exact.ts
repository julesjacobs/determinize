// The shape of test/fixtures/lean-exact.json, which scripts/lean-exact-fixtures.ts writes from the
// Lean CLI's `--result` output.

/** The fields of `PREFIX.result.json`, as `Finite.writeResult` (and `Finite.Reward.writeResult`
 * with `--additive`) writes them: fractions as text, `null` for an undefined conditional moment. */
export interface ExactResultJson {
  answer: string;
  return_mass: string;
  rejection_probability: string;
  divergence_probability: string;
  second_moment: string;
  conditional_mean: string | null;
  conditional_variance: string | null;
  states: number;
  rank_bound: number;
  subject: "source" | "determinized";
  mode?: "additive";
  kernel_checked: boolean;
  certificate_status: string;
  termination_statistics_scope: string;
}

/** The CLI's result for one program: the result file, or the message it printed instead. */
export type ExactOutcome = { result: ExactResultJson } | { message: string };

export type ExactSubject = "source" | "determinized";

/** The limits of `--max-states`, `--max-edges`, `--max-state-bytes` and `--max-result-states`;
 * absent ones keep the CLI's defaults. */
export interface ExactLimits {
  maxStates?: number;
  maxEdges?: number;
  maxStateBytes?: number;
  maxResultStates?: number;
}

export interface ExactCase {
  file: string;
  plain: Record<ExactSubject, ExactOutcome>;
  additive: Record<ExactSubject, ExactOutcome>;
}

/** A run with limits other than the defaults. */
export interface ExactLimitCase {
  file: string;
  subject: ExactSubject;
  additive: boolean;
  limits: ExactLimits;
  outcome: ExactOutcome;
}

export interface ExactFixture {
  cases: ExactCase[];
  limits: ExactLimitCase[];
}
