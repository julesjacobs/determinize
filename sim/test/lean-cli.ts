// The shape of test/fixtures/lean-cli.json, which scripts/lean-fixtures.ts writes from the Lean
// CLI's output.

/** What the CLI prints about the runs of one program, as `summarize` in lean/Main.lean prints it;
 * the numbers are the CLI's text. */
export interface CliSummary {
  returned: number;
  samples: number;
  mean?: string;
  variance?: string;
  firstValue?: string;
  rejected: number;
  firstFailure?: string;
}

/** What the CLI prints for an accepted program. */
export interface CliCase {
  file: string;
  checked: string;
  sites: {
    before: { discrete: number; continuous: number };
    after: { discrete: number; continuous: number };
  };
  annotated: string;
  determinized: string;
  runs: { seed: string; samples: number; source: CliSummary; determinized: CliSummary }[];
}

export interface CliFixture {
  runs: { seed: string; samples: number }[];
  accepted: CliCase[];
  rejected: { file: string; message: string }[];
}
