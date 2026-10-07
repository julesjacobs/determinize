// Records what the Lean CLI prints for every case of the corpus manifest (tests/cases.toml) in
// test/fixtures/lean-cli.json, which the tests of the evaluator port and the printer compare
// with. For each accepted case: the checked type, the sample-site counts, the printed programs
// and the statistics of `--seed S --samples N` for each entry of `runs`; for each rejected case:
// Lean's message. Run it in the sim shell after a change of Lean's runtime or front end, or of the
// manifest:
//
//   node scripts/lean-fixtures.ts
//
// It builds the CLI once through ./run.sh (in the lean shell if lake is not on PATH) and then
// calls the binary that ./run.sh runs.
import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { parse } from "smol-toml";
import type { CliCase, CliFixture, CliSummary } from "../test/lean-cli.ts";

const root = fileURLToPath(new URL("../../", import.meta.url));
const output = new URL("../test/fixtures/lean-cli.json", import.meta.url);
const binary = `${root}lean/.lake/build/bin/determinize`;

/** The sampling runs of every accepted case: the CLI's seed, as a decimal UInt64, and count. The
 * second wraps around 2⁶⁴. */
const runs = [
  { seed: "1", samples: 500 },
  { seed: "18446744073709551614", samples: 4 },
];

function cli(args: string[]) {
  try {
    return {
      ok: true,
      out: execFileSync(binary, args, {
        cwd: root,
        encoding: "utf8",
        stdio: ["ignore", "pipe", "pipe"],
      }),
    } as const;
  } catch (error) {
    const stderr = (error as { stderr?: unknown }).stderr;
    if (typeof stderr !== "string") throw error;
    return { ok: false, out: stderr.trim() } as const;
  }
}

/** The lines of `out` after the line `heading`, up to the next one that isn't indented. */
function summary(lines: string[], heading: string): CliSummary {
  const start = lines.findIndex((line) => line.startsWith(`${heading}: `));
  const counts = lines[start]?.match(/^\S+: (\d+)\/(\d+) runs returned a value$/);
  if (!counts) throw new Error(`no summary for ${heading}`);
  const result: CliSummary = {
    returned: Number(counts[1]),
    samples: Number(counts[2]),
    rejected: 0,
  };
  for (const line of lines.slice(start + 1)) {
    if (!line.startsWith("  ")) break;
    const statistics = line.match(
      /^ {2}empirical mean among returned values: (.*); variance: (.*)$/,
    );
    const first = line.match(/^ {2}first value: (.*)$/);
    const rejected = line.match(/^ {2}rejected observations: (\d+)$/);
    const failure = line.match(/^ {2}first failure: (.*)$/);
    if (statistics) {
      result.mean = statistics[1];
      result.variance = statistics[2];
    } else if (first) result.firstValue = first[1];
    else if (rejected) result.rejected = Number(rejected[1]);
    else if (failure) result.firstFailure = failure[1];
    else throw new Error(`unexpected line: ${line}`);
  }
  return result;
}

/** The value of `Name: value` or of the line after `Name:`. */
function field(lines: string[], name: string, nextLine = false) {
  const index = lines.findIndex((line) => line.startsWith(`${name}:`));
  if (index < 0) throw new Error(`no ${name}`);
  return nextLine ? lines[index + 1] : lines[index].slice(name.length + 2);
}

function sites(lines: string[], when: string) {
  const match = field(lines, `Sampling sites ${when} determinization`).match(
    /^discrete=(\d+), continuous=(\d+)$/,
  );
  if (!match) throw new Error(`no site counts ${when} determinization`);
  return { discrete: Number(match[1]), continuous: Number(match[2]) };
}

function acceptedCase(file: string): CliCase {
  let printed: Omit<CliCase, "runs"> | null = null;
  const summaries: CliCase["runs"] = [];
  for (const { seed, samples } of runs) {
    const result = cli(["--seed", seed, "--samples", String(samples), "--sample-sites", file]);
    if (!result.ok) throw new Error(`${file}: ${result.out}`);
    const lines = result.out.split("\n");
    printed ??= {
      file,
      checked: field(lines, "Checked"),
      sites: { before: sites(lines, "before"), after: sites(lines, "after") },
      annotated: field(lines, "Annotated source", true),
      determinized: field(lines, "Determinized", true),
    };
    summaries.push({
      seed,
      samples,
      source: summary(lines, "Source"),
      determinized: summary(lines, "Determinized"),
    });
  }
  if (!printed) throw new Error(`${file}: no runs`);
  return { ...printed, runs: summaries };
}

execFileSync(
  "bash",
  [
    "-c",
    'ROOT="$1"; source "$ROOT/tools/dev-shell.sh"; in_shell lean "$ROOT/run.sh" --help',
    "-",
    root,
  ],
  { stdio: ["ignore", "ignore", "inherit"] },
);

const manifest = parse(readFileSync(`${root}tests/cases.toml`, "utf8"));
if (!Array.isArray(manifest.case)) throw new Error("the manifest has no cases");
const fixture: CliFixture = { runs, accepted: [], rejected: [] };
for (const entry of manifest.case) {
  const { file, outcome } = entry as { file: string; outcome: string };
  if (outcome === "accept") {
    fixture.accepted.push(acceptedCase(file));
  } else {
    const result = cli(["--check", file]);
    if (result.ok) throw new Error(`${file}: Lean accepts a case the manifest rejects`);
    fixture.rejected.push({ file, message: result.out });
  }
}
writeFileSync(output, `${JSON.stringify(fixture, null, 2)}\n`);
console.log(
  `${fixture.accepted.length} accepted and ${fixture.rejected.length} rejected cases recorded`,
);
