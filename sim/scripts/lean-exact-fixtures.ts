// Records the Lean CLI's exact finite-model results in test/fixtures/lean-exact.json, which the
// tests of the finite-model port compare with. For every accepted case of the corpus manifest
// (tests/cases.toml), both subjects, with and without `--additive`: the fields of
// `PREFIX.result.json`, or the message the CLI prints instead. Then a few runs with smaller
// limits, among them, for some programs, the largest state each explores: the smallest
// `--max-state-bytes` at which no state is too large, and the run one byte below it. Run it in
// the sim shell after a change of Lean's finite models or front end, or of the manifest:
//
//   node scripts/lean-exact-fixtures.ts
//
// It builds the CLI once through ./run.sh (in the lean shell if lake is not on PATH) and then
// calls the binary that ./run.sh runs, several at a time.
import { execFile, execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from "node:fs";
import { availableParallelism, tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { parse } from "smol-toml";
import type {
  ExactCase,
  ExactFixture,
  ExactLimitCase,
  ExactLimits,
  ExactOutcome,
  ExactResultJson,
  ExactSubject,
} from "../test/lean-exact.ts";

const root = fileURLToPath(new URL("../../", import.meta.url));
const output = new URL("../test/fixtures/lean-exact.json", import.meta.url);
const binary = `${root}lean/.lake/build/bin/determinize`;
const scratch = mkdtempSync(join(tmpdir(), "lean-exact-"));

/** Runs with smaller limits: each exercises one limit or message of the port. */
const limitRuns: Omit<ExactLimitCase, "outcome">[] = [
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: false,
    limits: { maxStates: 30 },
  },
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: true,
    limits: { maxStates: 30 },
  },
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: false,
    limits: { maxEdges: 40 },
  },
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: true,
    limits: { maxEdges: 40 },
  },
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: false,
    limits: { maxResultStates: 67 },
  },
  {
    file: "examples/loops/geometric-addition.det",
    subject: "source",
    additive: true,
    limits: { maxResultStates: 44 },
  },
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: false,
    limits: { maxStates: 0 },
  },
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: true,
    limits: { maxStates: 0 },
  },
  {
    file: "examples/paper/dungeon.det",
    subject: "determinized",
    additive: true,
    limits: { maxEdges: 500 },
  },
];

/** Programs whose largest explored state is recorded, with the limits of the run. */
const byteRuns: Omit<ExactLimitCase, "outcome">[] = [
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: false,
    limits: {},
  },
  {
    file: "examples/paper/noisy-iteration.det",
    subject: "determinized",
    additive: true,
    limits: {},
  },
  {
    file: "examples/loops/geometric-addition.det",
    subject: "source",
    additive: true,
    limits: {},
  },
  {
    file: "examples/symbolic/recursion-and-list.det",
    subject: "determinized",
    additive: false,
    limits: {},
  },
  {
    file: "examples/simulator/random-list-sum.det",
    subject: "determinized",
    additive: false,
    limits: {},
  },
  {
    file: "examples/paper/dungeon.det",
    subject: "determinized",
    additive: false,
    limits: { maxStates: 2000 },
  },
];

function cli(args: string[]): Promise<{ ok: boolean; out: string }> {
  return new Promise((resolve, reject) => {
    execFile(binary, args, { cwd: root, encoding: "utf8" }, (error, stdout, stderr) => {
      if (error && typeof error.code !== "number") reject(error);
      else resolve({ ok: !error, out: error ? stderr.trim() : stdout });
    });
  });
}

let runCount = 0;

async function exact(
  file: string,
  subject: ExactSubject,
  additive: boolean,
  limits: ExactLimits,
): Promise<ExactOutcome> {
  const prefix = join(scratch, String(runCount++));
  const args = ["--check", "--result", prefix, "--subject", subject];
  if (additive) args.push("--additive");
  if (limits.maxStates !== undefined) args.push("--max-states", String(limits.maxStates));
  if (limits.maxEdges !== undefined) args.push("--max-edges", String(limits.maxEdges));
  if (limits.maxStateBytes !== undefined) {
    args.push("--max-state-bytes", String(limits.maxStateBytes));
  }
  if (limits.maxResultStates !== undefined) {
    args.push("--max-result-states", String(limits.maxResultStates));
  }
  const { ok, out } = await cli([...args, file]);
  if (!ok) return { message: out };
  const result = JSON.parse(readFileSync(`${prefix}.result.json`, "utf8")) as ExactResultJson;
  for (const suffix of [
    ".candidate.lean",
    ".replay.lean",
    ".result.lean",
    ".result.json",
    ".tra",
    ".lab",
    ".positive.state.rew",
    ".negative.state.rew",
  ]) {
    rmSync(`${prefix}${suffix}`, { force: true });
  }
  return { result };
}

/** Runs `tasks` with at most `width` in flight, keeping their order. */
async function pool<T>(tasks: (() => Promise<T>)[], width: number): Promise<T[]> {
  const results: T[] = new Array(tasks.length);
  let next = 0;
  async function worker() {
    while (next < tasks.length) {
      const index = next++;
      results[index] = await tasks[index]();
    }
  }
  await Promise.all(Array.from({ length: Math.min(width, tasks.length) }, worker));
  return results;
}

function tooLarge(outcome: ExactOutcome) {
  return "message" in outcome && outcome.message.includes("Limit.stateBytes");
}

/** The two runs around the smallest `--max-state-bytes` at which no explored state is too large:
 * one byte below it, where one is, and at it. */
async function largestState(run: Omit<ExactLimitCase, "outcome">): Promise<ExactLimitCase[]> {
  const at = (bytes: number) =>
    exact(run.file, run.subject, run.additive, { ...run.limits, maxStateBytes: bytes });
  let low = 0;
  let high = 1000000;
  if (tooLarge(await at(high))) throw new Error(`${run.file}: a state exceeds the default limit`);
  while (high - low > 1) {
    const middle = Math.floor((low + high) / 2);
    if (tooLarge(await at(middle))) low = middle;
    else high = middle;
  }
  return [low, high].map((bytes) => ({
    ...run,
    limits: { ...run.limits, maxStateBytes: bytes },
    outcome: { message: "" },
  }));
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
const files = manifest.case
  .map((entry) => entry as { file: string; outcome: string })
  .filter((entry) => entry.outcome === "accept")
  .map((entry) => entry.file);
const width = availableParallelism();
const subjects: ExactSubject[] = ["source", "determinized"];
const outcomes = await pool(
  files.flatMap((file) =>
    [false, true].flatMap((additive) =>
      subjects.map((subject) => () => exact(file, subject, additive, {})),
    ),
  ),
  width,
);
const cases: ExactCase[] = files.map((file, i) => ({
  file,
  plain: { source: outcomes[4 * i], determinized: outcomes[4 * i + 1] },
  additive: { source: outcomes[4 * i + 2], determinized: outcomes[4 * i + 3] },
}));
const bounds = (
  await pool(
    byteRuns.map((run) => () => largestState(run)),
    width,
  )
).flat();
const limited = [...limitRuns, ...bounds];
const limitOutcomes = await pool(
  limited.map((run) => () => exact(run.file, run.subject, run.additive, run.limits)),
  width,
);
const fixture: ExactFixture = {
  cases,
  limits: limited.map((run, i) => ({ ...run, outcome: limitOutcomes[i] })),
};
rmSync(scratch, { recursive: true, force: true });
writeFileSync(output, `${JSON.stringify(fixture, null, 2)}\n`);
const finite = outcomes.filter((outcome) => "result" in outcome).length;
console.log(
  `${files.length} accepted cases recorded: ${finite} of ${outcomes.length} results finite, ` +
    `${fixture.limits.length} runs with smaller limits`,
);
