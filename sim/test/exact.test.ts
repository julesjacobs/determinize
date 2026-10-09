// The port of Lean's finite models against the Lean CLI: for every accepted case of the corpus
// manifest, both subjects, without `--additive`, test/fixtures/lean-exact.json records the fields of
// `--result`'s `.result.json` or the message the CLI prints instead
// (scripts/lean-exact-fixtures.ts). The port must give the same fractions, state counts and
// messages, also with the fixture's smaller limits.
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import { analyze } from "../src/core/compiler/analyze.ts";
import type { Limits, Subject } from "../src/core/finite/explore.ts";
import {
  defaultLimits,
  explorationMessage,
  explore,
  subjectProgram,
} from "../src/core/finite/explore.ts";
import { defaultSolveStates } from "../src/core/finite/solve.ts";
import { resultFields, solveStatistics } from "../src/core/finite/statistics.ts";
import type { ExactFixture, ExactLimits, ExactOutcome } from "./lean-exact.ts";

const root = new URL("../../", import.meta.url);
const fixture: ExactFixture = JSON.parse(
  readFileSync(new URL("fixtures/lean-exact.json", import.meta.url), "utf8"),
);

function programsOf(file: string) {
  const analysis = analyze(readFileSync(new URL(file, root), "utf8"));
  if (!analysis.ok) throw new Error(`${file}: the simulator rejects a case Lean accepts`);
  return analysis.program;
}

/** What the port gives for `file`, in the fixture's form: the fields of `.result.json`, of which
 * the port computes those of `resultFields`, or the CLI's message. */
function outcome(file: string, subject: Subject, limits: ExactLimits = {}): ExactOutcome {
  const programs = programsOf(file);
  const bounds: Limits = {
    maxStates: limits.maxStates ?? defaultLimits.maxStates,
    maxEdges: limits.maxEdges ?? defaultLimits.maxEdges,
    maxStateBytes: limits.maxStateBytes ?? defaultLimits.maxStateBytes,
  };
  const program = subjectProgram(programs, subject);
  const solveStates = limits.maxResultStates ?? defaultSolveStates;
  const exploration = explore(program, programs.source, bounds);
  if (exploration.kind !== "complete") return { message: explorationMessage(exploration) };
  const solved = solveStatistics(exploration.model, solveStates);
  if (!solved.ok) return { message: solved.message };
  return {
    result: {
      ...resultFields(solved.result),
      subject,
      kernel_checked: false,
      certificate_status: "generated",
      termination_statistics_scope: "graph",
    },
  };
}

for (const entry of fixture.cases) {
  for (const subject of ["source", "determinized"] as const) {
    test(`${entry.file}, ${subject}`, () => {
      assert.deepEqual(outcome(entry.file, subject), entry.plain[subject]);
    });
  }
}

for (const entry of fixture.limits.filter((run) => !run.additive)) {
  const { file, subject, limits } = entry;
  test(`${file}, ${subject}, limits ${JSON.stringify(limits)}`, () => {
    assert.deepEqual(outcome(file, subject, limits), entry.outcome);
  });
}

test("the fixture has the reference results of SPEC §2.10", () => {
  const find = (file: string) => {
    const found = fixture.cases.find((entry) => entry.file === file);
    if (!found) throw new Error(`${file} isn't in the fixture`);
    return found;
  };
  const noisy = find("examples/paper/noisy-iteration.det");
  assert.deepEqual(noisy.plain.source, {
    message:
      "Exploration failed at state 34: unsupported execution: stochastic Determinize.Spec.Paper.Op.gaussian. No export written.",
  });
  const determinized = noisy.plain.determinized;
  assert.ok("result" in determinized);
  const { states, answer, return_mass, rejection_probability } = determinized.result;
  const { conditional_mean, second_moment, conditional_variance } = determinized.result;
  assert.deepEqual(
    [
      states,
      answer,
      return_mass,
      rejection_probability,
      conditional_mean,
      second_moment,
      conditional_variance,
    ],
    [68, "1/3", "1", "0", "1/3", "1/3", "2/9"],
  );
  for (const mode of ["plain", "additive"] as const) {
    const dungeon = find("examples/paper/dungeon.det")[mode].determinized;
    assert.ok("message" in dungeon && dungeon.message.includes("Limit.states"));
  }
  const loops = [
    ["examples/loops/geometric-addition.det", "1", "1", "3", "2"],
    ["examples/loops/geometric-random-increment.det", "1", "2", "13", "9"],
    ["examples/loops/geometric-rejection.det", "1/2", "1/2", "3/2", "2"],
  ];
  for (const [file, mass, first, second, variance] of loops) {
    const outcome = find(file).additive.source;
    assert.ok("result" in outcome, file);
    const { return_mass, answer, second_moment, conditional_variance } = outcome.result;
    assert.deepEqual(
      [return_mass, answer, second_moment, conditional_variance],
      [mass, first, second, variance],
    );
  }
});
