// Exact values of both programs from their finite models: the exploration and statistics of the
// port of Lean's `Finite/*`, of the source and then of the determinized program, plain or in the
// additive mode. Exploration and the solve run in slices of about 50 ms, after each of which the
// thread they run on handles messages and gets the progress; a newer request replaces an older one.
// The exact worker runs them, and so does the page where no worker can start.
import { analyze } from "./compiler/analyze.ts";
import type { Program } from "./compiler/core.ts";
import type { Exploration, Limit, Subject } from "./finite/explore.ts";
import { defaultLimits, explorationMessage, explorer } from "./finite/explore.ts";
import type { Failure } from "./finite/machine.ts";
import type { Rational } from "./finite/rational.ts";
import { fraction, toNumber } from "./finite/rational.ts";
import type { RewardExploration } from "./finite/reward.ts";
import { rewardExplorer, rewardStatisticsSteps } from "./finite/reward.ts";
import { defaultSolveStates } from "./finite/solve.ts";
import type { Solved } from "./finite/statistics.ts";
import { statisticsSteps } from "./finite/statistics.ts";
import type { ExactRequest, ExactResponse } from "./protocol.ts";
import { sliceMs } from "./sampler.ts";

/** An exact number: Lean's fraction, as `.result.json` writes it, and its nearest double. */
export interface Exact {
  fraction: string;
  value: number;
}

/** A range of the source. */
export interface Range {
  from: number;
  to: number;
}

/**
 * What exploration found for one program: its finite model's exact values; a model of more states
 * than the exact solver takes; or why it has none: a draw that only a sample can give (`draw`, the
 * site's distribution), a run that fails (`fails`), an output that isn't a number, or one of the
 * exploration's limits. `message` is what Lean's CLI prints. `sites` are the ranges of the sites
 * that the failing state may stand for (`Machine.sitesLike`). `error` is the simulator's own
 * failure, which Lean doesn't have.
 */
export type ExactOutcome =
  | {
      kind: "finite";
      states: number;
      returnProbability: Exact;
      rejectionProbability: Exact;
      /** The mean and the variance given that the program returns; null if it never does. */
      mean: Exact | null;
      variance: Exact | null;
    }
  | { kind: "too many"; states: number; maxStates: number; message: string }
  | { kind: "draw"; distribution: string; sites: Range[]; state: number; message: string }
  | { kind: "fails"; detail: string; sites: Range[]; state: number; message: string }
  | { kind: "not a number"; state: number; message: string }
  | { kind: "limit"; limit: Limit; discovered: number; message: string }
  | { kind: "unsolved"; message: string }
  | { kind: "error"; message: string };

/** One program's exploration so far: running, with the states found; the solve of its complete
 * model; or its outcome. */
export type ExactState =
  | { kind: "exploring"; discovered: number }
  | { kind: "solving"; states: number }
  | ExactOutcome;

function exact(q: Rational): Exact {
  return { fraction: fraction(q), value: toNumber(q) };
}

function failed(failure: Failure, state: number, message: string): ExactOutcome {
  if (failure.kind === "nonnumericResult") return { kind: "not a number", state, message };
  const sites = failure.sites.map(({ from, to }) => ({ from, to }));
  if (failure.kind === "unsupported") {
    const distribution = failure.at?.kind ?? failure.detail.replace(/^.*\./, "");
    return { kind: "draw", distribution, sites, state, message };
  }
  return { kind: "fails", detail: failure.detail, sites, state, message };
}

/** The outcome of a finished exploration, with the statistics of a complete one; it yields during
 * the solve, as `eliminate` does. */
export function* outcomeSteps(
  exploration: Exploration | RewardExploration,
  maxStates = defaultSolveStates,
): Generator<void, ExactOutcome> {
  if (exploration.kind === "failed") {
    return failed(exploration.failure, exploration.state, explorationMessage(exploration));
  }
  if (exploration.kind === "incomplete") {
    const { limit, discovered } = exploration;
    return { kind: "limit", limit, discovered, message: explorationMessage(exploration) };
  }
  const { model } = exploration;
  const solved: Solved =
    "edges" in model
      ? yield* rewardStatisticsSteps(model, maxStates)
      : yield* statisticsSteps(model, maxStates);
  if (!solved.ok) {
    return model.size > maxStates
      ? { kind: "too many", states: model.size, maxStates, message: solved.message }
      : { kind: "unsolved", message: solved.message };
  }
  const { result } = solved;
  return {
    kind: "finite",
    states: result.states,
    returnProbability: exact(result.returnMass),
    rejectionProbability: exact(result.rejectionProbability),
    mean: result.conditionalMean && exact(result.conditionalMean),
    variance: result.conditionalVariance && exact(result.conditionalVariance),
  };
}

/** What the explorer needs from the thread it runs on. */
export interface ExactHost {
  post(response: ExactResponse): void;
  /** Runs `task` once the thread has handled what is pending, as `setTimeout(task, 0)` does. */
  defer(task: () => void): void;
  now(): number;
}

interface Job {
  generation: number;
  /** The programs left to explore, the current one first. */
  queue: {
    subject: Subject;
    run: (deadline: () => boolean) => Exploration | RewardExploration | null;
    progress: () => { discovered: number };
    /** The solve of its complete exploration, once that has ended. */
    solving: Generator<void, ExactOutcome> | null;
  }[];
  states: Record<Subject, ExactState>;
}

/** The exploration of `program` in the mode of the request. */
function start(program: Program, source: Program, additive: boolean) {
  return additive
    ? rewardExplorer(program, source, defaultLimits)
    : explorer(program, source, defaultLimits);
}

/** A finished reply with the same state for both programs. */
function reply(generation: number, state: ExactState): ExactResponse {
  return { type: "exact", generation, source: state, determinized: state, done: true };
}

export function createExactServer(host: ExactHost) {
  let job: Job | null = null;
  let scheduled = false;

  function schedule() {
    if (scheduled) return;
    scheduled = true;
    host.defer(slice);
  }

  function post(current: Job) {
    host.post({
      type: "exact",
      generation: current.generation,
      source: current.states.source,
      determinized: current.states.determinized,
      done: current.queue.length === 0,
    });
  }

  function slice() {
    scheduled = false;
    const current = job;
    if (!current) return;
    const deadline = host.now() + sliceMs;
    const late = () => host.now() >= deadline;
    while (current.queue.length > 0) {
      const [next] = current.queue;
      try {
        if (!next.solving) {
          const result = next.run(late);
          if (!result) {
            current.states[next.subject] = {
              kind: "exploring",
              discovered: next.progress().discovered,
            };
            break;
          }
          next.solving = outcomeSteps(result);
        }
        let step = next.solving.next();
        while (!step.done && !late()) step = next.solving.next();
        if (!step.done) {
          current.states[next.subject] = { kind: "solving", states: next.progress().discovered };
          break;
        }
        current.states[next.subject] = step.value;
      } catch (error) {
        current.states[next.subject] = { kind: "error", message: String(error) };
      }
      current.queue.shift();
      if (late()) break;
    }
    post(current);
    if (current.queue.length === 0) job = null;
    else schedule();
  }

  return {
    handle(request: ExactRequest) {
      job = null;
      const exploring: ExactState = { kind: "exploring", discovered: 0 };
      try {
        const analysis = analyze(request.source);
        if (!analysis.ok) {
          // Nothing to explore; the reply says so, as the page waits for one.
          const none: ExactState = { kind: "error", message: "Lean rejects the program" };
          host.post(reply(request.generation, none));
          return;
        }
        const { source, determinized } = analysis.program;
        const subjects = [
          ["source", source],
          ["determinized", determinized],
        ] as const;
        job = {
          generation: request.generation,
          queue: subjects.map(([subject, program]) => ({
            subject,
            ...start(program, source, request.additive),
            solving: null,
          })),
          states: { source: exploring, determinized: exploring },
        };
      } catch (error) {
        host.post(reply(request.generation, { kind: "error", message: String(error) }));
        return;
      }
      schedule();
    },
    /** Drops the exploration in progress, if any. */
    cancel() {
      job = null;
    },
  };
}
