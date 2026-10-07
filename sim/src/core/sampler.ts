// Runs the batches of "Run N" with the port of Lean's evaluator: run i of a program and of its
// determinization at seed s + i, as Lean's CLI runs `--seed s --samples N`. A batch runs in
// slices of about 50 ms and reports the outcomes of each slice, so that the thread it runs on
// handles messages and input between slices. The worker runs it, and so does the page where no
// worker can start.
import type { Analysis } from "./compiler/analyze.ts";
import { analyze } from "./compiler/analyze.ts";
import type { Request, Response } from "./protocol.ts";
import type { Node, Outcome } from "./runtime/eval.ts";
import { display, prepare, run } from "./runtime/eval.ts";
import { uint64 } from "./runtime/sampling.ts";
import type { RunOutcome } from "./statistics.ts";
import { runsOf } from "./statistics.ts";

/** How long a slice runs: about how often outcomes arrive, and how late a cancel takes effect. */
export const sliceMs = 50;

/** The programs that a run evaluates, prepared for the evaluator. */
export interface Runner {
  source: Node;
  determinized: Node;
}

/** The programs of an analysis: the checked program and its determinization, or a program's
 * counterexample when Lean rejects it only for a mode conflict; none for other rejections. */
export function runnerOf(analysis: Analysis): Runner | null {
  const programs = analysis.ok ? analysis.program : analysis.counterexample?.program;
  if (!programs) return null;
  return { source: prepare(programs.source), determinized: prepare(programs.determinized) };
}

/** The seed of run `index` from `seed`: their sum as a UInt64, as Lean's CLI seeds its runs. */
export function runSeed(seed: number, index: number): bigint {
  return uint64(BigInt(seed) + BigInt(index));
}

/** What the CLI takes from an outcome. */
function reported(outcome: Outcome): RunOutcome {
  if (outcome.kind !== "returned") return outcome;
  const { value } = outcome;
  return {
    kind: "returned",
    number: value.tag === "number" ? value.value : null,
    display: display(value),
  };
}

/** Run `index` from `seed` of both programs. */
export function runIndex(runner: Runner, seed: number, index: number) {
  const at = runSeed(seed, index);
  return {
    source: reported(run(runner.source, at)),
    determinized: reported(run(runner.determinized, at)),
  };
}

/** What the sampler needs from the thread it runs on. */
export interface SamplerHost {
  post(response: Response, transfer: ArrayBuffer[]): void;
  /** Runs `task` once the thread has handled what is pending, as `setTimeout(task, 0)` does. */
  defer(task: () => void): void;
  now(): number;
}

interface Job {
  generation: number;
  runner: Runner | null;
  seed: number;
  next: number;
  end: number;
}

export function createSampler(host: SamplerHost) {
  let job: Job | null = null;
  let scheduled = false;

  function schedule() {
    if (scheduled) return;
    scheduled = true;
    host.defer(slice);
  }

  function slice() {
    scheduled = false;
    const current = job;
    if (!current) return;
    const { runner } = current;
    const deadline = host.now() + sliceMs;
    const source: RunOutcome[] = [];
    const determinized: RunOutcome[] = [];
    while (runner && current.next < current.end && (source.length === 0 || host.now() < deadline)) {
      const outcomes = runIndex(runner, current.seed, current.next);
      current.next += 1;
      source.push(outcomes.source);
      determinized.push(outcomes.determinized);
    }
    if (source.length > 0) {
      const batch = {
        type: "batch" as const,
        generation: current.generation,
        source: runsOf(source),
        determinized: runsOf(determinized),
      };
      host.post(batch, [batch.source.values.buffer, batch.determinized.values.buffer]);
    }
    if (!runner || current.next === current.end) {
      job = null;
      host.post({ type: "done", generation: current.generation }, []);
      return;
    }
    schedule();
  }

  return {
    handle(request: Request) {
      if (request.type === "run") {
        const { generation, source, seed, from, count } = request;
        const runner = runnerOf(analyze(source));
        job = { generation, runner, seed, next: from, end: from + count };
        schedule();
      } else if (job?.generation === request.generation) {
        job = null;
      }
    },
  };
}
