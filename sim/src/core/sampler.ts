// Runs the batches of "Run N" with the port of Lean's evaluator: run i of a program and of its
// determinization at seed s + i, as Lean's CLI runs `--seed s --samples N`. A batch runs in
// slices of about 50 ms and reports the outcomes of each slice, so that the thread it runs on
// handles messages and input between slices. The worker runs it, and so does the page where no
// worker can start.
import type { Analysis } from "./compiler/analyze.ts";
import { analyze } from "./compiler/analyze.ts";
import type { Request, Response } from "./protocol.ts";
import type { GDraw, Node, Outcome } from "./runtime/eval.ts";
import { display, prepare, run, siteNodes } from "./runtime/eval.ts";
import { discreteOps, uint64 } from "./runtime/sampling.ts";
import type { RunOutcome, SiteDraws } from "./statistics.ts";
import { runsOf } from "./statistics.ts";

/** How long a slice runs: about how often outcomes arrive, and how late a cancel takes effect. */
export const sliceMs = 50;

/** The programs that a run evaluates, prepared for the evaluator, and the indices of their
 * continuous G sites, which determinization keeps. */
export interface Runner {
  source: Node;
  determinized: Node;
  gSites: number[];
}

/** The programs of an analysis: the checked program and its determinization, or a program's
 * counterexample when Lean rejects it only for a mode conflict; none for other rejections. */
export function runnerOf(analysis: Analysis): Runner | null {
  const programs = analysis.ok ? analysis.program : analysis.counterexample?.program;
  if (!programs) return null;
  const source = prepare(programs.source);
  const gSites = siteNodes(source)
    .filter((site) => site.action === "G" && !discreteOps.has(site.op))
    .map((site) => site.index);
  return { source, determinized: prepare(programs.determinized), gSites };
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

/** Run `index` from `seed` of both programs, with their G traces: the G draws in order. */
export function runIndex(runner: Runner, seed: number, index: number) {
  const at = runSeed(seed, index);
  const traces = { source: [] as GDraw[], determinized: [] as GDraw[] };
  return {
    source: reported(run(runner.source, at, { onGDraw: (draw) => traces.source.push(draw) })),
    determinized: reported(
      run(runner.determinized, at, { onGDraw: (draw) => traces.determinized.push(draw) }),
    ),
    traces,
  };
}

/** The draws of runs with G traces `traces` at each of `sites`. */
export function siteDraws(sites: number[], traces: GDraw[][]): SiteDraws[] {
  const draws = sites.map((site) => ({ site, values: new Float64Array(traces.length) }));
  const bySite = new Map(draws.map((entry) => [entry.site, entry]));
  for (const [run, trace] of traces.entries()) {
    const counts = new Map<number, number>();
    for (const { site, value } of trace) {
      const entry = bySite.get(site);
      if (!entry) continue;
      counts.set(site, (counts.get(site) ?? 0) + 1);
      entry.values[run] = value;
    }
    for (const entry of draws) if (counts.get(entry.site) !== 1) entry.values[run] = NaN;
  }
  return draws;
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
    const traces = { source: [] as GDraw[][], determinized: [] as GDraw[][] };
    while (runner && current.next < current.end && (source.length === 0 || host.now() < deadline)) {
      const outcomes = runIndex(runner, current.seed, current.next);
      current.next += 1;
      source.push(outcomes.source);
      determinized.push(outcomes.determinized);
      traces.source.push(outcomes.traces.source);
      traces.determinized.push(outcomes.traces.determinized);
    }
    if (runner && source.length > 0) {
      const batch = {
        type: "batch" as const,
        generation: current.generation,
        source: runsOf(source, siteDraws(runner.gSites, traces.source)),
        determinized: runsOf(determinized, siteDraws(runner.gSites, traces.determinized)),
      };
      const buffers = [batch.source, batch.determinized].flatMap((runs) => [
        runs.values.buffer,
        ...runs.draws.map((draws) => draws.values.buffer),
      ]);
      host.post(batch, buffers);
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
