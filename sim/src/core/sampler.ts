// Runs a batch of seeds in slices of about 50 ms and reports the outcomes of each slice, so that
// the thread it runs on handles messages and input between slices. The worker runs it, and so
// does the page where no worker can start.
import type { Request, Response } from "./protocol.ts";
import type { CoupledTrace } from "./runtime/semantics.ts";
import type { RunOutcome } from "./trace.ts";
import { outcomesOf, runCoupling, runsOf } from "./trace.ts";

/** How long a slice runs: about how often outcomes arrive, and how late a cancel takes effect. */
export const sliceMs = 50;

/** What the sampler needs from the thread it runs on. */
export interface SamplerHost {
  post(response: Response, transfer: ArrayBuffer[]): void;
  /** Runs `task` once the thread has handled what is pending, as `setTimeout(task, 0)` does. */
  defer(task: () => void): void;
  now(): number;
}

interface Job {
  generation: number;
  source: string;
  seeds: Float64Array;
  next: number;
  last: CoupledTrace | null;
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
    const deadline = host.now() + sliceMs;
    const source: RunOutcome[] = [];
    const determinized: RunOutcome[] = [];
    let failed = false;
    while (current.next < current.seeds.length && (source.length === 0 || host.now() < deadline)) {
      let trace: CoupledTrace;
      try {
        trace = runCoupling(current.source, current.seeds[current.next]);
      } catch {
        failed = true;
        break;
      }
      current.next += 1;
      current.last = trace;
      const outcomes = outcomesOf(trace);
      source.push(outcomes.source);
      determinized.push(outcomes.determinized);
    }
    const batch = {
      type: "batch" as const,
      generation: current.generation,
      source: runsOf(source),
      determinized: runsOf(determinized),
    };
    host.post(batch, [batch.source.values.buffer, batch.determinized.values.buffer]);
    if (failed || current.next === current.seeds.length) {
      job = null;
      host.post({ type: "done", generation: current.generation, last: current.last }, []);
      return;
    }
    schedule();
  }

  return {
    handle(request: Request) {
      if (request.type === "run") {
        const { generation, source, seeds } = request;
        job = { generation, source, seeds, next: 0, last: null };
        schedule();
      } else if (job?.generation === request.generation) {
        job = null;
      }
    },
  };
}
