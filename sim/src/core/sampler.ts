// Runs a batch of seeds in slices of about 50 ms and reports the samples of each slice, so that
// the thread it runs on handles messages and input between slices. The worker runs it, and so
// does the page where no worker can start.
import type { Request, Response } from "./protocol.ts";
import type { CoupledTrace } from "./runtime/semantics.ts";
import { runCoupling, sampleOf } from "./trace.ts";

/** How long a slice runs: about how often samples arrive, and how late a cancel takes effect. */
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
    const original: number[] = [];
    const determinized: number[] = [];
    let runs = 0;
    let failed = false;
    while (current.next < current.seeds.length && (runs === 0 || host.now() < deadline)) {
      let trace: CoupledTrace;
      try {
        trace = runCoupling(current.source, current.seeds[current.next]);
      } catch {
        failed = true;
        break;
      }
      current.next += 1;
      current.last = trace;
      runs += 1;
      const sample = sampleOf(trace);
      if (sample) {
        original.push(sample.original);
        determinized.push(sample.determinized);
      }
    }
    const batch = {
      type: "batch" as const,
      generation: current.generation,
      runs,
      original: Float64Array.from(original),
      determinized: Float64Array.from(determinized),
    };
    host.post(batch, [batch.original.buffer, batch.determinized.buffer]);
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
