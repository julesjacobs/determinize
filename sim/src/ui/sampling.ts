// Sampling for the page: in the worker, or in this thread where no worker can start, as in
// Chrome on file://. Both run the same sampler, in slices of about 50 ms.
import type { Request, Response } from "../core/protocol.ts";
import { isResponse } from "../core/protocol.ts";
import { createSampler } from "../core/sampler.ts";

/** The worker's URL, which the build stamps with the worker's hash. */
declare const WORKER_URL: string;

export interface Sampling {
  send(request: Request): void;
}

export function createSampling(receive: (response: Response) => void): Sampling {
  // The latest run and how many of its seeds have run, to resume it if the worker fails.
  let active: { request: Request & { type: "run" }; runs: number } | null = null;
  let send: (request: Request) => void;

  function track(response: Response) {
    if (active?.request.generation === response.generation) {
      if (response.type === "batch") active.runs += response.source.values.length;
      else active = null;
    }
    receive(response);
  }

  function inThread() {
    const sampler = createSampler({
      post: (response) => track(response),
      defer: (task) => setTimeout(task, 0),
      now: () => performance.now(),
    });
    return (request: Request) => sampler.handle(request);
  }

  try {
    const worker = new Worker(WORKER_URL);
    worker.addEventListener("message", (event) => {
      const data: unknown = event.data;
      if (isResponse(data)) track(data);
    });
    worker.addEventListener("error", () => {
      worker.terminate();
      send = inThread();
      if (active) {
        const { request, runs } = active;
        send({ ...request, seeds: request.seeds.subarray(runs) });
      }
    });
    send = (request) => worker.postMessage(request);
  } catch {
    send = inThread();
  }

  return {
    send(request) {
      if (request.type === "run") active = { request, runs: 0 };
      else if (active?.request.generation === request.generation) active = null;
      send(request);
    },
  };
}
