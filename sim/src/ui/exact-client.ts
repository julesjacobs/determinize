// The exact values for the page: both programs' finite models, explored in their own worker, so
// that sampling and the step table go on meanwhile, or in this thread in slices of about 50 ms
// where no worker can start, as in Chrome on file://. A newer request replaces a worker that is
// still exploring an older one, since a single step of a large exploration can take long.
import { createExactServer } from "../core/exact.ts";
import type { ExactRequest, ExactResponse } from "../core/protocol.ts";
import { isExactResponse } from "../core/protocol.ts";

/** The worker's URL, which the build stamps with the worker's hash. */
declare const EXACT_WORKER_URL: string;

export function createExactClient(
  receive: (response: ExactResponse) => void,
): (request: ExactRequest) => void {
  // The latest request, to explore it here if the worker fails, and whether the worker is still
  // on it: one step of a large exploration can take long, so a newer request replaces a busy worker.
  let latest: ExactRequest | null = null;
  let busy = false;
  let worker: Worker | null = null;
  let workers = true;
  let server: ReturnType<typeof createExactServer> | null = null;

  function inThread(request: ExactRequest) {
    server ??= createExactServer({
      post: receive,
      defer: (task) => setTimeout(task, 0),
      now: () => performance.now(),
    });
    server.handle(request);
  }

  function start(): Worker | null {
    try {
      const started = new Worker(EXACT_WORKER_URL);
      started.addEventListener("message", (event) => {
        const data: unknown = event.data;
        if (!isExactResponse(data)) return;
        if (data.done && data.generation === latest?.generation) busy = false;
        receive(data);
      });
      // A worker that fails while it runs is replaced on the next request; this one's request is
      // explored here.
      started.addEventListener("error", () => {
        started.terminate();
        if (worker === started) worker = null;
        busy = false;
        if (latest) inThread(latest);
      });
      return started;
    } catch {
      workers = false;
      return null;
    }
  }

  return (request) => {
    latest = request;
    if (busy && worker) {
      worker.terminate();
      worker = null;
    }
    if (workers && !worker) worker = start();
    if (worker) {
      // An exploration on this thread, after a worker failed, gives way to the fresh worker's.
      server?.cancel();
      busy = true;
      worker.postMessage(request);
    } else inThread(request);
  };
}
