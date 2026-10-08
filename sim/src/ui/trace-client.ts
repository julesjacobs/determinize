// The step table's run for the page: computed in its own worker, so that the sampling worker's
// batches go on while it runs, or in this thread where no worker can start, as in Chrome on
// file://. A computation doesn't yield, so a newer run terminates the worker that computes an
// older one and starts a fresh worker; an idle worker just gets the request, and so do requests
// for further pages of the run it holds.
import type { TracePageRequest, TraceRequest, TraceResponse } from "../core/protocol.ts";
import { isTraceResponse } from "../core/protocol.ts";
import { createTraceServer } from "../core/trace-pages.ts";

/** The worker's URL, which the build stamps with the worker's hash. */
declare const TRACE_WORKER_URL: string;

export interface TraceClient {
  request(request: TraceRequest | TracePageRequest): void;
}

export function createTraceClient(receive: (response: TraceResponse) => void): TraceClient {
  let worker: Worker | null = null;
  let workers = true;
  /** The run that the worker computes, if any. */
  let computing: TraceRequest | null = null;
  /** The latest run's request, to compute it here if the worker fails. */
  let latest: TraceRequest | null = null;
  let server: ReturnType<typeof createTraceServer> | null = null;

  function inThread(request: TraceRequest | TracePageRequest) {
    server ??= createTraceServer();
    const here = server;
    // After a pause, so that the page shows that the table is being computed; a run that a newer
    // one replaced in the meantime is skipped.
    setTimeout(() => {
      if (request.generation !== latest?.generation) return;
      const response = here.handle(request);
      if (response) receive(response);
    }, 0);
  }

  function start(): Worker | null {
    try {
      const started = new Worker(TRACE_WORKER_URL);
      started.addEventListener("message", (event) => {
        const data: unknown = event.data;
        if (!isTraceResponse(data)) return;
        if (computing?.generation === data.generation) computing = null;
        receive(data);
      });
      // A worker that fails while it runs is replaced on the next request; this one's run is
      // computed here.
      started.addEventListener("error", () => {
        started.terminate();
        if (worker === started) worker = null;
        computing = null;
        if (latest) inThread(latest);
      });
      return started;
    } catch {
      workers = false;
      return null;
    }
  }

  return {
    request(request) {
      if (request.type === "trace") {
        latest = request;
        if (computing && worker) {
          worker.terminate();
          worker = null;
        }
        if (workers && !worker) worker = start();
        if (worker) computing = request;
      }
      if (worker) worker.postMessage(request);
      else inThread(request);
    },
  };
}
