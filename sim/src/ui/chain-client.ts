// The layout of chains for the page, from the chain worker, which holds Graphviz and is loaded only
// when a reader first opens a chain. Where no worker can start, as in Chrome on file://, the same
// bundle is loaded as a classic script, and lays out on this thread.
import type { ChainLayout } from "../core/chain-layout.ts";
import type { ChainLayoutRequest, ChainLayoutResponse } from "../core/protocol.ts";
import { isChainLayoutResponse } from "../core/protocol.ts";

/** The worker's URL, which the build stamps with the worker's hash. */
declare const CHAIN_WORKER_URL: string;

/** What the chain bundle gives the page when it runs as a classic script. */
type LayoutChain = (request: ChainLayoutRequest) => Promise<ChainLayoutResponse>;

/** Lays out a chain from its dot source and its counts. */
export type LayoutClient = (
  request: Omit<ChainLayoutRequest, "type" | "id">,
) => Promise<ChainLayout>;

export function createLayoutClient(): LayoutClient {
  let worker: Worker | null = null;
  let inThread: Promise<LayoutChain> | null = null;
  let next = 0;
  const pending = new Map<
    number,
    {
      request: ChainLayoutRequest;
      resolve: (layout: ChainLayout) => void;
      reject: (error: Error) => void;
    }
  >();

  function settle(response: ChainLayoutResponse) {
    const waiting = pending.get(response.id);
    if (!waiting) return;
    pending.delete(response.id);
    if (response.type === "chain-layout") waiting.resolve(response.layout);
    else waiting.reject(new Error(response.message));
  }

  /** The bundle as a script on this page, loaded once. */
  function script(): Promise<LayoutChain> {
    inThread ??= new Promise((resolve, reject) => {
      const element = document.createElement("script");
      element.src = CHAIN_WORKER_URL;
      element.addEventListener("load", () => {
        const layout = (globalThis as { determinizeLayoutChain?: LayoutChain })
          .determinizeLayoutChain;
        if (layout) resolve(layout);
        else reject(new Error("the chain bundle didn't load"));
      });
      element.addEventListener("error", () => {
        // A later layout tries to load it again.
        element.remove();
        inThread = null;
        reject(new Error("the chain bundle didn't load"));
      });
      document.head.append(element);
    });
    return inThread;
  }

  function here(request: ChainLayoutRequest) {
    script().then(
      (layout) => layout(request).then(settle),
      (error: Error) =>
        settle({ type: "chain-layout-failed", id: request.id, message: error.message }),
    );
  }

  function start(): Worker | null {
    try {
      const started = new Worker(CHAIN_WORKER_URL);
      started.addEventListener("message", (event) => {
        const data: unknown = event.data;
        if (isChainLayoutResponse(data)) settle(data);
      });
      // A worker that fails is given up, and what it had yet to answer is laid out here.
      started.addEventListener("error", () => {
        started.terminate();
        worker = null;
        for (const { request } of pending.values()) here(request);
      });
      return started;
    } catch {
      return null;
    }
  }

  let workers = true;
  return ({ dots, nodes, edges }) => {
    const request: ChainLayoutRequest = { type: "chain-layout", id: next++, dots, nodes, edges };
    const layout = new Promise<ChainLayout>((resolve, reject) => {
      pending.set(request.id, { request, resolve, reject });
    });
    if (workers && !worker && !inThread) {
      worker = start();
      workers = worker !== null;
    }
    if (worker) worker.postMessage(request);
    else here(request);
    return layout;
  };
}
