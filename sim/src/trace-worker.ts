// The step table's worker: computes the step table's run of each program that the page requests,
// keeps the latest run and sends it a page at a time. A computation can't be interrupted, so the
// page terminates this worker when a newer request arrives during one.
import { isTraceRequest } from "./core/protocol.ts";
import { createTraceServer } from "./core/trace-pages.ts";

const server = createTraceServer();

self.addEventListener("message", (event) => {
  const data: unknown = event.data;
  if (!isTraceRequest(data)) return;
  const response = server.handle(data);
  if (response) self.postMessage(response);
});
