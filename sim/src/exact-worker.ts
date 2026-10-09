// The exact worker: explores the finite models of the programs that the page requests, in slices,
// and posts both programs' explorations so far after each.
import { createExactServer } from "./core/exact.ts";
import { isExactRequest } from "./core/protocol.ts";

const server = createExactServer({
  post: (response) => self.postMessage(response),
  defer: (task) => setTimeout(task, 0),
  now: () => performance.now(),
});

self.addEventListener("message", (event) => {
  const data: unknown = event.data;
  if (isExactRequest(data)) server.handle(data);
});
