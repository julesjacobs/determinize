// The sampling worker: runs the batches that the page requests and posts their samples.
import { isRequest } from "./core/protocol.ts";
import { createSampler } from "./core/sampler.ts";

const sampler = createSampler({
  post: (response, transfer) => self.postMessage(response, { transfer }),
  defer: (task) => setTimeout(task, 0),
  now: () => performance.now(),
});

self.addEventListener("message", (event) => {
  const data: unknown = event.data;
  if (isRequest(data)) sampler.handle(data);
});
