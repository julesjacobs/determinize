// Lays out contracted chains with Graphviz's dot (`@viz-js/viz`, WebAssembly). The page loads it
// when a reader first opens a chain: as a worker, which answers the page's layout requests, or,
// where no worker can start, as a classic script, which gives the page `layoutChain`.
import { instance } from "@viz-js/viz";
import { layoutOf, narrowest } from "./core/chain-layout.ts";
import type { ChainLayoutRequest, ChainLayoutResponse } from "./core/protocol.ts";
import { isChainLayoutRequest } from "./core/protocol.ts";

const viz = instance();

/** The layout that `request` asks for, or why dot couldn't give it. */
async function layoutChain(request: ChainLayoutRequest): Promise<ChainLayoutResponse> {
  const { id, dots, nodes, edges } = request;
  try {
    const graphviz = await viz;
    const layouts = dots.map((dot) =>
      layoutOf(graphviz.renderJSON(dot, { yInvert: true }), nodes, edges),
    );
    return { type: "chain-layout", id, layout: narrowest(layouts) };
  } catch (error) {
    return { type: "chain-layout-failed", id, message: String(error) };
  }
}

if ("document" in globalThis) {
  Object.assign(globalThis, { determinizeLayoutChain: layoutChain });
} else {
  self.addEventListener("message", async (event) => {
    const data: unknown = event.data;
    if (isChainLayoutRequest(data)) self.postMessage(await layoutChain(data));
  });
}
