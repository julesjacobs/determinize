// The page's link to its state: the fragment follows the program, the seed, the example, the
// chart and the mode of the exact values, and a fragment that the page navigates to is restored.
import { effect } from "@preact/signals-core";
import { examples } from "../core/examples.ts";
import type { Decoded, SharedState } from "../core/share.ts";
import { decodeShare, encodeShare } from "../core/share.ts";
import type { Store } from "./store.ts";

/** How long the fragment waits for the state to settle. */
const writeDelayMs = 300;

/** The parts of the store that a link carries. */
export type SharedStore = Pick<Store, "source" | "seed" | "exampleId" | "view" | "additive">;

/** The state that a link to the page carries, as the fragment and Copy link give it. */
export function sharedStateOf(store: SharedStore): SharedState {
  return {
    source: store.source.value,
    seed: store.seed.value,
    example: store.exampleId.value,
    ...(store.view.value === "against" ? { view: "against" as const } : {}),
    ...(store.additive.value ? { additive: true as const } : {}),
  };
}

export function bindUrl(store: SharedStore, restore: (decoded: Decoded) => void) {
  let written = window.location.hash;
  let timer: ReturnType<typeof setTimeout> | undefined;
  let opened = true;
  effect(() => {
    const state = sharedStateOf(store);
    // The page opens in the state that its link names, or without a fragment.
    if (opened) {
      opened = false;
      return;
    }
    clearTimeout(timer);
    timer = setTimeout(async () => {
      const hash = await encodeShare(state);
      if (hash === written) return;
      written = hash;
      history.replaceState(history.state, "", hash);
    }, writeDelayMs);
  });
  // The fragment navigated to, which the page's own write may have replaced before the event.
  window.addEventListener("hashchange", async (event) => {
    const hash = new URL(event.newURL).hash;
    if (hash === written) return;
    written = hash;
    restore(await decodeShare(hash));
  });
}

/** The state's program, or its example's text where they differ only in trailing whitespace, as
 * in links whose program has a final line break. */
export function sharedSource(state: Pick<SharedState, "source" | "example">) {
  const example = examples.find((entry) => entry.id === state.example);
  return example && example.source === state.source.trimEnd() ? example.source : state.source;
}
