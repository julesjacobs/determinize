// "Predict first": on a first visit, before the first example's first runs, the distributions
// band asks what the runs will show; afterwards one line repeats the guess beside the result.
import type { ReadonlySignal } from "@preact/signals-core";
import { computed, effect, signal } from "@preact/signals-core";
import { examples } from "../core/examples.ts";
import { thin } from "./charts.ts";
import { readPref, writePref } from "./prefs.ts";
import type { Store } from "./store.ts";

export interface PredictElements {
  form: HTMLFormElement;
  runs: HTMLElement;
  /** The guess, repeated beside the result. */
  line: HTMLElement;
}

const meanWords: Record<string, string> = { same: "the same mean", different: "a different mean" };
const varianceWords: Record<string, string> = {
  smaller: "a smaller variance",
  same: "the same variance",
  larger: "a larger variance",
};

/** Whether the band asks for a guess now. */
export function mountPredict(
  elements: PredictElements,
  store: Pick<Store, "exampleId" | "source" | "samples" | "sampleCount" | "runBoth">,
): ReadonlySignal<boolean> {
  const first = examples[0];
  const predicted = signal(readPref("predicted") === "yes");
  const guess = signal<string | null>(null);
  const asking = computed(
    () =>
      !predicted.value &&
      store.exampleId.value === first.id &&
      store.source.value === first.source &&
      store.samples.value.original.summary.runs < 2,
  );
  effect(() => {
    elements.form.hidden = !asking.value;
    elements.runs.textContent = thin(store.sampleCount.value);
  });
  // Runs started any other way end the question too.
  effect(() => {
    if (store.samples.value.original.summary.runs > 1 && !predicted.peek()) {
      predicted.value = true;
      writePref("predicted", "yes");
    }
  });
  effect(() => {
    const text = guess.value;
    elements.line.hidden = !text || store.samples.value.original.summary.runs < 2;
    elements.line.textContent = text ? `You predicted ${text}.` : "";
  });
  elements.form.addEventListener("submit", (event) => {
    event.preventDefault();
    const data = new FormData(elements.form);
    const words = [
      meanWords[String(data.get("predict-mean"))],
      varianceWords[String(data.get("predict-variance"))],
    ].filter(Boolean);
    guess.value = words.length > 0 ? words.join(" and ") : null;
    store.runBoth();
  });
  return asking;
}
