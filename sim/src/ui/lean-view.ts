// What Lean prints for the checked program: its type, the sample sites before and after
// determinization, and the annotated and the determinized program in Lean's forms with the
// source's names, and which premises of the theorems typing leaves open. For a program that Lean
// rejects only for a mode conflict, the programs of its counterexample; for other rejections,
// nothing.
import type { ReadonlySignal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import { sampleSites } from "../core/compiler/core.ts";
import { sourcePretty } from "../core/compiler/print.ts";
import { counterexampleLabel } from "./html.ts";

export interface LeanViewElements {
  section: HTMLElement;
  counterexample: HTMLElement;
  summary: HTMLElement;
  type: HTMLElement;
  sites: HTMLElement;
  /** The notice that the type is not float[E], so that no theorem applies. */
  typePremise: HTMLElement;
  /** The notice that typing establishes neither domain safety nor a defined expectation. */
  safetyPremise: HTMLElement;
  annotated: HTMLElement;
  determinized: HTMLElement;
}

function describeSites(sites: { discrete: number; continuous: number }) {
  return `${sites.continuous} continuous and ${sites.discrete} discrete`;
}

export function mountLeanView(elements: LeanViewElements, analysis: ReadonlySignal<Analysis>) {
  effect(() => {
    const result = analysis.value;
    const program = result.ok ? result.program : result.counterexample?.program;
    elements.section.hidden = !program;
    if (!program) return;
    elements.counterexample.hidden = result.ok;
    elements.counterexample.textContent = result.ok ? "" : counterexampleLabel;
    elements.summary.hidden = !result.ok;
    elements.safetyPremise.hidden = !result.ok;
    elements.typePremise.hidden = !result.ok || result.type === "float[E]";
    if (result.ok) {
      elements.type.textContent = result.type;
      const before = sampleSites(program.source);
      const after = sampleSites(program.determinized);
      elements.sites.textContent = `${describeSites(before)} draws; after determinization, ${describeSites(after)}`;
    }
    elements.annotated.textContent = sourcePretty(program.source);
    elements.determinized.textContent = sourcePretty(program.determinized);
  });
}
