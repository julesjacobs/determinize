// The simulator's check of the theorems' premises for the source (Theorems.lean): the type, from
// the port of Lean's inference, exactly; domain safety and a positive return probability from the
// runs so far, as evidence and not proof; integrability not at all. A domain failure at an inexact
// parameter or divisor of exactly 0 refutes nothing: Lean's semantics is real-valued, and a
// floating-point underflow can reach a 0 that it doesn't. Below the determinized program, a line
// per premise, muted while none is found failing and a warning when one is, with the column framed
// as one that the theorems don't cover; both have one height, which the box keeps from load.
import type { ReadonlySignal } from "@preact/signals-core";
import { effect } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import type { DomainFailure, Summary } from "../core/statistics.ts";
import { thin } from "./charts.ts";

/** What the check says of a premise: it holds, it fails, the runs so far show nothing either way,
 * or it isn't checked. */
export type PremiseStatus = "holds" | "fails" | "open" | "unchecked";

export interface Premise {
  status: PremiseStatus;
  text: string;
}

export interface Verdict {
  type: Premise;
  safe: Premise;
  returns: Premise;
  moments: Premise;
  /** Whether Lean rejects the program for its modes, so that a counterexample runs instead. */
  counterexample: boolean;
  /** Whether the check finds a premise failing. */
  failing: boolean;
}

/** `count` runs, in words. */
function runs(count: number) {
  return `${thin(count)} run${count === 1 ? "" : "s"}`;
}

const stageNames = { parse: "parsing", elaboration: "elaboration", inference: "inference" };

/** The safe premise's finding where `failure`, of the runs at `seed`, is the first that failed on
 * a domain error: the witness, with Lean's message and the seed for Lean's CLI. */
export function failingRun(failure: DomainFailure, seed: number) {
  return `run ${thin(failure.run)} failed: ${failure.message} (seed ${runSeed(seed, failure.run)})`;
}

/** The safe premise's finding where `failure` is the first that failed at an inexact parameter or
 * divisor of exactly 0 only. */
export function zeroFailure(failure: DomainFailure, seed: number) {
  return `run ${thin(failure.run)} failed at exactly 0, in floating point: ${failure.message} (seed ${runSeed(seed, failure.run)})`;
}

/** The longest of the runtime's domain messages, and of those that a parameter or divisor of 0
 * gives, for the room of a witness (a test checks that no message is longer). */
export const longestMessages = [
  "discrete requires nonnegative probabilities",
  "discrete probabilities sum to more than one",
];
export const longestZeroMessage = "gamma requires positive shape and rate";

/** The safe premise's finding where no run failed on a domain error. */
export function noDomainFailure(count: number) {
  return `no domain failure in ${runs(count)}`;
}

/** Lean's seed of run `run` of the runs at `seed`: the seed plus the run, wrapping around as
 * Lean's UInt64 does. */
export function runSeed(seed: number, run: number) {
  return BigInt.asUintN(64, BigInt(seed) + BigInt(run));
}

/**
 * The verdict for the source of `analysis`, whose runs from `seed` are `summary`. A program that
 * Lean rejects for another reason than its modes fails the type premise and doesn't run; one that
 * the simulator fails on is left unchecked.
 */
export function premiseVerdict(analysis: Analysis, summary: Summary, seed: number): Verdict {
  const counterexample = !analysis.ok && !!analysis.counterexample;
  // float[G] is a subtype of float[E] (Ty.Sub.general), so a program of that type is typed at
  // float[E] too.
  const type: Premise = analysis.ok
    ? analysis.type === "float[E]"
      ? { status: "holds", text: "holds" }
      : analysis.type === "float[G]"
        ? { status: "holds", text: "holds, as float[G] is a subtype of float[E]" }
        : { status: "fails", text: `the output has type ${analysis.type}` }
    : counterexample
      ? { status: "fails", text: "Lean rejects the written modes" }
      : analysis.stage
        ? { status: "fails", text: `Lean rejects the program at ${stageNames[analysis.stage]}` }
        : { status: "unchecked", text: "not checked, as the simulator fails on the program" };
  const moments: Premise = { status: "unchecked", text: "not checked" };
  if (!analysis.ok && !counterexample) {
    const notRun: Premise = { status: "unchecked", text: "not run" };
    const failing = type.status === "fails";
    return { type, safe: notRun, returns: notRun, moments, counterexample, failing };
  }
  const failure = summary.firstDomainFailure;
  const zero = summary.firstZeroFailure;
  const safe: Premise = failure
    ? { status: "fails", text: failingRun(failure, seed) }
    : zero
      ? { status: "open", text: zeroFailure(zero, seed) }
      : { status: "open", text: noDomainFailure(summary.runs) };
  // A run that a limit of the runtime or floating point stopped might still have returned; one
  // that observe rejected or that failed on a domain error doesn't return.
  const returned = summary.runs - summary.rejected - summary.failed;
  const returns: Premise =
    returned > 0
      ? { status: "holds", text: `${thin(returned)} of ${runs(summary.runs)} returned` }
      : summary.stopped > 0
        ? {
            status: "open",
            text: `no run returned in ${runs(summary.runs)}; ${thin(summary.stopped)} stopped at the runtime's limits or in floating point, which says nothing either way`,
          }
        : { status: "fails", text: `no run returned in ${runs(summary.runs)}, so it likely fails` };
  const failing = [type, safe, returns].some((premise) => premise.status === "fails");
  return { type, safe, returns, moments, counterexample, failing };
}

export interface VerdictElements {
  /** The determinized program's pane, framed while the theorems don't cover it. */
  pane: HTMLElement;
  verdict: HTMLElement;
  lead: HTMLElement;
  /** Each premise's item, whose last child holds what the check says. */
  type: HTMLElement;
  safe: HTMLElement;
  returns: HTMLElement;
  moments: HTMLElement;
}

/** `text` with each count of runs at least `width` pixels wide, the width of the largest count, so
 * that a line that counts the runs as they arrive keeps where it wraps. */
function steadyCounts(text: string, width: number) {
  const parts = text.split(/(\d[\d\u202f]*)/);
  return parts.map((part, index) => {
    if (index % 2 === 0) return part;
    const count = document.createElement("span");
    count.className = "count";
    count.style.minWidth = `${width}px`;
    count.textContent = part;
    return count;
  });
}

/** The width of `text` as a count in `parent`. */
function countWidth(parent: HTMLElement, text: string) {
  const probe = document.createElement("span");
  probe.className = "count";
  probe.textContent = text;
  parent.append(probe);
  const width = probe.getBoundingClientRect().width;
  probe.remove();
  return Math.ceil(width);
}

export function mountVerdict(
  elements: VerdictElements,
  store: {
    analysis: ReadonlySignal<Analysis>;
    checkedSource: ReadonlySignal<string>;
    samples: ReadonlySignal<{ seed: number; original: { summary: Summary } }>;
    /** The number of runs that sampling brings the runs to. */
    sampleCount: ReadonlySignal<number>;
    running: ReadonlySignal<unknown>;
    samplesReady: ReadonlySignal<boolean>;
  },
) {
  // The page's line breaks after each finding and between the premises would show as spaces.
  const blank = (node: Node | null) =>
    node?.nodeType === Node.TEXT_NODE && !node.textContent?.trim();
  for (const key of ["type", "safe", "returns", "moments"] as const) {
    const item = elements[key];
    while (blank(item.lastChild)) item.lastChild?.remove();
    while (blank(item.nextSibling)) item.nextSibling?.remove();
  }
  effect(() => {
    const { seed, original } = store.samples.value;
    const analysis = store.analysis.value;
    const empty = store.checkedSource.value.trim() === "";
    if (empty) {
      elements.verdict.hidden = true;
      elements.pane.classList.remove("uncovered");
      return;
    }
    // Until the first runs of a new program have come in, the earlier verdict stays: run 0 alone
    // would show what isn't so. Before any verdict, it says that the runs are sampling.
    const runnable = analysis.ok || !!analysis.counterexample;
    let verdict = premiseVerdict(analysis, original.summary, seed);
    if (runnable && !store.samplesReady.value) {
      if (!elements.verdict.hidden) return;
      const sampling: Premise = { status: "open", text: "sampling…" };
      const failing = verdict.type.status === "fails";
      verdict = { ...verdict, safe: sampling, returns: sampling, failing };
    }
    elements.verdict.hidden = false;
    elements.pane.classList.toggle("uncovered", verdict.failing);
    elements.verdict.classList.toggle("alert", verdict.failing);
    elements.verdict.classList.toggle("quiet", !verdict.failing);
    if (verdict.counterexample) {
      const strong = document.createElement("strong");
      strong.textContent = "This is the counterexample";
      elements.lead.replaceChildren(
        strong,
        ": what replacing the [E] draws anyway does, which the theorems don't cover. The simulator's check:",
      );
    } else if (verdict.failing) {
      // Bold, the lead is short enough to keep the quiet one's line.
      const strong = document.createElement("strong");
      strong.textContent = "The simulator's check:";
      elements.lead.replaceChildren(strong);
    } else {
      elements.lead.textContent = "The simulator's check:";
    }
    // While runs arrive, the counts of runs keep the width of the largest, so that the lines keep
    // where they wrap. A witness's run and seed aren't counts.
    const sampling = store.running.value !== null;
    const widest = Math.max(store.sampleCount.value, original.summary.runs);
    const width = sampling ? countWidth(elements.lead, thin(widest)) : 0;
    const counted = (text: string) => (width > 0 ? steadyCounts(text, width) : [text]);
    const { firstDomainFailure, firstZeroFailure } = original.summary;
    for (const key of ["type", "safe", "returns", "moments"] as const) {
      const item = elements[key];
      const { status, text } = verdict[key];
      item.dataset.status = status;
      const finding = item.querySelector(".finding") as HTMLElement;
      const witness = key === "safe" && (firstDomainFailure || firstZeroFailure);
      finding.replaceChildren(...(witness ? [text] : counted(text)));
    }
    // The box keeps the height of an invisible copy of the list in which domain safety holds the
    // room of each of its findings, so that nothing moves when a run fails, and the spare room is
    // at the bottom.
    const list = elements.safe.parentElement as HTMLElement;
    list.parentElement?.querySelector(":scope > .premise-room")?.remove();
    if (verdict.safe.status === "unchecked") return;
    const copy = list.cloneNode(true) as HTMLElement;
    copy.classList.add("premise-room");
    copy.setAttribute("aria-hidden", "true");
    const safe = copy.querySelector("#premise-safe") as HTMLElement;
    for (const element of copy.querySelectorAll("[id]")) element.removeAttribute("id");
    for (const link of copy.querySelectorAll("a")) {
      const label = document.createElement("span");
      label.className = "label";
      label.append(...link.childNodes);
      link.replaceWith(label);
    }
    const label = safe.querySelector(".label") as HTMLElement;
    const line = (className: string, content: (string | HTMLElement)[]) => {
      const span = document.createElement("span");
      span.className = className;
      span.append(label.cloneNode(true), ": ", ...content);
      return span;
    };
    const run = (message: string) => ({ run: widest, message });
    safe.classList.add("stack");
    safe.replaceChildren(
      line("open", counted(noDomainFailure(widest))),
      ...longestMessages.map((message) => line("fails", [failingRun(run(message), seed)])),
      line("zero", [zeroFailure(run(longestZeroMessage), seed)]),
    );
    list.after(copy);
  });
}

/** The means of two programs, compared where the theorems don't cover them: they differ only
 * where their 95 % intervals are disjoint; nothing where a program has no returned number. */
export function compareMeans(
  a: { n: number; mean: number; standardError: number },
  b: { n: number; mean: number; standardError: number },
  format: (value: number) => string,
) {
  if (a.n === 0 || b.n === 0) return null;
  const apart = Math.abs(a.mean - b.mean) > 1.96 * (a.standardError + b.standardError);
  const means = `${format(a.mean)} and ${format(b.mean)}`;
  return apart ? `Means differ: ${means}.` : `Means: ${means}.`;
}
