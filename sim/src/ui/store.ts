// The simulator's state: its inputs as signals, what follows from them as computed signals, and
// the actions that change them.
import type { ReadonlySignal, Signal } from "@preact/signals-core";
import { action, computed, effect, signal, untracked } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import { analyze } from "../core/compiler/analyze.ts";
import type { CoupledTrace } from "../core/runtime/semantics.ts";
import type { Stats } from "../core/statistics.ts";
import { sampleStats } from "../core/statistics.ts";
import { runBatch, runCoupling, sampleOf } from "../core/trace.ts";

/** A range of the source. */
export interface Span {
  from: number;
  to: number;
}

/** The results of runs of one program. */
export interface Samples {
  source: string;
  original: number[];
  determinized: number[];
  /** The run that added the last sample, as `source:seed`. */
  lastKey: string;
}

/** The step table's run of the checked program. */
export type TraceState =
  | { kind: "not run" }
  | { kind: "run"; trace: CoupledTrace }
  | { kind: "unavailable"; message: string };

export interface Store {
  /** The editor's text, at every change. */
  source: Signal<string>;
  /** The text that is analyzed and run: the editor's text after a pause in typing. */
  checkedSource: ReadonlySignal<string>;
  /** Counts the times the editor's text was analyzed and run, also when it hadn't changed. */
  commits: ReadonlySignal<number>;
  /** The seed of the step table's run. */
  seed: ReadonlySignal<number>;
  /** The example chosen last. */
  exampleId: Signal<string>;
  /** The number of runs that "Run N" adds. */
  sampleCount: Signal<number>;
  typeHints: Signal<boolean>;
  /** The source range whose type hint the pointer or the focus is on. */
  hoveredSpan: Signal<Span | null>;
  /** The step of the step table whose checks are shown. */
  activeStep: Signal<number | null>;
  samples: ReadonlySignal<Samples>;
  analysis: ReadonlySignal<Analysis>;
  trace: ReadonlySignal<TraceState>;
  stats: ReadonlySignal<{ original: Stats; determinized: Stats }>;
  /** Analyzes and runs the editor's text now. */
  commitSource: () => void;
  /** Runs the editor's text at a new seed. */
  rerun: (seed: number) => void;
  /** Adds the runs at `seeds`, and shows the last one in the step table. */
  runMany: (seeds: number[]) => void;
}

/** The pause in typing after which the editor's text is analyzed and run. */
const analysisDelayMs = 500;

let lastAnalysis: { source: string; analysis: Analysis } | null = null;

/** The analysis of `source`; the editor's hints and the store share the result for one text. */
export function analyzeSource(source: string): Analysis {
  if (lastAnalysis?.source !== source) lastAnalysis = { source, analysis: analyze(source) };
  return lastAnalysis.analysis;
}

function noSamples(source: string): Samples {
  return { source, original: [], determinized: [], lastKey: "" };
}

function errorMessage(error: unknown) {
  return error instanceof Error ? error.message : String(error);
}

export function createStore(initial: { source: string; seed: number; exampleId: string }): Store {
  const source = signal(initial.source);
  const checkedSource = signal(initial.source);
  const commits = signal(0);
  const seed = signal(initial.seed);
  const exampleId = signal(initial.exampleId);
  const sampleCount = signal(200);
  const typeHints = signal(false);
  const hoveredSpan = signal<Span | null>(null);
  const activeStep = signal<number | null>(null);
  const samples = signal(noSamples(initial.source));

  // A run that was already computed elsewhere, so that showing it in the step table doesn't
  // repeat it; runs are determined by their source and seed.
  let knownRun: { source: string; trace: CoupledTrace } | null = null;

  const analysis = computed(() => analyzeSource(checkedSource.value));
  const trace = computed((): TraceState => {
    const result = analysis.value;
    if (!result.ok && !result.counterexample) return { kind: "not run" };
    const program = checkedSource.value;
    const at = seed.value;
    if (knownRun?.source === program && knownRun.trace.seed === at) {
      return { kind: "run", trace: knownRun.trace };
    }
    try {
      return { kind: "run", trace: runCoupling(program, at) };
    } catch (error) {
      return { kind: "unavailable", message: errorMessage(error) };
    }
  });
  const stats = computed(() => ({
    original: sampleStats(samples.value.original),
    determinized: sampleStats(samples.value.determinized),
  }));

  /** The samples so far if they are of `program`, otherwise none. */
  function samplesOf(program: string) {
    const current = samples.peek();
    return current.source === program ? current : noSamples(program);
  }

  // The step table's run adds its sample, once for each source and seed.
  effect(() => {
    const state = trace.value;
    const program = checkedSource.value;
    untracked(() => {
      const base = samplesOf(program);
      const key = state.kind === "run" ? `${program}:${state.trace.seed}` : "";
      const sample = state.kind === "run" ? sampleOf(state.trace) : null;
      if (sample && key !== base.lastKey) {
        samples.value = {
          source: program,
          original: [...base.original, sample.original],
          determinized: [...base.determinized, sample.determinized],
          lastKey: key,
        };
      } else if (base !== samples.value) {
        samples.value = base;
      }
    });
  });

  let pending: ReturnType<typeof setTimeout> | undefined;
  const commitSource = action(() => {
    clearTimeout(pending);
    checkedSource.value = source.value;
    commits.value += 1;
  });

  // Every change of the text is committed after a pause, also one back to the committed text.
  let opened = true;
  effect(() => {
    source.value;
    if (opened) {
      opened = false;
      return;
    }
    clearTimeout(pending);
    pending = setTimeout(commitSource, analysisDelayMs);
  });

  const rerun = action((next: number) => {
    commitSource();
    seed.value = next;
  });

  const runMany = action((seeds: number[]) => {
    commitSource();
    const program = checkedSource.value;
    const batch = runBatch(program, seeds);
    const last = batch.last;
    const base = samplesOf(program);
    samples.value = {
      source: program,
      original: [...base.original, ...batch.original],
      determinized: [...base.determinized, ...batch.determinized],
      lastKey: last && sampleOf(last) ? `${program}:${last.seed}` : base.lastKey,
    };
    if (last) {
      knownRun = { source: program, trace: last };
      seed.value = last.seed;
    }
  });

  return {
    source,
    checkedSource,
    commits,
    seed,
    exampleId,
    sampleCount,
    typeHints,
    hoveredSpan,
    activeStep,
    samples,
    analysis,
    trace,
    stats,
    commitSource,
    rerun,
    runMany,
  };
}
