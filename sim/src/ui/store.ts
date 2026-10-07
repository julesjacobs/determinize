// The simulator's state: its inputs as signals, what follows from them as computed signals, and
// the actions that change them.
import type { ReadonlySignal, Signal } from "@preact/signals-core";
import { action, computed, effect, signal, untracked } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import { analyze } from "../core/compiler/analyze.ts";
import type { Request, Response } from "../core/protocol.ts";
import type { CoupledTrace } from "../core/runtime/semantics.ts";
import type { Runs, Stats, Summary } from "../core/statistics.ts";
import { addRuns, noRuns, statsOf } from "../core/statistics.ts";
import { outcomesOf, runCoupling, runsOf } from "../core/trace.ts";

/** A range of the source. */
export interface Span {
  from: number;
  to: number;
}

/** The runs of one program: what Lean's CLI reports about them, and the numbers they returned. */
export interface ProgramRuns {
  summary: Summary;
  values: number[];
}

/** The runs of a program and of its determinization. */
export interface Samples {
  source: string;
  original: ProgramRuns;
  determinized: ProgramRuns;
  /** The last run that was added, as `source:seed`. */
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
  /** The batch of runs in progress, of `source`. */
  running: ReadonlySignal<{ generation: number; source: string } | null>;
  analysis: ReadonlySignal<Analysis>;
  trace: ReadonlySignal<TraceState>;
  stats: ReadonlySignal<{ original: Stats; determinized: Stats }>;
  /** Analyzes and runs the editor's text now. */
  commitSource: () => void;
  /** Analyzes the editor's text now and runs it at `seed`. */
  runAt: (seed: number) => void;
  /**
   * Starts a batch of runs at `seeds`, after the remaining runs of a batch in progress; the last
   * run is shown in the step table.
   */
  runMany: (seeds: number[]) => void;
  /** Takes in what the sampler reports about a batch. */
  receive: (response: Response) => void;
}

/** The pause in typing after which the editor's text is analyzed and run. */
const analysisDelayMs = 500;

let lastAnalysis: { source: string; analysis: Analysis } | null = null;

/** The analysis of `source`; the editor's hints and the store share the result for one text. */
export function analyzeSource(source: string): Analysis {
  if (lastAnalysis?.source !== source) lastAnalysis = { source, analysis: analyze(source) };
  return lastAnalysis.analysis;
}

const noProgramRuns: ProgramRuns = { summary: noRuns, values: [] };

function noSamples(source: string): Samples {
  return { source, original: noProgramRuns, determinized: noProgramRuns, lastKey: "" };
}

function addTo(runs: ProgramRuns, more: Runs): ProgramRuns {
  const values = runs.values.slice();
  for (const x of more.values) if (!Number.isNaN(x)) values.push(x);
  return { summary: addRuns(runs.summary, more), values };
}

function errorMessage(error: unknown) {
  return error instanceof Error ? error.message : String(error);
}

/**
 * The store, starting from `initial`; `send` hands a request to the sampler, whose responses go
 * to `receive`.
 */
export function createStore(
  initial: { source: string; seed: number; exampleId: string },
  send: (request: Request) => void,
): Store {
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
  const running = signal<{ generation: number; source: string } | null>(null);
  let generation = 0;
  // The seeds of the batch in progress, and how many of them have reported.
  let batchSeeds = new Float64Array(0);
  let reported = 0;

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
    original: statsOf(samples.value.original.summary),
    determinized: statsOf(samples.value.determinized.summary),
  }));

  /** The samples so far if they are of `program`, otherwise none. */
  function samplesOf(program: string) {
    const current = samples.peek();
    return current.source === program ? current : noSamples(program);
  }

  // The step table's run adds its outcomes, once for each source and seed.
  effect(() => {
    const state = trace.value;
    const program = checkedSource.value;
    untracked(() => {
      const base = samplesOf(program);
      const key = state.kind === "run" ? `${program}:${state.trace.seed}` : "";
      if (state.kind === "run" && key !== base.lastKey) {
        const outcomes = outcomesOf(state.trace);
        samples.value = {
          source: program,
          original: addTo(base.original, runsOf([outcomes.source])),
          determinized: addTo(base.determinized, runsOf([outcomes.determinized])),
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

  const runAt = action((next: number) => {
    commitSource();
    seed.value = next;
  });

  const runMany = action((seeds: number[]) => {
    commitSource();
    const program = checkedSource.value;
    // A batch in progress goes on with its remaining seeds, followed by the new ones.
    const left =
      running.value?.source === program ? batchSeeds.subarray(reported) : new Float64Array(0);
    batchSeeds = new Float64Array(left.length + seeds.length);
    batchSeeds.set(left);
    batchSeeds.set(seeds, left.length);
    reported = 0;
    generation += 1;
    running.value = { generation, source: program };
    send({ type: "run", generation, source: program, seeds: batchSeeds });
  });

  // Another program or seed makes the batch in progress stale.
  effect(() => {
    source.value;
    seed.value;
    untracked(() => {
      const current = running.value;
      if (!current) return;
      running.value = null;
      send({ type: "cancel", generation: current.generation });
    });
  });

  const receive = action((response: Response) => {
    const current = running.value;
    if (current?.generation !== response.generation) return;
    const base = samplesOf(current.source);
    if (response.type === "batch") {
      reported += response.source.values.length;
      samples.value = {
        source: current.source,
        original: addTo(base.original, response.source),
        determinized: addTo(base.determinized, response.determinized),
        lastKey: base.lastKey,
      };
      return;
    }
    running.value = null;
    const last = response.last;
    if (!last) return;
    samples.value = { ...base, lastKey: `${current.source}:${last.seed}` };
    knownRun = { source: current.source, trace: last };
    seed.value = last.seed;
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
    running,
    analysis,
    trace,
    stats,
    commitSource,
    runAt,
    runMany,
    receive,
  };
}
