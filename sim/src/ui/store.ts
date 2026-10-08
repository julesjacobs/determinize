// The simulator's state: its inputs as signals, what follows from them as computed signals, and
// the actions that change them.
import type { ReadonlySignal, Signal } from "@preact/signals-core";
import { action, computed, effect, signal, untracked } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import { analyze } from "../core/compiler/analyze.ts";
import type {
  Request,
  Response,
  TracePageRequest,
  TraceRequest,
  TraceResponse,
} from "../core/protocol.ts";
import type { GDraw } from "../core/runtime/eval.ts";
import type { Runner } from "../core/sampler.ts";
import { runIndex, runnerOf, siteDraws } from "../core/sampler.ts";
import type { Runs, Stats, Summary } from "../core/statistics.ts";
import { addRuns, noRuns, runsOf, statsOf } from "../core/statistics.ts";
import type { TraceOverview, TracePage } from "../core/trace-pages.ts";

/** A range of the source. */
export interface Span {
  from: number;
  to: number;
}

/**
 * The runs of one program: what Lean's CLI reports about them, the number each returned (NaN if
 * none), and the value each drew at each continuous G site (NaN unless it drew there exactly
 * once). Later runs of the same program and seed append to the arrays in place.
 */
export interface ProgramRuns {
  summary: Summary;
  values: readonly number[];
  draws: ReadonlyMap<number, readonly number[]>;
}

/** The runs of a program and of its determinization from a seed: run i at the seed plus i, as
 * Lean's CLI runs them. Run 0 is the step table's run; `traces` are its G traces. */
export interface Samples {
  source: string;
  seed: number;
  original: ProgramRuns;
  determinized: ProgramRuns;
  traces: { source: GDraw[]; determinized: GDraw[] } | null;
}

/** The step table's run of the checked program, which its worker computes and sends a page at a
 * time. */
export type TraceState =
  | { kind: "not run" }
  | { kind: "computing" }
  | { kind: "run"; overview: TraceOverview; page: TracePage }
  | { kind: "unavailable"; message: string };

export interface Store {
  /** The editor's text, at every change. */
  source: Signal<string>;
  /** The text that is analyzed and run: the editor's text after a pause in typing. */
  checkedSource: ReadonlySignal<string>;
  /** Counts the times the editor's text was analyzed and run, also when it hadn't changed. */
  commits: ReadonlySignal<number>;
  /** The seed of the step table's run, which is run 0 of the runs. */
  seed: ReadonlySignal<number>;
  /** The example chosen last. */
  exampleId: Signal<string>;
  /** The number of runs that "Run N" adds. */
  sampleCount: Signal<number>;
  /** The step of the step table whose checks are shown. */
  activeStep: Signal<number | null>;
  samples: ReadonlySignal<Samples>;
  /** The batch of runs in progress, of `source` at `seed`, up to run `end`. */
  running: ReadonlySignal<{ generation: number; source: string; seed: number; end: number } | null>;
  analysis: ReadonlySignal<Analysis>;
  /** The step table's run; computing while its worker works on it. */
  trace: ReadonlySignal<TraceState>;
  stats: ReadonlySignal<{ original: Stats; determinized: Stats }>;
  /** Analyzes and runs the editor's text now. */
  commitSource: () => void;
  /** Analyzes the editor's text now and runs it at `seed`. */
  runAt: (seed: number) => void;
  /** Starts a batch of `count` more runs, after the remaining runs of a batch in progress. */
  runMany: (count: number) => void;
  /** Takes in what the sampler reports about a batch. */
  receive: (response: Response) => void;
  /** Asks the step table's worker for page `index` of its run. */
  showPage: (index: number) => void;
  /** Takes in the step table's run, or a page of it, from its worker. */
  receiveTrace: (response: TraceResponse) => void;
}

/** The pause in typing after which the editor's text is analyzed and run. */
export const analysisDelayMs = 500;

let lastAnalysis: { source: string; analysis: Analysis } | null = null;

/** The analysis of `source`; the editor's hints and the store share the result for one text. */
export function analyzeSource(source: string): Analysis {
  if (lastAnalysis?.source !== source) lastAnalysis = { source, analysis: analyze(source) };
  return lastAnalysis.analysis;
}

function noSamples(source: string, seed: number): Samples {
  return {
    source,
    seed,
    original: { summary: noRuns, values: [], draws: new Map() },
    determinized: { summary: noRuns, values: [], draws: new Map() },
    traces: null,
  };
}

/** Run 0 of `program` at `seed`, the step table's run, as the evaluator runs it. */
function firstRun(program: string, seed: number, runner: Runner | null): Samples {
  const samples = noSamples(program, seed);
  if (!runner) return samples;
  const first = runIndex(runner, seed, 0);
  const { gSites } = runner;
  samples.original = addTo(
    samples.original,
    runsOf([first.source], siteDraws(gSites, [first.traces.source])),
  );
  samples.determinized = addTo(
    samples.determinized,
    runsOf([first.determinized], siteDraws(gSites, [first.traces.determinized])),
  );
  samples.traces = first.traces;
  return samples;
}

function append(target: number[], values: Float64Array) {
  for (const value of values) target.push(value);
}

function addTo(runs: ProgramRuns, more: Runs): ProgramRuns {
  append(runs.values as number[], more.values);
  const draws = runs.draws as Map<number, number[]>;
  for (const { site, values } of more.draws) {
    const known = draws.get(site);
    if (known) append(known, values);
    else draws.set(site, Array.from(values));
  }
  return { summary: addRuns(runs.summary, more), values: runs.values, draws };
}

/** The G sites that every run so far drew exactly once: those against whose draws its returned
 * numbers can be plotted. */
export function eligibleSites(runs: ProgramRuns): number[] {
  return [...runs.draws]
    .filter(([, values]) => values.every((value) => !Number.isNaN(value)))
    .map(([site]) => site);
}

/**
 * The store, starting from `initial`; `send` hands a request to the sampler, whose responses go
 * to `receive`, and `sendTrace` one to the step table's worker, whose responses go to
 * `receiveTrace`.
 */
export function createStore(
  initial: { source: string; seed: number; exampleId: string },
  send: (request: Request) => void,
  sendTrace: (request: TraceRequest | TracePageRequest) => void,
): Store {
  const source = signal(initial.source);
  const checkedSource = signal(initial.source);
  const commits = signal(0);
  const seed = signal(initial.seed);
  const exampleId = signal(initial.exampleId);
  const sampleCount = signal(200);
  const activeStep = signal<number | null>(null);
  const analysis = computed(() => analyzeSource(checkedSource.value));
  const runner = computed(() => runnerOf(analysis.value));
  const samples = signal(firstRun(initial.source, initial.seed, runner.peek()));
  const running = signal<{ generation: number; source: string; seed: number; end: number } | null>(
    null,
  );
  let generation = 0;
  const trace = signal<TraceState>({ kind: "not run" });
  let traceGeneration = 0;
  // The step table's run of each new program or seed; the worker's earlier answers are stale.
  effect(() => {
    const result = analysis.value;
    const program = checkedSource.value;
    const at = seed.value;
    traceGeneration += 1;
    if (!result.ok && !result.counterexample) {
      trace.value = { kind: "not run" };
      return;
    }
    trace.value = { kind: "computing" };
    untracked(() =>
      sendTrace({ type: "trace", generation: traceGeneration, source: program, seed: at }),
    );
  });
  const stats = computed(() => ({
    original: statsOf(samples.value.original.summary),
    determinized: statsOf(samples.value.determinized.summary),
  }));

  /** The runs of the checked program at the seed so far, from run 0. */
  function currentSamples(): Samples {
    const program = checkedSource.peek();
    const at = seed.peek();
    const existing = samples.peek();
    if (existing.source === program && existing.seed === at) return existing;
    const next = firstRun(program, at, runner.peek());
    samples.value = next;
    return next;
  }

  effect(() => {
    checkedSource.value;
    seed.value;
    untracked(currentSamples);
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

  const runMany = action((count: number) => {
    commitSource();
    const { source: program, seed: at, original } = currentSamples();
    // A batch in progress goes on with its remaining runs, followed by the new ones.
    const current = running.value;
    const from = original.summary.runs;
    const end = (current?.source === program && current.seed === at ? current.end : from) + count;
    generation += 1;
    running.value = { generation, source: program, seed: at, end };
    send({ type: "run", generation, source: program, seed: at, from, count: end - from });
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
    if (response.type === "done") {
      running.value = null;
      return;
    }
    const base = samples.peek();
    if (base.source !== current.source || base.seed !== current.seed) return;
    samples.value = {
      ...base,
      original: addTo(base.original, response.source),
      determinized: addTo(base.determinized, response.determinized),
    };
  });

  const showPage = action((index: number) => {
    const state = trace.peek();
    if (state.kind !== "run" || state.page.index === index) return;
    sendTrace({ type: "trace-page", generation: traceGeneration, page: index });
  });

  const receiveTrace = action((response: TraceResponse) => {
    if (response.generation !== traceGeneration) return;
    if (response.type === "trace-page") {
      const state = trace.peek();
      if (state.kind === "run") trace.value = { ...state, page: response.page };
      return;
    }
    trace.value =
      response.type === "trace"
        ? { kind: "run", overview: response.overview, page: response.page }
        : { kind: "unavailable", message: response.message };
  });

  return {
    source,
    checkedSource,
    commits,
    seed,
    exampleId,
    sampleCount,
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
    showPage,
    receiveTrace,
  };
}
