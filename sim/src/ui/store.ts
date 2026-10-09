// The simulator's state: its inputs as signals, what follows from them as computed signals, and
// the actions that change them.
import type { ReadonlySignal, Signal } from "@preact/signals-core";
import { action, computed, effect, signal, untracked } from "@preact/signals-core";
import type { Analysis } from "../core/compiler/analyze.ts";
import { analyze } from "../core/compiler/analyze.ts";
import type { ExactState } from "../core/exact.ts";
import type {
  ExactRequest,
  ExactResponse,
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
import type { Reduced } from "./steps.ts";

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

/** The page's colours: the system's scheme, or light or dark. */
export type Theme = "system" | "light" | "dark";

/** The distributions band's charts: the output distributions, or each run's output against a G
 * draw. */
export type ChartView = "outputs" | "against";

/** The step table's run of the checked program, which its worker computes and sends a page at a
 * time. */
export type TraceState =
  | { kind: "not run" }
  | { kind: "computing" }
  | { kind: "run"; overview: TraceOverview; page: TracePage }
  | { kind: "unavailable"; message: string };

/** The exact values of the checked program and of its determinization, from their finite models,
 * plain or in the additive mode: each program's exploration so far, or its outcome. */
export interface ExactValues {
  source: string;
  additive: boolean;
  programs: { source: ExactState; determinized: ExactState };
  /** Whether both explorations have ended. */
  done: boolean;
}

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
  /** The number of runs of each program that sampling brings the runs to, after an edit, a new
   * seed or a new count. */
  sampleCount: Signal<number>;
  /** The index of the run that the step table shows: run i is at the seed plus i. */
  shownRun: ReadonlySignal<number>;
  /** The G traces of the run that the step table shows. */
  shownTraces: ReadonlySignal<{ source: GDraw[]; determinized: GDraw[] } | null>;
  /** Which chart the distributions band shows. */
  view: Signal<ChartView>;
  /** The G site whose draws the conditional-mean plot puts on its x axis, by its index in Lean's
   * `Expr.sites`; null for the first eligible one. */
  plotSite: Signal<number | null>;
  /** The step of the step table's run that the table shows as current. */
  currentStep: Signal<number>;
  /** The row of the step table under the pointer or the focus. */
  hoveredStep: Signal<number | null>;
  /** The sample site under the pointer in either program pane, as a range of the checked source. */
  hoveredSite: Signal<Span | null>;
  /** What the pointer is on in either program pane, as a range of the checked source: a position
   * in the source pane, the smallest printed node's source in the determinized one. */
  hoveredRange: Signal<Span | null>;
  /** The part of the checked source that both panes highlight: what the hovered or current step
   * reduces, or the hovered site. */
  linked: Signal<Reduced | null>;
  /** Counts the reader's moves of the current step and hovers, each a request to scroll the
   * program panes to the linked lines; a new run or an edit changes `linked` without one. */
  followLinked: Signal<number>;
  theme: Signal<Theme>;
  samples: ReadonlySignal<Samples>;
  /** The batch of runs in progress, of `source` at `seed`, up to run `end`, on the way to
   * `target`. */
  running: ReadonlySignal<Running | null>;
  /** Sampling stopped before it reached its target: at its time budget, or on request. */
  paused: ReadonlySignal<{ why: "budget" | "stopped"; target: number } | null>;
  /** Whether the samples are the checked program's at the seed, and its first slice of runs after
   * run 0 has come in or sampling stopped before it did; until then the distributions and the check
   * keep showing the earlier runs, as run 0 alone would show what isn't so. */
  samplesReady: ReadonlySignal<boolean>;
  analysis: ReadonlySignal<Analysis>;
  /** The step table's run; computing while its worker works on it. */
  trace: ReadonlySignal<TraceState>;
  stats: ReadonlySignal<{ original: Stats; determinized: Stats }>;
  /** Whether the finite models are explored in Lean's additive mode (`--additive`). */
  additive: Signal<boolean>;
  /** The exact values of the checked program; null where Lean rejects it. */
  exact: ReadonlySignal<ExactValues | null>;
  /** Analyzes and runs the editor's text now. */
  commitSource: () => void;
  /** Analyzes the editor's text now and runs it at `seed`. */
  runAt: (seed: number) => void;
  /** Samples the runs of a new seed. */
  resample: () => void;
  /** Goes on sampling after a pause, with a new time budget. */
  resume: () => void;
  /** Stops sampling; the runs so far stay. */
  stop: () => void;
  /** Shows run `index` of the runs in the step table. */
  pickRun: (index: number) => void;
  /** Takes in what the sampler reports about a batch. */
  receive: (response: Response) => void;
  /** Asks the step table's worker for page `index` of its run. */
  showPage: (index: number) => void;
  /** Takes in the step table's run, or a page of it, from its worker. */
  receiveTrace: (response: TraceResponse) => void;
  /** Takes in the exact worker's explorations. */
  receiveExact: (response: ExactResponse) => void;
}

/** The pause in typing after which the editor's text is analyzed and run. */
export const analysisDelayMs = 500;

/** How long sampling runs before it stops and offers to go on. */
export const samplingBudgetMs = 5000;

/** A batch of runs in progress. */
export interface Running {
  generation: number;
  source: string;
  seed: number;
  end: number;
  target: number;
  /** When sampling towards `target` started, for its time budget. */
  since: number;
}

/** A random seed for a run, from 1 to 2³² − 1. */
export function randomSeed() {
  return Math.floor(1 + Math.random() * 0xffffffff);
}

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
 * to `receive`, `sendTrace` one to the step table's worker, whose responses go to
 * `receiveTrace`, and `sendExact` one to the exact worker, whose responses go to `receiveExact`.
 */
export function createStore(
  initial: {
    source: string;
    seed: number;
    exampleId: string;
    view?: ChartView;
    theme?: Theme;
    /** The runs of the first batch, which shows a program's distributions soon before the rest
     * of its runs arrive. */
    firstBatch?: number;
    additive?: boolean;
  },
  send: (request: Request) => void,
  sendTrace: (request: TraceRequest | TracePageRequest) => void,
  sendExact: (request: ExactRequest) => void,
): Store {
  const source = signal(initial.source);
  const checkedSource = signal(initial.source);
  const commits = signal(0);
  const seed = signal(initial.seed);
  const exampleId = signal(initial.exampleId);
  const sampleCount = signal(10000);
  const shownRun = signal(0);
  const view = signal<ChartView>(initial.view ?? "outputs");
  const plotSite = signal<number | null>(null);
  const currentStep = signal(0);
  const hoveredStep = signal<number | null>(null);
  const hoveredSite = signal<Span | null>(null);
  const hoveredRange = signal<Span | null>(null);
  const linked = signal<Reduced | null>(null);
  const followLinked = signal(0);
  const theme = signal<Theme>(initial.theme ?? "system");
  const analysis = computed(() => analyzeSource(checkedSource.value));
  const runner = computed(() => runnerOf(analysis.value));
  const samples = signal(firstRun(initial.source, initial.seed, runner.peek()));
  const running = signal<Running | null>(null);
  const paused = signal<{ why: "budget" | "stopped"; target: number } | null>(null);
  const firstBatch = initial.firstBatch ?? 1000;
  const samplesReady = computed(() => {
    const current = samples.value;
    if (current.source !== checkedSource.value || current.seed !== seed.value) return false;
    const first = Math.min(2, sampleCount.value);
    return current.original.summary.runs >= first || paused.value !== null;
  });
  let generation = 0;
  const trace = signal<TraceState>({ kind: "not run" });
  let traceGeneration = 0;
  // The step table's run of each new program or seed; the worker's earlier answers are stale.
  effect(() => {
    const result = analysis.value;
    const program = checkedSource.value;
    const at = seed.value + shownRun.value;
    traceGeneration += 1;
    currentStep.value = 0;
    if (!result.ok && !result.counterexample) {
      trace.value = { kind: "not run" };
      return;
    }
    if (!Number.isSafeInteger(at)) {
      trace.value = {
        kind: "unavailable",
        message: "this run's seed is too large for the step table",
      };
      return;
    }
    trace.value = { kind: "computing" };
    untracked(() =>
      sendTrace({ type: "trace", generation: traceGeneration, source: program, seed: at }),
    );
  });
  const additive = signal(initial.additive ?? false);
  const exact = signal<ExactValues | null>(null);
  let exactGeneration = 0;
  // Each analysed program, and each mode, is explored afresh; the worker's earlier answers are
  // stale.
  effect(() => {
    const result = analysis.value;
    const program = checkedSource.value;
    const mode = additive.value;
    exactGeneration += 1;
    // The worker drops the exploration of an earlier program also for one that Lean rejects.
    const request = { type: "exact" as const, generation: exactGeneration, source: program };
    untracked(() => sendExact({ ...request, additive: mode }));
    if (!result.ok) {
      exact.value = null;
      return;
    }
    const exploring: ExactState = { kind: "exploring", discovered: 0 };
    exact.value = {
      source: program,
      additive: mode,
      programs: { source: exploring, determinized: exploring },
      done: false,
    };
  });
  const shownTraces = computed(() => {
    const index = shownRun.value;
    if (index === 0) return samples.value.traces;
    const at = runner.value;
    return at ? runIndex(at, seed.value, index).traces : null;
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
  // Another program or seed shows its run 0 again.
  const commitSource = action(() => {
    clearTimeout(pending);
    if (checkedSource.value !== source.value) shownRun.value = 0;
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
    shownRun.value = 0;
    seed.value = next;
  });

  /** Sends a batch of runs from run `from` towards `target`: up to the first batch's end, or to
   * the target. */
  function sendBatch(program: string, at: number, from: number, target: number, since: number) {
    const end = from < firstBatch ? Math.min(target, firstBatch) : target;
    generation += 1;
    running.value = { generation, source: program, seed: at, end, target, since };
    send({ type: "run", generation, source: program, seed: at, from, count: end - from });
  }

  function cancelBatch() {
    const current = running.peek();
    if (!current) return;
    running.value = null;
    send({ type: "cancel", generation: current.generation });
  }

  /** Brings the runs of the checked program at the seed to `target`: on from the runs so far, or
   * from run 0 again for fewer runs than there are. */
  const sampleTo = action((target: number) => {
    cancelBatch();
    paused.value = null;
    if (!runner.peek()) return;
    let { source: program, seed: at, original } = currentSamples();
    if (original.summary.runs > target) {
      samples.value = firstRun(program, at, runner.peek());
      ({ source: program, seed: at, original } = samples.peek());
    }
    if (original.summary.runs < target) {
      sendBatch(program, at, original.summary.runs, target, performance.now());
    }
  });

  // Each analysed program and each seed is sampled at once, as is each new count of runs; a commit
  // of the same text, as after an edit undone within the pause, goes on with the runs so far.
  effect(() => {
    checkedSource.value;
    seed.value;
    analysis.value;
    commits.value;
    const target = sampleCount.value;
    untracked(() => sampleTo(target));
  });

  const resample = action(() => {
    runAt(randomSeed());
  });

  const resume = action(() => {
    const target = paused.peek()?.target ?? sampleCount.peek();
    sampleTo(target);
  });

  const stop = action(() => {
    const current = running.peek();
    if (!current) return;
    cancelBatch();
    paused.value = { why: "stopped", target: current.target };
  });

  const pickRun = action((index: number) => {
    shownRun.value = index;
  });

  // Typing makes a batch of another text stale, before the text is analysed again.
  effect(() => {
    const text = source.value;
    untracked(() => {
      if (running.peek()?.source !== text) cancelBatch();
    });
  });

  const receive = action((response: Response) => {
    const current = running.value;
    if (current?.generation !== response.generation) return;
    const base = samples.peek();
    if (base.source !== current.source || base.seed !== current.seed) {
      cancelBatch();
      return;
    }
    if (response.type === "batch") {
      samples.value = {
        ...base,
        original: addTo(base.original, response.source),
        determinized: addTo(base.determinized, response.determinized),
      };
    }
    const done = response.type === "done";
    const over = performance.now() - current.since > samplingBudgetMs;
    if (over && samples.peek().original.summary.runs < current.target) {
      // A heavy program stops at its time budget and offers to go on.
      cancelBatch();
      paused.value = { why: "budget", target: current.target };
    } else if (done && current.end < current.target) {
      sendBatch(current.source, current.seed, current.end, current.target, current.since);
    } else if (done) {
      running.value = null;
    }
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

  const receiveExact = action((response: ExactResponse) => {
    const current = exact.peek();
    if (response.generation !== exactGeneration || !current) return;
    exact.value = {
      ...current,
      programs: { source: response.source, determinized: response.determinized },
      done: response.done,
    };
  });

  return {
    source,
    checkedSource,
    commits,
    seed,
    exampleId,
    sampleCount,
    currentStep,
    hoveredStep,
    hoveredSite,
    hoveredRange,
    linked,
    followLinked,
    theme,
    samples,
    running,
    paused,
    samplesReady,
    analysis,
    trace,
    stats,
    commitSource,
    runAt,
    resample,
    resume,
    stop,
    pickRun,
    shownRun,
    shownTraces,
    view,
    plotSite,
    receive,
    showPage,
    receiveTrace,
    additive,
    exact,
    receiveExact,
  };
}
