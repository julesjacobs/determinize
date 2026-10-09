// A port of Lean's `Finite/Explore.lean` and of the model that `Proof/FiniteModel/BuildModel.lean`
// makes of a complete exploration: the states reachable from a program's initial state, numbered
// in the order in which they are found, each with its transitions, within Lean's limits on the
// states, the edges and the size of a state. Exploration runs in steps, so that the thread it runs
// on can stop between them and report its progress.
import type { Program } from "../compiler/core.ts";
import type { State, Step } from "./machine.ts";
import { Failure, Machine, scoped } from "./machine.ts";
import type { Model, StateKind } from "./model.ts";
import type { Rational } from "./rational.ts";
import { add, isZero, lt, one, sum, zero } from "./rational.ts";
import { StateSizes } from "./repr.ts";

/** Lean's `Limits`, with its defaults. */
export interface Limits {
  maxStates: number;
  maxEdges: number;
  maxStateBytes: number;
}

export const defaultLimits: Limits = { maxStates: 10000, maxEdges: 100000, maxStateBytes: 1000000 };

/** Which of Lean's limits an exploration hit, as Lean's `Limit`. */
export type Limit = "states" | "edges" | "stateBytes";

/** Which program of a source a model is of, as Lean's `Subject`. */
export type Subject = "source" | "determinized";

/** How an exploration ended, as Lean's `Exploration`. A failure names the state at which it
 * occurred and Lean's failure, with the draw it occurred at. */
export type Exploration =
  | { kind: "complete"; model: Model }
  | { kind: "incomplete"; limit: Limit; discovered: number; expanded: number; edges: number }
  | { kind: "failed"; state: number; failure: Failure };

/** Lean's `Limit` as its derived `Repr` prints it. */
function limitRepr(limit: Limit) {
  return `Determinize.Finite.Limit.${limit}`;
}

/** What the Lean CLI prints for an exploration that didn't complete. */
export function explorationMessage(
  exploration: Exclude<Exploration, { kind: "complete" }>,
): string {
  if (exploration.kind === "failed") {
    return `Exploration failed at state ${exploration.state}: ${exploration.failure.message}. No export written.`;
  }
  const { limit, discovered, expanded, edges } = exploration;
  return `Incomplete exploration (${limitRepr(limit)}): ${discovered} discovered, ${expanded} expanded, ${edges} edges. No export written.`;
}

/** The program of a subject: the source, or its determinization. */
export function subjectProgram(
  programs: { source: Program; determinized: Program },
  subject: Subject,
) {
  return subject === "source" ? programs.source : programs.determinized;
}

/** One state's step, as Lean's `Builder.readAction`: its kind and its successors, a terminal state
 * looping to itself; or the failure. */
export function readAction(
  machine: Machine,
  state: State,
): { kind: StateKind; successors: [Rational, State][] } | Failure {
  let step: Step;
  try {
    step = machine.step(state);
  } catch (error) {
    if (error instanceof Failure) return error;
    throw error;
  }
  if (step.kind === "returned") {
    return { kind: { kind: "returned", reward: step.reward }, successors: [[one, state]] };
  }
  if (step.kind === "rejected") return { kind: { kind: "rejected" }, successors: [[one, state]] };
  const { successors } = step;
  if (successors.some(([p]) => lt(p, zero))) {
    return new Failure("invalid", "negative transition probability");
  }
  if (!isZero(add(sum(successors.map(([p]) => p)), { num: -1n, den: 1n }))) {
    return new Failure("invalid", "transition probabilities do not sum to one");
  }
  return { kind: { kind: "transient" }, successors };
}

/** An exploration in progress: `run` takes steps until it ends or its deadline passes. */
export interface Explorer {
  /** Takes steps until the exploration ends or `deadline()` is true; the result once it ended. */
  run(deadline: () => boolean): Exploration | null;
  /** The states found and expanded so far. */
  progress(): { discovered: number; expanded: number };
}

/** The exploration of `program`'s states, as Lean's `explore`; `source` is the program whose
 * variables must be bound. */
export function explorer(
  program: Program,
  source: Program,
  limits: Limits = defaultLimits,
): Explorer {
  const machine = new Machine();
  const sizes = new StateSizes();
  const initial = machine.initial(program);
  const states: State[] = [initial];
  const index = new Map<number, number>([[initial.id, 0]]);
  const rows: { kind: StateKind; successors: [Rational, State][] }[] = [];
  let edgeCount = 0;
  let remaining = limits.maxStates;
  let result: Exploration | null = null;
  if (!scoped(source)) {
    result = {
      kind: "failed",
      state: 0,
      failure: new Failure("invalid", "source contains an unbound variable"),
    };
  } else if (limits.maxStates === 0) {
    result = { kind: "incomplete", limit: "states", discovered: 0, expanded: 0, edges: 0 };
  } else if (sizes.exceeds(initial, limits.maxStateBytes)) {
    result = { kind: "incomplete", limit: "stateBytes", discovered: 0, expanded: 0, edges: 0 };
  }

  function incomplete(
    limit: Limit,
    discovered: number,
    expanded: number,
    edges: number,
  ): Exploration {
    return { kind: "incomplete", limit, discovered, expanded, edges };
  }

  /** One step of Lean's `build`, or its result. */
  function build(): Exploration | null {
    if (rows.length === states.length) return { kind: "complete", model: model() };
    if (remaining === 0) return incomplete("states", states.length, rows.length, edgeCount);
    remaining -= 1;
    const action = readAction(machine, states[rows.length]);
    if (action instanceof Failure) return { kind: "failed", state: rows.length, failure: action };
    const positive = action.successors.filter(([p]) => lt(zero, p));
    const outgoing = new Set(positive.map(([, state]) => state.id)).size;
    if (edgeCount + outgoing > limits.maxEdges) {
      return incomplete("edges", states.length, rows.length, edgeCount);
    }
    if (positive.some(([, state]) => sizes.exceeds(state, limits.maxStateBytes))) {
      return incomplete("stateBytes", states.length, rows.length, edgeCount);
    }
    for (const [, state] of positive) {
      if (index.has(state.id)) continue;
      index.set(state.id, states.length);
      states.push(state);
    }
    rows.push(action);
    edgeCount += outgoing;
    if (states.length > limits.maxStates) {
      return incomplete("states", states.length, rows.length, edgeCount);
    }
    return null;
  }

  /** The model of a complete exploration, as `Work.candidate` with `sparseEdges`. */
  function model(): Model {
    return {
      size: states.length,
      kind: rows.map((row) => row.kind),
      rows: rows.map((row) => {
        const weights = new Map<number, Rational>();
        for (const [p, state] of row.successors) {
          const target = index.get(state.id);
          if (target === undefined) continue;
          weights.set(target, add(weights.get(target) ?? zero, p));
        }
        return [...weights]
          .filter(([, p]) => lt(zero, p))
          .sort(([a], [b]) => a - b)
          .map(([target, probability]) => ({ target, probability }));
      }),
    };
  }

  return {
    run(deadline) {
      while (!result) {
        result = build();
        if (!result && deadline()) return null;
      }
      return result;
    },
    progress: () => ({ discovered: states.length, expanded: rows.length }),
  };
}

/** The whole exploration of `program`, as Lean's `explore`. */
export function explore(
  program: Program,
  source: Program,
  limits: Limits = defaultLimits,
): Exploration {
  const result = explorer(program, source, limits).run(() => false);
  if (!result) throw new Error("an exploration without a deadline ended early");
  return result;
}
