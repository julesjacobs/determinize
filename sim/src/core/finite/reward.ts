// A port of Lean's additive mode (`--additive`, `Finite/Reward/*`): exploration that splits off
// the outermost pending additions of a state as a reward on the edge that leads to it, so that a
// loop like `acc + …` reaches finitely many states, and the statistics of the resulting reward
// model (`Spec/RewardModel/Model.lean`), whose edges add their rewards to the eventual output.
import type { Program } from "../compiler/core.ts";
import type { Limit, Limits } from "./explore.ts";
import { defaultLimits } from "./explore.ts";
import type { Frame, Stack, State } from "./machine.ts";
import { Failure, Machine, scoped } from "./machine.ts";
import type { Model, StateKind } from "./model.ts";
import type { Rational } from "./rational.ts";
import { add, equal, integer, lt, mul, one, sum, zero } from "./rational.ts";
import { StateSizes } from "./repr.ts";
import {
  analyze,
  cut,
  defaultSolveStates,
  eliminate,
  findPaths,
  finish,
  stateLimitMessage,
} from "./solve.ts";
import type { Solved } from "./statistics.ts";
import { exactResult } from "./statistics.ts";

/** An edge of a reward model: its target, its probability, and the reward it adds. */
export interface RewardEdge {
  target: number;
  probability: Rational;
  reward: Rational;
}

/** A reward model, as Lean's `Spec.RewardModel.Model`, whose initial state is 0. */
export interface RewardModel {
  size: number;
  kind: StateKind[];
  edges: RewardEdge[][];
}

/** An outcome of a normalized step, as Lean's `Reward.Outcome`. */
interface Outcome {
  probability: Rational;
  state: State;
  reward: Rational;
}

type RewardStep =
  | { kind: "next"; successors: Outcome[] }
  | { kind: "returned"; value: Rational }
  | { kind: "rejected" };

export type RewardExploration =
  | { kind: "complete"; model: RewardModel }
  | { kind: "incomplete"; limit: Limit; discovered: number; expanded: number; edges: number }
  | { kind: "failed"; state: number; failure: Failure };

/** Normalization of one machine's states, as Lean's `Reward.normalize`. */
class Normalizer {
  readonly machine: Machine;
  /** Lean's `guard`: the numeric check that stays when the additions are split off. */
  private readonly guard: Frame;

  constructor(machine: Machine) {
    this.machine = machine;
    this.guard = machine.makeFrame({ tag: "right", op: "add", left: machine.number(zero) });
  }

  /** Whether `frame` adds an evaluated number: `.right .add (.number c)`. */
  private offset(frame: Frame): Rational | null {
    return frame.tag === "right" && frame.op === "add" && frame.left.tag === "number"
      ? frame.left.value
      : null;
  }

  /** As Lean's `normalizeStack`: the sum of the outermost run of additions of evaluated numbers,
   * and the stack with that run replaced by the guard. */
  private stack(stack: Stack): [Rational, Stack] {
    const frames: Frame[] = [];
    for (let cell = stack; cell; cell = cell.tail) frames.push(cell.head);
    let inner = frames.length;
    while (inner > 0 && this.offset(frames[inner - 1]) !== null) inner--;
    if (inner === frames.length) return [zero, stack];
    if (inner === frames.length - 1 && frames[inner] === this.guard) return [zero, stack];
    const offsets = frames.slice(inner).map((frame) => this.offset(frame) as Rational);
    let rebuilt: Stack = this.machine.push(this.guard, null);
    for (let i = inner - 1; i >= 0; i--) rebuilt = this.machine.push(frames[i], rebuilt);
    return [sum(offsets), rebuilt];
  }

  /** As Lean's `normalize`. */
  state(state: State): [Rational, State] {
    if (state.tag === "rejected") return [zero, state];
    const [reward, stack] = this.stack(state.stack);
    if (stack === state.stack) return [reward, state];
    const normalized =
      state.tag === "eval"
        ? this.machine.evalState(state.expr, state.env, stack)
        : this.machine.deliver(state.value, stack);
    return [reward, normalized];
  }

  /** One transition followed by normalization, as Lean's `Reward.step`. */
  step(state: State): RewardStep {
    const step = this.machine.step(state);
    if (step.kind === "returned") return { kind: "returned", value: step.reward };
    if (step.kind === "rejected") return { kind: "rejected" };
    return {
      kind: "next",
      successors: step.successors.map(([probability, next]) => {
        const [reward, normalized] = this.state(next);
        return { probability, state: normalized, reward };
      }),
    };
  }
}

/** An additive exploration in progress, as `Explorer`. */
export interface RewardExplorer {
  run(deadline: () => boolean): RewardExploration | null;
  progress(): { discovered: number; expanded: number };
}

/** The additive exploration of `program`, as Lean's `Reward.explore`; `source` is the program
 * whose variables must be bound. */
export function rewardExplorer(
  program: Program,
  source: Program,
  limits: Limits = defaultLimits,
): RewardExplorer {
  const machine = new Machine();
  const normalizer = new Normalizer(machine);
  const sizes = new StateSizes();
  const root = machine.initial(program);
  const states: State[] = [root];
  const index = new Map<number, number>([[root.id, 0]]);
  const rows: { kind: StateKind; edges: RewardEdge[] }[] = [];
  let edgeCount = 0;
  let fuel = limits.maxStates;
  let result: RewardExploration | null = null;
  if (limits.maxStates === 0) {
    result = { kind: "incomplete", limit: "states", discovered: 0, expanded: 0, edges: 0 };
  } else if (!scoped(source)) {
    result = {
      kind: "failed",
      state: 0,
      failure: new Failure("invalid", "source contains an unbound variable"),
    };
  } else if (sizes.exceeds(root, limits.maxStateBytes)) {
    result = { kind: "incomplete", limit: "stateBytes", discovered: 0, expanded: 0, edges: 0 };
  }

  const incomplete = (limit: Limit, discovered: number, edges: number): RewardExploration => ({
    kind: "incomplete",
    limit,
    discovered,
    expanded: rows.length,
    edges,
  });

  function stepOf(state: State): RewardStep | Failure {
    try {
      return normalizer.step(state);
    } catch (error) {
      if (error instanceof Failure) return error;
      throw error;
    }
  }

  /** One step of Lean's `build`, or its result. */
  function build(): RewardExploration | null {
    if (rows.length === states.length) {
      const model = {
        size: states.length,
        kind: rows.map((row) => row.kind),
        edges: rows.map((row) => row.edges),
      };
      if (replayValid(model)) return { kind: "complete", model };
      return {
        kind: "failed",
        state: rows.length,
        failure: new Failure("invalid", "additive graph failed local replay"),
      };
    }
    if (fuel === 0) return incomplete("states", states.length, edgeCount);
    fuel -= 1;
    const i = rows.length;
    const step = stepOf(states[i]);
    if (step instanceof Failure) return { kind: "failed", state: i, failure: step };
    if (step.kind !== "next") {
      if (edgeCount + 1 > limits.maxEdges) return incomplete("edges", states.length, edgeCount);
      const kind: StateKind =
        step.kind === "returned" ? { kind: "returned", reward: step.value } : { kind: "rejected" };
      rows.push({ kind, edges: [{ target: i, probability: one, reward: zero }] });
      edgeCount += 1;
      return null;
    }
    const edges: RewardEdge[] = [];
    for (const outcome of step.successors) {
      if (lt(outcome.probability, zero)) {
        return {
          kind: "failed",
          state: i,
          failure: new Failure("invalid", "negative probability"),
        };
      }
      if (!lt(zero, outcome.probability)) continue;
      if (edgeCount + edges.length + 1 > limits.maxEdges) {
        return incomplete("edges", states.length, edgeCount + edges.length);
      }
      if (sizes.exceeds(outcome.state, limits.maxStateBytes)) {
        return incomplete("stateBytes", states.length, edgeCount + edges.length);
      }
      let target = index.get(outcome.state.id);
      if (target === undefined) {
        if (states.length + 1 > limits.maxStates) {
          return incomplete("states", states.length + 1, edgeCount + edges.length);
        }
        target = states.length;
        index.set(outcome.state.id, target);
        states.push(outcome.state);
      }
      edges.push({ target, probability: outcome.probability, reward: outcome.reward });
    }
    rows.push({ kind: { kind: "transient" }, edges });
    edgeCount += edges.length;
    return null;
  }

  /** As Lean's `Candidate.ReplayValid`, which complete exploration checks: every state is
   * normalized, and each row is the normalized step of its state. */
  function replayValid(model: RewardModel): boolean {
    if (!scoped(source) || states[0] !== root) return false;
    return states.every((state, i) => {
      if (normalizer.state(state)[1] !== state) return false;
      const edges = model.edges[i];
      const kind = model.kind[i];
      if (edges.some((e) => e.target >= model.size || !lt(zero, e.probability))) return false;
      if (!equal(sum(edges.map((e) => e.probability)), one)) return false;
      const step = stepOf(state);
      if (step instanceof Failure) return false;
      if (step.kind === "returned")
        return kind.kind === "returned" && equal(kind.reward, step.value);
      if (step.kind === "rejected") return kind.kind === "rejected";
      if (kind.kind !== "transient" || step.successors.some((o) => lt(o.probability, zero))) {
        return false;
      }
      const taken = step.successors.filter((o) => lt(zero, o.probability));
      return (
        taken.length === edges.length &&
        taken.every((o, k) => {
          const e = edges[k];
          return (
            equal(o.probability, e.probability) &&
            states[e.target] === o.state &&
            equal(o.reward, e.reward)
          );
        })
      );
    });
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

/** The whole additive exploration of `program`, as Lean's `Reward.explore`. */
export function exploreAdditive(
  program: Program,
  source: Program,
  limits: Limits = defaultLimits,
): RewardExploration {
  const result = rewardExplorer(program, source, limits).run(() => false);
  if (!result) throw new Error("an exploration without a deadline ended early");
  return result;
}

/** The model of a reward model's transitions, without its rewards, as Lean's `Model.control`. */
export function control(model: RewardModel): Model {
  return {
    size: model.size,
    kind: model.kind,
    rows: model.edges.map((edges) => {
      const weights = new Map<number, Rational>();
      for (const { target, probability } of edges) {
        weights.set(target, add(weights.get(target) ?? zero, probability));
      }
      return [...weights]
        .sort(([a], [b]) => a - b)
        .map(([target, probability]) => ({ target, probability }));
    }),
  };
}

/** As Lean's `Reward.solve`: the return mass, then the first and second moments, whose equations
 * add each edge's reward to the moments of its target, and the rejection probability; it yields
 * as `eliminate` does. */
export function* rewardStatisticsSteps(
  model: RewardModel,
  maxStates = defaultSolveStates,
): Generator<void, Solved> {
  if (model.size > maxStates)
    return { ok: false, message: stateLimitMessage(model.size, maxStates) };
  const graph = control(model);
  const boundary = analyze(graph);
  if (typeof boundary === "string") return { ok: false, message: boundary };
  const stopped = cut(graph, boundary.dead);
  const paths = findPaths(stopped, boundary.rank);
  if (typeof paths === "string") return { ok: false, message: paths };
  const live = (state: number) => !boundary.dead[state];
  const returnedMass = stopped.kind.map((k) => (k.kind === "returned" ? one : zero));
  const rejected = stopped.kind.map((k, state) =>
    k.kind === "rejected" && live(state) ? one : zero,
  );
  const values = yield* eliminate(stopped, [returnedMass, rejected]);
  if (!values) return { ok: false, message: "singular value equations; no absorption certificate" };
  const [mass, rejection] = values;
  /** The right-hand side of a moment: its terminal value, plus what the edges add. */
  const rhs = (terminal: (value: Rational) => Rational, edge: (e: RewardEdge) => Rational) =>
    model.kind.map((k, state) => {
      if (!live(state)) return zero;
      if (k.kind === "returned") return terminal(k.reward);
      if (k.kind === "rejected") return zero;
      return sum(model.edges[state].map(edge));
    });
  const first = yield* eliminate(stopped, [
    rhs(
      (b) => b,
      (e) => mul(mul(e.probability, e.reward), mass[e.target]),
    ),
  ]);
  if (!first) return { ok: false, message: "singular additive reward equations" };
  const second = yield* eliminate(stopped, [
    rhs(
      (b) => mul(b, b),
      (e) =>
        mul(
          e.probability,
          add(
            mul(mul(integer(2), e.reward), first[0][e.target]),
            mul(mul(e.reward, e.reward), mass[e.target]),
          ),
        ),
    ),
  ]);
  if (!second) return { ok: false, message: "singular additive reward equations" };
  const moments = {
    mass: mass[0],
    first: first[0][0],
    second: second[0][0],
    rejection: rejection[0],
  };
  return { ok: true, result: exactResult(model.size, moments, boundary) };
}

/** `rewardStatisticsSteps`, run to their end. */
export function solveRewardStatistics(model: RewardModel, maxStates = defaultSolveStates): Solved {
  return finish(rewardStatisticsSteps(model, maxStates));
}
