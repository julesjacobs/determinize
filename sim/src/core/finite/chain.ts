// The Markov chain of a finite model with its probability-1 steps contracted: a step is a
// transient state with a single successor, and every path of steps is followed to the state where
// it ends, which branches, returns or rejects; a path that comes back to a state it has passed
// diverges. The nodes are those states, and an edge leads from a branching state's successor to
// the end of its path, with the rewards along it summed in the additive mode. Contraction keeps the
// joint law of a run's outcome and its summed reward, so every exact statistic of the chain equals
// the model's. It takes time linear in the model's size.
import type { Model } from "./model.ts";
import type { Rational } from "./rational.ts";
import { add, fraction, zero } from "./rational.ts";
import type { RewardModel } from "./reward.ts";

/** A node of a contracted chain, with the model's state where its paths end; a diverging node's
 * state is one on its cycle. */
export type ChainNode = { state: number } & (
  | { kind: "branch" }
  | { kind: "returned"; value: Rational }
  | { kind: "rejected" }
  | { kind: "diverges" }
);

/** An edge of a contracted chain, between nodes, with the reward its path adds. */
export interface ChainEdge {
  from: number;
  to: number;
  probability: Rational;
  reward: Rational;
}

/** A contracted chain, whose initial node is node 0. */
export interface Chain {
  /** The number of the model's states. */
  states: number;
  /** The reward added on the way from the initial state to node 0. */
  entry: Rational;
  nodes: ChainNode[];
  /** The edges, each node's in the order of its state's edges, the nodes in order. */
  edges: ChainEdge[];
}

/** Where a path of steps ends, with the reward added on the way: at a state, or on the cycle of
 * a diverging path, which `state` is on. A diverging path adds nothing, as its run never ends. */
interface End {
  state: number;
  diverges: boolean;
  reward: Rational;
}

/** The chain of `model`, whose initial state is 0, with its steps contracted. */
export function contract(model: Model | RewardModel): Chain {
  const { size, kind } = model;
  const edgesOf = (state: number): { target: number; probability: Rational; reward: Rational }[] =>
    "edges" in model
      ? model.edges[state]
      : model.rows[state].map(({ target, probability }) => ({ target, probability, reward: zero }));
  const degree = (state: number) =>
    "edges" in model ? model.edges[state].length : model.rows[state].length;
  const isStep = (state: number) => kind[state].kind === "transient" && degree(state) === 1;

  const ends: (End | undefined)[] = new Array(size);
  const onPath: boolean[] = new Array(size).fill(false);

  /** Where the path of steps from `start` ends; each step's end is computed once. */
  function follow(start: number): End {
    const path: number[] = [];
    let state = start;
    while (isStep(state) && ends[state] === undefined && !onPath[state]) {
      onPath[state] = true;
      path.push(state);
      state = edgesOf(state)[0].target;
    }
    let end: End =
      ends[state] ??
      (isStep(state)
        ? { state, diverges: true, reward: zero }
        : { state, diverges: false, reward: zero });
    for (let i = path.length - 1; i >= 0; i--) {
      const step = path[i];
      if (!end.diverges) end = { ...end, reward: add(edgesOf(step)[0].reward, end.reward) };
      ends[step] = end;
      onPath[step] = false;
    }
    return end;
  }

  const nodes: ChainNode[] = [];
  const edges: ChainEdge[] = [];
  const numbers = new Map<string, number>();
  const pending: number[] = [];
  /** The node where `end` is, numbered when found first. */
  function nodeOf(end: End): number {
    const key = `${end.diverges ? "d" : "s"}${end.state}`;
    let number = numbers.get(key);
    if (number === undefined) {
      number = nodes.length;
      numbers.set(key, number);
      const at = kind[end.state];
      nodes.push(
        end.diverges
          ? { state: end.state, kind: "diverges" }
          : at.kind === "returned"
            ? { state: end.state, kind: "returned", value: at.reward }
            : { state: end.state, kind: at.kind === "rejected" ? "rejected" : "branch" },
      );
      pending.push(number);
    }
    return number;
  }

  const first = follow(0);
  nodeOf(first);
  for (let next = pending.shift(); next !== undefined; next = pending.shift()) {
    const node = nodes[next];
    if (node.kind !== "branch") continue;
    // Parallel edges with equal rewards are one edge, as a model's row has one entry per target.
    const out = new Map<string, ChainEdge>();
    for (const { target, probability, reward } of edgesOf(node.state)) {
      const end = follow(target);
      const to = nodeOf(end);
      const total = end.diverges ? zero : add(reward, end.reward);
      const key = `${to} ${fraction(total)}`;
      const same = out.get(key);
      if (same) same.probability = add(same.probability, probability);
      else out.set(key, { from: next, to, probability, reward: total });
    }
    edges.push(...out.values());
  }
  return { states: size, entry: first.reward, nodes, edges };
}
