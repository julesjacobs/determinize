// The finite models of Lean's `Spec/FiniteModel/Model.lean`, as exploration builds them.
import type { Rational } from "./rational.ts";

/** What happens at a state, as Lean's `StateKind`. */
export type StateKind =
  | { kind: "transient" }
  | { kind: "returned"; reward: Rational }
  | { kind: "rejected" };

/** A finite Markov chain with rational transitions, as Lean's `Model`, whose initial state is 0.
 * Each row lists the targets with positive probability, in increasing order. */
export interface Model {
  size: number;
  kind: StateKind[];
  rows: { target: number; probability: Rational }[][];
}
