// Ports of the solvers behind Lean's exact statistics: `Proof/FiniteModel/Boundary.lean`'s
// `analyze`, which finds the states that can't reach a terminal state, `Paths.lean`'s
// `findPaths`, and `Proof/LinearAlgebra/Solve.lean`'s Gaussian elimination over the rationals,
// which `Finite/Solve.lean` runs on a model's value equations.
import type { Model } from "./model.ts";
import type { Rational } from "./rational.ts";
import { div, isZero, mul, one, sub, zero } from "./rational.ts";

/** Lean's `SolveLimits`. */
export const defaultSolveStates = 256;

/** Lean's message when a model has more states than the solver takes. */
export function stateLimitMessage(size: number, maxStates: number) {
  return `exact solver state limit exceeded (${size} > ${maxStates})`;
}

/** The states that can't reach a terminal state (`dead`), and when each other state can, as
 * Lean's `Boundary`. */
export interface Boundary {
  dead: boolean[];
  rank: number[];
}

/** As Lean's `analyze`: reachability of the terminal states, one more step a round. */
export function analyze(model: Model): Boundary | string {
  const { size, kind, rows } = model;
  let reachable: boolean[] = new Array(size).fill(false);
  let rank: number[] = new Array(size).fill(0);
  for (let fuel = size + 2, level = 0; fuel > 0; fuel--, level++) {
    const now = reachable;
    const next = kind.map(
      (k, state) => k.kind !== "transient" || rows[state].some(({ target }) => now[target]),
    );
    if (next.every((value, state) => value === now[state])) {
      return { dead: now.map((value) => !value), rank };
    }
    const before = rank;
    rank = before.map((r, state) => (now[state] ? r : next[state] ? level : 0));
    reachable = next;
  }
  return "terminal reachability did not stabilize";
}

/** A model whose dead states reject, as Lean's `cut`. */
export function cut(model: Model, dead: boolean[]): Model {
  return {
    ...model,
    kind: model.kind.map((k, state) => (dead[state] ? { kind: "rejected" } : k)),
  };
}

/** As Lean's `findPaths`: each transient state's first successor of lower rank. The paths are
 * the certificate's; the solvers only need to know that they exist. */
export function findPaths(model: Model, rank: number[]): number[] | string {
  const next: number[] = [];
  for (let state = 0; state < model.size; state++) {
    if (model.kind[state].kind !== "transient") {
      next.push(state);
      continue;
    }
    const step = model.rows[state].find(({ target }) => rank[target] < rank[state]);
    if (!step) return "no descending path to a terminal boundary";
    next.push(step.target);
  }
  return next;
}

/** The result of `steps`, run to their end. */
export function finish<T>(steps: Generator<void, T>): T {
  for (;;) {
    const next = steps.next();
    if (next.done) return next.value;
  }
}

/**
 * The solutions of A x = b for each of `rhs`, where A is the matrix of a model's value equations:
 * row `state` is `x[state] − Σ p · x[target]` for a transient state and `x[state]` for a terminal
 * one, as `solveValues` builds it. Null if A is singular. Elimination takes as its pivot the first
 * row with a non-zero entry, as Lean's `solve` does; the solution doesn't depend on that choice.
 * It yields after each row it reduces or solves, so that a dense model's elimination can stop at
 * a deadline and go on later.
 */
export function* eliminate(model: Model, rhs: Rational[][]): Generator<void, Rational[][] | null> {
  const n = model.size;
  const rows: Map<number, Rational>[] = model.kind.map((k, state) => {
    const row = new Map<number, Rational>([[state, one]]);
    if (k.kind !== "transient") return row;
    for (const { target, probability } of model.rows[state]) {
      const entry = sub(row.get(target) ?? zero, probability);
      if (isZero(entry)) row.delete(target);
      else row.set(target, entry);
    }
    return row;
  });
  const b = rows.map((_, state) => rhs.map((vector) => vector[state]));
  for (let column = 0; column < n; column++) {
    let pivot = column;
    while (pivot < n && !rows[pivot].has(column)) pivot++;
    if (pivot === n) return null;
    [rows[column], rows[pivot]] = [rows[pivot], rows[column]];
    [b[column], b[pivot]] = [b[pivot], b[column]];
    const top = rows[column];
    const lead = top.get(column) as Rational;
    for (let row = column + 1; row < n; row++) {
      const entry = rows[row].get(column);
      if (!entry) continue;
      const factor = div(entry, lead);
      for (const [at, value] of top) {
        const updated = sub(rows[row].get(at) ?? zero, mul(factor, value));
        if (isZero(updated)) rows[row].delete(at);
        else rows[row].set(at, updated);
      }
      b[row] = b[row].map((value, k) => sub(value, mul(factor, b[column][k])));
      yield;
    }
  }
  const x: Rational[][] = new Array(n);
  for (let row = n - 1; row >= 0; row--) {
    const values = [...b[row]];
    let lead = one;
    for (const [at, value] of rows[row]) {
      if (at === row) lead = value;
      else for (let k = 0; k < values.length; k++) values[k] = sub(values[k], mul(value, x[at][k]));
    }
    x[row] = values.map((value) => div(value, lead));
    yield;
  }
  return rhs.map((_, k) => x.map((values) => values[k]));
}

/** `eliminate`, run to its end. */
export function solveEquations(model: Model, rhs: Rational[][]): Rational[][] | null {
  return finish(eliminate(model, rhs));
}
