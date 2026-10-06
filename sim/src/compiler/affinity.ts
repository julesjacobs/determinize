// A port of Lean's `Frontend/Affinity.lean`: the greatest solution of atomic constraints
// `lower ≤ upper` between modes, in the order G ≤ E.
import type { Mode } from "./ast.ts";

/** One side of a constraint: a mode variable or a mode the program fixes. */
export type AffinityTerm = { var: string } | { fixed: Mode };

export interface AffinityConstraint {
  lower: AffinityTerm;
  upper: AffinityTerm;
  /** The index of the subtyping constraint this one comes from. */
  origin: number;
}

export function evalTerm(term: AffinityTerm, rho: (v: string) => Mode): Mode {
  return "var" in term ? rho(term.var) : term.fixed;
}

/**
 * The variables that every solution sets to G: those below a G, directly or through other such
 * variables. This is the closure that Lean's `forcedGeneral` computes, found by a search
 * backwards from the constraints whose upper side is G.
 */
export function forcedGeneral(constraints: AffinityConstraint[]): Set<string> {
  const general = new Set<string>();
  const below = new Map<string, string[]>();
  const pending: string[] = [];
  for (const { lower, upper } of constraints) {
    if (!("var" in lower)) continue;
    if ("var" in upper) {
      const list = below.get(upper.var) ?? [];
      list.push(lower.var);
      below.set(upper.var, list);
    } else if (upper.fixed === "G") {
      pending.push(lower.var);
    }
  }
  for (let v = pending.pop(); v !== undefined; v = pending.pop()) {
    if (general.has(v)) continue;
    general.add(v);
    pending.push(...(below.get(v) ?? []));
  }
  return general;
}

/** The greatest solution, G on the forced variables and E elsewhere, or the first constraint it
 * violates; then no solution exists. */
export function solveAffinities(
  constraints: AffinityConstraint[],
): { ok: true; rho: (v: string) => Mode } | { ok: false; violated: AffinityConstraint } {
  const general = forcedGeneral(constraints);
  const rho = (v: string): Mode => (general.has(v) ? "G" : "E");
  const violated = constraints.find(
    (c) => evalTerm(c.lower, rho) === "E" && evalTerm(c.upper, rho) === "G",
  );
  return violated ? { ok: false, violated } : { ok: true, rho };
}
