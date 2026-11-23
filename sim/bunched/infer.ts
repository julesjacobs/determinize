// A port of Lean's `Frontend/Bunched.lean`, the experimental inference of `extensions/main.tex`.
// `bunched/build.ts` bundles it in place of `src/core/compiler/infer.ts`, whose `Inference` it
// returns. Only generation differs from `infer.ts`: every node also has its dependencies, the
// sample sites it depends on, and generation records two more kinds of constraint, that a site
// has its requested mode and that the operands of a product are separate (`sep`). The fragment
// has scalar types only, so shapes, decoration and read-back are those of `infer.ts` restricted
// to unit, bool and float.
import type { AffinityConstraint, AffinityTerm } from "../src/core/compiler/affinity.ts";
import { evalTerm, solveAffinities } from "../src/core/compiler/affinity.ts";
import type { Mode, Span } from "../src/core/compiler/ast.ts";
import type { Input } from "../src/core/compiler/core.ts";
import type { Inference } from "../src/core/compiler/infer.ts";
import type { Ty } from "../src/core/compiler/types.ts";
import type { Shape } from "../src/core/compiler/unify.ts";
import { unify } from "../src/core/compiler/unify.ts";

type UType =
  | { tag: "var"; index: number }
  | { tag: "unit" | "bool" }
  | { tag: "float"; affinity: AffinityTerm };
type Node = { tag: "node"; expression: Input; ty: UType; children: Draft[] };
type Draft = Node | { tag: "cast"; body: Node; ty: UType };

/** The sample sites a node depends on, without duplicates, named by their mode variables. */
type Dependencies = string[];

/** A constraint that is not subtyping, with the node it comes from and why. */
interface Recorded {
  lower: AffinityTerm;
  upper: AffinityTerm;
  at: Span;
  message: string;
}

const unit: UType = { tag: "unit" };
const bool: UType = { tag: "bool" };
const general: UType = { tag: "float", affinity: { fixed: "G" } };

class Unsupported extends Error {
  at: Span;
  constructor(at: Span) {
    super("bunched inference does not support this construct yet");
    this.at = at;
  }
}

const union = (...sets: Dependencies[]): Dependencies => [...new Set(sets.flat())];

/** Phase 1, Lean's `Bunched.generate`. One counter numbers type and mode variables. */
function generate(input: Input): { draft: Node; recorded: Recorded[] } {
  let counter = 0;
  const recorded: Recorded[] = [];
  const fresh = (): UType => ({ tag: "var", index: counter++ });
  const freshFloat = (): UType => ({ tag: "float", affinity: { var: `g${counter++}` } });
  const cast = (body: Node, ty: UType): Draft => ({ tag: "cast", body, ty });
  /** A sample site: a fresh mode variable, which identifies the site in dependency sets, set
   * equal to the requested mode if there is one. */
  const site = (requested: Mode | null, at: Span): [UType, string] => {
    const a = `g${counter++}`;
    if (requested) {
      const message = `this site is [${requested}], but separation requires it to be G`;
      recorded.push({ lower: { var: a }, upper: { fixed: requested }, at, message });
      recorded.push({ lower: { fixed: requested }, upper: { var: a }, at, message });
    }
    return [{ tag: "float", affinity: { var: a } }, a];
  };

  const go = (env: [UType, Dependencies][], e: Input): [Node, Dependencies] => {
    const node = (ty: UType, children: Draft[]): Node => ({
      tag: "node",
      expression: e,
      ty,
      children,
    });
    switch (e.kind) {
      case "bvar": {
        const [t, deps] = env[e.index];
        return [node(t, []), deps];
      }
      case "unit":
        return [node(unit, []), []];
      case "bool":
        return [node(bool, []), []];
      case "real":
        return [node(freshFloat(), []), []];
      case "ite": {
        const t = fresh();
        const [c, d0] = go(env, e.a);
        const [a, d1] = go(env, e.b);
        const [b, d2] = go(env, e.c);
        return [node(t, [cast(c, bool), cast(a, t), cast(b, t)]), union(d0, d1, d2)];
      }
      case "letE": {
        const [v, d1] = go(env, e.a);
        const [b, d2] = go([[v.ty, d1], ...env], e.b);
        return [node(b.ty, [v, b]), d2];
      }
      case "add":
      case "mul":
      case "lt": {
        const t = freshFloat();
        const operand = e.kind === "lt" ? general : t;
        const [a, d1] = go(env, e.a);
        const [b, d2] = go(env, e.b);
        // sep(B₁, B₂): every site both factors depend on is G.
        if (e.kind === "mul") {
          const message = "the factors share a sample site, which must then be G";
          for (const s of d1.filter((s) => d2.includes(s))) {
            recorded.push({ lower: { var: s }, upper: { fixed: "G" }, at: e, message });
          }
        }
        const ty = e.kind === "lt" ? bool : t;
        return [node(ty, [cast(a, operand), cast(b, operand)]), union(d1, d2)];
      }
      case "uniform":
      case "gaussian": {
        const [t, s] = site(e.site, e);
        const tb = e.kind === "uniform" ? t : general;
        const [a, d1] = go(env, e.a);
        const [b, d2] = go(env, e.b);
        return [node(t, [cast(a, t), cast(b, tb)]), union(d1, d2, [s])];
      }
      // Only as `flip`, which elaborates to `0 < bernoulli[G](p)`: with `lt`, the paper's Flip-G.
      case "bernoulli": {
        const [t, s] = site(e.site, e);
        const [a, d] = go(env, e.a);
        return [node(t, [cast(a, t)]), union(d, [s])];
      }
      default:
        throw new Unsupported(e);
    }
  };
  return { draft: go([], input)[0], recorded };
}

interface Relation {
  lower: UType;
  upper: UType;
  at: Span;
}

function relations(draft: Draft, out: Relation[] = []): Relation[] {
  if (draft.tag === "cast") {
    out.push({ lower: draft.body.ty, upper: draft.ty, at: draft.body.expression });
    relations(draft.body, out);
  } else {
    for (const child of draft.children) relations(child, out);
  }
  return out;
}

const shape = (t: UType): Shape => (t.tag === "var" ? t : { tag: t.tag });

/** `infer.ts`'s `decorate`: a type variable becomes its shape, a float with the mode variable of
 * the type variable. */
function decorate(theta: (index: number) => Shape, t: UType): UType {
  if (t.tag !== "var") return t;
  const s = theta(t.index);
  if (s.tag === "float") return { tag: "float", affinity: { var: `l${t.index}:` } };
  return s.tag === "unit" || s.tag === "bool" ? { tag: s.tag } : t;
}

function instantiate(t: UType, rho: (v: string) => Mode): Ty {
  if (t.tag === "float") return { tag: "float", mode: evalTerm(t.affinity, rho) };
  return t.tag === "bool" ? { tag: "bool" } : { tag: "unit" };
}

/** Lean's `Bunched.infer`: the modes of the sites and the types of the nodes, or why none exist. */
export function infer(input: Input): Inference {
  let generated: ReturnType<typeof generate>;
  try {
    generated = generate(input);
  } catch (error) {
    if (!(error instanceof Unsupported)) throw error;
    return { ok: false, failure: { kind: "shapes", message: error.message, at: error.at } };
  }
  const { draft, recorded } = generated;
  const constraints = relations(draft);
  const unification = unify(
    constraints.map((r, origin) => ({ left: shape(r.lower), right: shape(r.upper), origin })),
  );
  if (!unification.ok) {
    const message = "type mismatch";
    const at = constraints[unification.origin].at;
    return { ok: false, failure: { kind: "shapes", message, at } };
  }
  const theta = unification.unifier;
  // Subtyping constraints keep their relation's index as origin; recorded ones follow them.
  const atomic: AffinityConstraint[] = [];
  for (const [origin, r] of constraints.entries()) {
    const lower = decorate(theta, r.lower);
    const upper = decorate(theta, r.upper);
    if (lower.tag === "float" && upper.tag === "float") {
      atomic.push({ lower: lower.affinity, upper: upper.affinity, origin });
    }
  }
  for (const [i, c] of recorded.entries()) {
    atomic.push({ lower: c.lower, upper: c.upper, origin: constraints.length + i });
  }
  const solution = solveAffinities(atomic);
  if (!solution.ok) {
    const { origin } = solution.violated;
    const failure =
      origin < constraints.length
        ? {
            message: "mode mismatch: an [E] value is used where a G value is required",
            at: constraints[origin].at,
          }
        : recorded[origin - constraints.length];
    return { ok: false, failure: { kind: "modes", message: failure.message, at: failure.at } };
  }
  const { rho } = solution;
  const modes = new Map<Input, Mode>();
  const types = new Map<Input, Ty>();
  const readBack = (d: Draft): void => {
    const n = d.tag === "cast" ? d.body : d;
    const ty = instantiate(decorate(theta, n.ty), rho);
    types.set(n.expression, ty);
    if ("site" in n.expression) modes.set(n.expression, ty.tag === "float" ? ty.mode : "G");
    for (const child of n.children) readBack(child);
  };
  readBack(draft);
  return { ok: true, modes, types, type: instantiate(decorate(theta, draft.ty), rho) };
}
