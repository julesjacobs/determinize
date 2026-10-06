// A port of Lean's `Frontend/Infer.lean`. `infer` fills the omitted modes of an elaborated
// program with the greatest ones that make it typable, in Lean's phases: generation of a draft
// with a cast wherever typing uses subsumption, unification of the shapes of every cast, decoration
// of the type variables, decomposition into atomic constraints, the greatest solution of those, and
// read-back. It fails on exactly the programs that Lean's `infer` rejects; the failure carries
// the span of a cast that cannot hold.
import type { AffinityConstraint, AffinityTerm } from "./affinity.ts";
import { evalTerm, solveAffinities } from "./affinity.ts";
import type { Mode, Span } from "./ast.ts";
import type { Input } from "./core.ts";
import type { Ty } from "./types.ts";
import type { Shape } from "./unify.ts";
import { unify } from "./unify.ts";

/** Types during inference, over type variables and mode terms. */
type UType =
  | { tag: "var"; index: number }
  | { tag: "unit" | "bool" }
  | { tag: "float"; affinity: AffinityTerm }
  | { tag: "prod" | "sum" | "arr"; a: UType; b: UType }
  | { tag: "list"; a: UType };

/** The program with a type for every node, and a cast wherever its typing uses subsumption. */
type Draft =
  | { tag: "node"; expression: Input; ty: UType; children: Draft[] }
  | { tag: "cast"; body: Draft & { tag: "node" }; ty: UType };

/** A subtyping constraint: the type of a cast's body is a subtype of the cast's type. */
interface Relation {
  lower: UType;
  upper: UType;
  at: Span;
}

/** Why inference failed: Lean's `incompatible or infinite type shapes` (`shapes`) or
 * `inconsistent E/G constraints` (`modes`). */
export interface InferenceFailure {
  kind: "shapes" | "modes";
  message: string;
  at: Span;
}

export type Inference =
  | {
      ok: true;
      /** The mode of every sample site of the input. */
      modes: Map<Input, Mode>;
      /** The type of every node of the input. */
      types: Map<Input, Ty>;
      type: Ty;
    }
  | { ok: false; failure: InferenceFailure };

const unit: UType = { tag: "unit" };
const bool: UType = { tag: "bool" };
/** The type of an operand that the typing rules require to be G. */
const general: UType = { tag: "float", affinity: { fixed: "G" } };

/** Phase 1, Lean's `generate`. One counter numbers type and mode variables. */
function generate(input: Input): Draft & { tag: "node" } {
  let counter = 0;
  const fresh = (): UType => ({ tag: "var", index: counter++ });
  const freshFloat = (): UType => ({ tag: "float", affinity: { var: `g${counter++}` } });
  const site = (requested: Mode | null): UType =>
    requested ? { tag: "float", affinity: { fixed: requested } } : freshFloat();
  const cast = (body: Draft & { tag: "node" }, ty: UType): Draft => ({ tag: "cast", body, ty });

  const go = (env: UType[], e: Input): Draft & { tag: "node" } => {
    const node = (ty: UType, children: Draft[]) =>
      ({ tag: "node", expression: e, ty, children }) as const;
    switch (e.kind) {
      case "bvar":
        return node(env[e.index], []);
      case "reject":
        return node(fresh(), []);
      case "unit":
        return node(unit, []);
      case "bool":
        return node(bool, []);
      case "real":
        return node(freshFloat(), []);
      case "lam": {
        const a = fresh();
        const body = go([a, ...env], e.a);
        return node({ tag: "arr", a, b: body.ty }, [body]);
      }
      case "fix": {
        const a = fresh();
        const r = fresh();
        const body = go([a, { tag: "arr", a, b: r }, ...env], e.a);
        return node({ tag: "arr", a, b: r }, [cast(body, r)]);
      }
      case "app": {
        const f = go(env, e.a);
        const x = go(env, e.b);
        const a = fresh();
        const r = fresh();
        return node(r, [cast(f, { tag: "arr", a, b: r }), cast(x, a)]);
      }
      case "pair": {
        const a = go(env, e.a);
        const b = go(env, e.b);
        return node({ tag: "prod", a: a.ty, b: b.ty }, [a, b]);
      }
      case "fst":
      case "snd": {
        const a = fresh();
        const b = fresh();
        const p = go(env, e.a);
        return node(e.kind === "fst" ? a : b, [cast(p, { tag: "prod", a, b })]);
      }
      case "inl":
      case "inr": {
        const v = go(env, e.a);
        const other = fresh();
        const ty: UType =
          e.kind === "inl" ? { tag: "sum", a: v.ty, b: other } : { tag: "sum", a: other, b: v.ty };
        return node(ty, [v]);
      }
      case "matchSum": {
        const l = fresh();
        const r = fresh();
        const t = fresh();
        const s = go(env, e.a);
        const a = go([l, ...env], e.b);
        const b = go([r, ...env], e.c);
        return node(t, [cast(s, { tag: "sum", a: l, b: r }), cast(a, t), cast(b, t)]);
      }
      case "nil":
        return node({ tag: "list", a: fresh() }, []);
      case "cons": {
        const a = fresh();
        const h = go(env, e.a);
        const t = go(env, e.b);
        return node({ tag: "list", a }, [cast(h, a), cast(t, { tag: "list", a })]);
      }
      case "matchList": {
        const a = fresh();
        const t = fresh();
        const s = go(env, e.a);
        const n = go(env, e.b);
        const c = go([a, { tag: "list", a }, ...env], e.c);
        return node(t, [cast(s, { tag: "list", a }), cast(n, t), cast(c, t)]);
      }
      case "ite": {
        const t = fresh();
        const c = go(env, e.a);
        const a = go(env, e.b);
        const b = go(env, e.c);
        return node(t, [cast(c, bool), cast(a, t), cast(b, t)]);
      }
      case "letE": {
        const v = go(env, e.a);
        const b = go([v.ty, ...env], e.b);
        return node(b.ty, [v, b]);
      }
      case "neg": {
        const t = freshFloat();
        const b = go(env, e.a);
        return node(t, [cast(b, t)]);
      }
      case "add":
      case "mul":
      case "div":
      case "lt": {
        const t = freshFloat();
        const ta = e.kind === "mul" || e.kind === "lt" ? general : t;
        const tb = e.kind === "div" || e.kind === "lt" ? general : t;
        const a = go(env, e.a);
        const b = go(env, e.b);
        return node(e.kind === "lt" ? bool : t, [cast(a, ta), cast(b, tb)]);
      }
      case "uniform":
      case "gaussian":
      case "beta":
      case "gamma": {
        const t = site(e.site);
        const ta = e.kind === "beta" ? general : t;
        const tb = e.kind === "uniform" ? t : general;
        const a = go(env, e.a);
        const b = go(env, e.b);
        return node(t, [cast(a, ta), cast(b, tb)]);
      }
      case "discrete": {
        const t = site(e.site);
        const probabilities = go(env, e.a);
        return node(t, [cast(probabilities, { tag: "list", a: t })]);
      }
      case "poisson":
      case "bernoulli":
      case "exponential": {
        const t = site(e.site);
        const ta = e.kind === "exponential" ? general : t;
        const a = go(env, e.a);
        return node(t, [cast(a, ta)]);
      }
    }
  };
  return go([], input);
}

/** Lean's `Draft.relations`: every cast's constraint before those inside its body. */
function relations(draft: Draft, out: Relation[] = []): Relation[] {
  if (draft.tag === "cast") {
    out.push({ lower: draft.body.ty, upper: draft.ty, at: draft.body.expression });
    relations(draft.body, out);
  } else {
    for (const child of draft.children) relations(child, out);
  }
  return out;
}

function shape(t: UType): Shape {
  switch (t.tag) {
    case "var":
      return t;
    case "unit":
    case "bool":
      return { tag: t.tag };
    case "float":
      return { tag: "float" };
    case "list":
      return { tag: "list", a: shape(t.a) };
    default:
      return { tag: t.tag, a: shape(t.a), b: shape(t.b) };
  }
}

/** The type of a shape, with the mode variable of type variable `alpha` at each float, named by
 * its position: the child indices from the root (Lean's `Shape.decorate` with `leaf alpha`). */
function decorateShape(s: Shape, alpha: number, position: string): UType {
  switch (s.tag) {
    case "var":
      return s;
    case "unit":
    case "bool":
      return { tag: s.tag };
    case "float":
      return { tag: "float", affinity: { var: `l${alpha}:${position}` } };
    case "list":
      return { tag: "list", a: decorateShape(s.a, alpha, `${position}0`) };
    default:
      return {
        tag: s.tag,
        a: decorateShape(s.a, alpha, `${position}0`),
        b: decorateShape(s.b, alpha, `${position}1`),
      };
  }
}

/** Lean's `UType.decorate`: every type variable replaced by its decorated shape. */
function decorate(theta: (index: number) => Shape, t: UType): UType {
  switch (t.tag) {
    case "var":
      return decorateShape(theta(t.index), t.index, "");
    case "unit":
    case "bool":
    case "float":
      return t;
    case "list":
      return { tag: "list", a: decorate(theta, t.a) };
    default:
      return { tag: t.tag, a: decorate(theta, t.a), b: decorate(theta, t.b) };
  }
}

/** Lean's `decompose`: the atomic constraints under which `s` is a subtype of `t`, two types of
 * the same shape. Function arguments are contravariant. */
function decompose(s: UType, t: UType, origin: number, out: AffinityConstraint[]): void {
  if (s.tag === "float" && t.tag === "float") {
    out.push({ lower: s.affinity, upper: t.affinity, origin });
  } else if (s.tag === "arr" && t.tag === "arr") {
    decompose(t.a, s.a, origin, out);
    decompose(s.b, t.b, origin, out);
  } else if ((s.tag === "prod" || s.tag === "sum") && s.tag === t.tag) {
    decompose(s.a, t.a, origin, out);
    decompose(s.b, t.b, origin, out);
  } else if (s.tag === "list" && t.tag === "list") {
    decompose(s.a, t.a, origin, out);
  }
}

/** A type in the solution; the remaining type variables are unconstrained and become unit. */
function instantiate(t: UType, rho: (v: string) => Mode): Ty {
  switch (t.tag) {
    case "var":
    case "unit":
      return { tag: "unit" };
    case "bool":
      return { tag: "bool" };
    case "float":
      return { tag: "float", mode: evalTerm(t.affinity, rho) };
    case "list":
      return { tag: "list", a: instantiate(t.a, rho) };
    default:
      return { tag: t.tag, a: instantiate(t.a, rho), b: instantiate(t.b, rho) };
  }
}

/** A shape for a message. */
function describe(s: Shape, prec = 0): string {
  const wrap = (text: string, level: number) => (prec > level ? `(${text})` : text);
  switch (s.tag) {
    case "var":
      return "unknown";
    case "unit":
    case "bool":
    case "float":
      return s.tag;
    case "list":
      return `[${describe(s.a)}]`;
    case "prod":
      return wrap(`${describe(s.a, 3)} * ${describe(s.b, 3)}`, 2);
    case "sum":
      return wrap(`${describe(s.a, 3)} + ${describe(s.b, 3)}`, 2);
    case "arr":
      return wrap(`${describe(s.a, 1)} -> ${describe(s.b, 0)}`, 0);
  }
}

/** Lean's `infer`: the modes of the sites and the types of the nodes, or why none exist. */
export function infer(input: Input): Inference {
  const draft = generate(input);
  const constraints = relations(draft);
  const unification = unify(
    constraints.map((r, origin) => ({ left: shape(r.lower), right: shape(r.upper), origin })),
  );
  if (!unification.ok) {
    const relation = constraints[unification.origin];
    const found = describe(unification.resolve(shape(relation.lower)));
    const expected = describe(unification.resolve(shape(relation.upper)));
    const message =
      unification.reason === "infinite"
        ? "infinite type: the type of this expression would have to contain itself"
        : `type mismatch: expected ${expected}, found ${found}`;
    return { ok: false, failure: { kind: "shapes", message, at: relation.at } };
  }
  const theta = unification.unifier;
  const atomic: AffinityConstraint[] = [];
  for (const [origin, r] of constraints.entries()) {
    decompose(decorate(theta, r.lower), decorate(theta, r.upper), origin, atomic);
  }
  const solution = solveAffinities(atomic);
  if (!solution.ok) {
    const message = "mode mismatch: an [E] value is used where a G value is required";
    return {
      ok: false,
      failure: { kind: "modes", message, at: constraints[solution.violated.origin].at },
    };
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
