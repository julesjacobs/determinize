// A port of Lean's `Frontend/Unify.lean`: Robinson's unification of type shapes with an occurs
// check. Bindings are resolved when an equation is inspected rather than substituted into every
// remaining equation, which yields the same most general unifier.

/** A type without its modes. */
export type Shape =
  | { tag: "var"; index: number }
  | { tag: "unit" | "bool" | "float" }
  | { tag: "prod" | "sum" | "arr"; a: Shape; b: Shape }
  | { tag: "list"; a: Shape };

/** An equation between shapes, with the index of the constraint it comes from. */
export interface Equation {
  left: Shape;
  right: Shape;
  origin: number;
}

export type Unification =
  | { ok: true; unifier: (index: number) => Shape }
  | { ok: false; origin: number; reason: "mismatch" | "infinite"; resolve: (s: Shape) => Shape };

/** For two shapes that are not variables, the equations between their children, or null if
 * their head constructors differ. */
function childEquations(s: Shape, t: Shape, origin: number): Equation[] | null {
  if (s.tag !== t.tag || s.tag === "var") return null;
  if ("b" in s && "b" in t) {
    return [
      { left: s.a, right: t.a, origin },
      { left: s.b, right: t.b, origin },
    ];
  }
  if ("a" in s && "a" in t) return [{ left: s.a, right: t.a, origin }];
  return [];
}

export function unify(equations: Equation[]): Unification {
  const bindings = new Map<number, Shape>();
  const head = (s: Shape): Shape => {
    let current = s;
    while (current.tag === "var") {
      const bound = bindings.get(current.index);
      if (!bound) break;
      current = bound;
    }
    return current;
  };
  const resolved = new Map<number, Shape>();
  /** The shape with every bound variable replaced, as Lean's eager substitution leaves it. */
  const resolve = (s: Shape): Shape => {
    const h = head(s);
    if (h.tag === "var") {
      return h;
    }
    if ("b" in h) return { tag: h.tag, a: resolve(h.a), b: resolve(h.b) };
    if ("a" in h) return { tag: h.tag, a: resolve(h.a) };
    return h;
  };
  const occurs = (index: number, s: Shape): boolean => {
    const h = head(s);
    if (h.tag === "var") return h.index === index;
    if ("b" in h) return occurs(index, h.a) || occurs(index, h.b);
    if ("a" in h) return occurs(index, h.a);
    return false;
  };

  // Lean's `unify` handles the first equation and continues with the rest, putting the
  // equations between children first.
  const stack = equations.toReversed();
  for (let equation = stack.pop(); equation; equation = stack.pop()) {
    const s = head(equation.left);
    const t = head(equation.right);
    const variable = s.tag === "var" ? s : t.tag === "var" ? t : null;
    if (variable) {
      const other = variable === s ? t : s;
      if (other.tag === "var" && other.index === variable.index) continue;
      if (occurs(variable.index, other)) {
        return { ok: false, origin: equation.origin, reason: "infinite", resolve };
      }
      bindings.set(variable.index, other);
      continue;
    }
    const children = childEquations(s, t, equation.origin);
    if (!children) return { ok: false, origin: equation.origin, reason: "mismatch", resolve };
    stack.push(...children.toReversed());
  }
  return {
    ok: true,
    unifier: (index) => {
      let shape = resolved.get(index);
      if (!shape) {
        shape = resolve({ tag: "var", index });
        resolved.set(index, shape);
      }
      return shape;
    },
  };
}
