// Programs after elaboration: Lean's `Expr Rat Site` (`Spec/Syntax.lean`), with de Bruijn
// indices. `Input` is Lean's `Input`, whose sites carry the requested mode or null.
import type { Expr, Mode, Span } from "./ast.ts";
import type { Rational } from "./rational.ts";

export type UnaryCore = "lam" | "fix" | "fst" | "snd" | "inl" | "inr" | "neg";
export type BinaryCore = "app" | "pair" | "cons" | "letE" | "add" | "mul" | "div" | "lt";
export type TernaryCore = "matchSum" | "matchList" | "ite";
export type UnarySite = "poisson" | "discrete" | "bernoulli" | "exponential";
export type BinarySite = "uniform" | "gaussian" | "beta" | "gamma";

/**
 * A node of a program. `source` is the parsed node that the node elaborates; nodes that
 * desugaring introduces have none, and take the span of the source they stand for.
 */
export type Core<S> = Span & { source: Expr | null } & (
    | { kind: "bvar"; index: number }
    | { kind: "reject" | "unit" | "nil" }
    | { kind: "bool"; value: boolean }
    | { kind: "real"; value: Rational }
    | { kind: UnaryCore; a: Core<S> }
    | { kind: BinaryCore; a: Core<S>; b: Core<S> }
    | { kind: TernaryCore; a: Core<S>; b: Core<S>; c: Core<S> }
    | { kind: UnarySite; site: S; a: Core<S> }
    | { kind: BinarySite; site: S; a: Core<S>; b: Core<S> }
  );

export type Input = Core<Mode | null>;
export type Annotated = Core<Mode>;

/** The children of a node, in syntax order. */
export function children<S>(e: Core<S>): Core<S>[] {
  if ("c" in e) return [e.a, e.b, e.c];
  if ("b" in e) return [e.a, e.b];
  if ("a" in e) return [e.a];
  return [];
}

/** The sample sites of a program in syntax order, as Lean's `Expr.sites`. */
export function sites<S>(e: Core<S>, out: Core<S>[] = []): Core<S>[] {
  if ("site" in e) out.push(e);
  for (const child of children(e)) sites(child, out);
  return out;
}
