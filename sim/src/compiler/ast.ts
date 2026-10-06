import type { Affine } from "../runtime/affine.ts";
import type { Rational } from "./rational.ts";
import type { Type } from "./types.ts";

export type Mode = "E" | "G";

/** Distributions whose parameters are expressions. */
export type ParamDistributionKind =
  | "Uniform"
  | "Gauss"
  | "Exponential"
  | "Gamma"
  | "Beta"
  | "Flip"
  | "Bernoulli"
  | "Poisson";
/** `Discrete` has literal choices; `DiscreteList` draws an index from a list of probabilities
 * whose last outcome takes the remainder, as Lean's `Expr.discrete` does. */
export type DistributionKind = ParamDistributionKind | "Discrete" | "DiscreteList";
/** The distributions over floats, which have a mean. */
export type MeanKind = Exclude<DistributionKind, "Flip">;

export type BinaryKind = "Pair" | "Add" | "Sub" | "Mul" | "Div" | "Lt" | "Leq";
export type UnaryKind = "Fst" | "Snd" | "Inl" | "Inr" | "Neg";

/** A source range; a type alias, so that expressions also read as records of their fields. */
export type Span = {
  from: number;
  to: number;
};

export interface Choice<C> {
  probability: number;
  value: C;
}

/**
 * A source program, a determinized one, which adds the mean of a distribution, or a state of the
 * runtime, which adds rejection, a symbolic affine float and a domain error. The parser produces
 * `DiscreteWeights` and the `remainder` and `list` forms of `DiscreteList`; the front end replaces
 * `DiscreteWeights` by `Discrete` once it has checked the weights.
 */
export type Expr = Span &
  (
    | { kind: "Var"; name: string }
    /** `exact` is the value of a number literal. */
    | { kind: "Const"; value: number; exact?: Rational }
    | { kind: "Bool"; value: boolean }
    | { kind: "Unit" | "Nil" }
    | { kind: "Reject" }
    | { kind: "Lam"; param: string; body: Expr }
    | { kind: "Rec"; name: string; param: string; body: Expr }
    | { kind: "App"; fn: Expr; arg: Expr }
    | { kind: BinaryKind; left: Expr; right: Expr }
    | { kind: UnaryKind; expr: Expr }
    | { kind: "Cons"; head: Expr; tail: Expr }
    | {
        kind: "Case";
        scrutinee: Expr;
        leftName: string;
        left: Expr;
        rightName: string;
        right: Expr;
      }
    | {
        kind: "MatchList";
        scrutinee: Expr;
        nilBranch: Expr;
        headName: string;
        tailName: string;
        consBranch: Expr;
      }
    | { kind: "If"; cond: Expr; thenBranch: Expr; elseBranch: Expr }
    | { kind: "Let"; name: string; value: Expr; body: Expr }
    | { kind: "Observe"; cond: Expr }
    | { kind: ParamDistributionKind; mode: Mode | null; args: Expr[] }
    | { kind: "Discrete"; mode: Mode | null; choices: Choice<Expr>[] }
    /** `discrete(w0, …, wn)`. */
    | { kind: "DiscreteWeights"; mode: Mode | null; weights: Expr[] }
    /** `discrete(p0, …, pn, *)` and `discrete(*)` (`remainder`), or `discrete_list(e)` (`list`). */
    | { kind: "DiscreteList"; mode: Mode | null; probabilities: Expr; form: "remainder" | "list" }
    | { kind: "Mean"; distribution: MeanKind; args: Expr[] }
    | { kind: "SymFloat"; affine: Affine }
    | {
        kind: "DomainError";
        message: string;
        distribution: DistributionKind | null;
        reason: string;
      }
  );

/** A source program with the type inferred for every subexpression. */
export type TypedExpr = Span & { typ: Type } & (
    | { kind: "Var"; name: string }
    | { kind: "Const"; value: number }
    | { kind: "Bool"; value: boolean }
    | { kind: "Unit" | "Nil" }
    | { kind: "Lam"; param: string; body: TypedExpr }
    | { kind: "Rec"; name: string; param: string; body: TypedExpr }
    | { kind: "App"; fn: TypedExpr; arg: TypedExpr }
    | { kind: BinaryKind; left: TypedExpr; right: TypedExpr }
    | { kind: UnaryKind; expr: TypedExpr }
    | { kind: "Cons"; head: TypedExpr; tail: TypedExpr }
    | {
        kind: "Case";
        scrutinee: TypedExpr;
        leftName: string;
        left: TypedExpr;
        rightName: string;
        right: TypedExpr;
      }
    | {
        kind: "MatchList";
        scrutinee: TypedExpr;
        nilBranch: TypedExpr;
        headName: string;
        tailName: string;
        consBranch: TypedExpr;
      }
    | { kind: "If"; cond: TypedExpr; thenBranch: TypedExpr; elseBranch: TypedExpr }
    | { kind: "Let"; name: string; value: TypedExpr; body: TypedExpr }
    | { kind: "Observe"; cond: TypedExpr }
    | { kind: ParamDistributionKind; mode: Mode | null; args: TypedExpr[] }
    | { kind: "Discrete"; mode: Mode | null; choices: Choice<TypedExpr>[] }
    | {
        kind: "DiscreteList";
        mode: Mode | null;
        probabilities: TypedExpr;
        form: "remainder" | "list";
      }
  );

/** The members of the union U whose kind admits K. */
export type OfKind<U, K> = U extends { kind: infer UK } ? (K extends UK ? U : never) : never;
export type ExprOf<K extends Expr["kind"]> = OfKind<Expr, K>;
export type TypedExprOf<K extends TypedExpr["kind"]> = OfKind<TypedExpr, K>;

export function node<K extends Expr["kind"]>(
  kind: K,
  props: Omit<ExprOf<K>, "kind" | "from" | "to">,
  from: number,
  to: number,
): ExprOf<K> {
  return { kind, ...props, from, to } as ExprOf<K>;
}

/** The names of the primitives in programs, as in Lean's parser. */
export const distributions = new Set([
  "uniform",
  "gauss",
  "gaussian",
  "exponential",
  "gamma",
  "beta",
  "flip",
  "bernoulli",
  "poisson",
  "discrete",
  "discrete_list",
]);

export function stripSpans(expr: unknown): unknown {
  if (!expr || typeof expr !== "object") return expr;
  const out: Record<string, unknown> = { kind: (expr as { kind?: unknown }).kind };
  for (const [key, value] of Object.entries(expr)) {
    if (key === "kind" || key === "from" || key === "to") continue;
    if (Array.isArray(value)) {
      out[key] = value.map((item) => stripSpans(item));
    } else if (value && typeof value === "object" && "kind" in value) {
      out[key] = stripSpans(value);
    } else {
      out[key] = value;
    }
  }
  return out;
}
