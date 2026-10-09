// A port of Lean's `Frontend/Elaborate.lean`: resolves names to de Bruijn indices, checks
// literal discrete weights, and desugars subtraction, `<=`, `observe`, `flip` and
// multiplication by a literal on the right. It fails on exactly the programs that Lean's
// elaborator rejects, with source spans and the simulator's messages.
import type { Expr, Span } from "./ast.ts";
import type { Input } from "./core.ts";
import { CompileError } from "./errors.ts";
import type { Rational } from "./rational.ts";
import { add, compare, formatDecimal, rational } from "./rational.ts";

/** A name no program can bind; lowering under it shifts free variables by one. */
const hidden = "";

function at(span: Span, source: Expr | null = null) {
  return { from: span.from, to: span.to, source };
}

function index(env: string[], name: string, span: Span): number {
  const i = env.indexOf(name);
  if (i < 0) throw new CompileError(`unbound variable \`${name}\``, span.from, span.to);
  return i;
}

/** The weights of a literal discrete distribution, checked as `Checking.finiteDistribution`. A
 * number literal is never negative: the parser reads `-1` as the negation of the literal 1, which
 * Lean's elaborator rejects as a negative weight, `-0` included. */
function finiteDistribution(expr: Expr & { kind: "DiscreteWeights" }): Rational[] {
  const weights = expr.weights.map((weight) => {
    if (weight.kind === "Neg" && weight.expr.kind === "Const") {
      throw new CompileError(
        "discrete expects nonnegative literal weights",
        weight.from,
        weight.to,
      );
    }
    if (weight.kind !== "Const" || weight.exact === undefined) {
      throw new CompileError("discrete weights must be number literals", weight.from, weight.to);
    }
    return weight.exact;
  });
  const total = weights.reduce(add, rational(0n));
  if (compare(total, rational(1n)) !== 0) {
    throw new CompileError(
      `discrete weights must sum to 1, not ${formatDecimal(total)}`,
      expr.from,
      expr.to,
    );
  }
  return weights;
}

/** The list `p0 :: … :: pn :: []` of literal probabilities. */
function literalList(values: Rational[], spans: Span[], end: Span): Input {
  let list: Input = { kind: "nil", ...at(end) };
  for (let i = values.length - 1; i >= 0; i--) {
    const head: Input = { kind: "real", value: values[i], ...at(spans[i]) };
    list = { kind: "cons", a: head, b: list, from: spans[i].from, to: end.to, source: null };
  }
  return list;
}

function lower(env: string[], e: Expr): Input {
  const node = at(e, e);
  switch (e.kind) {
    case "Var":
      return { kind: "bvar", index: index(env, e.name, e), ...node };
    case "Const":
      if (e.exact === undefined) throw new Error("a number without its exact value");
      return { kind: "real", value: e.exact, ...node };
    case "Lam":
      return { kind: "lam", a: lower([e.param, ...env], e.body), ...node };
    case "Rec":
      return { kind: "fix", a: lower([e.param, e.name, ...env], e.body), ...node };
    case "Let":
      return {
        kind: "letE",
        a: lower(env, e.value),
        b: lower([e.name, ...env], e.body),
        ...node,
      };
    case "MatchList":
      return {
        kind: "matchList",
        a: lower(env, e.scrutinee),
        b: lower(env, e.nilBranch),
        c: lower([e.headName, e.tailName, ...env], e.consBranch),
        ...node,
      };
    case "Case":
      return {
        kind: "matchSum",
        a: lower(env, e.scrutinee),
        b: lower([e.leftName, ...env], e.left),
        c: lower([e.rightName, ...env], e.right),
        ...node,
      };
    case "Unit":
      return { kind: "unit", ...node };
    case "Nil":
      return { kind: "nil", ...node };
    case "Bool":
      return { kind: "bool", value: e.value, ...node };
    case "If":
      return {
        kind: "ite",
        a: lower(env, e.cond),
        b: lower(env, e.thenBranch),
        c: lower(env, e.elseBranch),
        ...node,
      };
    case "DiscreteWeights": {
      const weights = finiteDistribution(e);
      const last = e.weights[e.weights.length - 1];
      const a = literalList(weights.slice(0, -1), e.weights, last);
      return { kind: "discrete", site: e.mode, a, ...node };
    }
    case "DiscreteList":
      return { kind: "discrete", site: e.mode, a: lower(env, e.probabilities), ...node };
    case "Neg":
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr": {
      const kind = ({ Neg: "neg", Fst: "fst", Snd: "snd", Inl: "inl", Inr: "inr" } as const)[
        e.kind
      ];
      return { kind, a: lower(env, e.expr), ...node };
    }
    case "Poisson":
    case "Exponential":
    case "Bernoulli": {
      const kind = (
        { Poisson: "poisson", Exponential: "exponential", Bernoulli: "bernoulli" } as const
      )[e.kind];
      return { kind, site: e.mode, a: lower(env, e.args[0]), ...node };
    }
    case "Observe": {
      const condition = lower(env, e.cond);
      return {
        kind: "ite",
        a: condition,
        b: { kind: "unit", ...at(e) },
        c: { kind: "reject", ...at(e) },
        ...node,
      };
    }
    case "Flip": {
      if (e.mode === "E") {
        throw new CompileError("flip produces a Boolean and requires [G]", e.from, e.to);
      }
      const draw: Input = { kind: "bernoulli", site: "G", a: lower(env, e.args[0]), ...at(e) };
      return { kind: "lt", a: { kind: "real", value: rational(0n), ...at(e) }, b: draw, ...node };
    }
    case "App":
      return { kind: "app", a: lower(env, e.fn), b: lower(env, e.arg), ...node };
    case "Pair":
      return { kind: "pair", a: lower(env, e.left), b: lower(env, e.right), ...node };
    case "Cons":
      return { kind: "cons", a: lower(env, e.head), b: lower(env, e.tail), ...node };
    case "Add":
      return { kind: "add", a: lower(env, e.left), b: lower(env, e.right), ...node };
    case "Div":
      return { kind: "div", a: lower(env, e.left), b: lower(env, e.right), ...node };
    case "Lt":
      return { kind: "lt", a: lower(env, e.left), b: lower(env, e.right), ...node };
    case "Sub": {
      const a = lower(env, e.left);
      const b = lower(env, e.right);
      return { kind: "add", a, b: { kind: "neg", a: b, ...at(b) }, ...node };
    }
    case "Mul": {
      const a = lower(env, e.left);
      const b = lower(env, e.right);
      // A literal factor goes to the left, where multiplication requires G.
      if (a.kind !== "real" && b.kind === "real") return { kind: "mul", a: b, b: a, ...node };
      return { kind: "mul", a, b, ...node };
    }
    case "Leq": {
      // a <= b is let x = a in let y = b in if y < x then false else true.
      const a = lower(env, e.left);
      const b = lower([hidden, ...env], e.right);
      const y: Input = { kind: "bvar", index: 0, ...at(e.right) };
      const x: Input = { kind: "bvar", index: 1, ...at(e.left) };
      const test: Input = {
        kind: "ite",
        a: { kind: "lt", a: y, b: x, ...at(e) },
        b: { kind: "bool", value: false, ...at(e) },
        c: { kind: "bool", value: true, ...at(e) },
        ...at(e),
      };
      return { kind: "letE", a, b: { kind: "letE", a: b, b: test, ...at(e) }, ...node };
    }
    case "Uniform":
    case "Gauss":
    case "Beta":
    case "Gamma": {
      const kind = (
        { Uniform: "uniform", Gauss: "gaussian", Beta: "beta", Gamma: "gamma" } as const
      )[e.kind];
      return { kind, site: e.mode, a: lower(env, e.args[0]), b: lower(env, e.args[1]), ...node };
    }
    default:
      throw new Error(`cannot elaborate ${e.kind}`);
  }
}

/** The resolved program of a parsed one, as Lean's `elaborate`. */
export function elaborate(e: Expr): Input {
  return lower([], e);
}
