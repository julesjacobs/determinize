// Programs as Lean's `Frontend/Pretty.lean` prints them: with an annotation at every sample site,
// `mean_<dist>` for a mean, and the forms that elaboration desugars to (`observe` as `if`, `flip` as
// a Bernoulli draw, `a - b` as `a + -b`, `a <= b` as a comparison of two lets, `discrete` as
// `discrete_list`). In `lean` mode a program prints exactly as Lean prints it, binders named by
// their depth (`x0`, `f0`) and every compound expression parenthesized. In `source` mode its
// binders keep the source's names, and it has only the parentheses and the line breaks that the
// simulator's printer needs.
import type { Expr, MeanKind, Mode, ParamDistributionKind } from "./ast.ts";
import { node } from "./ast.ts";
import type { Action, Core } from "./core.ts";
import { children } from "./core.ts";
import { prettyExpr } from "./pretty.ts";
import { leanLiteral, toNumber } from "./rational.ts";

const siteNames: Record<string, string> = {
  uniform: "uniform",
  gaussian: "gauss",
  poisson: "poisson",
  discrete: "discrete_list",
  bernoulli: "bernoulli",
  exponential: "exponential",
  beta: "beta",
  gamma: "gamma",
};

function siteHead(name: string, site: Action | Mode | null) {
  if (site === "mean") return `mean_${name}`;
  return site === null ? name : `${name}[${site}]`;
}

/** Lean's `pretty`: the program in Lean's form, character for character. */
export function leanPretty<S extends Action | Mode | null>(program: Core<S>): string {
  const render = (env: string[], depth: number, e: Core<S>): string => {
    const go = (child: Core<S>) => render(env, depth, child);
    const x = `x${depth}`;
    switch (e.kind) {
      case "bvar":
        return env[e.index] ?? `unbound_${e.index}`;
      case "reject":
        return "observe(false)";
      case "unit":
        return "()";
      case "nil":
        return "[]";
      case "bool":
        return String(e.value);
      case "real":
        return leanLiteral(e.value);
      case "lam":
        return `(fun ${x} => ${render([x, ...env], depth + 1, e.a)})`;
      case "fix":
        return `(rec f${depth} ${x} => ${render([x, `f${depth}`, ...env], depth + 1, e.a)})`;
      case "app":
        return `(${go(e.a)} ${go(e.b)})`;
      case "pair":
        return `(${go(e.a)}, ${go(e.b)})`;
      case "fst":
      case "snd":
      case "inl":
      case "inr":
        return `(${e.kind} ${go(e.a)})`;
      case "matchSum": {
        const inner = [x, ...env];
        return `(match ${go(e.a)} with inl ${x} => ${render(inner, depth + 1, e.b)} | inr ${x} => ${render(inner, depth + 1, e.c)})`;
      }
      case "cons":
        return `(${go(e.a)} :: ${go(e.b)})`;
      case "matchList": {
        const xs = `xs${depth}`;
        return `(match ${go(e.a)} with [] => ${go(e.b)} | ${x} :: ${xs} => ${render([x, xs, ...env], depth + 1, e.c)})`;
      }
      case "ite":
        if (e.b.kind === "unit" && e.c.kind === "reject") return `observe(${go(e.a)})`;
        return `(if ${go(e.a)} then ${go(e.b)} else ${go(e.c)})`;
      case "letE":
        return `(let ${x} = ${go(e.a)} in ${render([x, ...env], depth + 1, e.b)})`;
      case "neg":
        return `(-${go(e.a)})`;
      case "add":
        return `(${go(e.a)} + ${go(e.b)})`;
      case "mul":
        return `(${go(e.a)} * ${go(e.b)})`;
      case "div":
        return `(${go(e.a)} / ${go(e.b)})`;
      case "lt":
        return `(${go(e.a)} < ${go(e.b)})`;
      default:
        return `${siteHead(siteNames[e.kind], e.site)}(${children(e).map(go).join(", ")})`;
    }
  };
  return render([], 0, program);
}

const distributionKinds: Record<string, ParamDistributionKind> = {
  uniform: "Uniform",
  gaussian: "Gauss",
  poisson: "Poisson",
  bernoulli: "Bernoulli",
  exponential: "Exponential",
  beta: "Beta",
  gamma: "Gamma",
};

const binary = { add: "Add", mul: "Mul", div: "Div", lt: "Lt", pair: "Pair" } as const;
const unary = { fst: "Fst", snd: "Snd", inl: "Inl", inr: "Inr", neg: "Neg" } as const;

/** The names that a program's source gives its binders and variables. */
function sourceNames<S>(program: Core<S>, out = new Set<string>()): Set<string> {
  const source = program.source;
  if (source) {
    for (const key of ["name", "param", "leftName", "rightName", "headName", "tailName"]) {
      const name = (source as Record<string, unknown>)[key];
      if (typeof name === "string") out.add(name);
    }
  }
  for (const child of children(program)) sourceNames(child, out);
  return out;
}

/**
 * The program as a parsed expression in Lean's forms, with the source's names for its binders;
 * binders that elaboration introduced get names that the source doesn't use.
 */
function toExpr<S extends Action | Mode | null>(program: Core<S>): Expr {
  const used = sourceNames(program);
  const fresh = (depth: number) => {
    let name = `x${depth}`;
    while (used.has(name)) name += "'";
    return name;
  };
  const go = (env: string[], depth: number, e: Core<S>): Expr => {
    const at = [e.from, e.to] as const;
    const source = e.source;
    const sub = (child: Core<S>) => go(env, depth, child);
    switch (e.kind) {
      case "bvar":
        return node("Var", { name: env[e.index] ?? `unbound_${e.index}` }, ...at);
      case "reject":
        // Lean prints a rejection as the observation that fails.
        return node("Observe", { cond: node("Bool", { value: false }, ...at) }, ...at);
      case "unit":
        return node("Unit", {}, ...at);
      case "nil":
        return node("Nil", {}, ...at);
      case "bool":
        return node("Bool", { value: e.value }, ...at);
      case "real":
        return node("Const", { value: toNumber(e.value), exact: e.value }, ...at);
      case "lam": {
        const param = source?.kind === "Lam" ? source.param : fresh(depth);
        return node("Lam", { param, body: go([param, ...env], depth + 1, e.a) }, ...at);
      }
      case "fix": {
        const self = source?.kind === "Rec" ? source.name : fresh(depth);
        const param = source?.kind === "Rec" ? source.param : fresh(depth + 1);
        const body = go([param, self, ...env], depth + 1, e.a);
        return node("Rec", { name: self, param, body }, ...at);
      }
      case "app":
        return node("App", { fn: sub(e.a), arg: sub(e.b) }, ...at);
      case "pair":
      case "add":
      case "mul":
      case "div":
      case "lt":
        return node(binary[e.kind], { left: sub(e.a), right: sub(e.b) }, ...at);
      case "fst":
      case "snd":
      case "inl":
      case "inr":
      case "neg":
        return node(unary[e.kind], { expr: sub(e.a) }, ...at);
      case "cons":
        return node("Cons", { head: sub(e.a), tail: sub(e.b) }, ...at);
      case "matchSum": {
        const leftName = source?.kind === "Case" ? source.leftName : fresh(depth);
        const rightName = source?.kind === "Case" ? source.rightName : fresh(depth);
        return node(
          "Case",
          {
            scrutinee: sub(e.a),
            leftName,
            left: go([leftName, ...env], depth + 1, e.b),
            rightName,
            right: go([rightName, ...env], depth + 1, e.c),
          },
          ...at,
        );
      }
      case "matchList": {
        const headName = source?.kind === "MatchList" ? source.headName : fresh(depth);
        const tailName = source?.kind === "MatchList" ? source.tailName : fresh(depth + 1);
        return node(
          "MatchList",
          {
            scrutinee: sub(e.a),
            nilBranch: sub(e.b),
            headName,
            tailName,
            consBranch: go([headName, tailName, ...env], depth + 1, e.c),
          },
          ...at,
        );
      }
      case "ite":
        if (e.b.kind === "unit" && e.c.kind === "reject") {
          return node("Observe", { cond: sub(e.a) }, ...at);
        }
        return node("If", { cond: sub(e.a), thenBranch: sub(e.b), elseBranch: sub(e.c) }, ...at);
      case "letE": {
        const bound = source?.kind === "Let" ? source.name : fresh(depth);
        return node(
          "Let",
          { name: bound, value: sub(e.a), body: go([bound, ...env], depth + 1, e.b) },
          ...at,
        );
      }
      default: {
        const args = children(e).map(sub);
        if (e.kind === "discrete") {
          if (e.site === "mean") {
            return node("Mean", { distribution: "DiscreteList", args }, ...at);
          }
          return node(
            "DiscreteList",
            { mode: e.site as Mode | null, probabilities: args[0], form: "list" },
            ...at,
          );
        }
        const kind = distributionKinds[e.kind];
        if (e.site === "mean") {
          return node("Mean", { distribution: kind as MeanKind, args }, ...at);
        }
        return node(kind, { mode: e.site as Mode | null, args }, ...at);
      }
    }
  };
  return go([], 0, program);
}

/** The program in Lean's forms with the source's names, minimal parentheses and line breaks. */
export function sourcePretty<S extends Action | Mode | null>(program: Core<S>): string {
  return prettyExpr(toExpr(program));
}
