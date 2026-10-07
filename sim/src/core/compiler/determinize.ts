import type { Expr, Mode } from "./ast.ts";
import { node } from "./ast.ts";

/**
 * The parsed program with the mode of every sample site, in the simulator's forms: Lean's
 * annotated program before desugaring. With `means`, every E site becomes the mean of its
 * distribution, as Lean's `Expr.determinize` turns the annotated program into the determinized
 * one; the draws that stay keep no annotation. Literal discrete weights become choices.
 */
export function annotate(e: Expr, modeOf: (site: Expr) => Mode, means = false): Expr {
  const go = (child: Expr) => annotate(child, modeOf, means);
  const at = [e.from, e.to] as const;
  switch (e.kind) {
    case "Var":
    case "Bool":
    case "Unit":
    case "Nil":
      return e;
    case "Const":
      return node("Const", { value: e.value }, ...at);
    case "Lam":
      return node("Lam", { param: e.param, body: go(e.body) }, ...at);
    case "Rec":
      return node("Rec", { name: e.name, param: e.param, body: go(e.body) }, ...at);
    case "App":
      return node("App", { fn: go(e.fn), arg: go(e.arg) }, ...at);
    case "Pair":
    case "Add":
    case "Sub":
    case "Mul":
    case "Div":
    case "Lt":
    case "Leq":
      return node(e.kind, { left: go(e.left), right: go(e.right) }, ...at);
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return node(e.kind, { expr: go(e.expr) }, ...at);
    case "Cons":
      return node("Cons", { head: go(e.head), tail: go(e.tail) }, ...at);
    case "Case":
      return node(
        "Case",
        {
          scrutinee: go(e.scrutinee),
          leftName: e.leftName,
          left: go(e.left),
          rightName: e.rightName,
          right: go(e.right),
        },
        ...at,
      );
    case "MatchList":
      return node(
        "MatchList",
        {
          scrutinee: go(e.scrutinee),
          nilBranch: go(e.nilBranch),
          headName: e.headName,
          tailName: e.tailName,
          consBranch: go(e.consBranch),
        },
        ...at,
      );
    case "If":
      return node(
        "If",
        { cond: go(e.cond), thenBranch: go(e.thenBranch), elseBranch: go(e.elseBranch) },
        ...at,
      );
    case "Let":
      return node("Let", { name: e.name, value: go(e.value), body: go(e.body) }, ...at);
    case "Observe":
      return node("Observe", { cond: go(e.cond) }, ...at);
    case "Flip":
      // flip(p) samples bernoulli[G](p).
      return node("Flip", { mode: means ? null : "G", args: e.args.map(go) }, ...at);
    case "Uniform":
    case "Gauss":
    case "Exponential":
    case "Gamma":
    case "Beta":
    case "Bernoulli":
    case "Poisson": {
      const mode = modeOf(e);
      const args = e.args.map(go);
      if (!means) return node(e.kind, { mode, args }, ...at);
      if (mode === "E") return node("Mean", { distribution: e.kind, args }, ...at);
      return node(e.kind, { mode: null, args }, ...at);
    }
    case "DiscreteWeights": {
      const mode = modeOf(e);
      const probabilities = e.weights.map((weight) => {
        if (weight.kind !== "Const") throw new Error("discrete weights are not literals");
        return weight.value;
      });
      if (means && mode === "E") {
        const args = probabilities.map((value) => node("Const", { value }, ...at));
        return node("Mean", { distribution: "Discrete", args }, ...at);
      }
      const choices = probabilities.map((probability, index) => ({
        probability,
        value: node("Const", { value: index }, ...at),
      }));
      return node("Discrete", { mode: means ? null : mode, choices }, ...at);
    }
    case "DiscreteList": {
      const mode = modeOf(e);
      const probabilities = go(e.probabilities);
      if (means && mode === "E") {
        return node("Mean", { distribution: "DiscreteList", args: [probabilities] }, ...at);
      }
      return node(
        "DiscreteList",
        { mode: means ? null : mode, probabilities, form: e.form },
        ...at,
      );
    }
    default:
      throw new Error(`a parsed program has no ${e.kind}`);
  }
}
