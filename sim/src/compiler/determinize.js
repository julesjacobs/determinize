import { node } from "./ast.js";
import { zonk } from "./types.js";

function exprNode(kind, props, from, to) {
  return node(kind, props, from ?? 0, to ?? from ?? 0);
}

function floatMode(typedExpr) {
  const ty = zonk(typedExpr.typ);
  return ty.tag === "Float" ? ty.mode.mode : null;
}

export function determinize(typedExpr) {
  return ofTyped(typedExpr);
}

function ofTyped(te) {
  switch (te.kind) {
    case "Var":
      return exprNode("Var", { name: te.name }, te.from, te.to);
    case "Lam":
      return exprNode("Lam", { param: te.param, body: ofTyped(te.body) }, te.from, te.to);
    case "Rec":
      return exprNode("Rec", { name: te.name, param: te.param, body: ofTyped(te.body) }, te.from, te.to);
    case "App":
      return exprNode("App", { fn: ofTyped(te.fn), arg: ofTyped(te.arg) }, te.from, te.to);
    case "Unit":
      return exprNode("Unit", {}, te.from, te.to);
    case "Pair":
      return exprNode("Pair", { left: ofTyped(te.left), right: ofTyped(te.right) }, te.from, te.to);
    case "Fst":
    case "Snd":
    case "Inl":
    case "Inr":
    case "Neg":
      return exprNode(te.kind, { expr: ofTyped(te.expr) }, te.from, te.to);
    case "Nil":
      return exprNode("Nil", {}, te.from, te.to);
    case "Cons":
      return exprNode("Cons", { head: ofTyped(te.head), tail: ofTyped(te.tail) }, te.from, te.to);
    case "Case":
      return exprNode(
        "Case",
        {
          scrutinee: ofTyped(te.scrutinee),
          leftName: te.leftName,
          left: ofTyped(te.left),
          rightName: te.rightName,
          right: ofTyped(te.right),
        },
        te.from,
        te.to,
      );
    case "MatchList":
      return exprNode(
        "MatchList",
        {
          scrutinee: ofTyped(te.scrutinee),
          nilBranch: ofTyped(te.nilBranch),
          headName: te.headName,
          tailName: te.tailName,
          consBranch: ofTyped(te.consBranch),
        },
        te.from,
        te.to,
      );
    case "Bool":
      return exprNode("Bool", { value: te.value }, te.from, te.to);
    case "If":
      return exprNode("If", { cond: ofTyped(te.cond), thenBranch: ofTyped(te.thenBranch), elseBranch: ofTyped(te.elseBranch) }, te.from, te.to);
    case "Let":
      return exprNode("Let", { name: te.name, value: ofTyped(te.value), body: ofTyped(te.body) }, te.from, te.to);
    case "Const":
      return exprNode("Const", { value: te.value }, te.from, te.to);
    case "Add":
    case "Mul":
    case "Sub":
    case "Div":
    case "Lt":
    case "Leq":
      return exprNode(te.kind, { left: ofTyped(te.left), right: ofTyped(te.right) }, te.from, te.to);
    case "Uniform":
      if (floatMode(te) === "E") return exprNode("Mul", { left: exprNode("Add", { left: ofTyped(te.args[0]), right: ofTyped(te.args[1]) }, te.from, te.to), right: exprNode("Const", { value: 0.5 }, te.from, te.to) }, te.from, te.to);
      return exprNode("Uniform", { mode: null, args: te.args.map(ofTyped) }, te.from, te.to);
    case "Gauss":
      if (floatMode(te) === "E") return ofTyped(te.args[0]);
      return exprNode("Gauss", { mode: null, args: te.args.map(ofTyped) }, te.from, te.to);
    case "Exponential":
      if (floatMode(te) === "E") return exprNode("Div", { left: exprNode("Const", { value: 1 }, te.from, te.to), right: ofTyped(te.args[0]) }, te.from, te.to);
      return exprNode("Exponential", { mode: null, args: te.args.map(ofTyped) }, te.from, te.to);
    case "Gamma":
      if (floatMode(te) === "E") return exprNode("Div", { left: ofTyped(te.args[0]), right: ofTyped(te.args[1]) }, te.from, te.to);
      return exprNode("Gamma", { mode: null, args: te.args.map(ofTyped) }, te.from, te.to);
    case "Beta":
      if (floatMode(te) === "E") {
        const a = ofTyped(te.args[0]);
        const b = ofTyped(te.args[1]);
        return exprNode("Div", { left: a, right: exprNode("Add", { left: a, right: b }, te.from, te.to) }, te.from, te.to);
      }
      return exprNode("Beta", { mode: null, args: te.args.map(ofTyped) }, te.from, te.to);
    case "Flip":
      return exprNode("Flip", { mode: null, args: te.args.map(ofTyped) }, te.from, te.to);
    case "Bernoulli":
    case "Poisson":
      if (floatMode(te) === "E") return ofTyped(te.args[0]);
      return exprNode(te.kind, { mode: null, args: te.args.map(ofTyped) }, te.from, te.to);
    case "Discrete":
      if (floatMode(te) === "E") return weightedSum(te.choices.map((choice) => ({ probability: choice.probability, value: ofTyped(choice.value) })), te);
      return exprNode("Discrete", { mode: null, choices: te.choices.map((choice) => ({ probability: choice.probability, value: ofTyped(choice.value) })) }, te.from, te.to);
    case "Observe":
      return exprNode("Observe", { cond: ofTyped(te.cond) }, te.from, te.to);
    default:
      throw new Error(`unsupported typed expression ${te.kind}`);
  }
}

function weightedSum(choices, source) {
  if (choices.length === 0) return exprNode("Const", { value: 0 }, source.from, source.to);
  const [first, ...rest] = choices;
  const term = exprNode("Mul", { left: exprNode("Const", { value: first.probability }, source.from, source.to), right: first.value }, source.from, source.to);
  if (rest.length === 0) return term;
  return exprNode("Add", { left: term, right: weightedSum(rest, source) }, source.from, source.to);
}
