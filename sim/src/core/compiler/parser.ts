// A port of the Pratt parser of Lean's `Frontend/Parser.lean`. It accepts exactly the programs
// that Lean's parser accepts and builds the same tree, with source spans and its own messages.
import type { Expr, Mode, Span } from "./ast.ts";
import { distributions, node } from "./ast.ts";
import { CompileError } from "./errors.ts";
import type { Token } from "./lexer.ts";
import { lex } from "./lexer.ts";
import type { Rational } from "./rational.ts";
import { rational, toNumber } from "./rational.ts";

/** Words that are not an operand, so they end an application. */
const nonOperands = new Set(["in", "then", "else", "with", "let", "if", "match", "fun", "rec"]);

const keywords = new Set([
  ...nonOperands,
  "lambda",
  "true",
  "false",
  "fst",
  "snd",
  "inl",
  "inr",
  "observe",
  ...distributions,
]);

/** Left and right binding power of each infix operator. */
const infix: Record<string, [number, number]> = {
  "<": [30, 31],
  "<=": [30, 31],
  "::": [40, 40],
  "+": [50, 51],
  "-": [50, 51],
  "*": [60, 61],
  "/": [60, 61],
};

const infixKind = {
  "<": "Lt",
  "<=": "Leq",
  "+": "Add",
  "-": "Sub",
  "*": "Mul",
  "/": "Div",
} as const;

const arity: Record<string, number> = {
  uniform: 2,
  gauss: 2,
  gaussian: 2,
  poisson: 1,
  exponential: 1,
  bernoulli: 1,
  beta: 2,
  gamma: 2,
  discrete_list: 1,
  flip: 1,
  observe: 1,
};

const primitiveKind = {
  uniform: "Uniform",
  gauss: "Gauss",
  gaussian: "Gauss",
  poisson: "Poisson",
  exponential: "Exponential",
  bernoulli: "Bernoulli",
  beta: "Beta",
  gamma: "Gamma",
  flip: "Flip",
} as const;

function label(token: Token): string {
  if (token.kind === "end") return "end of input";
  if (token.kind === "number") return "number";
  if (token.kind === "word" && !keywords.has(token.text)) return "identifier";
  return `\`${token.text}\``;
}

function expected(what: string, found: Token): CompileError {
  if (found.kind === "end") {
    return new CompileError(`expected ${what} before end of input`, found.from, found.to);
  }
  return new CompileError(`expected ${what}, found ${label(found)}`, found.from, found.to);
}

const startsName = (token: Token) => token.kind === "word";

/** A token that starts an operand of an application, as Lean's `startsAtom`. */
function startsAtom(token: Token): boolean {
  if (token.text === "(" || token.text === "[") return true;
  return (token.kind === "word" || token.kind === "number") && !nonOperands.has(token.text);
}

/** The value of a decimal literal: digits, an optional fraction and an optional exponent. */
function decimal(token: Token): Rational {
  const [mantissa, exponentText] = token.text.toLowerCase().split("e");
  const exponent = exponentText === undefined ? 0n : BigInt(exponentText.replace(/^\+/, ""));
  if (exponent > 10000n || exponent < -10000n) {
    throw new CompileError("decimal exponent exceeds 10000", token.from, token.to);
  }
  const [whole, fraction = ""] = mantissa.split(".");
  const value = rational(BigInt(whole + fraction), 10n ** BigInt(fraction.length));
  const scale = 10n ** (exponent < 0n ? -exponent : exponent);
  return exponent >= 0n
    ? rational(value.num * scale, value.den)
    : rational(value.num, value.den * scale);
}

class Parser {
  declare tokens: Token[];
  declare pos: number;

  constructor(source: string) {
    this.tokens = lex(source);
    this.pos = 0;
  }

  peek(): Token {
    return this.tokens[Math.min(this.pos, this.tokens.length - 1)];
  }

  /** The end of the last token taken. */
  end(): number {
    return this.tokens[this.pos - 1].to;
  }

  take(): Token {
    const token = this.peek();
    this.pos++;
    return token;
  }

  expect(text: string): Token {
    const token = this.peek();
    if (token.kind === "end" || token.text !== text) throw expected(`\`${text}\``, token);
    return this.take();
  }

  name(): string {
    const token = this.peek();
    if (!startsName(token)) throw expected("identifier", token);
    return this.take().text;
  }

  parseMain(): Expr {
    const expr = this.expr(0);
    const rest = this.peek();
    if (rest.kind !== "end") throw expected("end of input", rest);
    return expr;
  }

  expr(minPrec: number): Expr {
    let left = this.prefix();
    for (;;) {
      const op = this.peek();
      const power = op.kind === "symbol" ? infix[op.text] : undefined;
      if (power) {
        const [leftPower, rightPower] = power;
        if (leftPower < minPrec) break;
        this.take();
        const right = this.expr(rightPower);
        left =
          op.text === "::"
            ? node("Cons", { head: left, tail: right }, left.from, right.to)
            : node(
                infixKind[op.text as keyof typeof infixKind],
                { left, right },
                left.from,
                right.to,
              );
      } else if (minPrec <= 80 && startsAtom(op)) {
        const arg = this.expr(81);
        left = node("App", { fn: left, arg }, left.from, arg.to);
      } else {
        break;
      }
    }
    return left;
  }

  prefix(): Expr {
    const token = this.peek();
    const from = token.from;
    if (token.kind === "end") throw expected("expression", token);
    this.take();
    switch (token.text) {
      case "let": {
        const name = this.name();
        this.expect("=");
        const value = this.expr(0);
        this.expect("in");
        const body = this.expr(0);
        return node("Let", { name, value, body }, from, body.to);
      }
      case "fun":
      case "lambda":
      case "\\": {
        const param = this.name();
        this.expect("=>");
        const body = this.expr(0);
        return node("Lam", { param, body }, from, body.to);
      }
      case "rec": {
        const name = this.name();
        const param = this.name();
        this.expect("=>");
        const body = this.expr(0);
        return node("Rec", { name, param, body }, from, body.to);
      }
      case "if": {
        const cond = this.expr(0);
        this.expect("then");
        const thenBranch = this.expr(0);
        this.expect("else");
        const elseBranch = this.expr(0);
        return node("If", { cond, thenBranch, elseBranch }, from, elseBranch.to);
      }
      case "match":
        return this.match(from);
      case "(": {
        if (this.peek().text === ")" && this.peek().kind === "symbol") {
          this.take();
          return node("Unit", {}, from, this.end());
        }
        const first = this.expr(0);
        if (this.peek().text === "," && this.peek().kind === "symbol") {
          this.take();
          const second = this.expr(0);
          this.expect(")");
          return node("Pair", { left: first, right: second }, from, this.end());
        }
        this.expect(")");
        return { ...first, from, to: this.end() };
      }
      case "[":
        this.expect("]");
        return node("Nil", {}, from, this.end());
      case "-": {
        const expr = this.expr(70);
        return node("Neg", { expr }, from, expr.to);
      }
      case "fst":
      case "snd":
      case "inl":
      case "inr": {
        const expr = this.expr(81);
        const kind = ({ fst: "Fst", snd: "Snd", inl: "Inl", inr: "Inr" } as const)[token.text];
        return node(kind, { expr }, from, expr.to);
      }
      case "true":
      case "false":
        return node("Bool", { value: token.text === "true" }, from, token.to);
    }
    if (token.kind === "word" && distributions.has(token.text)) return this.primitive(token);
    if (token.text === "observe") return this.primitive(token);
    if (token.kind === "number") {
      const exact = decimal(token);
      return node("Const", { value: toNumber(exact), exact }, from, token.to);
    }
    if (token.kind === "word") return node("Var", { name: token.text }, from, token.to);
    throw expected("expression", token);
  }

  match(from: number): Expr {
    const scrutinee = this.expr(0);
    this.expect("with");
    if (this.peek().text === "|" && this.peek().kind === "symbol") this.take();
    if (this.peek().text === "[" && this.peek().kind === "symbol") {
      this.expect("[");
      this.expect("]");
      this.expect("=>");
      const nilBranch = this.expr(0);
      this.expect("|");
      const headName = this.name();
      this.expect("::");
      const tailName = this.name();
      this.expect("=>");
      const consBranch = this.expr(0);
      return node(
        "MatchList",
        { scrutinee, nilBranch, headName, tailName, consBranch },
        from,
        consBranch.to,
      );
    }
    this.expect("inl");
    const leftName = this.name();
    this.expect("=>");
    const left = this.expr(0);
    this.expect("|");
    this.expect("inr");
    const rightName = this.name();
    this.expect("=>");
    const right = this.expr(0);
    return node("Case", { scrutinee, leftName, left, rightName, right }, from, right.to);
  }

  /** `name[m](args)`; `discrete` also takes `*` as its last argument. */
  primitive(head: Token): Expr {
    const name = head.text;
    let mode: Mode | null = null;
    let modeSpan: Span | null = null;
    if (this.peek().text === "[" && this.peek().kind === "symbol") {
      const open = this.take();
      const modeToken = this.take();
      if (modeToken.text !== "E" && modeToken.text !== "G") {
        if (modeToken.kind === "end") throw expected("distribution mode `E` or `G`", modeToken);
        throw new CompileError(
          "expected distribution mode `E` or `G`",
          modeToken.from,
          modeToken.to,
        );
      }
      mode = modeToken.text;
      this.expect("]");
      modeSpan = { from: open.from, to: this.end() };
    }
    this.expect("(");
    const args: Expr[] = [];
    let star: Token | null = null;
    const atStar = () => name === "discrete" && this.peek().text === "*";
    if (atStar()) {
      star = this.take();
    } else if (!(this.peek().text === ")" && this.peek().kind === "symbol")) {
      args.push(this.expr(0));
      while (this.peek().text === "," && this.peek().kind === "symbol") {
        this.take();
        if (atStar()) {
          star = this.take();
          break;
        }
        args.push(this.expr(0));
      }
    }
    this.expect(")");
    const span = { from: head.from, to: this.end() };
    if (name === "discrete") {
      if (!star) return node("DiscreteWeights", { mode, weights: args }, span.from, span.to);
      let probabilities: Expr = node("Nil", {}, star.from, star.to);
      for (const probability of args.toReversed()) {
        probabilities = node(
          "Cons",
          { head: probability, tail: probabilities },
          probability.from,
          star.to,
        );
      }
      return node("DiscreteList", { mode, probabilities, form: "remainder" }, span.from, span.to);
    }
    if (args.length !== arity[name]) {
      const noun = arity[name] === 1 ? "argument" : "arguments";
      throw new CompileError(
        `\`${name}\` takes ${arity[name]} ${noun}, found ${args.length}`,
        span.from,
        span.to,
      );
    }
    if (name === "observe") {
      if (modeSpan) {
        throw new CompileError("`observe` has no sampling mode", modeSpan.from, modeSpan.to);
      }
      return node("Observe", { cond: args[0] }, span.from, span.to);
    }
    if (name === "discrete_list") {
      return node(
        "DiscreteList",
        { mode, probabilities: args[0], form: "list" },
        span.from,
        span.to,
      );
    }
    const kind = primitiveKind[name as keyof typeof primitiveKind];
    return node(kind, { mode, args }, span.from, span.to);
  }
}

export function parse(source: string): Expr {
  return new Parser(source).parseMain();
}
