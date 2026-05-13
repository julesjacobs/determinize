import { distributions, node } from "./ast.js";
import { CompileError } from "./errors.js";
import { lex } from "./lexer.js";

const distTokenToKind = {
  UNIFORM: "Uniform",
  GAUSS: "Gauss",
  EXPONENTIAL: "Exponential",
  GAMMA: "Gamma",
  BETA: "Beta",
  FLIP: "Flip",
  BERNOULLI: "Bernoulli",
  POISSON: "Poisson",
  DISCRETE: "Discrete",
};

const distKindToName = Object.fromEntries(
  Object.entries(distTokenToKind).map(([token, kind]) => [kind, token.toLowerCase()]),
);

const tokenLabels = {
  EOF: "end of input",
  IDENT: "identifier",
  FLOAT: "number",
  TRUE: "`true`",
  FALSE: "`false`",
  FUN: "`fun`",
  REC: "`rec`",
  LET: "`let`",
  IN: "`in`",
  IF: "`if`",
  THEN: "`then`",
  ELSE: "`else`",
  MATCH: "`match`",
  WITH: "`with`",
  INL: "`inl`",
  INR: "`inr`",
  FST: "`fst`",
  SND: "`snd`",
  OBSERVE: "`observe`",
  LPAREN: "`(`",
  RPAREN: "`)`",
  LBRACK: "`[`",
  RBRACK: "`]`",
  COMMA: "`,`",
  BAR: "`|`",
  EQ: "`=`",
  PLUS: "`+`",
  MINUS: "`-`",
  TIMES: "`*`",
  DIVIDE: "`/`",
  LT: "`<`",
  LEQ: "`<=`",
  DARROW: "`=>`",
  CONS: "`::`",
};

for (const token of Object.keys(distTokenToKind)) {
  tokenLabels[token] = `\`${token.toLowerCase()}\``;
}

function tokenLabel(kind) {
  return tokenLabels[kind] ?? kind;
}

function expectedMessage(expected, found) {
  const expectedText = tokenLabel(expected);
  if (found.kind === "EOF") return `expected ${expectedText} before end of input`;
  return `expected ${expectedText}, found ${tokenLabel(found.kind)}`;
}

class Parser {
  constructor(source) {
    this.source = source;
    this.tokens = lex(source);
    this.pos = 0;
  }

  current() {
    return this.tokens[this.pos];
  }

  at(kind) {
    return this.current().kind === kind;
  }

  take(kind) {
    const tok = this.current();
    if (tok.kind !== kind) {
      throw new CompileError(expectedMessage(kind, tok), tok.from, tok.to);
    }
    this.pos++;
    return tok;
  }

  maybe(kind) {
    if (!this.at(kind)) return null;
    return this.take(kind);
  }

  parseMain() {
    const expr = this.parseExpr();
    this.take("EOF");
    return expr;
  }

  parseExpr() {
    if (this.at("IF")) {
      const start = this.take("IF").from;
      const cond = this.parseExpr();
      this.take("THEN");
      const thenBranch = this.parseExpr();
      this.take("ELSE");
      const elseBranch = this.parseExpr();
      return node("If", { cond, thenBranch, elseBranch }, start, elseBranch.to);
    }

    if (this.at("LET")) {
      const start = this.take("LET").from;
      const name = this.take("IDENT");
      this.take("EQ");
      const value = this.parseExpr();
      this.take("IN");
      const body = this.parseExpr();
      return node("Let", { name: name.value, value, body }, start, body.to);
    }

    if (this.at("MATCH")) return this.parseMatch();
    return this.parseFun();
  }

  parseMatch() {
    const start = this.take("MATCH").from;
    const scrutinee = this.parseExpr();
    this.take("WITH");
    if (this.at("INL")) {
      this.take("INL");
      const leftName = this.take("IDENT");
      this.take("DARROW");
      const left = this.parseExpr();
      this.take("BAR");
      this.take("INR");
      const rightName = this.take("IDENT");
      this.take("DARROW");
      const right = this.parseExpr();
      return node(
        "Case",
        { scrutinee, leftName: leftName.value, left, rightName: rightName.value, right },
        start,
        right.to,
      );
    }

    this.take("LBRACK");
    this.take("RBRACK");
    this.take("DARROW");
    const nilBranch = this.parseExpr();
    this.take("BAR");
    const headName = this.take("IDENT");
    this.take("CONS");
    const tailName = this.take("IDENT");
    this.take("DARROW");
    const consBranch = this.parseExpr();
    return node(
      "MatchList",
      {
        scrutinee,
        nilBranch,
        headName: headName.value,
        tailName: tailName.value,
        consBranch,
      },
      start,
      consBranch.to,
    );
  }

  parseFun() {
    if (this.at("FUN")) {
      const start = this.take("FUN").from;
      const param = this.take("IDENT");
      this.take("DARROW");
      const body = this.parseExpr();
      return node("Lam", { param: param.value, body }, start, body.to);
    }

    if (this.at("REC")) {
      const start = this.take("REC").from;
      const name = this.take("IDENT");
      const param = this.take("IDENT");
      this.take("DARROW");
      const body = this.parseExpr();
      return node("Rec", { name: name.value, param: param.value, body }, start, body.to);
    }

    return this.parseCmp();
  }

  parseCmp() {
    let left = this.parseCons();
    while (this.at("LT") || this.at("LEQ")) {
      const op = this.current();
      this.pos++;
      const right = this.parseCons();
      left = node(op.kind === "LT" ? "Lt" : "Leq", { left, right }, left.from, right.to);
    }
    return left;
  }

  parseCons() {
    const head = this.parseAdd();
    if (this.maybe("CONS")) {
      const tail = this.parseCons();
      return node("Cons", { head, tail }, head.from, tail.to);
    }
    return head;
  }

  parseAdd() {
    let left = this.parseMul();
    while (this.at("PLUS") || this.at("MINUS")) {
      const op = this.current();
      this.pos++;
      const right = this.parseMul();
      left = node(op.kind === "PLUS" ? "Add" : "Sub", { left, right }, left.from, right.to);
    }
    return left;
  }

  parseMul() {
    let left = this.parseUnary();
    while (this.at("TIMES") || this.at("DIVIDE")) {
      const op = this.current();
      this.pos++;
      const right = this.parseUnary();
      left = node(op.kind === "TIMES" ? "Mul" : "Div", { left, right }, left.from, right.to);
    }
    return left;
  }

  parseUnary() {
    if (this.at("MINUS")) {
      const start = this.take("MINUS").from;
      const expr = this.parseUnary();
      return node("Neg", { expr }, start, expr.to);
    }
    return this.parseApp();
  }

  parseApp() {
    let fn = this.parseAtom();
    while (this.startsAtom()) {
      const arg = this.parseAtom();
      fn = node("App", { fn, arg }, fn.from, arg.to);
    }
    return fn;
  }

  startsAtom() {
    return [
      "IDENT",
      "FLOAT",
      "TRUE",
      "FALSE",
      "LBRACK",
      "LPAREN",
      "FST",
      "SND",
      "INL",
      "INR",
      "OBSERVE",
      ...Object.keys(distTokenToKind),
    ].includes(this.current().kind);
  }

  parseAtom() {
    const tok = this.current();
    switch (tok.kind) {
      case "IDENT":
        this.pos++;
        return node("Var", { name: tok.value }, tok.from, tok.to);
      case "FLOAT":
        this.pos++;
        return node("Const", { value: tok.value }, tok.from, tok.to);
      case "TRUE":
      case "FALSE":
        this.pos++;
        return node("Bool", { value: tok.kind === "TRUE" }, tok.from, tok.to);
      case "LBRACK": {
        const start = this.take("LBRACK").from;
        const end = this.take("RBRACK").to;
        return node("Nil", {}, start, end);
      }
      case "LPAREN":
        return this.parseParen();
      case "FST":
      case "SND":
      case "INL":
      case "INR":
        return this.parseUnaryKeyword();
      case "OBSERVE":
        return this.parseObserve();
      default:
        if (tok.kind in distTokenToKind) return this.parseDistribution();
        if (tok.kind === "EOF") {
          throw new CompileError("expected expression before end of input", tok.from, tok.to);
        }
        throw new CompileError(`expected expression, found ${tokenLabel(tok.kind)}`, tok.from, tok.to);
    }
  }

  parseParen() {
    const start = this.take("LPAREN").from;
    if (this.at("RPAREN")) {
      const end = this.take("RPAREN").to;
      return node("Unit", {}, start, end);
    }
    const first = this.parseExpr();
    if (this.maybe("COMMA")) {
      const second = this.parseExpr();
      const end = this.take("RPAREN").to;
      return node("Pair", { left: first, right: second }, start, end);
    }
    this.take("RPAREN");
    return { ...first, from: start, to: this.tokens[this.pos - 1].to };
  }

  parseUnaryKeyword() {
    const keyword = this.current();
    this.pos++;
    const expr = this.parseAtom();
    const map = { FST: "Fst", SND: "Snd", INL: "Inl", INR: "Inr" };
    return node(map[keyword.kind], { expr }, keyword.from, expr.to);
  }

  parseObserve() {
    const start = this.take("OBSERVE").from;
    this.take("LPAREN");
    const cond = this.parseExpr();
    const end = this.take("RPAREN").to;
    return node("Observe", { cond }, start, end);
  }

  parseDistribution() {
    const nameTok = this.current();
    this.pos++;
    const kind = distTokenToKind[nameTok.kind];
    const name = distKindToName[kind];
    let mode = null;
    if (this.at("LBRACK")) {
      this.take("LBRACK");
      const modeTok = this.take("IDENT");
      if (modeTok.value !== "E" && modeTok.value !== "G") {
        throw new CompileError("expected distribution mode `E` or `G`", modeTok.from, modeTok.to);
      }
      mode = modeTok.value;
      this.take("RBRACK");
    }
    this.take("LPAREN");
    const args = [];
    if (kind === "Discrete") {
      const first = this.take("FLOAT");
      args.push(first.value);
      while (this.maybe("COMMA")) args.push(this.take("FLOAT").value);
      const end = this.take("RPAREN").to;
      const choices = args.map((p, i) => ({ probability: p, value: node("Const", { value: i }, nameTok.from, end) }));
      return node("Discrete", { mode, choices, displayName: name }, nameTok.from, end);
    }

    if (kind === "Exponential" || kind === "Flip" || kind === "Bernoulli" || kind === "Poisson") {
      args.push(this.parseExpr());
    } else {
      args.push(this.parseExpr());
      this.take("COMMA");
      args.push(this.parseExpr());
    }
    const end = this.take("RPAREN").to;
    return node(kind, { mode, args, displayName: name }, nameTok.from, end);
  }
}

export function parse(source) {
  return new Parser(source).parseMain();
}

export { distributions };
