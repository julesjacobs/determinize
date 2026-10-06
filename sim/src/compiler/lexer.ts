import type { OfKind, Span } from "./ast.ts";
import { CompileError } from "./errors.ts";

/** The kinds of tokens whose value is their text. */
export type TextTokenKind =
  | "IDENT"
  | "TRUE"
  | "FALSE"
  | "FUN"
  | "REC"
  | "LET"
  | "IN"
  | "IF"
  | "THEN"
  | "ELSE"
  | "MATCH"
  | "WITH"
  | "INL"
  | "INR"
  | "FST"
  | "SND"
  | "UNIFORM"
  | "GAUSS"
  | "EXPONENTIAL"
  | "GAMMA"
  | "BETA"
  | "FLIP"
  | "BERNOULLI"
  | "POISSON"
  | "DISCRETE"
  | "OBSERVE"
  | "DARROW"
  | "LEQ"
  | "CONS"
  | "LPAREN"
  | "RPAREN"
  | "LBRACK"
  | "RBRACK"
  | "LT"
  | "GT"
  | "COMMA"
  | "BAR"
  | "EQ"
  | "DOT"
  | "PLUS"
  | "TIMES"
  | "MINUS"
  | "DIVIDE";

export type Token = Span &
  (
    | { kind: TextTokenKind; value: string }
    | { kind: "FLOAT"; value: number }
    | { kind: "EOF"; value: null }
  );
export type TokenKind = Token["kind"];
export type TokenOf<K extends TokenKind> = OfKind<Token, K>;

const keywords = new Map<string, TextTokenKind>([
  ["true", "TRUE"],
  ["false", "FALSE"],
  ["fun", "FUN"],
  ["lambda", "FUN"],
  ["rec", "REC"],
  ["let", "LET"],
  ["in", "IN"],
  ["if", "IF"],
  ["then", "THEN"],
  ["else", "ELSE"],
  ["match", "MATCH"],
  ["with", "WITH"],
  ["inl", "INL"],
  ["inr", "INR"],
  ["fst", "FST"],
  ["snd", "SND"],
  ["uniform", "UNIFORM"],
  ["gauss", "GAUSS"],
  ["exponential", "EXPONENTIAL"],
  ["gamma", "GAMMA"],
  ["beta", "BETA"],
  ["flip", "FLIP"],
  ["bernoulli", "BERNOULLI"],
  ["poisson", "POISSON"],
  ["discrete", "DISCRETE"],
  ["observe", "OBSERVE"],
]);

const punct: [string, TextTokenKind][] = [
  ["=>", "DARROW"],
  ["<=", "LEQ"],
  ["::", "CONS"],
  ["(", "LPAREN"],
  [")", "RPAREN"],
  ["[", "LBRACK"],
  ["]", "RBRACK"],
  ["<", "LT"],
  [">", "GT"],
  [",", "COMMA"],
  ["|", "BAR"],
  ["=", "EQ"],
  [".", "DOT"],
  ["+", "PLUS"],
  ["*", "TIMES"],
  ["-", "MINUS"],
  ["/", "DIVIDE"],
  ["\\", "FUN"],
];

export function lex(source: string): Token[] {
  const tokens: Token[] = [];
  let i = 0;

  const push = (kind: TextTokenKind | "FLOAT", value: string | number, from: number, to: number) =>
    tokens.push({ kind, value, from, to } as Token);

  while (i < source.length) {
    const ch = source[i];

    if (/\s/.test(ch)) {
      i++;
      continue;
    }

    if (source.startsWith("(*", i)) {
      const start = i;
      i += 2;
      while (i < source.length && !source.startsWith("*)", i)) i++;
      if (i >= source.length)
        throw new CompileError("unterminated comment; expected `*)`", start, source.length);
      i += 2;
      continue;
    }

    const num = source.slice(i).match(/^[0-9]+(?:\.[0-9]*)?(?:[eE][+-]?[0-9]+)?/);
    if (num) {
      const text = num[0];
      push("FLOAT", Number(text), i, i + text.length);
      i += text.length;
      continue;
    }

    const ident = source.slice(i).match(/^[A-Za-z_][A-Za-z0-9_]*/);
    if (ident) {
      const text = ident[0];
      push(keywords.get(text) ?? "IDENT", text, i, i + text.length);
      i += text.length;
      continue;
    }

    const matched = punct.find(([text]) => source.startsWith(text, i));
    if (matched) {
      const [text, kind] = matched;
      push(kind, text, i, i + text.length);
      i += text.length;
      continue;
    }

    throw new CompileError(`unexpected character \`${ch}\``, i, i + 1);
  }

  tokens.push({ kind: "EOF", value: null, from: source.length, to: source.length });
  return tokens;
}
