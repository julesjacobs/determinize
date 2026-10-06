// A port of the tokenizer of Lean's `Frontend/Parser.lean`, with source spans.
import type { Span } from "./ast.ts";
import { CompileError } from "./errors.ts";

/** A token: a word (identifier or keyword), a decimal number, a symbol, or the end of input. */
export type Token = Span & { kind: "word" | "number" | "symbol" | "end"; text: string };

const isDigit = (c: string) => c >= "0" && c <= "9";
const isAlpha = (c: string) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z");
export const identStart = (c: string) => isAlpha(c) || c === "_";
const identRest = (c: string) => identStart(c) || isDigit(c) || c === "'";
/** Lean's `Char.isWhitespace`. */
const isWhitespace = (c: string) => c === " " || c === "\t" || c === "\r" || c === "\n";
const symbols = "()[],|=+-*/<>\\";

/** The end of the comment that opens at `start`; comments nest. */
function commentEnd(source: string, start: number): number {
  let depth = 1;
  let i = start + 2;
  while (i < source.length) {
    if (source.startsWith("(*", i)) {
      depth += 1;
      i += 2;
    } else if (source.startsWith("*)", i)) {
      i += 2;
      depth -= 1;
      if (depth === 0) return i;
    } else {
      i += 1;
    }
  }
  throw new CompileError("unterminated comment; expected `*)`", start, source.length);
}

/** `(*` after `discrete` or `discrete[m]` opens the remainder form `discrete(*)`. */
function discreteArguments(tokens: Token[]): boolean {
  const back = (k: number) => tokens[tokens.length - 1 - k]?.text;
  return back(0) === "discrete" || (back(0) === "]" && back(2) === "[" && back(3) === "discrete");
}

export function lex(source: string): Token[] {
  const tokens: Token[] = [];
  const push = (kind: Token["kind"], from: number, to: number) =>
    tokens.push({ kind, text: source.slice(from, to), from, to });
  let i = 0;
  while (i < source.length) {
    const c = source[i];
    if (source.startsWith("(*", i)) {
      if (discreteArguments(tokens)) {
        let close = i + 2;
        while (close < source.length && isWhitespace(source[close])) close++;
        if (source[close] === ")") {
          push("symbol", i, i + 1);
          push("symbol", i + 1, i + 2);
          push("symbol", close, close + 1);
          i = close + 1;
          continue;
        }
      }
      i = commentEnd(source, i);
      continue;
    }
    if (isWhitespace(c)) {
      i++;
      continue;
    }
    if (identStart(c)) {
      let end = i + 1;
      while (end < source.length && identRest(source[end])) end++;
      push("word", i, end);
      i = end;
      continue;
    }
    if (isDigit(c)) {
      let end = i;
      while (end < source.length && isDigit(source[end])) end++;
      if (source[end] === ".") {
        end++;
        while (end < source.length && isDigit(source[end])) end++;
      }
      if (source[end] === "e" || source[end] === "E") {
        let digits = end + 1;
        if (source[digits] === "+" || source[digits] === "-") digits++;
        let exponentEnd = digits;
        while (exponentEnd < source.length && isDigit(source[exponentEnd])) exponentEnd++;
        if (exponentEnd === digits) {
          throw new CompileError("missing decimal exponent", i, exponentEnd);
        }
        end = exponentEnd;
      }
      push("number", i, end);
      i = end;
      continue;
    }
    const pair = source.slice(i, i + 2);
    if (pair === "=>" || pair === "::" || pair === "<=") {
      push("symbol", i, i + 2);
      i += 2;
      continue;
    }
    if (symbols.includes(c)) {
      push("symbol", i, i + 1);
      i++;
      continue;
    }
    const char = String.fromCodePoint(source.codePointAt(i) ?? 0);
    throw new CompileError(`unexpected character \`${char}\``, i, i + char.length);
  }
  tokens.push({ kind: "end", text: "", from: source.length, to: source.length });
  return tokens;
}
