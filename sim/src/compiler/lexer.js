import { CompileError } from "./errors.js";

const keywords = new Map([
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

const punct = [
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

export function lex(source) {
  const tokens = [];
  let i = 0;

  const push = (kind, value, from, to) => tokens.push({ kind, value, from, to });

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
      if (i >= source.length) throw new CompileError("unterminated comment; expected `*)`", start, source.length);
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
