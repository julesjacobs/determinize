import { StreamLanguage, HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { tags as t } from "@lezer/highlight";

const keywords = new Set(["let", "in", "if", "then", "else", "match", "with", "fun", "lambda", "rec", "true", "false"]);
const constructors = new Set(["inl", "inr", "fst", "snd", "observe"]);
const distributions = new Set(["uniform", "gauss", "exponential", "gamma", "beta", "flip", "bernoulli", "poisson", "discrete"]);

export const detLanguage = StreamLanguage.define({
  token(stream) {
    if (stream.eatSpace()) return null;

    if (stream.match("(*")) {
      while (!stream.eol()) {
        if (stream.match("*)")) break;
        stream.next();
      }
      return "comment";
    }

    if (stream.match(/^[0-9]+(?:\.[0-9]*)?(?:[eE][+-]?[0-9]+)?/)) return "number";
    if (stream.match(/^[A-Za-z_][A-Za-z0-9_]*/)) {
      const word = stream.current();
      if (keywords.has(word)) return "keyword";
      if (distributions.has(word)) return "variableName.special";
      if (constructors.has(word)) return "atom";
      if (word === "E" || word === "G") return "labelName";
      return "variableName";
    }
    if (stream.match("=>") || stream.match("<=") || stream.match("::")) return "operator";
    if (stream.match(/[+\-*/=<|]/)) return "operator";
    stream.next();
    return "punctuation";
  },
  languageData: {
    commentTokens: { block: { open: "(*", close: "*)" } },
  },
});

export const detHighlighting = syntaxHighlighting(
  HighlightStyle.define([
    { tag: t.keyword, color: "#8a3ffc", fontWeight: "700" },
    { tag: t.number, color: "#b54708" },
    { tag: t.comment, color: "#667085", fontStyle: "italic" },
    { tag: t.operator, color: "#344054" },
    { tag: t.atom, color: "#0f766e", fontWeight: "650" },
    { tag: t.special(t.variableName), color: "#1d4ed8", fontWeight: "700" },
    { tag: t.labelName, color: "#15803d", fontWeight: "800" },
  ]),
);
