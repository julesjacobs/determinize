// Quotes the conclusion of each theorem that the landing page cites, as lean/Determinize/
// Theorems.lean states it, in the page's <code data-theorem="NAME"> elements.
//
//   node site/theorems.mts           writes the quotes into site/index.html
//   node site/theorems.mts --check   fails unless every quote equals the statement in Lean
import { readFileSync, writeFileSync } from "node:fs";

const page = new URL("index.html", import.meta.url);
const lean = readFileSync(new URL("../lean/Determinize/Theorems.lean", import.meta.url), "utf8");

const closing: Record<string, string> = { "(": ")", "[": "]", "{": "}", "⦃": "⦄" };

/**
 * The conclusion of `theorem name`: the text between the colon that ends its binders and the
 * `:=` before its proof, both outside brackets, without the indentation its lines share.
 */
export function conclusion(source: string, name: string): string {
  const header = new RegExp(`^theorem ${name}\\b`, "m").exec(source);
  if (!header) throw new Error(`Theorems.lean has no theorem ${name}`);
  const stack: string[] = [];
  let start = -1;
  for (let i = header.index + header[0].length; i < source.length; i++) {
    const c = source[i];
    if (c in closing) stack.push(closing[c]);
    else if (c === stack.at(-1)) stack.pop();
    else if (stack.length === 0 && c === ":") {
      if (source[i + 1] === "=") {
        if (start < 0) break;
        const lines = source.slice(start, i).replace(/^ *\n/, "").trimEnd().split("\n");
        const indent = Math.min(...lines.map((line) => line.length - line.trimStart().length));
        return lines.map((line) => line.slice(indent)).join("\n");
      }
      if (start < 0) start = i + 1;
    }
  }
  throw new Error(`could not find the conclusion of ${name}`);
}

const escapeHtml = (text: string) =>
  text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");

const quote = /(<code data-theorem="(\w+)">)([^<]*)(<\/code>)/g;
const html = readFileSync(page, "utf8");
const names = [...html.matchAll(quote)].map((match) => match[2]);
if (names.length === 0) throw new Error("index.html quotes no theorem");

if (process.argv.includes("--check")) {
  const stale = [...html.matchAll(quote)].filter(
    ([, , name, text]) => text !== escapeHtml(conclusion(lean, name)),
  );
  for (const [, , name] of stale) {
    console.error(`site/index.html quotes ${name} differently from Theorems.lean.`);
  }
  if (stale.length > 0) {
    console.error("Run 'node site/theorems.mts' to quote the statements in Lean.");
    process.exit(1);
  }
  console.log(`The landing page quotes ${names.join(", ")} as Theorems.lean states them.`);
} else {
  writeFileSync(
    page,
    html.replace(
      quote,
      (_, open, name, _text, close) => open + escapeHtml(conclusion(lean, name)) + close,
    ),
  );
  console.log(`Quoted ${names.join(", ")} in site/index.html.`);
}
