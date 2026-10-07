// Quotes the conclusion of each theorem that the landing page cites, as lean/Determinize/
// Theorems.lean states it, in the page's <code data-theorem="NAME"> elements. Each name in a
// quote that a documented module of the development declares links its declaration in the API
// documentation; names from Lean and Mathlib stay plain.
//
//   node site/theorems.mts           writes the quotes into site/index.html
//   node site/theorems.mts --check   fails unless every quote is what the first command writes
import { readFileSync, writeFileSync } from "node:fs";

const root = new URL("../", import.meta.url);
const page = new URL("index.html", import.meta.url);
const lean = readFileSync(new URL("lean/Determinize/Theorems.lean", root), "utf8");

const closing: Record<string, string> = { "(": ")", "[": "]", "{": "}", "⦃": "⦄" };

/**
 * The parts of `theorem name`: where it starts, its binders, and its conclusion, the text between
 * the colon that ends its binders and the `:=` before its proof, both outside brackets.
 */
function statement(source: string, name: string) {
  const header = new RegExp(`^theorem ${name}\\b`, "m").exec(source);
  if (!header) throw new Error(`Theorems.lean has no theorem ${name}`);
  const from = header.index + header[0].length;
  const stack: string[] = [];
  let colon = -1;
  for (let i = from; i < source.length; i++) {
    const c = source[i];
    if (c in closing) stack.push(closing[c]);
    else if (c === stack.at(-1)) stack.pop();
    else if (stack.length === 0 && c === ":") {
      if (source[i + 1] === "=") {
        if (colon < 0) break;
        const lines = source
          .slice(colon + 1, i)
          .replace(/^ *\n/, "")
          .trimEnd()
          .split("\n");
        const indent = Math.min(...lines.map((line) => line.length - line.trimStart().length));
        return {
          at: header.index,
          binders: source.slice(from, colon),
          conclusion: lines.map((line) => line.slice(indent)).join("\n"),
        };
      }
      if (colon < 0) colon = i;
    }
  }
  throw new Error(`could not find the conclusion of ${name}`);
}

/** A declaration's place in the documentation: its module and its full name. */
interface Declaration {
  module: string;
  name: string;
}

const declarationLine =
  /^\s*(?:@\[[^\]]*\]\s*)*((?:(?:noncomputable|private|protected|partial|unsafe)\s+)*)(?:def|abbrev|structure|inductive|class|theorem|lemma|opaque|instance)\s+([^\s({[:]+)/;

/** `source` without its comments, which Lean nests, and with its line breaks kept. */
function code(source: string) {
  let out = "";
  let depth = 0;
  for (let i = 0; i < source.length; i++) {
    const two = source.slice(i, i + 2);
    if (two === "/-") {
      depth++;
      i++;
    } else if (depth > 0 && two === "-/") {
      depth--;
      i++;
    } else if (depth > 0) {
      if (source[i] === "\n") out += "\n";
    } else if (two === "--") {
      while (i + 1 < source.length && source[i + 1] !== "\n") i++;
    } else if (source[i] === '"') {
      const close = source.slice(i + 1).search(/(?<!\\)"/);
      const to = close < 0 ? source.length - 1 : i + 1 + close;
      out += source.slice(i, to + 1);
      i = to;
    } else {
      out += source[i];
    }
  }
  return out;
}

const leanDirectory = new URL("lean/", root);

/**
 * The public declarations, by full name, of the modules whose pages the documentation has: those
 * that lean/Determinize.lean imports, directly or not, without Proof's, which no statement uses.
 */
function declarations() {
  const found = new Map<string, Declaration>();
  const pending = ["Determinize"];
  const seen = new Set<string>();
  for (let next = pending.pop(); next !== undefined; next = pending.pop()) {
    if (seen.has(next)) continue;
    seen.add(next);
    const module = next.replaceAll(".", "/");
    const source = code(readFileSync(new URL(`${module}.lean`, leanDirectory), "utf8"));
    for (const [, imported] of source.matchAll(/^import\s+(Determinize\.\S+)/gm)) {
      if (!imported.startsWith("Determinize.Proof.")) pending.push(imported);
    }
    // Each open block: a namespace's components, or none for a section or a mutual block.
    const blocks: string[][] = [];
    for (const line of source.split("\n")) {
      const opened = /^\s*(?:noncomputable\s+)?(namespace|section|mutual)\b\s*(\S*)/.exec(line);
      if (opened) blocks.push(opened[1] === "namespace" ? opened[2].split(".") : []);
      else if (/^\s*end\b/.test(line)) blocks.pop();
      const declared = declarationLine.exec(line);
      if (!declared || declared[1].includes("private")) continue;
      const name = declared[2].startsWith("_root_.")
        ? declared[2].slice("_root_.".length)
        : [...blocks.flat(), declared[2]].join(".");
      found.set(name, { module, name });
    }
  }
  return found;
}

const join = (a: string, b: string) => (a && b ? `${a}.${b}` : a || b);

/** The namespaces in which Lean looks up a name at offset `at` of `source`. */
function scopes(source: string, at: number) {
  const current: string[] = [];
  const opened: string[] = [];
  for (const line of source.slice(0, at).split("\n")) {
    const namespace = /^namespace\s+(\S+)/.exec(line);
    if (namespace) current.push(...namespace[1].split("."));
    const open = /^open\s+(?!scoped\b)(.*?)(\s+in)?$/.exec(line);
    if (open && !open[2]) opened.push(...open[1].split(/\s+/));
  }
  const prefixes = current.map((_, i) => current.slice(0, current.length - i).join("."));
  prefixes.push("");
  return [
    ...prefixes,
    ...opened.flatMap((namespace) => prefixes.map((prefix) => join(prefix, namespace))),
  ];
}

const known = declarations();
const escapeHtml = (text: string) =>
  text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
const identifier = /[A-Za-z_][\w']*(?:\.[A-Za-z_][\w']*)*/g;

/** The conclusion of `theorem name` as HTML, with links to the declarations it names. */
function quoted(name: string) {
  const { at, binders, conclusion } = statement(lean, name);
  const lookup = scopes(lean, at);
  // The declaration a name means, if exactly one of the development's fits.
  const resolve = (ident: string) => {
    const fits = new Set(lookup.map((scope) => known.get(join(scope, ident))));
    fits.delete(undefined);
    return fits.size === 1 ? [...fits][0] : undefined;
  };
  // The theorem's binders with their types, and every name its conclusion binds.
  const local = new Set<string>();
  const bind = (names: string) => {
    for (const bound of names.match(/[A-Za-z_][\w']*/g) ?? []) local.add(bound);
  };
  for (const [, names] of binders.matchAll(/[({[⦃]\s*([^(){}[\]⦃⦄:]+?)\s*:/g)) bind(names);
  const types = new Map<string, string>();
  for (const [, names, type] of binders.matchAll(/\(([^():]+):\s*([^()]+?)\)/g)) {
    for (const bound of names.trim().split(/\s+/)) types.set(bound, type.trim());
  }
  const binding = /(?:∀ᵐ|∀ᶠ|∀|∃!?|∑|∏|∫⁻?|fun|λ)\s+(.+?)\s*(?=[,:∈∂]|↦|=>)/g;
  for (const [, names] of conclusion.matchAll(binding)) bind(names);
  // The variables of match arms, `| pattern =>`; constructors are written with a leading dot.
  for (const [, pattern] of conclusion.matchAll(/\|([^|]*?)=>/g)) {
    bind(pattern.replace(/\.[A-Za-z_][\w']*/g, ""));
  }
  const link = (text: string, declaration: Declaration) =>
    `<a href="docs/${declaration.module}.html#${declaration.name}">${escapeHtml(text)}</a>`;
  let html = "";
  let last = 0;
  for (const match of conclusion.matchAll(identifier)) {
    const [token] = match;
    if (match.index > 0 && /[\w'.]/.test(conclusion[match.index - 1])) continue;
    html += escapeHtml(conclusion.slice(last, match.index));
    last = match.index + token.length;
    const [head, ...fields] = token.split(".");
    if (!local.has(head)) {
      const declaration = resolve(token);
      html += declaration ? link(token, declaration) : escapeHtml(token);
      continue;
    }
    // A field of a bound name, such as `program.determinize`, through the binder's type.
    const type = types.get(head);
    const declaration = type && fields.length === 1 ? resolve(`${type}.${fields[0]}`) : undefined;
    html += declaration ? `${escapeHtml(head)}.${link(fields[0], declaration)}` : escapeHtml(token);
  }
  return html + escapeHtml(conclusion.slice(last));
}

const quote = /(<code data-theorem="(\w+)">)([\s\S]*?)(<\/code>)/g;
const html = readFileSync(page, "utf8");
const names = [...html.matchAll(quote)].map((match) => match[2]);
if (names.length === 0) throw new Error("index.html quotes no theorem");

if (process.argv.includes("--check")) {
  const stale = [...html.matchAll(quote)].filter(([, , name, content]) => content !== quoted(name));
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
    html.replace(quote, (_, open, name, _content, close) => open + quoted(name) + close),
  );
  console.log(`Quoted ${names.join(", ")} in site/index.html.`);
}
