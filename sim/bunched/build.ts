// Builds the simulator with the experimental bunched inference, presented as "Independence",
// into dist/independence/, beside the original in dist/. Run from sim/: `node bunched/build.ts`.
// It first runs `build.ts`, then rebuilds the page and the workers that analyze programs, with
// `analyze.ts`'s import of `./infer.ts` resolved to `bunched/infer.ts`, and adds a link to the
// new page to the built dist/index.html. No source file of the original changes; the new page
// shares dist/'s styles, fonts and chain worker.
import { createHash } from "node:crypto";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join, resolve } from "node:path";
import type { BuildOptions, Plugin } from "esbuild";
import { build } from "esbuild";

await import("../build.ts");

const outdir = "dist/independence";
await mkdir(outdir, { recursive: true });

const bunchedInfer: Plugin = {
  name: "bunched-infer",
  setup(b) {
    b.onResolve({ filter: /^\.\/infer\.ts$/ }, (args) =>
      args.importer.endsWith(join("compiler", "analyze.ts"))
        ? { path: resolve("bunched/infer.ts") }
        : undefined,
    );
  },
};

// The options of build.ts's bundles, with the plugin.
const bundle: BuildOptions = {
  bundle: true,
  format: "iife",
  loader: { ".det": "text" },
  minify: true,
  sourcemap: "linked",
  preserveSymlinks: true,
  logLevel: "warning",
  plugins: [bunchedInfer],
};

async function stamp(file: string) {
  const hash = createHash("sha256").update(await readFile(file)).digest("hex");
  return hash.slice(0, 10);
}

for (const worker of ["worker", "trace-worker", "exact-worker"]) {
  await build({ ...bundle, entryPoints: [`src/${worker}.ts`], outfile: `${outdir}/${worker}.js` });
}
const local = async (file: string) => `./${file}?v=${await stamp(`${outdir}/${file}`)}`;
await build({
  ...bundle,
  entryPoints: ["src/main.ts"],
  globalName: "DeterminizeSim",
  outfile: `${outdir}/app.js`,
  define: {
    WORKER_URL: JSON.stringify(await local("worker.js")),
    TRACE_WORKER_URL: JSON.stringify(await local("trace-worker.js")),
    EXACT_WORKER_URL: JSON.stringify(await local("exact-worker.js")),
    CHAIN_WORKER_URL: JSON.stringify(`../chain-worker.js?v=${await stamp("dist/chain-worker.js")}`),
  },
});

// dist/index.html, one level down: its own app.js, dist/'s styles, and a badge with a link back.
const label = "Independence";
const replacements: [RegExp, string][] = [
  [/"\.\/app\.js\?v=[^"]*"/, `"${await local("app.js")}"`],
  [/"\.\/styles\.css/, '"../styles.css'],
  [/<title>([^<]*)<\/title>/, `<title>$1 (${label})</title>`],
  [
    /<a class="home" href="\.\.\/">Project page<\/a>/,
    '<a class="home" href="../../">Project page</a>\n' +
      '        <a class="home" href="../">Original</a>',
  ],
  [
    /<h1>([^<]*)<\/h1>/,
    '<h1>$1 <span style="font-size: 0.6em; padding: 0.1em 0.4em; border: 1px solid; ' +
      `border-radius: 0.3em; vertical-align: middle">${label}</span></h1>`,
  ],
  [
    /(<section id="intro"[^>]*>\s*<p>[\s\S]*?<\/p>)/,
    "$1\n        <p>\n" +
      "          <strong>This is an experimental version.</strong> Modes are inferred by the " +
      "bunched type system instead, which tracks which draws each value depends on: a product " +
      "of independent factors keeps their draws E, and only draws that both factors share must " +
      "be G. It supports variables, literals, <code>let</code>, <code>if</code>, " +
      "<code>+</code>, <code>*</code>, <code>&lt;</code>, uniform, gaussian and flip.\n" +
      "        </p>",
  ],
];
function rewrite(html: string, edits: [RegExp, string][]) {
  for (const [pattern, replacement] of edits) {
    if (!pattern.test(html)) throw new Error(`dist/index.html does not match ${pattern}`);
    html = html.replace(pattern, replacement);
  }
  return html;
}
const original = await readFile("dist/index.html", "utf8");
await writeFile(`${outdir}/index.html`, rewrite(original, replacements));
// The original page, built by build.ts, gets a link to the new one.
const link = `<a class="home" href="./independence/">${label}</a>`;
await writeFile(
  "dist/index.html",
  rewrite(original, [
    [/(<a class="home" href="\.\.\/">Project page<\/a>)/, `$1\n        ${link}`],
  ]),
);
