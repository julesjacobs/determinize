// Builds the simulator into dist/: the minified bundles app.js and worker.js with their source
// maps, and copies of index.html and styles.css. With --no-minify, the bundles are readable and
// have no source maps. dist/index.html refers to each asset, and app.js to the worker, with ?v=
// and the first 10 hex digits of the file's SHA-256, so a browser never combines a cached copy
// with a newer one.
import { createHash } from "node:crypto";
import { copyFile, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { parseArgs } from "node:util";
import type { BuildOptions } from "esbuild";
import { build } from "esbuild";

const { values: options } = parseArgs({
  options: { minify: { type: "boolean", default: true } },
  allowNegative: true,
});
const outdir = "dist";

await rm(outdir, { recursive: true, force: true });
await mkdir(outdir);

const bundle: BuildOptions = {
  bundle: true,
  format: "iife",
  // The examples are the programs in ../examples/, imported as text.
  loader: { ".det": "text" },
  minify: options.minify,
  sourcemap: options.minify && "linked",
  // node_modules holds links into the Nix store. Resolving packages through the links keeps the
  // paths in the bundle's comments and source map independent of the store and the checkout.
  preserveSymlinks: true,
  logLevel: "warning",
};

async function stamp(file: string) {
  const hash = createHash("sha256")
    .update(await readFile(`${outdir}/${file}`))
    .digest("hex");
  return `./${file}?v=${hash.slice(0, 10)}`;
}

await build({ ...bundle, entryPoints: ["src/worker.ts"], outfile: `${outdir}/worker.js` });
await build({
  ...bundle,
  entryPoints: ["src/main.ts"],
  globalName: "DeterminizeSim",
  outfile: `${outdir}/app.js`,
  define: { WORKER_URL: JSON.stringify(await stamp("worker.js")) },
});
await copyFile("styles.css", `${outdir}/styles.css`);

let html = await readFile("index.html", "utf8");
for (const file of ["app.js", "styles.css"]) {
  const reference = new RegExp(`"\\./${file.replace(".", "\\.")}(\\?v=[^"]*)?"`, "g");
  if (html.match(reference)?.length !== 1) {
    throw new Error(`index.html must refer to ./${file} exactly once`);
  }
  html = html.replace(reference, `"${await stamp(file)}"`);
}
await writeFile(`${outdir}/index.html`, html);
