// Builds the simulator into dist/: the minified bundle app.js with its source map app.js.map, and
// copies of index.html and styles.css. With --no-minify, app.js is readable and has no source map.
// dist/index.html refers to each asset with ?v= and the first 10 hex digits of the file's SHA-256,
// so a browser never combines a cached copy with a newer one.
import { createHash } from "node:crypto";
import { copyFile, mkdir, readFile, rm, writeFile } from "node:fs/promises";
import { parseArgs } from "node:util";
import { build } from "esbuild";

const { values: options } = parseArgs({
  options: { minify: { type: "boolean", default: true } },
  allowNegative: true,
});
const outdir = "dist";

await rm(outdir, { recursive: true, force: true });
await mkdir(outdir);

await build({
  entryPoints: ["src/main.js"],
  bundle: true,
  format: "iife",
  globalName: "DeterminizeSim",
  outfile: `${outdir}/app.js`,
  minify: options.minify,
  sourcemap: options.minify && "linked",
  logLevel: "warning",
});
await copyFile("styles.css", `${outdir}/styles.css`);

async function stamp(file) {
  const hash = createHash("sha256")
    .update(await readFile(`${outdir}/${file}`))
    .digest("hex");
  return `./${file}?v=${hash.slice(0, 10)}`;
}

let html = await readFile("index.html", "utf8");
for (const file of ["app.js", "styles.css"]) {
  const reference = new RegExp(`"\\./${file.replace(".", "\\.")}(\\?v=[^"]*)?"`, "g");
  if (html.match(reference)?.length !== 1) {
    throw new Error(`index.html must refer to ./${file} exactly once`);
  }
  html = html.replace(reference, `"${await stamp(file)}"`);
}
await writeFile(`${outdir}/index.html`, html);
