// Imports of .det files give their text in the tests, as esbuild's text loader does in the bundle.
// `npm test` loads this module with --import.
import { readFileSync } from "node:fs";
import { registerHooks } from "node:module";

registerHooks({
  load(url, context, nextLoad) {
    if (!url.endsWith(".det")) return nextLoad(url, context);
    const text = readFileSync(new URL(url), "utf8");
    return {
      format: "module",
      source: `export default ${JSON.stringify(text)};`,
      shortCircuit: true,
    };
  },
});
