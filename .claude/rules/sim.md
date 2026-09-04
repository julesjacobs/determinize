---
paths:
  - "sim/**"
---
# sim/ (browser simulator: ES modules, esbuild, CodeMirror 6, node --test)

Toolchain comes from the `.#sim` devshell (`sim/.envrc`); `node`/`npm` are NOT on the bare PATH.
Run commands as `cd sim && direnv exec . npm test` or `nix develop .#sim --command npm test`. Sources: nodejs.org/api/test,
esbuild.github.io/api, codemirror.net/docs, docs.npmjs.com, verified 2026-09.

## Commands (inside `sim/`)
- `npm test` = `node --test` (picks up `test/*.test.js`). One file: `node --test test/semantics.test.js`. One test: `node --test --test-name-pattern="gamma"`. Watch: `node --test --watch`.
- `npm run build` = esbuild IIFE bundle `src/main.js` -> `app.bundle.js` (global `DeterminizeSim`). Dev loop: add `--watch --servedir=. --sourcemap=inline` to the same command.
- `npm ci` (never `npm install`) to get exactly the locked dependency set; `esbuild` output only matches across machines when versions match.

## The bundle is committed
`app.bundle.js` is tracked and copied verbatim to the website by `deploy-to-website.sh`. After ANY change under `src/`: run `npm run build` and include the regenerated bundle in the same commit. The Stop hook flags a stale bundle; the `protect-generated` hook blocks hand edits to it. Never run the deploy script yourself (it pushes to another repository).
After changing `src/`, also bump the `?v=` cache-buster on the `<script>` tag in `index.html`.

## `src/compiler/*` is a hand port of `../ocaml`
`ast.js/types.js/infer.js/determinize.js/pretty.js/lexer.js/parser.js` mirror `ast.ml/types.ml/infer.ml/determinize.ml/pretty.ml/lexer.mll/parser.mly`
name-for-name (`fresh_mode_meta` -> `freshModeMeta`, etc.). There is no generator and no cross-check: a typing/mode/determinization change on one side must be
ported to the other. Use the `sync-sim` skill for the checklist. `src/runtime/semantics.js` plays the role of `interp.ml` (plus symbolic coupling); `to_mc.ml` has no JS counterpart.
`src/examples.js` holds copies of example programs as string literals (not imports of `../det`); `test/semantics.test.js` runs every entry, so a new example must analyze and run.

## Conventions
- ESM everywhere (`"type": "module"`); relative imports carry the `.js` extension.
- Double quotes, 2-space indent, alphabetized imports; no linter or formatter is configured, so keep the file's existing style.
- Tests: flat `test("name", ...)` with `node:assert/strict` (`assert.deepEqual`, `assert.throws`, `assert.match`). Put new tests next to the existing ones in `test/*.test.js`.
- `test/diagnostics.test.js` imports `@codemirror/state`, so tests need `node_modules` (run `npm ci` once).
- CodeMirror: the `.det` language is a `StreamLanguage` (token-level highlighting, `src/language.js`); diagnostics use a `StateField` (`src/diagnostics.js`). Keep DOM code in `main.js`; keep `compiler/` and `runtime/` DOM-free and pure so they stay testable under node.
- Do not change `package.json` dependencies casually: lockfile + bundle must be regenerated together.
