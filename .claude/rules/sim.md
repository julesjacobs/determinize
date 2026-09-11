---
paths:
  - "sim/**"
---
# sim/ (browser simulator: ES modules, esbuild, CodeMirror 6, node --test)

Toolchain comes from the `.#sim` devshell (`sim/.envrc`); use installed Node/npm directly when available.
Run commands as `cd sim && direnv exec . npm test` or `nix develop .#sim --command npm test`. Sources: nodejs.org/api/test,
esbuild.github.io/api, codemirror.net/docs, docs.npmjs.com, verified 2026-09.

## Commands (inside `sim/`)
- `npm test` = `node --test` (picks up `test/*.test.js`). One file: `node --test test/semantics.test.js`. One test: `node --test --test-name-pattern="gamma"`. Watch: `node --test --watch`.
- `npm run build` = esbuild IIFE bundle `src/main.js` -> `app.bundle.js` (global `DeterminizeSim`). Dev loop: add `--watch --servedir=. --sourcemap=inline` to the same command.
- `npm ci` (never `npm install`) to get exactly the locked dependency set; `esbuild` output only matches across machines when versions match.

## The bundle is committed
`app.bundle.js` is tracked and copied verbatim to the website by `deploy-to-website.sh`. After ANY change under `src/`: run `npm run build` and include the regenerated bundle in the same commit. The Stop hook flags a stale bundle; the `protect-generated` hook blocks hand edits to it. Never run the deploy script yourself (it pushes to another repository).
After changing `src/`, also bump the `?v=` cache-buster on the `<script>` tag in `index.html`.

## Relationship to Lean
`src/compiler/` and `src/runtime/` are a separate, unverified implementation.
Compare changes with `lean/Determinize/{Frontend,Checking,Statement,Runtime}` and
record differences using the `sync-sim` checklist. See `migration-audit.md` and
`lean/mul-div-typing.md`; the simulator is not automatically identical to Lean.
`src/examples.js` contains copied program strings. Tests execute every example;
`test/coupling-migration.test.js` also reads shared `.det` fixtures.

## Conventions
- ESM everywhere (`"type": "module"`); relative imports carry the `.js` extension.
- Double quotes, 2-space indent, alphabetized imports; no linter or formatter is configured, so keep the file's existing style.
- Tests: flat `test("name", ...)` with `node:assert/strict` (`assert.deepEqual`, `assert.throws`, `assert.match`). Put new tests next to the existing ones in `test/*.test.js`.
- `test/diagnostics.test.js` imports `@codemirror/state`, so tests need `node_modules` (run `npm ci` once).
- CodeMirror: the `.det` language is a `StreamLanguage` (token-level highlighting, `src/language.js`); diagnostics use a `StateField` (`src/diagnostics.js`). Keep DOM code in `main.js`; keep `compiler/` and `runtime/` DOM-free and pure so they stay testable under node.
- Do not change `package.json` dependencies casually: lockfile + bundle must be regenerated together.
