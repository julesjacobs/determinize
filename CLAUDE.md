# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

@AGENTS.md

## What this repo is
Research project on *determinizing* probabilistic programs: a small language (`.det`) with a mode system that marks each float `G` (keep sampling) or `E` (replace by its expectation), a transformation that rewrites `E`-moded draws to their means, and a soundness argument. Three artifacts must stay in sync:

| Directory | Artifact | Toolchain (Nix devshell) |
|---|---|---|
| `ocaml/` | reference implementation + CLI `determinize_main` (lexer/parser -> `infer` -> `determinize` -> `interp` / `to_mc` Storm export) | `.#ocaml`: dune 3.23, menhir, ocamllex, ocaml-lsp (loaded by the root `.envrc`) |
| `sim/` | browser simulator, a hand-written JS port of the compiler plus a coupled-trace runtime, CodeMirror UI | `.#sim`: node 24, esbuild, `node --test` (`sim/.envrc`) |
| `tex/` | the paper (acmart, PACMPL style) | `.#tex`: texliveMedium + latexmk + chktex (`tex/.envrc`) |
| `det/`, `examples/` | `.det` programs with generated `.dout` golden outputs (and Storm `.tra/.lab/.state.rew`) | uses `ocaml/` via `./det.sh`, `./run.sh` |
| `flake.nix`, `flake-modules/` | flake-parts, dendritic layout (every `.nix` under `flake-modules/` is auto-imported) | Nix 2.34, direnv + nix-direnv |

Per-directory details, conventions, and pitfalls load automatically from `.claude/rules/{ocaml,sim,tex,nix}.md` when you touch those files. Language reference: the `det-lang` skill.

## Environment: tools live in Nix devshells
Only the `.#ocaml` shell is on PATH in a normal session (`dune` works; `node`, `npm`, `latexmk`, `chktex` do not). Run other toolchains through direnv's cached shells or `nix develop`:
```
cd sim && direnv exec . npm test          # or: nix develop .#sim --command npm test
cd tex && direnv exec . latexmk -pdf main.tex   # or: nix develop .#tex --command latexmk -pdf main.tex
```
Nix only sees git-tracked files: `git add` (or `git add -N`) new files before any `nix`/`direnv reload`. `.envrc` files are ignored by the owner's global gitignore and need `git add -f`. Storm (model checker) is not in nixpkgs and may be absent; `./run.sh --storm` then fails only at the final `storm` call.

## Commands
| Task | Command |
|---|---|
| Build OCaml | `cd ocaml && dune build` (dev profile: warnings are errors; fix them, never silence) |
| Run one program | `./run.sh det/FILE.det` (writes `det/FILE.det.dout`); all: `./det.sh` |
| Golden test | `./det.sh && git diff det/` (an unexpected `.dout` diff is a regression) |
| Model check | `./run.sh --storm [--limit N] FILE.det` |
| Sim tests | `cd sim && direnv exec . npm test`; one file `node --test test/semantics.test.js`; one test `node --test --test-name-pattern="gamma"` |
| Sim bundle | `cd sim && direnv exec . npm run build` (regenerates the committed `app.bundle.js`; bump `?v=` in `index.html`) |
| Paper | `cd tex && direnv exec . latexmk -pdf -interaction=nonstopmode -file-line-error main.tex`; lint `chktex FILE.tex` (flags in `.claude/rules/tex.md`) |
| Everything | `/check-all` (= `.claude/scripts/check.sh --all`) |
| Flake | `nix flake show`, `nix flake check` |

## Workflow rules
- **Verification is automatic.** Editing an OCaml file runs `dune build`; editing `sim/src` or `sim/test` runs the sim tests; editing `.tex` runs chktex. A Stop hook re-runs the build/tests/latexmk for every area with uncommitted changes and blocks finishing on failure. Treat hook output as feedback to fix, not noise. Run `/check-all` before saying a task is done.
- **Generated files are never hand-edited**: `*.dout`, `*.tra`, `*.lab`, `*.state.rew`, `sim/app.bundle.js`, lockfiles, `tex/acmart.cls`, `tex/ACM-Reference-Format.bst`. Regenerate them with the commands above (a hook blocks direct edits).
- **Change semantics in three places.** A typing/mode/determinization change touches `ocaml/`, `sim/src/compiler`, and the paper. Use the `sync-sim` skill for the port and the `spec-impl-checker` subagent to confirm agreement; the `paper-reviewer` subagent for TeX-side review.
- **Do not reformat.** The OCaml code is not ocamlformat-formatted and `sim/` has no formatter; match surrounding style. Whole-file reformatting is blocked by a hook.
- **Outward-facing actions are the user's**: never run `sim/deploy-to-website.sh` (pushes to another repo); `git push` and `nix flake update` ask first; commit only when asked, with the regenerated artifacts included in the same commit.
- **TODO.md** is the task list: mark items `[x]` when you complete them (AGENTS.md); do not add speculative items.
- **Current, canonical, best practice only.** Whatever you do here, do it the way the tool's maintainers recommend today; when unsure, verify against primary sources (official docs, maintainers' repos) rather than memory. Third-party skills/plugins may be adopted when they help, but only after `/vet-skill` clears them for prompt-injection / poisoning, preferring Anthropic's official marketplace.
- **New tool or dependency?** Run `/learn-tool <name>` first: it researches current best practices from primary sources and records them as a rule in `.claude/rules/`. The post-edit hook reminds you when `flake-modules/`, `package.json`, or `ocaml/dune` change.
- **Notifications**: the Stop/Notification hooks handle the audible alert AGENTS.md asks for (macOS `say`, Linux `notify-send`, else a bell). Do not call `say` yourself.

## Non-obvious facts worth knowing up front
- `ocaml/symbolic_coupling.ml` is deliberately outside the dune `(modules ...)` list: it is a standalone prototype that dune never compiles.
- `.dout` files are reports, not re-parseable programs (pairs print as `<a, b>`, typed output has `x : T` ascriptions). Evaluation uses 100 trials with `Random.init 0`, so outputs are deterministic.
- `to_mc.ml` handles only discrete programs (`flip`, `bernoulli`, `discrete`); `uniform`/`gauss` raise. Determinize first.
- `sim/src/examples.js` is a hand-maintained copy of example programs; `sim/test/semantics.test.js` executes every entry.
- `tex/8_old.tex` is dead; `tex/fig_symbolic_coupling.svg` is not included anywhere; `\nocite{*}` is still in `main.tex`.
- `examples/baselines/*.sgcl` are reference encodings in another tool's input language; nothing here parses them.
