---
name: check-all
description: Run the full verification for this repo (dune build, sim tests, bundle freshness, LaTeX build with error/undefined-reference triage, optionally regenerate det/ golden outputs) and summarize failures. Use before declaring a task done, after touching several areas, or when asked to "run everything" / "check that it still builds". Also invoked as /check-all [areas].
argument-hint: "[ocaml|sim|bundle|tex|det ...] (default: --all)"
allowed-tools: Bash, Read, Grep
---

Run the repository checks and act on the result.

Command: `.claude/scripts/check.sh $ARGUMENTS` (use `--all` when no argument is given; `--changed` selects areas from `git status`).

Areas: `ocaml` (dune build), `sim` (npm test), `bundle` (fails if `sim/src` changed but `sim/app.bundle.js` was not rebuilt), `tex` (latexmk; hard errors and undefined references fail, overfull boxes and multiply-defined labels are reported), `det` (runs `./det.sh`, which rewrites every `det/*.det.dout`, then lists the golden files that changed; not part of `--all` because it modifies tracked files).

The script runs each tool inside its Nix devshell via direnv or `nix develop`, so it works from a bare shell.

Afterwards:
- If something failed, fix the root cause and rerun the failing area only. Never silence a warning-as-error, skip a test, or build with a different profile to get green.
- If `det` reports changed golden outputs, inspect `git diff det/` and decide whether each change is intended (a semantics change) or a regression; say which in your summary.
- Report the final status per area in one line each, quoting exact error lines for anything still failing.
