---
name: storm
description: How to model-check a .det program with the Storm probabilistic model checker via ./run.sh --storm, what the generated explicit-format files (.tra, .lab, .state.rew) contain, how to write properties, and how to obtain Storm (it is not in nixpkgs). Use when asked about expected values via model checking, Storm errors, the .tra/.lab/.state.rew files, or when run.sh --storm fails.
paths:
  - "**/*.tra"
  - "**/*.lab"
  - "**/*.state.rew"
  - "ocaml/to_mc.ml"
  - "run.sh"
---

## Pipeline
`./run.sh --storm [--limit N] FILE.det` builds the OCaml tool, writes `FILE.det.tra/.lab/.state.rew` next to the input (explicit-state DTMC exploration in `ocaml/to_mc.ml`), then runs
```
storm --explicit FILE.det.tra FILE.det.lab --staterew FILE.det.state.rew --prop 'R=? [ F "done" || F "accept" ]'
```
whose `Result (for initial states): X` is the expected value of the program.

## Constraints of the exporter
- Only discrete programs: `to_mc` raises on `uniform`/`gauss`. Determinize first so continuous E-draws become constants, or use `flip`/`bernoulli`/`discrete` programs.
- State 0 is `init`; constant-valued states are labelled `accept`; sink states `done`. With `--limit N`, states `N` and `N+1` are the "done" and "diverging" sinks, so cut-off states are distinguishable. `.state.rew` maps a state to its numeric value (`true`=1, `false`=0), so the expected reward equals the expected program value.
- States are keyed by `Marshal.to_string` of the AST (closed terms only). Some checked-in `.lab` files predate the `accept` label and are stale.

## Explicit format (Storm docs: stormchecker.org/documentation/background/languages.html)
- `.tra`: first line `dtmc`, then `from to probability`, rows sorted by source state, no comments, every state needs an outgoing transition.
- `.lab`: `#DECLARATION` / space-separated label names / `#END`, then `state label` lines with strictly increasing states.
- `.state.rew`: `state reward` lines, increasing states, non-negative rewards, omitted states = 0. Explicit mode has exactly one unnamed reward model: write `R=? [...]`, never `R{"name"}=?`.
- Properties are PRISM syntax with quoted labels: `P=? [ F "done" ]`, `R=? [ F "done" ]` (expected accumulated reward until the label; infinite if it is not reached almost surely), `R=? [ F "a" || F "b" ]`, bounded `F<=k`. Flags: `--exact` (rational arithmetic), `--precision 1e-8`, `--sound`, `--timemem`, `--engine sparse` (default).

## Getting Storm
Not in nixpkgs (`pkgs.storm` is Apache Storm) and not in the devshells. Check with `command -v storm && storm --version`.
- macOS: `brew tap moves-rwth/storm && brew install stormchecker`.
- Any OS with Docker: `docker run --rm -v "$PWD":/data -w /data movesrwth/storm:stable storm --explicit ...` (image `movesrwth/storm`, tags `stable`, `x.y.z`).
- From source: github.com/stormchecker/storm (CMake; boost, gmp, ginac, glpk, hwloc, xerces-c, z3).
If Storm is absent, still run `./run.sh --storm FILE.det` up to the file generation (it fails only at the `storm` call) and tell the user the files are ready.
