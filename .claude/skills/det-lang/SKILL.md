---
name: det-lang
description: Reference for the .det probabilistic language implemented in this repo (syntax, mode system G/E, determinization rules, CLI output format) and the procedure for adding or changing example programs in det/ and examples/. Use when writing, reading, or debugging .det programs, interpreting .dout output, adding a distribution or construct, or explaining what "determinize" computes.
paths:
  - "det/**"
  - "examples/**"
  - "**/*.det"
  - "**/*.dout"
---

## The language (as accepted by `ocaml/parser.mly`)
- Functions: `fun x => e`, `rec f x => e`, application by juxtaposition; `let x = e in e`; `if c then e else e`.
- Data: floats, `true`/`false`, pairs `(a, b)` with `fst`/`snd`, sums `inl e`/`inr e` with `match e with inl x => e | inr y => e`, lists `[]`, `x :: xs`, `match e with [] => e | x::xs => e`.
- Operators: `+ - * /`, unary `-`, `<`, `<=`. Comments `(* ... *)`. `\`/`lambda` are synonyms for `fun`.
- Effects: `observe(e)` (conditioning; rejected trials are skipped in evaluation), distributions `uniform(a,b)`, `gauss(mu,var)`, `exponential(r)`, `gamma(a,b)`, `beta(a,b)`, `flip(p)`, `bernoulli(p)`, `poisson(l)`, `discrete(p1,...,pn)` (literal probabilities only; values are `0..n-1`).
- Not supported / open (see `TODO.md`): a principled expectation rule for subtraction and division, the `discrete` branching.

## Modes and determinization
- Every float has a mode: `float[G]` (a genuine sample must be drawn) or `float[E]` (the value may be replaced by its expectation). `G <= E` is the submode order; subtyping is contravariant on arrows. Unresolved mode metas print as `float[?mN]` and default to `E`.
- Constructs that force `G`: comparison operands (`<`, `<=`), both operands of a non-scaling `*`/`/` (multiplying by a literal constant is "scaling" and keeps the context mode), `gauss` variance, `exponential` rate, `gamma` rate, `beta` parameters.
- The transform replaces E-moded draws by their means: `uniform(a,b) -> (a+b)*0.5`, `gauss(m,v) -> m`, `exponential(r) -> 1/r`, `gamma(a,b) -> a/b`, `beta(a,b) -> a/(a+b)`, `bernoulli(p)/poisson(p) -> p`, `discrete -> sum p_i * i`. `flip` is never determinized.

## Running
- One file: `./run.sh det/FILE.det` (from the repo root; builds first). All: `./det.sh`. Storm model checking: `./run.sh --storm [--limit N] FILE.det`, see the `storm` skill.
- Output `FILE.det.dout` (also on stdout) has three sections: `== Elaboration ==` (typed AST before and after mode defaulting), `== Determinized ==`, `== Evaluation (100 trials) ==` with `program mean` vs `determinized mean`. Runs are reproducible (`Random.init 0`). The two means should agree up to sampling noise when the analysis is sound; a large gap on a new example is a finding worth reporting.
- `.dout` is a report, not re-parseable input (pairs print as `<a, b>`, typed output carries `x : T` ascriptions).

## Adding an example
1. Write `det/NAME.det` (keep it small and focused on one feature); run `./run.sh det/NAME.det` and read the `.dout`.
2. Commit the `.det` together with its `.dout` (generated, never hand-edited; the protect hook blocks edits).
3. If it should appear in the browser simulator, add it to `sim/src/examples.js` (the sim test suite executes every entry) and rebuild the bundle.
4. `examples/` holds larger benchmark programs (`loops/`, `paper/ex1-6` are the paper's running examples, `symbolic/`, `baselines/*.sgcl` are reference encodings in another tool's language and are not parsed by anything here).
