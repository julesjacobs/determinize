---
name: sync-sim
description: Port a change between the OCaml reference implementation (ocaml/) and its hand-written JavaScript mirror (sim/src/compiler, sim/src/runtime), or verify they still agree. Use whenever typing rules, mode inference, submoding/subtyping, determinization (expectation) rules, distributions, the parser/lexer, pretty-printing, or the .det syntax change on either side. The two are separate codebases with no generator, so every semantic change must be applied twice.
paths:
  - "ocaml/types.ml"
  - "ocaml/infer.ml"
  - "ocaml/determinize.ml"
  - "ocaml/interp.ml"
  - "ocaml/parser.mly"
  - "ocaml/lexer.mll"
  - "ocaml/pretty.ml"
  - "sim/src/compiler/**"
  - "sim/src/runtime/**"
---

Keep the OCaml implementation and the browser simulator semantically identical.

## Correspondence
| OCaml | JS | Notes |
|---|---|---|
| `ast.ml` | `sim/src/compiler/ast.js` | constructors as tagged objects |
| `types.ml` | `sim/src/compiler/types.js` | modes `G`/`E`, `fresh_mode_meta`->`freshModeMeta`, `submode`, `zonk`, `assert_subtype`, `default_modes_typ` |
| `infer.ml` | `sim/src/compiler/infer.js` | bidirectional inference, per-construct mode constraints |
| `determinize.ml` | `sim/src/compiler/determinize.js` | `default_modes` (unresolved metas -> `E`), the transform: E-moded draws -> means |
| `pretty.ml` | `sim/src/compiler/pretty.js` | doc combinators |
| `lexer.mll` / `parser.mly` | `sim/src/compiler/lexer.js` / `parser.js` | JS parser is recursive descent; keep precedence `cmp < cons < add < mul < unary < app` |
| `interp.ml` (+ `symbolic_coupling.ml`) | `sim/src/runtime/semantics.js`, `distributions.js`, `affine.js`, `rng.js` | JS additionally does symbolic/coupled traces |
| `to_mc.ml` | none | Storm export has no JS counterpart |
| `det/*.det` | `sim/src/examples.js` | examples are string copies, not imports |

## Procedure
1. Read the changed function on the source side and its counterpart on the target side (same name modulo snake/camel case).
2. Port the logic, keeping the target file's style (JS: double quotes, 2-space indent, `.js` import extensions; OCaml: match surrounding layout, dev-profile warnings are errors).
3. Update the language surface consistently: keywords/distributions in `lexer.mll`+`parser.mly` and `lexer.js`+`parser.js`, highlighting in `sim/src/language.js`, autocompletion in `sim/src/main.js`, TeX macros in `tex/macros.tex` if the paper names it.
4. Verify both sides: `cd ocaml && dune build && ../det.sh` then `git diff det/` (intended changes only), and `cd sim && direnv exec . npm test`. Add a `test/*.test.js` case for the new behaviour and, if it is user-visible, an example in `examples.js` (which the tests execute).
5. Rebuild the bundle: `cd sim && direnv exec . npm run build`, and bump `?v=` in `sim/index.html`.
6. Finish by running the `spec-impl-checker` subagent when the change touches typing/mode/determinization rules, so the paper is checked too.
