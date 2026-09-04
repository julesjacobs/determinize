---
paths:
  - "ocaml/**"
  - "det.sh"
  - "run.sh"
---
# OCaml (dune 3.23, menhir, ocamllex, ocamlformat 0.29, ocaml-lsp)

Toolchain comes from the `.#ocaml` devshell (loaded by the root `.envrc`). Sources: dune manual
(dune.readthedocs.io), ocaml.org docs, Menhir manual (gallium.inria.fr/~fpottier/menhir), verified 2026-09.

## Commands (run inside `ocaml/`)
- `dune build` builds everything, incl. `determinize_main.exe`. `dune build @check` type-checks only (fastest; what ocaml-lsp needs).
- `dune build -w` watch mode; `dune exec -- ./determinize_main.exe FILE.det`; `dune clean`; `dune printenv` shows effective flags.
- `../det.sh` runs all `det/*.det` and rewrites their `.dout`; `../run.sh [--storm [--limit N]] FILE.det` runs one.
- There is no `dune test` yet. Golden testing = `./det.sh` then `git diff det/` (the `.dout` files are the expected outputs, deterministic because `Random.init 0`). Never switch to `Random.self_init`.

## Dev profile: warnings are errors
The default (dev) profile compiles with `-strict-sequence` and warnings `@1..3@5..28@30..39@43@46..47@49..57@61..62@67@69` fatal.
So the following are build failures, not hints: non-exhaustive match (8), unused variable (26/27), unused value/open/type/rec (32-39),
redundant case (11), missing record field in pattern (9), unused field (69). Fix the cause; do not add `-warn-error -a`, do not build with `--release` to hide it.
Prefix intentionally unused bindings with `_`.

## Style of this codebase
- Not ocamlformat-formatted (a whole-tree reformat would rewrite ~90% of lines). Match the surrounding layout by hand; the `guard-bash` hook blocks the formatter.
  If the team adopts formatting: add `ocaml/.ocamlformat` with `version = 0.29.0` and `profile = default`, reformat everything in one standalone commit.
- One module per file, `(wrapped false)` library, no `.mli` files, no ppx. Keep it that way unless asked.
- `Format`-style doc combinators live in `pretty.ml`; use them (or `Printf` for plain lines). Don't mix `Format.printf` and `print_string` without flushing.
- Errors: exceptions are used (`ObserveFailure`, `Failure`); keep messages informative and include the construct.

## Gotchas specific to this project
- `symbolic_coupling.ml` is NOT in the `(modules ...)` list of `ocaml/dune`, so dune silently never compiles it (no warning, no LSP diagnostics). It is a standalone prototype; if you touch it, compile it by hand (`ocamlfind ocamlopt -package menhirLib symbolic_coupling.ml`) or give it its own `(executable)` stanza.
- Any new `.ml` file must be added to the `(modules ...)` list or it is ignored the same way.
- `(menhir (modules parser) (flags --explain))` with `(using menhir 2.1)`: the `.conflicts` file is NOT produced (dune sandboxing). Upgrading to `(using menhir 3.0)` and dropping `--explain` makes dune generate `_build/default/parser.conflicts` by default; with 3.0 the explicit flag is an error. Grammar currently has 0 conflicts; tokens `DOT` and `GT` are declared but unused.
- Menhir: precedence declarations later in the file bind tighter; `%prec` overrides; reduce/reduce conflicts need a grammar rewrite. `(infer true)` is already the default (semantic actions get precise type errors).
- New files are invisible to Nix until `git add` (or `git add -N`).

## OCaml pitfalls to double-check before building
- `rec` missing (or present but unused: warning 39 is fatal).
- `;` binds looser than `if`: `if c then a; b` always runs `b`. Use `begin ... end` or parentheses.
- Nested `match` must be parenthesised or later arms attach to the inner match.
- Precedence: application > unary `-` > `*` `/` > `+` `-` > `::` (right assoc) > `@` `^` > comparisons > `&&` > `||` > `,`. So `f x :: l` is `(f x) :: l`, `f -1` is subtraction, `a, b :: l` is a pair.
- `;;` only at the toplevel/REPL; in files use `let () = ...`.
- Printf formats are typed (`%d` int, `%f`/`%g` float, `%s` string). `%s` does not take a float.
- Shadowing is silent; `let open` shadows too.
