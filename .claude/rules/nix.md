---
paths:
  - "flake.nix"
  - "flake.lock"
  - "flake-modules/**"
  - "**/.envrc"
---
# Nix flake (flake-parts + dendritic import-tree), direnv/nix-direnv

Sources: nix.dev manual, flake.parts, github.com/mightyiam/dendritic, github.com/denful/import-tree, nix-community/nix-direnv, RFC 166 (nixfmt), verified 2026-09.

## Layout rules (dendritic pattern)
- `flake.nix` is a one-liner: every `*.nix` under `flake-modules/` is a flake-parts module, auto-imported by `import-tree`. To add a feature, create `flake-modules/<feature>.nix` (or `devshells/<name>.nix`) containing `{ perSystem = { pkgs, config, ... }: { ... }; }`. No registration step.
- Paths containing a `_`-prefixed segment are ignored (private helpers to `import` manually). `systems.nix` sets the systems list.
- `devShells.default` (`devshells/all.nix`) is the union of `ocaml`, `sim`, `tex` via `inputsFrom`; `inputsFrom` merges packages and shellHooks but not plain env attributes.
- In a devshell use `packages = [ ... ]` for tools (compilers, LSPs, formatters); `nativeBuildInputs` is equivalent; `buildInputs` is for libraries (no `$PATH` effect). `pkgs.mkShell` (OCaml needs a C compiler).
- Storm is NOT in nixpkgs (`pkgs.storm` is Apache Storm). Do not add it; see the `storm` skill for Docker/Homebrew options.

## Git is part of the build
- Flakes see only git-tracked files: a new `.nix` file (or any new file a shell needs) is invisible until `git add` / `git add -N`. "No such file or directory" under `/nix/store/...-source/` means exactly this.
- `.envrc` files are ignored by the user's global gitignore: `git add -f path/.envrc` for new ones. `.direnv/` stays ignored.
- `warning: Git tree is dirty` is informational.

## Commands
- `nix flake show`, `nix flake check` (evaluates all devShells; builds `checks.*`, none defined yet), `nix flake metadata`.
- `nix flake update` (all) or `nix flake update nixpkgs` (one input). `nix flake lock --update-input` is deprecated. Touching `flake.lock` changes everyone's toolchain: the guard hook asks first.
- Non-interactive: `nix develop .#ocaml --command dune build`; ad-hoc tool: `nix shell nixpkgs#foo --command foo`.
- Package lookup: `nix eval --raw nixpkgs#foo.name` (fast existence check), `nix search nixpkgs foo`, or search.nixos.org. Verified names: `ocamlPackages.ocamlformat`, `prettier` (top-level; `nodePackages.*` is gone), `texlive.pkgs.latexindent`, `nixfmt` (= RFC-166 style; `nixfmt-classic` removed).
- direnv: after editing an `.envrc` run `direnv allow`; nix-direnv watches `.envrc`, `flake.nix`, `flake.lock` only, so after changing `flake-modules/*.nix` run `direnv reload` (or add `watch_dir flake-modules` before `use flake`). `direnv exec DIR CMD` runs CMD in DIR's cached shell (fast).
- No `formatter` output exists yet. If one is added: `perSystem.formatter = pkgs.nixfmt-tree;` in `flake-modules/formatter.nix`; treefmt-nix is overkill for this repo.

## Writing Nix
- Attribute bindings end with `;`; list elements have no separators; a missing `;` errors on the *next* line.
- Module file is either an attrset `{ perSystem = ...; }` or a function `{ inputs, lib, ... }: { ... }`, not both; keep `...` in argument patterns.
- Prefer explicit `pkgs.x` over broad `with pkgs;` in module files; `with` only inside short lists.
- `inherit x;` = `x = x;`; `"${expr}"` interpolates; `''...''` strips common indentation (escape `${` as `''${`); paths are unquoted.
- Style is RFC 166 (nixfmt): 2-space indent, nested attrsets multiline, one list element per line when multiline. Existing files already conform; mimic them.
