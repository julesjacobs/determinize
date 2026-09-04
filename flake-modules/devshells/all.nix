# `nix develop` with no attribute: everything needed to build every part of the repo.
# The per-directory .envrc files load only the matching subset.
{
  perSystem =
    { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        name = "determinize";

        inputsFrom = [
          config.devShells.ocaml
          config.devShells.tex
          config.devShells.sim
        ];
      };
    };
}
