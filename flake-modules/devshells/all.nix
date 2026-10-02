# `nix develop` with no attribute: everything needed to build every part of the repo.
# The per-directory .envrc files load only the matching subset.
{
  perSystem =
    { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        name = "determinize";

        # Not inherited through inputsFrom; see lean.nix.
        hardeningDisable = [ "bindnow" ];

        inputsFrom = [
          config.devShells.tex
          config.devShells.sim
          config.devShells.lean
        ];
      };
    };
}
