# `nix develop` with no attribute: everything needed to build every part of the repo, apart from
# sim/node_modules, which only the sim shell links. The per-directory .envrc files load only the
# matching subset.
{
  perSystem =
    { config, pkgs, ... }:
    {
      devShells.default = pkgs.mkShell {
        name = "determinize";

        # Not inherited through inputsFrom; see lean.nix.
        hardeningDisable = [ "bindnow" ];

        # Neither is the sim shell's npmDeps, so this shell links no node_modules (see sim.nix): the
        # sim shell, or direnv in sim/, provides sim/node_modules.
        inputsFrom = [
          config.devShells.tex
          config.devShells.sim
          config.devShells.lean
        ];
      };
    };
}
