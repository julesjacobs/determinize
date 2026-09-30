{
  description = "Determinize: Lean implementation and checked certificates, paper (LaTeX) and browser simulator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    import-tree.url = "github:denful/import-tree";

    # Lean FRO's comparator and the exporter it reads (flake-modules/devshells/lean.nix).
    # Their tags follow Lean releases: bump both together with lean/lean-toolchain.
    comparator = {
      url = "github:leanprover/comparator/v4.33.0";
      flake = false;
    };
    lean4export = {
      url = "github:leanprover/lean4export/v4.33.0";
      flake = false;
    };
  };

  # Dendritic pattern: every .nix file under ./flake-modules is a flake-parts module
  # and is imported automatically. See https://github.com/mightyiam/dendritic
  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./flake-modules);
}
