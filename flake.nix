{
  description = "Determinize: OCaml implementation, paper (LaTeX) and browser simulator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    import-tree.url = "github:denful/import-tree";
  };

  # Dendritic pattern: every .nix file under ./flake-modules is a flake-parts module
  # and is imported automatically. See https://github.com/mightyiam/dendritic
  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./flake-modules);
}
