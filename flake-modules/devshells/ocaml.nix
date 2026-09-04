# Toolchain for ./ocaml (dune + menhir), also used by det.sh and run.sh at the repo root.
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.ocaml = pkgs.mkShell {
        name = "determinize-ocaml";

        nativeBuildInputs = with pkgs.ocamlPackages; [
          ocaml
          dune_3
          findlib
          menhir
          ocaml-lsp
          ocamlformat
        ];

        buildInputs = with pkgs.ocamlPackages; [
          menhirLib
        ];

        # run.sh --storm additionally needs the Storm probabilistic model checker
        # (https://www.stormchecker.org). It is not packaged in nixpkgs; install it
        # separately (Homebrew tap `moves-rwth/storm`, Docker, or from source).
      };
    };
}
