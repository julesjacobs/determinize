# Toolchain for ./tex: pdflatex + bibtex driven by latexmk, plus chktex for linting.
# scheme-medium covers most of it; listed below are only the packages it lacks.
# acmart.cls and ACM-Reference-Format.bst are vendored in ./tex.
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.tex = pkgs.mkShell {
        name = "determinize-tex";

        packages = [
          (pkgs.texliveMedium.withPackages (
            ps: with ps; [
              # main.tex
              cleveref
              todonotes

              # acmart.cls
              libertine
              newtx
              inconsolata # zi4
              comment
              draftwatermark
              environ
              framed
              hyperxmp
              ifmtarg # required by hyperxmp
              ncctools # manyfoot
              pbalance
              preprint # balance
              totpages
              zref
              upquote
            ]
          ))
        ];
      };
    };
}
