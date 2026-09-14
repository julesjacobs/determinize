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
              pgfplots
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

        # Make a bare `latexmk` (run in ./tex) build main.pdf with pdflatex:
        # $pdf_mode = 1 selects pdflatex over latexmk's default DVI mode, and
        # @default_files replaces the default '*.tex' glob (which would also try
        # to build macros.tex, lean-links.tex, ...). latexmk reads this file as
        # its system rc (LATEXMKRCSYS), so no .latexmkrc is needed in the repo.
        LATEXMKRCSYS = pkgs.writeText "latexmkrc" ''
          $pdf_mode = 1;
          @default_files = ('main.tex');
        '';
      };
    };
}
