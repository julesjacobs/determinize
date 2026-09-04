---
paths:
  - "tex/**"
---
# tex/ (acmart paper: pdflatex + bibtex via latexmk, chktex, cleveref, mathpartir, todonotes)

Toolchain comes from the `.#tex` devshell (`tex/.envrc`, texliveMedium + extras listed in `flake-modules/devshells/tex.nix`);
`latexmk` is NOT on the bare PATH: `cd tex && direnv exec . latexmk` or `nix develop .#tex --command latexmk`.
Sources: latexmk manual (CTAN), acmart guide v2.20 (CTAN/GitHub borisveytsman/acmart), ChkTeX manual, cleveref docs, verified 2026-09.

## Build and check (inside `tex/`)
- `latexmk -pdf -interaction=nonstopmode -file-line-error main.tex` builds `main.pdf` in place (bibtex runs automatically); artifacts are gitignored.
- Exit code: 12 means a hard error (`!` line or `file.tex:LINE:` line in `main.log`). Undefined references/citations do NOT fail the build unless `-Werror`; the Stop hook and `.claude/scripts/check.sh tex` grep them out and treat them as failures. Overfull boxes and "multiply defined" labels are reported as warnings.
- Lint: `chktex -q -n1 -n3 -n8 -n13 -n24 -n36 -n44 -n46 FILE.tex` (the muted numbers are the usual false positives in math-heavy papers; exit 2 = warnings, 3 = errors, 0 = clean). The post-edit hook runs this on every edited `.tex`.
- To find a missing package: `grep "File .* not found" main.log`, then add the TeX Live package (name != file name, e.g. `newtxmath` is in `newtx`) to `tex.nix`, `git add`, `direnv reload`.
- Known state (2026-09-04): label `fig:determinization` is defined twice (see `main.log`); 3 overfull boxes.

## Document conventions
- `main.tex` is `\documentclass[acmsmall,screen,review,nonacm]{acmart}`: add `anonymous` for submission, drop `review` for camera-ready. Sections are `\input` in numeric order (`1_introduction` ... `7_related_work`); `8_old.tex` is dead and must not be re-included (duplicate `\section`s).
- All notation lives in `macros.tex`. Use the macros, never raw markup: keyword macros (`\letkw \inkw \ifkw \thenkw \elsekw \funkw \matchkw \withkw \observekw \fstkw \sndkw ...`), distributions (`\uniform \gaussian \flip \discrete \exponential \betafn \gammafn \poisson`), types (`\Float{m} \Bool \Nat \List{}`, modes `\E` `\G`), semantics brackets (`\sem \bigsem \symsem ...`, `\monbind`), the transform `\exptrans{e}`. Add a new macro to `macros.tex` instead of inlining `\textnormal{\ttfamily ...}`.
- Inference rules: `mathpartir` (`\begin{mathpar} \inferrule[Name]{P_1 \\ P_2}{C} \and ... \end{mathpar}`); rules without premises still get a bar (`\inferrule{ }{C}`).
- Theorems: acmart defines `theorem`, `lemma`, `corollary`, `proposition`, `definition`, `example` itself (`acmplain`/`acmdefinition` styles, shared counter). Do not `\newtheorem` those names; `remark` is defined in `main.tex`. `amsthm`/`amsmath`/`amssymb`/`hyperref`/`natbib`/`graphicx`/`booktabs`/`xcolor` are loaded by the class: never `\usepackage` them again (option clash).
- References: `\cref` mid-sentence, `\Cref` at sentence start; `\label` after `\caption` inside floats; labels without commas. Citations with `\citep`/`\citet` (`\citestyle{acmauthoryear}`), BibTeX only (`ACM-Reference-Format`), keep `doi` fields, no duplicate keys. Remove `\nocite{*}` once real citations exist.
- Math: `align*`/`gather*` for displays, `\text{}` for words inside math, `\DeclareMathOperator` for named operators; never `\[ \]` or `$$` inside `align`. `\allowdisplaybreaks` is on.
- `fig_symbolic_coupling.svg` is not included anywhere. If it is needed: convert once (`rsvg-convert -f pdf -o fig.pdf fig.svg`), commit the PDF, `\includegraphics`; do not add the `svg` package (needs `--shell-escape` + Inkscape).
- `todonotes` is loaded but unused; open work is tracked in `TODO.md`, not in `\todo{}`.
