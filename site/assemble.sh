#!/usr/bin/env bash
# Assembles the published site in a new directory: the landing page at its root, the built
# simulator under sim/, the API documentation under docs/ and the paper as determinize.pdf.
#
#   site/assemble.sh --out DIR [--sim DIR] [--docs DIR] [--paper PDF]
#
# --sim defaults to sim/dist, which `npm run build` in sim/ writes. Without --docs or --paper,
# those parts are left out. The landing page's footer gets the commit that HEAD names.
# pages.yml publishes the result. To preview it under the same
# /determinize/ prefix as GitHub Pages, from the repository root in the sim shell:
#
#   rm -rf _preview && site/assemble.sh --out _preview/determinize
#   esbuild --servedir=_preview      # http://127.0.0.1:8000/determinize/
set -euo pipefail
site="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  echo "usage: $0 --out DIR [--sim DIR] [--docs DIR] [--paper PDF]" >&2
  exit 2
}
out="" sim="$(dirname "$site")/sim/dist" docs="" paper=""
while [[ $# -gt 0 ]]; do
  [[ $# -ge 2 ]] || usage
  case "$1" in
    --out) out="$2" ;;
    --sim) sim="$2" ;;
    --docs) docs="$2" ;;
    --paper) paper="$2" ;;
    *) usage ;;
  esac
  shift 2
done
[[ -n "$out" ]] || usage
if [[ -e "$out" ]]; then
  echo "$out exists; remove it first." >&2
  exit 1
fi
if [[ ! -f "$sim/index.html" ]]; then
  echo "$sim/index.html is missing; run 'npm run build' in sim/ or pass --sim." >&2
  exit 1
fi
[[ -z "$docs" || -d "$docs" ]] || { echo "$docs is not a directory." >&2; exit 1; }
[[ -z "$paper" || -f "$paper" ]] || { echo "$paper is not a file." >&2; exit 1; }

commit="$(git -C "$site" rev-parse HEAD)"

mkdir -p "$out/fonts"
# The published files of the landing page; the scripts and sources next to them stay behind.
cp "$site"/*.html "$site"/*.css "$out/"
cp "$site"/fonts/*.woff2 "$site"/fonts/*.txt "$out/fonts/"
# The footer names the commit whose theorems and documentation the page links.
grep -q '@commit@' "$site/index.html" || { echo "site/index.html has no @commit@ to stamp." >&2; exit 1; }
sed -e "s/@short-commit@/${commit:0:7}/g" -e "s/@commit@/$commit/g" "$site/index.html" >"$out/index.html"

mkdir "$out/sim"
cp -R "$sim/." "$out/sim/"

if [[ -n "$docs" ]]; then
  mkdir "$out/docs"
  cp -R "$docs/." "$out/docs/"
  # doc-gen4's index.html is a placeholder, so docs/ redirects to the root module's page.
  cat >"$out/docs/index.html" <<'EOF'
<!doctype html>
<meta charset="utf-8">
<title>Determinize</title>
<link rel="canonical" href="Determinize.html">
<meta http-equiv="refresh" content="0; url=Determinize.html">
<a href="Determinize.html">Determinize</a>
EOF
else
  echo "No --docs: the site has no API documentation under docs/."
fi

if [[ -n "$paper" ]]; then
  cp "$paper" "$out/determinize.pdf"
else
  echo "No --paper: the site has no determinize.pdf."
fi
