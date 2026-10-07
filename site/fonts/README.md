# Fonts

Both families are under the SIL Open Font License 1.1 (`OFL-*.txt`, from the upstream
repositories). Each file is a subset, made with fontTools from the files in nixpkgs:

| File | Source | Weights |
|---|---|---|
| `atkinson-next-var.woff2` | `AtkinsonHyperlegibleNext[wght].ttf` 2.001, nixpkgs `atkinson-hyperlegible-next` | 200–800 |
| `determinize-mono-400.woff2` | `JuliaMono-Regular.ttf` 0.63.2, nixpkgs `julia-mono` | 400 |
| `determinize-mono-600.woff2` | `JuliaMono-SemiBold.ttf` 0.63.2, nixpkgs `julia-mono` | 600 |

```sh
pyftsubset FONT.ttf --flavor=woff2 --layout-features=kern,liga,tnum,lnum,case \
  --unicodes=U+0020-007E,U+00A0-00FF,U+0152-0153,U+2009,U+2013-2014,U+2018-201D,U+2026,U+2032,U+2190-2192,U+21A6,U+2200,U+2202,U+2208,U+2212,U+2227,U+222B,U+2264-2265,U+22A2,U+00D7,U+03BC,U+03C4,U+207B,U+00B9,U+1D50,U+2248,U+2260,U+211D,U+00B2
```

A subset is a Modified Version under the OFL, and JuliaMono's licence reserves the name
"JuliaMono", so its subsets are named "Determinize Mono" (`name` records 1, 4 and 6) and keep
its copyright notice. Atkinson Hyperlegible Next reserves no name; it lacks `↦ ∀ ⊢ ᵐ ℝ ⁻ ∧`,
which the pages set only in the monospace.
