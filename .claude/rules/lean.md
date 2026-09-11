---
paths:
  - "lean/**"
---
# lean/ (Lean 4 + Lake + Mathlib formalization of the paper)

Toolchain comes from the `.#lean` devshell (`lean/.envrc`): `elan` reads `lean/lean-toolchain` and installs that exact
Lean release into `~/.elan` (nixpkgs' `lean4` is too old for current Mathlib and must not be used). `lake` is NOT on the
bare PATH: `cd lean && direnv exec . lake build` or `nix develop .#lean --command lake build`.
Sources: Lake README (leanprover/lean4 `src/lake`), Mathlib wiki "Using mathlib4 as a dependency", Mathlib style/naming
guides (leanprover-community.github.io/contribute), elan README, nixpkgs `elan`/`lean4` packages, verified 2026-09-04.

## Pinning: three files move together
- `lean-toolchain` (`leanprover/lean4:v4.33.1`), `lakefile.toml` `[[require]] rev = "v4.33.1"` (Mathlib tag), and the
  generated `lake-manifest.json` (exact commits of Mathlib and its 9 transitive deps). Mathlib supports only the Lean
  release it was built with; Mathlib tags `v4.X.Y` are created whenever its toolchain changes.
- Bump = copy `https://raw.githubusercontent.com/leanprover-community/mathlib4/<tag>/lean-toolchain`, set `rev`, run
  `lake update mathlib` (asks first; never bare `lake update`, which bumps everything), then `lake exe cache get`.
- `lake-manifest.json` is generated: never hand-edit (hook blocks it). `.lake/` (cloned packages + build) is gitignored.
- Do not `require` batteries/aesop/Qq yourself; Mathlib pulls them.

## Layout: what is trusted and what is not
- `Determinize/Spec/*.lean` (syntax, typing, primitives, semantics, the public `Prop`s `mainThm`,
  `extendedExpectationThm`, `jensenThm`) and `Determinize/Spec/Traces/*.lean` (trace semantics, `correspondenceThm`,
  `soundnessThm`) are the reviewable interface. They import only Mathlib and each other, never `Proof`;
  `Proof/InterfaceChecks.lean` asserts that they put no measurable structure on `Expr`. Keep it that way.
- `Determinize/Theorems.lean` exports every public proposition as a theorem followed by `#print axioms`. A new public
  result = a `def ... : Prop` in `Spec/Main.lean` or `Spec/Traces/Main.lean`, a proof in `Proof/`, an export + axiom
  line in `Theorems.lean`, and a line in `lean/README.md`.
- `Determinize/Proof/**` (about 17K lines) is complete: no `sorry`, no axioms beyond `propext`, `Classical.choice`,
  `Quot.sound`. `Determinize.lean` (the default target) imports `Theorems`, `Proof/Examples`, `Proof/InterfaceChecks`;
  a module not reachable from it is never checked, so wire new modules in.
- `lean/README.md` lists the deviations from the paper (silent structural subsumption, the `×`/`/` typing
  rules, domain-checked mean operators, the `DoesNotGetStuck` validity hypothesis). Update it when a statement changes.

## Build and check (inside `lean/`)
- `lake exe cache get` once after any manifest change, BEFORE `lake build`; otherwise Lake compiles Mathlib from source
  for hours. `.claude/scripts/check.sh lean` refuses to build while `.lake/packages/mathlib/.lake/build` is missing.
- `lake build --wfail` is the check (`check.sh lean` runs it and then verifies the axiom reports). The tree is
  warning-free; a `sorry` or a linter warning fails the build. Plain `lake build` only fails on errors.
- Cost: a change to `Spec/` or `Proof/Internal/` rebuilds nearly everything (about ten minutes);
  `Proof/Measurability.lean` (5.8K lines), `Proof/Symbolic.lean` and `Proof/SymbolicSoundness.lean` dominate.
  Edit a leaf file (`Proof/Corollaries.lean`, `Proof/Soundness.lean`, `Theorems.lean`) when you can.
- One file: `lake env lean Determinize/Foo.lean` (exit 1 on error, no artifacts) checks the file against the *built*
  oleans of its imports; after an upstream edit those oleans are gone until `lake build` recreates them.
- The post-edit hook runs `check.sh lean` after every `.lean` edit with a 120 s cap; long rebuilds are cut off and
  resume on the next build. Do not run two `lake build`s concurrently.
- Failure signatures: `error: FILE:L:C: ...`, `error: build failed`. Thousands of `Mathlib.*` jobs being *built*
  (not replayed) = cache/toolchain mismatch; `leantar not found` = wrong toolchain; `lake build --no-build` is not a check.
- `lake test` / `lake lint` do nothing (no driver configured).

## Writing Lean here
- Lean 4 + Mathlib syntax only: `fun x ↦ e` (never `λ x,`), `·` for focused goals, `rw [h]` with brackets,
  `open scoped ProbabilityTheory ENNReal` (never `open_locale`), `Type*`.
- `lakefile.toml` sets no `leanOptions`: `autoImplicit` is on (the existing statements rely on auto-bound implicits
  such as `context`, `ty`, `laws`) and no Mathlib linters run. Several `Proof/` files disable core linters at the top
  and raise `maxHeartbeats` for single theorems; match the surrounding style, but add no `set_option` to
  `Spec/` or `Spec/Traces/`.
- Naming follows the existing files: descriptive lowerCamelCase hypothesis names (`typed`, `sourceForm`,
  `measurableOutput`), theorems in `snake_case` or lowerCamelCase as their neighbours, docstrings on public
  declarations, lines <= 100 chars.
- Prefer targeted imports over `import Mathlib`. Verified module paths (v4.33.1):
  `Mathlib.MeasureTheory.Measure.GiryMonad` (`Measure.dirac/bind/join`, `lintegral_bind`, monad laws),
  `Mathlib.MeasureTheory.Integral.Bochner.Basic` (`∫ x, f x ∂μ`), `Mathlib.MeasureTheory.Integral.Lebesgue.Basic` (`∫⁻`),
  `Mathlib.Probability.Kernel.Composition.CompNotation` (`κ ∘ₘ μ` is notation for `μ.bind κ`),
  `Mathlib.Probability.Kernel.Composition.MeasureComp` / `MeasureCompProd` (`⊗ₘ`, `snd_compProd`),
  `Mathlib.Probability.Kernel.Basic` (`Kernel`, `IsMarkovKernel`), `Mathlib.Probability.Distributions.{Gaussian.Real,
  Exponential, Beta, Poisson.Basic}` (`gaussianReal μ v` takes the variance, `expMeasure r` the rate,
  `gammaMeasure shape rate`), `Mathlib.Analysis.Convex.Integral` (`ConvexOn.map_integral_le`, Jensen),
  `Mathlib.Analysis.Convex.Continuous` (`ConvexOn.continuousOn`), `Mathlib.Data.EReal.Operations`.
  Old paths `Integral/Bochner.lean`, `Distributions/Gaussian.lean`, `Kernel/Composition/Basic.lean` no longer exist.
- Traps: `ProbabilityTheory.uniformOn s` is the *counting* measure conditioned on `s` (finite sets only); the uniform
  law on `[a,b]` is built by hand in `Spec/Primitives.lean` (`uniformMeasure`). `Measure.map`/`bind` of a
  non-measurable function is `0`, so measurability side conditions are load-bearing. Mathlib files use the
  `module`/`public import` system; ours do not need the `module` header.
- Editor: VS Code Lean 4 extension needs `elan`/`lake` on PATH (launch from the direnv'd shell); after a dependency
  rebuild use "Server: Restart File".
