# D7 feasibility study: the whole pipeline in Lean, a Lean+OCaml split, or Rocq

Date: 2026-09-11. Scope: analysis only (nothing implemented). Question: how to get a
machine-checked guarantee for each *specific* `.det` program that the tool processes, given
the existing artifact (`lean/`: 16.4K lines, `find lean/Determinize -name '*.lean' | xargs wc -l`,
all proofs on Mathlib, eight exported theorems with standard axioms only after the variance commit of the same day) and the OCaml pipeline
(`lexer.mll`/`parser.mly` 150 lines, `infer.ml` 252, `determinize.ml` 356, `interp.ml` 215 with a
100-trial Monte Carlo, `to_mc.ml` 721 for Storm; 22 programs in `det/`, the largest 18 lines).

All numbers marked "measured" were produced on this machine today (Lean `v4.33.1`, Mathlib
`v4.33.1`, warm Mathlib cache). Numbers marked "estimate" are my judgement.

## 1. What a per-program certificate can say, in any option

The theorems (`lean/Determinize/Statement/Main.lean`, `Traces/Main.lean`) quantify over
`program : Expr` (literals in `ℝ`) with four hypotheses: `Typed [] program (.float m)`,
`program.sourceForm = true`, `DoesNotGetStuck program`, and (for `mainThm`) integrability.
A certificate for a concrete program therefore consists of:

1. the Lean term `p : Expr` and the claim that it *is* the program in the `.det` file (trusted in
   every option that has a parser; only a Lean-side parser with a specification moves this);
2. `Typed [] p (.float m)`: a hand-written derivation today (`Proof/Examples.lean`); a Bool
   checker `check : List Ty → Expr → Ty → Bool` with `check_sound` makes it `by decide`. `Typed`
   is syntax-directed (one rule per constructor, `lean/README.md`), so the checker is direct;
3. `p.sourceForm = true`: `by decide` works today (measured);
4. `p.determinize = q` where `q` is what the tool printed as the determinized program: `rfl`
   works today (measured); `decide` fails on `Expr ℝ` because `ℝ` has no `DecidableEq`
   (measured: "failed to synthesize Decidable (p.determinize = q)"). With a decidable literal
   type (`Rat`, or a mantissa/exponent pair) `decide` works as well;
5. `DoesNotGetStuck p`: a measure-theoretic statement (`∀ᵐ` over the fiber at each depth), not
   decidable. Today it is proved by hand per program (`reciprocal_safe`, `scaledSample_safe`).
   Discharging it automatically needs a lemma library (analysis doc 2.2 P1) and, for parameters
   computed from earlier draws, interval or sign reasoning. This is the hard part of any
   per-program certificate and is independent of the language choice. The certificate can
   instead state the theorem *conditional* on validity, which is what the paper assumes anyway.

What no option certifies: the sampler, IEEE-754 arithmetic against the `ℝ` semantics of
`reduce`, the Storm export, and the printed `.dout` numbers. Lean's `Float` is IEEE-754 binary64
with a logical model for `add/sub/mul/div/sqrt` (`Init/Data/Float/Model/Float.lean` in the
installed toolchain) but opaque transcendental functions (`@[extern "log"] opaque Float.log`,
`Init/Data/Float.lean`; https://lean-lang.org/doc/reference/latest/Basic-Types/Floating-Point-Numbers/).
There is no `Float → ℝ` map in Mathlib and no theorem relating the two, so the expectation
theorem about `ℝ` never transfers to floating-point runs; the Monte Carlo means stay evidence.
Rocq is in the same position: primitive floats are axiomatized through Flocq's `SpecFloat`
(`Axiom mul_spec : forall x y, Prim2SF (x * y) = SF64mul (Prim2SF x) (Prim2SF y)`,
https://rocq-prover.org/doc/master/refman/language/core/primitive.html), and a rounding-error
analysis would be a separate Flocq project (https://flocq.gitlabpages.inria.fr/, v4.2.2).

## 2. Lean 4 as the implementation language (option a)

**Executables and Mathlib.** `lake exe` links the object files of the root module's transitive
imports (`LeanExe.recBuildExe` uses `root.transImports`; `src/lake/Lake/Build/Executable.lean`,
`Module.lean` in the installed toolchain; Lake README: the default `nativeFacets` is "the object
file compiled from the Lean source",
https://github.com/leanprover/lean4/blob/master/src/lake/README.md). Mathlib's cache ships
".olean files, .ilean files, .trace files, .c files (generated C code)" but no `.o`
(https://github.com/leanprover-community/mathlib4/blob/master/Cache/README.md; measured
locally: 8322 `.c`, 0 `.o`). Importing `Determinize.Statement.Syntax` loads 4641 modules, 2850 of
them Mathlib (measured with `env.header.moduleNames`; `Syntax` imports `Primitives`, which imports
the Gaussian/Beta/Poisson/Exponential laws, and `Mathlib.Tactic.DeriveCountable`, alone 225
Mathlib modules). An executable importing the current `Statement` would therefore compile about
2850 Mathlib C files first (estimate: one to several CPU hours once, then cached) and be
correspondingly large. A Mathlib-free executable is cheap: a hello-world `lean_exe` with an AST,
`StdGen` sampling and `Float.sqrt/log` builds in 2.1 s and is a 4.1 MB static binary depending
only on libc/libm (measured). **Consequence:** the runtime must live in a module that imports no
Mathlib, so `Expr`, `Mode`, `Ty`, `Op`, `Kind`, `mapVars`, `determinize`, `sourceForm` and the
typing checker have to move into a Mathlib-free core (the `Countable` deriving handler comes
from Mathlib; derive it later in `Primitives`). `Expr (Literal : Type := ℝ)` is already
parametrised; `determinize`/`sourceForm`/`mapVars` are currently fixed to `Expr ℝ` and would
become literal-polymorphic. A change in `Statement/` rebuilds nearly all of `Proof/` (about ten
minutes, `.claude/rules/lean.md`); the proof repair for polymorphic definitions is mechanical but
touches many files (estimate: 1 week including the rebuild loop).

**Literal types and the cast.** Runtime: `Expr Float`; theorems: `Expr ℝ`. There is no
computable `Float → ℝ`; the workable design is a shared exact literal type produced by the
parser (`Rat`, or `(mantissa : Int, exponent : Int)` for `0.25`), with `Expr.map : (α → β) → Expr α
→ Expr β`, `Rat.cast` to `ℝ` inside theorems and `Float.ofScientific` for the interpreter. The
needed lemma `determinize (map f e) = map f (determinize e)` is a structural induction (trivial).
Then certificates are about `Expr Rat` (decidable, `decide`/`rfl`), and the `ℝ` theorem applies to
`map Rat.cast p`. Guarantees that survive: everything in section 1 items 2 to 5; nothing about
floating point, exactly as in OCaml.

**Parsing.** Core has `Std.Internal.Parsec` (namespace `Internal`, hence no stability promise;
`Std/Internal/Parsec/{Basic,String}.lean`); `Lean.Parser` needs a Lean environment and is not
suited to a standalone tool. A hand-written recursive-descent parser for the 90-line grammar of
`parser.mly` is 1 to 2 days (estimate). Core has no `String.toFloat?`; decimal literals are
parsed to mantissa/exponent anyway. A pretty-printer for `.dout` is a day.

**Sampling.** Core: `StdGen`, `mkStdGen`, `randNat`, `randBool`, `IO.rand`, `IO.setRandSeed`
(`Init/Data/Random.lean`); Mathlib: `Mathlib/Control/Random.lean` (`Random`, `randFin`,
`randBool`), no `Float` instance; no Gaussian, gamma, beta or Poisson sampler anywhere in core or
Mathlib (grep of both). The samplers in `interp.ml` (Box-Muller, Marsaglia-Tsang, Knuth) port in
about 100 lines using `Float.sqrt/log/cos/exp`, or C via `@[extern]`
(https://lean-lang.org/doc/reference/latest/Run-Time-Code/Foreign-Function-Interface/, "Float
is represented by double"; the reference calls the FFI "unstable"). The sampler is trusted in
every option; nothing connects it to `uniformFiber` and friends.

**Decidable typing and axioms.** `by decide` and `rfl` add no axioms. `native_decide` does: on
this toolchain `#print axioms` shows `t._native.native_decide.ax_1_1` (measured), one
auto-generated axiom per computation since Lean 4.29.0 ("native computation ... is represented
in the logic as one axiom per computation",
https://lean-lang.org/doc/reference/latest/releases/v4.29.0/; before 4.29 it was
`Lean.ofReduceBool`, `axiom ofReduceBool` in `Init/Core.lean`). Either form violates the repo
rule (only `propext`, `Classical.choice`, `Quot.sound`), so certificates must use `decide`/`rfl`.
Kernel `decide` on a Bool checker over programs of the size in `det/` (at most 18 lines) is
well within budget (estimate; `sourceForm` by `decide` was instantaneous in the measurements).

**Checking cost.** `lake env lean` on a certificate file importing `Determinize.Theorems` with
all three decidable obligations: 22.9 s cold, 4.2 s warm; a file importing only
`Statement.Syntax`: 3.9 s; an empty file: 1.4 s (measured). The "Mathlib import per check" cost
that D7 worried about is about 3 s per file. One generated `Generated/Certificates.lean` inside
`lake build --wfail` costs the same once, and its axiom lines are covered by `check.sh`.

**Storm export.** `to_mc.ml` (721 lines, `Marshal`-keyed state space) is the largest port; it
has no theorem behind it in any option.

## 3. Lean + OCaml splits (option b)

| Split | Lean checks | Trusted beyond Lean kernel, sampler, floats |
|---|---|---|
| b1 per-program `.lean` via `lake env lean` | items 2 to 4 (5 when automated) | OCaml parser, `infer.ml` elaboration, `to_lean.ml` emitter (de Bruijn, `promote` insertion, desugaring of `-`, `<=`, discrete primitives, `observe`); that the printed `p` is the `.det` file |
| b2 one generated `Certificates.lean` in the build | same, plus axiom report in CI | same |
| b3 OCaml emits `p` and `q`, Lean checks `p.determinize = q` | ties `determinize.ml`'s output to the certified function | same minus `determinize.ml` (it becomes untrusted) |
| b4 Lean `lake exe` (Mathlib-free core) called by OCaml, or Lean C linked via FFI | as b3, and the executable *is* `Expr.determinize` | OCaml front end only; FFI adds Lean runtime initialisation for no gain |

b1 vs b2 is only a workflow question: 4 s per program either way; b2 is preferable because
`lake build --wfail` and `check.sh lean` already exist. b3 is what makes the certificate say
something about the tool: `rfl`/`decide` on `p.determinize = q` proves that the program the
OCaml tool printed as "Determinized" equals the Lean transformation of the program the tool
printed as elaborated source. The emitter remains trusted (it is a printer, about 150 lines,
reviewable by eye), as does the claim that `p` is the `.det` file. Engineering (estimate):
`to_lean.ml` 1 week (named to de Bruijn, `Sub`/`Leq` desugaring, rejecting or encoding
`Flip`/`Bernoulli`/`Discrete`/`Observe` until L2/L3 land), Lean checker + soundness 1 week,
`Certificate.lean` wrapper theorem, generator script and `certify.sh`, golden test 0.5 week;
total 2.5 to 3 weeks on top of the already planned OCaml alignment (analysis doc 2.3 P0).
Section 7 story: "the OCaml tool emits, for every benchmark, a Lean certificate that its
determinized output is the mechanized transformation of a well-typed source program; the
theorems of Section 6 then apply". Workflow for Section 8: `./det.sh` regenerates `.dout` and
`Generated/Certificates.lean`; one `lake build` checks all programs.

## 4. Rocq with certified extraction (option c)

**Library coverage** (MathComp-Analysis 1.18.0, 2026-09-02, Rocq 9.0 to 9.3; cloned and
grepped today, https://github.com/math-comp/analysis): Lebesgue measure and integral for
`\bar R`-valued functions (`lebesgue_integral_theory/`; no Bochner integral, which suffices
here since outputs are real); s-finite kernels and kernel composition (`kernel.v`:
`sfinite_kernel`, `mkcomp`, `l \; k`); the Giry monad (`lebesgue_integral_theory/giry.v`,
`giry_ret`, `giry_bind` taking a measurability proof, added in 1.15.0); laws: bernoulli,
binomial, uniform, exponential, poisson, normal, beta (`probability_theory/*_distribution.v`);
expectation, variance, moment generating function (`random_variable.v`). **Missing:** the gamma
law (no file), Jensen's inequality for the integral (no lemma found; `convex.v` has convex
combinations only), and mean/variance closed forms for the exponential, Poisson and beta laws
(only `integral_*_pdf` lemmas). Mathlib features the Lean proof uses: `Measure.bind`, `Kernel`,
`IsMarkovKernel`, `⊗ₘ`, Bochner integral, Gaussian/exponential/beta/gamma/Poisson laws with
moments, `ConvexOn.map_integral_le` (all imported by `Statement/Primitives.lean` and
`Proof/`). Re-formalization: the Lean development is 16.4K lines and took the collaborator's
2026 effort; a MathComp port with the gaps filled is 12 to 24 person-months (estimate; the
`probability_theory` + `measure_theory` directories of MCA are 11.3K lines in total, which
calibrates how much each new law costs). The existing Lean proof would be thrown away.

**Extraction.** Standard `Extraction` is trusted: "the extraction process is part of the
trusted code base (TCB), as are Coq's kernel and the compiler used to compile the extracted
code" (Forster, Sozeau, Tabareau, PLDI 2024,
https://rocq-prover.org/papers/verified-extraction-from-coq-to-ocaml); axioms are realized by
user-supplied ML strings copied verbatim
(https://rocq-prover.org/doc/master/refman/addendum/extraction.html). Verified extraction
(MetaRocq, https://github.com/yforster/coq-verified-extraction, Rocq 9.1 branch, MetaRocq
v1.3.2) targets Malfunction, "supports all of Rocq's constructs including primitive integers,
floats and arrays", proves correctness for "extracted programs of first-order data type"
(higher-order interoperation "can lead to incorrect behaviour and even outright segfaults"),
leaves cofixpoint translation unverified, and trusts "the Malfunction and OCaml compilers";
IO and randomness are axioms realized in a generated `Axioms.ml`. CertiRocq 0.9.1 (Rocq 9.1,
2025-03) compiles Gallina to Clight/WebAssembly with primitive integers and floats since 0.9
(2024-05), "large parts ... verified" (https://github.com/CertiRocq/certirocq/releases,
README). In all of these the sampler is an axiom: a probabilistic sampler cannot be certified
against a measure semantics (there is no theorem linking a PRNG stream to `uniform_prob`),
and floats are the Flocq-specified primitives of section 1. So the certified-extraction route
certifies exactly the same items 2 to 5 as Lean, plus "the compiled parser/inference/
determinizer is the Gallina one" (which the Lean `lake exe` route gets by construction up to
the Lean compiler, itself trusted, https://lean-lang.org/doc/reference/latest/ValidatingProofs/).

## 5. Comparison

| | (a) all in Lean | (b) Lean + OCaml (b2+b3) | (c) Rocq + certified extraction |
|---|---|---|---|
| Certified per program | items 2 to 5; `determinize` executed is the certified function | items 2 to 5 for the emitted `p`, `q`; ties `determinize.ml` to Lean | items 2 to 5; whole pipeline is the certified Gallina |
| Trusted | Lean kernel + compiler, parser (unless specified), sampler, floats, Storm export | + OCaml parser/inference, `to_lean.ml`, that `p` is the file | Rocq kernel, extraction pipeline remnants, OCaml/C compiler, parser spec, sampler axiom, floats, Storm export |
| Effort (estimate) | 8 to 12 person-weeks: Mathlib-free core + proof repair 1, parser/printer 1, inference + `promote` elaboration 1 to 2, interpreter + samplers 1, `to_mc` port 1 to 2, checker + soundness 1, generator/CI 1, plus validity automation 2 to 6 | 2.5 to 3 person-weeks plus validity automation 2 to 6 | 12 to 24 person-months for the theory, 1 to 2 months for extraction/tooling |
| Risk | moderate: `Statement/` refactor rebuilds `Proof/` (10 min per iteration); sim (`sim/`) must be re-synced from a Lean reference instead of OCaml | low: additive; existing pipeline untouched except the planned P0 alignment | high: library gaps (gamma, Jensen, moments), no reuse, two proof assistants |
| Existing 16.4K-line proof | reused, `Statement/` reorganised | reused unchanged | discarded |
| Paper Section 7 | "reference implementation is the verified definition, compiled by Lean" | "certificate per benchmark, checked by `lake build`" | "certified extraction" (strongest story, but delayed by a year or more) |
| Per-program check | inside `lake build`, seconds | 4 s per program, or one build | `rocq` check of a generated file, comparable |

## 6. Recommendation and staged path

Choose (b) now, designed so that it grows into (a); do not pursue (c).

1. **Lean, leaf-only (about 1 week).** `Determinize/Certificate.lean`: `check`/`check_sound` for
   `Typed`; wrapper theorem `certify (p q) (typed : check [] p (.float m) = true)
   (src : p.sourceForm = true) (det : p.determinize = q) : DoesNotGetStuck p → (conclusions of
   mainThm, extendedExpectationThm, jensenThm, MeanOnTraces p q)`. This needs no change to
   `Statement/` and no `Proof/` rebuild.
2. **OCaml emitter + generated file (about 1.5 weeks).** `to_lean.ml` prints `p` and `q` as
   `Expr` terms (after D1 to D4 alignment and elaboration with explicit `promote`);
   `det.sh` regenerates `lean/Determinize/Generated/Certificates.lean`, listed in
   `Determinize.lean`; `check.sh lean` verifies it and its axiom lines. **This is already a real
   certificate:** for every benchmark, Lean checks typing, source form, and that the tool's
   determinized output is the mechanized transformation; validity is an explicit hypothesis, as
   in the paper. Section 7 can be written at this point.
3. **Validity automation (2 to 6 weeks, open-ended).** Public `safe_*` lemmas for all primitives,
   `let`, `if`, application, and a syntactic in-domain check with a soundness lemma for programs
   whose primitive parameters are literals or sign-preserving arithmetic on earlier draws;
   the generator emits `DoesNotGetStuck` proofs where the check succeeds and leaves the
   hypothesis otherwise. Only this stage turns the conditional certificate into an
   unconditional one, and it is the same work in every option.
4. **Optional, towards (a) (3 to 5 weeks).** Make `Expr`'s literal-polymorphic core Mathlib-free
   (with a `Rat` or mantissa/exponent literal type and the `map` commutation lemma), and build a
   `lake exe determinize` from it that parses, infers modes, elaborates, determinizes and prints
   both the `.dout` report and the certificate. The OCaml emitter then disappears from the trust
   base; `interp.ml`'s Monte Carlo and `to_mc.ml`'s Storm export can stay in OCaml (they carry no
   guarantee either way) or be ported last. Decide after stage 3 whether the paper deadline
   allows it; nothing in stages 1 to 3 is wasted if it does not.
