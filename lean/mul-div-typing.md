# Typing of multiplication and division: which operands may be expectation-moded

The four artifacts of this repository disagree on when `e₁ × e₂` and `e₁ / e₂` may
take an operand of type `Float[E]`. This note derives the mathematically correct rule
from the soundness argument itself, explains why independence is not the relevant
condition, and recommends how to reconcile the artifacts.

## 1. The disagreement

| Artifact | `e₁ × e₂` | `e₁ / e₂` |
|---|---|---|
| paper, `tex/3_typing.tex` | `Mul-G`: both operands G, any result mode; `Mul-ConstL`/`Mul-ConstR`: a literal on either side, both operands at most the result mode | `Div`: the denominator is a literal at most G, the numerator at most the result mode |
| `ocaml/infer.ml` | as the paper | as the paper, plus non-literal `G / G` at any result mode |
| `sim/src/compiler/infer.js` | left operand at the result mode `m`, right operand G, result `m` | same |
| `lean/` (`Typed.mul`, `Typed.div`) | as the sim | as the sim |

So the sim and the Lean accept `x : E` times `y : G` at mode E, which the paper and the
OCaml reject, while the paper and the OCaml accept a literal on the *left* of an E
operand (`2 × x`), which the sim and the Lean only accept as `x × 2`.

## 2. The invariant that makes determinization sound

Fix a well-typed closed program and condition on the complete sequence of its
general-mode draws, the *G-trace*. Write `𝒢` for the σ-algebra it generates. Two
facts hold along every execution, and both are established by the Lean development:

1. **Every G-typed value is a function of the G-trace alone.** No typing rule turns an
   E value into a G value: float subtyping only goes from G to E, comparisons and
   conditionals need G operands, products and sums carry their component modes, and a
   function cannot be applied to an E argument where a G parameter is expected. In
   the Lean this is the `GConstant` invariant of the symbolic language: every G-moded
   coordinate has zero coefficients on the E draws (`WellTyped.gconstant` in
   `Proof/Symbolic.lean`).
2. **Every E-typed value is an affine function of the E draws** whose coefficients
   are `𝒢`-measurable, and the E-moded parameters of every E draw are affine in the
   earlier E draws, so conditional means are affine as well. In the Lean, expressions
   of the symbolic language carry coordinates in `Affine n = ℝ × (Fin n → ℝ)`, and
   `SymbolicMoments.integral_affine` proves that the expectation of such a coordinate
   is the coordinate evaluated at the mean valuation. This is the paper's lemma
   "Mean valuation correctness", whose proof is still marked TODO in
   `tex/6_soundness.tex`.

Determinization replaces every E draw `u` by its conditional mean. Soundness for a
single output is the identity

    E[ c₀ + Σᵢ cᵢ uᵢ | 𝒢 ] = c₀ + Σᵢ cᵢ E[uᵢ | 𝒢]      (all cᵢ 𝒢-measurable),

that is, linearity of conditional expectation plus the rule "taking out what is
known". Nothing here mentions independence.

## 3. Multiplication

**E times G is sound.** Let `x = c₀ + Σ cᵢ uᵢ` be E-typed and `y` be G-typed, hence
`𝒢`-measurable by fact 1. Then

    x · y = (y c₀) + Σᵢ (y cᵢ) uᵢ

is again affine in the E draws with `𝒢`-measurable coefficients, so fact 2 is
preserved, and

    E[x · y | 𝒢] = y · E[x | 𝒢],

which is exactly the value the determinized program computes. Integrability is not an
issue: given `𝒢`, `y` is a constant and `x` has a finite first moment, which the Lean
proves for almost every trace (`Traces.soundnessThm` needs no integrability
hypothesis).

Dependence between `x` and `y` is allowed, in one direction. The E factor may depend
on the G factor arbitrarily. Take `y ~ Uniform(0, 1)` in mode G and `x = y + u` with
`u ~ Uniform(0, 1)` in mode E. Then `E[x · y] = E[y²] + E[u] E[y] = 1/3 + 1/4`, and
the determinized program computes `(y + 1/2) · y`, whose expectation is again
`1/3 + 1/4`. What must not happen is the other direction, a G factor that depends on
an E draw, and that is precisely what fact 1 rules out. Independence is therefore
neither necessary nor the right concept; the condition is `𝒢`-measurability of one
factor, and the mode system tracks exactly that.

**G times E is sound** by the same argument with the roles swapped. Multiplication
commutes, so the asymmetry of the sim and Lean rule is an artifact of how the rule
was written, not of the mathematics.

**E times E is unsound and must stay rejected.** A product of two affine functions of
the E draws is quadratic. With `x ~ Uniform(0, 1)` in mode E, `E[x · x] = 1/3`, while
the determinized `(1/2) · (1/2) = 1/4`. Independence would rescue `E[u₁ u₂] =
E[u₁] E[u₂]`, but the type system cannot see independence, and `x · x` shows that a
syntactic product of E values is wrong in general.

**Literals are G.** A literal is deterministic, so it is a special case of a
`𝒢`-measurable factor; the paper's `Mul-ConstL` and `Mul-ConstR` are instances of
"one factor is G" once the literal is typed at G, which every literal can be.

The maximal sound rule is therefore symmetric and needs no literal case:

    Γ ⊢ e₁ : Float[m₁]   Γ ⊢ e₂ : Float[m₂]   m₁ ≼ m   m₂ ≼ m   G ∈ {m₁, m₂}
    ---------------------------------------------------------------------- [Mul]
                          Γ ⊢ e₁ × e₂ : Float[m]

## 4. Division

`x / y` with `y` G-typed is multiplication by the `𝒢`-measurable number `1 / y`, so the
argument of section 3 applies verbatim and the numerator may be E. The convention for
`y = 0` does not matter for soundness as long as it is a fixed function of `y`: the
Lean uses `x / 0 = 0`; the paper still carries the note "Consider divide by 0".

An E denominator is unsound: for a non-degenerate positive `u`, Jensen's inequality
gives `E[1 / u] > 1 / E[u]`. With `u ~ Uniform(1, 2)`, `E[1 / u] = ln 2 ≈ 0.693` while
`1 / E[u] = 2 / 3 ≈ 0.667`.

The maximal sound rule replaces the paper's literal denominator by any G expression:

    Γ ⊢ e₁ : Float[m₁]   Γ ⊢ e₂ : Float[m₂]   m₁ ≼ m   m₂ ≼ G
    ---------------------------------------------------------- [Div]
                        Γ ⊢ e₁ / e₂ : Float[m]

This is the rule the sim and the Lean already implement.

## 5. The general principle

An operator may take an expectation-moded operand exactly in those argument positions
in which its result is affine in that argument once all other arguments are held
fixed. This single principle reproduces every existing rule:

- addition, negation and subtraction: every operand may be E;
- multiplication: one operand; division: the numerator;
- comparison: no operand, since a threshold is not affine, and its result drives
  control flow that G values depend on;
- distribution parameters: those in which the *mean* is affine may be E, namely the
  uniform bounds, the Gaussian mean, the Poisson rate and the gamma shape, but not the
  exponential rate, the beta parameters or the gamma rate, whose means are
  `1/λ`, `α/(α+β)` and `k/θ`. (The Gaussian variance is a curiosity: the mean does
  not depend on it at all, so the principle would even permit an E-moded variance.
  All four artifacts require G there, which is simpler and safe, and there is no
  reason to change it.)

## 6. Where each artifact stands

- Lean and sim implement the right-operand half of `[Mul]` and all of `[Div]`. Their
  soundness is machine-checked (`Theorems.lean`, standard axioms only).
- Paper and OCaml implement the literal fragment of `[Mul]` and a literal-only `[Div]`
  (OCaml additionally allows `G / G`).
- Neither side implements the full rule, and they differ in both directions: an
  E value times a G expression is accepted only by Lean and sim; a literal on the left
  of an E operand is accepted only by the paper and OCaml, where Lean and sim need the
  literal commuted to the right and typed at G.

## 7. Recommendation

**Option A, recommended: adopt the symmetric rules everywhere.**

- Paper: replace `Mul-G`, `Mul-ConstL` and `Mul-ConstR` by `[Mul]`, and the literal
  `c` in `Div` by an arbitrary `e₂ : Float[m₂]` with `m₂ ≼ G`. The soundness proof
  needs no new ideas; the symbolic semantics already multiplies an affine expression
  by a constant.
- OCaml and sim: the typing rule is symmetric, but inference with mode metavariables
  needs a deterministic choice when neither operand's mode is known yet. A sound
  strategy is: if one operand is a literal or already known to be G, use it as the G
  factor; otherwise default the right operand to G, which is what the sim and the
  Lean do today. This keeps every currently accepted program accepted and adds
  `x:E × y:G`, `y:G × x:E` and non-literal denominators.
- Lean: add the mirrored constructor `mul` with `left : Float[G]` and
  `right : Float[m]` to `Statement/Syntax.lean`'s `Typed`, then extend every case
  analysis on `Typed`: preservation in `Proof/Typing.lean`, `WellTyped` and
  `wellTyped_ofExpr_of_typed` in `Proof/Symbolic.lean`, the `mul` case of
  `symbolicReduce`, `typed_determinize` in `Proof/OrdinarySemantics.lean`, and the
  corresponding cases in `Proof/SymbolicSoundness.lean`. The mathematics is the
  mirror image of the existing case; the cost is proof engineering, on the order of a
  few hundred lines and several ten-minute rebuilds.

**Option B, cheapest: adopt the Lean and sim rule in the paper and OCaml.** No proof
work; the paper documents that a literal scaling factor goes on the right. It leaves
the visible asymmetry in the paper's rule, which reviewers will ask about.

Either way this is a four-artifact change (see `CLAUDE.md`), so it should be one
deliberate decision by the authors rather than a drift fix.

## 8. Other paper-side items from the audit

These do not concern typing but were found while comparing the Lean statements
against `tex/6_soundness.tex`:

- **Validity is a hypothesis, and it concerns E draws too.** The Lean theorems assume
  `DoesNotGetStuck source`: almost surely, at every reduction depth, no primitive is
  sampled outside its parameter domain. The paper's "valid G-trace" is informal and
  mentions only G draws. The hypothesis is necessary: for `x ~ Uniform(0, 1)` in
  mode E followed by `Uniform(x, 1/2)` in mode E, the source loses half its mass and
  has unnormalized expectation `∫₀^{1/2} (x + 1/2)/2 dx = 3/16`, while the
  determinized program evaluates `mean_Uniform(1/2, 1/2) = 1/2` with full mass.
- **Mean operators off-domain.** The paper's table defines `mean_D` unconditionally;
  the Lean makes it the zero measure outside the primitive's domain, which is what
  makes the example above consistent. The Lean also proves that a valid source never
  drives the determinized program off-domain, via convexity of the parameter domains.
- **`Uniform(a, a)`.** The paper's small-step table has rows for `v₁ < v₂` and
  `v₁ > v₂` only; the Lean takes the Dirac measure at `a`.
- **Integrability is a theorem, not an assumption.** The paper's tracewise theorems
  assume an integrable symbolic environment; the Lean proves that almost every fiber
  is integrable from validity alone, which also discharges "Mean valuation
  correctness".
- **Extended reals and Jensen.** The paper's global theorem in `[-∞, ∞]` and its
  Jensen corollary are now `extendedExpectationThm` and `jensenThm` in
  `Statement/Main.lean`, both proved in `Proof/Corollaries.lean`.
