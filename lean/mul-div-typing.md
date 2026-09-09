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
| `lean/` (`Typed.mul`, `Typed.div`) | left operand G, right operand at the result mode `m`, result `m` (the left-G half of the symmetric `[Mul]` of section 3) | as the sim |

So the sim and the Lean each accept an E operand next to an arbitrary G operand, which
the paper and the OCaml reject unless the G operand is a literal, but on opposite sides:
the sim types `x : E` times `y : G` at mode E, the Lean `y : G` times `x : E`. A literal
on either side of an E operand (`2 × x`, `x × 2`) is accepted by the paper and the OCaml,
by the sim only as `x × 2`, and by the Lean only as `2 × x`.

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

### The pre-sampled G-tape

Conditioning on `𝒢` has an elementary reading that needs no σ-algebras. In program
order the two kinds of draws are interleaved: an E draw, then a G draw whose
parameters use earlier values, then another E draw, and so on, and the determinized
program follows the same order, substituting at each E draw the mean computed from
the values available at that point, G draws already made included. For the
comparison, pretend instead that the entire G-trace was sampled up front and written
on a tape, and that both programs read their G draws off that tape. Conditioning on
`𝒢` then means: fix one tape and compare the two programs on it. For a fixed tape the
determinized run is a single number, and the source run is a distribution over the E
draws alone, whose mean must equal that number.

Moving every G draw to the start is a genuine reordering of the sampling process,
and fact 1 is what makes it legitimate. In general a draw cannot be moved earlier
without changing the joint distribution, because a later draw's parameters may depend
on earlier values. But no G draw depends on an E draw, so the G draws have the same
joint law whether sampled in program order or all at once, and the E draws between
them see the same G values either way. The G-trace is a self-contained random object
that the E draws read from and never write to. This is also why fixing the *whole*
tape, including G draws that occur later in program order, does not bias the E draws
that precede them: a later G draw would carry information about an earlier E draw
only if it had depended on it.

Reversing the direction breaks the argument. With `y ~ Uniform(0, x)` at mode G and
`x` at mode E, fixing `y` carries information about `x`, so the average of `x` over
the runs with that `y` is not `E[x]`, and substituting the unconditional mean for `x`
is wrong. The mode system forbids exactly this dependence.

The artifacts implement the tape literally: the sim's coupled-trace runtime runs both
programs on one shared stream of G draws, and the Lean's trace is the list of G draws
only, with `Traces.soundnessThm` stating that for almost every such list the source's
fiber over it is a probability measure whose mean is the determinized output.

### Why no G draw depends on an E draw

Fact 1 is a property of the typing rules, proved by induction on the typing
derivation: no rule manufactures a `Float[G]` from anything that touches an E value.
Walking through `Typed` in `Statement/Syntax.lean`:

- The only mode change is `promote`, and it goes from G to E. Types are exact in the
  core language, so a G position never silently accepts an E value.
- Every rule with a `Float[G]` conclusion has only G float premises. Addition and
  negation keep their mode. Multiplication and division at mode G need both operands
  at G, since the non-G operand must match the result mode. A G draw is a primitive
  rule at mode G: the parameters in which the mean is affine (the uniform bounds, the
  Gaussian mean, the Poisson rate, the gamma shape) are typed at the draw's mode, hence G,
  and the remaining parameters are always G. So the parameters of a G draw are G values.
- Control flow cannot leak an E value either. The only way to obtain a `bool` from a
  float is `lt`, which demands two G operands; conditionals and matches branch on
  booleans and sum or list tags, none of which can encode an E float. Which branch
  runs, and therefore which G draw happens, is decided by G values.
- Data structures and functions carry modes componentwise. A pair, list or closure
  may contain E values, but projecting a `Float[G]` component yields a G value by the
  pair's type, and a function of type `Float[E] → Float[G]` can only build its result
  from G things. The E argument may sit in the closure but never reaches the G result
  through data or control flow.

In the Lean this is `WellTyped.gconstant` in `Proof/Symbolic.lean`. The symbolic
language represents every float as an affine expression, a constant plus coefficients
on the E draws made so far, and `GConstant` states that every G-moded float literal
has zero coefficients. The typing rule for a G literal carries that as a side
condition, and every other case follows by induction because no rule mixes an E
premise into a G conclusion. The symbolic semantics preserves well-typedness, so the
invariant holds at every step of every run, in particular when a G draw reads its
parameters.

This is exactly what the multiplication and comparison rules pay for. Allowing
`E × E` or `E < c` would not merely break the affine invariant on the E side; it would
let a G value or a branch depend on an E draw, and the pre-sampled tape argument would
collapse.

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

- Lean implements the left-G half of `[Mul]` (`Typed.mul`: `left : Float[G]`,
  `right : Float[m]`) and all of `[Div]`; its soundness is machine-checked
  (`Theorems.lean`, standard axioms only). It had both halves on 2026-09-08
  (`Typed.mulLeftG`); the second rule was dropped on 2026-09-09 so that `Typed` has one
  rule per constructor and checking against a type is syntax-directed. The sim implements
  the right-G half of `[Mul]` and all of `[Div]`.
- Paper and OCaml implement the literal fragment of `[Mul]` and a literal-only `[Div]`
  (OCaml additionally allows `G / G`).
- The four artifacts differ pairwise: an E value times a G expression is accepted only by
  the sim; a G expression times an E value only by the Lean; a literal on either side of
  an E operand by the paper and the OCaml, where the sim needs the literal on the right
  and the Lean on the left, typed at G.

## 7. Recommendation

**Option A, recommended: adopt the symmetric rules everywhere.**

- Paper: replace `Mul-G`, `Mul-ConstL` and `Mul-ConstR` by `[Mul]`, and the literal
  `c` in `Div` by an arbitrary `e₂ : Float[m₂]` with `m₂ ≼ G`. The soundness proof
  needs no new ideas; the symbolic semantics already multiplies an affine expression
  by a constant.
- OCaml and sim: the typing rule is symmetric, but inference with mode metavariables
  needs a deterministic choice when neither operand's mode is known yet. A sound
  strategy is: if one operand is a literal or already known to be G, use it as the G
  factor; otherwise default one fixed side to G (the sim defaults the right operand,
  the Lean the left). This keeps every currently accepted program accepted and adds
  `x:E × y:G`, `y:G × x:E` and non-literal denominators.
- Lean: the symmetric rule was in place on 2026-09-08 (`Typed.mulLeftG` mirroring
  `Typed.mul`, the symbolic `WellTyped.mulGE` mirroring `WellTyped.mulEG`, about 150
  lines of mirrored induction cases in `Proof/Typing.lean`, `Proof/Symbolic.lean`,
  `Proof/SymbolicSoundness.lean` and `Proof/OrdinarySemantics.lean`; commit f9cc30d)
  and was reduced to the left-G half on 2026-09-09 to keep `Typed` syntax-directed.
  Restoring it is that commit again; `Affine.mul?` accepts a constant factor on either
  side, so no new proof idea is needed. `Proof/Examples.lean` checks `Traces.soundness`
  on `let y = Uniform_G(0, 1) in y × Uniform_E(0, 1)`.

**Option B, cheaper for the paper: adopt one asymmetric rule everywhere.** The sim has
`left : m, right : G`, the Lean `left : G, right : m`; the paper and the OCaml take one
of the two, and the artifact on the other side flips (for the Lean that is the mirror
commit above). No new proof idea is needed, and the paper documents that a scaling
factor goes on one fixed side. It leaves the visible asymmetry in the paper's rule, which
reviewers will ask about.

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
