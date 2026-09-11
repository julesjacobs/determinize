# Silent structural subtyping

The experiment replaces `Expr.promote` with a subsumption rule on the unchanged
expression:

```text
Γ ⊢ e : A    A <: B
──────────────────
     Γ ⊢ e : B
```

`Ty.Sub` permits `Float[G] <: Float[E]`, covariant products, sums and lists,
and contravariant function arguments with covariant results. There is no
`Float[E] <: Float[G]` rule. Sample annotations remain part of the expression:
using a G draw at type E does not change that draw into an E draw.

## Options

| Design | Program representation | Proof work |
| --- | --- | --- |
| Scalar promotion | Explicit float wrapper; compound conversions need elaboration | Existing baseline, but cannot directly express the paper's structural subsumption |
| General explicit promotion | A wrapper for every structural subtype | Requires structural typing and canonical-form changes, plus wrapper evaluation, trace and determinization lemmas |
| Silent structural subtyping | No expression change; subsumption belongs to the derivation | Requires structural typing and canonical-form changes; no wrapper semantics |

Silent subtyping is the simplest fit for this language. G and E values have the
same runtime representation, and determinization already depends only on sample
annotations. Explicit wrappers would add execution steps without contributing to
the transform.

## Proof changes

The symbolic typing judgment has the same subsumption rule. Its crucial invariant
survives: a symbolic real typed G has zero coefficients on E samples. Inverting a
subtype whose target is `Float[G]` forces its source to be `Float[G]` too.

Functions require care. A recursive closure widened from `A → B` to `C → D`
still checks its body with its original recursive self type `A → B`. The revised
canonical-form lemmas retain `A`, `B`, the original body typing, `C <: A`, and
`B <: D`. Application subsumes the argument to `A`, substitutes using the original
self type, and subsumes the result to `D`. This avoids an invalid claim that the
recursive body can simply be rechecked with its widened self type.

Substitution and reduction preservation propagate subsumption. Semantic commuting
proofs reuse the induction hypothesis because the expression is unchanged. The
compact-trace theorem assigns a G program type E through subsumption; the old
promotion-step and trace-erasure lemmas disappear.

## Implementation boundary

The unverified frontend emits `Certificate.sub target child`. The proof-producing
checker recursively checks the same expression with the child certificate and
checks `Ty.Sub child.ty target`. A successful check contains an actual `Typed`
proof. The Lean determinization transform then operates directly on that checked
expression.

Input preservation now erases only sampling modes. There are no inserted
conversions to erase. Exported certificates are independently checked using
`decide +kernel`.

Inference generates structural mode constraints and fresh modes at newly resolved
type shapes. It remains monomorphic; completeness and optimality are not proved.
The mathematical theorems retain their domain-safety and integrability conditions.
Parsing, inference and numerical execution remain unverified.

## Result

The implementation passes the full warning-free Lean build, including expectation
preservation, trace soundness and Jensen's inequality (which yields variance
non-increase). The exported theorems retain only `propext`, `Classical.choice`, and
`Quot.sound`; no axioms or proof placeholders were added.

`lean/test.sh` passes all 46 example/fixture programs, the inference and negative
certificate tests, and seven independently kernel-checked exported certificates.
The recursive-function fixture also completes 100 source and 100 determinized
numerical runs, including recursive calls. The paper's LaTeX build is up to date.

The prior implementation is committed as `a0c8167`. This experiment is on
`jujacobs/silent-subtyping`.
