---
name: det-lang
description: Reference for the Lean .det language, sampling modes, checked determinization, execution, and shared corpus.
paths:
  - "tests/**"
  - "examples/**"
  - "**/*.det"
---

The maintained parser is `lean/Determinize/Frontend/Parser.lean`. Syntax includes
functions/recursion, let/if, pairs, sums, lists/matches, arithmetic/comparisons,
observations, and primitive distributions. See `lean/README.md` and parser tests
for accepted aliases and precedence.

Draws carry E/G annotations; inference supplies omitted annotations. Subtyping is
silent and structural. The transform replaces E draws by mean sites, preserving
evaluation of every operand. Means are rational functions of rational parameters:
uniform `(a+b)/2`, Gaussian `mu`, exponential `1/r`, gamma `a/r`, beta `a/(a+b)`,
Poisson/Bernoulli `p`, and discrete `sum p_i*i`. Discrete literal weights are
normalized. `flip` returns a Boolean and retains G sampling.

Comparison operands must be G; multiplication uses a G left operand in the core,
and division a G denominator. Frontend elaboration handles eligible product
reordering. See `lean/mul-div-typing.md`; do not copy simulator rules blindly.
`observe(false)` rejects with zero output mass. Exact expected rewards are not
conditional on acceptance. Lean's total arithmetic defines division by zero as zero.

- `./run.sh FILE.det`: checked annotation and determinization output.
- `./run.sh --samples N --seed S FILE.det`: numerical estimates and explicit rejections/failures.
- `./run.sh --result PREFIX --subject source FILE.det`: exact finite-model theorem.
- `./det.sh --all`: corpus, statistical, and certificate tests.

Add programs under tests/examples and register expectations in `tests/cases.toml`.
Use analytical ground truth, not generated random-output reports. If a program is
added to the browser examples, run simulator tests and regenerate its bundle.
