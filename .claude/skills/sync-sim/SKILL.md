---
name: sync-sim
description: Compare and maintain the Lean implementation, paper, and unverified browser simulator after language changes. Record deliberate differences explicitly.
paths:
  - "lean/Determinize/**"
  - "sim/src/compiler/**"
  - "sim/src/runtime/**"
---

## Correspondence

| Lean | Simulator |
|---|---|
| `Spec/Syntax.lean`, `Spec/Types.lean` | `src/compiler/ast.js`, `types.js` |
| `Frontend/Parser.lean`, `Elaborate.lean`, `Infer.lean` | `src/compiler/lexer.js`, `parser.js`, `infer.js` |
| Core `Expr.determinize`, `Frontend/Pretty.lean` | `src/compiler/determinize.js`, `pretty.js` |
| `Spec/Primitives.lean`, `Runtime/` | `src/runtime/distributions.js`, `semantics.js`, `rng.js` |
| `Proof/Symbolic*.lean`, `Spec/Traces/` | `src/runtime/semantics.js`, `affine.js` |
| `Finite/`, `Checking/FiniteModel.lean`, `Checking/Result.lean` | No certified simulator counterpart |

These are separate implementations. See `migration-audit.md` and
`lean/mul-div-typing.md` for existing differences; do not assume equivalence.

1. Read the relevant Lean specification, implementation, simulator code, and paper rule.
2. Preserve the formal theorem premises and explain any intended semantic difference.
3. Add focused tests to Lean and the simulator for changed behavior. Register shared
   `.det` fixtures in `tests/cases.toml`; avoid using sampled agreement as ground truth.
4. Run `./det.sh --all` and `(cd sim && npm test)`.
5. After simulator source changes, rebuild with `npm run build` and bump `index.html`'s
   cache-buster if the bundle changes. Do not deploy.
