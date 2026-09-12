# Review of the local PR stack

Status: review complete; all three P2 findings fixed locally and follow-up review clean.

## Scope

No open GitHub PRs at review time. Branch `jujacobs/silent-subtyping` has 19 commits above fetched `origin/main` (`b3995d4b5fab60a8b5224eded02ac3301875fcf4`), ending at `a725b701380236eca30a7fc80c918983af4af95b`. Six implementation/migration commits are followed by the specification-refactor plan, eleven retained refactor commits, and the closeout record. The uncommitted domain-safety trial is separate.

The built-in review ran `codex review --base origin/main` from a clean detached checkout at `/tmp/determinize-stack-review`, so uncommitted work is excluded. Its output is `/tmp/determinize-stack-codex-review.log`. Local review covers workflow migration, certificate acceptance/semantic boundaries, public theorem changes, and the identified frontend/runtime defects.

## Confirmed findings

### P2: CLI variance suffers cancellation and conceals overflow

`lean/Main.lean:72–77` accumulates raw squares and subtracts the squared mean. A constant program `1000000001` with `--samples 1000` prints empirical variance `18304.000000` instead of zero. This reproduction needs no extreme exponent or overflow. The earlier two-valued `1e200` fixture also demonstrates that the clamp can conceal a nonfinite calculation as zero. Use centered accumulation and explicitly handle nonfinite statistics before clamping. This is an executable reporting defect, independent of the mathematical variance theorem.

Introduced in `4dc3efb`; unchanged by the domain-safety trial.

### P2: Literal multiplication normalization is not idempotent

`lean/Determinize/Frontend/Elaborate.lean:76–79` swaps whenever the right operand is a literal, including when both are literals. Compiling/printing/recompiling `2 * 3` alternates `(3 * 2)` and `(2 * 3)`, failing the structural roundtrip property used by `Tests/Parsing.lean`. Keep operand order when both are literals. Preserve structural and explicit-affinity checks rather than replacing them with numeric equality.

Introduced in `4dc3efb`; independently reproduced during the preceding Pro assessment; relevant source is unchanged. The numeric result itself remains correct.

### P2: Corpus affinity expectations are silently skipped

`lean/Tests/Corpus.lean:33` expects the JSON field `affinities`, but `tests/cases.toml` supplies `modes` and `tests/run.py:112–120` forwards it unchanged. The optional field decodes as none, bypassing the assertion at Corpus.lean:107–109. All four fixtures with per-site expectations are affected: variance-operand and mixed-list/pair/sum.

Independent Codex review identified this and reproduced decoding as `Except.ok none`. I also ran the actual corpus runner with an intentionally wrong `[G,G]` expectation for variance-operand: the unmodified `modes` key passed; translating it to `affinities` failed with `expected affinities [G, G], got [E, G]`. Align the schema or translate the key, and test that wrong expectations fail.

Introduced by `540ab43` when renaming modes to affinities in Lean but not in the manifest/adapter. The reported 117 passing corpus cases do not establish these four skipped affinity assertions.

## Validation

- Fresh Python integration run: 17 tests, successful; optional real Storm test skipped.
- Immediately preceding domain-safety trial: `lake build --wfail`, Lean tests, all 117 corpus cases, and all 24 independent generated typing certificates passed. All 41 axiom reports use only standard axioms. This evidence is for the current working tree, not a fresh rebuild of every committed boundary.
- The stack's existing refactor record documents prior builds and checks of retained steps; those historical claims have not all been rerun in this review.
- Committed and working-tree diff whitespace checks pass.

## Review outcome and stack structure

`codex review --base origin/main` exited 0 with the affinity-schema finding; it did not report the two previously known executable findings. All three are accepted after independent checks. No findings were rejected. This is not a clean review despite the review process exiting successfully.

The fetched main is an ancestor of HEAD (0 commits behind, 19 ahead); no rebase is currently needed. Migration and follow-up refactors are separated into commits. The two first-commit bugs can be fixed independently; the schema regression belongs with the affinity rename. The domain-safety trial remains uncommitted and is not part of the reviewed 19-commit stack. I found no additional defect in that trial; its separate validation is in `domain-safety-trial.md`.

The clean detached checkout lacked Lean build artifacts, so the automated reviewer did not run full Lean tests there. Its simulator attempt had 35 passing tests and a missing `@codemirror/state` dependency failure; this is an environment limitation, not an accepted code regression. Working-tree build/test evidence above is distinct from exact-HEAD and per-commit testing. The temporary review checkout was removed after review.

No fixes, commits, or pushes were made. The findings remain open for a subsequent repair pass.


## Follow-up repairs

All three findings now have local fixes and regressions:

- Manifest validation, TOML cases, and documentation use `affinities`, matching the Lean decoder. A wrong-affinity test fails while the correct expectation passes.
- Multiplication preserves operand order when the left operand is already a literal. Repeated exact-core roundtrips cover two literals, an E draw multiplied by a literal, and nested multiplication with an explicit G draw.
- CLI statistics use centered online accumulation with the original population denominator. Nonfinite statistics produce an explicit unavailable/overflow result. CLI tests cover constant cancellation, population variance, observation rejection, and variance overflow.

Validation passes: `lake build --wfail`, Lean tests, all 117 corpus cases and independent kernel checks, and 21 Python tests (optional real Storm test skipped). `codex review --uncommitted` exited 0 with no actionable findings; no findings were rejected and no extra review was run. Review log: `/tmp/determinize-fixes-codex-review.log`. The earlier domain-safety trial remains separate in the working tree. No commits or pushes requested or made.
