# Storm reward construction consultation

Conversation: https://chatgpt.com/c/6aaeec02-85e4-83ea-be66-940d8463a00c
Model: GPT-6 Pro (verified in browser).
Date: 2026-09-19.

Objective: design finite control Markov reward construction for additive recursion,
preserving expected terminal output including rejection and nontermination;
identify unbounded-loop evaluation examples. Begin with the geometric recursion,
then consult on the repository implementation and certificate boundary.

Status: complete. Both Pro replies were read and substantively saved. General
affine design synthesized in affine-design.md. Exact Storm experiments pass;
no production implementation changes. Five-minute monitor paused on completion.

User refinement: design must handle an affine context multiplying the recursive
result and nonconstant left operands of addition; avoid a constant-addition special
case as the conceptual design. The next Pro follow-up will ask for a general affine
continuation abstraction and a precise boundary between Markov rewards and weighted
moment equations, including integrability.

Further user framing: algebraically simplify residual programs, moving constant
additions/scalings onto edges of the graph that sets up equations. Adopt this as
the presentation: nodes are reusable residual computations, edges carry affine
uses of their results. Constant can mean evaluated relative to the residual
computation, not only a syntactic literal. Existing general-affine Pro follow-up
covers this architecture; do not interrupt it to duplicate the question.


## Addition-only implementation planning

User requested Pro to make a build plan using all Lean context.
Submitted a follow-up in the same verified GPT-6 Pro conversation; generation
visibly started. Attached 300 first-party source/config/test/example files
(1584355 bytes), including the entire Lean development.
External Mathlib source/build artifacts excluded; pinned dependencies included.
Manifest: addition-context-manifest.txt.
Bundle SHA256: bc7dbce6d680cd1505fdad4ea7ff6ae18322aec523efc39908b95bf303e27bcc.
Repository HEAD: a9031e018a01b19788ba7d477a7a9cf4704de570.

Scope: additive translation of returned values only; concrete arithmetic remains
available, but no general scaling/affine-matrix extraction. Ask for concrete
normalization and replay, proof dependency order/reuse, model and integrability
proofs, Storm/export/checker integration, tests, and separately scoped accumulator
extraction. No production implementation requested.

Status: complete. Pro returned its addition-only build plan after 16m12s.
Substantive answer saved in pro-addition-plan-response.md; source-checked, detailed
implementation plan saved in addition-plan.md. Referenced declarations were checked,
and the coin_flip_unif limitation was independently reproduced. No production code
changes or feature implementation. Monitor paused after completion.
