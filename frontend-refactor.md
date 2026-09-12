# Frontend AST refactor

## Parsed syntax — KEEP

Before: `Surface.node` combined a string tag, binder-name list, optional affinity,
and operand list. Elaboration checked constructor names and arities dynamically.

After: dedicated constructors encode arities and binders. Optional affinities
appear only on distribution syntax (including Boolean `flip`). `observe` carries
only its condition. The parser rejects wrong primitive arities and annotations
on `observe`; name resolution and weight validation remain in elaboration.

This makes missing constructor handling a compiler error and removes malformed
internal node combinations. The existing corpus retains its rejection stages.

Validation: warning-free build, Lean unit tests, all 117 corpus cases, independent
kernel certificates, and clean `codex review --uncommitted`.

## Resolved syntax — KEEP

Before: `Input` paired a `Core` with a preorder list of optional affinities.
Elaboration concatenated lists; inference consumed the list with mutable state;
certification separately checked erased-core equality and annotation-list alignment.

After: `Checking.Input` has dedicated source constructors and an optional affinity
at each sample site. Inference reads the annotation from the node. `Input.matches`
checks the candidate's constructor, payload, children, and affinity structurally.
`Certified input` exposes this correspondence alongside typing and source form.
Exported certificates reconstruct the resolved input and check the same claim in
the kernel. Name resolution, desugaring, and inference remain unverified.

The cost is a small source AST, variable-shifting traversal, and serializer.
The benefit is removing a representation invariant across three pipeline stages.
The mathematical `Spec.Expr` and determinization are unchanged. `sampleAffinities`
remains only for reporting and corpus assertions, with no certification role.

Validation: warning-free full build, Lean unit tests, all 117 corpus cases including
statistical ground truth, 24 independent kernel typing certificates, and 21 Python
integration tests (optional real Storm skipped). Model/result certificate tampering
checks passed. `codex review --uncommitted` returned no actionable findings.

The parsed and resolved refactors are separate commits. The previously completed
domain-safety change, bug fixes, and review records were committed separately
before the frontend work. Nothing was pushed.
