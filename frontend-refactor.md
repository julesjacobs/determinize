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
