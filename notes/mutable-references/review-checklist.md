# Independent review checklist

The following are review questions, not conclusions from Pro.

- Current `Spec.Main.mainThm` preserves expectation, domain safety and integrability; Jensen, mass, variance and conditional results are separate public claims. Reference migration must preserve all of their stated assumptions or explicitly state changed assumptions.
- Current `Proof.Soundness` factors source output through trace fibers and target output through fiber means. The generic measure arguments should survive once a heap-aware factorization is established; the difficult work belongs below that boundary.
- Reading an E cell twice must reuse the same affine symbolic coordinate. Updating a cell must update all aliases, and G/E reference subtyping must be invariant unless separately justified.
- A reference whose contents affect control flow, sampling parameters, address selection, or observation cannot safely contain an erased E sample under the present architecture.
- Pointwise equality of heap means alone is too weak if correlated E values may multiply. A shared symbolic environment plus static restrictions must account for dependencies across cells and closures.
- Full reference semantics does not imply finite-state extraction: allocation counters, unreachable heaps, and changing numeric contents can each cause unbounded states.
- Additive outer numeric-frame extraction need not read a heap; evaluating a postponed RHS earlier requires explicit effect reasoning and cannot rely on deterministic evaluation alone.
