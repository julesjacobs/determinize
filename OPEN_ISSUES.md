# Open issues

Unresolved paper work and deferred implementation. The paper's remaining TODOs
stay in its sources until the underlying work is resolved.

## Paper

- Replace the artifact URL placeholder in `tex/sections/1_introduction.tex` and the
  `github.com/[anonymized]` link in `tex/sections/7_evaluation.tex`. The review build
  points to the anonymized archive (`artifact/build.py`), submitted as supplementary
  material; its Lean marks link to an appendix that locates their declarations in it.
- State the Artifact Evaluation plan in `tex/sections/data_availability.tex`, which
  OOPSLA requires.
- Supply the trace-indexed kernels, compatibility lemma, and derivation of
  tracewise soundness promised by Section 5 and the appendix. Review the derivation
  of the global results from tracewise soundness, the symbolic invariants and
  induction, and the normalization convention in the prose.
- Add the full benchmark programs to the appendix. Review its original typing
  figure and remove the editorial note once its role is settled.
- Finish the related-work comparisons with approximate inference, delayed sampling,
  semi-symbolic inference, and expectation programming. Review the paragraph on
  quantitative bounds and the coverage of mechanized semantics, verified samplers,
  and certified probabilistic model checking.
- Replace the estimator-variance PNGs with vector plots and permit those outputs
  in `.gitignore`. Preserve the committed data used to reproduce them.
- Resolve the open paper–Lean differences in
  [PAPER_DEVIATIONS.md](lean/PAPER_DEVIATIONS.md), including the scope of the
  list-valued random-walk example, concrete syntax, and checked benchmark certificates.

Before submission, recheck the earlier implementation review's concerns against
the final paper: source-to-target transfer hypotheses and which the tool checks;
the specification review and trust model; coverage of every mechanization claim;
the finite-model and additive-extraction boundary; and the architecture figure's
legend and space use. Check benchmark counts and table/prose numbers against the
same recorded runs, document G/E random streams and paired seeds, and assess
confidence intervals for variance-reduction factors. These are final checks;
earlier findings may already have been addressed by subsequent edits.

## Deferred implementation

### Portable certificates and solving

Portable kernel checking remains a scaling concern. Profile the current checker
before adding shared elimination, matrix factorization, or verified strongly
connected component analysis. Existing development timings are not paper benchmarks.
Retain the distinction between a proved successful result and algorithmic completeness.

### General affine residualization

The implemented additive extraction does not cover general affine return contexts
`a*x+b`. A proposed extension must preserve the joint distribution of coefficients
and successor control state, evaluation order, rejection, and domain checks.
Affinity in E samples does not establish affinity in a recursive result.

Weighted moment equations need separate integrability evidence: almost-sure
termination and a rational equation solution do not suffice. First and second
moments can have different integrability conditions. A scaled Storm chain would
encode the weighted equations; its termination probabilities would not be those
of the program. Persistent random coefficients need conditional treatment.

### Accumulator extraction

Growing environment accumulators still require a separate translation-equivariance
pass. A proposed initial fragment uses direct tail calls, updates `a+r` independent
of `a`, and terminal output `a+c`, retaining finite control. The accumulator must
not affect guards, sampling parameters, divisors, scaling, or escaping closures.
Unbounded pending calls and irrelevant unbounded computations need their own
abstraction. Residual stochastic continuous calls require separate marginalization.

### General mutable references

General `ref τ`, including nested references and stored functions, is a proposed
extension with no implementation or Lean proof. Reference subtyping must preserve
the complete content type. Aliases, cyclic stored closures, and active computation
must share one symbolic E-sample history; reading a cell twice reuses the same
sample, and writes update all aliases. Closures capture locations.

The proof work includes heap/store typing and preservation, whole-configuration
realization and determinization, weakening stored code after new draws,
measurability, heap-aware trace factorization, and all existing public corollaries
under their current assumptions. Machine equality, reification, replay, and
serialization must include the heap. General references do not ensure finite
reachable state space. Moving deferred reads or effectful calls requires explicit
effect reasoning.

A proposed regression combines aliases, nested references, cyclic stored functions,
and later draws: outputs `0,1,2,3` with probabilities `(9,15,7,1)/32`, mean `1`,
variance `5/8`, and determinized output `1`. Also distinguish a closure that
captures one sample from a closure that samples on each call.
