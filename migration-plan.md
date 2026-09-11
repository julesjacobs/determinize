# Complete migration to Lean and certified Storm results

Work stays local. The completed migration is organized into local commits for
review with user authorization. Do not push until requested. Preserve the existing theorem
assumptions; do not use axioms or proof placeholders to bridge missing results.

## 1. Complete discrete language support and observation handling

- [x] Define checked, normalized rational finite distributions shared by the frontend and finite-state explorer.
- [x] Prove nonnegativity, total mass one, and the finite weighted-mean formula.
- [x] Define the corresponding real-valued probability measures and prove their mean and integrability properties.
- [x] Integrate both distributions into `Op`, parameter domains, primitive kernels, affine means, domain convexity, and first-absolute-moment bounds.
- [x] Add Bernoulli sampling with explicit E/G modes to core syntax, semantics, typing, and determinization.
- [x] Add finite discrete sampling with explicit E/G modes. Initially support literal rational weights and numeric outcomes; retain the current surface interpretation of `discrete(w0,...,wn)` as weights for outcomes `0,...,n`.
- [x] Specify which parameters can depend on E values. Bernoulli's mean is affine in its probability, but its parameter domain is restricted; a random parameter must satisfy the existing safety assumptions. Keep discrete weights literal initially.
- [x] Extend symbolic semantics, trace semantics, measurability, typing preservation, and full soundness proofs for both primitives.
- [x] Extend inference, proof-producing certificates, pretty printing, and numerical execution. Do not implement E draws by prematurely replacing them with their means during elaboration.
- [x] Add a runtime observation-rejection outcome distinct from fuel exhaustion and invalid parameters. Preserve rejected paths' zero output mass and evaluate conditions once.
- [x] Establish the rejection representation's connection to the formal semantics. Do not identify arbitrary divergence with observation failure.
- [x] Add exact, support, statistical, invalid-domain, mode-rejection, and independently kernel-checked certificate tests. Include zero weights, endpoint probabilities, and nested uses.

Acceptance: discrete E/G programs run through the existing checked pipeline; exported
soundness theorems retain their assumptions and standard axiom dependencies. Failed
observations terminate promptly in the runtime and are reported separately from
numerical failure and divergence. Conditional expectations are not claimed here.

## 2. State the end-to-end contract and model format

- [x] Define a finite rational Markov chain with an initial state, terminal rewards, and rejection states.
- [x] Fix the queried quantity: expected terminal reward, with zero contribution from paths that never terminate or are rejected.
- [x] Specify exact rational arithmetic and output interpretation. Define the initial exporter policy excluding residual continuous distributions and unsupported arithmetic.
- [x] State the model-certificate checker soundness proposition in the reviewer-facing statement layer.
- [x] State the result-certificate checker soundness proposition, including the termination/uniqueness conditions needed for a linear-system solution to determine the expectation.
- [x] Define the final theorem connecting an accepted program/model/result certificate to the program's expected terminal reward. Make clear whether the result describes the source or determinized program; apply determinization soundness only when its assumptions are supplied.

## 3. Build an unverified finite-state explorer in Lean

- [x] Define an executable small-step machine with canonical states, closures/environments, and exact rational values.
- [x] Enumerate successors and aggregate duplicate edges with exact rational probabilities.
- [x] Explore reachable states with resource limits and deterministic state numbering.
- [x] Distinguish complete exploration from state-limit exhaustion. Never certify a truncated graph as the complete program.
- [x] Emit model-certificate candidate data containing states and transition evidence tags (unverified until point 4).
- [x] Write Storm `.tra`, `.lab`, and reward files and a Lean CLI command to produce them.
- [x] Test finite loops, branching, recursion, rejection, duplicate successors, unsupported distributions, and incomplete exploration.

## 4. Verify the exported model

- [x] Build a proof-producing checker for individual machine transitions (relative to the executable machine; its paper-semantics correspondence is below).
- [x] Check initial-state alignment, probability normalization, rewards, successor coverage, and closure of the reachable graph.
- [x] Reify values, closure environments, and continuation stacks as paper expressions.
- [x] Prove environment expansion agrees with one- and two-binder substitution.
- [x] Connect deterministic root operations and bookkeeping transitions to paper expressions.
- [x] Prove context lifting under the primitive-arity invariant and stack-dropping rejection soundness.
- [x] Check source scope and prove accepted initial states reify to the selected paper program.
- [x] Prove supported sampling transitions agree with the paper's real-valued primitive measures (under the continuation-shape invariant).
- [x] Establish reachable continuation shape and a finite bound on consecutive bookkeeping transitions.
- [x] Prove the machine's execution corresponds to the formal program semantics.
- [x] Prove accepted finite model certificates preserve expected terminal rewards under the stated conditions (via full output-law equality).
- [x] Add negative certificates for omitted successors, changed probabilities/rewards, malformed states, and truncated exploration.

## 5. Integrate Storm and certify answers

- [x] Invoke Storm on exported models, recording its version, options, and completion status.
- [x] Convert candidate answers into exact rational certificates, using rational reconstruction or an independent rational solver if needed. Floating-point answers alone are not certificates.
- [x] Check the terminal equations and transient equations `v = r + Pv` exactly.
- [x] Certify absorption/uniqueness for the initial supported class. For example, provide a finite-step probability bound proving eventual absorption. Linear equations alone do not establish the least solution in the presence of nontermination.
- [x] Prove that an accepted result certificate gives the requested expected terminal reward.
- [x] Add examples with known answers and malformed candidate solutions, including a nonabsorbing counterexample whose equations have multiple solutions.
- [x] Independently kernel-check exported end-to-end certificates.
- [x] Document residual trust: input parsing/desugaring, the reviewed specification, and any assumptions not discharged by the certificate.

## 6. Finish replacement and remove OCaml

- [x] Establish a shared `.det` corpus independent of OCaml.
- [x] Preserve the original programs and OCaml output baselines.
- [x] Cover every required OCaml/Storm use case with Lean tests, documenting deliberate behavioral differences.
- [x] Decide whether the standalone symbolic-coupling CLI needs a replacement beyond existing proofs and the browser simulator.
- [x] Remove OCaml source and archived outputs once no longer needed; history retains the original implementation.
- [x] Replace/remove `run.sh`, `det.sh`, OCaml Nix shells, the root `.envrc`, and OCaml-specific hooks, scripts, skills, and documentation.
- [x] Run the full Lean, corpus, Storm, simulator, and paper verification workflows.

## 7. Organize the completed migration for review

- [x] Review the full final diff for accidental or unrelated changes.
- [x] Split into six buildable commits on current main: frontend, checked language, and proofs; shared corpus; finite-model specification/exploration; model correspondence; result certificates and Storm; OCaml retirement and workflows.
- [x] Re-run checks at commit boundaries where practical.
- [x] Present the local commit series for review. Do not push until requested.

## Later extensions

- [ ] Certified intervals for results that cannot conveniently be reconstructed as exact rationals.
- [ ] Least-solution bounds for nonabsorbing models.
- [ ] Conditional expectations with certified positive acceptance probability.
- [ ] Richer finite distributions, state abstractions, and additional Storm properties.

## Progress

The first increment is implemented in `lean/Determinize/Statement/FiniteDistribution.lean`,
`Proof/FiniteDistribution.lean`, and `Checking/FiniteDistribution.lean`. The existing
G-mode discrete elaboration uses the checked normalizer. Unit tests cover invalid
weights, scale invariance, preserved indices, Bernoulli endpoint laws, and exact means.

The real-valued finite and Bernoulli laws are defined in `Statement/Primitives.lean`
and `Statement/FiniteDistributionMeasure.lean`. The corresponding proof modules
establish probability mass, integrability, means, Bernoulli variance, endpoint behavior,
and measurability in the Bernoulli probability parameter.

Observation rejection now has an explicit core term, propagated through typing,
determinization, certificates, symbolic semantics, and the numerical runtime.
`Proof/Rejection.lean` proves its zero-output-mass semantics, including failed
observations and rejected let-bound computations. The runtime and CLI distinguish
observation rejection from fuel exhaustion. Core Bernoulli/discrete E/G integration is complete; surface forms now elaborate
to their own stochastic core terms.

The shared primitive interface now includes Bernoulli and literal finite distributions.
Bernoulli has one affine parameter, no general parameters, domain `0 ≤ p ∧ p ≤ 1`,
and mean `p`. Its probability parameter may therefore have the draw's E/G mode;
validity remains a source-safety obligation. Finite discrete weights are checked,
normalized rational data carried by `Op.discrete`, with no expression parameters.
Its mean is the constant weighted outcome index. These choices preserve the countable
primitive labels used by trace measurability. Boolean `flip` remains G-only.

`primitiveLaws` and `primitiveMomentBounds` now cover both distributions, including
invalid Bernoulli probabilities and the constant mean of finite distributions.
The symbolic soundness proof also establishes that Bernoulli's parameter domain is
preserved by convex combinations; finite discrete distributions have no variable
parameters.
`Tests/PrimitiveLaws.lean` checks the shared interface, wrong operand counts, invalid
probabilities, stochastic means, and mean fibers. Expression constructors, typing, symbolic reductions, trace semantics, certificates,
inference, pretty printing, and numerical execution now support both primitives.
`flip` elaborates to a G-mode Bernoulli comparison. The source terms retain their
stochastic laws until the Lean determinization transform changes E sites to mean
sites. Discrete pretty printing rescales probabilities to literal integer weights;
kernel exports contain exact probabilities with kernel-checked validity proofs.

Point 1 passes the full warning-free Lean build, six harness failure-detection tests,
110 corpus cases (19 statistical cases, each with 20,000 source and target runs),
and 18 independent kernel certificate checks. The exported soundness theorems retain
only `propext`, `Classical.choice`, and `Quot.sound`. TeX builds successfully.
New cases cover endpoints, zero weights and retained indices, invalid domains and
weights, E/G restrictions, nested functions and draws, mixed-mode variance, changed
certificate data, and stochastic-source alignment. Exact decimal pretty printing
also preserves literal structure when reparsing terminating rational literals.
The finite rational model and expected-terminal-reward contract are now implemented.

Point 2 is defined in `lean/Determinize/Statement/FiniteModel/`, with design details in
`lean/finite-model-contract.md`. Models have exact rational transitions, absorbing
returned/rejected states, and unnormalized output measures. Result certificates
specify exact value equations and a uniform finite-step absorption bound. Model
checker soundness includes source/target selection, output-law equality, and
non-stuckness. The result-checker contract requires both certificate validity and
the actual expected reward; the proof linking those conditions remains in point 5.

`Proof/FiniteModel/` proves monotonic finite-horizon output, agreement of rational
reward iteration with the real integral, and composition of the future checker
soundness guarantees. The source corollary retains all existing determinization
premises. These composition theorems do not implement or establish either checker.
Tests cover once-only signed rewards, rejection mass, valid and malformed algebraic
certificates, and the spurious equation solutions of a nonterminating self-loop.
Point 3 implements the exact rational state explorer described below.

Point 2 validation passes: warning-free default Lean build, six harness tests, all
110 corpus cases in fast mode, 18 independent kernel certificates, and the TeX
build. The new specification examples are checked by Lean, including exact
certificate conditions and infinite-horizon output laws for the small models.

Point 3 is implemented in `lean/Determinize/Finite/`: a rational small-step
machine, deterministic breadth-first exploration, resource limits, candidate
serialization, and Storm transition/label/signed-reward files. The CLI accepts
`--export PREFIX` and `--subject source|determinized` (default determinized).
Incomplete exploration produces no candidate or export. The candidate includes
machine states and transition evidence tags; the verified model checker remains
point 4. Successful exploration does not establish termination.

Explorer tests cover exact rational rewards and means, lexical closures, list/sum
matching, recursion, finite probabilistic cycles, nontermination, rejection,
zero-probability paths, duplicate edges, errors, and all three resource limits.
Python integration tests solve exported equations independently against known
answers, check signed once-only rewards, recompile and replay generated Lean data,
and ensure failed exploration emits no files.

Point 3 validation passes: warning-free Lean build, six corpus-harness tests,
three export integration tests with multiple fixtures, all 110 corpus cases
(including 19 statistical cases), 18 independent kernel typing certificates,
and the TeX build. The next item is point 4: verified model checking.

Point 4 now has a proof-producing machine replay checker. It validates the graph
independently of the explorer, constructs the specified rational model, and
proves coverage and absence of failure for every reachable machine state.
Exports include a kernel-replayable `.replay.lean` certificate bound to the
requested source and subject. The local replay invariants live in
`Proof/FiniteModel/Replay.lean`; the paper-level contract is unchanged.
The remaining point 4 work is the semantic correspondence proof between the
closure machine and paper reduction, and the resulting output-law equality.

Machine replay validation passes: warning-free Lean build, six harness tests,
four export integration tests (including three independent kernel replay
certificates and their tampered variants), all 110 corpus cases in fast mode,
18 independent kernel typing certificates, and the TeX build. Exported replay
and reachability theorems use only standard Lean axioms. No commits or pushes.

The semantic bridge now includes `Substitution.lean`, `Reification.lean`,
`Contexts.lean`, `Reduction.lean`, `Administrative.lean`, and `Initial.lean`
under `Proof/FiniteModel/`. Values expand to closed paper values, and environment
expansion agrees with the paper's one- and two-variable substitution. Actual
machine steps for deterministic operations agree with root paper reductions;
bookkeeping steps preserve the represented expression. Context lifting includes
sampling actions under the primitive-arity invariant.

Rejection is handled by output-law equality: dropping the stack preserves zero
output and non-stuckness, although paper reduction leaves the surrounding context
in place. Source scope is now checked by replay, including unused function bodies.
Consequently `replay_initial_reification` identifies an accepted graph's initial
state with the selected paper program without an extra typing assumption.

`Tests/SemanticBridge.lean` checks captured variables beneath binders, recursive
self-reference, list binder order, total division, rejection inside sampling
contexts, and source/determinized initial alignment. No paper semantics or
end-to-end contract has changed. The full model correspondence theorem remains
unchecked until these local results are composed into unbounded output-law equality.

Semantic bridge validation passes: warning-free Lean build, binding/context
proof examples, six harness tests, four export integration tests with independent
kernel replay, all 110 fast corpus cases, 18 independent kernel typing
certificates, and the TeX build. The new theorems use only standard Lean axioms.
Work remains uncommitted and unpushed.

`Sampling.lean` now proves that every successful `finiteLaw` call has nonnegative
exact weights summing to one and reproduces the complete paper sampling measure.
It covers stochastic Bernoulli/discrete and all supported deterministic means.
`draw_step` and `discrete_step` identify actual machine successors;
`draw_correspondence` and `discrete_correspondence` identify their paper sampling
actions under well-shaped continuation stacks. `Invariants.lean` now establishes
that invariant for reachable states. Tests cover Bernoulli endpoints, invalid and unsupported calls,
a rational gamma mean, and a draw followed by rejection.

Sampling correspondence validation passes: warning-free full Lean build, six
harness tests, four export integration tests, all 110 fast corpus cases, 18
independent kernel typing certificates, and the TeX build. The new proofs use
only standard Lean axioms. Work remains uncommitted and unpushed.

`Invariants.lean` proves that every successful machine transition preserves
sampling-frame arity, then lifts this to arbitrary reachable states from either
source or determinized initial programs. The reachable sampling-correspondence
theorems therefore need no separate continuation-shape premise. The three machine
operation helpers are now visible to proofs; their implementations are unchanged.

`Progress.lean` defines a natural-number measure of pending operand work and
proves that every bookkeeping transition strictly decreases it. A finite path
of such transitions has length at most its initial measure, ruling out infinite
bookkeeping. This classification includes expression setup, value delivery,
operand sequencing, and rejection. Sampling and application transitions are
excluded; recursive programs may still diverge through paper reductions.

Remaining point 4 work is to compose the local transition results, probability
laws, reachability invariant, and bookkeeping bound into safety and equality of
unbounded output measures, then establish the existing `Model.Matches` contract.

Continuation/progress validation passes: warning-free full Lean build, six harness
tests, four export integration tests, 110 fast corpus cases, 18 independent kernel
typing certificates, and the TeX build. New theorems use only standard Lean axioms.
No commits or pushes.

### Autonomous batch: complete model semantic correspondence

- [x] Cover value-construction bookkeeping and classify all successful machine transitions.
- [x] Prove finite sampling continuation sums and almost-everywhere successor safety.
- [x] Prove execution safety from replay coverage and local correspondence.
- [x] Prove both finite-horizon comparisons and unbounded output-law equality.
- [x] Connect replay acceptance to `Model.Matches` and export independently checked certificates.
- [x] Add regressions, run the full validation workflows, and update the trust-boundary documentation.

This batch remains local and uncommitted. Result certificates, Storm invocation,
and OCaml retirement follow the model correspondence batch.

The semantic correspondence batch now establishes `replay_matches` and
`checkModelCertificate_sound`. It covers safety and the full unbounded output law
for source or determinized programs, including rejection and divergence. The
checker validates unique stored states and returns `CheckedModel` with its proof.
Exports include a kernel-checkable `modelMatches` theorem. Value-construction
transitions are included in the bookkeeping bound. The earlier notes about an
unproved machine/paper connection are superseded by this completed proof.

Batch validation passes: warning-free full Lean build; all executable unit tests;
110 fast and 19 statistical corpus cases; six harness tests; four export integration
tests; independent model-correspondence certificates for seven source/determinized
programs with tampered variants rejected; 18 independent typing certificates; and
the TeX build. Both `replay_matches` and `checkModelCertificate_sound` use only
`propext`, `Classical.choice`, and `Quot.sound`. Nothing was committed or pushed.

### Autonomous batch: result certificates and exact Storm comparison

- [x] Prove integrability of finite-model output and its unbounded value equations.
- [x] Prove absorption makes those equations unique and establish result-checker soundness.
- [x] Add a proved tabulated survival calculation and instantiate the program expected-reward theorem.
- [x] Generate rational certificates with bounded dense Gaussian elimination and absorption search.
- [x] Export standalone kernel-checkable expected-reward theorems through `--result PREFIX`.
- [x] Invoke real Storm with exact rational matrices, record version/options/status, and require exact agreement.
- [x] Test ground truth, signed rewards, rejection, recursion, malformed values/bounds, nonabsorption, and resource limits.
- [x] Update the trust-boundary documentation and preserve all work without commits or pushes.

`resultCertificate_sound`, `checkResult_sound`, and `checked_expectedReward` now
establish the existing result contract without additional assumptions. The proof
uses finite terminal support for integrability and the absorption bound for
uniqueness. A maximum-error argument avoids a separate limit proof for signed
integrals. The Boolean checker’s tabulated survival calculation is proved equal
to the specification.

The unverified rational solver supplies certificate values; Storm independently
computes the same rational answer. `tools/storm.py` reads the exported rational
data into Storm’s exact sparse-matrix API, since the default explicit-file reader
rejects fractional literals. The wrapper checks the exported theorem with Lean’s
kernel before reporting agreement and records failures and timeouts. Neither
Storm nor file serialization is a premise of the expected-reward theorem.

Point 5 is complete. Point 6 remains: audit remaining OCaml use cases, decide the
symbolic-coupling CLI replacement, retire the old implementation/workflows, and
validate that final replacement before organizing local commits.

Validation passes: warning-free full Lean build; executable unit tests; 110 fast
and 19 statistical corpus cases; six harness tests; four model-export tests;
18 independent typing certificates; five independent expected-reward certificates
with tampered values and escape bounds rejected; seven exact ground-truth cases;
and seven real Storm 1.14.0 comparisons, including signed rewards, rejection,
probabilistic recursion, fractional terminals, and a determinized continuous draw.
Timeout, kernel-failure, and disagreement reports are tested. TeX builds.
The new soundness theorems use only `propext`, `Classical.choice`, and `Quot.sound`.
No commits or pushes were made.


### Autonomous batch: retire OCaml and replace its workflows

- [x] Audit CLI/language/model-checking coverage and deliberate behavior changes in `migration-audit.md`.
- [x] Verify removed sources and archived reports are unchanged from Git history; keep all original `.det` programs.
- [x] Retire the standalone symbolic-coupling prototype and preserve its three motivating examples in shared Lean/simulator regressions.
- [x] Remove OCaml source, baselines, Dune configuration, and its Nix devshell.
- [x] Route `run.sh` to Lean/Storm and `det.sh` to the full shared test suite.
- [x] Migrate the root environment, verification hooks, permissions, skills, and documentation.
- [x] Run all replacement workflows, including real Storm, then report remaining limitations.

No commit organization or pushing is part of this batch. The root README now
provides setup and maintained commands. The Nix Lean shell supplies Python and uv;
the full devshell combines Lean, simulator, and paper tools. The default test suite
has no dependency on archived outputs or an OCaml installation.


Retirement validation passed through the migrated `check.sh --all` with
`STORM_PYTHON` set: warning-free Lean build with 37 standard-axiom reports;
112 corpus cases including 21 statistical cases; 20 independent typing
certificates; model/result kernel replay and tampered-certificate tests;
eight exact Storm comparisons; both public-wrapper tests; 51 simulator tests;
byte-identical bundle regeneration; and the paper build. TeX retains three
overfull boxes and its existing duplicate `fig:determinization` label warnings.
Shell syntax, JSON configuration, and `git diff --check` pass. Nix is unavailable
on this host, so devshell definitions were inspected but not evaluated.
Point 6 is complete. Commit organization (point 7) remains unstarted, and nothing
was committed, pushed, or deployed.


### Local commit organization

Point 7 is complete. The existing Lean frontend commit is followed by:

1. Extend the checked language with silent subtyping and finite draws.
2. Move language regression tests into a shared analytical corpus.
3. Define exact finite models, exploration, and certificate contracts.
4. Prove finite models match the paper semantics.
5. Check exact result certificates and integrate Storm.
6. Retire OCaml and migrate project workflows to Lean.

Silent subtyping, finite draws, and rejection share changes to the typing and
soundness inductions and stay together so intermediate commits remain buildable.
Each boundary passes a Lean build and the relevant frontend, corpus, model, or
result tests. The final tree passes the full workflow, including real Storm,
simulator tests and bundle regeneration, and the paper build. The full migration
review found no actionable issues. Nix remains unavailable locally; its devshells
were inspected but not evaluated. Existing TeX layout/duplicate-label warnings
remain. All six commits are local; nothing was pushed or deployed.


### Integration onto main (2026-09-11)

The stack is rebased onto `b3995d4`. The frontend and language/proof update form
one buildable commit because upstream added syntax constructors between the
original frontend and migration commits. The remaining commits separate the
shared corpus, finite-model specification, model correspondence, result
certificates/Storm, and OCaml retirement.

- [x] Preserve all nine upstream theorem statements and assertions, including the refined replay theorem and variance/conditional-expectation results.
- [x] Adapt their proofs to silent subtyping and the core rejection representation.
- [x] Require literal discrete probabilities to sum to one exactly; update printing and analytical fixtures.
- [x] Preserve upstream's four new programs byte-for-byte and check their typing certificates.
- [x] Preserve upstream's paper/simulator changes and theorem-assertion coverage check.
- [x] Run the integrated Lean, model/result, Storm, simulator, and paper checks; rerun the corpus after correcting its uniform variance fixture.
- [x] Verify and adopt the six buildable local commits.
- [x] Complete the final integration review.

The integrated corpus has 117 cases, including 21 statistical cases and 24
independent typing certificates. The warning-free Lean build reports 41 theorems
using only `propext`, `Classical.choice`, and `Quot.sound`. Simulator tests (51),
byte-identical bundle regeneration, and the paper build pass. The three existing
overfull boxes and duplicate figure labels remain. Nix was not evaluated locally.
Nothing is pushed.

The final `codex review --base origin/main` found no actionable regressions after
the uniform fixture correction. Each commit boundary builds and passes its
relevant tests; the result boundary includes real Storm comparisons.
