[x] Make float look like float[m], use a macro.
[x] Change sample[e1,e2] to "uniform(e1,e2)"
[x] Rules without a premise should still show a horizontal line
[x] Add submoding G <= E, and corresponding subtyping rule.
[x] Have only + and times, and unary minus, as separate operators in the grammar and rules
[x] Format related work as a list mirroring the provided structure
[x] Add a gaussian sampling rule
[x] Implement the syntax of types/metas/modes with constraints
[x] Implement the type/meta/mode syntax and constraint scaffolding in OCaml
[x] In another subdir called det, put a bunch of example programs with extension filename.det.
[x] Implement 10 such examples.
[x] Implement a main driver that parses an input file and prints its pretty printed version.
[x] Implement det.sh that runs that on all files in det/
[x] Give mode variables and metas each a unique id (to be used for comparison / printing later)
[x] Rename case/of to match/with (and propagate to tex)
[x] Add a submode function and set_mode propagation for mode variables
[x] Change lambda syntax in the code to fun x => body; rec f x => body
[x] When the ocaml program processes a file, save the pretty output to a suffixed file
[x] Use a record for the constraints rather than a single constructor ADT.
[x] Implement a subtype function along similar lines as the submode function.
[x] Implement an interpreter for the language; run 100 trials and write .det.out with pretty-print and mean
[x] Add typed expressions (mutual recursion with per-subexpression type annotations)
[x] Add lists to the language with [], x::xs, and match on nil/cons.
[x] Add foldr example summing a list with mixed uniform/gauss samples

Misc:
[x] Remove \floattype/\float macros and replace all \floattype{...} usages with \Float{...}.
[x] Insert 4_inference.tex and renumber subsequent TeX section files/includes.
[x] Set fixed-width wrapped columns for the expectation-transformation figure table.
[x] Fill expectation transformation figure with recursive clauses for if/let/add/mult.
[x] Merge uniform transformation clauses into one table row with case-split condition/RHS cells.
[x] Introduce \exptrans macro and replace angle-bracket expectation-transform uses in soundness.tex.
[x] Macroize language construct keywords in TeX and centralize definitions in tex/macros.tex.
[x] Make expectation transformation table a figure with column/row rules.
[x] Convert expectation transformation equations in soundness.tex to a 3-column table.
[x] Align non-scaling multiplication inference with Mul-G (result float[G]).
[x] Move TeX \newcommand definitions into tex/macros.tex.
[x] Prefix TeX section include files with numeric order.
[x] Refactor tex/main.tex into per-section include files.
[x] Make Transformation equations type-directed in tex/main.tex.
[x] Finish justification tags in affine-continuation proof in tex/main.tex.
[x] Fix project-local LaTeX build/PDF display workflow (VS Code + script).
[x] Add support for other distributions, i.e. exponential, gamma, beta, etc.
[x] Support subtraction and division in the Lean implementation.
[x] Implement checked discrete branching in Lean.

Lean implementation:
[x] Add executable core operations and a proof-producing typing certificate checker.
[x] Add .det parsing, elaboration, type/mode inference, and pretty printing in Lean.
[x] Check that inference preserves the elaborated input and apply the existing determinization theorem.
[x] Add a numerical runtime and CLI using the Lean determinization transform.
[x] Test language coverage, rejected certificates, theorem boundaries, and builds.

Silent subtyping experiment:
[x] Remove core promotion and prove soundness with structural subsumption.
[x] Support structural subsumption in inference and checked certificates.
[x] Test products, sums, lists, function variance, and recursive functions.

Shared test corpus:
[x] Migrate .det cases to a root corpus with explicit expectations.
[x] Run exact execution, rejection, and analytical statistical tests through Lean.
[x] Preserve OCaml reference outputs and document the replacement workflow.

Complete Lean migration (details in migration-plan.md):
[x] Write the local migration and certified Storm checklist.
[x] Implement checked rational finite distributions with normalization and expectation proofs.
[x] Connect finite distributions to real-measure semantics and the shared primitive kernel/moment proofs.
[x] Add E/G discrete expression constructors, typing, symbolic semantics, and frontend support.
[x] Validate discrete primitives with exact, statistical, negative, and independent kernel-certificate tests.
[x] Implement explicit observation rejection with a semantics connection.
[x] Define finite rational models, expected-terminal-reward semantics, and checker contracts.
[x] Prove finite-horizon reward interpretation and composition of future checker guarantees.
[x] Implement exact rational exploration and unverified finite model/Storm file export.
[x] Check model replay, successor coverage, and machine safety with independent kernel certificates.
[x] Prove closure substitution, deterministic-step correspondence, contextual rejection, and initial paper alignment.
[x] Prove successful finite sampling laws and sampling-step correspondence under well-shaped continuations.
[x] Prove reachable continuation shape and a finite bound on consecutive bookkeeping transitions.
[x] Prove unbounded machine/paper correspondence and model output-law equality.
[x] Prove result-certificate soundness, generate exact certificates, and integrate exact Storm comparison.
[x] Retire OCaml and migrate scripts, development shells, hooks, and documentation.
[x] Validate the complete Lean/Storm/simulator/paper replacement.
[x] Organize the completed work into reviewable local commits.

[x] Integrate the migration onto current main and verify each commit boundary.

Development/specification review:
[x] Review redundant representations and theorem witnesses; kernel-check result-mode and canonical trace-output simplifications (development-review.md).
[x] Complete the fresh post-refactor Pro design/specification/architecture audit and assess findings (pro-architecture-review.md; baseline record in pro-spec-review.md).
[x] Compare sampling representations against the statement surface and specify affinity-free mean syntax with direct typing rules (development-review.md).

Full refactor (per-step checks and decisions in refactor-plan.md):
[x] Complete the specification and implementation refactor, with one reviewed commit per retained step.

[x] Independently challenge the Pro recommendations and record which survive (pro-recommendations-critique.md).

[x] Trial public distribution-domain safety premises; preserve untyped finite-certificate safety and assess before/after (KEEP; domain-safety-trial.md).

[x] Review the 19-commit migration/refactor stack and separate uncommitted domain-safety trial (three P2 findings in pr-stack-review.md).

[x] Fix stack findings: affinity schema, multiplication roundtrips, and numerical statistics; regressions and follow-up review pass.

Frontend AST refactor:
[x] Replace string-tagged parsed syntax with explicit constructors.
[x] Store optional affinities in resolved syntax and check alignment structurally.
[x] Validate certificates and corpus; compare each refactor and retain improvements (frontend-refactor.md).
