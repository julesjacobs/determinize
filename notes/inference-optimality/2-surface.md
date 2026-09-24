# Step 2: the trusted statements

The statements from `1-claims.md` are now Lean definitions with `sorry` proofs. `infer`
returns a type instead of a certificate, and the statements mention no `Certificate`.

## Where everything lives

| What | Where |
| --- | --- |
| `Completion`, `AffinityLE` (the order), the five `…Thm : Prop` statements, examples | `lean/Determinize/Spec/Inference.lean` (new) |
| `inferenceSoundness`, `inferenceOptimality`, `inferenceCompleteness`, `inferenceFloatSoundness`, `inferenceFloatCompleteness`, each `:= sorry`, with `#print axioms` | `lean/Determinize/Theorems.lean` |
| `infer : Input → Except String (Core × Ty)`, `inferWithCertificate` (the old `infer`) | `lean/Determinize/Frontend/Infer.lean` |
| `Spec/Inference.lean` added to the files whose `…Thm` statements must be asserted in `Theorems.lean` | `check.sh` |

A reviewer needs to read `Spec/Inference.lean` and the definitions it reuses:
`Input` and `Input.matches` (`Spec/Frontend.lean`), `Typed` (`Spec/Syntax.lean`), `Ty` and
`Ty.Sub` (`Spec/Types.lean`), and `Core` and `interpret` (`Checking/Certificate.lean`,
which step 5 moves). `Frontend/Infer.lean` is imported because `infer` is the subject of
the statements. Its body does not need to be read: the statements describe only what
`infer` returns.

This is the first `Spec` file that imports the front end. `lean/README.md` says that
`Spec` does not import it; see "Stale documentation" below.

## Statements and the claims they formalise

Everything below is in namespace `Determinize.Spec`, with `Paper` and `Checking` open.

```lean
def Completion (input : Input) (program : Core) : Prop :=
  input.matches program = true ∧ ∃ ty, Typed [] (interpret program) ty

def AffinityLE : Core → Core → Prop   -- structural, 40 cases; see below

def inferSoundThm : Prop :=
  ∀ (input : Input) (program : Core) (ty : Ty),
    Frontend.infer input = .ok (program, ty) →
      input.matches program = true ∧ Typed [] (interpret program) ty

def inferOptimalThm : Prop :=
  ∀ (input : Input) (program : Core) (ty : Ty) (completion : Core),
    Frontend.infer input = .ok (program, ty) →
    Completion input completion →
    AffinityLE completion program

def inferCompleteThm : Prop :=
  ∀ (input : Input) (completion : Core),
    Completion input completion →
    ∃ program ty, Frontend.infer input = .ok (program, ty)

def inferFloatSoundThm : Prop :=
  ∀ (input : Input) (program : Core) (affinity : Affinity),
    Frontend.infer input = .ok (program, .float affinity) →
      input.matches program = true ∧ Typed [] (interpret program) (.float .E)

def inferFloatCompleteThm : Prop :=
  ∀ (input : Input) (completion : Core),
    input.matches completion = true →
    Typed [] (interpret completion) (.float .E) →
    ∃ program ty, Frontend.infer input = .ok (program, ty) ∧ AffinityLE completion program
```

| `1-claims.md` | Lean | Differences |
| --- | --- | --- |
| `Completion e ê` | `Completion input program` | none |
| `Core.Below ê' ê` | `AffinityLE completion program` | renamed; same structural definition |
| Claim 1, `inferSoundThm` | `inferSoundThm` | `c.ty` becomes the returned `ty` |
| Claim 2, `inferOptimalThm` | `inferOptimalThm` | `c` becomes `ty`; the order of binders changed |
| Claim 3, `inferCompleteThm` | `inferCompleteThm` | `∃ ê' c` becomes `∃ program ty` |
| Corollary "`c.ty = .float m` ⟹ `Typed … (.float .E)`" | `inferFloatSoundThm` | hypothesis is `infer input = .ok (program, .float affinity)`; the conclusion also repeats `matches` |
| Corollary "`float E` completion ⟹ `infer` succeeds and is above it" | `inferFloatCompleteThm` | none |

Soundness and optimality together say that `infer`'s program is the greatest completion.
There is no separate "greatest" statement: `inferSoundThm` gives `Completion input program`
(its conclusion is that conjunction with a specific `ty`), and `inferOptimalThm` puts every
completion below it.

The corollaries follow from the three main statements in a few lines each: the first by
`Typed.sub` with `Ty.Sub.float`/`Ty.Sub.general`, the second by combining completeness
and optimality. I checked this in a scratch file and did not commit it, because this
session writes no proofs. `Theorems.lean` currently proves them by `sorry` too. Step 4 can
derive them from the main three instead of proving them separately.

## Choices, and where a different reading was possible

1. **`Completion` quantifies the type existentially**, as `1-claims.md` requires. A
   completion typed only at a type unrelated to `infer`'s type still counts for
   optimality and completeness. So `reject` is a completion of `reject` even though `infer`
   types it at `unit`; an example checks this.
2. **Closed programs only.** `Typed []` is the empty context. An open input has no
   completion, and `infer` fails on it (unbound variable). The theorems quantify over
   *every* `Input`, not only the outputs of `elaborate`. That is stronger than needed for
   the CLI, and the brute-force checker enumerated such inputs.
3. **The order is structural and `Prop`-valued.** `AffinityLE lower upper` mirrors
   `Input.matches` case by case: the same constructor, equal payloads (`=` on indices,
   `Bool` and `Rat`), related children, and at each sample site
   `Ty.Sub (.float affinity) (.float affinity')`. Reusing `Ty.Sub` means that `G ≤ E` is not
   stated again. Any other pair is `False`, including two mean sites, so `AffinityLE` is not
   reflexive on programs with mean sites. That is harmless: `matches` rejects mean sites, so
   no completion and no output of `infer` contains one.
   * Alternative I did not take: `List.Forall₂ (fun a b => Ty.Sub (.float a) (.float b))
     (sampleAffinities ·) (sampleAffinities ·)`. Inside the theorems both sides match the
     same input, so the two readings are equivalent there. But on its own that relation
     does not say the programs are the same, and `sampleAffinities` lives in
     `Checking/Elaboration.lean`, which imports the certificate checker that step 5
     removes.
   * It is named `AffinityLE`, not `Core.Below`, so that its name does not depend on
     where `Core` lives after step 5.
4. **Optimality compares against any completion, and says nothing more about `ty`.** No
   statement says anything about the returned *type* beyond soundness. For example,
   nothing says `ty` is principal, least, or the type of the greatest completion's most
   precise typing. `infer` could return any valid type for its program and still satisfy
   all five statements. That matches `1-claims.md`, which claims nothing about the type.
   It is still worth confirming that this is intended.
5. **Completeness is stated positively** ("a completion exists ⟹ `infer` returns
   `.ok`") rather than as "`infer` fails ⟹ no completion". The two are equivalent because
   `Except` has two constructors. Nothing is said about the error message.
6. **`inferFloatSoundThm` has the float hypothesis built into the pattern**
   `.ok (program, .float affinity)`, which is `c.ty = .float m` from `1-claims.md`. Without
   it the statement would be false (`reject`, and every non-float program).
7. **`inferFloatCompleteThm` could be stronger.** It does not say that `infer`'s own
   program has type `float E`. It only puts the program above a `float E` completion,
   and `infer`'s type may be `unit` (again `reject`). So you cannot always apply `mainThm` to
   `infer`'s output from the corollaries alone. A stronger conclusion,
   `… ∧ AffinityLE completion program ∧ Typed [] (interpret program) (.float .E)`, is true
   according to the brute-force checker. The checker verifies that whenever some completion
   is typable at `float E`, every typable completion is, the greatest included.
   **This is not a consequence of the three main theorems**, though. Its proof needs an
   extra argument: the result shape in the most general unifier is either `float` or an
   unconstrained variable, and an unconstrained variable takes part in no affinity
   constraint. I kept the `1-claims.md` version. Whether to strengthen it is a question for
   your review.
8. **Namespaces.** The new definitions and statements are in `Determinize.Spec`, like
   `Spec/Main.lean`. `Input` and `matches` stay in `Determinize.Checking`, where
   `Spec/Frontend.lean` already puts them.

## Examples (in `Spec/Inference.lean`)

All of them build, with real proofs. The programs come from the targeted list in
`Checker.lean`, except the last one.

* `if true then uniform(0,1) else uniform(0,1) * uniform(0,1)`, written as its elaborated
  `Input` (checked against `elaborate ∘ parse` in a scratch run):
  * `infer` returns `E, G, E` at `float E` (`#guard`, evaluated when the file is built);
  * that output is a `Completion` (explicit `Typed` derivation at `float E`);
  * the all-`G` program is a `Completion` (derivation at `float G`);
  * the all-`G` completion is strictly below the inferred one: `AffinityLE` one way and
    not the other.
* `uniform[E](0,1) < 0.5`: `infer` rejects it (`#guard`), and it has no completion. The
  proof shows that `matches` forces the shape `lt (uniform (sample E) _ _) _`, and that
  typing it would need `Ty.Sub (float E) (float G)`. The two small inversion lemmas it uses
  are local `have`s in the example, not part of the surface.
* The core program `reject`: `infer` types it at `unit` (`#guard`), but it is a completion
  of itself and also has type `float E`. This is why `inferFloatSoundThm` needs its float
  hypothesis.

The `#guard`s run the interpreter on `Frontend.infer` at build time. They are sanity
checks that tie the examples to the implementation, not part of the statements. Once `infer`
is total and reduces in the kernel (step 3), they could become `example`s proved by
`decide`, if that is fast enough.

## The interface change to `infer`

* `inferWithCertificate : Input → Except String (Core × Certificate)` is the old `infer`,
  unchanged.
* `infer : Input → Except String (Core × Ty)` is `inferWithCertificate` followed by
  `Certificate.ty`. Its behaviour is unchanged.

What still uses `inferWithCertificate`, and so the certificate:

* `Frontend/Compile.lean`, `compile`: runs `certify` and returns `Program` with
  `checked : Certified input`. Every consumer of `p.checked` (the CLI in `Main.lean`,
  `Tests/Parsing`, `Inference`, `Runtime`, `Explorer`, `RewardModel`, `ModelReplay`,
  `Corpus`) goes through this.
* `Frontend/Certificate.lean`, `certificateText`: the exported standalone `.lean` file
  contains the `Certificate`, and rechecks it with `certify … (by decide +kernel)`.
* `Tests/Corpus.lean`, `compileStage`: calls `certify` separately so that it can report
  a "certificate" rejection stage.
* `notes/inference-optimality/Checker.lean` (twice): it `certify`s `infer`'s output.

What uses the new `infer`: `Spec/Inference.lean` (statements and `#guard`s), and
`Tests/Inference.lean`, which only calls `.isOk` and needed no change.

Things step 5 should know that do not use `inferWithCertificate` but depend on the
certificate path: `Tests/Checking.lean` and `Tests/MeanTyping.lean` test `certify` and
`check` directly; `Proof/Checking/Elaboration.lean` states `certified_alignment`,
`certified_trace_conditional_law` and `certified_expectation` about `Certified`; and
`Proof/FiniteModel/Initial.lean` imports that file. The exported certificate file is the
hard case. Today the kernel re-derives the typing by running `check`. Without a
checker, the file would have to evaluate `infer` itself in the kernel
(`infer original = .ok (annotated, ty)` by `decide +kernel`) and cite the soundness
theorem. That needs `infer` to reduce in the kernel, which argues against well-founded
recursion in step 3's rewrite, or at least for checking kernel reduction early.

## Build and test status

* `lake build --wfail` fails **only** on the five `declaration uses 'sorry'` warnings in
  `Theorems.lean`. `lake build` without `--wfail` succeeds, and the axiom report lists
  `sorryAx` for exactly the five new theorems. Everything else is unchanged.
* `./test.sh` passed on the interface-change commit (`d82c996`). On the final tree it
  stops at its first step, `lake build --wfail`, because of the `sorry`s. The remaining
  steps of `lean/test.sh`, run after a plain `lake build`, pass. Until step 4, run them
  that way, for example
  `sed 's/lake build --wfail/lake build/' lean/test.sh | (cd lean && bash)`.
* `Checker.lean --targeted` passes after the switch to `inferWithCertificate`.
* `./check.sh lean` will report both the build failure and the `sorryAx` axioms until
  step 4. The new check.sh entry makes it also fail if a statement in
  `Spec/Inference.lean` is ever left unasserted.

## Stale documentation (not changed here)

`lean/README.md`:

* "`Spec`, `Spec/Traces`, and the existing soundness proofs do not import the front end":
  `Spec/Inference.lean` does now.
* "the build is warning-free and contains no `sorry`": false until step 4, which also
  keeps `./test.sh` from getting past its `--wfail` build.
* "Inference completeness and optimality are not claimed; some valid programs can be
  rejected depending on conditional branch order": stale since `8e4698f` (see
  `1-claims.md`), and contradicted by these statements once proved.
* The list of entry points for reviewers should mention `Spec/Inference.lean`.

These are best updated when the proofs land (step 4) or when the certificate goes
(step 5).

## Review
