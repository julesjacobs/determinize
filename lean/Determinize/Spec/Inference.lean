import Determinize.Spec.Frontend
import Determinize.Checking.Certificate
import Determinize.Frontend.Infer

/-!
# Affinity inference

`Frontend.infer` fills the omitted sample affinities of a resolved program `input : Input`.
A *completion* of `input` is an annotated program that fills exactly those affinities and is
closed and well typed at some type. Completions of the same input differ only in the affinities
of their free sample sites; they are compared site by site in the order `G ≤ E` of
`Ty.Sub.general`. The greatest completion has the most E sites, which determinization replaces
by the most means.

`inferSoundThm` states that the output of `infer` is a completion, typed at the returned type;
`inferOptimalThm` that every completion lies below it, so it is the greatest completion; and
`inferCompleteThm` that `infer` succeeds whenever a completion exists. `inferFloatSoundThm` and
`inferFloatCompleteThm` restate soundness and completeness at `float E`, the type assumed by the
theorems in `Spec/Main.lean`, and `inferFloatTypedThm` states that the inferred program has type
`float E` whenever some completion does.

These statements do not depend on how `infer` works. Soundness, optimality, and completeness
determine its output program uniquely, but not its output type.
-/

namespace Determinize.Spec

open Paper Checking

/-- `program` fills exactly the omitted affinities of `input`, and is closed and well typed.
The type is existential: a completion counts even when it is typed only at a type other than
the one `infer` returns. -/
def Completion (input : Input) (program : Core) : Prop :=
  input.matches program = true ∧ ∃ ty, Typed [] (interpret program) ty

/-- `lower` and `upper` are the same program, except that at every sample site the affinity of
`lower` is below that of `upper` in the order `G ≤ E`. Mean sites never occur in a completion
and are not related. -/
def AffinityLE : Core → Core → Prop
  | .bvar index, .bvar index' => index = index'
  | .reject, .reject => True
  | .unit, .unit => True
  | .bool value, .bool value' => value = value'
  | .real value, .real value' => value = value'
  | .nil, .nil => True
  | .lam body, .lam body' =>
      AffinityLE body body'
  | .fix body, .fix body' =>
      AffinityLE body body'
  | .app fn arg, .app fn' arg' =>
      AffinityLE fn fn' ∧ AffinityLE arg arg'
  | .pair left right, .pair left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .fst body, .fst body' =>
      AffinityLE body body'
  | .snd body, .snd body' =>
      AffinityLE body body'
  | .inl body, .inl body' =>
      AffinityLE body body'
  | .inr body, .inr body' =>
      AffinityLE body body'
  | .matchSum scrutinee left right, .matchSum scrutinee' left' right' =>
      AffinityLE scrutinee scrutinee' ∧ AffinityLE left left' ∧ AffinityLE right right'
  | .cons head tail, .cons head' tail' =>
      AffinityLE head head' ∧ AffinityLE tail tail'
  | .matchList scrutinee nilCase consCase, .matchList scrutinee' nilCase' consCase' =>
      AffinityLE scrutinee scrutinee' ∧ AffinityLE nilCase nilCase' ∧ AffinityLE consCase consCase'
  | .ite condition thenBranch elseBranch, .ite condition' thenBranch' elseBranch' =>
      AffinityLE condition condition' ∧ AffinityLE thenBranch thenBranch' ∧
        AffinityLE elseBranch elseBranch'
  | .letE value body, .letE value' body' =>
      AffinityLE value value' ∧ AffinityLE body body'
  | .neg body, .neg body' =>
      AffinityLE body body'
  | .add left right, .add left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .mul left right, .mul left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .div left right, .div left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .lt left right, .lt left' right' =>
      AffinityLE left left' ∧ AffinityLE right right'
  | .uniform (.sample affinity) lower upper, .uniform (.sample affinity') lower' upper' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE lower lower' ∧ AffinityLE upper upper'
  | .gaussian (.sample affinity) mean variance, .gaussian (.sample affinity') mean' variance' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE mean mean' ∧
        AffinityLE variance variance'
  | .poisson (.sample affinity) rate, .poisson (.sample affinity') rate' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE rate rate'
  | .bernoulli (.sample affinity) probability, .bernoulli (.sample affinity') probability' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE probability probability'
  | .exponential (.sample affinity) rate, .exponential (.sample affinity') rate' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE rate rate'
  | .beta (.sample affinity) alpha betaArg, .beta (.sample affinity') alpha' betaArg' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE alpha alpha' ∧
        AffinityLE betaArg betaArg'
  | .gamma (.sample affinity) shape rate, .gamma (.sample affinity') shape' rate' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE shape shape' ∧ AffinityLE rate rate'
  | .discrete (.sample affinity) distribution, .discrete (.sample affinity') distribution' =>
      Ty.Sub (.float affinity) (.float affinity') ∧ AffinityLE distribution distribution'
  | _, _ => False

/-- Soundness: the inferred program is a completion of the input, typed at the inferred type. -/
def inferSoundThm : Prop :=
  ∀ (input : Input) (program : Core) (ty : Ty),
    Frontend.infer input = .ok (program, ty) →
      input.matches program = true ∧ Typed [] (interpret program) ty

/-- Optimality: every completion lies below the inferred program, which by soundness is
therefore the greatest completion. -/
def inferOptimalThm : Prop :=
  ∀ (input : Input) (program : Core) (ty : Ty) (completion : Core),
    Frontend.infer input = .ok (program, ty) →
    Completion input completion →
    AffinityLE completion program

/-- Completeness: inference succeeds on every input that has a completion. -/
def inferCompleteThm : Prop :=
  ∀ (input : Input) (completion : Core),
    Completion input completion →
    ∃ program ty, Frontend.infer input = .ok (program, ty)

/-- Soundness at `float E`. It needs the inferred type to be a float: `infer` returns `unit`
for the input `reject`, which also has type `float E`. -/
def inferFloatSoundThm : Prop :=
  ∀ (input : Input) (program : Core) (affinity : Affinity),
    Frontend.infer input = .ok (program, .float affinity) →
      input.matches program = true ∧ Typed [] (interpret program) (.float .E)

/-- Completeness and optimality for completions of type `float E`. -/
def inferFloatCompleteThm : Prop :=
  ∀ (input : Input) (completion : Core),
    input.matches completion = true →
    Typed [] (interpret completion) (.float .E) →
    ∃ program ty, Frontend.infer input = .ok (program, ty) ∧ AffinityLE completion program

/-- If some completion has type `float E`, then so does the inferred program, even when the
inferred type is not a float. The theorems in `Spec/Main.lean` then apply to the output of
`infer` itself. -/
def inferFloatTypedThm : Prop :=
  ∀ (input : Input) (completion : Core),
    input.matches completion = true →
    Typed [] (interpret completion) (.float .E) →
    ∃ program ty, Frontend.infer input = .ok (program, ty) ∧
      Typed [] (interpret program) (.float .E)

/-! ## Examples

The programs come from the hand-written list in `notes/inference-optimality/Checker.lean`. -/

/-- The elaboration of `if true then uniform(0,1) else uniform(0,1) * uniform(0,1)`. -/
private def branches : Input :=
  .ite (.bool true) (.uniform none (.real 0) (.real 1))
    (.mul (.uniform none (.real 0) (.real 1)) (.uniform none (.real 0) (.real 1)))

/-- `branches` with the affinities `infer` chooses: only the left factor of the product
must be G. -/
private def branchesInferred : Core :=
  .ite (.bool true) (.uniform (.sample .E) (.real 0) (.real 1))
    (.mul (.uniform (.sample .G) (.real 0) (.real 1)) (.uniform (.sample .E) (.real 0) (.real 1)))

/-- `branches` with every site G. -/
private def branchesGeneral : Core :=
  .ite (.bool true) (.uniform (.sample .G) (.real 0) (.real 1))
    (.mul (.uniform (.sample .G) (.real 0) (.real 1)) (.uniform (.sample .G) (.real 0) (.real 1)))

#guard match Frontend.infer branches with
  | .ok (program, ty) => program == branchesInferred && ty == .float .E
  | .error _ => false

example : Completion branches branchesInferred :=
  ⟨by decide, .float .E,
    .ite .bool (.uniform .real .real) (.mul (.uniform .real .real) (.uniform .real .real))⟩

example : Completion branches branchesGeneral :=
  ⟨by decide, .float .G,
    .ite .bool (.uniform .real .real) (.mul (.uniform .real .real) (.uniform .real .real))⟩

/-- The all-G completion lies strictly below the inferred one. -/
example : AffinityLE branchesGeneral branchesInferred ∧
    ¬ AffinityLE branchesInferred branchesGeneral := by
  refine ⟨⟨rfl, ⟨.general, rfl, rfl⟩, ⟨.float .G, rfl, rfl⟩, .general, rfl, rfl⟩, ?_⟩
  rintro ⟨-, ⟨below, -⟩, -⟩
  cases below

/-- The elaboration of `uniform[E](0,1) < 0.5`. A comparison needs G operands. -/
private def comparison : Input :=
  .lt (.uniform (some .E) (.real 0) (.real 1)) (.real (1 / 2))

#guard !(Frontend.infer comparison).isOk

example : ¬ ∃ program, Completion comparison program := by
  rintro ⟨program, aligned, ty, typed⟩
  -- Alignment fixes the head of `program` and the affinity of its sample site.
  obtain ⟨lower, upper, right, rfl⟩ :
      ∃ lower upper right, program = .lt (.uniform (.sample .E) lower upper) right := by
    cases program <;> simp [comparison, Input.matches] at aligned
    rename_i site right
    cases site <;> try simp [Input.matches] at aligned
    rename_i action lower upper
    cases action <;> simp [Input.matches] at aligned
    obtain ⟨⟨⟨rfl, -⟩, -⟩, -⟩ := aligned
    exact ⟨lower, upper, right, rfl⟩
  -- A comparison needs a G operand, and an E site is not G.
  have left : ∀ {context program ty}, Typed context program ty →
      ∀ {left right}, program = .lt left right → Typed context left (.float .G) := by
    intro context program ty typed
    induction typed with
    | lt typedLeft _ => intro _ _ equal; cases equal; exact typedLeft
    | sub _ _ ih => exact ih
    | _ => intro _ _ equal; cases equal
  have sampleE : ∀ {context program ty}, Typed context program ty →
      ∀ {lower upper}, program = .uniform (.sample .E) lower upper → Ty.Sub (.float .E) ty := by
    intro context program ty typed
    induction typed with
    | uniform => intro _ _ equal; cases equal; exact .float .E
    | sub _ sub ih => intro _ _ equal; cases ih equal; exact sub
    | _ => intro _ _ equal; cases equal
  cases sampleE (left typed rfl) rfl

-- `infer` types the input `reject` at `unit`, which is not a float, although `reject` also has
-- type `float E`. This is why `inferFloatSoundThm` assumes a float type.
#guard match Frontend.infer .reject with
  | .ok (.reject, .unit) => true
  | _ => false

example : Completion .reject .reject ∧ Typed [] (interpret .reject) (.float .E) :=
  ⟨⟨rfl, .unit, .reject⟩, .reject⟩

end Determinize.Spec
