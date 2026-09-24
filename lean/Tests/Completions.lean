import Determinize.Spec.Inference

/-! `Completion` and `AffinityLE` on two small inputs. For `branches`, both the program that
`infer` returns and the all-G program are completions, and the all-G one lies strictly below.
`comparison` has no completion, and `infer` rejects it. -/

namespace Determinize.Tests.Completions
open Determinize.Spec Determinize.Spec.Paper

/-- The elaboration of `if true then uniform(0,1) else uniform(0,1) * uniform(0,1)`. -/
private def branches : Input :=
  .ite (.bool true) (.uniform none (.real 0) (.real 1))
    (.mul (.uniform none (.real 0) (.real 1)) (.uniform none (.real 0) (.real 1)))

/-- `branches` with the affinities `infer` chooses: only the left factor of the product
must be G. -/
private def branchesInferred : Annotated :=
  .ite (.bool true) (.uniform .E (.real 0) (.real 1))
    (.mul (.uniform .G (.real 0) (.real 1)) (.uniform .E (.real 0) (.real 1)))

/-- `branches` with every site G. -/
private def branchesGeneral : Annotated :=
  .ite (.bool true) (.uniform .G (.real 0) (.real 1))
    (.mul (.uniform .G (.real 0) (.real 1)) (.uniform .G (.real 0) (.real 1)))

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
      ∃ lower upper right, program = .lt (.uniform .E lower upper) right := by
    cases program <;> simp only [comparison, Input.matches, Expr.Sitewise] at aligned
    rename_i left right
    obtain ⟨aligned, -⟩ := aligned
    cases left <;> simp [Expr.Sitewise] at aligned
    obtain ⟨rfl, -⟩ := aligned
    exact ⟨_, _, _, rfl⟩
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

end Determinize.Tests.Completions
