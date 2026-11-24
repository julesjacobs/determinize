import Determinize.Proof.Bunched.Soundness

/-!
# Soundness of bunched inference

If `Frontend.Bunched.infer` succeeds, the annotated program it returns is typed by
`Spec.Paper.Bunched.Typed`, with no shared variables and the empty bunch, at the type it returns.
This is the first part of the original `Theorems.inference_correctness`, for the bunched
extension; optimality and completeness are left for later.

Part of the experimental bunched extension. Until its proof is complete this file is not imported
by `Determinize/Proof.lean`, since `lake build --wfail` rejects a `sorry`; build it alone with
`lake build Determinize.Proof.Bunched.Inference`.
-/

namespace Determinize.Proof.Bunched
open Determinize.Frontend Spec.Paper

/-- Soundness of bunched inference. -/
theorem infer_sound (input : Input) {program : Annotated} {ty : Ty}
    (h : Frontend.Bunched.infer input = .ok (program, ty)) :
    Spec.Paper.Bunched.Typed [] .empty (interpret program) ty := by
  sorry

end Determinize.Proof.Bunched
