import Determinize.Spec.Bunched

/-!
# Bunches

Facts about the bunches of `Spec/Bunched.lean` that the soundness proof of bunched inference
uses: well-formedness (`Bunch.wf`, `contextWF`) under shifting and filling holes, the equivalence
`Bunch.Equiv`, and structural rules derived from those of `Bunched.Typed`.

Part of the experimental bunched extension: nothing outside `Proof/Bunched/` imports it.
-/

namespace Determinize.Proof.Bunched
open Spec.Paper Spec.Paper.Bunched

end Determinize.Proof.Bunched
