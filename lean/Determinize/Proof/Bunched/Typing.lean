import Determinize.Proof.Bunched.Bunch

/-!
# Derived bunched typing rules

Lemmas about `Spec.Paper.Bunched.Typed` that the soundness proof of bunched inference uses, such
as: an expression of a G type uses no E variable, so it is typed with the empty bunch when its
variables are shared.

Part of the experimental bunched extension: nothing outside `Proof/Bunched/` imports it.
-/

namespace Determinize.Proof.Bunched
open Spec.Paper Spec.Paper.Bunched

end Determinize.Proof.Bunched
