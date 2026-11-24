import Determinize.Frontend.Bunched
import Determinize.Proof.Bunched.Typing

/-!
# Soundness of bunched inference: the construction

From the draft that `Frontend.Bunched.generate` builds, the greatest solution of its constraints,
and the dependency set of every node, a bunch and a `Spec.Paper.Bunched.Typed` derivation for
every node of the read-back program. Let-bound values of a G type go to the shared context (Let-G);
the others go to the bunch (Let-E), placed so that a product whose factors have disjoint
dependencies can split its bunch (Mul-Ind).

Part of the experimental bunched extension: nothing outside `Proof/Bunched/` imports it.
-/

namespace Determinize.Proof.Bunched
open Determinize.Frontend Spec.Paper

end Determinize.Proof.Bunched
