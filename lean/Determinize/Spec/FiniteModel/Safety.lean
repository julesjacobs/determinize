import Determinize.Spec.Semantics

/-! Full safety for finite replay certificates, which do not require typing.
The typed determinization statements use `PrimitiveDomainSafe` instead. -/

namespace Determinize.Spec.Paper
open MeasureTheory

/-- Operational non-stuckness through every finite stochastic execution depth. -/
def DoesNotGetStuckAt : Nat → Expr → Prop
  | 0, _ => True
  | fuel + 1, expression =>
      if expression.isValue then True
      else match reduce expression with
      | .next next => DoesNotGetStuckAt fuel next
      | .sample _ fiber continuation =>
          fiber Set.univ = 1 ∧ ∀ᵐ value ∂fiber, DoesNotGetStuckAt fuel (continuation value)
      | .stuck => False

def DoesNotGetStuck (program : Expr) : Prop :=
  ∀ fuel, DoesNotGetStuckAt fuel program


end Determinize.Spec.Paper
