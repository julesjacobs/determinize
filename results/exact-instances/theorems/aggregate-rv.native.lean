import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.letE (.fix (.lam (.ite (.lt (.bvar 1) (.bvar 3)) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))) (.app (.app (.bvar 3) (.add (.bvar 2) (.real ((1 : Rat) / 1)))) (.add (.bvar 1) (.bvar 0)))) (.bvar 0)))) (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
theorem certified : (⟨1, 1, 1⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
