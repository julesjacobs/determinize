import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.bvar 0)))))
theorem certified : (⟨1, 1/16, 1/256⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
