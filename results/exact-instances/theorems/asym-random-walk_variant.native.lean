import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.fix (.ite (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((1 : Rat) / 1))) (.ite (.letE (.real ((2 : Rat) / 1)) (.letE (.bvar 1) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.add (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((0 : Rat) / 1))) (.app (.bvar 1) (.real ((0 : Rat) / 1)))) (.add (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))) (.app (.bvar 1) (.add (.bvar 0) (.real ((1 : Rat) / 1)))))))) (.app (.bvar 0) (.real ((0 : Rat) / 1))))
theorem certified : (⟨1, 5/14, 59/196⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
