import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.lam (.ite (.letE (.bvar 1) (.letE (.bvar 1) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.letE (.bvar 0) (.letE (.bvar 2) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.bool false)))) (.letE (.fix (.ite (.app (.app (.bvar 2) (.bvar 0)) (.real ((2 : Rat) / 1))) (.real ((0 : Rat) / 1)) (.letE (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.letE (.ite (.bvar 0) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 2))) (.uniform (.sample .E) (.real ((1 : Rat) / 2)) (.real ((1 : Rat) / 1)))) (.ite (.bvar 1) (.add (.bvar 0) (.app (.bvar 3) (.add (.bvar 2) (.real ((1 : Rat) / 1))))) (.add (.bvar 0) (.app (.bvar 3) (.bvar 2)))))))) (.app (.bvar 0) (.real ((0 : Rat) / 1)))))
theorem certified : (⟨1, 2, 25/4⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
