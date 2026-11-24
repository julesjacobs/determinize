import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.letE (.fix (.lam (.lam (.ite (.lt (.bvar 0) (.bvar 4)) (.letE (.discrete (.sample .G) (.cons (.real ((1 : Rat) / 2)) (.cons (.real ((1 : Rat) / 4)) (.nil)))) (.ite (.lt (.bvar 0) (.real ((1 : Rat) / 1))) (.letE (.add (.real ((9 : Rat) / 10)) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 5)))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.bvar 3)) (.add (.bvar 2) (.real ((1 : Rat) / 1)))))) (.ite (.lt (.bvar 0) (.real ((2 : Rat) / 1))) (.letE (.add (.real ((11 : Rat) / 10)) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 10)))) (.ite (.letE (.bvar 3) (.letE (.bvar 5) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.add (.bvar 3) (.real ((1 : Rat) / 1)))) (.add (.bvar 2) (.real ((1 : Rat) / 1))))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.bvar 3)) (.bvar 2))))) (.letE (.add (.real ((4 : Rat) / 5)) (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 10)))) (.ite (.letE (.bvar 4) (.letE (.bvar 4) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.add (.bvar 4) (.real ((1 : Rat) / 1)))) (.bvar 3)) (.add (.bvar 2) (.real ((1 : Rat) / 1))))) (.add (.bvar 0) (.app (.app (.app (.bvar 5) (.bvar 4)) (.bvar 3)) (.bvar 2)))))))) (.real ((0 : Rat) / 1)))))) (.app (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
theorem certified : (⟨1, 13/6, 71551/14400⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
