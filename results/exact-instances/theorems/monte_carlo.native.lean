import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.lam (.fix (.ite (.letE (.bvar 0) (.letE (.real ((0 : Rat) / 1)) (.ite (.lt (.bvar 0) (.bvar 1)) (.bool false) (.bool true)))) (.real ((0 : Rat) / 1)) (.letE (.app (.bvar 1) (.add (.bvar 0) (.neg (.real ((1 : Rat) / 1))))) (.letE (.app (.bvar 4) (.unit)) (.div (.add (.app (.bvar 4) (.bvar 0)) (.mul (.add (.bvar 2) (.neg (.real ((1 : Rat) / 1)))) (.bvar 1))) (.bvar 2)))))))) (.letE (.lam (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1)))) (.letE (.lam (.add (.mul (.real ((2 : Rat) / 1)) (.bvar 0)) (.real ((1 : Rat) / 1)))) (.app (.app (.app (.bvar 2) (.bvar 1)) (.bvar 0)) (.real ((10 : Rat) / 1))))))
theorem certified : (⟨1, 2, 4⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
