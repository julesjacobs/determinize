import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.fix (.lam (.lam (.matchList (.bvar 2) (.bvar 0) (.letE (.gaussian (.sample .E) (.bvar 2) (.add (.bvar 0) (.neg (.bvar 3)))) (.app (.app (.app (.bvar 6) (.bvar 2)) (.bvar 1)) (.bvar 0))))))) (.letE (.cons (.real ((1 : Rat) / 1)) (.cons (.real ((2 : Rat) / 1)) (.cons (.real ((3 : Rat) / 1)) (.cons (.real ((4 : Rat) / 1)) (.cons (.real ((5 : Rat) / 1)) (.cons (.real ((6 : Rat) / 1)) (.nil))))))) (.app (.app (.app (.bvar 1) (.bvar 0)) (.real ((0 : Rat) / 1))) (.real ((0 : Rat) / 1)))))
theorem certified : (⟨1, 0, 0⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
