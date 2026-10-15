import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.fix (.lam (.matchList (.bvar 1) (.nil) (.letE (.gaussian (.sample .E) (.bvar 0) (.bvar 2)) (.letE (.app (.app (.bvar 5) (.bvar 2)) (.bvar 3)) (.cons (.bvar 1) (.bvar 0))))))) (.letE (.fix (.matchList (.bvar 0) (.real ((0 : Rat) / 1)) (.add (.bvar 0) (.app (.bvar 3) (.bvar 1))))) (.letE (.cons (.real ((1 : Rat) / 1)) (.cons (.real ((2 : Rat) / 1)) (.cons (.real ((3 : Rat) / 1)) (.nil)))) (.letE (.app (.app (.bvar 2) (.bvar 0)) (.real ((1 : Rat) / 1))) (.app (.bvar 2) (.bvar 0))))))
theorem certified : (⟨1, 6, 36⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
