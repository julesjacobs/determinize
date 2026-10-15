import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.letE (.fix (.ite (.lt (.bvar 0) (.real ((1 : Rat) / 1))) (.nil) (.letE (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 4)))) (.cons (.bvar 0) (.app (.bvar 2) (.add (.bvar 1) (.neg (.real ((1 : Rat) / 1))))))))) (.letE (.fix (.matchList (.bvar 0) (.real ((0 : Rat) / 1)) (.letE (.ite (.bvar 0) (.uniform (.sample .E) (.real ((3 : Rat) / 2)) (.real ((5 : Rat) / 2))) (.uniform (.sample .E) (.real ((1 : Rat) / 2)) (.real ((3 : Rat) / 2)))) (.add (.bvar 0) (.app (.bvar 4) (.bvar 2)))))) (.letE (.app (.bvar 1) (.bvar 2)) (.app (.bvar 1) (.bvar 0)))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
theorem certified : (⟨1, 5/2, 53/8⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
