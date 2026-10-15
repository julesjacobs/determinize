import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.letE (.mul (.real ((5 : Rat) / 1)) (.bernoulli (.sample .E) (.real ((1 : Rat) / 2)))) (.letE (.add (.real ((4 : Rat) / 1)) (.mul (.real ((3 : Rat) / 1)) (.bernoulli (.sample .E) (.real ((1 : Rat) / 2))))) (.letE (.fix (.lam (.lam (.ite (.lt (.bvar 2) (.bvar 6)) (.letE (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((1 : Rat) / 1))) (.letE (.uniform (.sample .E) (.neg (.real ((2 : Rat) / 1))) (.real ((2 : Rat) / 1))) (.app (.app (.app (.bvar 5) (.add (.bvar 4) (.real ((1 : Rat) / 1)))) (.add (.bvar 2) (.bvar 1))) (.add (.bvar 3) (.bvar 0))))) (.bvar 1))))) (.app (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.bvar 2)) (.bvar 1)))))) (.app (.bvar 0) (.real ((1 : Rat) / 1))))
theorem certified : (⟨1, 11/2, 121/4⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
