import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.fix (.letE (.ite (.bvar 0) (.uniform (.sample .E) (.real ((8 : Rat) / 1)) (.real ((12 : Rat) / 1))) (.uniform (.sample .E) (.real ((2 : Rat) / 1)) (.real ((6 : Rat) / 1)))) (.letE (.uniform (.sample .E) (.real ((0 : Rat) / 1)) (.bvar 0)) (.letE (.ite (.bvar 2) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((17 : Rat) / 20)))) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((3 : Rat) / 10))))) (.ite (.bvar 0) (.bvar 1) (.letE (.ite (.bvar 3) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((4 : Rat) / 5)))) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 4))))) (.add (.bvar 2) (.app (.bvar 5) (.bvar 0))))))))) (.app (.bvar 0) (.bool true)))
theorem certified : (⟨1, 9740/1651, 110904220/2725801⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
