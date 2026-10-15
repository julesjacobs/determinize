import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.app (.fix (.ite (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.bvar 0) (.app (.bvar 1) (.add (.add (.real ((1 : Rat) / 1)) (.neg (.bvar 0))) (.gaussian (.sample .E) (.real ((0 : Rat) / 1)) (.real ((1 : Rat) / 1))))))) (.real ((0 : Rat) / 1)))
theorem certified : (⟨1, 1/3, 1/3⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
