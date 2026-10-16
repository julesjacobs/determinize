import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.letE (.uniform (.sample .E) (.neg (.real ((5 : Rat) / 1))) (.neg (.real ((3 : Rat) / 1)))) (.letE (.uniform (.sample .E) (.real ((2 : Rat) / 1)) (.real ((3 : Rat) / 1))) (.letE (.uniform (.sample .E) (.real ((1 : Rat) / 1)) (.real ((5 : Rat) / 1))) (.letE (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 2))) (.real ((1 : Rat) / 2))) (.letE (.fix (.lam (.lam (.lam (.lam (.ite (.lt (.bvar 4) (.bvar 10)) (.letE (.add (.bvar 4) (.real ((1 : Rat) / 1))) (.letE (.add (.bvar 4) (.mul (.real ((1 : Rat) / 100)) (.bvar 3))) (.letE (.add (.add (.add (.add (.mul (.real ((1 : Rat) / 50)) (.bvar 0)) (.mul (.real ((103 : Rat) / 100)) (.bvar 4))) (.neg (.mul (.real ((3 : Rat) / 10)) (.bvar 3)))) (.neg (.mul (.real ((3 : Rat) / 50)) (.bvar 2)))) (.uniform (.sample .E) (.neg (.real ((1 : Rat) / 1))) (.real ((1 : Rat) / 1)))) (.letE (.add (.bvar 4) (.mul (.real ((1 : Rat) / 100)) (.bvar 3))) (.letE (.add (.add (.add (.add (.mul (.real ((1 : Rat) / 25)) (.bvar 2)) (.mul (.real ((7 : Rat) / 100)) (.bvar 1))) (.neg (.mul (.real ((51 : Rat) / 100)) (.bvar 0)))) (.mul (.real ((17 : Rat) / 20)) (.bvar 4))) (.uniform (.sample .E) (.neg (.real ((4 : Rat) / 5))) (.real ((4 : Rat) / 5)))) (.app (.app (.app (.app (.app (.bvar 10) (.bvar 4)) (.bvar 3)) (.bvar 2)) (.bvar 1)) (.bvar 0))))))) (.bvar 1))))))) (.app (.app (.app (.app (.app (.bvar 0) (.real ((0 : Rat) / 1))) (.bvar 4)) (.bvar 3)) (.bvar 2)) (.bvar 1)))))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
theorem certified : (⟨1, 59684537/20000000, 3562243956904369/400000000000000⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
