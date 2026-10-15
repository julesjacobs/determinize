import Determinize.Tactic.Storm
open Determinize.Spec.Paper Determinize.Spec.FiniteModel
set_option maxHeartbeats 0
set_option Elab.async false
set_option determinize.storm.maxStates 1000
set_option determinize.storm.timeout 15
def checkedSource : Expr Rat := (.letE (.lam (.letE (.lam (.letE (.ite (.bvar 0) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((9 : Rat) / 10)))) (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((2 : Rat) / 5))))) (.letE (.ite (.bvar 0) (.exponential (.sample .E) (.real ((2 : Rat) / 1))) (.exponential (.sample .E) (.real ((1 : Rat) / 1)))) (.pair (.bvar 1) (.bvar 0))))) (.letE (.lam (.letE (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((19 : Rat) / 20)))) (.letE (.ite (.bvar 0) (.exponential (.sample .E) (.real ((3 : Rat) / 2))) (.exponential (.sample .E) (.real ((3 : Rat) / 4)))) (.pair (.bvar 1) (.bvar 0))))) (.letE (.fix (.lam (.lam (.ite (.lt (.bvar 1) (.real ((1 : Rat) / 1))) (.real ((0 : Rat) / 1)) (.letE (.app (.bvar 2) (.bvar 0)) (.letE (.fst (.bvar 0)) (.letE (.snd (.bvar 1)) (.add (.bvar 0) (.app (.app (.app (.bvar 6) (.bvar 5)) (.add (.bvar 4) (.neg (.real ((1 : Rat) / 1))))) (.bvar 1)))))))))) (.letE (.ite (.lt (.real ((0 : Rat) / 1)) (.bernoulli (.sample .G) (.real ((1 : Rat) / 2)))) (.bvar 2) (.bvar 1)) (.app (.app (.app (.bvar 1) (.bvar 0)) (.bvar 4)) (.bool false))))))) (.app (.bvar 0) (.real ((2 : Rat) / 1))))
theorem certified : (⟨1, 29/20, 3989/1800⟩ : OutputStatistics).Matches
    (bigStepMeasure (Subject.determinized.program checkedSource)) := by storm
#print axioms certified
